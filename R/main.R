#' Initiate a classdash instance
#'
#' @return
#' @export
#'
#' @examples none
Classdash <- function(ss){
  classdash <- list()

  classdash$postClipboard2Gitterchat <- postClipboard2Gitterchat

  classdash$record_gitterThread <- function(){
    sheet_write_threadMsgInfoFromLinkAddress(ss)
  }

  return(classdash)
}
#' Extract a gitter thread info and write to google sheet
#'
#' @description need to have gitter's roomId stored as .roomId in the global environment, also a .sheetname in the global environment as well.
#' @return
#' @export
#'
#' @examples none
sheet_write_threadMsgInfoFromLinkAddress <- function(ss){
  messageId <- get_messageIdFromGitterMessageCopyLink()
  threadMsg <- generate_threadMessageInfo(roomId = .GlobalEnv$.roomId, messageId = messageId)
  .sheetname <- get_sheetname(threadMsg)
  sheet_write_threadMsgInfo(threadMsg = threadMsg,
    sheetname=.sheetname,
    ss=ss)
}
generate_threadMessageInfo <- function(roomId, messageId) {
  threadMsg <- get_threadMessagesInfo(roomId, messageId)

  if(!is.null(threadMsg$threadMessages)){
    threadMsg$firstReplyTime <- compute_1stReplayTime(threadMsg = threadMsg)
  }

  threadMsg <- updateThreadMsgFor_poster_repliers(threadMsg)

  threadMsg$url <- glue::glue("https://gitter.im/110-1-r4ds/main?at={threadMsg$leadingMessage$id}")
  return(threadMsg)
}


#' Post clipboard text to gitter chatroom
#'
#' @description must set .roomId in the global environment
#' @return
#' @export
#'
#' @examples none
postClipboard2Gitterchat <- function(){
  assertthat::assert_that(
    exists(".roomId", envir=.GlobalEnv),
    msg=".roomId does not exist in Global environment"
  )
  clipr::read_clip() |>
    paste0(collapse="\n") |>
    gitterhub::postMessageToARoom(
      roomId = .roomId
    )
}
get_threadMessagesInfo <- function(roomId, messageId){
  messageInfo <- list()
  leadingMessage <- getAMessage(
    roomId = roomId, messageId = messageId
  )
  messageInfo$leadingMessage <- leadingMessage
  if(!is.null(leadingMessage$threadMessageCount) &&
      leadingMessage$threadMessageCount !=0){
    threadMessages <-
      gitterhub:::get_threadMessagesInRoomFromAMessage(
        roomId, messageId)
    messageInfo$threadMessages <- threadMessages
  }
  return(messageInfo)
}

compute_1stReplayTime <- function(threadMsg){
  postingTime <- lubridate::ymd_hms(threadMsg$leadingMessage$sent)
  firstReplyTime <- lubridate::ymd_hms(threadMsg$threadMessages[[1]]$sent)
  (firstReplyTime - postingTime)
}
updateThreadMsgFor_poster_repliers <- function(threadMsg){
  threadMsg$poster <- threadMsg$leadingMessage$fromUser$username
  if(!is.null(threadMsg$threadMessages)){
    threadMsg$repliers <- {
      purrr::map_chr(
        threadMsg$threadMessages,
        ~.x$fromUser$username
      )
    }
  }
  return(threadMsg)
}
sheet_write_threadMsgInfo <- function(threadMsg, sheetname, ss,
  unique_repliers = T){
  {
    googlesheets4::sheet_properties(ss) ->
      sheetsProperties
  }
  newColLetter <- LETTERS[[sheetsProperties$grid_columns+1]]

  repliers <- if(unique_repliers){
    unique(threadMsg$repliers)
  } else {
    threadMsg$repliers
  }
  googlesheets4::range_write(
    data = as.data.frame(c(threadMsg$url,
      threadMsg$poster,
      round(threadMsg$firstReplyTime, 2),
      repliers
    )),
    sheet=sheetname,
    range=glue::glue("{newColLetter}1"),
    ss=ss,
    col_names = F
  )
}
get_messageIdFromGitterMessageCopyLink <- function() {
  clipr::read_clip() -> cliptext
  assertthat::assert_that(
    stringr::str_detect(cliptext, "\\?at\\="),
    msg="This is not a gitter message link."
  )
  messageId <- stringr::str_extract(cliptext, "(?<=\\?at\\=).+")
  return(messageId)
}
get_sheetname <- function(threadMsg){
  (lubridate::ymd_hms(threadMsg$leadingMessage$sent)+lubridate::days(1) - .GlobalEnv$.startingSemester) /
    lubridate::dweeks(1) -> numberOfWeeks
  schoolWeeks <- ceiling(numberOfWeeks)
  paste0("week", schoolWeeks)
}
getAMessage <- function(roomId, messageId) {
  postingMessage = glue::glue("GET /v1/rooms/{roomId}/chatMessages/{messageId}")
  getAMessage_apiFun <- gitterhub:::gitter_apiFunctional(postingMessage)
  getAMessage_apiFun() -> response
  invisible(response)
}

