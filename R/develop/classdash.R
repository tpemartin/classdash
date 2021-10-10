"GET https://api.gitter.im/v1/rooms/:roomId/chatMessages/:messageId"

getAMessage <- function(roomId, messageId) {
  postingMessage = glue::glue("GET /v1/rooms/{roomId}/chatMessages/{messageId}")
  getAMessage_apiFun <- gitterhub:::gitter_apiFunctional(postingMessage)
  getAMessage_apiFun() -> response
  invisible(response)
}


replyMessageAsThread <- function (text, roomId, messageId)
{
  postingMessage = glue::glue("POST /v1/rooms/{roomId}/chatMessages/{messageId}/thread")
  replyMessageAsThread_apiFun <- gitterhub:::gitter_apiFunctional(postingMessage)
  bodyContent <-
    list(
      text=text
    )
  replyMessageAsThread_apiFun(
    body=bodyContent,
    encode="json"
  ) -> response
  invisible(response)
}
postMessageToARoom <- function (message, roomId)
{
  postingMessage = glue::glue("POST /v1/rooms/{roomId}/chatMessages")
  postMessageToARoom_apiFun <- gitterhub:::gitter_apiFunctional(postingMessage)
  postMessageToARoom_response <- postMessageToARoom_apiFun(query = list(text = message))
  return(postMessageToARoom_response)
}

#' Title
#'
#' @param groupId
#' @param name
#' @param topic
#' @param securityType
#'
#' @return
#' @export
#'
#' @examples
#'   roomId <- gitter$roomIds$`110-1-r4ds/main`
#'   text <- '
#' (程式測試，請忽略)
#' ```{r}
#' list("1", 1)
#' ```
#' '
postMessageToARoom <- function(text, roomId)
{

  postingMessage <- glue::glue(
    "POST /v1/rooms/{roomId}/chatMessages"
  )
  postMessageToARoomFun <- gitterhub:::gitter_apiFunctional(postingMessage)
  bodyContent <-
    list(
      text=text
    )
  postMessageToARoomFun(
    body=bodyContent,
    encode="json"
  ) -> response
  invisible(response)
}
replyMessageAsThread <- function(text, roomId, messageId)
{
  roomId <- gitter$roomIds$`110-1-r4ds/main`
  messageId <- "6162434629ddcd029313a5b8"
  text <- "請回答於此討論串"
  postingMessage <- glue::glue(
    "POST /v1/rooms/{roomId}/chatMessages/{messageId}"
  )
  postMessageToARoomFun <- gitterhub:::gitter_apiFunctional(postingMessage)
  bodyContent <-
    list(
      text=text
    )
  postMessageToARoomFun(
    body=bodyContent,
    encode="json"
  ) -> response
  invisible(response)
}
"請回答於此討論串" |>
  replyMessageAsThread(gitter$roomIds$`110-1-r4ds/main`,"6162434629ddcd029313a5b8") ->
  response
