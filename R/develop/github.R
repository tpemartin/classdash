get_issueNumber <- function(issue_url){
  issue_url |>
    stringr::str_extract("(?<=\\/)[0-9]+$") |>
    as.integer()
}
library(gitterhub)

initiate_gitHubService <- function(){
  gitterhub::githubService()
}
gh <- gitterhub::githubService()
# /repos/{owner}/{repo}/issues/{issue_number}/comments
owner="tpemartin"
repo="110-1-r4ds-main"
issue_number=6

githubdash <- function(){

}
get_classIssues <- function(.owner, .repo){
  gh$list_issues(
    owner = .owner,
    repo = .repo
  )
}
get_issueCommentsFromClipboard <- function(){

}
class <- list()
class$github$issues$list <-
gh$list_issues(
  owner = "tpemartin",
  repo = "110-1-r4ds-main"
)
class$github$issues$data.frame <- list2DF(purrr::transpose(class$github$issues))


class$github$issues$list[[6]]$url |>
  get_issueNumber() -> issue_number

class$github$comments$issue[[6]] <-
  gh$list_issue_comments(
    owner, repo,
    issue_number = issue_number
  )
class$github$issues$list[[6]]$commentDetails <-
  gh$list_issue_comments(
    owner,repo, issue_number=issue_number
  )
gh$list_issue_comments
