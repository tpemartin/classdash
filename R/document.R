refresh_document <- function()
{
  rstudioapi::getSourceEditorContext() -> sourcePath
  rstudioapi::documentClose(
    sourcePath$id
  )
  file.edit(sourcePath$path)
}
