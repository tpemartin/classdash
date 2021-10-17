refresh_document <- function()
{
  rstudioapi::getSourceEditorContext() -> sourcePath
  rstudioapi::documentClose(
    sourcePath$id
  )
  file.edit(sourcePath$path)
}

{
  rstudioapi::setCursorPosition(
    position = sourcePath$selection[[1]]$range$start,
    id = sourcePath2$id
  )
  rstudioapi::setCursorPosition()
  rstudioapi::document_position(.row, .col)
}