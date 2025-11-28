#' when selecting a text elements, it add tags to underline it in an HTML report

underlineHtml<-function(){
  selected_text <- rstudioapi::getActiveDocumentContext()$selection[[1]]$text

  # Format each element with quotes and a comma at the end
  formatted_text <- paste0("<u> ", selected_text, "</u>")

  rstudioapi::insertText(formatted_text)
}
