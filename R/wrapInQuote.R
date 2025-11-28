#' when selecting a vector of unquoted elements, it add quotes to each element

wrapInQuote<-function(){
selected_text <- rstudioapi::getActiveDocumentContext()$selection[[1]]$text
# Detect the delimiter (comma, space, or others)
if (grepl(",", selected_text)) {
  separator <- ","
} else if (grepl("\\s", selected_text)) {
  separator <- "\\s+"  # Matches one or more spaces
} else {
  separator <- ""  # If no obvious separator, treat as a single element
}

# Split the text by the detected separator
elements <- if (separator != "") trimws(strsplit(selected_text, separator)[[1]]) else c(selected_text)

# Format each element with quotes and a comma at the end
formatted_text <- paste0("\"", elements, "\",", collapse = "\n")

# Remove the trailing comma from the last element
formatted_text <- sub(",$", "", formatted_text)
rstudioapi::insertText(formatted_text)
}
