#' Format Selection or Full File with Custom Styler
#'
#' @export
MB_style_report <- function() {
  if (!rstudioapi::isAvailable()) {
    stop("RStudio API is not available.")
  }

  context <- rstudioapi::getActiveDocumentContext()
  selection <- context$selection[[1]]$text
  has_selection <- nchar(selection) > 0

  if (has_selection) {
    # Style just the selected code
    styled <- styler::style_text(
      text = selection,
      transformers = MosaBiomeUtils::report_custom_transformers()
    )

    rstudioapi::modifyRange(
      location = context$selection[[1]]$range,
      text = paste(styled, collapse = "\n"),
      id = context$id
    )
  } else {
    # Style the entire document
    styled <- styler::style_text(
      text = context$contents,
      transformers = MosaBiomeUtils::report_custom_transformers()
    )

    full_range <- rstudioapi::document_range(
      start = rstudioapi::document_position(1, 0),
      end = rstudioapi::document_position(length(context$contents), nchar(context$contents[length(context$contents)]))
    )

    rstudioapi::modifyRange(
      location = full_range,
      text = paste(styled, collapse = "\n"),
      id = context$id
    )
  }
}
