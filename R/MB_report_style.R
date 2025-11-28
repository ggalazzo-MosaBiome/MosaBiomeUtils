#' Custom Styler Function
#'
#' @return A named list of styler transformers
#' @export
report_custom_transformers <- function() {
  transformers <- styler::tidyverse_style(strict = TRUE)

  # Force newlines after commas in function calls (e.g., data.frame, slice_max)
  transformers$line_break$add_line_break_after_commas <- function(pd) {
    idx <- which(pd$token == "COMMA")
    if (length(idx) > 0) pd$lag_newlines[idx] <- 1
    pd
  }

  # Force line break after %>%, +, ==, etc. (not before!)
  transformers$line_break$add_line_break_after_pipes <- function(pd) {
    tryCatch({
      pipe_idx <- which(trimws(pd$text) == "%>%")
      for (i in pipe_idx) {
        if (i + 1 <= nrow(pd)) {
          pd$lag_newlines[i + 1] <- 1
        }
      }
      pd
    }, error = function(e) {
      message("Styler transformer 'add_line_break_after_pipes' failed: ", conditionMessage(e))
      pd  # return unmodified pd in case of error
    })
  }

  # Function calls: each argument on a new line
  transformers$token$force_function_args_newlines <- function(pd) {
    if ("SYMBOL_FUNCTION_CALL" %in% pd$token) {
      lp <- which(pd$token == "LPAREN")
      rp <- which(pd$token == "RPAREN")
      if (length(lp) > 0) pd$lag_newlines[lp] <- 1
      if (length(rp) > 0) pd$lead_newlines[rp] <- 1
    }
    pd
  }

  transformers
}
