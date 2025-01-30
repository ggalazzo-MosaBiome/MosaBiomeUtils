library(styler)
library(dplyr)

tidyverse_style_mod <- tidyverse_style(
  strict = TRUE
) %>%
  purrr::modify_at("line_break", function(transformers) {
    transformers$add_line_break_after_assignment <- function(pd) {
      pd$lag_newlines[which(pd$token == "=")] <- 1  # Ensures line break after =
      pd$lag_newlines[which(pd$token == "EQ_SUB")] <- 1  # Ensures break after <-
      pd
    }

    transformers$add_line_break_after_operator <- function(pd) {
      pd$lag_newlines[which(pd$token %in% c("GT", "LT", "EQ", "PLUS", "MINUS",
                                            "EQ_SUB", "AND", "OR", "TILDE", "SPECIAL"))] <- 1
      pd
    }

    transformers$add_line_break_after_comma <- function(pd) {
      pd$lag_newlines[which(pd$token == "COMMA")] <- 1  # Forces line break after commas
      pd
    }

    transformers
  }) %>%
  purrr::modify_at("spacing", function(transformers) {
    transformers$space_around_operators <- function(pd) {
      pd$spaces[which(pd$token %in% c("EQ", "EQ_SUB", "GT", "LT", "PLUS", "MINUS"))] <- 1
      pd
    }

    transformers
  }) %>%
  purrr::modify_at("token", function(transformers) {
    transformers$force_function_args_newlines <- function(pd) {
      if ("SYMBOL_FUNCTION_CALL" %in% pd$token) {
        pd$lag_newlines[which(pd$token == "LPAREN")] <- 1
        pd$lead_newlines[which(pd$token == "RPAREN")] <- 1
      }
      pd
    }

    transformers
  })
