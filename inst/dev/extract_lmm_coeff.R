library(lme4)
library(lmerTest)
library(nlme)
library(glmmTMB)
library(dplyr)

extract_lmm_coeff <- function(...,model.labels = NULL, p_method="Satterthwaite", ci = FALSE, p_adjust = NULL, num_tests = NULL, ci_method = "Wald") {
  models <- list(...)

  results <- lapply(models, function(model) {

    # Detect model class
    model_class <- class(model)

    # Extract Coefficients from Models
    if ("lmerMod" %in% model_class) {
      # Convert lme4 model to lmerTest for p-values
      model <- lmerTest::as_lmerModLmerTest(model)
      fixed_effects <- summary(model, ddf=p_method)$coefficients
      random_effects <- lme4::VarCorr(model)
      p_values <- fixed_effects[, "Pr(>|t|)"]
      total_N <- nobs(model)  # Total number of observations

      # Confidence Intervals for lmer models (default to Wald)
      ci_values <- if (ci) tryCatch({
        confint(model, method = ci_method) %>%
          as.data.frame() %>%
          dplyr::slice(-c(1:2))
      }, error = function(e) {
        warning("CI could not be computed for lmer model.")
        NULL
      }) else NULL

    } else if ("lmerModLmerTest" %in% model_class) {
      # lmerTest::lmer model
      fixed_effects <- summary(model, ddf=p_method)$coefficients
      random_effects <- lme4::VarCorr(model)
      p_values <- fixed_effects[, "Pr(>|t|)"]
      total_N <- nobs(model)

      # Compute Confidence Intervals for lmerTest models
      ci_values <- if (ci) tryCatch({
        confint(model, method = ci_method) %>%
          as.data.frame() %>%
          dplyr::slice(-c(1:2))
      }, error = function(e) {
        warning("CI could not be computed for lmerTest model.")
        NULL
      }) else NULL

    } else if ("lme" %in% model_class) {
      # nlme::lme model
      fixed_effects <- summary(model)$tTable
      random_effects <- nlme::VarCorr(model)
      p_values <- fixed_effects[, "p-value"]
      total_N <- model$dims$N  # Extract N for nlme

      # Confidence Intervals for nlme::lme models
      ci_values <- if (ci) tryCatch({
        intervals(model, which = "fixed")$fixed[, c("lower", "upper")]
      }, error = function(e) {
        warning("CI could not be computed for lme model.")
        NULL
      }) else NULL

    } else if ("glmmTMB" %in% model_class) {
      # glmmTMB model
      fixed_effects <- summary(model)$coefficients$cond
      random_effects <- glmmTMB::VarCorr(model)
      p_values <- fixed_effects[, "Pr(>|z|)"]
      total_N <- nobs(model)

      # Confidence Intervals for glmmTMB models
      ci_values <- if (ci) tryCatch({
        confint(model, method = ci_method)
      }, error = function(e) {
        warning("CI could not be computed for glmmTMB model.")
        NULL
      }) else NULL

    } else {
      stop("Unsupported model type: ", model_class)
    }

    # Extract Variance and Number of Unique Observations for Random Effects
    random_df <- data.frame(
      Group = names(random_effects),
      Variance = sapply(random_effects, function(re) re[[1]]),
      Num_Obs = sapply(names(random_effects), function(group) {
        length(unique(model@frame[[group]]))
      })
    ) %>%
      mutate(Total_N = total_N)  # Add total N to random effects table

    # Adjust p-values if requested
    if (!is.null(p_adjust)) {
      if (is.null(num_tests)) {
        num_tests <- length(p_values)
      }
      adjusted_p_values <- p.adjust(p_values, method = p_adjust, n = num_tests)
    } else {
      adjusted_p_values <- p_values
    }

    # Convert fixed effects to dataframe and add adjusted p-values
    fixed_effects <- as.data.frame(fixed_effects) %>%
      dplyr::rename(
        "Coeff." = "Estimate",
        "Std. Err" = "Std. Error",
        "Df" = "df",
        "t-value" = "t value",
        "p-value" = "Pr(>|t|)"
      ) %>%
      mutate(
        `adjusted p-value` = adjusted_p_values,
        CI.low = ci_values[,1],
        CI.upp = ci_values[,2]
      ) %>%
      dplyr::relocate(
        "CI.low", .after = "Coeff."
      ) %>%
      dplyr::relocate(
        "CI.upp", .after = "CI.low"
      ) %>%
      mutate(Observations = total_N)  # Add total N column to fixed effects

    list(`Fixed Effect` = fixed_effects, `Random Effect` = random_df, ci = ci_values)
  })
  names(results)=model.labels
  return(results)
}




xxx<-extract_lmm_coeff(reduced_model.LMM.test,full_model.LMM.test,model.labels=c("Reduced model","Full model"),p_method = "Satterthwaite",ci = TRUE,p_adjust = "fdr")
