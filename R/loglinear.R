#' Translate shortcut strings to formulas
#'
#' @param shortcut Character: "independence", "saturated", or "conditional"
#' @param vars Character vector of margin variable names
#' @param conds Character vector of conditioning variable names (optional)
#' @return Formula object
#' @keywords internal
shortcut_to_formula <- function(shortcut, vars, conds = NULL) {
  shortcut <- match.arg(tolower(shortcut),
                        c("independence", "saturated", "conditional"))

  all_vars <- c(vars, conds)

  if (shortcut == "independence") {
    # Main effects only: ~ A + B + C
    formula_str <- paste("~", paste(all_vars, collapse = " + "))

  } else if (shortcut == "saturated") {
    # All interactions: ~ A * B * C
    formula_str <- paste("~", paste(all_vars, collapse = " * "))

  } else if (shortcut == "conditional") {
    # Conditional independence
    # Margins are independent given conditions
    # ~ marg1 + marg2 + cond + marg1:cond + marg2:cond
    if (length(conds) == 0) {
      stop("'conditional' shortcut requires conditioning variables (use conds aesthetic)",
           call. = FALSE)
    }
    main_effects <- paste(all_vars, collapse = " + ")
    # Create all interactions between margin vars and condition vars
    interactions <- paste(
      apply(expand.grid(vars, conds), 1, paste, collapse = ":"),
      collapse = " + "
    )
    formula_str <- paste("~", main_effects, "+", interactions)
  }

  as.formula(formula_str)
}


#' Build model formula from user specification
#'
#' @param expected Formula, character shortcut, or NULL
#' @param vars Character vector of margin variable names
#' @param conds Character vector of conditioning variable names (optional)
#' @return Formula object or NULL
#' @keywords internal
build_model_formula <- function(expected, vars, conds = NULL) {
  # If expected is NULL, no model
  if (is.null(expected)) {
    return(NULL)
  }

  # If expected is already a formula, use it directly
  if (is.formula(expected)) {
    return(expected)
  }

  # If expected is a character, treat as shortcut
  if (is.character(expected) && length(expected) == 1) {
    return(shortcut_to_formula(expected, vars, conds))
  }

  # Otherwise, invalid input
  stop("'expected' must be a formula, character shortcut ('independence', ",
       "'saturated', 'conditional'), or NULL", call. = FALSE)
}


#' Fit Poisson GLM and calculate Pearson residuals
#'
#' @param data Data frame with .n column (observed counts)
#' @param vars Character vector of all variable names (margins + conds)
#' @param model_formula Formula for the GLM
#' @return Data frame with added .expected and .residual columns
#' @keywords internal
#' @importFrom dplyr select all_of distinct left_join
fit_loglinear_model <- function(data, vars, model_formula) {
  # Check for reserved column names
  if (any(c(".expected", ".residual") %in% names(data))) {
    warning("Data contains reserved column names (.expected, .residual). ",
            "These will be overwritten.", call. = FALSE)
  }

  # Create aggregated dataset for modeling
  # Select only the variables needed for the model plus observed counts
  mod_data <- data |>
    dplyr::select(dplyr::all_of(c(vars, ".n"))) >
    dplyr::distinct()

  # Fit Poisson GLM
  tryCatch({
    # Build GLM formula with .n as response
    glm_formula <- reformulate(
      attr(terms(model_formula), "term.labels"),
      response = ".n"
    )

    # Fit the model
    model <- glm(glm_formula, data = mod_data, family = poisson())

    # Calculate expected values (fitted values from the model)
    mod_data$.expected <- predict(model, type = "response")

    # Calculate Pearson residuals with protection against zero expected values
    # Use machine epsilon as minimum to avoid division by zero
    expected_safe <- pmax(mod_data$.expected, .Machine$double.eps)
    mod_data$.residual <- (mod_data$.n - expected_safe) / sqrt(expected_safe)

    # Join the expected values and residuals back to original data
    data <- dplyr::left_join(
      data,
      mod_data[, c(vars, ".expected", ".residual")],
      by = vars
    )

    data

  }, error = function(e) {
    warning("Loglinear model fitting failed: ", e$message,
            "\nProceeding without residual shading.", call. = FALSE)
    # On error, set expected = observed and residuals = 0
    data$.expected <- data$.n
    data$.residual <- 0
    data
  })
}
