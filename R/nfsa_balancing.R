#' Reconcile all time periods using the fixed equation workbook
#'
#' @param data_input Data frame with columns: ref_area, id, time_period, obs_value.
#' @param Country Optional label used only to build <Country>_all_periods.
#' @param run_name Optional explicit output prefix; overrides Country.
#' @param output_dir Optional complete output directory. By default a subfolder
#'   named <run_name>_reconciliation_output is created in getwd() at call time.
#' @param scale_floor,weight_rules Original scale and weight settings.
#' @param osqp_eps_abs,osqp_eps_rel,osqp_max_iter,osqp_verbose Original OSQP controls.
#' @param check_tolerance Post-solve tolerance.
#' @param equation_file Optional path to an equation workbook.
#'
#' @return A data frame with exactly the same columns and row order as
#'   data_input, with obs_value replaced by the reconciled value where
#'   reconciliation was performed.
#'
#' @export
nfsa_balancing <- function(
    data_input,
    Country = NULL,
    run_name = NULL,
    output_dir = NULL,
    scale_floor = NULL,
    weight_rules = data.frame(
      match_type = "exact",
      pattern = "S1.B1GQ.B",
      weight = 1e10
    ),
    osqp_eps_abs = 1e-6,
    osqp_eps_rel = 1e-6,
    osqp_max_iter = 100000L,
    osqp_verbose = FALSE,
    check_tolerance = 0.1,
    equation_file = "M:/nas/Rprod/assets/balancing_equations.xlsx"
)
{

  .check_reconciliation_packages()

  if (is.null(run_name)) {
    if (is.null(Country)) {
      run_name <- "reconciliation_all_periods"
    } else {

      if (
        length(Country) != 1L ||
        is.na(Country) ||
        trimws(as.character(Country)) == ""
      ) {
        stop(
          "Country must be NULL or one non-empty label.",
          call. = FALSE
        )
      }

      run_name <- paste0(Country, "_all_periods")
    }
  }

  if (is.null(output_dir)) {
    output_dir <- file.path(
      getwd(),
      paste0(run_name, "_reconciliation_output")
    )
  }



  # ------------------------------------------------------------
  # Run the original reconciliation
  # ------------------------------------------------------------

  reconciliation_result <- .run_original_reconciliation(
    data_input = data_input,
    equation_file = equation_file,
    output_dir = output_dir,
    run_name = run_name,
    scale_floor = scale_floor,
    weight_rules = weight_rules,
    osqp_eps_abs = osqp_eps_abs,
    osqp_eps_rel = osqp_eps_rel,
    osqp_max_iter = osqp_max_iter,
    osqp_verbose = osqp_verbose,
    check_tolerance = check_tolerance
  )

  # ------------------------------------------------------------
  # Create output with EXACTLY the same structure as data_input
  # ------------------------------------------------------------

  output_data <- data_input

  # Find the relevant columns.
  # This allows minor differences such as Time Period / TIME_PERIOD.
  normalized_names <- tolower(
    gsub("[^[:alnum:]]+", "", names(output_data))
  )

  time_col <- which(normalized_names == "timeperiod")
  value_col <- which(normalized_names == "obsvalue")

  if (length(time_col) != 1L) {
    stop(
      "Could not identify exactly one 'time_period' column.",
      call. = FALSE
    )
  }

  if (length(value_col) != 1L) {
    stop(
      "Could not identify exactly one 'obs_value' column.",
      call. = FALSE
    )
  }

  # The reconciliation code uses column 2 as the variable identifier.
  id_col <- 2L

  # Work with normalized character versions for matching.
  input_time_period <- as.character(output_data[[time_col]])
  input_time_period[is.na(input_time_period)] <- ""
  input_time_period <- trimws(input_time_period)

  input_id <- as.character(output_data[[id_col]])
  input_id[is.na(input_id)] <- ""
  input_id <- gsub("\u00A0", " ", input_id, fixed = TRUE)
  input_id <- gsub("[[:space:]]+", "", input_id)

  # Make sure the output value column can contain reconciled numeric values.
  output_data[[value_col]] <- suppressWarnings(
    as.numeric(
      gsub(
        ",",
        ".",
        as.character(output_data[[value_col]]),
        fixed = TRUE
      )
    )
  )

  # ------------------------------------------------------------
  # Put reconciled values back into the corresponding input rows
  # ------------------------------------------------------------

  for (period_result in reconciliation_result$period_results) {

    current_period <- as.character(period_result$time_period)

    # x_new is a named vector:
    # names = variable identifiers
    # values = reconciled values
    reconciled_values <- period_result$solution$x_new

    if (length(reconciled_values) == 0L) {
      next
    }

    reconciled_ids <- names(reconciled_values)

    # Rows belonging to this period and to variables that were reconciled.
    rows_to_update <-
      input_time_period == current_period &
      input_id %in% reconciled_ids

    if (!any(rows_to_update)) {
      next
    }

    # match() ensures each ID receives its corresponding reconciled value.
    output_data[[value_col]][rows_to_update] <-
      reconciled_values[
        match(
          input_id[rows_to_update],
          reconciled_ids
        )
      ]
  }

  # ------------------------------------------------------------
  # Return same rows, same columns, same column order as input
  # ------------------------------------------------------------

  output_data
}


################ HELP FCT ######################

# Same dependency check as the original script, now delayed until the call.
.check_reconciliation_packages <- function() {
  required_packages <- c(
    "readxl", "openxlsx", "Matrix", "osqp", "ggplot2", "dplyr", "tibble"
  )
  missing_packages <- required_packages[
    !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
  ]

  if (length(missing_packages) > 0) {
    stop(
      paste0(
        "Please install the missing packages first:\n",
        "install.packages(c(",
        paste(shQuote(missing_packages), collapse = ", "),
        "))"
      ),
      call. = FALSE
    )
  }
  invisible(NULL)
}



.run_original_reconciliation <- local({

  # >>> BEGIN ORIGINAL WORKFLOW: copied byte-for-byte from sections 2 through 11.
  # ============================================================
  # 2. TEXT NORMALIZATION AND SMALL LOOKUP HELPERS
  # ============================================================

  # normalize_symbol_text()
  # -----------------------
  # Converts symbols and fill values to a consistent compact representation.
  #
  # The function:
  #   1. converts the input to character;
  #   2. replaces missing values with empty strings;
  #   3. replaces non-breaking spaces, which often come from Excel;
  #   4. removes every remaining whitespace character.
  #
  # Example:
  #   "  S1 . P2 . D  "  becomes  "S1.P2.D".
  normalize_symbol_text <- function(x) {
    x <- as.character(x)
    x[is.na(x)] <- ""
    x <- gsub("\u00A0", " ", x, fixed = TRUE)
    x <- gsub("[[:space:]]+", "", x)
    x
  }


  # normalize_formula_text()
  # ------------------------
  # Standardizes equation text before it is parsed.
  #
  # In addition to removing whitespace, the function converts common Unicode
  # mathematical symbols and dash variants into the ASCII notation expected by
  # the parser.
  normalize_formula_text <- function(x) {
    x <- as.character(x)
    x[is.na(x)] <- ""

    x <- gsub("\u00A0", " ", x, fixed = TRUE)
    x <- gsub("≤", "<=", x, fixed = TRUE)
    x <- gsub("≥", ">=", x, fixed = TRUE)
    x <- gsub("−", "-", x, fixed = TRUE)
    x <- gsub("–", "-", x, fixed = TRUE)
    x <- gsub("—", "-", x, fixed = TRUE)
    x <- gsub("[[:space:]]+", "", x)
    x
  }


  # normalize_header_text()
  # -----------------------
  # Standardizes Excel column names so that harmless differences in capitalization
  # or spacing do not affect recognition of "Accounting Check", "Left", "Mid",
  # and "Right".
  normalize_header_text <- function(x) {
    x <- as.character(x)
    x[is.na(x)] <- ""
    x <- gsub("\u00A0", " ", x, fixed = TRUE)
    x <- trimws(x)
    x <- gsub("[[:space:]]+", " ", x)
    tolower(x)
  }


  # split_dot_components()
  # ----------------------
  # Splits a dot-separated identifier into non-empty components.
  #
  # Example:
  #   "S1.P2.D" becomes c("S1", "P2", "D").
  split_dot_components <- function(x) {
    x <- normalize_symbol_text(x)

    if (length(x) == 0 || x == "") {
      return(character(0))
    }

    parts <- unlist(strsplit(x, ".", fixed = TRUE), use.names = FALSE)
    parts[parts != ""]
  }


  # is_number_token()
  # -----------------
  # Returns TRUE when a token is a non-negative numerical literal. A preceding
  # plus or minus sign is handled separately by the expression parser.
  #
  # Accepted examples include 12, 3.4, .5, and 1e-3.
  is_number_token <- function(x) {
    grepl("^((\\d+\\.?\\d*)|(\\.\\d+))([eE][+-]?\\d+)?$", x)
  }


  # split_expression_terms()
  # ------------------------
  # Splits a normalized linear expression into signed terms. A leading plus sign
  # is added when necessary, so every returned term starts with + or -.
  #
  # The negative look-behind (?<![eE]) ensures that a sign inside a scientific
  # notation exponent, for example the minus sign in 1e-3, is not mistaken for the
  # beginning of a new algebraic term.
  split_expression_terms <- function(expression) {
    expression <- normalize_formula_text(expression)

    if (expression == "") {
      stop("An expression side is empty.", call. = FALSE)
    }

    if (!grepl("^[+-]", expression)) {
      expression <- paste0("+", expression)
    }

    sign_positions <- gregexpr(
      "(?<![eE])[+-]",
      expression,
      perl = TRUE
    )[[1]]

    term_ends <- c(
      sign_positions[-1] - 1L,
      nchar(expression)
    )

    substring(
      expression,
      first = sign_positions,
      last = term_ends
    )
  }


  # split_fill_values()
  # -------------------
  # Converts one Excel cell from a Left/Mid/Right column into a vector of fill
  # values. Commas, semicolons, spaces, tabs, and line breaks are accepted as
  # separators. Duplicate values are removed while preserving the first occurrence.
  split_fill_values <- function(x) {
    if (length(x) == 0 || is.na(x) || trimws(as.character(x)) == "") {
      return(character(0))
    }

    x <- as.character(x)
    x <- gsub("\u00A0", " ", x, fixed = TRUE)
    x <- gsub(",", " ", x, fixed = TRUE)
    x <- gsub(";", " ", x, fixed = TRUE)
    x <- gsub("[\r\n\t]", " ", x)
    x <- trimws(x)

    values <- unlist(strsplit(x, "[[:space:]]+"), use.names = FALSE)
    values <- values[values != ""]

    unique(normalize_symbol_text(values))
  }


  # lookup_named_numeric()
  # ----------------------
  # Looks up several keys in a named numeric vector while returning NA for keys
  # that are missing, blank, or not present. This is useful when output tables also
  # contain symbols that did not enter the optimization or equations without a
  # slack variable.
  lookup_named_numeric <- function(named_vector, keys) {
    keys <- as.character(keys)
    result <- rep(NA_real_, length(keys))

    if (length(named_vector) == 0) {
      return(result)
    }

    valid <- !is.na(keys) & keys != "" & keys %in% names(named_vector)
    result[valid] <- as.numeric(unname(named_vector[keys[valid]]))
    result
  }


  # ============================================================
  # 3. CONSTRUCTION OF COMPLETE VARIABLE IDENTIFIERS
  # ============================================================

  # insert_fill_component()
  # -----------------------
  # Inserts a Left, Mid, or Right fill value into the components of a compact
  # variable token.
  #
  # Examples:
  #   token c("P2", "D"), fill "S1", position "Left"
  #     -> c("S1", "P2", "D")
  #
  #   token c("S1"), fill "P2.D", position "Right"
  #     -> c("S1", "P2", "D")
  #
  #   token c("S1", "_Z"), fill "B9FX9", position "Mid"
  #     -> c("S1", "B9FX9", "_Z")
  insert_fill_component <- function(token_parts, fill_value, fill_position) {
    fill_parts <- split_dot_components(fill_value)

    if (length(fill_parts) == 0) {
      stop("A Left/Mid/Right fill value is empty.", call. = FALSE)
    }

    if (fill_position == "Left") {
      return(c(fill_parts, token_parts))
    }

    if (fill_position == "Right") {
      return(c(token_parts, fill_parts))
    }

    if (fill_position == "Mid") {
      if (length(token_parts) == 0) {
        stop(
          "A Mid component cannot be inserted into an empty variable token.",
          call. = FALSE
        )
      }

      return(c(token_parts[1], fill_parts, token_parts[-1]))
    }

    stop("fill_position must be Left, Mid, or Right.", call. = FALSE)
  }


  # classify_component()
  # --------------------
  # Supports the compact prefix notation used by some equation rows, for example
  #
  #   D29:S1.D = S1.C.
  #
  # The prefix D29 supplies the missing transaction component. To reconstruct the
  # final sector.transaction.entry identifier, every component is classified as a
  # sector, transaction, or entry component.
  classify_component <- function(part) {
    if (part %in% c("C", "D", "_Z")) {
      return("entry")
    }

    if (grepl("^S[0-9A-Z_]*$", part)) {
      return("sector")
    }

    "transaction"
  }


  # complete_variable_key_from_parts()
  # ----------------------------------
  # Reorders components identified by classify_component() into the standard
  # sector.transaction.entry order. The function stops when a dimension is
  # missing or supplied more than once, because the intended variable would then
  # be ambiguous.
  complete_variable_key_from_parts <- function(all_parts, token_for_error = "") {
    dimensions <- list(
      sector = NA_character_,
      transaction = NA_character_,
      entry = NA_character_
    )

    for (part in all_parts) {
      dimension_name <- classify_component(part)

      if (!is.na(dimensions[[dimension_name]])) {
        stop(
          paste0(
            "Dimension conflict while parsing token '", token_for_error,
            "'. Component '", part, "' was classified as ", dimension_name,
            ", but that dimension is already filled."
          ),
          call. = FALSE
        )
      }

      dimensions[[dimension_name]] <- part
    }

    missing_dimensions <- names(dimensions)[is.na(unlist(dimensions))]

    if (length(missing_dimensions) > 0) {
      stop(
        paste0(
          "Could not complete token '", token_for_error,
          "' to sector.transaction.entry. Missing: ",
          paste(missing_dimensions, collapse = ", "),
          "."
        ),
        call. = FALSE
      )
    }

    paste(
      dimensions$sector,
      dimensions$transaction,
      dimensions$entry,
      sep = "."
    )
  }


  # make_variable_key()
  # -------------------
  # Converts one variable token from an equation into its final three-part symbol.
  #
  # The function first applies an optional Left/Mid/Right fill. If the result has
  # exactly three dot-separated components, it is already complete. Otherwise,
  # the function tries the supported prefix notation described above.
  #
  # Products, powers, and brackets are rejected deliberately: the reconciliation
  # model is linear and the parser supports only sums and differences.
  make_variable_key <- function(
    token,
    prefix_parts = character(0),
    fill_position = NA_character_,
    fill_value = NA_character_
  ) {
    token <- normalize_symbol_text(token)

    if (token == "") {
      stop("A variable token is empty.", call. = FALSE)
    }

    if (grepl("[*/^()]", token)) {
      stop(
        paste0(
          "Unsupported term '", token, "'. The parser supports sums and ",
          "differences only, not products, powers, or brackets."
        ),
        call. = FALSE
      )
    }

    token_parts <- split_dot_components(token)

    has_fill <- !is.na(fill_position) &&
      !is.na(fill_value) &&
      fill_position %in% c("Left", "Mid", "Right") &&
      normalize_symbol_text(fill_value) != ""

    if (has_fill) {
      token_parts <- insert_fill_component(
        token_parts = token_parts,
        fill_value = fill_value,
        fill_position = fill_position
      )
    }

    # The normal case: the fill logic has produced a complete identifier.
    if (length(token_parts) == 3) {
      return(paste(token_parts, collapse = "."))
    }

    # Compatibility with compact prefix notation such as D29:S1.D=S1.C.
    if (length(prefix_parts) > 0) {
      completed <- tryCatch(
        complete_variable_key_from_parts(
          all_parts = c(prefix_parts, token_parts),
          token_for_error = token
        ),
        error = function(e) e
      )

      if (!inherits(completed, "error")) {
        return(completed)
      }
    }

    stop(
      paste0(
        "Variable token '", token, "' became '",
        paste(token_parts, collapse = "."),
        "', but the final variable must contain exactly three dot-components. ",
        "Check the Accounting Check and the Left/Mid/Right fill value."
      ),
      call. = FALSE
    )
  }


  # add_coefficient()
  # -----------------
  # Adds a coefficient to a named coefficient vector. If the same variable appears
  # more than once in an expression, its coefficients are combined.
  #
  # Example:
  #   x + x - y = 0
  # becomes coefficients 2 for x and -1 for y.
  add_coefficient <- function(coefficients, variable_name, value) {
    if (variable_name %in% names(coefficients)) {
      coefficients[variable_name] <- coefficients[variable_name] + value
    } else {
      coefficients[variable_name] <- value
    }

    coefficients
  }


  # ============================================================
  # 4. PARSING OF LINEAR EQUATIONS AND INEQUALITIES
  # ============================================================

  # detect_relation_operator()
  # --------------------------
  # Finds the relation operator in one normalized equation. Longer operators are
  # checked first so that "<=" is not mistaken for "<" and ">=" is not mistaken
  # for ">".
  detect_relation_operator <- function(equation_body) {
    supported_operators <- c("<=", ">=", "<", ">", "=")

    for (operator in supported_operators) {
      if (grepl(operator, equation_body, fixed = TRUE)) {
        return(operator)
      }
    }

    stop("No equality or inequality operator was found.", call. = FALSE)
  }


  # parse_expression_side()
  # -----------------------
  # Parses one side of a linear relation into:
  #   - a named vector of variable coefficients;
  #   - one numerical constant.
  #
  # side_multiplier is +1 for the left-hand side and -1 for the right-hand side.
  # Consequently, the combined result represents
  #
  #   LHS - RHS = 0.
  parse_expression_side <- function(
    expression,
    prefix_parts,
    side_multiplier,
    fill_position,
    fill_value
  ) {
    tokens <- split_expression_terms(expression)

    coefficients <- numeric(0)
    constant <- 0

    for (token in tokens) {
      sign_character <- substr(token, 1, 1)
      token_body <- substr(token, 2, nchar(token))

      if (token_body == "") {
        stop("An empty term was found in an expression.", call. = FALSE)
      }

      term_sign <- if (sign_character == "-") -1 else 1
      final_sign <- side_multiplier * term_sign

      if (is_number_token(token_body)) {
        # Numerical terms contribute to the constant part.
        constant <- constant + final_sign * as.numeric(token_body)
      } else {
        # Non-numerical terms are interpreted as variable identifiers.
        variable_name <- make_variable_key(
          token = token_body,
          prefix_parts = prefix_parts,
          fill_position = fill_position,
          fill_value = fill_value
        )

        coefficients <- add_coefficient(
          coefficients = coefficients,
          variable_name = variable_name,
          value = final_sign
        )
      }
    }

    list(
      coefficients = coefficients,
      constant = constant
    )
  }


  # render_expression_side()
  # ------------------------
  # Reconstructs one expression side after all compact variable tokens have been
  # expanded. This rendered text is written to the diagnostic output workbooks so
  # that users can inspect the exact three-part equations used by the code.
  render_expression_side <- function(
    expression,
    prefix_parts,
    fill_position,
    fill_value
  ) {
    tokens <- split_expression_terms(expression)

    rendered_terms <- character(0)

    for (token in tokens) {
      sign_character <- substr(token, 1, 1)
      token_body <- substr(token, 2, nchar(token))

      rendered_body <- if (is_number_token(token_body)) {
        token_body
      } else {
        make_variable_key(
          token = token_body,
          prefix_parts = prefix_parts,
          fill_position = fill_position,
          fill_value = fill_value
        )
      }

      rendered_terms <- c(
        rendered_terms,
        paste0(sign_character, rendered_body)
      )
    }

    rendered_expression <- paste0(rendered_terms, collapse = "")

    # A leading plus sign is mathematically redundant and less readable.
    if (substr(rendered_expression, 1, 1) == "+") {
      rendered_expression <- substr(
        rendered_expression,
        2,
        nchar(rendered_expression)
      )
    }

    rendered_expression
  }


  # parse_equation()
  # ----------------
  # Parses one expanded equation instance and returns all information needed to
  # build one row of the mathematical system.
  #
  # The returned coefficient vector describes the real-variable part of
  #
  #   sum_i coefficient_i * x_i = rhs.
  #
  # For inequalities, build_system() later adds the appropriate slack coefficient:
  #   +1 for <= and -1 for >=.
  parse_equation <- function(
    equation_raw,
    sheet_name,
    row_number,
    fill_position,
    fill_value
  ) {
    equation_clean <- normalize_formula_text(equation_raw)

    if (equation_clean == "") {
      stop("The Accounting Check is empty.", call. = FALSE)
    }

    prefix_parts <- character(0)
    equation_body <- equation_clean

    # Supported compact prefix example:
    #   D29:S1.D=S1.C
    # The text before the colon supplies a missing dimension to both sides.
    if (grepl(":", equation_clean, fixed = TRUE)) {
      colon_position <- regexpr(":", equation_clean, fixed = TRUE)[1]
      prefix_text <- substr(equation_clean, 1, colon_position - 1)
      equation_body <- substr(
        equation_clean,
        colon_position + 1,
        nchar(equation_clean)
      )
      prefix_parts <- split_dot_components(prefix_text)
    }

    operator_original <- detect_relation_operator(equation_body)

    # Strict inequalities cannot be represented as closed numerical constraints.
    # They are therefore interpreted as their non-strict counterparts.
    operator_used <- if (operator_original == "<") {
      "<="
    } else if (operator_original == ">") {
      ">="
    } else {
      operator_original
    }

    sides <- strsplit(
      equation_body,
      operator_original,
      fixed = TRUE
    )[[1]]

    if (length(sides) != 2) {
      stop(
        "The Accounting Check could not be split into exactly two sides.",
        call. = FALSE
      )
    }

    lhs <- sides[1]
    rhs_expression <- sides[2]

    lhs_parsed <- parse_expression_side(
      expression = lhs,
      prefix_parts = prefix_parts,
      side_multiplier = +1,
      fill_position = fill_position,
      fill_value = fill_value
    )

    rhs_parsed <- parse_expression_side(
      expression = rhs_expression,
      prefix_parts = prefix_parts,
      side_multiplier = -1,
      fill_position = fill_position,
      fill_value = fill_value
    )

    # Combine the coefficient contributions from LHS and -RHS.
    coefficients <- lhs_parsed$coefficients

    for (variable_name in names(rhs_parsed$coefficients)) {
      coefficients <- add_coefficient(
        coefficients = coefficients,
        variable_name = variable_name,
        value = rhs_parsed$coefficients[variable_name]
      )
    }

    # Remove variables whose combined coefficient is exactly zero.
    coefficients <- coefficients[abs(coefficients) > 0]

    if (length(coefficients) == 0) {
      stop(
        "The Accounting Check contains no variables after parsing.",
        call. = FALSE
      )
    }

    # The parser represents the relation as
    #
    #   sum_i coefficient_i * x_i + constant = 0.
    #
    # Moving the constant to the right gives
    #
    #   sum_i coefficient_i * x_i = -constant.
    constant <- lhs_parsed$constant + rhs_parsed$constant
    rhs_number <- -constant

    is_inequality <- operator_used %in% c("<=", ">=")

    # Slack signs in the full equality representation:
    #   LHS <= RHS  -> LHS - RHS + slack = 0  -> +1
    #   LHS >= RHS  -> LHS - RHS - slack = 0  -> -1
    slack_sign <- if (operator_used == "<=") {
      +1
    } else if (operator_used == ">=") {
      -1
    } else {
      NA_real_
    }

    lhs_rendered <- render_expression_side(
      expression = lhs,
      prefix_parts = prefix_parts,
      fill_position = fill_position,
      fill_value = fill_value
    )

    rhs_rendered <- render_expression_side(
      expression = rhs_expression,
      prefix_parts = prefix_parts,
      fill_position = fill_position,
      fill_value = fill_value
    )

    list(
      sheet = sheet_name,
      row_number = row_number,
      equation_raw = as.character(equation_raw),
      equation_clean = equation_clean,
      equation_expanded = paste0(
        lhs_rendered,
        operator_original,
        rhs_rendered
      ),
      fill_position = fill_position,
      fill_value = fill_value,
      operator_original = operator_original,
      operator_used = operator_used,
      is_inequality = is_inequality,
      slack_sign = slack_sign,
      coefficients = coefficients,
      rhs = rhs_number,
      n_real_variables = length(coefficients)
    )
  }


  # ============================================================
  # 5. READING THE SUBMITTED DATA
  # ============================================================

  # read_time_period_info()
  # -----------------------
  # Locates the input column named "time_period" and identifies the distinct time
  # periods that contain a usable symbol/value observation.
  #
  # Column-name matching is deliberately forgiving: capitalization, spaces,
  # underscores, and other separators are ignored. Thus, for example,
  # "time_period", "Time Period", and "TIME-PERIOD" are treated alike.
  #
  # The periods are returned in the order in which they first occur in data_input.
  read_time_period_info <- function(data_input) {
    if (!is.data.frame(data_input)) {
      stop("data_input must be a data frame.", call. = FALSE)
    }

    if (ncol(data_input) < 4) {
      stop(
        "data_input must contain at least four columns.",
        call. = FALSE
      )
    }

    normalized_column_names <- tolower(
      gsub("[^[:alnum:]]+", "", names(data_input))
    )

    time_period_columns <- which(normalized_column_names == "timeperiod")

    if (length(time_period_columns) != 1) {
      stop(
        paste0(
          "data_input must contain exactly one column named 'time_period'. ",
          "Capitalization and separators such as spaces, underscores, or hyphens ",
          "do not matter."
        ),
        call. = FALSE
      )
    }

    time_period_column <- time_period_columns[1]
    time_period_values <- as.character(data_input[[time_period_column]])
    time_period_values[is.na(time_period_values)] <- ""
    time_period_values <- gsub("\u00A0", " ", time_period_values, fixed = TRUE)
    time_period_values <- trimws(time_period_values)

    # Use the same basic row definition as read_input_values(): a row is usable
    # when it has a non-blank symbol in column 2 and a numerical value in column 4.
    symbols <- normalize_symbol_text(data_input[[2]])
    values_as_text <- as.character(data_input[[4]])
    values_as_text <- gsub(",", ".", values_as_text, fixed = TRUE)
    numerical_values <- suppressWarnings(as.numeric(values_as_text))
    usable_rows <- symbols != "" & !is.na(numerical_values)

    if (any(usable_rows & time_period_values == "")) {
      stop(
        paste0(
          "At least one usable input row has no time_period. Every row with a ",
          "symbol and numerical value must have a time_period."
        ),
        call. = FALSE
      )
    }

    time_periods <- unique(time_period_values[usable_rows])

    if (length(time_periods) == 0) {
      stop(
        "No usable symbol/value observations with a time_period were found.",
        call. = FALSE
      )
    }

    list(
      column_index = time_period_column,
      row_values = time_period_values,
      usable_rows = usable_rows,
      time_periods = time_periods
    )
  }


  # read_input_values()
  # -------------------
  # Extracts the symbol and numerical value from columns 2 and 4 of one
  # time-period subset of data_input. Blank symbols and non-numerical values are
  # removed.
  #
  # Duplicate symbols are allowed only when every duplicate carries the same value
  # up to a very small numerical tolerance. Identical duplicates are collapsed to
  # one row; conflicting duplicates stop the run because the old value would be
  # ambiguous.
  read_input_values <- function(data_input) {
    if (!is.data.frame(data_input)) {
      stop("data_input must be a data frame.", call. = FALSE)
    }

    if (ncol(data_input) < 4) {
      stop(
        "data_input must contain at least four columns.",
        call. = FALSE
      )
    }

    symbols <- normalize_symbol_text(data_input[[2]])

    # Replace decimal commas before converting values to numeric.
    values_as_text <- as.character(data_input[[4]])
    values_as_text <- gsub(",", ".", values_as_text, fixed = TRUE)
    old_values <- suppressWarnings(as.numeric(values_as_text))

    data_values <- tibble::tibble(
      symbol = symbols,
      old_value = old_values
    ) |>
      dplyr::filter(symbol != "", !is.na(old_value))

    duplicate_check <- data_values |>
      dplyr::group_by(symbol) |>
      dplyr::summarise(
        number_of_rows = dplyr::n(),
        minimum_value = min(old_value),
        maximum_value = max(old_value),
        .groups = "drop"
      ) |>
      dplyr::filter(
        number_of_rows > 1,
        abs(maximum_value - minimum_value) > 1e-12
      )

    if (nrow(duplicate_check) > 0) {
      stop(
        paste0(
          "Some symbols occur more than once with different values. ",
          "Resolve these duplicates before reconciliation."
        ),
        call. = FALSE
      )
    }

    data_values |>
      dplyr::group_by(symbol) |>
      dplyr::summarise(
        old_value = dplyr::first(old_value),
        .groups = "drop"
      )
  }


  # ============================================================
  # 6. READING AND EXPANDING THE EQUATION WORKBOOK
  # ============================================================

  # read_equation_instances_from_sheet()
  # ------------------------------------
  # Reads one Excel sheet and creates one row for every equation instance that must
  # be parsed.
  #
  # If a source row contains several fill values, the compact Accounting Check is
  # repeated once for each value. When no Left/Mid/Right value is supplied, the
  # Accounting Check is used as written.
  read_equation_instances_from_sheet <- function(equation_file, sheet_name) {
    raw_sheet <- readxl::read_excel(
      equation_file,
      sheet = sheet_name,
      col_types = "text",
      .name_repair = "minimal"
    )

    if (ncol(raw_sheet) == 0 || nrow(raw_sheet) == 0) {
      return(tibble::tibble())
    }

    normalized_headers <- normalize_header_text(names(raw_sheet))

    accounting_columns <- which(normalized_headers == "accounting check")

    if (length(accounting_columns) != 1) {
      stop(
        paste0(
          "Sheet '", sheet_name,
          "' must contain exactly one column named 'Accounting Check'."
        ),
        call. = FALSE
      )
    }

    accounting_column <- accounting_columns[1]
    placement_columns <- which(
      normalized_headers %in% c("left", "mid", "right")
    )

    placement_names <- c(
      left = "Left",
      mid = "Mid",
      right = "Right"
    )

    equation_instances <- list()

    for (row_index in seq_len(nrow(raw_sheet))) {
      equation_raw <- raw_sheet[[accounting_column]][row_index]

      if (is.na(equation_raw) || trimws(as.character(equation_raw)) == "") {
        next
      }

      instance_created <- FALSE

      for (placement_column in placement_columns) {
        placement_key <- normalized_headers[placement_column]
        placement_name <- unname(placement_names[placement_key])

        fill_values <- split_fill_values(
          raw_sheet[[placement_column]][row_index]
        )

        if (length(fill_values) == 0) {
          next
        }

        for (fill_value in fill_values) {
          equation_instances[[length(equation_instances) + 1]] <-
            tibble::tibble(
              sheet = sheet_name,
              # Excel row 1 contains the header, so data row i is Excel row i + 1.
              row_number = row_index + 1L,
              equation_raw = as.character(equation_raw),
              fill_position = placement_name,
              fill_value = fill_value
            )

          instance_created <- TRUE
        }
      }

      # A row without any Left/Mid/Right fill is still a valid equation instance.
      if (!instance_created) {
        equation_instances[[length(equation_instances) + 1]] <-
          tibble::tibble(
            sheet = sheet_name,
            row_number = row_index + 1L,
            equation_raw = as.character(equation_raw),
            fill_position = NA_character_,
            fill_value = NA_character_
          )
      }
    }

    dplyr::bind_rows(equation_instances)
  }


  # read_all_equation_instances()
  # -----------------------------
  # Reads every sheet in the equation workbook and combines all expanded source
  # rows into one table. No fixed number or fixed names of sheets are assumed.
  read_all_equation_instances <- function(equation_file) {
    selected_sheets <- readxl::excel_sheets(equation_file)

    if (length(selected_sheets) == 0) {
      stop("The equation workbook contains no sheets.", call. = FALSE)
    }

    raw_equations <- dplyr::bind_rows(
      lapply(
        selected_sheets,
        function(sheet_name) {
          read_equation_instances_from_sheet(
            equation_file = equation_file,
            sheet_name = sheet_name
          )
        }
      )
    )

    if (nrow(raw_equations) == 0) {
      stop(
        "No equation instances were found in the equation workbook.",
        call. = FALSE
      )
    }

    list(
      selected_sheets = selected_sheets,
      raw_equations = raw_equations
    )
  }


  # ============================================================
  # 7. SCALES AND WEIGHTS
  # ============================================================

  # make_relative_scales()
  # ----------------------
  # Constructs the positive normalization scales
  #
  #   s_i = max(abs(x_old_i), scale_floor).
  #
  # When scale_floor is NULL, the code chooses
  #
  #   median(abs(non-zero old values)) * 1e-8.
  #
  # If every old value is zero, the fallback floor is 1. The floor prevents zero
  # scales and avoids extreme normalization for observations very close to zero.
  make_relative_scales <- function(x_old, scale_floor = NULL) {
    if (is.null(scale_floor)) {
      nonzero_values <- abs(
        x_old[is.finite(x_old) & abs(x_old) > 0]
      )

      scale_floor <- if (length(nonzero_values) == 0) {
        1
      } else {
        stats::median(nonzero_values) * 1e-8
      }
    }

    if (
      length(scale_floor) != 1 ||
      !is.finite(scale_floor) ||
      scale_floor <= 0
    ) {
      stop("scale_floor must be NULL or one positive finite number.", call. = FALSE)
    }

    scales <- pmax(abs(x_old), scale_floor)
    names(scales) <- names(x_old)
    scales
  }


  # make_weights()
  # --------------
  # Starts every real variable at weight 1 and applies optional matching rules in
  # their listed order. All weights must remain positive and finite so that the
  # objective is strictly convex in the real data adjustments.
  make_weights <- function(variable_names, weight_rules = tibble::tibble()) {
    weights <- rep(1, length(variable_names))
    names(weights) <- variable_names

    if (is.null(weight_rules) || nrow(weight_rules) == 0) {
      return(weights)
    }

    if (!is.data.frame(weight_rules)) {
      stop("weight_rules must be a data frame.", call. = FALSE)
    }

    required_columns <- c("match_type", "pattern", "weight")
    missing_columns <- setdiff(required_columns, names(weight_rules))

    if (length(missing_columns) > 0) {
      stop(
        paste0(
          "weight_rules must contain these columns: ",
          paste(required_columns, collapse = ", "),
          "."
        ),
        call. = FALSE
      )
    }

    for (rule_index in seq_len(nrow(weight_rules))) {
      match_type <- as.character(weight_rules$match_type[rule_index])
      pattern <- as.character(weight_rules$pattern[rule_index])
      new_weight <- as.numeric(weight_rules$weight[rule_index])

      if (
        length(match_type) != 1 ||
        is.na(match_type) ||
        match_type == "" ||
        length(pattern) != 1 ||
        is.na(pattern) ||
        pattern == ""
      ) {
        stop(
          paste0(
            "weight_rules row ", rule_index,
            " must contain a non-empty match_type and pattern."
          ),
          call. = FALSE
        )
      }

      if (
        length(new_weight) != 1 ||
        !is.finite(new_weight) ||
        new_weight <= 0
      ) {
        stop(
          paste0(
            "Invalid weight in weight_rules row ", rule_index,
            ". Every weight must be positive and finite."
          ),
          call. = FALSE
        )
      }

      matched <- if (match_type == "exact") {
        variable_names == pattern
      } else if (match_type == "prefix") {
        startsWith(variable_names, pattern)
      } else if (match_type == "suffix") {
        endsWith(variable_names, pattern)
      } else if (match_type == "contains") {
        grepl(pattern, variable_names, fixed = TRUE)
      } else if (match_type == "regex") {
        grepl(pattern, variable_names, perl = TRUE)
      } else {
        stop(
          paste0(
            "Unknown match_type in weight_rules row ", rule_index,
            ". Use exact, prefix, suffix, contains, or regex."
          ),
          call. = FALSE
        )
      }

      # Later rules deliberately overwrite earlier rules for matching variables.
      weights[matched] <- new_weight
    }

    weights
  }


  # ============================================================
  # 8. BUILDING THE LINEAR SYSTEM
  # ============================================================

  # make_slack_name()
  # -----------------
  # Creates a unique internal name for the slack variable of an inequality. The
  # collision check makes the generated name safe even if a submitted data symbol
  # happens to use a similar naming convention.
  make_slack_name <- function(equation_id, existing_names) {
    base_name <- paste0("__slack_eq_", sprintf("%05d", equation_id))
    candidate <- base_name
    suffix <- 1L

    while (candidate %in% existing_names) {
      candidate <- paste0(base_name, "_", suffix)
      suffix <- suffix + 1L
    }

    candidate
  }


  # build_system()
  # --------------
  # Converts the submitted data and equation workbook into the matrix form
  #
  #   A_full * (x, u)' = h,
  #
  # where x contains real data variables and u contains one slack variable for
  # each accepted inequality.
  #
  # Processing logic:
  #   1. read and expand every equation workbook row;
  #   2. parse the resulting equation instance;
  #   3. accept it only if every required real variable exists in data_input;
  #   4. identify the real variables used by at least one accepted equation;
  #   5. add one slack variable to every accepted inequality;
  #   6. construct a sparse coefficient matrix and metadata tables.
  build_system <- function(data_values, equation_file) {
    equation_info <- read_all_equation_instances(equation_file)

    selected_sheets <- equation_info$selected_sheets
    raw_equations <- equation_info$raw_equations

    # Named vector used for fast checks of whether a required variable is present.
    old_value_lookup <- stats::setNames(
      data_values$old_value,
      data_values$symbol
    )

    accepted_equations <- list()
    skipped_rows <- list()
    expansion_rows <- list()

    for (instance_index in seq_len(nrow(raw_equations))) {
      equation_raw <- raw_equations$equation_raw[instance_index]
      sheet_name <- raw_equations$sheet[instance_index]
      row_number <- raw_equations$row_number[instance_index]
      fill_position <- raw_equations$fill_position[instance_index]
      fill_value <- raw_equations$fill_value[instance_index]

      parsed <- tryCatch(
        parse_equation(
          equation_raw = equation_raw,
          sheet_name = sheet_name,
          row_number = row_number,
          fill_position = fill_position,
          fill_value = fill_value
        ),
        error = function(e) e
      )

      # Parsing errors are documented and skipped rather than stopping the entire
      # run. They remain visible in the expanded-equations and checks workbooks.
      if (inherits(parsed, "error")) {
        reason_text <- paste0("Parse error: ", parsed$message)

        expansion_rows[[length(expansion_rows) + 1]] <- tibble::tibble(
          sheet = sheet_name,
          row_number = row_number,
          equation_raw = equation_raw,
          equation_expanded = NA_character_,
          fill_position = fill_position,
          fill_value = fill_value,
          operator_used = NA_character_,
          rhs = NA_real_,
          variables_expanded = NA_character_,
          parse_status = "parse_error",
          reason = reason_text
        )

        skipped_rows[[length(skipped_rows) + 1]] <- tibble::tibble(
          sheet = sheet_name,
          row_number = row_number,
          equation_raw = equation_raw,
          equation_expanded = NA_character_,
          fill_position = fill_position,
          fill_value = fill_value,
          reason = reason_text
        )

        next
      }

      required_variables <- names(parsed$coefficients)
      missing_variables <- setdiff(
        required_variables,
        names(old_value_lookup)
      )

      # An otherwise valid equation is data-defined, and therefore accepted, only
      # when all of its required real variables are present in the submitted data.
      if (length(missing_variables) > 0) {
        reason_text <- paste0(
          "Missing variables in input data: ",
          paste(missing_variables, collapse = ", ")
        )

        expansion_rows[[length(expansion_rows) + 1]] <- tibble::tibble(
          sheet = sheet_name,
          row_number = row_number,
          equation_raw = equation_raw,
          equation_expanded = parsed$equation_expanded,
          fill_position = fill_position,
          fill_value = fill_value,
          operator_used = parsed$operator_used,
          rhs = parsed$rhs,
          variables_expanded = paste(required_variables, collapse = ", "),
          parse_status = "missing_variables",
          reason = reason_text
        )

        skipped_rows[[length(skipped_rows) + 1]] <- tibble::tibble(
          sheet = sheet_name,
          row_number = row_number,
          equation_raw = equation_raw,
          equation_expanded = parsed$equation_expanded,
          fill_position = fill_position,
          fill_value = fill_value,
          reason = reason_text
        )

        next
      }

      expansion_rows[[length(expansion_rows) + 1]] <- tibble::tibble(
        sheet = sheet_name,
        row_number = row_number,
        equation_raw = equation_raw,
        equation_expanded = parsed$equation_expanded,
        fill_position = fill_position,
        fill_value = fill_value,
        operator_used = parsed$operator_used,
        rhs = parsed$rhs,
        variables_expanded = paste(required_variables, collapse = ", "),
        parse_status = "accepted",
        reason = NA_character_
      )

      accepted_equations[[length(accepted_equations) + 1]] <- parsed
    }

    if (length(accepted_equations) == 0) {
      stop(
        paste0(
          "No equation could be accepted. Check equation syntax and the ",
          "symbols available in data_input."
        ),
        call. = FALSE
      )
    }

    # Only variables used by at least one accepted equation enter the QP.
    real_variable_names <- sort(
      unique(
        unlist(
          lapply(
            accepted_equations,
            function(parsed) names(parsed$coefficients)
          ),
          use.names = FALSE
        )
      )
    )

    # Store the slack name associated with each accepted equation. Equalities keep
    # NA because they require no slack variable.
    slack_names_by_equation <- rep(
      NA_character_,
      length(accepted_equations)
    )

    names_already_used <- real_variable_names

    for (equation_id in seq_along(accepted_equations)) {
      if (accepted_equations[[equation_id]]$is_inequality) {
        slack_name <- make_slack_name(
          equation_id = equation_id,
          existing_names = names_already_used
        )

        slack_names_by_equation[equation_id] <- slack_name
        names_already_used <- c(names_already_used, slack_name)
      }
    }

    slack_names <- slack_names_by_equation[
      !is.na(slack_names_by_equation)
    ]

    all_variable_names <- c(real_variable_names, slack_names)

    # The sparse matrix is built from triplets:
    #   matrix row index, matrix column index, coefficient value.
    matrix_rows <- integer(0)
    matrix_columns <- integer(0)
    matrix_values <- numeric(0)
    h <- numeric(length(accepted_equations))

    equation_metadata <- vector(
      "list",
      length(accepted_equations)
    )
    slack_metadata <- list()

    for (equation_id in seq_along(accepted_equations)) {
      parsed <- accepted_equations[[equation_id]]
      coefficients <- parsed$coefficients
      slack_name <- slack_names_by_equation[equation_id]

      if (!is.na(slack_name)) {
        coefficients <- add_coefficient(
          coefficients = coefficients,
          variable_name = slack_name,
          value = parsed$slack_sign
        )

        slack_meaning <- if (parsed$operator_used == "<=") {
          "RHS minus LHS"
        } else {
          "LHS minus RHS"
        }

        slack_metadata[[length(slack_metadata) + 1]] <- tibble::tibble(
          equation_id = equation_id,
          slack_name = slack_name,
          sheet = parsed$sheet,
          row_number = parsed$row_number,
          equation_expanded = parsed$equation_expanded,
          operator_used = parsed$operator_used,
          slack_sign = parsed$slack_sign,
          slack_meaning = slack_meaning
        )
      }

      matrix_rows <- c(
        matrix_rows,
        rep(equation_id, length(coefficients))
      )
      matrix_columns <- c(
        matrix_columns,
        match(names(coefficients), all_variable_names)
      )
      matrix_values <- c(
        matrix_values,
        as.numeric(coefficients)
      )

      h[equation_id] <- parsed$rhs

      equation_metadata[[equation_id]] <- tibble::tibble(
        equation_id = equation_id,
        sheet = parsed$sheet,
        row_number = parsed$row_number,
        equation_raw = parsed$equation_raw,
        equation_clean = parsed$equation_clean,
        equation_expanded = parsed$equation_expanded,
        fill_position = parsed$fill_position,
        fill_value = parsed$fill_value,
        operator_original = parsed$operator_original,
        operator_used = parsed$operator_used,
        is_inequality = parsed$is_inequality,
        slack_name = slack_name,
        slack_sign = if (is.na(slack_name)) NA_real_ else parsed$slack_sign,
        rhs = parsed$rhs,
        n_real_variables = parsed$n_real_variables
      )
    }

    A_full <- Matrix::sparseMatrix(
      i = matrix_rows,
      j = matrix_columns,
      x = matrix_values,
      dims = c(
        length(accepted_equations),
        length(all_variable_names)
      ),
      dimnames = list(
        paste0("eq_", seq_along(accepted_equations)),
        all_variable_names
      )
    )

    x_old <- as.numeric(old_value_lookup[real_variable_names])
    names(x_old) <- real_variable_names

    # Typed empty tables preserve meaningful column names in output workbooks even
    # when no row was skipped or when no inequality was accepted.
    skipped_equations <- if (length(skipped_rows) == 0) {
      tibble::tibble(
        sheet = character(),
        row_number = integer(),
        equation_raw = character(),
        equation_expanded = character(),
        fill_position = character(),
        fill_value = character(),
        reason = character()
      )
    } else {
      dplyr::bind_rows(skipped_rows)
    }

    slack_metadata_table <- if (length(slack_metadata) == 0) {
      tibble::tibble(
        equation_id = integer(),
        slack_name = character(),
        sheet = character(),
        row_number = integer(),
        equation_expanded = character(),
        operator_used = character(),
        slack_sign = numeric(),
        slack_meaning = character()
      )
    } else {
      dplyr::bind_rows(slack_metadata)
    }

    list(
      A_full = A_full,
      h = h,
      x_old = x_old,
      real_variable_names = real_variable_names,
      slack_names = slack_names,
      equation_metadata = dplyr::bind_rows(equation_metadata),
      slack_metadata = slack_metadata_table,
      skipped_equations = skipped_equations,
      expanded_equations = dplyr::bind_rows(expansion_rows),
      selected_sheets = selected_sheets,
      raw_equations = raw_equations
    )
  }


  # ============================================================
  # 9. CONSTRAINT CHECKS AND OSQP SOLUTION
  # ============================================================

  # constraint_violation()
  # ----------------------
  # Converts LHS - RHS into the non-negative violation of the original relation.
  # Slack notation is deliberately ignored here.
  #
  # Equality:     violation = abs(LHS - RHS)
  # LHS <= RHS:   violation = max(LHS - RHS, 0)
  # LHS >= RHS:   violation = max(RHS - LHS, 0)
  constraint_violation <- function(lhs_minus_rhs, operator_used) {
    violation <- rep(NA_real_, length(lhs_minus_rhs))

    equality_rows <- operator_used == "="
    less_equal_rows <- operator_used == "<="
    greater_equal_rows <- operator_used == ">="

    violation[equality_rows] <- abs(lhs_minus_rhs[equality_rows])
    violation[less_equal_rows] <- pmax(
      lhs_minus_rhs[less_equal_rows],
      0
    )
    violation[greater_equal_rows] <- pmax(
      -lhs_minus_rhs[greater_equal_rows],
      0
    )

    violation
  }


  # solve_with_osqp()
  # -----------------
  # Builds and solves the OSQP problem in normalized adjustment variables.
  #
  # Define
  #
  #   y_i = (x_i - x_old_i) / s_i,
  #
  # so that
  #
  #   x = x_old + S y.
  #
  # If A_real contains the coefficients of the real data variables and A_slack
  # contains the slack coefficients, the accepted accounting relations become
  #
  #   A_real S y + A_slack u = h - A_real x_old.
  #
  # OSQP solves
  #
  #   minimize  0.5 * y' W y
  #
  # subject to the equality above and u >= 0.
  #
  # The QP decision vector is theta = (y, u). Slack variables receive zero entries
  # in the quadratic objective because they are margins, not adjustment costs.
  solve_with_osqp <- function(
    system,
    weights,
    scales,
    osqp_eps_abs,
    osqp_eps_rel,
    osqp_max_iter,
    osqp_verbose,
    check_tolerance
  ) {
    A_full <- system$A_full
    h <- system$h
    x_old <- system$x_old

    real_variable_names <- system$real_variable_names
    slack_names <- system$slack_names

    number_of_real_variables <- length(real_variable_names)
    number_of_slacks <- length(slack_names)
    number_of_qp_variables <- number_of_real_variables + number_of_slacks

    # Separate the real-variable columns from the slack columns.
    A_real <- A_full[, real_variable_names, drop = FALSE]

    # Required correction at equation level:
    #   r = h - A_real x_old.
    correction_target <- as.numeric(h - A_real %*% x_old)

    # Replace x - x_old by S y. Multiplication by the diagonal scale matrix is
    # implemented as sparse matrix multiplication.
    A_y <- A_real %*% Matrix::Diagonal(x = as.numeric(scales))
    colnames(A_y) <- real_variable_names

    if (number_of_slacks > 0) {
      A_slack <- A_full[, slack_names, drop = FALSE]
      accounting_constraint_matrix <- cbind(A_y, A_slack)
    } else {
      # The same OSQP formulation remains valid when all accepted relations happen
      # to be equalities and therefore no slack columns exist.
      accounting_constraint_matrix <- A_y
    }

    qp_variable_names <- c(real_variable_names, slack_names)
    colnames(accounting_constraint_matrix) <- qp_variable_names

    # OSQP represents linear constraints as lower <= A theta <= upper.
    # Setting lower = upper imposes an exact equality.
    lower_bounds <- correction_target
    upper_bounds <- correction_target
    osqp_constraint_matrix <- accounting_constraint_matrix

    if (number_of_slacks > 0) {
      # Add one identity row for every slack variable. Together with bounds
      # 0 <= slack <= Inf, these rows impose slack non-negativity.
      slack_bound_matrix <- Matrix::sparseMatrix(
        i = seq_len(number_of_slacks),
        j = number_of_real_variables + seq_len(number_of_slacks),
        x = 1,
        dims = c(number_of_slacks, number_of_qp_variables),
        dimnames = list(slack_names, qp_variable_names)
      )

      osqp_constraint_matrix <- rbind(
        osqp_constraint_matrix,
        slack_bound_matrix
      )
      lower_bounds <- c(lower_bounds, rep(0, number_of_slacks))
      upper_bounds <- c(upper_bounds, rep(Inf, number_of_slacks))
    }

    # The quadratic objective is
    #
    #   0.5 * theta' P theta + q' theta.
    #
    # P contains the real-variable weights followed by zeros for the slacks.
    # q is zero because the objective has no linear term.
    P_diagonal <- c(
      as.numeric(weights),
      rep(0, number_of_slacks)
    )

    nonzero_diagonal_positions <- which(P_diagonal != 0)

    P <- Matrix::sparseMatrix(
      i = nonzero_diagonal_positions,
      j = nonzero_diagonal_positions,
      x = P_diagonal[nonzero_diagonal_positions],
      dims = c(number_of_qp_variables, number_of_qp_variables),
      dimnames = list(qp_variable_names, qp_variable_names)
    )

    q <- rep(0, number_of_qp_variables)

    settings <- osqp::osqpSettings(
      verbose = osqp_verbose,
      eps_abs = osqp_eps_abs,
      eps_rel = osqp_eps_rel,
      max_iter = as.integer(osqp_max_iter),
      polishing = TRUE,
      polish_refine_iter = 10000L
    )

    osqp_result <- osqp::solve_osqp(
      P = P,
      q = q,
      A = osqp_constraint_matrix,
      l = as.numeric(lower_bounds),
      u = as.numeric(upper_bounds),
      pars = settings
    )

    solver_status <- osqp_result$info$status
    solver_status_lower <- tolower(solver_status)

    # A status beginning with "solved" includes both "solved" and
    # "solved inaccurate". Other statuses, including an iteration-limit status,
    # do not certify that both feasibility and optimality stopping tests passed.
    if (!grepl("^solved", solver_status_lower)) {
      stop(
        paste0(
          "OSQP did not return a converged solution. Status: ",
          solver_status
        ),
        call. = FALSE
      )
    }

    if (grepl("inaccurate", solver_status_lower)) {
      warning(
        paste0(
          "OSQP returned status '", solver_status,
          "'. The solution will still be checked against check_tolerance."
        ),
        call. = FALSE
      )
    }

    candidate_available <- !is.null(osqp_result$x) &&
      length(osqp_result$x) == number_of_qp_variables &&
      all(is.finite(osqp_result$x))

    if (!candidate_available) {
      stop(
        paste0(
          "OSQP did not return a usable solution vector. Status: ",
          solver_status
        ),
        call. = FALSE
      )
    }

    # OSQP returns theta = (y, u). The name theta_solution avoids confusion with
    # OSQP's separate internal projection variable often denoted by z.
    theta_solution <- as.numeric(osqp_result$x)
    names(theta_solution) <- qp_variable_names

    y_new <- theta_solution[real_variable_names]

    # Transform normalized adjustments back to reconciled data values.
    x_new <- x_old + as.numeric(scales) * y_new
    names(x_new) <- real_variable_names

    slack_values <- if (number_of_slacks > 0) {
      theta_solution[slack_names]
    } else {
      numeric(0)
    }

    adjustment <- x_new - x_old
    names(adjustment) <- real_variable_names

    # Original LHS - RHS values without slack notation. These values are used to
    # assess the original economic/accounting relations directly.
    old_lhs_minus_rhs <- as.numeric(A_real %*% x_old - h)
    new_lhs_minus_rhs <- as.numeric(A_real %*% x_new - h)

    # Full residual of the equality representation including slacks:
    #   A_full * (x_new, slack_values)' - h.
    full_solution <- c(x_new, slack_values)
    names(full_solution) <- c(real_variable_names, slack_names)

    new_residual_with_slack <- as.numeric(
      A_full %*% full_solution - h
    )

    operator_used <- system$equation_metadata$operator_used

    old_violation <- constraint_violation(
      lhs_minus_rhs = old_lhs_minus_rhs,
      operator_used = operator_used
    )
    new_violation <- constraint_violation(
      lhs_minus_rhs = new_lhs_minus_rhs,
      operator_used = operator_used
    )

    maximum_new_violation <- max(new_violation, na.rm = TRUE)
    maximum_full_residual <- max(
      abs(new_residual_with_slack),
      na.rm = TRUE
    )

    # This independent post-solve check is performed in the original accounting
    # units. Both conditions must pass for the candidate to be accepted as the
    # final reconciliation:
    #   1. the original equations and inequalities are satisfied;
    #   2. the equality representation including slacks is algebraically coherent.
    #
    # A failed check does not discard the candidate. The function returns it with
    # final_checks_passed = FALSE so that the diagnostic workbooks can still be
    # written and the problematic equations can be inspected.
    final_checks_passed <-
      maximum_new_violation <= check_tolerance &&
      maximum_full_residual <= check_tolerance

    final_check_message <- if (final_checks_passed) {
      paste0(
        "Final checks passed. Maximum original-relation violation: ",
        signif(maximum_new_violation, 6),
        "; maximum full residual including slack: ",
        signif(maximum_full_residual, 6),
        "."
      )
    } else {
      paste0(
        "Final checks failed. OSQP status: '", solver_status, "'. ",
        "Maximum original-relation violation: ",
        signif(maximum_new_violation, 6),
        "; maximum full residual including slack: ",
        signif(maximum_full_residual, 6),
        ". The output values are a diagnostic OSQP candidate and must not be ",
        "treated as an accepted reconciliation. See the 'failed_checks' sheet ",
        "in the equation-check workbook for the affected rows."
      )
    }

    objective_value <- 0.5 * sum(
      weights * ((x_new - x_old) / scales)^2
    )

    list(
      solver_used = "OSQP",
      solver_status = solver_status,
      osqp_info = osqp_result$info,
      final_checks_passed = final_checks_passed,
      final_check_message = final_check_message,
      check_tolerance = check_tolerance,
      x_old = x_old,
      x_new = x_new,
      slack_values = slack_values,
      adjustment = adjustment,
      weights = weights,
      scales = scales,
      old_lhs_minus_rhs = old_lhs_minus_rhs,
      new_lhs_minus_rhs = new_lhs_minus_rhs,
      old_violation = old_violation,
      new_violation = new_violation,
      new_residual_with_slack = new_residual_with_slack,
      maximum_constraint_violation_old = max(old_violation, na.rm = TRUE),
      maximum_constraint_violation_new = maximum_new_violation,
      maximum_abs_residual_with_slack = maximum_full_residual,
      objective_value = objective_value
    )
  }


  # ============================================================
  # 10. OUTPUT HELPERS
  # ============================================================

  # make_safe_sheet_name()
  # ----------------------
  # Converts a source sheet name into a valid, unique Excel sheet name. Excel does
  # not allow several special characters and limits sheet names to 31 characters.
  make_safe_sheet_name <- function(sheet_name, existing_names = character()) {
    safe_name <- gsub("[][*?:/\\\\]", "_", sheet_name)
    safe_name <- substr(safe_name, 1, 31)

    # A sheet name must contain at least one visible character.
    if (is.na(safe_name) || trimws(safe_name) == "") {
      safe_name <- "Sheet"
    }

    base_name <- safe_name
    suffix_number <- 1L

    while (safe_name %in% existing_names) {
      suffix <- paste0("_", suffix_number)
      safe_name <- paste0(
        substr(base_name, 1, 31 - nchar(suffix)),
        suffix
      )
      suffix_number <- suffix_number + 1L
    }

    safe_name
  }


  # write_expanded_equations_workbook()
  # -----------------------------------
  # Creates one workbook with the same sheet structure as the input equation file.
  # It retains the original source columns and adds the fully expanded equation,
  # the variables used, the parse status, and any reason for skipping the row.
  #
  # The diagnostics from all time periods are stacked in the same source-sheet tab.
  # The time_period and equation_instance_period_id columns identify every row.
  write_expanded_equations_workbook <- function(
    period_results,
    equation_file,
    output_dir,
    run_name
  ) {
    workbook <- openxlsx::createWorkbook()
    existing_sheet_names <- character()
    selected_sheets <- period_results[[1]]$system$selected_sheets

    for (sheet_name in selected_sheets) {
      safe_sheet_name <- make_safe_sheet_name(
        sheet_name = sheet_name,
        existing_names = existing_sheet_names
      )
      existing_sheet_names <- c(
        existing_sheet_names,
        safe_sheet_name
      )

      openxlsx::addWorksheet(workbook, safe_sheet_name)

      original_sheet <- readxl::read_excel(
        equation_file,
        sheet = sheet_name,
        col_types = "text",
        .name_repair = "unique"
      ) |>
        tibble::as_tibble()

      original_sheet <- tibble::add_column(
        original_sheet,
        excel_row_number = seq_len(nrow(original_sheet)) + 1L,
        .before = 1
      )

      expanded_sheet <- dplyr::bind_rows(
        lapply(
          seq_along(period_results),
          function(period_index) {
            period_result <- period_results[[period_index]]
            period_label <- period_result$time_period

            period_result$system$expanded_equations |>
              dplyr::mutate(
                period_order = period_index,
                time_period = period_label,
                equation_instance_period_id = paste0(
                  period_label,
                  " | instance_",
                  sprintf("%05d", dplyr::row_number())
                )
              ) |>
              dplyr::filter(sheet == sheet_name) |>
              dplyr::transmute(
                period_order = period_order,
                time_period = time_period,
                equation_instance_period_id = equation_instance_period_id,
                excel_row_number = row_number,
                fill_position_used = fill_position,
                fill_value_used = fill_value,
                accounting_check_expanded = equation_expanded,
                operator_used = operator_used,
                rhs = rhs,
                variables_expanded = variables_expanded,
                parse_status = parse_status,
                reason = reason
              )
          }
        )
      )

      output_sheet <- original_sheet |>
        dplyr::left_join(
          expanded_sheet,
          by = "excel_row_number"
        ) |>
        dplyr::arrange(excel_row_number, period_order) |>
        dplyr::select(-period_order) |>
        dplyr::relocate(
          time_period,
          equation_instance_period_id,
          .after = excel_row_number
        )

      openxlsx::writeData(
        workbook,
        safe_sheet_name,
        output_sheet
      )
    }

    expanded_file <- file.path(
      output_dir,
      paste0(run_name, "_expanded_equations.xlsx")
    )

    openxlsx::saveWorkbook(
      workbook,
      expanded_file,
      overwrite = TRUE
    )

    expanded_file
  }


  # write_one_old_variable_residuals_workbook()
  # -------------------------------------------
  # Creates the additional diagnostic workbook requested for equation tracing.
  #
  # The workbook contains one sheet per time period. Every sheet retains the same
  # table structure as before:
  #   - rows    : all real variables that entered that period's optimization;
  #   - columns : all accepted expanded equations used in that optimization;
  #   - cells   : the residual of the column equation when all variables are set
  #               to their NEW values, except the row variable, which is set back
  #               to its OLD value.
  #
  # The residual is always computed in the original accounting units as
  #
  #   LHS - RHS.
  #
  # Interpretation:
  #   equality (=) : 0 means the equation is satisfied;
  #   <= equation : positive values mean a violation, negative values mean margin;
  #   >= equation : negative values mean a violation, positive values mean margin.
  #
  # Slack variables are deliberately not included. The purpose is to see which
  # original accounting equation would become problematic if one real data
  # variable were not revised.
  write_one_old_variable_residuals_workbook <- function(
    period_results,
    output_dir,
    run_name
  ) {
    workbook <- openxlsx::createWorkbook()
    existing_sheet_names <- character()

    for (period_result in period_results) {
      time_period <- period_result$time_period
      system <- period_result$system
      solution <- period_result$solution

      real_variable_names <- system$real_variable_names
      equation_texts <- system$equation_metadata$equation_expanded
      number_of_variables <- length(real_variable_names)
      number_of_equations <- length(equation_texts)

      # Excel limits: 1,048,576 rows and 16,384 columns.
      # The table uses one header row and one first column for variable names.
      if (number_of_variables + 1L > 1048576L) {
        stop(
          paste0(
            "The one-old-variable residual table for time period '",
            time_period,
            "' has too many rows for Excel: ",
            number_of_variables + 1L,
            "."
          ),
          call. = FALSE
        )
      }

      if (number_of_equations + 1L > 16384L) {
        stop(
          paste0(
            "The one-old-variable residual table for time period '",
            time_period,
            "' has too many columns for Excel: ",
            number_of_equations + 1L,
            "."
          ),
          call. = FALSE
        )
      }

      # Keep only the real-variable part of the accounting matrix.
      # Slack columns are excluded because the diagnostic asks what happens to the
      # original equation when one real variable is kept at its OLD value.
      A_real <- system$A_full[, real_variable_names, drop = FALSE]

      # Residual with the fully reconciled NEW values:
      #   residual_new = A_real * x_new - h.
      new_residual <- as.numeric(A_real %*% solution$x_new - system$h)

      # For one row variable v, replacing x_new[v] by x_old[v] changes equation e by
      #   coefficient[e, v] * (x_old[v] - x_new[v]).
      # Therefore the mixed residual is
      #   residual_new[e] + coefficient[e, v] * (x_old[v] - x_new[v]).
      old_minus_new <- as.numeric(solution$x_old - solution$x_new)
      names(old_minus_new) <- real_variable_names

      # The residual matrix has variables in rows and equations in columns.
      residual_matrix <- t(as.matrix(A_real))
      residual_matrix <- residual_matrix * old_minus_new
      residual_matrix <- sweep(
        residual_matrix,
        MARGIN = 2,
        STATS = new_residual,
        FUN = "+"
      )

      rownames(residual_matrix) <- real_variable_names
      colnames(residual_matrix) <- equation_texts

      sheet_name <- make_safe_sheet_name(
        sheet_name = time_period,
        existing_names = existing_sheet_names
      )
      existing_sheet_names <- c(existing_sheet_names, sheet_name)
      openxlsx::addWorksheet(workbook, sheet_name)

      # Write the equation texts as plain cells in the first row. This avoids using
      # equation IDs as column names and also preserves duplicate equation texts.
      openxlsx::writeData(
        workbook,
        sheet_name,
        x = matrix(c("variable", equation_texts), nrow = 1),
        startRow = 1,
        startCol = 1,
        colNames = FALSE
      )

      # Write variable names in the first column.
      openxlsx::writeData(
        workbook,
        sheet_name,
        x = matrix(real_variable_names, ncol = 1),
        startRow = 2,
        startCol = 1,
        colNames = FALSE
      )

      # Write the numerical residuals. These cells remain numeric in Excel.
      openxlsx::writeData(
        workbook,
        sheet_name,
        x = residual_matrix,
        startRow = 2,
        startCol = 2,
        rowNames = FALSE,
        colNames = FALSE
      )

      header_style <- openxlsx::createStyle(
        textDecoration = "bold",
        wrapText = TRUE,
        valign = "top"
      )

      openxlsx::addStyle(
        workbook,
        sheet_name,
        style = header_style,
        rows = 1,
        cols = seq_len(number_of_equations + 1L),
        gridExpand = TRUE
      )

      openxlsx::setColWidths(
        workbook,
        sheet_name,
        cols = 1,
        widths = 28
      )

      if (number_of_equations > 0) {
        openxlsx::setColWidths(
          workbook,
          sheet_name,
          cols = 2:(number_of_equations + 1L),
          widths = 45
        )
      }

      openxlsx::freezePane(
        workbook,
        sheet_name,
        firstActiveRow = 2,
        firstActiveCol = 2
      )
    }

    residual_file <- file.path(
      output_dir,
      paste0(run_name, "_one_old_variable_residuals.xlsx")
    )

    openxlsx::saveWorkbook(
      workbook,
      residual_file,
      overwrite = TRUE
    )

    residual_file
  }


  # make_period_output_tables()
  # ---------------------------
  # Builds the same user-facing tables as the original single-period output code,
  # but adds time_period and unique period-specific IDs before the tables from all
  # periods are combined.
  make_period_output_tables <- function(
    data_values,
    system,
    solution,
    run_name,
    time_period,
    check_tolerance
  ) {
    period_label <- as.character(time_period)
    real_variable_names <- system$real_variable_names

    # match() maps each submitted symbol to its position in the optimized real
    # variable vector. Symbols not used by an accepted equation keep their old
    # value and receive NA for weight and scale.
    optimized_position <- match(
      data_values$symbol,
      real_variable_names
    )
    used_flag <- !is.na(optimized_position)

    reconciled_values <- data_values$old_value
    reconciled_values[used_flag] <- solution$x_new[
      optimized_position[used_flag]
    ]

    adjustment_vector <- reconciled_values - data_values$old_value

    output_values <- data_values |>
      dplyr::mutate(
        time_period = period_label,
        variable_period_id = paste(period_label, symbol, sep = " | "),
        used_in_optimization = used_flag,
        new_value = reconciled_values,
        adjustment = adjustment_vector,
        abs_adjustment = abs(adjustment_vector),
        relative_adjustment = dplyr::if_else(
          abs(old_value) > 0,
          adjustment_vector / abs(old_value),
          NA_real_
        ),
        weight = lookup_named_numeric(solution$weights, symbol),
        scale = lookup_named_numeric(solution$scales, symbol)
      )

    # Split the standard three-part symbol into separate output columns.
    symbol_components <- strsplit(
      output_values$symbol,
      ".",
      fixed = TRUE
    )

    get_component <- function(parts, component_number) {
      if (length(parts) >= component_number) {
        parts[component_number]
      } else {
        NA_character_
      }
    }

    output_values$component_1 <- vapply(
      symbol_components,
      get_component,
      character(1),
      component_number = 1
    )
    output_values$component_2 <- vapply(
      symbol_components,
      get_component,
      character(1),
      component_number = 2
    )
    output_values$component_3 <- vapply(
      symbol_components,
      get_component,
      character(1),
      component_number = 3
    )

    output_values <- output_values |>
      dplyr::select(
        time_period,
        variable_period_id,
        symbol,
        component_1,
        component_2,
        component_3,
        old_value,
        new_value,
        adjustment,
        abs_adjustment,
        relative_adjustment,
        weight,
        scale,
        used_in_optimization
      )

    used_variables_only <- output_values |>
      dplyr::filter(used_in_optimization) |>
      dplyr::arrange(dplyr::desc(abs_adjustment))

    weights_and_scales <- tibble::tibble(
      time_period = period_label,
      variable_period_id = paste(
        period_label,
        names(solution$weights),
        sep = " | "
      ),
      symbol = names(solution$weights),
      weight = as.numeric(solution$weights),
      scale = as.numeric(solution$scales)
    ) |>
      dplyr::arrange(symbol)

    slack_table <- system$slack_metadata
    slack_table <- tibble::add_column(
      slack_table,
      time_period = rep(period_label, nrow(slack_table)),
      .before = 1
    )

    if (nrow(slack_table) > 0) {
      slack_table <- slack_table |>
        dplyr::mutate(
          slack_period_id = paste(time_period, slack_name, sep = " | "),
          slack_value = lookup_named_numeric(
            solution$slack_values,
            slack_name
          )
        ) |>
        dplyr::relocate(slack_period_id, .after = time_period)
    } else {
      # Explicit zero-length columns preserve a stable output structure when a
      # period contains no inequalities and therefore no slack variables.
      slack_table <- tibble::add_column(
        slack_table,
        slack_period_id = character(0),
        .after = 1
      )
      slack_table$slack_value <- numeric(0)
    }

    equation_check <- system$equation_metadata |>
      dplyr::mutate(
        time_period = period_label,
        equation_period_id = paste0(
          period_label,
          " | eq_",
          sprintf("%05d", equation_id)
        ),
        lhs_minus_rhs_old = solution$old_lhs_minus_rhs,
        lhs_minus_rhs_new = solution$new_lhs_minus_rhs,
        constraint_violation_old = solution$old_violation,
        constraint_violation_new = solution$new_violation,
        constraint_satisfied_old =
          constraint_violation_old <= check_tolerance,
        constraint_satisfied_new =
          constraint_violation_new <= check_tolerance,
        slack_or_margin_new = dplyr::case_when(
          operator_used == "<=" ~ -lhs_minus_rhs_new,
          operator_used == ">=" ~ lhs_minus_rhs_new,
          TRUE ~ NA_real_
        ),
        slack_value_new = lookup_named_numeric(
          solution$slack_values,
          slack_name
        ),
        constraint_residual_new_with_slack =
          solution$new_residual_with_slack,
        abs_constraint_residual_new_with_slack =
          abs(constraint_residual_new_with_slack),
        full_residual_satisfied_new =
          abs_constraint_residual_new_with_slack <= check_tolerance,
        final_check_failed =
          !constraint_satisfied_new | !full_residual_satisfied_new,
        diagnostic_error = pmax(
          constraint_violation_new,
          abs_constraint_residual_new_with_slack
        )
      ) |>
      dplyr::relocate(time_period, equation_period_id)

    # Collect all rows that fail at least one of the independent final checks.
    # This table is written to a dedicated worksheet so that the largest
    # numerical discrepancies can be inspected immediately.
    failed_checks <- equation_check |>
      dplyr::filter(final_check_failed) |>
      dplyr::arrange(dplyr::desc(diagnostic_error))

    summary_table <- tibble::tibble(
      time_period = period_label,
      item = c(
        "run_name",
        "time_period",
        "solver_used",
        "solver_status",
        "final_checks_passed",
        "final_check_status",
        "check_tolerance",
        "final_check_message",
        "selected_sheets",
        "number_of_selected_sheets",
        "number_of_expanded_equation_instances_from_excel",
        "number_of_used_equations",
        "number_of_skipped_equations",
        "number_of_real_variables",
        "number_of_slack_variables",
        "number_of_distinct_weights",
        "minimum_weight",
        "maximum_weight",
        "number_of_violated_constraints_old",
        "number_of_violated_constraints_new",
        "number_of_failed_final_checks",
        "max_constraint_violation_old",
        "max_constraint_violation_new",
        "max_abs_new_constraint_residual_with_slack",
        "objective_value"
      ),
      value = as.character(
        c(
          run_name,
          period_label,
          solution$solver_used,
          solution$solver_status,
          solution$final_checks_passed,
          ifelse(
            solution$final_checks_passed,
            "accepted",
            "diagnostic candidate - final checks failed"
          ),
          solution$check_tolerance,
          solution$final_check_message,
          paste(system$selected_sheets, collapse = ", "),
          length(system$selected_sheets),
          nrow(system$raw_equations),
          nrow(system$equation_metadata),
          nrow(system$skipped_equations),
          length(system$real_variable_names),
          length(system$slack_names),
          length(unique(solution$weights)),
          min(solution$weights),
          max(solution$weights),
          sum(solution$old_violation > check_tolerance, na.rm = TRUE),
          sum(solution$new_violation > check_tolerance, na.rm = TRUE),
          nrow(failed_checks),
          solution$maximum_constraint_violation_old,
          solution$maximum_constraint_violation_new,
          solution$maximum_abs_residual_with_slack,
          solution$objective_value
        )
      )
    )

    skipped_equations <- tibble::add_column(
      system$skipped_equations,
      time_period = rep(period_label, nrow(system$skipped_equations)),
      .before = 1
    )

    if (nrow(skipped_equations) > 0) {
      skipped_equations <- skipped_equations |>
        dplyr::mutate(
          skipped_equation_period_id = paste0(
            time_period,
            " | skipped_",
            sprintf("%05d", dplyr::row_number())
          )
        ) |>
        dplyr::relocate(skipped_equation_period_id, .after = time_period)
    } else {
      skipped_equations <- tibble::add_column(
        skipped_equations,
        skipped_equation_period_id = character(0),
        .after = 1
      )
    }

    all_equation_instances <- tibble::add_column(
      system$raw_equations,
      time_period = rep(period_label, nrow(system$raw_equations)),
      .before = 1
    ) |>
      dplyr::mutate(
        equation_instance_period_id = paste0(
          time_period,
          " | instance_",
          sprintf("%05d", dplyr::row_number())
        )
      ) |>
      dplyr::relocate(equation_instance_period_id, .after = time_period)

    expanded_equations <- tibble::add_column(
      system$expanded_equations,
      time_period = rep(period_label, nrow(system$expanded_equations)),
      .before = 1
    ) |>
      dplyr::mutate(
        equation_instance_period_id = paste0(
          time_period,
          " | instance_",
          sprintf("%05d", dplyr::row_number())
        )
      ) |>
      dplyr::relocate(equation_instance_period_id, .after = time_period)

    list(
      output_values = output_values,
      used_variables_only = used_variables_only,
      weights_and_scales = weights_and_scales,
      slack_table = slack_table,
      equation_check = equation_check,
      failed_checks = failed_checks,
      summary_table = summary_table,
      skipped_equations = skipped_equations,
      all_equation_instances = all_equation_instances,
      expanded_equations = expanded_equations
    )
  }


  # make_empty_adjustment_plot()
  # ----------------------------
  # Creates a readable placeholder panel when relative adjustments cannot be
  # calculated, for example because all corresponding old values are zero.
  make_empty_adjustment_plot <- function(title, message_text) {
    ggplot2::ggplot() +
      ggplot2::annotate(
        "text",
        x = 0,
        y = 0,
        label = message_text,
        size = 4
      ) +
      ggplot2::xlim(-1, 1) +
      ggplot2::ylim(-1, 1) +
      ggplot2::labs(title = title) +
      ggplot2::theme_void() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(face = "bold")
      )
  }


  # write_adjustment_overview_plot()
  # --------------------------------
  # Writes one combined PNG with three complementary views:
  #   1. the largest absolute adjustments, ranked by abs(x_new - x_old);
  #   2. the largest relative adjustments, ranked by
  #        abs((x_new - x_old) / abs(x_old));
  #   3. a heatmap of every input variable and every time period.
  #
  # The heatmap uses adjustment / scale, where scale is the same normalization used
  # in the objective function. This remains meaningful when an old value is zero.
  # To prevent one extreme observation from making every other tile look white,
  # the displayed heatmap values are capped symmetrically at the 99th percentile of
  # their absolute size. The underlying Excel values are never capped.
  write_adjustment_overview_plot <- function(
    output_values,
    time_periods,
    output_dir,
    run_name
  ) {
    nonzero_adjustments <- output_values |>
      dplyr::filter(abs_adjustment > 0)

    if (nrow(nonzero_adjustments) == 0) {
      return(NA_character_)
    }

    # Twenty-five labels per ranking remain readable in a two-panel overview. This
    # number can be changed here without affecting any numerical result.
    number_of_top_adjustments <- 25L

    top_absolute <- nonzero_adjustments |>
      dplyr::arrange(
        dplyr::desc(abs_adjustment),
        time_period,
        symbol
      ) |>
      dplyr::slice_head(n = number_of_top_adjustments) |>
      dplyr::arrange(abs_adjustment) |>
      dplyr::mutate(
        plot_label = factor(
          variable_period_id,
          levels = variable_period_id
        )
      )

    absolute_plot <- ggplot2::ggplot(
      top_absolute,
      ggplot2::aes(
        x = plot_label,
        y = adjustment
      )
    ) +
      ggplot2::geom_col() +
      ggplot2::geom_hline(yintercept = 0) +
      ggplot2::coord_flip() +
      ggplot2::labs(
        title = "Largest absolute adjustments",
        subtitle = "Ranked by absolute size; bars retain the direction of the change",
        x = "Time period | variable",
        y = "x_new - x_old"
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(face = "bold"),
        axis.text.y = ggplot2::element_text(size = 7)
      )

    top_relative <- nonzero_adjustments |>
      dplyr::filter(is.finite(relative_adjustment)) |>
      dplyr::mutate(
        abs_relative_adjustment = abs(relative_adjustment)
      ) |>
      dplyr::arrange(
        dplyr::desc(abs_relative_adjustment),
        time_period,
        symbol
      ) |>
      dplyr::slice_head(n = number_of_top_adjustments) |>
      dplyr::arrange(abs_relative_adjustment) |>
      dplyr::mutate(
        plot_label = factor(
          variable_period_id,
          levels = variable_period_id
        )
      )

    relative_plot <- if (nrow(top_relative) > 0) {
      ggplot2::ggplot(
        top_relative,
        ggplot2::aes(
          x = plot_label,
          y = 100 * relative_adjustment
        )
      ) +
        ggplot2::geom_col() +
        ggplot2::geom_hline(yintercept = 0) +
        ggplot2::coord_flip() +
        ggplot2::labs(
          title = "Largest relative adjustments",
          subtitle = "Relative to the absolute old value; zero old values are excluded",
          x = "Time period | variable",
          y = "100 × (x_new - x_old) / |x_old|"
        ) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
          plot.title = ggplot2::element_text(face = "bold"),
          axis.text.y = ggplot2::element_text(size = 7)
        )
    } else {
      make_empty_adjustment_plot(
        title = "Largest relative adjustments",
        message_text = paste0(
          "No finite relative adjustments are available.\n",
          "Relative change is undefined when the old value is zero."
        )
      )
    }

    heatmap_data <- output_values |>
      dplyr::mutate(
        normalized_adjustment = dplyr::if_else(
          !is.na(scale) & is.finite(scale) & scale > 0,
          adjustment / scale,
          0
        )
      )

    finite_heatmap_values <- heatmap_data$normalized_adjustment[
      is.finite(heatmap_data$normalized_adjustment)
    ]

    maximum_heatmap_value <- max(abs(finite_heatmap_values), na.rm = TRUE)
    percentile_99 <- as.numeric(
      stats::quantile(
        abs(finite_heatmap_values),
        probs = 0.99,
        na.rm = TRUE,
        names = FALSE,
        type = 8
      )
    )

    heatmap_limit <- if (
      is.finite(percentile_99) && percentile_99 > 0
    ) {
      percentile_99
    } else if (
      is.finite(maximum_heatmap_value) && maximum_heatmap_value > 0
    ) {
      maximum_heatmap_value
    } else {
      1
    }

    heatmap_data <- heatmap_data |>
      dplyr::mutate(
        heatmap_value = pmax(
          -heatmap_limit,
          pmin(heatmap_limit, normalized_adjustment)
        ),
        time_period = factor(
          time_period,
          levels = time_periods
        )
      )

    symbol_order <- heatmap_data |>
      dplyr::group_by(symbol) |>
      dplyr::summarise(
        maximum_abs_normalized_adjustment = max(
          abs(normalized_adjustment),
          na.rm = TRUE
        ),
        .groups = "drop"
      ) |>
      dplyr::arrange(maximum_abs_normalized_adjustment, symbol) |>
      dplyr::pull(symbol)

    heatmap_data$symbol <- factor(
      heatmap_data$symbol,
      levels = symbol_order
    )

    heatmap_plot <- ggplot2::ggplot(
      heatmap_data,
      ggplot2::aes(
        x = time_period,
        y = symbol,
        fill = heatmap_value
      )
    ) +
      ggplot2::geom_tile() +
      ggplot2::scale_fill_gradient2(
        low = "#2166AC",
        mid = "white",
        high = "#B2182B",
        midpoint = 0,
        limits = c(-heatmap_limit, heatmap_limit),
        name = paste0(
          "Adjustment / scale\n",
          "capped at 99th percentile"
        )
      ) +
      ggplot2::labs(
        title = "Overview of all variables and all time periods",
        subtitle = paste0(
          "Blue = downward adjustment, red = upward adjustment, white = no change. ",
          "Rows are ordered by their largest adjustment across periods."
        ),
        x = "Time period",
        y = "Variable"
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(face = "bold"),
        panel.grid = ggplot2::element_blank(),
        axis.text.x = ggplot2::element_text(
          angle = 45,
          hjust = 1
        ),
        axis.text.y = ggplot2::element_text(size = 6)
      )

    plot_file <- file.path(
      output_dir,
      paste0(run_name, "_adjustment_overview.png")
    )

    number_of_symbols <- length(symbol_order)
    number_of_periods <- length(time_periods)
    heatmap_height <- max(7, min(30, 4 + 0.14 * number_of_symbols))
    plot_width <- max(16, min(28, 13 + 0.45 * number_of_periods))
    plot_height <- 8 + heatmap_height

    grDevices::png(
      filename = plot_file,
      width = plot_width,
      height = plot_height,
      units = "in",
      res = 200
    )

    tryCatch(
      {
        grid::grid.newpage()
        overview_layout <- grid::grid.layout(
          nrow = 2,
          ncol = 2,
          heights = grid::unit(c(8, heatmap_height), "null"),
          widths = grid::unit(c(1, 1), "null")
        )
        grid::pushViewport(grid::viewport(layout = overview_layout))

        print(
          absolute_plot,
          vp = grid::viewport(
            layout.pos.row = 1,
            layout.pos.col = 1
          )
        )
        print(
          relative_plot,
          vp = grid::viewport(
            layout.pos.row = 1,
            layout.pos.col = 2
          )
        )
        print(
          heatmap_plot,
          vp = grid::viewport(
            layout.pos.row = 2,
            layout.pos.col = 1:2
          )
        )
      },
      finally = {
        grDevices::dev.off()
      }
    )

    plot_file
  }


  # write_outputs()
  # ---------------
  # Combines the period-specific tables and writes the same four Excel workbooks as
  # before. The only graphical output is now the three-panel adjustment overview.
  write_outputs <- function(
    period_results,
    equation_file,
    output_dir,
    run_name,
    check_tolerance
  ) {
    dir.create(
      output_dir,
      showWarnings = FALSE,
      recursive = TRUE
    )

    period_tables <- lapply(
      period_results,
      function(period_result) {
        make_period_output_tables(
          data_values = period_result$data_values,
          system = period_result$system,
          solution = period_result$solution,
          run_name = run_name,
          time_period = period_result$time_period,
          check_tolerance = check_tolerance
        )
      }
    )

    time_periods <- vapply(
      period_results,
      function(period_result) period_result$time_period,
      character(1)
    )

    output_values <- dplyr::bind_rows(
      lapply(period_tables, function(x) x$output_values)
    )
    used_variables_only <- dplyr::bind_rows(
      lapply(period_tables, function(x) x$used_variables_only)
    )
    weights_and_scales <- dplyr::bind_rows(
      lapply(period_tables, function(x) x$weights_and_scales)
    )
    slack_table <- dplyr::bind_rows(
      lapply(period_tables, function(x) x$slack_table)
    )
    equation_check <- dplyr::bind_rows(
      lapply(period_tables, function(x) x$equation_check)
    )
    failed_checks <- dplyr::bind_rows(
      lapply(period_tables, function(x) x$failed_checks)
    )
    skipped_equations <- dplyr::bind_rows(
      lapply(period_tables, function(x) x$skipped_equations)
    )
    all_equation_instances <- dplyr::bind_rows(
      lapply(period_tables, function(x) x$all_equation_instances)
    )
    expanded_equations <- dplyr::bind_rows(
      lapply(period_tables, function(x) x$expanded_equations)
    )

    period_summary_table <- dplyr::bind_rows(
      lapply(period_tables, function(x) x$summary_table)
    )

    final_checks_by_period <- vapply(
      period_results,
      function(period_result) period_result$solution$final_checks_passed,
      logical(1)
    )
    failed_time_periods <- time_periods[!final_checks_by_period]

    overall_summary <- tibble::tibble(
      time_period = "ALL",
      item = c(
        "run_name",
        "number_of_time_periods",
        "time_periods",
        "all_final_checks_passed",
        "number_of_periods_with_failed_final_checks",
        "periods_with_failed_final_checks"
      ),
      value = as.character(
        c(
          run_name,
          length(time_periods),
          paste(time_periods, collapse = ", "),
          all(final_checks_by_period),
          length(failed_time_periods),
          if (
            length(failed_time_periods) == 0
          ) {
            "none"
          } else {
            paste(failed_time_periods, collapse = ", ")
          }
        )
      )
    )

    summary_table <- dplyr::bind_rows(
      overall_summary,
      period_summary_table
    )

    # ----------------------------------------------------------
    # Workbook 1: reconciled values and variable-level output
    # ----------------------------------------------------------
    values_workbook <- openxlsx::createWorkbook()

    openxlsx::addWorksheet(values_workbook, "summary")
    openxlsx::writeData(values_workbook, "summary", summary_table)

    openxlsx::addWorksheet(values_workbook, "all_values")
    openxlsx::writeData(values_workbook, "all_values", output_values)

    openxlsx::addWorksheet(values_workbook, "used_variables_only")
    openxlsx::writeData(
      values_workbook,
      "used_variables_only",
      used_variables_only
    )

    openxlsx::addWorksheet(values_workbook, "weights_and_scales")
    openxlsx::writeData(
      values_workbook,
      "weights_and_scales",
      weights_and_scales
    )

    openxlsx::addWorksheet(values_workbook, "slack_variables")
    openxlsx::writeData(
      values_workbook,
      "slack_variables",
      slack_table
    )

    values_file <- file.path(
      output_dir,
      paste0(run_name, "_reconciled_values.xlsx")
    )

    openxlsx::saveWorkbook(
      values_workbook,
      values_file,
      overwrite = TRUE
    )

    # ----------------------------------------------------------
    # Workbook 2: equation-level checks, grouped by source sheet
    # ----------------------------------------------------------
    checks_workbook <- openxlsx::createWorkbook()

    openxlsx::addWorksheet(checks_workbook, "summary")
    openxlsx::writeData(checks_workbook, "summary", summary_table)

    # Reserve the names of the diagnostic sheets before creating the source-sheet
    # tabs. This prevents a source sheet with the same name from causing a later
    # duplicate-name error in the output workbook.
    existing_sheet_names <- c(
      "summary",
      "failed_checks",
      "skipped_equations",
      "all_equation_instances",
      "expanded_equations",
      "slack_variables"
    )

    openxlsx::addWorksheet(checks_workbook, "failed_checks")
    openxlsx::writeData(
      checks_workbook,
      "failed_checks",
      failed_checks
    )

    selected_sheets <- period_results[[1]]$system$selected_sheets

    for (sheet_name in selected_sheets) {
      safe_sheet_name <- make_safe_sheet_name(
        sheet_name = sheet_name,
        existing_names = existing_sheet_names
      )
      existing_sheet_names <- c(
        existing_sheet_names,
        safe_sheet_name
      )

      openxlsx::addWorksheet(checks_workbook, safe_sheet_name)

      sheet_checks <- equation_check |>
        dplyr::filter(sheet == sheet_name) |>
        dplyr::arrange(
          match(time_period, time_periods),
          row_number,
          equation_id
        )

      openxlsx::writeData(
        checks_workbook,
        safe_sheet_name,
        sheet_checks
      )
    }

    openxlsx::addWorksheet(checks_workbook, "skipped_equations")
    openxlsx::writeData(
      checks_workbook,
      "skipped_equations",
      skipped_equations
    )

    openxlsx::addWorksheet(checks_workbook, "all_equation_instances")
    openxlsx::writeData(
      checks_workbook,
      "all_equation_instances",
      all_equation_instances
    )

    openxlsx::addWorksheet(checks_workbook, "expanded_equations")
    openxlsx::writeData(
      checks_workbook,
      "expanded_equations",
      expanded_equations
    )

    openxlsx::addWorksheet(checks_workbook, "slack_variables")
    openxlsx::writeData(
      checks_workbook,
      "slack_variables",
      slack_table
    )

    checks_file <- file.path(
      output_dir,
      paste0(run_name, "_equation_checks.xlsx")
    )

    openxlsx::saveWorkbook(
      checks_workbook,
      checks_file,
      overwrite = TRUE
    )

    # ----------------------------------------------------------
    # Workbook 3: source workbook with expanded equations added
    # ----------------------------------------------------------
    expanded_file <- write_expanded_equations_workbook(
      period_results = period_results,
      equation_file = equation_file,
      output_dir = output_dir,
      run_name = run_name
    )

    # ----------------------------------------------------------
    # Workbook 4: one-old-variable residual diagnostics
    # ----------------------------------------------------------
    one_old_variable_residuals_file <- write_one_old_variable_residuals_workbook(
      period_results = period_results,
      output_dir = output_dir,
      run_name = run_name
    )

    # ----------------------------------------------------------
    # Plot: absolute ranking, relative ranking, and full heatmap
    # ----------------------------------------------------------
    plot_file <- write_adjustment_overview_plot(
      output_values = output_values,
      time_periods = time_periods,
      output_dir = output_dir,
      run_name = run_name
    )

    list(
      values_file = values_file,
      checks_file = checks_file,
      expanded_file = expanded_file,
      one_old_variable_residuals_file = one_old_variable_residuals_file,
      plot_file = plot_file,
      summary_table = summary_table,
      all_final_checks_passed = all(final_checks_by_period),
      failed_time_periods = failed_time_periods
    )
  }


  # ============================================================
  # 11. MAIN FUNCTION
  # ============================================================

  # run_reconciliation()
  # --------------------
  # Coordinates the complete workflow:
  #   1. validate the user inputs and numerical settings;
  #   2. identify all distinct time periods in data_input;
  #   3. for each time period separately:
  #        a. read the submitted values;
  #        b. read, expand, parse, and accept equation instances;
  #        c. construct scales and weights;
  #        d. solve the quadratic program with OSQP;
  #   4. combine all period-specific results in the same four Excel output files;
  #   5. write the combined adjustment overview;

  run_reconciliation <- function(
    data_input,
    equation_file,
    output_dir,
    run_name = "reconciliation",
    scale_floor = NULL,
    weight_rules = tibble::tibble(),
    osqp_eps_abs = 1e-6,
    osqp_eps_rel = 1e-6,
    osqp_max_iter = 500000L,
    osqp_verbose = FALSE,
    check_tolerance = 1e-5
  ) {
    if (!is.data.frame(data_input)) {
      stop("data_input must be a data frame.", call. = FALSE)
    }

    if (
      length(equation_file) != 1 ||
      is.na(equation_file) ||
      !file.exists(equation_file)
    ) {
      stop(
        "equation_file must be the path to an existing Excel workbook.",
        call. = FALSE
      )
    }

    if (
      length(output_dir) != 1 ||
      is.na(output_dir) ||
      trimws(output_dir) == ""
    ) {
      stop("output_dir must be one non-empty path.", call. = FALSE)
    }

    if (
      length(run_name) != 1 ||
      is.na(run_name) ||
      trimws(run_name) == ""
    ) {
      stop("run_name must be one non-empty text value.", call. = FALSE)
    }

    if (
      length(osqp_eps_abs) != 1 ||
      !is.finite(osqp_eps_abs) ||
      osqp_eps_abs <= 0
    ) {
      stop("osqp_eps_abs must be positive and finite.", call. = FALSE)
    }

    if (
      length(osqp_eps_rel) != 1 ||
      !is.finite(osqp_eps_rel) ||
      osqp_eps_rel < 0
    ) {
      stop("osqp_eps_rel must be non-negative and finite.", call. = FALSE)
    }

    if (
      length(osqp_max_iter) != 1 ||
      !is.finite(osqp_max_iter) ||
      osqp_max_iter < 1
    ) {
      stop("osqp_max_iter must be a positive integer.", call. = FALSE)
    }

    if (
      length(check_tolerance) != 1 ||
      !is.finite(check_tolerance) ||
      check_tolerance <= 0
    ) {
      stop("check_tolerance must be positive and finite.", call. = FALSE)
    }

    time_period_info <- read_time_period_info(data_input)
    time_periods <- time_period_info$time_periods
    period_results <- vector("list", length(time_periods))
    names(period_results) <- time_periods

    for (period_index in seq_along(time_periods)) {
      time_period <- time_periods[period_index]
      message(
        "Reconciling time period ",
        period_index,
        " of ",
        length(time_periods),
        ": ",
        time_period
      )

      period_rows <- time_period_info$row_values == time_period
      period_data_input <- data_input[
        period_rows,
        ,
        drop = FALSE
      ]

      data_values <- read_input_values(period_data_input)

      system <- build_system(
        data_values = data_values,
        equation_file = equation_file
      )

      scales <- make_relative_scales(
        x_old = system$x_old,
        scale_floor = scale_floor
      )

      weights <- make_weights(
        variable_names = names(system$x_old),
        weight_rules = weight_rules
      )

      solution <- solve_with_osqp(
        system = system,
        weights = weights,
        scales = scales,
        osqp_eps_abs = osqp_eps_abs,
        osqp_eps_rel = osqp_eps_rel,
        osqp_max_iter = osqp_max_iter,
        osqp_verbose = osqp_verbose,
        check_tolerance = check_tolerance
      )

      period_results[[period_index]] <- list(
        time_period = time_period,
        data_values = data_values,
        system = system,
        solution = solution
      )
    }

    outputs <- write_outputs(
      period_results = period_results,
      equation_file = equation_file,
      output_dir = output_dir,
      run_name = run_name,
      check_tolerance = check_tolerance
    )

    print(outputs$summary_table)

    if (outputs$all_final_checks_passed) {
      message("Done. The final checks passed for every time period.")
    } else {
      message(
        paste0(
          "Done. Diagnostic output files were written, but the final checks failed ",
          "for these time periods: ",
          paste(outputs$failed_time_periods, collapse = ", "),
          "."
        )
      )
    }

    message("Number of reconciled time periods: ", length(time_periods))
    message("Time periods: ", paste(time_periods, collapse = ", "))
    message("Solver: OSQP")
    message("Reconciled values written to: ", outputs$values_file)
    message("Equation checks written to: ", outputs$checks_file)
    message("Expanded equations written to: ", outputs$expanded_file)
    message(
      "One-old-variable residuals written to: ",
      outputs$one_old_variable_residuals_file
    )

    if (!is.na(outputs$plot_file)) {
      message("Adjustment overview written to: ", outputs$plot_file)
    } else {
      message(
        paste0(
          "No adjustment overview was written because no non-zero adjustment ",
          "was found in any time period."
        )
      )
    }

    # The warning is deliberately issued only after all output files have been
    # saved. It clearly flags failed validations without removing the diagnostic
    # material needed to identify the affected equations and periods.
    if (!outputs$all_final_checks_passed) {
      failed_messages <- vapply(
        period_results[outputs$failed_time_periods],
        function(period_result) {
          paste0(
            period_result$time_period,
            ": ",
            period_result$solution$final_check_message
          )
        },
        character(1)
      )

      warning(
        paste(failed_messages, collapse = "\n"),
        call. = FALSE
      )
    }

    invisible(
      list(
        time_periods = time_periods,
        period_results = period_results,
        outputs = outputs
      )
    )
  }




  run_reconciliation


})
