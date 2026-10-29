#' Validate accounting equations using vectorized country-period evaluation
#'
#' @param data_input Data frame with columns: ref_area, id, time_period, obs_value.
#' @param equation_file Path to the Excel workbook containing the accounting
#'   equations. Each worksheet must contain an `Accounting Check` column and
#'   may optionally contain a `Left`, `Mid`, or `Right` fill column.
#' @param threshold Non-negative tolerance used to determine whether an
#'   equation is satisfied. Defaults to 3.
#'
#' @return a data frame with the rules that do not satisfy the equations
#' @importFrom data.table := data.table dcast rbindlist
#' @importFrom readxl excel_sheets read_excel
#' @importFrom stats setNames
#' @export
nfsa_check_equations <- function(
    data_input,
    equation_file = "M:/nas/Rprod/assets/balancing_equations.xlsx",
    threshold = 3
) {

  # ------------------------------------------------------------
  # 1. Validate inputs
  # ------------------------------------------------------------

  if (!is.data.frame(data_input)) {
    stop("data_input must be a data frame.", call. = FALSE)
  }

  required_columns <- c("ref_area", "id", "time_period", "obs_value")
  if (!all(required_columns %in% names(data_input))) {
    stop("data_input must contain: ",
         paste(required_columns, collapse = ", "), call. = FALSE)
  }

  if (!file.exists(equation_file)) {
    stop("The equation workbook does not exist.", call. = FALSE)
  }

  if (length(threshold) != 1L ||
      !is.finite(threshold) ||
      threshold < 0) {
    stop("threshold must be a non-negative finite number.",
         call. = FALSE)
  }


  # ------------------------------------------------------------
  # 2. Helper functions
  # ------------------------------------------------------------

  normalize_text <- function(x) {
    x <- as.character(x)
    x[is.na(x)] <- ""
    x <- gsub("\\u00A0", " ", x, fixed = TRUE)
    trimws(x)
  }

  normalize_symbol <- function(x) {
    x <- normalize_text(x)
    gsub("[[:space:]]+", "", x)
  }

  normalize_formula <- function(x) {
    x <- normalize_text(x)
    x <- gsub("≤", "<=", x, fixed = TRUE)
    x <- gsub("≥", ">=", x, fixed = TRUE)
    x <- gsub("−", "-", x, fixed = TRUE)
    x <- gsub("–", "-", x, fixed = TRUE)
    x <- gsub("—", "-", x, fixed = TRUE)
    gsub("[[:space:]]+", "", x)
  }

  normalize_header <- function(x) {
    x <- tolower(normalize_text(x))
    gsub("[[:space:]]+", " ", x)
  }

  split_components <- function(x) {
    x <- normalize_symbol(x)

    if (x == "") {
      return(character(0))
    }

    parts <- strsplit(x, ".", fixed = TRUE)[[1]]
    parts[parts != ""]
  }

  split_fill_values <- function(x) {
    if (length(x) == 0L || is.na(x) ||
        trimws(as.character(x)) == "") {
      return(character(0))
    }

    x <- as.character(x)
    x <- gsub("\\u00A0", " ", x, fixed = TRUE)
    x <- gsub("[,;\\r\\n\\t]+", " ", x)

    values <- strsplit(trimws(x), "[[:space:]]+")[[1]]
    unique(normalize_symbol(values))
  }

  is_number <- function(x) {
    grepl(
      "^((\\\\d+\\\\.?\\\\d*)|(\\\\.\\\\d+))([eE][+-]?\\\\d+)?$",
      x
    )
  }

  split_terms <- function(x) {
    x <- normalize_formula(x)

    if (x == "") {
      stop("An expression side is empty.")
    }

    if (!grepl("^[+-]", x)) {
      x <- paste0("+", x)
    }

    positions <- gregexpr(
      "(?<![eE])[+-]",
      x,
      perl = TRUE
    )[[1]]

    ends <- c(positions[-1] - 1L, nchar(x))

    substring(
      x,
      first = positions,
      last = ends
    )
  }

  classify_component <- function(x) {
    if (x %in% c("C", "D", "_Z")) {
      return("entry")
    }

    if (grepl("^S[0-9A-Z_]*$", x)) {
      return("sector")
    }

    "transaction"
  }

  complete_key <- function(parts) {
    dimensions <- list(
      sector = character(0),
      transaction = character(0),
      entry = character(0)
    )

    for (part in parts) {
      dimension <- classify_component(part)

      if (length(dimensions[[dimension]]) > 0L) {
        stop("A variable identifier has conflicting components.")
      }

      dimensions[[dimension]] <- part
    }

    if (any(lengths(dimensions) != 1L)) {
      stop("A variable identifier could not be completed.")
    }

    paste(
      dimensions$sector,
      dimensions$transaction,
      dimensions$entry,
      sep = "."
    )
  }

  make_variable_key <- function(
    token,
    prefix_parts = character(0),
    fill_position = NA_character_,
    fill_value = NA_character_
  ) {

    token <- normalize_symbol(token)

    if (token == "" || grepl("[*/^()]", token)) {
      stop("Invalid or unsupported variable token.")
    }

    parts <- split_components(token)

    has_fill <- length(fill_position) == 1L &&
      !is.na(fill_position) &&
      length(fill_value) == 1L &&
      !is.na(fill_value) &&
      fill_position %in% c("Left", "Mid", "Right") &&
      normalize_symbol(fill_value) != ""

    if (has_fill) {
      fill_parts <- split_components(fill_value)

      if (length(fill_parts) == 0L) {
        stop("The fill value is empty.")
      }

      if (fill_position == "Left") {
        parts <- c(fill_parts, parts)
      } else if (fill_position == "Right") {
        parts <- c(parts, fill_parts)
      } else {
        if (length(parts) == 0L) {
          stop("Cannot insert a middle component.")
        }

        parts <- c(parts[1], fill_parts, parts[-1])
      }
    }

    if (length(parts) == 3L) {
      return(paste(parts, collapse = "."))
    }

    if (length(prefix_parts) > 0L) {
      return(complete_key(c(prefix_parts, parts)))
    }

    stop("The variable identifier does not have three components.")
  }

  # ------------------------------------------------------------
  # 3. Parse an equation
  # ------------------------------------------------------------

  parse_equation <- function(
    equation,
    fill_position = NA_character_,
    fill_value = NA_character_
  ) {

    equation_clean <- normalize_formula(equation)

    if (equation_clean == "") {
      stop("The equation is empty.")
    }

    prefix_parts <- character(0)
    equation_body <- equation_clean

    if (grepl(":", equation_clean, fixed = TRUE)) {
      colon_position <- regexpr(
        ":",
        equation_clean,
        fixed = TRUE
      )[1]

      prefix_parts <- split_components(
        substr(equation_clean, 1, colon_position - 1L)
      )

      equation_body <- substr(
        equation_clean,
        colon_position + 1L,
        nchar(equation_clean)
      )
    }

    operators <- c("<=", ">=", "<", ">", "=")

    operator <- operators[
      vapply(
        operators,
        function(op) grepl(op, equation_body, fixed = TRUE),
        logical(1)
      )
    ][1]

    if (is.na(operator)) {
      stop("No valid relation operator was found.")
    }

    sides <- strsplit(
      equation_body,
      operator,
      fixed = TRUE
    )[[1]]

    if (length(sides) != 2L ||
        any(trimws(sides) == "")) {
      stop("The equation must have exactly two non-empty sides.")
    }

    parse_side <- function(expression) {
      terms <- split_terms(expression)
      coefficients <- numeric(0)
      constant <- 0

      for (term in terms) {
        sign <- if (substr(term, 1, 1) == "-") -1 else 1
        body <- substr(term, 2, nchar(term))

        if (body == "") {
          stop("An empty expression term was found.")
        }

        if (is_number(body)) {
          constant <- constant + sign * as.numeric(body)
        } else {
          key <- make_variable_key(
            token = body,
            prefix_parts = prefix_parts,
            fill_position = fill_position,
            fill_value = fill_value
          )

          if (!key %in% names(coefficients)) {
            coefficients[key] <- 0
          }

          coefficients[key] <- coefficients[key] + sign
        }
      }

      list(
        coefficients = coefficients,
        constant = constant
      )
    }

    lhs <- parse_side(sides[1])
    rhs <- parse_side(sides[2])

    coefficients <- lhs$coefficients

    for (key in names(rhs$coefficients)) {
      if (!key %in% names(coefficients)) {
        coefficients[key] <- 0
      }

      coefficients[key] <- coefficients[key] - rhs$coefficients[key]
    }

    coefficients <- coefficients[abs(coefficients) > 0]

    if (length(coefficients) == 0L) {
      stop("The equation contains no variables.")
    }

    list(
      operator = operator,
      coefficients = coefficients,
      constant = lhs$constant - rhs$constant,
      variables = unique(c(names(lhs$coefficients), names(rhs$coefficients))),
      lhs = lhs,
      rhs = rhs
    )
  }

  # 4. Prepare observations without repeated full-data scans -----------------
  area_values <- normalize_text(data_input$ref_area)
  time_values <- normalize_text(data_input$time_period)
  symbols <- normalize_symbol(data_input$id)
  numeric_values <- suppressWarnings(
    as.numeric(gsub(",", ".", as.character(data_input$obs_value), fixed = TRUE))
  )

  usable <- area_values != "" & time_values != "" &
    symbols != "" & is.finite(numeric_values)
  if (!any(usable)) {
    stop("No usable area/time/symbol/value observations were found.")
  }

  dt <- data.table::data.table(
    ref_area = area_values[usable],
    time_period = time_values[usable],
    id = symbols[usable],
    obs_value = numeric_values[usable]
  )

  dt <- dt[,obs_value:= round(obs_value,2)]

  # Same duplicate policy as the original:
  # first value is used; any differing duplicates invalidate the ENTIRE period.
  # .SD[1L] preserves the first input observation for the id.
  distinct_values <- dt[
    , .(
      obs_value = obs_value[1L],
      has_conflict = (max(obs_value) - min(obs_value)) > 1e-12
    ),
    by = .(ref_area, time_period, id)
  ]

  period_conflicts <- distinct_values[
    , .(has_conflict = any(has_conflict)),
    by = .(ref_area, time_period)
  ]

  # Wide lookup means every equation can be evaluated across all periods at once.
  # Missing series become NA and therefore skip a check, matching the original.
  wide <- data.table::dcast(
    distinct_values,
    ref_area + time_period ~ id,
    value.var = "obs_value"
  )
  wide[period_conflicts, has_conflict := i.has_conflict,
       on = .(ref_area, time_period)]

  available_variables <- setdiff(
    names(wide), c("ref_area", "time_period", "has_conflict")
  )

  evaluate_side_vec <- function(side, idx) {
    out <- rep(side$constant, length(idx))
    for (variable in names(side$coefficients)) {
      out <- out + unname(side$coefficients[[variable]]) * wide[[variable]][idx]
    }
    out
  }

  # Expressions are constructed ONLY for failed checks, not every period.
  format_number <- function(x) {
    format(x, scientific = FALSE, trim = TRUE, digits = 1)
  }
  side_values <- function(side, lookup) {
    pieces <- character(0)
    for (variable in names(side$coefficients)) {
      coefficient <- unname(side$coefficients[[variable]])
      if (coefficient == 0) next
      value <- unname(lookup[[variable]])
      displayed_value <- if (value < 0) {
        paste0("(", format_number(value), ")")
      } else {
        format_number(value)
      }
      term <- if (abs(coefficient) == 1) {
        displayed_value
      } else {
        paste0(format_number(abs(coefficient)), "*", displayed_value)
      }
      pieces <- c(pieces, paste0(if (coefficient < 0) "-" else "+", term))
    }
    if (side$constant != 0 || !length(pieces)) {
      pieces <- c(pieces, paste0(if (side$constant < 0) "-" else "+",
                                 format_number(abs(side$constant))))
    }
    expression <- paste0(pieces, collapse = "")
    if (startsWith(expression, "+")) substring(expression, 2L) else expression
  }

  # 5. Read and parse equations once per rule --------------------------------
  sheet_names <- readxl::excel_sheets(equation_file)
  if (!length(sheet_names)) {
    stop("The equation workbook contains no sheets.")
  }

  diagnostics <- list()
  batch <- 0L

  for (sheet_name in sheet_names) {
    source_sheet <- readxl::read_excel(
      equation_file,
      sheet = sheet_name,
      col_types = "text",
      .name_repair = "unique"
    )
    headers <- normalize_header(names(source_sheet))
    accounting_columns <- which(headers == "accounting check")
    if (length(accounting_columns) != 1L) {
      stop(paste0(
        "Sheet '", sheet_name,
        "' must contain exactly one Accounting Check column."
      ))
    }
    accounting_col <- accounting_columns[1L]
    fill_columns <- which(headers %in% c("left", "mid", "right"))

    for (row_index in seq_len(nrow(source_sheet))) {
      equation_raw <- source_sheet[[accounting_col]][row_index]
      if (is.na(equation_raw) || trimws(as.character(equation_raw)) == "") next

      instances <- list()
      for (fill_col in fill_columns) {
        fill_position <- switch(
          headers[fill_col], left = "Left", mid = "Mid", right = "Right"
        )
        fill_values <- split_fill_values(source_sheet[[fill_col]][row_index])
        for (fill_value in fill_values) {
          instances[[length(instances) + 1L]] <- list(
            fill_position = fill_position,
            fill_value = fill_value
          )
        }
      }
      if (!length(instances)) {
        instances <- list(list(
          fill_position = NA_character_, fill_value = NA_character_
        ))
      }

      for (instance in instances) {
        parsed <- tryCatch(
          parse_equation(
            equation = equation_raw,
            fill_position = instance$fill_position,
            fill_value = instance$fill_value
          ),
          error = function(e) e
        )
        if (inherits(parsed, "error")) next

        required <- parsed$variables
        if (!all(required %in% available_variables)) next

        valid <- !wide$has_conflict
        for (v in required) {
          valid <- valid & is.finite(wide[[v]])
        }
        idx <- which(valid)
        if (!length(idx)) next

        lhs_value <- evaluate_side_vec(parsed$lhs, idx)
        rhs_value <- evaluate_side_vec(parsed$rhs, idx)
        difference <- lhs_value - rhs_value
        violation <- switch(
          parsed$operator,
          "=" = abs(difference),
          "<" = pmax(difference, 0),
          "<=" = pmax(difference, 0),
          ">" = pmax(-difference, 0),
          ">=" = pmax(-difference, 0)
        )
        failed <- which(violation > threshold)
        if (!length(failed)) next

        rows <- idx[failed]
        # Keep formatting exactly as in the original function.
        lhs_text <- character(length(rows))
        rhs_text <- character(length(rows))
        for (j in seq_along(rows)) {
          lookup <- setNames(
            lapply(required, function(v) wide[[v]][rows[j]]), required
          )
          lhs_text[j] <- side_values(parsed$lhs, lookup)
          rhs_text[j] <- side_values(parsed$rhs, lookup)
        }

        batch <- batch + 1L
        diagnostics[[batch]] <- data.frame(
          ref_area = wide$ref_area[rows],
          category = sheet_name,
          time_period = wide$time_period[rows],
          accounting_check_original = as.character(equation_raw),
          lhs_value = lhs_value[failed],
          rhs_value = rhs_value[failed],
          lhs_values = lhs_text,
          rhs_values = rhs_text,
          operator = parsed$operator,
          lhs_minus_rhs = difference[failed],
          violation = violation[failed],
          threshold = threshold,
          source_excel_row = row_index + 1L,
          fill_position_used = instance$fill_position,
          fill_value_used = instance$fill_value,
          validation_status = "NOT FULFILLED",
          stringsAsFactors = FALSE
        )
      }
    }
  }

  if (length(diagnostics)) {
    result <- data.table::rbindlist(diagnostics, use.names = TRUE)
    result <- as.data.frame(result)
    result[order(result$ref_area, result$category, result$time_period),
           , drop = FALSE]
  } else {
    # Preserve the existing no-failures return contract exactly.
    data.frame(message = "No failed checks found for any country or category.")
  }
}
