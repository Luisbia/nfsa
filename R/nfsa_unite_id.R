#' Unite columns to create a unique identifier.
#'
#' This function takes a data frame and unites three specified columns
#' (`ref_sector`, `sto`, and `accounting_entry`) into a single column named "id".
#' The columns are joined using a period (".") as a separator.
#'
#' @param data A data frame containing the columns to be united.  Must contain columns named `ref_sector`, `sto`, and `accounting_entry`.
#'
#' @return A data frame with the specified columns united into a new column named "id", and the original columns removed.
#'
#' @examples
#' # Create a sample data frame
#' df <- data.frame(
#'   ref_sector = c("S1", "S2", "S11"),
#'   sto = c("B1G", "P3", "D41"),
#'   accounting_entry = c("B", "D", "C"),
#'   value = c(10, 20, 30)
#' )
#'
#' # Unite the columns
#' result <- nfsa_unite_id(df)
#'
#' # Print the result
#' print(result)
#'
#' @export
nfsa_unite_id <- function(data){
  tmp <- data |>
    tidyr::unite(col = "id",
          c(ref_sector,sto,accounting_entry),
          sep = ".")

  return(tmp)
}
