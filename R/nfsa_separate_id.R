#' @title Separate ID Column in NFSA Data
#'
#' @description This function separates the `id` column in an NFSA data frame into three new columns: `ref_area`, `sto`, and `accounting_entry`. The separation is based on the "." delimiter.
#'
#' @param data A data frame containing the `id` column to be separated.
#'
#' @return A data frame with the original `id` column removed and three new columns: `ref_area`, `sto`, and `accounting_entry`.
#' @export
#' @examples
#' # Create a sample data frame
#' df <- data.frame(id = c("REF_SECTOR1.STO1.ENTRY1", "REF_SECTOR2.STO2.ENTRY2"))
#'
#' # Separate the id column
#' separated_df <- nfsa_separate_id(df)
#'
#' # Print the result
#' print(separated_df)
#'
nfsa_separate_id <- function(data){
  tmp <- data |>
    tidyr::separate_wider_delim(cols = id,
                         delim = ".",
                         names = c("ref_sector", "sto", "accounting_entry"))

  return(tmp)
}
