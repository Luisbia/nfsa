#' Compare the content of different xml files loaded as data frames
#'
#' @param df1 one data.frame
#' @param df2 the other data.frame
#' @param flags if obs_status and conf_status are taken into account. By default false
#' @import dplyr
#'
#' @return a data frame
#' @export nfsa_compare_xml
#'
#'@examples
#'file1 <- nfsa::nfsa_read_xml(file_sel = "M:/Incoming_SDMX_files/NASEC_T0801_Q_FR_2025_0001_V0001.xml",
#'interactive = FALSE)
#'
#'file2 <- nfsa::nfsa_read_xml(file_sel = "M:/Incoming_SDMX_files/NASEC_T0801_Q_FR_2025_0001_V0002.xml",
#'                             interactive = FALSE)
#'
#'tmp <-nfsa_compare_xml(file1,file2)
#'tmp <-nfsa_compare_xml(file1,file2, flags = TRUE)
#'
nfsa_compare_xml <- function(df1, df2, flags = FALSE){
library(dplyr)

  if(flags == FALSE) {
dat1 <- df1 |>
  select(ref_area,sto,ref_sector,accounting_entry,counterpart_area,consolidation,adjustment,time_period, df1=obs_value)

dat2 <- df2 |>
  select(ref_area,sto,ref_sector,accounting_entry,counterpart_area,consolidation,adjustment,time_period, df2=obs_value)

dat <- full_join(dat1, dat2) |>
  group_by(across(c(-df1, -df2))) |>
  mutate(change = df1 - df2) |>
  filter(change != 0) |>
  ungroup()

return(dat)
  } else {
    dat1 <- df1 |>
      select(ref_area,sto,ref_sector,accounting_entry,counterpart_area,consolidation,adjustment,time_period, obs_status, conf_status, df1=obs_value)

    dat2 <- df2 |>
      select(ref_area,sto,ref_sector,accounting_entry,counterpart_area,consolidation,adjustment,time_period, obs_status, conf_status,df2=obs_value)

    dat <- full_join(dat1, dat2) |>
      group_by(across(c(-df1, -df2))) |>
      mutate(change = df1 - df2) |>
      filter(change != 0) |>
      ungroup()


    }

}

