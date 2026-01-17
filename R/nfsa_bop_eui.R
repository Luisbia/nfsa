#' @title Extract and Process Balance of Payments (BOP) Data for the Euro Area
#'
#' @description This function extracts quarterly and annual Balance of Payments (BOP) data
#' from Eurostat for the EU INstitutions (EUI), processes it, and saves it to an Excel file.
#' It focuses on transactions related to sectors S1 (Total Economy), S12 (Financial Corporations),
#' and S13 (Government). The function retrieves data for both credits and debits, and performs
#' some data imputation for earlier periods where sectoral data is unavailable.
#'
#' @param output_sel A character string specifying the directory where the output Excel file
#' should be saved.
#'
#' @return A list containing six data frames: REC_S1, REC_S12, REC_S13 (credits for sectors S1,
#' S12, and S13, respectively), and PAY_S1, PAY_S12, PAY_S13 (debits for sectors S1, S12, and
#' S13, respectively).  Each data frame contains time series data with BOP items as columns and
#' time periods as rows. Monetary values are expressed in millions of euros.
#'
#' @details The function uses the `restatapi` package to retrieve data from Eurostat and the
#' `tidyverse` package for data manipulation. It also uses `openxlsx` to write the output to
#' an Excel file. The function performs the following steps:
#' \enumerate{
#'   \item Downloads annual and quarterly BOP data from Eurostat for the Euro Area.
#'   \item Filters the data to include only sectors S1, S12, and S13, and transactions with the
#'         rest of the world.
#'   \item Converts credits and debits to a common representation ('C' and 'D', respectively).
#'   \item Imputes missing sectoral data for earlier periods (1999-Q1 to 2007-Q4) by distributing
#'         total economy (S1) data to sectors S12 and S13 based on their relative shares in later
#'         periods.
#'   \item Pivots the data to create data frames with BOP items as columns and time periods as rows.
#'   \item Converts the monetary values to millions of euros.
#'   \item Saves the resulting data frames to an Excel file in the specified output directory.
#' }
#'
#' @examples
#' \dontrun{
#' # Example usage: Save the output to a directory named "output" in the current working directory.
#' output_dir <- file.path(getwd(), "output")
#' if (!dir.exists(output_dir)) {
#'   dir.create(output_dir)
#' }
#' result <- nfsa_bop_eui(output_dir)
#'
#' # The result is a list of six data frames containing the processed BOP data.
#' # The data is also saved to an Excel file in the specified output directory.
#' }
#'
#' @export
nfsa_bop_eui <- function(output_sel) {
library(restatapi)
library(tidyverse)
library(openxlsx)

## Annual data
eui_a <-get_eurostat_data("bop_euins6_a",
                      filters=list(sectpart = "S1",
                                   partner="WORLD",
                                   geo = "EUI_X_EAI",
                                   stk_flow= c("CRE","DEB")),
                       stringsAsFactors = FALSE) |>
  filter(time >= 1999) |>
  select(ref_sector = sector10,
         bop_item,
         accounting_entry = stk_flow,
         time_period = time,
         obs_value = values) |>
  mutate(accounting_entry = case_when(accounting_entry == "CRE" ~ "C",
                                      accounting_entry == "DEB" ~ "D"),
         ref_sector = if_else(ref_sector == "S12M","S12", ref_sector))

## Quarterly data
eui_q <-get_eurostat_data("bop_euins6_q",
                          filters=list(sectpart = "S1",
                                       partner="WORLD",
                                       geo = "EUI_X_EAI",
                                       stk_flow= c("CRE","DEB")),
                          stringsAsFactors = FALSE) |>
  select(ref_sector = sector10,
         bop_item,
         accounting_entry = stk_flow,
         time_period = time,
         obs_value = values) |>
  mutate(accounting_entry = case_when(accounting_entry == "CRE" ~ "C",
                                      accounting_entry == "DEB" ~ "D"),
         ref_sector = if_else(ref_sector == "S12M","S12", ref_sector))


## Calculate back data. Data by sector is not available 1999-Q1-2007Q4

back_a <- eui_a |>
  filter(time_period <= 2008,
         time_period >=1999,
         ref_sector == "S12")

tmp <- eui_q |>
  select(quarter = time_period) |>
  distinct() |>
  filter(quarter <= "2007-Q4") |>
  mutate(time_period = str_sub(quarter,1,4))

back_a <- left_join(tmp,back_a,join_by(time_period),relationship =
                      "many-to-many") |>
  select(-time_period) |>
  rename(time_period = quarter) |>
  mutate(obs_value = obs_value/4)

back_q <- bind_rows(eui_q,back_a) |>
  filter(time_period <= "2007-Q4") |>
  pivot_wider(names_from = ref_sector,
              values_from = obs_value) |>
  mutate(S13 = S1 -S12) |>
  mutate(S13 = if_else(is.na(S12),S1,S13)) |>
  replace_na(list(S12 = 0)) |>
  pivot_longer(cols = c(S1,S12,S13),
               names_to = "ref_sector",
               values_to = "obs_value") |>
  filter(ref_sector != "S1")

eui_q <- bind_rows(eui_q,back_q) |>
  arrange(time_period,accounting_entry,bop_item,ref_sector)

rec_s1 <- eui_q |>
  filter(ref_sector == "S1" & accounting_entry == "C") |>
  arrange(time_period) |>
  mutate(obs_value = obs_value/1000000) |>
  pivot_wider(names_from = bop_item,
              values_from = obs_value)

rec_s12 <- eui_q |>
  filter(ref_sector == "S12" & accounting_entry == "C") |>
  arrange(time_period) |>
  mutate(obs_value = obs_value/1000000) |>
  pivot_wider(names_from = bop_item,
              values_from = obs_value)

rec_s13 <- eui_q |>
  filter(ref_sector == "S13" & accounting_entry == "C") |>
  arrange(time_period) |>
  mutate(obs_value = obs_value/1000000) |>
  pivot_wider(names_from = bop_item,
              values_from = obs_value)

pay_s1 <- eui_q |>
  filter(ref_sector == "S1" & accounting_entry == "D") |>
  arrange(time_period) |>
  mutate(obs_value = obs_value/1000000) |>
  pivot_wider(names_from = bop_item,
              values_from = obs_value)

pay_s12 <- eui_q |>
  filter(ref_sector == "S12" & accounting_entry == "D") |>
  arrange(time_period) |>
  mutate(obs_value = obs_value/1000000) |>
  pivot_wider(names_from = bop_item,
              values_from = obs_value)

pay_s13 <- eui_q |>
  filter(ref_sector == "S13" & accounting_entry == "D") |>
  arrange(time_period) |>
  mutate(obs_value = obs_value/1000000) |>
  pivot_wider(names_from = bop_item,
              values_from = obs_value)



l <- list(REC_S1 = rec_s1,
          REC_S12 = rec_s12,
          REC_S13 = rec_s13,
          PAY_S1 = pay_s1,
          PAY_S12 = pay_s12,
          PAY_S13 = pay_s13)

write.xlsx(l, file = paste0(output_sel,"/",as.character(format(Sys.time(), "%Y%m%d_%H%M%S")),"_BOP_EUI_input.xlsx"),
           overwrite = TRUE)

cli::cli_alert_success("File created at: {output_sel}/{as.character(format(Sys.time(), '%Y%m%d_%H%M%S'))}_BOP_EUI_input.xlsx")
return(l)
}


