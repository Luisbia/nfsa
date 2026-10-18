
#' Detect Outliers in National Financial Sector Accounts (NFSA)
#'
#' @description
#' Identifies potentially anomalous observations in NFSA time series
#' using three sequential criteria: growth rates, changes relative
#' to GDP, and revisions compared with the previous data vintage.
#'
#' For observations meeting all three criteria, the function generates
#' a multi-page PDF containing time-series charts comparing the
#' current and previous vintages, with identified outliers highlighted.
#'
#' @details
#' The outlier detection procedure consists of three stages:
#'
#' 1. **Growth-rate criterion:** Calculates the percentage change
#'    in each series relative to the observation `type` periods
#'    earlier. Observations are retained when the absolute
#'    percentage change exceeds `rate_limit`.
#'
#' 2. **GDP criterion:** Calculates the absolute change in the
#'    observation as a percentage of GDP (identified by
#'    `"S1.B1GQ.B"`). Observations are retained when the absolute
#'    percentage exceeds `gdp_limit`.
#'
#' 3. **Vintage revision criterion:** Compares the observation
#'    with the corresponding value from the previous data vintage.
#'    Observations are retained when the absolute percentage
#'    revision exceeds `change_prev`, or when the revision
#'    cannot be calculated (e.g., no previous observation).
#'
#' All criteria use strict inequalities (`>`).
#'
#' Before applying these criteria, selected balancing items
#' and transactions are excluded using predefined ID patterns.
#' These include B101, B9, D43, P5M, D8, B11, B12, B6G,
#' B8G, B7G, and B5G.
#'
#' For each combination of country and transaction (`sto`),
#' a time-series chart is generated using:
#'
#' - Separate line types for current and previous vintages.
#' - Different colours for institutional sectors.
#' - Facets by accounting entry.
#' - Dark red points highlighting the identified outliers.
#'
#' Charts are saved in a timestamped, multi-page PDF file.
#'
#' @param country_sel Character vector. Country code for which
#'   outliers should be identified (e.g., `"LU"`).
#'
#' @param type Integer. Number of periods used to calculate
#'   percentage changes. The default is `4L`, corresponding
#'   to year-on-year changes for quarterly data. Use `1L`
#'   for quarter-on-quarter changes.
#'
#' @param time_min Character scalar. Earliest reporting period
#'   to include, expressed in `"YYYY-Qn"` format.
#'   Default is `"1999-Q1"`.
#'
#' @param rate_limit Numeric scalar. Minimum absolute percentage
#'   growth rate required for an observation to be considered
#'   a potential outlier. Default is `30` (30 percent).
#'
#' @param gdp_limit Numeric scalar. Minimum absolute change,
#'   expressed as a percentage of GDP, required for an
#'   observation to be retained. Default is `0.3`
#'   (0.3 percent of GDP).
#'
#' @param change_prev Numeric scalar. Minimum absolute percentage
#'   revision between the current and previous data vintages.
#'   Default is `10` (10 percent).
#'   Observations with missing revision percentages are also
#'   retained.
#'
#' @param output_sel Character scalar. Directory where the
#'   PDF containing outlier charts will be saved.
#'   Defaults to `here::here("output", "plots")`.
#'   The directory must already exist.
#'
#' @return
#' If outliers are identified, a tibble containing the observations
#' satisfying all three detection criteria, including:
#'
#' - Country, transaction identifier, and reporting period.
#' - Current observation value (`obs_value`).
#' - Absolute change relative to the selected lag (`change`).
#' - Percentage growth rate (`growth`).
#' - GDP value (`gdp`).
#' - Change expressed as a percentage of GDP (`as_gdp`).
#' - Additional transaction components created by
#'   [nfsa::nfsa_separate_id()].
#'
#' If no outliers are identified, the function prints
#' `"No outliers according to this criteria"` and returns
#' `NULL` invisibly.
#'
#' @section Output files:
#' When outliers are found, a PDF file is saved in `output_sel`
#' using the following naming convention:
#'
#' `YYYYMMDD_HHMMSS_outliers.pdf`
#'
#' Each page contains one chart comparing the current and
#' previous vintages. A success message displays the file path.
#'
#' @note
#' The function relies on [nfsa::nfsa_get_data()] to retrieve
#' current and previous NFSA vintages. The previous vintage
#' is requested using `type = "prev"`.
#'
#' The initial time filter is applied before calculating growth
#' rates, so the first `type` observations of each time series
#' generally cannot be evaluated.
#'
#' @examples
#' \dontrun{
#' # Detect outliers for Luxembourg using default thresholds
#' outliers <- nfsa_outliers(country_sel = "LU")
#'
#'
#' # Use quarter-on-quarter changes and custom thresholds
#' outliers <- nfsa_outliers(
#'   country_sel = "LU",
#'   type = 1L,
#'   time_min = "2010-Q1",
#'   rate_limit = 20,
#'   gdp_limit = 0.2,
#'   change_prev = 5
#' )
#'
#' # Specify an alternative output directory
#' outliers <- nfsa_outliers(
#'   country_sel = "LU",
#'   output_sel = here::here("reports", "outliers")
#' )
#' }
#'
#' @seealso
#' [nfsa::nfsa_get_data()],
#' [nfsa::nfsa_separate_id()],
#' [nfsa::nfsa_theme()]
#'
#' @export
nfsa_outliers <- function(country_sel,
                          type = 4L,
                          time_min = "2010-Q1",
                          rate_limit = 30,
                          gdp_limit = 0.3,
                          change_prev = 10,
                          output_sel = here::here("output","plots")) {

  dat_o <- nfsa::nfsa_get_data(country = country_sel) |>
   filter(time_period >= time_min) |>
   filter(!str_detect(id,".B101.|.B9.|.D43.|.P5M.|.D8.|.B11.|.B12.|.B6G.|.B8G.|.B7G.|.B5G."))

  gdp_lookup <- dat_o |>
  filter(id == "S1.B1GQ.B") |>
  select(ref_area,time_period, gdp = obs_value)

  dat <- dat_o |>
  group_by(ref_area,id) |>
  arrange(time_period,.by_group = TRUE) |>
  mutate(change = obs_value - lag(obs_value,type)) |>
  mutate(growth = round(change*100/lag(obs_value,type),1)) |>
  filter(abs(growth)> rate_limit) |>
  left_join(y= gdp_lookup,by = join_by(ref_area, time_period)) |>
  mutate(as_gdp = round(change*100/gdp,1)) |>
  filter(abs(as_gdp) > gdp_limit)

  ## Get id of transactions
  trans <- unique(dat$id)

  ## Isolate values
  obs <- dat |>
    select(ref_area,id,time_period) |>
    mutate(obs = "X")

  ## Filter time series
dat_filtered <- dat_o |>
  filter (id %in% trans)

## Get previous vintage
dat_prev <- nfsa::nfsa_get_data(country = country_sel,
                                type = "prev") |>
  filter(time_period >= time_min) |>
  filter(id %in% trans) |>
  rename(prev = obs_value)


dat_comp_o <- left_join(dat_filtered, dat_prev,by = join_by(ref_area, id,
                                                          time_period)) |>
  left_join(y = obs,by = join_by(ref_area, id,
                                 time_period)) |>
  filter(obs == "X") |>
  mutate(change = (obs_value - prev)*100/prev) |>
  filter(abs(change)>change_prev | is.na(change)) #is.na to keep last q

dat_comp <- dat_comp_o |>
  select(ref_area,id,time_period) |>
  mutate(obs = "X")

trans_new <- unique(dat_comp$id)
obs_new <- dat_comp |>
  #nfsa_to_excel() |>
  select(ref_area,id,time_period) |>
  mutate(obs = "X")

if (nrow(obs_new) == 0) {
  cli::cli_alert_info("No outliers according to these criteria.")
  return(invisible(NULL))
} else {

dat_chart <- full_join(obs_new,dat_o, by = join_by(ref_area, id, time_period)) |>
  filter(id %in% trans_new) |>
  left_join(dat_prev,by = join_by(ref_area, id, time_period)) |>
  rename(new = obs_value) |>
  pivot_longer(cols = c(new,prev),
              names_to = "source",
              values_to = "obs_value") |>
  mutate(time_period = lubridate::yq(time_period)) |>
  nfsa::nfsa_separate_id() |>
  arrange(ref_area,sto) |>
  nest(.by = c(ref_area, sto))

  charts <- dat_chart |>
    dplyr::transmute(chart = purrr::map2(data, sto, ~ ggplot2::ggplot(.x) +
                     ggplot2::geom_line(ggplot2::aes(time_period, obs_value, colour = ref_sector,linetype = source), linewidth = 0.65) +
                     ggplot2::geom_point(data =.x |> filter(obs == "X"),ggplot2::aes(time_period, obs_value),colour= "darkred")+
                     ggplot2::facet_wrap(~accounting_entry) +
                     nfsa::nfsa_theme()+
                     nfsa::nfsa_scale_colour() +
                     ggplot2::scale_y_continuous(labels = scales::label_number(), position = "right") +
                     ggplot2::ggtitle(paste0(.y)) +
                     ggplot2::ylab("") + ggplot2::xlab("") +
                     ggplot2::theme(axis.ticks.y = ggplot2::element_blank()))) |>
                     tibble::deframe()

  ts <- format(Sys.time(), "%Y%m%d_%H%M%S")
  filename <- paste0(output_sel, "/",ts,"_outliers.pdf")

  ggplot2::ggsave(
                  filename = filename,
                  plot = gridExtra::marrangeGrob(charts, nrow = 1, ncol = 1),
                  width = 15, height = 9
                  )

  cli::cli_alert_success("Charts created: {.file {filename}}")

  dat <- left_join(obs_new,dat,by = join_by(ref_area, id,
                                            time_period)) |>
    select(-obs) |>
    nfsa_separate_id()

    return(dat)

  }
}
