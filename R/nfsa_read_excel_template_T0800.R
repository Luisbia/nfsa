
#' Read excel template where T0800 is estimated
#'
#' @param file
#'
#' @returns a data frame
#' @export
#'
#' @examples
#' nfsa_read_excel_template_T0800("C:/Users/biedmlu/OneDrive - European Commission/NFSA/EE1_2024_F.xlsx")
nfsa_read_excel_template_T0800 <- function (file){
library(tidyverse)
library(readxl)

S1 <- read_xlsx(file,
                sheet = "S1",
                range = "A30:NU100") |>
  select(time_period = `TIME ▼`, P2.D,P3.D,P31.D,P32.D,P5.D,P51G.D,
         P51G_N111G.D, P51G_N112G.D,P51G_N1121G.D,P51G_N1122G.D, P52.D,
         P53.D,,D1.D,D11.D, D12.D,D2.D,D21.D,D29.D,D3.D,D31.D,D39.D,D4.D,D41.D,
         D42.D,D421.D, D422.D,D43.D,D44.D,D441.D, D442.D, D443.D,D45.D,D41G.D,
         D5.D,D51.D,D59.D,D6.D,D61.D,D611.D, D612.D, D613.D, D614.D,D61SC.D,
         D62.D,D63.D,D631.D,D632.D,D7.D,D71.D,D72.D,D74.D,D74_4Y.D,
         D75.D,D76.D,D8.D,D9.D,D91.D,D92.D,D99.D,P51C.D,NP.D,
         P1.C,P11.C, P12.C, P13.C, D1.C,D11.C, D12.C,D2.C,D21.C,D211.C, D212.C, D214.C,
         D29.C,D3.C,D31.C,D39.C,D21X31.C,D4.C,D41.C,D42.C,D421.C, D422.C,
         D43.C,D44.C,D441.C,D442.C,D443.C,D45.C,D41G.C,D5.C,D51.C,D59.C,D6.C,D61.C,
         D611.C, D612.C,D613.C, D614.C,D61SC.D,D62.C,D63.C,D631.C, D632.C,D7.C,D71.C,D72.C,
         D74.C, D75.C,D8.C,D9.C,D91.C,D92.C,D99.C,P51C.C,B1GQ.B,
         B1NQ.B, B2A3G.B, B2G.B,B3G.B, B4G.B, B5G.B, B6G.B, B7G.B,B8G.B, B101.B,
         B9.B, B9X9F._Z) |>
  mutate(across(-time_period,as.numeric)) |>
  mutate(time_period = as.integer(time_period)) |>
  pivot_longer(-time_period,
               names_to = "sto",
               values_to = "obs_value") |>
  mutate(ref_sector = "S1") |>
  separate_wider_delim(sto,
                       delim = ".",
                       names= c("sto","accounting_entry")) |>
  select(time_period,ref_sector,sto,accounting_entry,obs_value) |>
  na.omit()

S1N <- read_xlsx(file,
                 sheet = "S1N",
                 range = "A30:V100") |>
  select(time_period = `TIME ▼`, D2.D, D21.D,D3.C,D31.C,D21X31.C,B1G.B, B1N.B) |>
  mutate(across(-time_period,as.numeric)) |>
  mutate(time_period = as.integer(time_period)) |>
  pivot_longer(-time_period,
               names_to = "sto",
               values_to = "obs_value") |>
  mutate(ref_sector = "S1N") |>
  separate_wider_delim(sto,
                       delim = ".",
                       names= c("sto","accounting_entry")) |>
  select(time_period,ref_sector,sto,accounting_entry,obs_value)|>
  na.omit()

S11 <- read_xlsx(file,
                 sheet = "S11",
                 range = "A30:JH100") |>
  select(time_period = `TIME ▼`, P2.D,P5.D,P51G.D,P51G_N111G.D,
         P51G_N112G.D, P51G_N1121G.D, P51G_N1122G.D, P52.D, P53.D,
         D1.D,D11.D, D12.D,D2.D,D29.D,D4.D,D41.D,
         D42.D,D421.D, D422.D,D43.D,D44.D,D441.D, D442.D, D443.D,D45.D,D41G.D,
         D5.D,D51.D,D59.D,D6.D,D62.D,D7.D,D71.D,D75.D,D8.D,D9.D,D91.D,D99.D,P51C.D,NP.D,
         P1.C,P11.C, P12.C, D3.C,D39.C,D4.C,D41.C,D42.C,D421.C, D422.C,
         D43.C,D44.C,D441.C,D442.C,D443.C,D45.C,D41G.C,D6.C,D61.C,
         D611.C, D612.C,D613.C, D614.C,D61SC.C,D7.C,D72.C,
         D75.C,D9.C,D92.C,D99.C,P51C.C,B1G.B,
         B1N.B, B2A3G.B, B4G.B, B5G.B, B6G.B, B8G.B, B101.B,
         B9.B, B9X9F._Z) |>
  mutate(across(-time_period,as.numeric)) |>
  mutate(time_period = as.integer(time_period)) |>
  pivot_longer(-time_period,
               names_to = "sto",
               values_to = "obs_value") |>
  mutate(ref_sector = "S11") |>
  separate_wider_delim(sto,
                       delim = ".",
                       names= c("sto","accounting_entry")) |>
  select(time_period,ref_sector,sto,accounting_entry,obs_value)|>
  na.omit()

S12 <- read_xlsx(file,
                 sheet = "S12",
                 range = "A30:JN100") |>
  select(time_period = `TIME ▼`, P2.D,P5.D,P51G.D,P51G_N111G.D,
         P51G_N112G.D, P51G_N1121G.D, P51G_N1122G.D, P52.D, P53.D,
         D1.D,D11.D, D12.D,D2.D,D29.D,D4.D,D41.D,
         D42.D,D421.D, D422.D,D43.D,D44.D,D441.D, D442.D, D443.D,D45.D,D41G.D,
         D5.D,D51.D,D59.D,D6.D,D62.D,D7.D,D71.D,D72.D,D75.D,D8.D,D9.D,D91.D,D99.D,P51C.D,NP.D,
         P1.C,P11.C, P12.C, D3.C,D39.C,D4.C,D41.C,D42.C,D421.C, D422.C,
         D43.C,D44.C,D441.C,D442.C,D443.C,D45.C,D41G.C,D6.C,D61.C,
         D611.C, D612.C,D613.C, D614.C,D61SC.C,D7.C,D71.C,D72.C,
         D75.C,D9.C,D92.C,D99.C,P51C.C,B1G.B,
         B1N.B, B2A3G.B, B4G.B, B5G.B, B6G.B, B8G.B, B101.B,
         B9.B, B9X9F._Z) |>
  mutate(across(-time_period,as.numeric)) |>
  mutate(time_period = as.integer(time_period)) |>
  pivot_longer(-time_period,
               names_to = "sto",
               values_to = "obs_value") |>
  mutate(ref_sector = "S12") |>
  separate_wider_delim(sto,
                       delim = ".",
                       names= c("sto","accounting_entry")) |>
  select(time_period,ref_sector,sto,accounting_entry,obs_value)|>
  na.omit()


S13 <- read_xlsx(file,
                 sheet = "S13",
                 range = "A30:LP100") |>
  select(time_period = `TIME ▼`, P2.D,P3.D,P31.D,P32.D,P5.D,P51G.D,
         P51G_N111G.D, P51G_N112G.D,P51G_N1121G.D,P51G_N1122G.D, P52.D,
         P53.D,,D1.D,D11.D, D12.D,D2.D,D29.D,D3.D,D31.D,D39.D,D4.D,D41.D,
         D42.D,D421.D, D44.D,D441.D, D442.D, D443.D,D45.D,D41G.D,
         D5.D,D51.D,D59.D,D6.D,D62.D,D63.D,D631.D,D632.D,D7.D,D71.D,D72.D,D74.D,D74_4Y.D,
         D75.D,D76.D,D8.D,D9.D,D92.D,D99.D,P51C.D,NP.D,OTE.D,
         P1.C,P1O.C,P11.C, P12.C, P13.C, D2.C,D21.C,D211.C, D212.C, D214.C,
         D29.C,D3.C,D39.C,D4.C,D41.C,D42.C,D421.C, D422.C,
         D43.C,D44.C,D441.C,D442.C,D443.C,D45.C,D41G.C,D5.C,D51.C,D59.C,D6.C,D61.C,
         D611.C, D612.C,D613.C, D614.C,D61SC.C,D7.C,D71.C,D72.C,
         D74.C, D75.C,D9.C,D91.C,D92.C,D99.C,P51C.C,OTR.C,B1G.B,
         B1N.B, B2A3G.B, B4G.B, B5G.B, B6G.B, B7G.B,B8G.B, B101.B,
         B9.B, B9X9F._Z) |>
  mutate(across(-time_period,as.numeric)) |>
  mutate(time_period = as.integer(time_period)) |>
  pivot_longer(-time_period,
               names_to = "sto",
               values_to = "obs_value") |>
  mutate(ref_sector = "S13") |>
  separate_wider_delim(sto,
                       delim = ".",
                       names= c("sto","accounting_entry")) |>
  select(time_period,ref_sector,sto,accounting_entry,obs_value)|>
  na.omit()

S1M <- read_xlsx(file,
                 sheet = "S1M",
                 range = "A30:KR100") |>
  select(time_period = `TIME ▼`, P2.D,P3.D,P31.D,P5.D,P51G.D,
         P51G_N111G.D, P51G_N112G.D,P51G_N1121G.D,P51G_N1122G.D, P52.D,
         P53.D,,D1.D,D11.D, D12.D,D2.D,D29.D,D4.D,D41.D,
         D43.D,D44.D,D441.D, D442.D, D443.D,D45.D,D41G.D,
         D5.D,D51.D,D59.D,D6.D,D61.D,D611.D, D612.D, D613.D, D614.D,D61SC.D,
         D62.D,D63.D,D631.D,D632.D,D7.D,D71.D,
         D75.D,D8.D,D9.D,D91.D,D99.D,P51C.D,NP.D,
         P1.C,P11.C, P12.C, P13.C, D1.C,D11.C, D12.C,
         D3.C,D39.C,D4.C,D41.C,D42.C,D421.C, D422.C,
         D43.C,D44.C,D441.C,D442.C,D443.C,D45.C,D41G.C,D6.C,D61.C,
         D611.C, D612.C,D613.C, D614.C,D61SC.D,D62.C,D63.C,D631.C, D632.C,D7.C,D72.C,
         D75.C,D8.C,D9.C,D92.C,D99.C,P51C.C,B1G.B,
         B1N.B, B2A3G.B, B2G.B,B3G.B, B4G.B, B5G.B, B6G.B, B7G.B,B8G.B, B101.B,
         B9.B, B9X9F._Z) |>
  mutate(across(-time_period,as.numeric)) |>
  mutate(time_period = as.integer(time_period)) |>
  pivot_longer(-time_period,
               names_to = "sto",
               values_to = "obs_value") |>
  mutate(ref_sector = "S1M") |>
  separate_wider_delim(sto,
                       delim = ".",
                       names= c("sto","accounting_entry")) |>
  select(time_period,ref_sector,sto,accounting_entry,obs_value)|>
  na.omit()

S2 <- read_xlsx(file,
                 sheet = "S2",
                 range = "A30:KC100") |>
  select(time_period = `TIME ▼`, P6.D,P61.D, P62.D, P62F.D,
         D1.C,D11.C, D12.C,D3.C,D31.C,D39.C,D4.C,D41.C, D42.C, D421.C, D422.C,D43.C,
         D44.C,D441.C, D442.C,D443.C,D45.C,D41G.C,D5.C,D51.C,D59.C,D6.C,
         D61.C,D611.C,D612.C,D613.C,D614.C,D61SC.D,D62.C,D7.C,D71.C,D72.C,D74.C,
         D75.C,D8.C,D9.C,D91.C,D92.C,D99.C,NP.C,
         P7.C,P71.C,P72.C,P72F.C,D1.D,D11.D,D12.D,D2.D,D21.D,D211.D,D212.D,D214.D,D29.D,D4.D,
         D41.D,D42.D,D421.D,D422.D,D43.D,
         D44.D,D441.D, D442.D, D443.D,D45.D,D41G.D,D5.D,D51.D, D59.D,D6.D,D61.D,D611.D, D612.D,D613.D,
         D614.D, D61SC.D,D62.D,
         D7.D,D71.D,D72.D,D74.D,D75.D,D76.D,D8.D,D9.D,D91.D,D92.D,D99.D,B11.B, B12.B,
         B101.B,B9.B,B9X9F._Z) |>
  mutate(across(-time_period,as.numeric)) |>
  mutate(time_period = as.integer(time_period)) |>
  pivot_longer(-time_period,
               names_to = "sto",
               values_to = "obs_value") |>
  mutate(ref_sector = "S2") |>
  separate_wider_delim(sto,
                       delim = ".",
                       names= c("sto","accounting_entry")) |>
  select(time_period,ref_sector,sto,accounting_entry,obs_value)|>
  na.omit()


data <- bind_rows(S1,S1N,S11,S12,S13,S1M,S2)
}
