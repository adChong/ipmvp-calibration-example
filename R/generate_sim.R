library(here)
library(eplusr)
library(tidyverse)
library(jsonlite)
library(lubridate)
library(lhs)
source(here("R", "util.R"))


idf_path <- here("data", "seed.idf")
epw_path <- here("data", "AMY_2013.epw")

idf_model <- read_idf(idf_path)



meters <- list(
    key_name = c(
        "Electricity:Facility"
    ),
    Reporting_Frequency = "Monthly"
)

variables <- list(
    key_value = "*",
    Variable_Name = c(
        "Site Outdoor Air Drybulb Temperature",
        "Site Outdoor Air Relative Humidity"
    ),
    Reporting_Frequency = "Monthly"
)

idf_model <- preprocess_idf(idf_model, meters, variables)

idf_model$save(here("data", "seed_preprocessed.idf"), overwrite = TRUE)

# Infiltration: 0 to 100%
# Electric equipment: 0 to 100%

# set seed to ensure repeatability
set.seed(1)

lhs_sample <- maximinLHS(n = 30, k = 2) %>%
    as.data.frame() %>%
    rename(infil_frac = V1, equip_design_frac = V2)

plot(lhs_sample$infil_frac, lhs_sample$equip_design_frac)




param_model <- param_job(idf_model, epw_path)



param_model$apply_measure(run_param,
                          infil_frac = lhs_sample$infil_frac,
                          equip_design_frac = lhs_sample$equip_design_frac)

param_model$run()

param_model$status()

sim_electricity <- param_model$report_data(year = 2013,
                                       tz = "America/Denver") %>%
    mutate(datetime = datetime %m-% months(1),
           year = lubridate::year(datetime),
           month = lubridate::month(datetime, label = TRUE),
           value = ifelse(units == "J", value * 2.77778e-7, value),
           units = ifelse(units == "J", "kWh", units)) %>%
    filter(name == "Electricity:Facility") %>%
    transmute(datetime, case, electricity_kwh = value, year, month, source = "simulated")

actual_electricity <- fromJSON(here("data", "actual_electric.json")) %>%
    as.data.frame() %>%
    mutate(datetime = ymd_hms(`data.from`),
           datetime = force_tz(datetime, tz = "America/Denver"),
           year = lubridate::year(datetime),
           month = lubridate::month(datetime, label = TRUE),
           electricity_kwh = data.tot_kwh,
           case = "actual",
           source = "actual") %>%
    select(datetime, month, year, electricity_kwh, case, source) %>%
    filter(year == 2013)

dat <- bind_rows(sim_electricity, actual_electricity)

ggplot(dat, aes(x = month, y= electricity_kwh, color = source, group = source)) +
    geom_point(size = ifelse(dat$source == "actual", 2, 0.8)) +
    scale_color_brewer(palette = "Set2") +
    labs(x = "Month", y = "Electricity:Facility (kWh)", color = "")
    theme_minimal()


save(param_model, actual_electricity, lhs_sample,
     file = here("data", "sim_data.RData"))






