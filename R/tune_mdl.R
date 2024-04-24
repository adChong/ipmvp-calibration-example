library(here)
library(eplusr)
library(tidyverse)
library(lubridate)

load(here("data", "sim_data.RData"))


datacomp <- param_model$report_data(year = 2013,
                                    tz = "America/Denver") %>%
    mutate(datetime = datetime %m-% months(1),
           value = ifelse(units == "J", value * 2.77778e-7, value),
           units = ifelse(units == "J", "kWh", units)) %>%
    unite(name, name, units, sep = "_") %>%
    select(case, datetime, name, value) %>%
    pivot_wider(
        names_from = name,
        values_from = value
    )




eta <- datacomp$`Electricity:Facility_kWh`
xc <- datacomp %>%
    select(`Site Outdoor Air Drybulb Temperature_C`,
           `Site Outdoor Air Relative Humidity_%`)
tc <- lhs_sample %>%
    slice(rep(row_number(), each = nrow(datacomp)/nrow(lhs_sample)))

y <- actual_electricity$electricity_kwh
xf <- datacomp %>%
    filter(case == "run_param_1") %>%
    select(`Site Outdoor Air Drybulb Temperature_C`,
           `Site Outdoor Air Relative Humidity_%`)

x_pred <- xf # design points for predictions
n_pred <- nrow(x_pred) # number of predictions


n <- nrow(actual_electricity)
m <- nrow(datacomp)
q <- ncol(tc)
p <- ncol(xc)


# standardization of output y and eta
eta_mu <- mean(eta, na.rm = TRUE) # mean value
eta_sd <- sd(eta, na.rm = TRUE) # standard deviation
y <- (y - eta_mu) / eta_sd
eta <- (eta - eta_mu) / eta_sd

# Put design points xf and xc on [0,1]
x <- rbind(as.matrix(xf), as.matrix(xc))
for (i in (1:ncol(x))){
    x_min <- min(x[,i], na.rm = TRUE)
    x_max <- max(x[,i], na.rm = TRUE)
    xf[,i] <- (xf[,i] - x_min) / (x_max - x_min)
    xc[,i] <- (xc[,i] - x_min) / (x_max - x_min)
    x_pred[,i] <- (x_pred[,i] - x_min) / (x_max - x_min)
}

# create data as list for input to Stan
stan_data <- list(n=n, m=m, n_pred=n_pred, p=p, y=y, q=q, eta=eta,
                  xf=as.matrix(xf), xc=as.matrix(xc),
                  x_pred=as.matrix(x_pred), tc=as.matrix(tc))

