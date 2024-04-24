preprocess_idf <- function(idf, meters, variables) {
    # make sure weather file input is respected
    idf$SimulationControl$Run_Simulation_for_Weather_File_Run_Periods <- "Yes"

    # remove all existing meter and variable outputs
    if (!is.null(idf$`Output:Meter`)) {
        idf$Output_Meter <- NULL
    }

    # remove all existing meter and variable outputs
    if (!is.null(idf$`Output:Table:Monthly`)) {
        idf$`Output:Table:Monthly` <- NULL
    }

    if (!is.null(idf$`Output:Variable`)) {
        idf$Output_Variable <- NULL
    }

    # add meter outputs to get hourly time-series energy consumption
    idf$add(Output_Meter := meters)

    # add variable outputs to get hourly zone air temperature
    idf$add(Output_Variable := variables)

    # make sure the modified model is returned
    return(idf)
}


nmbe <- function(y_predicted, y_measured) {
    n <- length(y_predicted)
    m_bar <- mean(y_predicted)
    error <- sum(y_predicted - y_measured)/n/m_bar*100
    error
}

cvrmse <- function(y_predicted, y_measured) {
    n <- length(y_predicted)
    m_bar <- mean(y_measured)
    diff <- y_predicted - y_measured
    error <- sqrt(sum(diff^2)/n)/m_bar*100
    error
}





