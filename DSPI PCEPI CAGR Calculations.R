## reading and labeling data, converting to a time series
DSPI  <- readxl::read_excel("PCEPI_DSPI_Data.xlsx", sheet = "DSPI", skip=10)
DSPI <- ts(DSPI[,"DSPI"], frequency=12, start=c(1959, 1))
PCE  <- readxl::read_excel("PCEPI_DSPI_Data.xlsx", sheet = "PCE", skip=10)
PCEPI <- ts(PCE[,"PCEPI"], frequency=12, start=c(1959, 1))

## extracting data within a window, taking the mean of data within that window, creating a PCEPI index relative to 2023 average, and calculating index for Dec 2023
PCEPI_2023 <- window(PCEPI, start=c(2023, 1), end=c(2023, 12))
average_PCEPI_2023 <- mean(PCEPI_2023)
PCEPI_index <- (PCEPI / average_PCEPI_2023) * 100 
value_Dec_2023 <- PCEPI_index[780]
print(value_Dec_2023)

## calculating real DSPI, extracting a subset of real DSPI from 2000 to 2023, extracting real DSPI in Jan 2020
real_DSPI <- DSPI / PCEPI_index * 100
real_DSPI_subset <- window(real_DSPI, start=c(2000, 1), end=c(2023, 12))
real_DSPI_Jan_2020 <- real_DSPI_subset[241]
print(real_DSPI_Jan_2020)

## extracting Real DSPI in Jan 2000 and Dec 2023
real_DSPI_Jan_2000 <- real_DSPI_subset[1]
print(real_DSPI_Jan_2000)
real_DSPI_Dec_2023 <- real_DSPI_subset[288]
print(real_DSPI_Dec_2023)

## creating a sub-series plot of Real DSPI
library(ggplot2)
library(forecast)
ggsubseriesplot(real_DSPI) +
         ylab("real_DSPI") +
         ggtitle("Seasonal sub-series plot:  observation_date")
## visualize the sub-series plot
autoplot(real_DSPI_subset)+
     ggtitle("real DSPI subset.") +
     xlab("observation_date") +
     ylab("real_DSPI_subset")

## calculating monthly growth rates of Real DSPI and plotting
real_DSPI_log_diff <- diff(log(real_DSPI_subset))
real_DSPI_growth_rate <- real_DSPI_log_diff * 100
autoplot(real_DSPI_growth_rate)+
    ggtitle("real DSPI growth rate")+
    xlab("observation_date")+
    ylab("real_DSPI_growth_rate")

## average monthly and annual growth rates of Real DSPI
average_monthly_growth_rate <- mean(real_DSPI_growth_rate)
average_annual_growth_rate <- (exp(average_monthly_growth_rate)-1)*12
## extracting start and end values + sample of subset for CAGR calculation
start_value <- real_DSPI_subset[1]
end_value <- real_DSPI_subset[length(real_DSPI_subset)]
n <- length(real_DSPI_subset)/12

## Compound Annual Growth Rate Calculation of subset
CAGR <- ((end_value / start_value)^(1/n)) - 1