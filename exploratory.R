# Description: Extract the meta data about the various soil respiration
# time series.

# 0. Set Up -------------------------------------------------------------------
# Packages that are going to be helpful
library(cosore)
library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)

# Define the preferred plotting aesthetics
theme_set(theme_bw())


# Define some helper functions we can use to extract information about the
# datasets to help make decisions about which time series to try and process


# Categorize by latitude band
# Args
#   x: numeric value referring to the lat band
# Return: str indicating the latitude band
lat_band <- function(x){

    # Mapping dataframe of the lat bands to the lat band name
    binds <- data.frame(lower = c(0, 23.5, 35, 50, 66.5),
                        upper = c(23.5, 35, 50, 66.5, 90),
                        band = c("tropical", "subtropical", "temperate",
                                 "boreal", "polar"))
    n <- nrow(binds)
    id <- numeric(n)

    for(i in 1:n){

        # determine which band the dataset belongs to
        id[i] <- binds$lower[i] < abs(x) &  abs(x) <  binds$upper[i]

    }


    out <- binds$band[which(id == 1)]
    return(out)

}


# Helper function to extract extracts the meta data information
# for each of the data sets
# Args
#   f: str full file path to the csv file
#   meta_data: data.frame return by the corse helper function
# Return: data.frame of the meta data for each of the data sets
get_meta_data <- function(file, meta_data = csr_database()){

    # parse out the file name
    name <- gsub(pattern = ".csv", replacement = "", x = basename(file))
    name_csv <- basename(file)

    # read the data set
    d <- read.csv(file)

    d %>%
        select(Date, y = CSR_FLUX_CO2, x = LST_Day) %>%
        na.omit  %>%
        distinct() ->
        dd

    # Check to see if negative respiration is detected and if that is the
    # case then replace the value as NA
    neg_res <- any(dd$y < 0)

    dd %>%
        mutate(y = ifelse(y < 0, NA, y)) %>%
        na.omit ->
        dd

    # Store the data as either 0 or 1
    duplicate_id <- as.integer(any(duplicated(dd$Date)))

    # The number of observations
    nobs <- nrow(dd)

    # Save some informaiton about the years of data we have the number of years of data we have
    yrs <- paste0(range(year(dd$Date)), collapse = "-")
    n_yrs <- round(nobs/365, 1)

    # Look up information about where the data was collected from
    type <- meta_data$CSR_IGBP[meta_data$CSR_DATASET == name]
    lat <- meta_data$CSR_LATITUDE[meta_data$CSR_DATASET == name]
    band <- lat_band(lat)

    # Save some information about the autocorrealtion
    ccf_values <- ccf(dd$x, dd$y, plot = FALSE)
    max_corr <- max(abs(ccf_values$acf))
    min_corr <- min(abs(ccf_values$acf))
    lag_at_max <- ccf_values$lag[which.max(abs(ccf_values$acf))]
    lag_at_min <- ccf_values$lag[which.min(abs(ccf_values$acf))]

    # Based on the data come up with an initial best guess estimate
    # that can be used in the nonlinear optimizer
    max_id <- which.max(dd$x)
    min_id <- which.min(dd$x)
    a_init <- mean(dd$y)
    b_init  <- log(dd$y[max_id]/ dd$y[min_id]) / (dd$x[max_id] - dd$y[min_id])

    # Format output to return
    out <- data.frame(name, name_csv, yrs, n_yrs, type, band, lat,
                      duplicate = duplicate_id,
                      neg_res = neg_res,
                      max_corr = max_corr,
                      lag_at_max = lag_at_max,
                      min_corr = min_corr,
                      lag_at_min = lag_at_min,
                      a_init = a_init,
                      b_init = b_init)

}


# 1. Main Chunk ----------------------------------------------------------------
# Find all of the files
here::here("data", "raw-data") %>%
    list.files(pattern = "csv", full.names = TRUE) ->
    files

# Extract meta data and arrange by the number of years, also come up with
# an initial best guess for the parameter values that will be optimized.
lapply(files, FUN = get_meta_data) %>%
    do.call(what = "rbind") %>%
    arrange(desc(n_yrs)) ->
    my_meta_data

write.csv(my_meta_data, "meta_data.csv", row.names = FALSE)

# 2. Preview Data ---------------------------------------------------------------
# How many of the time series have negative value?
my_meta_data %>%
    filter(neg_res) %>%
    dim()
# It looks like there are 20 time series that have negative values
# Which is questionable...

# Subset the meta data so that it only contains the values
# that are strictly positive.
my_meta_data %>%
    filter(!neg_res) ->
    good_data


# Let's take a look at some of the summary results related to the
# auto correlation
good_data %>%
    select(band, max_corr, min_corr, lag_at_max, lag_at_min) %>%
    pivot_longer(-band) %>%
    summarise(min = min(value),
              mean = mean(value),
              max = max(value), .by = c("band", "name"))

# It looks like it varies a bit but is generally around a month long...


# Let's use the initial guesses to take a look at q10
good_data$q10 <- exp(good_data$b_init * 10)

summary(good_data$q10)
# wow it looks like there are some est. q10 values that are WAY OUTSIDE
# of what we were expecting...


# Okay so it looks like
good_data %>%
    filter(q10 < 20)  %>%
    pull(q10) %>%
    summary()

# okay what is up wiht the data that has a wild q10 estiamte! okay it looks
# like we were not selecting from the correct spot...
here::here("data", "raw-data", "d20190617_SCOTT_WKG.csv") %>%
    read.csv() %>%
    select(Date, y = CSR_FLUX_CO2, x = LST_Day) %>%
    na.omit ->
    dd

ggplot(data = dd) +
    geom_point(aes(x, y))


# Okay so it looks like we've got some normal looking q10 values!
# although it looks like there might be some errors that are propping up
# places.
good_data %>%
    filter(q10 > 20)

# Determine the data to process
write.csv(good_data, "meta_data-to_process.csv", row.names = FALSE)

