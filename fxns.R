# Description: Define helper functions that will be used in the analysis
# Notation
# G&H: Givens, G. H., & Hoeting, J. A. (2012). Computational statistics. Wiley.

# Notes
# Kalyn - you left off on working on the nonlinear opt. solver which still needs
# to be resolved! the one implemented in R works well but the custom ones
# seem to struggle! we don't converges often times and then
# some times it converges to a bad value...

# steps
# TODO need to figure out how to do the get q10 value based on own
# implementation of a nonlinear optimizer algo.
# for the analaysis I will need to make sure that all the convercen
# values are TURE! There would be a bit of a problem if the
# algo failed to converge

# Helper function that calculates q10 based on a sample set of data,
# note that this is going to be the default implementation using
# the tools in R and not the
# Args
#   d: data.frame of the temperature and soil respiration
#   x0: numeric vector of initial parameter guesses
Rbased_get_q10_fxn <- function(d, x0){


    # TODO this is going to be replaced with custom
    # nls solver, I am slightly worried about what
    # the first guess is.
    model <- nls(y ~ a * exp(b * x), data = d,
                 start = list(a = x0[1], b = x0[2]))

    q10 <- exp(coefficients(model)[["b"]] * 10)
    itt <- model$convInfo$finIter
    conv <- model$convInfo$isConv

    out <- data.frame(a = coefficients(model)[1],
                      b = coefficients(model)[2],
                      "q10" = q10,
                      "conv" = conv,
                      "itt" = itt,
                      "x_mean" = mean(d$x),
                      "y_mean" = mean(d$y))

    return(out)

}


# Do standard bootstrapping with replacement
# Args
#   data: data.frame of the temperature and soil respiration
#   x0: numeric vector of initial parameter guesses
#   N: integer for the number of bootstrap samples to generate
# Return: data.frame of q10 value and information about the nonlinear convergence
standard_bootstrap_fxn <- function(data, x0, N = 1e3){

    rslts <- data.frame()
    itt <- 0
    while(itt < N){

        # Determine the number of observations in
        # the original data frame
        n_obs <- nrow(data)

        # Sample the original data indices in order to
        # generate the pseudo data set.
        indicies <- sample(1:n_obs, replace = TRUE)
        psuedo_data <- data[indicies, ]

        # Get the q10 value and add to the results
        # data frame.
        q10_val <- Rbased_get_q10_fxn(d = psuedo_data, x0 = x0)
        rslts <- bind_rows(rslts, q10_val)
        itt <- itt + 1
    }

    row.names(rslts) <- NULL
    return(rslts)

}


# Non moving block bootstrapping approach
# Args
#   data: data.frame of the temperature and soil respiration
#   x0: numeric vector of initial parameter guesses
#   N: integer for the number of bootstrap samples to generate
#   l: integer length of the boot strap to consider
# Return: data.frame of q10 value and information about the nonlinear convergence
nonmoving_block_bootstrap_fxn <- function(data, x0, N, l){


    n <- nrow(data)
    # Determine the number of blocks to split the data into
    # TODO, not sure what to do if the blocks do line up perfectly
    # for now let's drop the data set that is incomplete
    b <- floor(n / l)
    nn <- b * l
    blocks <- 1:b

    data <- data[1:nn, ]
    data$block <- rep(x = blocks, each = l)

    rslts <- data.frame()
    itt <- 0

    while(itt < N){
        sampled_blocks <- sample(blocks, replace = TRUE)
        psuedo_data <- data.frame()

        for(b in sampled_blocks){
            psuedo_data <- rbind(psuedo_data, data[data$block == b, ])
        }


        # Get the q10 value and add to the results
        # data frame.
        q10_val <- Rbased_get_q10_fxn(d = psuedo_data, x0 = x0)
        rslts <- bind_rows(rslts, q10_val)
        itt <- itt + 1

    }

    row.names(rslts) <- NULL
    return(rslts)


}





# Moving block bootstrapping approach
# Args
#   data: data.frame of the temperature and soil respiration
#   x0: numeric vector of initial parameter guesses
#   N: integer for the number of bootstrap samples to generate
#   l: integer length of the block to consider
# Return: data.frame of q10 value and information about the nonlinear convergence
moving_block_bootstrap_fxn <- function(data, x0, N, l){


    n <- nrow(data)
    # Determine the number of blocks to split the data into
    # TODO, not sure what to do if the blocks do line up perfectly
    # for now let's drop the data set that is incomplete
    blocks <- 1:n

    # Determine the number of blocks to sample
    n_blocks <- ceiling(n/l)


    rslts <- data.frame()
    itt <- 0

    while(itt < N){
        sampled_blocks <- sample(x = 1:n, size = n_blocks, replace = TRUE)
        psuedo_data <- data.frame()

        for(start in sampled_blocks){

            end <- (start+l-1)

            # If the block is going to extend beyond the end of the
            # data frame wrap around to the start of the data set
            if(end > nrow(data)){

                new_end <- abs(end - nrow(data))
                to_use <- rbind(data[start:nrow(data), ], data[1:new_end, ])

            } else {
                to_use <- data[start:end, ]
            }

            psuedo_data <- rbind(psuedo_data, to_use)
        }

        # Get the q10 value and add to the results
        # data frame.
        q10_val <- Rbased_get_q10_fxn(d = psuedo_data, x0 = x0)
        rslts <- bind_rows(rslts, q10_val)
        itt <- itt + 1

    }

    row.names(rslts) <- NULL
    return(rslts)

    }










