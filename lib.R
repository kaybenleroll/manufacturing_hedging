library(ggplot2)
library(scales)
library(data.table)
library(MASS)
library(corpcor)
library(RQuantLib)
library(pbapply)
library(cowplot)



calculate_output <- function(N, fixed_cost, item_price, input_weight, vols, corrs) {
    corrmatrix <- create_corr_matrix(corrs)

    price_covar <- create_covar_matrix(vols, corrmatrix)

    hedge_cost_calc <- sapply(vols, function(itervol)
        AmericanOption(type          = 'call'
                      ,underlying    = 1
                      ,strike        = 1
                      ,dividendYield = 0
                      ,riskFreeRate  = 0.01
                      ,maturity      = 1
                      ,volatility = itervol)$value
        )


    sim_calc <- create_simulation_calc(
        input_weight     = input_weight
       ,fixed_cost       = fixed_cost
       ,item_price       = item_price
       ,hedge_cost_ratio = hedge_cost_calc
       ,price_Mu         = rep(0, length(diag(price_covar)))
       ,price_Sigma      = price_covar
    )

    pnl_sim <- list(
        fixed_profit = sim_calc$fixed_profit
       ,simdata      = replicate(N, sim_calc$mciter_func())
    )

    pnl_sim
}


generate_price_simulations <- function(N = 10000, Mu, Sigma) {
    price_return <- 1 + MASS::mvrnorm(N, Mu, Sigma)

    return(price_return)
}


create_simulation_calc <- function(input_weight, fixed_cost, item_price, hedge_cost_ratio, price_Mu, price_Sigma) {
    stopifnot(length(item_price) == length(price_Mu))
    stopifnot(length(item_price) == length(hedge_cost_ratio) | length(hedge_cost_ratio) == 1)

    N <- length(item_price)


    if(length(hedge_cost_ratio) == 1) hedge_cost_ratio <- rep(hedge_cost_ratio, N)


    ### Calculate cost of hedging and hedged PnL
    init_fx_price     <- item_price[N]
    init_output_price <- item_price[N-1]


    fx_hedge_amount <- init_output_price - (input_weight %*% item_price[1:(N-2)])[1,1]

    ### Hedge cost is in non-base currency
    hedge_cost <- (input_weight %*% (item_price[1:(N-2)] * hedge_cost_ratio[1:(N-2)]))[1,1] +
                  init_output_price * hedge_cost_ratio[N-1] +
                  fx_hedge_amount * hedge_cost_ratio[N]

    hedge_cost <- hedge_cost / init_fx_price



    ### Calculate the fixed PnL
    fixed_input_cost <- fixed_cost + ((input_weight %*% item_price[1:(N-2)])[1,1] / init_fx_price)
    fixed_profit     <- round(((init_output_price / init_fx_price) - fixed_input_cost), 2)


    pnl_calc <- function(verbose_output = FALSE) {
        ### First we generate prices
        price_returns <- 1 + MASS::mvrnorm(1, price_Mu, price_Sigma)

        new_price <- price_returns * item_price


        fx_price     <- new_price[N]
        output_price <- new_price[N-1]


        ### Calculate unhedged PnL
        input_cost <- fixed_cost + ((input_weight %*% new_price[1:(N-2)])[1,1] / fx_price)

        unhedged_profit <- round(((output_price / fx_price) - input_cost), 2)


        ### Calculate hedged PnL
        hedged_price        <- pmin(item_price[1:(N-2)], new_price[1:(N-2)])
        hedged_output_price <- pmax(item_price[N-1],     new_price[N-1])
        hedged_fx_price     <- pmin(item_price[N],       new_price[N])

        hedged_input_cost <- fixed_cost + hedge_cost + ((input_weight %*% hedged_price)[1,1] / hedged_fx_price)
        hedged_profit     <- round(((hedged_output_price / hedged_fx_price) - hedged_input_cost), 2)

        output_data <- c(unhedged_profit, hedged_profit)

        return(output_data)
    }

    return(list(mciter_func  = pnl_calc
               ,fixed_profit = fixed_profit
                ))
}


create_corr_matrix <- function(corrs) {
    ## Dirty hack for the moment
    stopifnot(length(corrs) == 10)

    diagmat <- diag(rep(1, 5))

    zeromat <- matrix(rep(0,5*5), ncol = 5)

    zeromat[1,2:5] <- corrs[1:4]
    zeromat[2,3:5] <- corrs[5:7]
    zeromat[3,4:5] <- corrs[8:9]
    zeromat[4,5]   <- corrs[10]

    corrmat <- diagmat + zeromat + t(zeromat)

    return(corrmat)
}


create_covar_matrix <- function(vol, corrmat) {
    Sigma <- diag(vol) %*% corrmat %*% diag(vol)

    Sigma <- make.positive.definite(Sigma)

    return(Sigma)
}
