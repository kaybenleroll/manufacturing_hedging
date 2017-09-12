source("lib.R")

### List of products is as follows:
###
### Prod1: (Iron Ore)
### Prod2: (Coal)
### Prod3: (Scrap Iron)
### Prod4: (Iron Bars)
### Prod5: (CAD/USD)


N   <- 100000
Mu  <- c(0, 0, 0, 0, 0)
vol <- c(0.30, 0.22, 0.21, 0.20, 0.11)

corrmat <- matrix(c( 1.00,  0.50,  0.80,  0.70, -0.60
                   , 0.50,  1.00,  0.50,  0.60, -0.50
                   , 0.80,  0.50,  1.00,  0.70, -0.40
                   , 0.70,  0.60,  0.70,  1.00, -0.50
                   ,-0.60, -0.50, -0.40, -0.50,  1.00)
                 ,ncol = 5, byrow = TRUE)


hedge_cost_calc <- sapply(vol, function(itervol)
    AmericanOption(type          = 'call'
                  ,underlying    = 1
                  ,strike        = 1
                  ,dividendYield = 0
                  ,riskFreeRate  = 0.01
                  ,maturity      = 1
                  ,volatility = itervol)$value
    )




Sigma <- diag(vol) %*% corrmat %*% diag(vol)
print(is.positive.definite(Sigma))

Sigma <- make.positive.definite(Sigma)

fixed_cost <- 196
item_price <- c(162, 68, 64, 542, 0.75)

input_weight <- c(1.5, 0.6, 0.2)



sim_calc <- create_simulation_calc(input_weight     = input_weight
                                  ,fixed_cost       = fixed_cost
                                  ,item_price       = item_price
                                  ,hedge_cost_ratio = hedge_cost_calc
                                  ,price_Mu         = Mu
                                  ,price_Sigma      = Sigma
                                   )


pnl_sim <- pbreplicate(N, sim_calc$mciter_func())

print(ecdf(pnl_sim)(sim_calc$fixed_profit))


plot_dt <- melt(pnl_sim)
setDT(plot_dt)

plot_dt[Var1 == 1, approach := 'Unhedged']
plot_dt[Var1 == 2, approach := 'Option Hedging']


hedge_pdf_plot <- ggplot(data = plot_dt) +
    stat_density(aes(x = value, colour = approach), geom = 'line', position = 'identity', size = 0.4) +
    geom_vline(xintercept = sim_calc$fixed_profit, size = 0.25) +
    scale_x_continuous(labels = scales::dollar, limits = c(-500, 1000)) +
    xlab("Profit") +
    ylab("Probability Density") +
    ggtitle("Plot of Distributions for Hedging Approaches\n(Black line is profit for Linear Hedging)")


hedge_cdf_plot <- ggplot(data = plot_dt[order(Var1, value)][, .(cumlprob = (1:.N / .N), value), by = approach]) +
    geom_line(aes(x = cumlprob, y = value, colour = approach), size = 0.4) +
    geom_hline(yintercept = sim_calc$fixed_profit, size = 0.25) +
    scale_y_continuous(labels = scales::dollar, limits = c(-500, 1000)) +
    xlab("Cumulative Probability") +
    ylab("Profit") +
    ggtitle("Plot of Cumulative Probability Distributions for Hedging Approaches\n(Black Line is profit for Linear Hedging)")

ggsave(hedge_pdf_plot, file = "hedge_pdf_plot.png", height = 10, width = 14)
ggsave(hedge_cdf_plot, file = "hedge_cdf_plot.png", height = 10, width = 14)
