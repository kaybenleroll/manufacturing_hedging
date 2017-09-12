library(shiny)

set.seed(42)

source("lib.R")
source("default_values.R")


shinyServer(function(input, output) {
    sim_params <- reactiveValues(input_weight = default_values$input_weight
                                ,vols         = default_values$vols
                                ,corrs        = default_values$corrs
                                ,plot_data    = NULL
                                ,fixed_profit = NULL
                                ,calc_message = "Click 'Run Simulation' to calculate the distributions"
                                ,params_message = NULL
                                 )

    observeEvent(input$update_params, {
        sim_params$vols <- c(as.numeric(input$input1vol)
                            ,as.numeric(input$input2vol)
                            ,as.numeric(input$input3vol)
                            ,as.numeric(input$output1vol)
                            ,as.numeric(input$fxvol))

        sim_params$input_weight <- c(as.numeric(input$input1wt)
                                    ,as.numeric(input$input2wt)
                                    ,as.numeric(input$input3wt))


        sim_params$corrs <- c(as.numeric(input$i12corr)
                             ,as.numeric(input$i13corr)
                             ,as.numeric(input$i14corr)
                             ,as.numeric(input$i15corr)
                             ,as.numeric(input$i23corr)
                             ,as.numeric(input$i24corr)
                             ,as.numeric(input$i25corr)
                             ,as.numeric(input$i34corr)
                             ,as.numeric(input$i35corr)
                             ,as.numeric(input$i45corr))

        sim_params$params_message <- 'Parameters updated'
        print(sim_params$params_message)
    })

    observeEvent(input$runsim, {
        N <- as.numeric(input$samplecount)

        fixed_cost <- as.numeric(input$fixed_cost)
        item_price <- c(as.numeric(input$input1)
                       ,as.numeric(input$input2)
                       ,as.numeric(input$input3)
                       ,as.numeric(input$output1)
                       ,as.numeric(input$fxprice)
                        )

        print(paste("Generating", N, "iters"))

        sim_params$calc_message <- "Calculating..."

        output_data <- calculate_output(
            N            = N
           ,fixed_cost   = fixed_cost
           ,item_price   = item_price
           ,input_weight = sim_params$input_weight
           ,vols         = sim_params$vols
           ,corrs        = sim_params$corrs
        )

        sim_params$plot_data    <- output_data$simdata
        sim_params$fixed_profit <- output_data$fixed_profit

        sim_params$calc_message <- paste("Calculated", N, "iterations")
        print(sim_params$calc_message)
    })


    output$plot <- renderPlot({
        if(is.null(sim_params$plot_data)) return()

        plot_dt <- melt(sim_params$plot_data)
        setDT(plot_dt)

        plot_dt[Var1 == 1, approach := 'Unhedged']
        plot_dt[Var1 == 2, approach := 'Option Hedging']

        hedge_pdf_plot <- ggplot(data = plot_dt) +
            stat_density(aes(x = value, colour = approach), geom = 'line', position = 'identity', size = 0.4) +
            geom_vline(xintercept = sim_params$fixed_profit, size = 0.25) +
            scale_x_continuous(labels = scales::dollar, limits = c(-500, 1000)) +
            xlab("Profit") +
            ylab("Probability Density") +
            ggtitle("Plot of Distributions for Hedging Approaches\n(Black line is profit for Linear Hedging)")


        hedge_cdf_plot <- ggplot(data = plot_dt[order(Var1, value)][, .(cumlprob = (1:.N / .N), value), by = approach]) +
            geom_line(aes(x = cumlprob, y = value, colour = approach), size = 0.4) +
            geom_hline(yintercept = sim_params$fixed_profit, size = 0.25) +
            scale_y_continuous(labels = scales::dollar, limits = c(-500, 1000)) +
            xlab("Cumulative Probability") +
            ylab("Profit") +
            ggtitle("Plot of Cumulative Probability Distributions for Hedging Approaches\n(Black Line is profit for Linear Hedging)")

        plot_grid(hedge_pdf_plot, hedge_cdf_plot, nrow = 2)
    })

    output$calc_message <- renderText({
        sim_params$calc_message
    })

    output$params_message <- renderText({
        sim_params$params_message
    })
})
