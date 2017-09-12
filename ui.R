library(shiny)

source("default_values.R")

sidebar_layout <- sidebarPanel(
    helpText("Simulates the profit from inputs, currency and output price - all of which are correlated"),

    numericInput("fixed_cost", "Fixed Costs", default_values$fixed_cost, min = 0, max = 10000, step = 1),

    numericInput("input1",  "Input1 Price", default_values$item_price[1], min = 0, max = 10000, step = 1),
    numericInput("input2",  "Input2 Price", default_values$item_price[2], min = 0, max = 10000, step = 1),
    numericInput("input3",  "Input3 Price", default_values$item_price[3], min = 0, max = 10000, step = 1),

    numericInput("output1", "Output Price", default_values$item_price[4], min = 0, max = 10000, step = 1),

    numericInput("fxprice", "FX Price",     default_values$item_price[5], min = 0, max = 10, step = 0.01),

    numericInput("samplecount", "Number of Simulations", default_values$N, min = 1000, max = 1000000, step = 1000),

    actionButton("runsim", "Run Simulation"),
    width = 2
)


shinyUI(navbarPage("Manufacturing Profit Simulator",
    tabPanel("Profit/Loss Plot",
        sidebarLayout(
            sidebar_layout,
            mainPanel(
                textOutput('calc_message'),
                plotOutput('plot', height = 1200)
            )
        )
    ),

    tabPanel("Parameters",
        sidebarLayout(
            sidebar_layout,
            mainPanel(
                h4("Price Volatilities"),
                tags$table(
                tags$tr(tags$td(numericInput("input1vol",  "Input1 Vol",  default_values$vols[1], min=0, max=10, step=0.01, width=100))
                       ,tags$td(numericInput("input2vol",  "Input2 Vol",  default_values$vols[2], min=0, max=10, step=0.01, width=100))
                       ,tags$td(numericInput("input3vol",  "Input2 Vol",  default_values$vols[3], min=0, max=10, step=0.01, width=100))
                       ,tags$td(numericInput("output1vol", "Output1 Vol", default_values$vols[4], min=0, max=10, step=0.01, width=100))
                       ,tags$td(numericInput("fxvol",      "F/X Vol",     default_values$vols[5], min=0, max=10, step=0.01, width=100))
                        )),
                hr(),
                h4("Input Weights"),
                tags$table(
                tags$tr(tags$td(numericInput("input1wt", "Input1 Wt",     default_values$input_weight[1], min=0, max=10, step=0.01, width=100))
                       ,tags$td(numericInput("input2wt", "Input2 Wt",     default_values$input_weight[2], min=0, max=10, step=0.01, width=100))
                       ,tags$td(numericInput("input3wt", "Input2 Wt",     default_values$input_weight[3], min=0, max=10, step=0.01, width=100))
                        )),
                hr(),
                h4("Price Correlations"),
                tags$table(
                tags$tr(tags$td("Input1", width=85)
                       ,tags$td(numericInput("i12corr", "Input2",  default_values$corrs[1], min=-1, max=1, step=0.01, width=85))
                       ,tags$td(numericInput("i13corr", "Input3",  default_values$corrs[2], min=-1, max=1, step=0.01, width=85))
                       ,tags$td(numericInput("i14corr", "Output1", default_values$corrs[3], min=-1, max=1, step=0.01, width=85))
                       ,tags$td(numericInput("i15corr", "F/X",     default_values$corrs[4], min=-1, max=1, step=0.01, width=85))
                        ),
                tags$tr(tags$td("")
                       ,tags$td("Input2")
                       ,tags$td(numericInput("i23corr", "",        default_values$corrs[5], min=-1, max=1, step=0.01, width=85))
                       ,tags$td(numericInput("i24corr", "",        default_values$corrs[6], min=-1, max=1, step=0.01, width=85))
                       ,tags$td(numericInput("i25corr", "",        default_values$corrs[7], min=-1, max=1, step=0.01, width=85))
                        ),
                tags$tr(tags$td("")
                       ,tags$td("")
                       ,tags$td("Input3")
                       ,tags$td(numericInput("i34corr", "",        default_values$corrs[8], min=-1, max=1, step=0.01, width=85))
                       ,tags$td(numericInput("i35corr", "",        default_values$corrs[9], min=-1, max=1, step=0.01, width=85))
                        ),
                tags$tr(tags$td("")
                       ,tags$td("")
                       ,tags$td("")
                       ,tags$td("Output1")
                       ,tags$td(numericInput("i45corr", "",        default_values$corrs[10], min=-1, max=1, step=0.01, width=85))
                        )
                ),
                actionButton("update_params", "Update Parameters"),
                textOutput('params_message')
            )
        )
    )
))
