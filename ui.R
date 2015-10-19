library(shiny)

# Define UI
shinyUI(fluidPage(

        titlePanel(h3("Central Limit Theorem Demonstration")),

        p("We simulate the distribution of means and variances of a samples from the ", strong("ostensibly non-gaussian exponential distribution.")),
        p("The means and variances of the sampled mean and variance are themselves random variates that tend to centrality, unlike the underlying exponential distribution from which the samples are taken. ", strong("The purpose"), " is to demonstrate empirically that as the number of samples increases toward infinity:"),
        tags$ol(
            tags$li("The mean of the sample means tends to the theoretical population mean as the ", strong("number of samples"), " increases;"),
            tags$li("The variance of the population mean tends toward the theoretical population variance ", strong("divided by the sample size;")),
            tags$li("The mean of the sample variances tends to the theoretical population variance as the ", strong("number of samples"), " increases;")),
        p("Although ultimate convergence to the asymptote is guaranteed, a ", strong("rapid"), " convergence for any arbitrary underlying distribution is not."),
        p("Furthermore: the variance of the sample variances is right-skewed, but tends to lose its skew as the ", strong("sample size"), " becomes large."),

        sidebarLayout(
                sidebarPanel(
                        h4("Use the sliders to select the desired parameters..."),

                        sliderInput(inputId="nosim",
                                    label=h5("Number of samples"),
                                    min = 10, max = 5000, value = 100),

                        sliderInput(inputId="n",
                                    label=h5("Sample size"),
                                    min = 4, max = 500, value = 40),

                        sliderInput(inputId="lambda",
                                    label=h5("Exponential rate"),
                                    min = 0.01, max = 30, value = 0.2),

                        textOutput(outputId="asymptoticMeanVal"),
                        textOutput(outputId="asymptoticVarVal"),
                        textOutput(outputId="lambda"),

                        br(),
                        em(strong("Vertical line colour key for distribution plots:")),
                        br(),
                        tags$span(style="color:red", "Sample Mean of Means --------"),
                        br(),
                        tags$span(style="color:blue", "Asymptotic Mean of Means --------"),
                        br(),
                        tags$span(style="color:purple", "Sample Mean of Variances --------"),
                        br(),
                        tags$span(style="color:darkgreen", "Asymptotic Mean of Variances --------"),
                        br(),
                        tags$span(style="color:black", "Population mean --------"),
                        br(),
                        br(),
                        em("(Note that sample lines may obscure asymptote lines when these are equal.)"),
                        br(),

                        h4("..then select one of the tabs to the right to see the desired output plot.")

                ),
                mainPanel(
                        tabsetPanel(

                                tabPanel("Exponential distribution",
                                         plotOutput(outputId="barplot"),
                                         h5("Note that the exponential distribution is highly non-gaussian in shape. In this plot we have plotted the selected number of random samples from the exponential distribution."),
                                         br(),
                                         h4("Theoretical population distribution"),
                                         plotOutput(outputId="expPlot")
                                         ),

                                tabPanel("Sample asymptotics",
                                         plotOutput(outputId="cumMean"),
                                         plotOutput(outputId="cumVar")),

                                tabPanel("Distribution of sample means",
                                         plotOutput(outputId="sampleMeanDist"),
                                         plotOutput(outputId="sampleVarDist"))

                        )
                )
)
))
