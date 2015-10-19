library(shiny)
library(ggplot2)

shinyServer(
        function(input, output) {

                output$asymptoticMeanVal <- reactive({
                        paste("Asymptotic sample mean = ",
                              round(1 / input$lambda, 6))

                })

                output$asymptoticVarVal <- reactive({
                        paste("Asymptotic sample variance = ",
                              round(1 / input$lambda^2 / input$n, 6))

                })

                output$lambda <- reactive({
                    paste("Exponential rate = ",
                          round(input$lambda, 6))

                })

                output$barplot <- renderPlot({
                    n <- input$n; nosim <- input$nosim; lambda <- input$lambda
                    mainTitle <- paste(nosim, " random samples of rate ", lambda)

                    par(mfrow = c(1, 1), mar = c(4, 4, 3, 2), lwd = 0.5,
                        col="#009E73", cex.main = 1.0, cex.lab = 1.0, lwd = 1.5)
                    set.seed(1096) # Set random number generation seed to ensure repeatability.
                    d <- rexp(nosim, rate = lambda)
                    hist(d, freq = TRUE,
                         main = mainTitle,
                         xlab = "Value", col = "lightblue", breaks = "FD")
                    abline(v = mean(d), col = "red")
                    abline(v = 1/lambda, col = "blue")
                })

                output$expPlot <- renderPlot({
                    n <- input$n; nosim <- input$nosim; lambda <- input$lambda
                    par(mfrow = c(1, 1), mar = c(4, 4, 3, 2), lwd = 0.5,
                        col="#009E73", cex.main = 1.0, cex.lab = 1.0, lwd = 1.5)
                    set.seed(1096) # Set random number generation seed to ensure repeatability.
                    curve(dexp(x, rate = lambda), from = 0, to = 10/lambda,
                          xlab = "x", ylab = "exp(x)")
                    abline(v = 1/lambda, col = "black")

                })

                output$cumMean <- renderPlot({

                    n <- input$n; nosim <- input$nosim; lambda <- input$lambda

                    set.seed(1096) # Set random number generation seed to ensure repeatability.

                    ### Generate chosen number of samples of selected size from exponential
                    ### distribution.
                    samples <- matrix(data = rexp(n * nosim, lambda), nrow = nosim)

                    ### Calculate sample means  for each of the samples
                    sampleData <- data.frame(sampleMean = rowMeans(samples),
                                             sampleVariance = apply(samples, 1, var))

                    ### Calculate the cumulative means of the sample means
                    sampleData$cumulatedMeanOfSampleMeans <- cumsum(sampleData$sampleMean)/
                            seq_along(sampleData$sampleMean)

                    ### Plot asymptotic tendency of mean of sample means
                    mainTitle <- paste("Cumulative sample means for", nosim,
                                       "simulations, ","sample size = ", n, ", rate = ", lambda)

                    asymptoteTitle <- "Asymptotic sample mean"
                    par(mfrow = c(1, 1), mar = c(4, 4, 3, 2), lwd = 0.5, col="#0072B2",
                        cex.main = 1.0, cex.lab = 1.0, lwd = 1.5)
                    plot(sampleData$cumulatedMeanOfSampleMeans, type = "l",
                         main = mainTitle,
                         xlab = "Number of generated samples (simulations)",
                         ylab = "Cumulative mean of sample means")
                    abline(h = 1 / lambda, col = "#009E73")
                    text(x = nosim * 3/4, y = 1 / lambda, pos = 1,
                         labels = asymptoteTitle, col="#009E73")

                })

                output$cumVar <- renderPlot({

                    n <- input$n; nosim <- input$nosim; lambda <- input$lambda

                    set.seed(1096) # Set random number generation seed to ensure repeatability.
                    ### Plot asymptotic tendency of variance of sample means

                    ### Generate chosen number of samples of selected size from exponential
                    ### distribution.
                    samples <- matrix(data = rexp(n * nosim, lambda),
                                 nrow = nosim)

                    ### Calculate sample means and sample variances for each of the samples
                    sampleData <- data.frame(sampleMean = rowMeans(samples),
                                            sampleVariance = apply(samples, 1, var))

                    ### Calculate the cumulative means and variances of the sample means
                    sampleData$cumulatedMeanOfSampleMeans <- cumsum(sampleData$sampleMean)/
                            seq_along(sampleData$sampleMean)

                    sampleData$cumulatedVarianceOfSampleMeans <-
                            cumsum((sampleData$sampleMean - 1/lambda)^2)/
                            (seq_along(sampleData$sampleMean - 1))

                    mainTitle <- paste("Cumulative sample variances for", nosim,
                                       "simulations, ","sample size = ", n, ", rate = ", lambda)
                    variance <- 1/lambda^2/n
                    asymptoteTitleV <- "Asymptotic sample variance"

                    par(mfrow = c(1, 1), mar = c(4, 4, 3, 2), lwd = 0.5,
                       col="#0072B2", cex.main = 1.0, cex.lab = 1.0, lwd = 1.5)
                    plot(sampleData$cumulatedVarianceOfSampleMeans, type = "l",
                    main = mainTitle, xlab = "Number of generated samples (simulations)",
                       ylab = "Cumulative variance of sample means")
                    abline(h = 1/ lambda^2/n, col = "#009E73")
                    text(x = nosim * 3/4, y = 1 / lambda^2 / n, pos = 1,
                         labels = asymptoteTitleV, col="#009E73")
                    })


                output$sampleMeanDist <- renderPlot({
                        n <<- input$n
                        nosim <<- input$nosim
                        lambda <<- input$lambda

                        asymptoticSE <<- 1/lambda/sqrt(n) ## Standard error of sample mean.

                        set.seed(1096)
                        ### distribution.
                        samples <- matrix(data = rexp(n * nosim, lambda),
                                          nrow = nosim)

                        ### Calculate sample means and sample variances for each of the samples
                        sampleData <- data.frame(sampleMean = rowMeans(samples),
                                                 sampleVariance = apply(samples, 1, var) / n)

                        ### Calculate the cumulative means and variances of the sample means
                        sampleData$cumulatedMeanOfSampleMeans <- cumsum(sampleData$sampleMean)/
                                seq_along(sampleData$sampleMean)

                        sampleData$cumulatedVarianceOfSampleMeans <-
                                cumsum((sampleData$sampleMean - 1/lambda)^2)/
                                (seq_along(sampleData$sampleMean - 1))


                        sampleMeanSE <<- sd(sampleData$sampleMean)
                        meanEstimator <<- sampleData$cumulatedMeanOfSampleMeans[nosim]

                        ## Used to plot vertical lines corresponding to SD intervals:
                        SD <<- 1/lambda + c(-3, -2, -1, 1, 2, 3) * asymptoticSE

                        ## Compute plot title
                        mainTitle <- paste("Sample means distribution with superimposed theoretical gaussian distribution curve\nfor", nosim, "means of", n, "exponentially distributed values, rate = ", round(lambda, 6))
                        binwd <- 3.5 * asymptoticSE / nosim^(1/3)

                        ggplot(data = sampleData, aes(x = sampleMean)) +
                                geom_histogram(aes(y = ..density..), fill = I("lightgreen"),
                                               binwidth = binwd, color = I("black")) +
                                stat_function(fun = dnorm,
                                          arg = list(mean = 1/lambda,
                                                     sd = 1/lambda/sqrt(n))) +
                                ggtitle(mainTitle) +
                                theme_bw() +
                                xlab("Mean of sample means") +
                                ylab("Density") +
                                geom_vline(xintercept = 1/lambda, colour="blue", size = 0.8,
                                           linetype = "solid") +
                                geom_vline(xintercept = sampleData[nosim, 3],
                                           colour="red", linetype = "solid", size = 0.8) +
                                geom_vline(xintercept = SD,
                                           colour="green", linetype = "longdash", size = 0.6)
                })

                output$sampleVarDist <- renderPlot({
                    n <<- input$n
                    nosim <<- input$nosim
                    lambda <<- input$lambda
                    ## Estimate standard error of sample variance:
                    asymptoticSE_V <<- sqrt(2/(n - 1))/lambda^2/n

                    set.seed(1096)
                    ### Generate random exponential samples.
                    samples <- matrix(data = rexp(n * nosim, lambda), nrow = nosim)

                    ### Calculate sample means and sample variances for each of the samples
                    sampleDataV <- data.frame(sampleVariance =
                                                  apply(samples, 1, var) / n)
                    means <- data.frame(apply(samples, 1, mean) / n)

                    ### Calculate the cumulative means and variances of the sample variances
                    sampleDataV$cumulatedMeanOfSampleVariances <-
                        cumsum(sampleDataV$sampleVariance)/
                        seq_along(sampleDataV$sampleVariance)

                    sampleDataV$cumulatedVarianceOfSampleVariances <-
                        cumsum((sampleDataV$sampleVariance - 1/lambda^2/n)^2)/
                        (seq_along(sampleDataV$sampleVariance - 1))


                    sampleVarianceSE <<- sd(sampleDataV$sampleVariance)
                    meanEstimatorV <<- sampleDataV$cumulatedMeanOfSampleVariances[nosim]

                    ## Used to plot vertical lines corresponding to SD intervals:
                    SD_V <<- meanEstimatorV + c(-3, -2, -1, 1, 2, 3) * asymptoticSE_V

                    ## Calculate the bin width to apply
                    varbinwd <- 3.5 * asymptoticSE_V / nosim^(1/3)
                    ## Compute plot title
                    mainTitle <- paste("Sample variances distribution for", nosim, "means of", n, " values, rate = ", round(lambda, 6))

                    ggplot(data = sampleDataV, aes(x = sampleVariance)) +
                        geom_histogram(aes(y = ..density..),
                                       fill = I("lightblue"),
                                       binwidth = varbinwd,
                                       color = I("black")) +
                        ggtitle(mainTitle) +
                        theme_bw() +
                        xlab("Mean of sample variances") +
                        ylab("Density") +
                        geom_vline(xintercept = 1/lambda^2/n, colour="darkgreen",
                                   linetype = "solid", size = 0.8) +
                        geom_vline(xintercept =
                                       sampleDataV$cumulatedMeanOfSampleVariances[nosim],
                                   colour="purple", linetype = "solid", size = 0.8) +
                        geom_vline(xintercept = SD_V,
                                   colour="green", linetype = "longdash", size = 0.6)

                })

        }
)
