---
title       : Developing Data Products - Reproducible Pitch
subtitle    : MPG Predictor Presentation in Slidify
author      : Mystic Source
job         : R Hacker
framework   : io2012        # {io2012, html5slides, shower, dzslides, ...}
highlighter : highlight.js  # {highlight.js, prettify, highlight}
hitheme     : tomorrow      # 
widgets     : []            # {mathjax, quiz, bootstrap}
mode        : selfcontained # {standalone, draft}
knit        : slidify::knit2slides
---

## Slide 1

## MPG Predictors - Explore mtcars
### Part 1

Discover which variables predict MPG
https://mysticsource.shinyapps.io/DevelopingDataProducts/

### Part 2

This presentation done in Slidify, published to https://github.com/Sriramjay/DevelopingDataProducts

Code available at above github URL

--- .class #id 

## Slide 2
### Dataset - Motor Trend Road Tests

Description

The data was extracted from the 1974 Motor Trend US magazine, and comprises fuel consumption and 10 aspects of automobile design and performance for 32 automobiles (1973-74 models).

Source

Henderson and Velleman (1981), Building multiple regression models interactively. Biometrics, 37, 391-411.

library(datasets)
head(mtcars,2)
              mpg cyl disp  hp drat    wt  qsec vs am gear carb
Mazda RX4      21   6  160 110  3.9 2.620 16.46  0  1    4    4
Mazda RX4 Wag  21   6  160 110  3.9 2.875 17.02  0  1    4    4

--- .class #id 

## Slide 3
### Format

A data frame with 32 observations on 11 variables.

mpg  Miles/(US) gallon

cyl  Number of cylinders

disp  Displacement (cu.in.)

hp  Gross horsepower

drat  Rear axle ratio

wt  Weight (1000 lbs)

qsec  1/4 mile time

vs  V/S

am  Transmission (0 = automatic, 1 = manual)

gear  Number of forward gears

carb  Number of carburetors

--- .class #id 

## Slide 4
### Server Code

library(shiny)
data(mtcars)

modelFit <- lm(mpg ~ hp + cyl + wt, data=mtcars)

mpg <- function(hp, cyl, wt) {
        modelFit$coefficients[1] + modelFit$coefficients[2] * hp + 
                modelFit$coefficients[3] * cyl + modelFit$coefficients[4] * wt
}

shinyServer(
        function(input, output) {
                adjusted_weight <- reactive({input$wt/1000})
                predicted_mpg <- reactive({mpg(input$hp, as.numeric(input$cyl), adjusted_weight())})
                output$inputValues <- renderPrint({paste(input$cyl, "cylinders, ",
                                                         input$hp, "horsepower, ",
                                                         input$wt, "lbs")})
                output$prediction <- renderPrint({paste(round(predicted_mpg(), 2), "miles per gallon")})
                output$plots <- renderPlot({
                        par(mfrow = c(1, 3))
                        # (1, 1)
                        with(mtcars, plot(hp, mpg,
                                          xlab='Gross horsepower',
                                          ylab='MPG',
                                          main='MPG vs horsepower'))
                        points(input$hp, predicted_mpg(), col='red', cex=3)                 
                        # (1, 2)
                        with(mtcars, plot(cyl, mpg,
                                          xlab='Number of cylinders',
                                          ylab='MPG',
                                          main='MPG vs cylinders'))
                        points(as.numeric(input$cyl), predicted_mpg(), col='red', cex=3)  
                        # (1, 3)
                        with(mtcars, plot(wt, mpg,
                                          xlab='Weight (lb/1000)',
                                          ylab='MPG',
                                          main='MPG vs weight'))
                        points(adjusted_weight(), predicted_mpg(), col='red', cex=3)  
                })
        }
)









