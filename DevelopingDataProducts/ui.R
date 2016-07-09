library(shiny)
shinyUI(pageWithSidebar(
        headerPanel('Developing Data Products - Course Project'),
        sidebarPanel(
                h3('Instructions'),
                p('Enter the gross horsepower, number of cylinders, and weight of your 
                  car below.  The predicted MPG will be shown to the right.'),
                h3('Please enter predictors of MPG below.'),
                numericInput('hp', 'Gross horsepower:', 140, min = 50, max = 330, step = 10), # example of numeric input
                radioButtons('cyl', 'Number of cylinders:', c('4' = 4, '6' = 6, '8' = 8), selected = '4'), # example of radio button input
                numericInput('wt', 'Weight (lbs):', 3200, min = 1500, max = 5500, step = 100)
                ),
        mainPanel(
                h6('Course Project by Mystic Source'),
                h3('Predicted MPG'),
                h4('You entered:'),
                verbatimTextOutput("inputValues"),
                h4('Which resulted in a prediction of:'),
                verbatimTextOutput("prediction"),
                h4('MPG relative to cars in mtcars data set'),
                plotOutput('plots'),
                h3('Method'),
                p('I fit a linear model to the mtcars dataset, modeling the effect of 
                  horsepower, number of cylinders, and weight on the mpg.  Since the 
                  only valid possibilities for number of cylinders in the dataset were 
                  4, 6, and 8, I limited the choice using radio buttons.  For the weight,
                  reactive() is used to convert the user input weight into the units 
                  used by the model, lb/1000.  Finally, a pre-set function using the 
                  linear model is used to predict the mpg based on the three variables 
                  input by the user.'),
                h5('References'),
                p('Henderson and Velleman (1981), Building multiple regression models interactively. Biometrics, 37, 391-411.')
                )
        ))