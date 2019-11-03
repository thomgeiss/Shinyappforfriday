#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#Running shiny and tidyverse to start working on the app
library(shiny)
library(tidyverse)

#this is setting the minimum and maximun range for the slider on the app
min.carat <- min(diamonds$carat)
max.carat <- max(diamonds$carat)

#this is adding a character vector as the axis variable
axis_vars <- names(diamonds)

#Here I am making the column of diamonds into a character vector using vapply 
factor.indices <- vapply(diamonds, is.factor, TRUE)
factor.columns <- axis_vars[factor.indices]

#user interface. The sidebar/layout functions make the slider using the min and max carat values assigned above
ui <- fluidPage(
      titlePanel("Diamonds Viewer"),
       sidebarLayout(
        sidebarPanel(
          sliderInput("caratrange",
                      "Range of Carats",
                      min = min.carat,
                      max = max.carat,
                      value = c(min.carat, max.carat)),
          selectInput(inputId = "xvar",
                      label = "x-axis",
                      choices = axis_vars,
                      selected = "x"),
          selectInput(inputId = "yvar",
                      label = "y-axis",
                      choices = axis_vars,
                      selected = "y"),
          submitButton(text = "Go!",
                       icon = icon("thumbs-up"))),
          
          #This will show a plot of the diamonds dataframe
          mainPanel(
            plotOutput("diamonds_plot"))
      )
)

#server functions that will make the histogram 
server <- function(input, output) {
  filt_dia <- reactive(
                       {diamonds %>% #this uses reactive functions which will change according to their input
                        filter(carat >= min(input$caratrange)) %>%
                        filter(carat <= max(input$caratrange))
    stop("I added this stop")
    })
#using ggplot to make the plot with a reactive function that will change according to the input$go
 p_diamonds <- reactive({
                             ggplot(filt_dia(), aes_string(x = input$xvar, y = input$yvar)) +
                            geompoint()
   })
  #these last functions draw the plot and the double slider to interact with
 output_diagnostic <- renderText(input$caratrange)

 output$diamonds_plot <- renderPlot(p_diamonds)
}

shinyApp(ui = ui, server = server)
