## Jeffrey Jiefu Huang
## Assignment 10
## For this assignment you will make a Shiny app using American Community Survey data.

##Please write a Shiny app with the following features:
  
## * a choice of which state to plot
## * a choice of plotting either median household income (B19013_001), median gross rent (B25064_001), or the ratio of median gross rent to median household income
## * a plot of the chosen variable in the chosen state.
## You can make the plot using choroplethr, ggplot with geom_sf, or leaflet (if you want to try leaflet take a look at https://rstudio.github.io/leaflet/shiny.html). You can plot county or census tract data; if you use choroplethr I suggest sticking to county-level.
## Finally, save your work in a Github repository. For your submission, please send the address so that I can clone your repository.
##This assignment is due Sunday, April 22 at 6 pm

## Loading Resources
library(shiny)
library(tidyverse)
library(tidycensus)
source("api-keys.R")
census_api_key(api.key.census)



TmpFunc <- fluidPage(

  titlePanel("Assignment #10 Data Set"), sidebarLayout(sidebarPanel(selectInput("State", "State", choices = state.abb, selected = "NY"), radioButtons("Type", "Type", choices = list("agg_gross_rent", "asian_household_income", "ratio"), selected = "ratio")), mainPanel(plotOutput("Plot"))))


#Plotting
Realm <- function(input, output) {
  
  fixed <- reactive({
    get_acs(
      geography = "tract",
      variables = c(agg_gross_rent = "B25067_001" , asian_household_income = "B19013D_001"),
      state = input$State,
      geometry = TRUE
    ) %>% .[, -5] %>% data.frame() %>% 
      spread(key = variable, value = estimate) %>% 
      mutate(ratio = agg_gross_rent / asian_household_income)
  })
  
  output$Plot <- renderPlot({
    fixed() %>% 
      ggplot(aes_string(fill = input$Type)) + geom_sf() + ggtitle(input$Type) + 
      scale_fill_gradientn(colours = rainbow(7))
  })
  
}

##Finally test run the program by:
shinyApp(ui = TmpFunc, server = Realm)