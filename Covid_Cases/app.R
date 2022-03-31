#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)     # for data cleaning and plotting
library(lubridate)     # for date manipulation

#COVID-19 data from the New York Times
covid19 <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv")

# Census data
census_pop_est_2018 <- read_csv("https://www.dropbox.com/s/6txwv3b4ng7pepe/us_census_2018_state_pop_est.csv?dl=1") %>% 
  separate(state, into = c("dot","state"), extra = "merge") %>% 
  select(-dot) %>% 
  mutate(state = str_to_lower(state))

covid_with_2018_pop_est <-
  covid19 %>% 
  mutate(state = str_to_lower(state)) %>% 
  left_join(census_pop_est_2018) %>% 
  mutate(cases_per_100000 = (cases/est_pop_2018)*100000) %>% 
  mutate(state = str_to_title(state))

ui <- fluidPage(
  sliderInput(inputId = "date", 
              label = "Date Range",
              min = as.Date("2020-01-21"), 
              max =as.Date("2022-03-30"), 
              value = as.Date(c("2020-01-21", "2022-03-30"))),
  selectInput("state", 
              "States of Interest", 
              multiple = TRUE,
              choices = covid_with_2018_pop_est %>% arrange(state) %>%  distinct(state) %>% pull(state) ),
  submitButton("Submit"),
  plotOutput(outputId = "state_plot")
)

server <- function(input, output){
  output$state_plot <- renderPlot(
    covid_with_2018_pop_est %>% 
      filter(state == input$state) %>% 
      ggplot(aes(y=cases_per_100000, x=date, color = state))+
      geom_line()+
      scale_x_date(limits = input$date) +
      theme_minimal() +
      labs(y = "Cases per 100,000 People", x = "", title = "Cumulative Cases for US States", color = "States of Interest")
  )
}

shinyApp(ui = ui, server = server)

# covid19 %>% arrange(state) %>%  distinct(state) %>% pull(state)
