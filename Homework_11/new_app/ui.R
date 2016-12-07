library(shiny)
library(ggplot2)
library(dplyr)
library(DT)

bcl <- read.csv("bcl-data.csv", stringsAsFactors = FALSE)

fluidPage(
  img(src = "logo.gif"),
  titlePanel("BC Liquor Store prices"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("priceInput", "Price", min = 0, max = 100,
                  value = c(25,40), pre = "$"),
      uiOutput("typeOutput"),
      checkboxInput("selectCountry", "Filter by country?", FALSE),           # 1. Added a filter by country checkbox
      conditionalPanel(                                                      # 2. Reveal a panel if checkbox is ticked
        condition = "input.selectCountry",
        uiOutput("countryOutput")
      )
    ),
    mainPanel(
      plotOutput("coolplot"),
      br(), br(),
      textOutput("search_results"),             #3.  added a text output for search results
      br(), br(),
      DT::dataTableOutput("results")
    )
  )
)
