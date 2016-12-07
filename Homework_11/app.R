library(shiny)
library(ggplot2)
library(dplyr)
library(DT)

bcl <- read.csv("bcl-data.csv", stringsAsFactors = FALSE)

ui <- fluidPage(
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

server <- function(input, output, session) {
  
  filtered <- reactive({
    if (is.null(input$countryInput) ||                      
        is.null(input$typeInput)) {                   
      return(NULL)
    }
    
    if (input$selectCountry) {
    bcl %>%
      filter(Price >= input$priceInput[1],
             Price <= input$priceInput[2],
             Type == input$typeInput,
             Country == input$countryInput
      )
    } else {                                     #5. If no country is selected, then show all countries' results
      bcl %>%
        filter(Price >= input$priceInput[1],
               Price <= input$priceInput[2],
               Type == input$typeInput
        )
    }
  })
  
  output$coolplot <- renderPlot({
    if (is.null(filtered())) {        
      return()
    }
    if (nrow(filtered()) == 0) {        ## If no results, do not attempt to create a plot (removes the error warning)
      return()
    }
    ggplot(filtered(), aes(x = Alcohol_Content, color = Country, fill = Country)) +
      geom_histogram()
  })
  
  output$results <- DT::renderDataTable({          #6. create an interactive table using DT package
    filtered()
  })
  
  output$countryOutput <- renderUI({
    selectInput("countryInput", "Country",
                sort(unique(bcl$Country)),
                selected = "CANADA")
  })
  
  output$typeOutput <- renderUI ({
    radioButtons("typeInput", "Product type",
               choices = sort(unique(bcl$Type)),
               selected = "WINE")
  })
  
  output$search_results <- renderText({paste( "You search has returned " ,nrow(filtered()), " results") })
  
}

shinyApp(ui = ui, server = server)
