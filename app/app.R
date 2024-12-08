# A Shiny app that allows users to search for items on Tradera using a query.
# The app retrieves search results and displays them in a data table format.

devtools::install_github("aurora-mm/TradeRa")

library(shiny)
library(DT)
library(TradeRa)

# Define the UI
ui <- fluidPage(
  titlePanel("Tradera Search"),

  # Text input for the user query
  textInput("query", "Search Query:", value = ""),

  # Button to trigger search
  actionButton("searchBtn", "Find"),

  # Display the Data Table
  DTOutput("searchResults")
)

# Define server logic
server <- function(input, output, session) {
  # Reactive values to store search data
  results <- reactiveValues(df = NULL)

  # Event handler for search button
  observeEvent(input$searchBtn, {
    # Call the Search function with the specified AppId, AppKey, etc.
    searchResult <- Search(
      AppId = 5205,
      AppKey = "180bdaf7-0455-45b5-8750-69ac9225ee6f",
      orderBy = "Relevance",
      query = input$query
    )
    
    # Extract the data frame from the search result
    if (!is.null(searchResult)) {
      # Select relevant columns for display
      results$df <- data.frame(
        ShortDescription = searchResult$ShortDescription,
        LongDescription = searchResult$LongDescription,
        Price = searchResult$Price,
        ItemUrl = searchResult$ItemUrl
      )
    } else {
      results$df <- data.frame(
        ShortDescription = "Cannot fetch search results!",
        LongDescription = "",
        Price = "",
        ItemUrl = ""
      )
    }
  })

  # Render the data table with the search results
  output$searchResults <- renderDataTable({
    req(results$df)
    results$df
  })
}

# Run the app
shinyApp(ui = ui, server = server)
