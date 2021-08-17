## app.R
## Shiny R app - creates interactive tabs by industry charts and tables
## Last edited 8/17/21 by Tal Roded
###########################################################################

## Load the packages
library(shiny)
library(ggplot)

## Set working directory for data used in the charts
setwd("~/menti/industry_insights")


## How do I render a plot?
output$PLOTNAME <- renderPlot({
  ggplot(reactivePager(), aes(x = black_use, y = callback, fill = black_use)) + 
    geom_bar(stat = "identity",  width = .8, alpha = .75)
})  

## How do I make a reactive dataframe?
reactivePager <- reactive({return(tbl_df(Pager_data) %>%
                                    filter(job_type == input$jobType) ## filtering our data based on an input
)})

## can then use this df to create a reactive table
output$dfPager <- renderTable({reactivePager()})




## Shiny app
ui <- fluidPage(
  
  # App title ----
  titlePanel("Title of your app"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    tabPanel(),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----
      plotOutput(outputId = "distPlot") 
      
    )
  )
)

server <- function(input, output) {
  
  ## We create the plot here, in the server
  output$distPlot <- renderPlot({
    
    hist(x, breaks = bins, col = "#75AADB", border = "white",
         xlab = "Waiting time to next eruption (in mins)",
         main = "Histogram of waiting times")
    
  })
  
}

shinyApp(ui = ui, server = server)

