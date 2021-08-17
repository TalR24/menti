## app.R
## Shiny R app - creates interactive tabs by industry charts and tables
## Last edited 8/17/21 by Tal Roded
###########################################################################

## Load the packages
library(shiny)
library(ggplot2)
library(shinythemes)
library(dplyr)
library(readr)
library(colorspace)
library(tidyr)
library(png)
library(shinyWidgets)
library(gifski)
library(gganimate)
library(extrafont)
library(readxl)
library(ggthemes)
library(RColorBrewer)

## Set working directory for data used in the charts
# setwd("~/menti/industry_insights")

## Import the data - BLS OOH
industry_data <- read_excel("occupation.xlsx", sheet="Table 1.7", col_names=T, skip=1)

## Clean the data
industry_data <- industry_data %>%
  filter(!is.na(emp_2019))

industry_data <- industry_data %>%
  mutate(ind_growth =
           case_when(
             emp_change_pct_2019_2029 >0 ~ 1, 
             emp_change_pct_2019_2029 <0 ~ 0
           )) 

industry_data <- industry_data %>%
  mutate(wage_buckets =
           case_when(
             median_annual_wage_2020>0 & median_annual_wage_2020<33333 ~ "Less than $33,333", 
             median_annual_wage_2020>=33333 & median_annual_wage_2020<66666 ~ "Between $33,333 & $66,666", 
             median_annual_wage_2020>=66666 & median_annual_wage_2020<99999 ~ "Between $66,666 & $100,000", 
             median_annual_wage_2020>=99999 & median_annual_wage_2020<133333 ~ "Between $100,000 & $133,333", 
             median_annual_wage_2020>=133333 & median_annual_wage_2020<166666 ~ "Between $133,333 & $166,666",
             median_annual_wage_2020>=166666 & median_annual_wage_2020<199999 ~ "Between $166,666 & $200,000",
             median_annual_wage_2020>199999 ~ "More than $200,000"
)) 

industry_data$Industry <- factor(industry_data$Industry, levels = c("Management", "Business and financial operations", 
                                 "Computer and mathematical", "Architecture and engineering", "Life, physical, and social science", 
                                 "Community and social service",
                                 "Legal", "Educational instruction and library", "Arts, design, entertainment, sports, and media",
                                 "Healthcare practitioners and technical", "Healthcare support", 
                                 "Protective service", "Food preparation and serving related", 
                                 "Building and grounds cleaning and maintenance", "Personal care and service", 
                                 "Sales and related", "Office and administrative support", 
                                 "Farming, fishing, and forestry", "Construction and extraction", 
                                 "Installation, maintenance, and repair", "Production", "Transportation and material moving"))

industry_data <- industry_data %>%
  filter(!is.na(Industry))

##########################
## Shiny app
##########################
# Use taglist layout - this allows us to have multiple navigation tabs
ui = tagList(
  
  fluidPage(
    column(12,offset = 0, titlePanel(tags$em("Industry Insights"), windowTitle = "Industry Insights")),
    tags$style(HTML("a {color: #FA6900}"))
  ),
  tags$br(),
  
  navbarPage("  ", id="nav", position = "fixed-bottom",
             #Define first navigation panel
             tabPanel(
               
               tags$head(
                 # Include our custom CSS
                 includeCSS("styles.css")
               ),
               
               #Define sidebar
               sidebarPanel(id = "sidebar",
                            
                            tags$head(tags$style(
                              HTML('
                      
                        body, label, input, button, select { 
                        font-family: "Helvetica";
                        }'))),
                            
                            #Create checkbox inputs
                            p(tags$strong("Explore the data and compare jobs in different industries!"), color = "black"),
                            helpText("Select the filters below to compare employment, wages, and other characteristics among jobs in collections of industries.", color = "black"),
                            tags$br(),
                            selectInput(inputId = "Industry", label = "Industry", choices = c("Management", "Business and financial operations", 
                                                                                             "Computer and mathematical", "Architecture and engineering", "Life, physical, and social science", 
                                                                                             "Community and social service",
                                                                                             "Legal", "Educational instruction and library", "Arts, design, entertainment, sports, and media",
                                                                                             "Healthcare practitioners and technical", "Healthcare support", 
                                                                                             "Protective service", "Food preparation and serving related", 
                                                                                             "Building and grounds cleaning and maintenance", "Personal care and service", 
                                                                                             "Sales and related", "Office and administrative support", 
                                                                                             "Farming, fishing, and forestry", "Construction and extraction", 
                                                                                             "Installation, maintenance, and repair", "Production", "Transportation and material moving"), 
                                        "Management", multiple = FALSE),
                            
                            tags$br(),
                            radioButtons(inputId = "education_needed", label = "Typical Education Requirement", choices = c("No requirement" = "—", "High School Diploma" = "High school diploma or equivalent", 
                                                                                                                            "Bachelor's degree" = "Bachelor's degree", "Post-High School Training" = "Postsecondary nondegree award", 
                                                                                                                            "Master's degree" = "Master's degree", "Associate's degree" = "Associate's degree", 
                                                                                                                            "Some College" = "Some college, no degree", 
                                                                                                                            "Advanced degree" = "Doctoral or professional degree"), selected = NULL,
                                         inline = FALSE, width = NULL, choiceNames = NULL,
                                         choiceValues = NULL),
                            tags$br(),
                            radioButtons(inputId = "work_experience", label = "Work Experience Requirement", choices = c("No requirement" = "—", "Less than 5 years" = "Less than 5 years", 
                                                                                                                            "5 years or more" = "5 years or more"), selected = NULL,
                                         inline = FALSE, width = NULL, choiceNames = NULL,
                                         choiceValues = NULL),
                            
                            tags$br(),
                            radioButtons(inputId = "ind_growth", label = "Is it a growing industry?", choices = c("Yes" = "1", "No" = "0"), selected = NULL,
                                         inline = FALSE, width = NULL, choiceNames = NULL,
                                         choiceValues = NULL),
                            
                            tags$br(),
                            radioButtons(inputId = "wage_buckets", label = "Average Yearly Income in 2020", choices = c("Less than $33,333" = "Less than $33,333", "Between $33,333 & $66,666" = "Between $33,333 & $66,666", 
                                                                                                                        "Between $66,666 & $100,000" = "Between $66,666 & $100,000", "Between $100,000 & $133,333" = "Between $100,000 & $133,333", 
                                                                                                                        "Between $133,333 & $166,666" = "Between $133,333 & $166,666", "Between $166,666 & $200,000" = "Between $166,666 & $200,000", 
                                                                                                                        "More than $200,000" = "More than $200,000"), selected = NULL,
                                         inline = FALSE, width = NULL, choiceNames = NULL,
                                         choiceValues = NULL),
                            
                            
                            #Add text to interactivity bar
                            helpText("This interactive app was created with data from", a("the US Bureau of Labor Statistics", 
                                                                                          href="https://www.bls.gov/ooh/")), 
                            
                            
                            #Download Button
                            uiOutput("download_button")),
               
               #Create main panel with two tabs: one for visual and one for data
               mainPanel(
                 
                 tabsetPanel(type = "tabs",
                             tabPanel(
                               "Summary", 
                               tags$br(),
                               p("I will provide a short intro to industry insights here. Probably put some main summary charts below", "Using the inputs on the left (I will provide basic instructions here)", tags$strong(tags$em("explore the findings for yourself."))), 
                               tags$br(),
                               p("Then", tags$strong("visualize"), "the data in the 'Visualize' tab or", tags$strong("get right to the numbers"), "with the 'Numbers' tab."),
                               tags$br()),
                             
                             tabPanel("Visualize",
                                      
                                      
                                      p("  "),
                                      p("  "),
                                      
                                      p("   "),
                                      
                                      plotOutput("empGrowth_plot"),
                                      tags$br(),
                                      plotOutput("wages_plot"),
                                        tags$br()),
                             
                             
                             tabPanel("Data",
                                      column(6,offset = 2,
                                             p(),
                                             tableOutput("Industry_df"))),
                            
                            
                            tabPanel("Industry Tables", 
                                        tags$br(),
                                        p("Still itching for more? Check out our Industry Tables:", a("In-depth interviews with professionals from all types of industries", href = "https://www.menti.club/career-explore")),
                                        tags$br(),
                                        img(src="IG Fresh Menu.png", align = "left",  height="65%", width="65%"),
                                        tags$br(),
                                        p("If Vanessa wants to make anything I can include further images/links/infographics here")
                                      )
                                      )
                             
                             
                 )
               )
             )
             
             
  )




####################
## Server Side 
####################
server <- function(input, output){
  
  formulaText <- reactive(function() {
    paste("You chose", input$Industry)
  })
  
  # Return the formula text for printing as a caption
  output$caption <- renderText(function() {
    formulaText()
  })
  
  #
  
  reactiveIndustry <- reactive({return(tbl_df(industry_data) %>%
                                         filter(Industry == input$Industry) %>%
                                         filter(education_needed == input$education_needed) %>%
                                         filter(work_experience == input$work_experience) %>%
                                         filter(ind_growth == input$ind_growth) %>%
                                         filter(wage_buckets == input$wage_buckets))})
  
  output$dfInd <- renderTable({reactiveIndustry()})
  
  output$empGrowth_plot <- renderPlot({
    
    ggplot(reactiveIndustry(), aes(x=reorder(occ_title, emp_change_pct_2019_2029), y=emp_change_pct_2019_2029)) + 
      geom_bar(stat="identity") + theme_fivethirtyeight() + coord_flip() + 
      labs(title="Projected Employment Growth, 2019-2029") + 
      theme(plot.title = element_text(hjust=0.5, size=20), axis.text.x = element_text(face="bold", size=12), 
            axis.text.y = element_text(size=12)) + 
      scale_y_continuous(labels = function(x) paste0(x, "%")) + 
      scale_color_brewer(type="seq", palette="Oranges")
  })  
  
  output$wages_plot <- renderPlot({
    
    ggplot(reactiveIndustry(), aes(x=reorder(occ_title, median_annual_wage_2020), y=median_annual_wage_2020)) + 
      geom_bar(stat="identity") + theme_fivethirtyeight() + coord_flip() + 
      labs(title="Median Yearly Income, 2020") + 
      theme(plot.title = element_text(hjust=0.5, size=20), axis.text.x = element_text(face="bold", size=12), 
            axis.text.y = element_text(size=12)) + 
      scale_y_continuous(labels = function(x) paste0("$", x))
  }) 
  
  
  reactiveDfIndustry <- reactive({return(tbl_df(industry_data) %>% 
                                   filter(Industry == input$Industry) %>%
                                   filter(education_needed == input$education_needed) %>%
                                   filter(work_experience == input$work_experience) %>%
                                   filter(ind_growth == input$ind_growth) %>%
                                   filter(wage_buckets == input$wage_buckets) %>%
                                   group_by(occ_title) %>%
                                   summarise("Number Employed, 2019" = mean(emp_2019*1000),
                                    "Employment Change (#), 2019-2029" = mean(emp_change_2019_2029*1000),
                                   "Employment Change (%), 2019-2029" = mean(emp_change_pct_2019_2029),
                                   "Yearly Income, 2020" = mean(median_annual_wage_2020))
                                    )})
  
  output$Industry_df <- renderTable({reactiveDfIndustry()})

  
}

shinyApp(ui=ui, server=server) 

