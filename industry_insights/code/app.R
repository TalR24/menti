## app.R
## Shiny R app - creates interactive tabs by industry charts and tables
## Last edited 8/17/21 by Tal Roded
###########################################################################

## Load the packages
library(shiny)
library(ggplot)

## Set working directory for data used in the charts
setwd("~/menti/industry_insights")


# ## How do I render a plot?
# output$PLOTNAME <- renderPlot({
#   ggplot(reactivePager(), aes(x = black_use, y = callback, fill = black_use)) + 
#     geom_bar(stat = "identity",  width = .8, alpha = .75)
# })  
# 
# ## How do I make a reactive dataframe?
# reactivePager <- reactive({return(tbl_df(Pager_data) %>%
#                                     filter(job_type == input$jobType) ## filtering our data based on an input
# )})
# 
# ## can then use this df to create a reactive table
# output$dfPager <- renderTable({reactivePager()})


## Import the data - BLS OOH
industry_data <- read_excel("data/occupation.xlsx", sheet="Table 1.7", col_names=T, skip=1)

## Clean the data
industry_data <- industry_data %>%
  filter(!is.na(emp_2019) | occ_title=="Total, all occupations")

industry_data <- industry_data %>%
  mutate(ind_growth =
           case_when(
             emp_change_pct_2019_2029 >0 ~ 1, 
             emp_change_pct_2019_2029 <0 ~ 0
           )) 





#############################################################
## Shiny app
#############################################################
# ui <- fluidPage(
#   
#   # App title ----
#   titlePanel("Title of your app"),
#   
#   # Sidebar layout with input and output definitions ----
#   sidebarLayout(
#     
#     tabPanel(),
#     
#     # Main panel for displaying outputs ----
#     mainPanel(
#       
#       # Output: Histogram ----
#       plotOutput(outputId = "distPlot") 
#       
#     )
#   )
# )

# Use taglist layout - this allows us to have multiple navigation tabs
ui = tagList(
  
  fluidPage(
    column(12,offset = 0, titlePanel("Industry Insights", windowTitle = "Industry Insights")),
    tags$style(HTML("a {color: #FA6900}"))
  ),
  tags$br(),
  
  navbarPage("  ", id="nav", position = "fixed-bottom",
             #make something italics: tags$em("Boston Edition.")
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
                            p(tags$strong("Explore the data on jobs!"), color = "black"),
                            helpText("Select the filters below to compare employment, wages, and other characteristics among different industries.", color = "black"),
                            tags$br(),
                            selectInput(inputId = "Industry", "Industry", c("Management" = "Management", "Business & Finance" = "Business and financial operations", 
                                          "Computer Science & Math" = "Computer and mathematical", "Architecture & Engineering" = "Architecture and engineering", 
                                          "Sciences" = "Life, physical, and social science", "Community & Social Service" = "Community and social service",
                                          "Law" = "Legal", "Teaching" = "Educational instruction and library", "Arts & Media & Entertainment" = "Arts, design, entertainment, sports, and media",
                                          "Healthcare" = "Healthcare practitioners and technical", "Healthcare Support" = "Healthcare support", 
                                          "Public Service" = "Protective service", "Culinary & Food Prep" = "Food preparation and serving related", 
                                          "Maintenance" = "Building and grounds cleaning and maintenance", "Personal Care Services" = "Personal care and service", 
                                          "Sales" = "Sales and related", "Office & Administrative" = "Office and administrative support", 
                                          "Farming & Forestry" = "Farming, fishing, and forestry", "Construction & Mining" = "Construction and extraction", 
                                          "Repair" = "Installation, maintenance, and repair", "Production" = "Production", "Transportation" = "Transportation and material moving"), 
                                        "Management", multiple = FALSE),
                            tags$br(),
                            selectInput(inputId = "education_needed", "Typical Education Requirement", c("None" = "—", "High School Diploma" = "High school diploma or equivalent", 
                                                                                                         "Bachelor's degree" = "Bachelor's degree", "Post-High School Training" = "Postsecondary nondegree award", 
                                                                                                         "None" = "No formal educational credential", "Master's degree" = "Master's degree", 
                                                                                                         "Associate's degree" = "Associate's degree", ""), "black_mutate", multiple = FALSE),
                            
                            tags$br(),
                            
                            
                            radioButtons(inputId = "ind_growth", label = "Is it a growing industry?", choices = c("Yes" = "1", "No" = "0"), selected = NULL,
                                         inline = FALSE, width = NULL, choiceNames = NULL,
                                         choiceValues = NULL),
                            
                            tags$br(),
                            p("Requirements", color = "black"),
                            checkboxInput(inputId = "manualskill", label = "Manual Labor", value = TRUE),
                            checkboxInput(inputId = "customserv", label = "Customer Service", value = TRUE),
                            
                            
                            #Add text to interactivity bar
                            helpText("This interactive app was created with data from", a("Pager, 2002", 
                                                                                          href="https://davisvanguard.org/wp-content/uploads/2013/07/pager_ajs.pdf")), 
                            
                            
                            #Download Button
                            uiOutput("download_button")),
               
               #Create main panel with two tabs: one for visual and one for data
               mainPanel(
                 
                 tabsetPanel(type = "tabs",
                             tabPanel(
                               "About the Demo (Start here!)", 
                               tags$br(),
                               p("In 2003, Devah Pager, wanted to explore how having a criminal record affects hireability.", "Using the inputs on the left", tags$strong(tags$em("explore the findings for yourself."))), 
                               tags$br(),
                               p("Then", tags$strong("visualize"), "the data in the 'Visualize' tab or", tags$strong("get right to the numbers"), "with the 'Numbers' tab."),
                               
                               
                               tags$br(),
                               
                               p("Still itching for more? Listen to our podcast,", a("Race bias in hiring: when both applicant and employer lose.", href = "https://outsmartinghumanminds.org/module/race-bias-in-hiring/")),
                               img(src="pager_picture.png", align = "left",  height="65%", width="65%", href = "https://outsmartinghumanminds.org/module/race-bias-in-hiring/")
                             ),
                             
                             tabPanel("Visualize",
                                      
                                      
                                      p("  "),
                                      p("  "),
                                      
                                      p("   "),
                                      
                                      conditionalPanel(
                                        condition = "input.compare_across == 'black_mutate'",
                                        plotOutput("Race_plot")),
                                      conditionalPanel(
                                        condition = "input.compare_across == 'crim_record_mutate'",
                                        plotOutput("Crimrec_plot")),
                                      conditionalPanel(
                                        condition = "input.compare_across == 'race_crim_mutate'",
                                        plotOutput("Race_crim_plot"),
                                        tags$br(),
                                        p("Does something surprise you here?"),
                                        p("Devah found that", tags$strong("black applicants"), "with", tags$strong("clean records"), "were seen as", tags$u("equivalent"), "to",  tags$strong("white applicants"), "who had just been", tags$strong("released from prison.")))),
                             
                             
                             tabPanel("The Numbers",
                                      column(6,offset = 2,
                                             p(),
                                             conditionalPanel(
                                               p(),
                                               condition = "input.compare_across == 'race_crim_mutate'",
                                               tableOutput("dfracecrim")),
                                             conditionalPanel(
                                               condition = "input.compare_across == 'black_mutate'",
                                               tableOutput("dfblack")),
                                             conditionalPanel(
                                               condition = "input.compare_across == 'crim_record_mutate'",
                                               tableOutput("dfcrim"))
                                             
                                      ))
                             
                             
                 )
               )
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

