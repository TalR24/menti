## charts.R
## This file cleans the data and creates the charts that will be pulled by 
## the Shiny API in the next R script - blank.R
## Last edited 8/17/21 by Tal Roded
###########################################################################

## Load the packages
library(tidyverse)
library(ggplot2)
library(readxl)
library(ggthemes)
library(RColorBrewer)

## Set working directory for data used in the charts
setwd("~/menti/industry_insights")

## Load in data - BLS OOH
major_occs <- read_excel("data/occupation.xlsx", sheet="Table 1.1", col_names=T, skip=1)

## Clean the data
major_occs <- major_occs %>%
  filter(!is.na(emp_2019) | occ_titles=="Total, all occupations") %>%
  mutate(occ_titles = str_sub(occ_titles, end=-13))

## Create plots for use in the interactive page
# Compare employment growth across major occupation groups
major_occs_emp_growth <- ggplot(major_occs, aes(x=reorder(occ_titles, emp_change_pct), y=emp_change_pct)) + 
  geom_bar(stat="identity") + theme_fivethirtyeight() + coord_flip() + 
  labs(title="Projected Employment Growth, 2019-2029") + 
  theme(plot.title = element_text(hjust=0.5, size=16), axis.text.x = element_text(face="bold", size=10)) + 
  scale_y_continuous(labels = function(x) paste0(x, "%")) + 
  scale_color_brewer(type="seq", palette="Oranges")
major_occs_emp_growth

# Compare median annual wages across major occupation groups
major_occs_wages <- ggplot(major_occs, aes(x=reorder(occ_titles, median_annual_wage_2020), y=median_annual_wage_2020)) + 
  geom_bar(stat="identity") + theme_fivethirtyeight() + coord_flip() + 
  labs(title="2020 Median Annual Wage") + 
  theme(plot.title = element_text(hjust=0.5, size=16), axis.text.x = element_text(face="bold", size=10)) + 
  scale_y_continuous(labels = function(x) paste0("$", x))
major_occs_wages

# Scatterplot comparing wages to employment growth
# Label each quadrant - top right is high growth, high wages; top left is low growth, high wages...







### Interactive Expected Lifetime Earnings

# User Inputs Age, State, and Job (which comes with median income)
age <- ageInput
state <- stateInput

lifeEarning <- (67-ageInput)*medianIncomeState

output$text_use <- renderText({
  paste("You chose", input$compare_across)
})








