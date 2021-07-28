## charts.R
## This file cleans the data and creates the charts that will be pulled by 
## the Shiny API in the next R script - blank.R
## Last edited 7/28/21 by Tal Roded
###########################################################################

## Load the packages
library(tidyverse)
library(ggplot2)
library(readxl)

## Set working directory for data used in the charts
setwd("~/menti/industry_insights")

## Load in data - BLS OOH
test_data <- read_excel("data/occupation.xlsx", sheet="Table 1.1", col_names=T, skip=1)

## Clean the data
test_data2 <- test_data %>%
  filter(!is.na("Employment, 2019"))

## Create plots for use in the interactive page

major_occs_emp_growth <- ggplot(test_data, aes(x="2019 National Employment Matrix title",y="Employment change, percent, 2019-2029")) + 
  geom_bar(stat="identity")
major_occs_emp_growth
