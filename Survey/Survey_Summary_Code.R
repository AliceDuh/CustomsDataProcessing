## ---------------------------
##
## Script name: Code to produce summary statistics
##
## Author: Yana Myachenkova
##
## Date Created: 1 July 2021
##
## Last Updated: 17 June 2022
##
## ---------------------------
##
## Notes: in this script I produce summary statistics, infographics
##
##
## ---------------------------
## 
## In this summary script, I am going to cover the following important modules:
## 
## 1. Prepare the data
##
## 2. Create summary infographics
## 
## ---------------------------

# Clear everything and set up working directory 
rm(list=ls())
getwd()

# Please, set up working directory when running the script on your personal computer 
setwd()

#Load libraries
library(haven)
library(VIM)
library(outliers)
library(grid)
library(RColorBrewer)
library(psych)
library(lubridate)
library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)
library(stringr)
library(plyr)
library(dplyr)
library(knitr)
library(ggthemes)
library(ggrepel)
library(plotly)
library(gapminder)
library(countrycode)
library(viridis)
library(data.table)
library(readstata13)
library(tidyverse)
library(scales)
library(tidyr)
library(rjson)
library(readxl)
library(foreign)
library(readstata13)
library(reshape2)
library(skmr)
library(stargazer)
library(xlsx)
library(openxlsx)
library(ggthemes)
library(car)
library(naniar)
library(hrbrthemes)

#------------------------------------ 1. Preparing to run summary checks -------------------------------#

# Import raw encrypted date
Data_original <- fread("Survey_Mock_Data.csv")

# De-identify the data - get rid of firm's name
Data <- unique(Data_original[ ,-c('firmname')])    # run if we want to de-identify
Data <- Data_original                              # run if you want to keep firm's name

# Make sure immediately there are no "complete" duplicates
Data <- unique(Data)

# Make sure all of the surveys have consent
Data <- subset(Data, Data$consent == 1)

# Make sure that all empty observations are marked as "NA"
Data[Data == ""] <- NA




#------------------------------------ 2. Create summary infographics -------------------------------#

# Calculate and plot an average duration for the survey
df_duration <- Data %>%
  mutate(date = as.POSIXct(Data$SubmissionDate, format = "%d-%b-%Y")) %>%
  group_by(date) %>%
  summarize(mean_duration = mean(duration)/60)

# Plot average survey duration
Average_duration <- ggplot(data = df_duration, aes(x=date, y=mean_duration)) +
  geom_bar(stat ="identity", binwidth=1, fill="#c0392b", alpha=0.75) +
  labs(title="Average survey duration (in minutes) by date", x="Date", y="Average duration") 

# Save submission data in a different format
Data$SubDate <- as.POSIXct(Data$SubmissionDate, format = "%d-%b-%Y")

# palette
palette <- brewer.pal("Greys", n=9)
color.background = palette[2]

# Check the sample(DEMOGRAPHICS) - demographic population of "traders" - average age reported by a submission data
Age <- ggplot(Data, aes(x = Data$SubDate, y = Data$age)) + 
  geom_bar(stat = "summary", fun = "mean") +
  #fte_theme() +
  labs(title="Checking the sample: demographics" ,x="Date", y="Average Age") +
  geom_hline(yintercept=0, size=0.4, color="black")
Age  

# Subset positive value for sales
Data_sales <- subset(Data, Data$annual_sales > 0)

# Mutate sales var to account for different categories = units
Data_sales <- Data_sales %>%
  group_by(id) %>%
  dplyr::mutate(sales_adjusted = case_when(revenue_cat == 1 ~ annual_sales,
                                           revenue_cat == 2 ~ annual_sales*1000,
                                           revenue_cat == 3 ~ annual_sales*1000000))

# Check the resulted variable
view(Data_sales[,c("annual_sales","revenue_cat","sales_adjusted")])

# Checking the sample (KEY VARS) -  Annual sales reported by a submission date
Sale <- ggplot(Data_sales, aes(x = Data_sales$SubDate, y = Data_sales$sales_adjusted)) + 
  geom_bar(stat = "summary", fun = "mean") +
  #fte_theme() +
  labs(title="Checking the sample: sales key variable" ,x="Date", y="Average Sales Reported") +
  geom_hline(yintercept=0, size=0.4, color="black")
Sale 

# Make sure bribe1_amt is saved as an integer, numeric
Data$bribe1_unit <- as.numeric(Data$bribe1_unit)
Data$bribe1_amt <- as.numeric(Data$bribe1_amt)
Data$bribe2_unit <- as.numeric(Data$bribe2_unit)
Data$bribe2_amt <- as.numeric(Data$bribe2_amt)

# Subset positive value for bribe
Data_bribes <- subset(Data, Data$bribe1_amt > 0)

# Mutate sales var to account for different categories = units
Data_bribes <- Data_bribes %>%
  group_by(id) %>%
  dplyr::mutate(bribe1_adjusted = case_when(bribe1_unit == 1 ~ bribe1_amt,
                                            bribe1_unit == 2 ~ bribe1_amt*1000, 
                                            bribe1_unit == 3 ~ bribe1_amt*1000000))

# Check the resulted variable
view(Data_bribes[,c("bribe1_amt","bribe1_unit","bribe1_adjusted")])

# Checking the sample (KEY VARS) -  Bribe amt reported reported by a submission date
Bribe <- ggplot(Data_bribes, aes(x = Data_bribes$SubDate, y = Data_bribes$bribe1_adjusted)) + 
  geom_bar(stat = "summary", fun = "mean") +
  #fte_theme() +
  labs(title="Checking the sample: bribe key variable" ,x="Date", y="Average Bribe Reported") +
  geom_hline(yintercept=0, size=0.4, color="black")
Bribe 

# Prepare data to see correlation between sales and bribes reported
Data_corr <- subset(Data, Data$annual_sales > 0 & Data$bribe1_amt > -1 )
view(Data_corr[,c("annual_sales","bribe1_amt")]) # to check 

# Adjust amounts reported
Data_corr <- Data_corr %>%
  group_by(id) %>%
  dplyr::mutate(sales_adjusted = case_when(revenue_cat == 1 ~ annual_sales,
                                           revenue_cat == 2 ~ annual_sales*1000,
                                           revenue_cat == 3 ~ annual_sales*1000000))
Data_corr <- Data_corr %>%
  group_by(id) %>%
  dplyr::mutate(bribe1_adjusted = case_when(bribe1_unit == 1 ~ bribe1_amt,
                                            bribe1_unit == 2 ~ bribe1_amt*1000, 
                                            bribe1_unit == 3 ~ bribe1_amt*1000000))
view(Data_corr[,c("annual_sales","revenue_cat","sales_adjusted",
                  "bribe1_amt","bribe1_unit","bribe1_adjusted")]) # to check 

# Checking correlation between two vars: annual sales and reported bribery amount 
Correlation_1 <- ggplot(Data_corr, aes(x = sales_adjusted, y = bribe1_adjusted)) +
  geom_point(alpha = 3/5) +
  labs(title="Checking correlation: sales and bribe to get special treatment" ,x="Sales", y="Bribe Reported") +
  theme(#axis.text.x = element_text(angle =30, size = 8), 
    #axis.title.x=element_blank(),
    plot.title = element_text(size=8))

# Prepare data to see correlation between sales and bribes reported
Data_corr_1 <- subset(Data, Data$annual_sales > 0 & Data$bribe2_amt > -1 )
view(Data_corr[,c("annual_sales","bribe1_amt")]) # to check 

# Adjust amounts reported
Data_corr_1 <- Data_corr_1 %>%
  group_by(id) %>%
  dplyr::mutate(sales_adjusted = case_when(revenue_cat == 1 ~ annual_sales,
                                           revenue_cat == 2 ~ annual_sales*1000,
                                           revenue_cat == 3 ~ annual_sales*1000000))
Data_corr_1 <- Data_corr_1 %>%
  group_by(id) %>%
  dplyr::mutate(bribe2_adjusted = case_when(bribe2_unit == 1 ~ bribe2_amt,
                                            bribe2_unit == 2 ~ bribe2_amt*1000, 
                                            bribe2_unit == 3 ~ bribe2_amt*1000000))
view(Data_corr_1[,c("annual_sales","revenue_cat","sales_adjusted",
                    "bribe2_amt","bribe2_unit","bribe2_adjusted")]) # to check 

# Checking correlation between two vars: annual sales and reported bribery amount 
Correlation_2 <- ggplot(Data_corr_1, aes(x = sales_adjusted, y = bribe2_adjusted)) +
  geom_point(alpha = 3/5) +
  labs(title="Checking correlation: sales and bribe to avoid hassle" ,x="Sales", y="Bribe Reported") +
  theme(#axis.text.x = element_text(angle =30, size = 8), 
    #axis.title.x=element_blank(),
    plot.title = element_text(size=8)) +
  scale_x_continuous(labels = scales::comma)

# Combine plots
library(patchwork)
#patchwork <-Average_duration / Correlation_1 | Correlation_2) / Age / (Sale |Bribe) # old version
patchwork <- Average_duration / Age / (Sale |Bribe) / (Correlation_1|Correlation_2)
patchwork + plot_annotation(
  title = 'Summary: HFCs Infographic',
  subtitle = 'Reporting number of surveys submitted, Missing percentages, Checking sample obtained'
)



