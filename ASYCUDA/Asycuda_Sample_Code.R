## ---------------------------
##
## Script name: Code to clean the data and calculate delay times
##
## Author: Yana Myachenkova
##
## Date Created: 17 June 2021
##
## Last Updated: 17 June 2022
##
## ---------------------------
##
## Notes: in this script I prepare the data and construct delay time indicator
##
##
## ---------------------------
## 
## In this summary script, I am going to cover the following important modules:
## 
## 1. Prepare the data
##
## 2. Calculate delay time
## 
## 3. Explore data with delay times 
##
## ---------------------------

# Clear everything and set up working directory 
rm(list=ls())
getwd()

# Please, set up working directory when running the script on your personal computer 
setwd()
setwd("~/Downloads")

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

#------------------------------------ 1. Preparing Asycuda extracts -------------------------------#

# Upload the data
Jan_2022 <- read_excel("Asycuda_mock_pt1.xlsx") 
Jan_2022times <- read_excel("Asycuda_mock_pt2_times.xlsx") 

# Count number of declarations
Data_Jan2022_dec <- unique(Jan_2022[ ,c('OFFICE','ENTRY','TPIN','DEC_CODE','REGDATE','REGNO')]) 
Data_Jan2022times_dec <- unique(Jan_2022times[ ,c('OFFICE','REGDATE','REGNO')]) 

# Try to merge with declaration level Jan_2022 data 
Merge_Jan2022 <- dplyr::left_join(Jan_2022times, Data_Jan2022_dec)

# Check now # of declarations in the merged data set
Dec_Jan_2022 <- unique(Merge_Jan2022[ ,c('OFFICE','ENTRY','TPIN','DEC_CODE','REGDATE','REGNO')]) 

# Subset two operations: "Payment", and "Clear declaration"
Merge_Jan2022_sub <- subset(Merge_Jan2022, Merge_Jan2022$OPERATION == "Payment" | Merge_Jan2022$OPERATION == "Clear declaration")

# Check now # of declarations in the merged data set
Dec_Jan_2022_sub <- unique(Merge_Jan2022_sub[ ,c('OFFICE','ENTRY','TPIN','DEC_CODE','REGDATE','REGNO')]) 

# Subset "payment"
Merge_Jan2022_sub_payment <- subset(Merge_Jan2022, Merge_Jan2022$OPERATION == "Payment")

# Subset "clear declaration"
Merge_Jan2022_sub_cd <- subset(Merge_Jan2022, Merge_Jan2022$OPERATION == "Clear declaration") 

# Look at how many declarations we have for "payment"
P <- unique(Merge_Jan2022_sub_payment[ ,c('OFFICE','ENTRY','TPIN','DEC_CODE','REGDATE','REGNO')]) 

# Look how many declarations we have for "clear declaration"
CD <- unique(Merge_Jan2022_sub_cd[ ,c('OFFICE','ENTRY','TPIN','DEC_CODE','REGDATE','REGNO')]) 

# Create a "dup" variable indicating whether payment appears twice
Payment  <- Merge_Jan2022_sub_payment %>%
  arrange(OFFICE,ENTRY,TPIN,DEC_CODE,REGDATE,REGNO,OPERATION) %>%                     
  group_by(OFFICE,ENTRY,TPIN,DEC_CODE,REGDATE,REGNO,OPERATION) %>%                    
  mutate(dup = row_number())

# Create a "dup" variable indicating whether payment appears twice
CD  <- Merge_Jan2022_sub_cd %>%
  arrange(OFFICE,ENTRY,TPIN,DEC_CODE,REGDATE,REGNO,OPERATION) %>%                     
  group_by(OFFICE,ENTRY,TPIN,DEC_CODE,REGDATE,REGNO,OPERATION) %>%                    
  mutate(dup_cd = row_number())

# Exclude declarations for which "payment" appears twice
Payment_sub <- subset(Payment, Payment$dup < 2) # 5821

# Exclude declarations for which "clear declaration" appears twice
CD_sub <- subset(CD, CD$dup_cd < 2) # 3965

# Rename vars for operation time 
Payment_sub <- plyr::rename(Payment_sub, c("OPERATION_TIME" = "OPERATION_TIME_p"))
CD_sub <- plyr::rename(CD_sub, c("OPERATION_TIME" = "OPERATION_TIME_cd"))

# Exclude "DOCUMENT_VERSION" and "OPERATION", "CONTAINER_NUMBER", "dup", "USERNAME" vars from subsets
Payment_sub <- Payment_sub[ ,c("OFFICE", "REGDATE", "REGNO", "REGIME", "Lane At Selectivity", "Current Lane",
                               "VEHICLE_REG", "VALUE_OF_DECLARATION", "ENTRY", "TPIN", "DEC_CODE", 
                               "OPERATION_TIME_p")]
CD_sub <- CD_sub[ ,c("OFFICE", "REGDATE", "REGNO", "REGIME", "Lane At Selectivity", "Current Lane",
                     "VEHICLE_REG", "VALUE_OF_DECLARATION", "ENTRY", "TPIN", "DEC_CODE", 
                     "OPERATION_TIME_cd")]

# Merge two operations
df_merged <- dplyr::inner_join(Payment_sub, CD_sub, 
                               by = c("OFFICE", "REGDATE", "REGNO", "REGIME", 
                                      "Lane At Selectivity", "Current Lane", 
                                      "VEHICLE_REG", "VALUE_OF_DECLARATION", 
                                      "ENTRY", "TPIN", "DEC_CODE")) # 3965




#------------------------------------ 2. Calculate delay times -------------------------------#

# Calculate delay time in seconds
df_merged$delay <- as.numeric(difftime(strptime(df_merged$OPERATION_TIME_p,"%d/%m/%Y %H:%M"),
                                       strptime(df_merged$OPERATION_TIME_cd,"%d/%m/%Y %H:%M"))) 

# Calculate delay time in minutes 
df_merged$delay_minutes <- as.numeric(difftime(strptime(df_merged$OPERATION_TIME_p,"%d/%m/%Y %H:%M"),
                                               strptime(df_merged$OPERATION_TIME_cd,"%d/%m/%Y %H:%M"), units = "min")) 

# Calculate delay in hours
df_merged$delay_hours <- as.numeric(difftime(strptime(df_merged$OPERATION_TIME_p,"%d/%m/%Y %H:%M"),
                                             strptime(df_merged$OPERATION_TIME_cd,"%d/%m/%Y %H:%M"), units = "hour")) 

# what is the average delay in minutes
mean(df_merged$delay_minutes)
table(df_merged$`Lane At Selectivity`, df_merged$delay_minutes)
table(df_merged$delay_minutes)

# Let's add a dummy
df_merged$matched <- 1

# Let's calculate delay as the positive number by replacing "-"with nothing
df_merged$delay_minutes_positive <- str_replace_all(df_merged$delay_minutes, fixed("-"), "")
df_merged$delay_hours_positive <- str_replace_all(df_merged$delay_hours, fixed("-"), "")

# Make sure delay in minutes is now saved as numeric var
df_merged$delay_minutes_positive <- as.numeric(df_merged$delay_minutes_positive )
df_merged$delay_hours_positive <- as.numeric(df_merged$delay_hours_positive )




#------------------------------------ 3. Explore data with delay times -------------------------------#

# Let's see the number of border posts covered in the data
borders <- unique(df_merged$OFFICE) 
# view(borders) # if you want to check 

# Explore mean delay by border post
summary(df_merged %>% filter(OFFICE == "BIR") %>% .$delay_hours_positive, digits = 2)
summary(df_merged %>% filter(OFFICE == "BLA") %>% .$delay_hours_positive, digits = 2)
summary(df_merged %>% filter(OFFICE == "CDE") %>% .$delay_hours_positive, digits = 2)
summary(df_merged %>% filter(OFFICE == "CKA") %>% .$delay_hours_positive, digits = 2)
summary(df_merged %>% filter(OFFICE == "DED") %>% .$delay_hours_positive, digits = 2)
summary(df_merged %>% filter(OFFICE == "KIA") %>% .$delay_hours_positive, digits = 2)
summary(df_merged %>% filter(OFFICE == "LIL") %>% .$delay_hours_positive, digits = 2)
summary(df_merged %>% filter(OFFICE == "LIW") %>% .$delay_hours_positive, digits = 2)
summary(df_merged %>% filter(OFFICE == "MCH") %>% .$delay_hours_positive, digits = 2)
summary(df_merged %>% filter(OFFICE == "MQO") %>% .$delay_hours_positive, digits = 2)
summary(df_merged %>% filter(OFFICE == "MUL") %>% .$delay_hours_positive, digits = 2)
summary(df_merged %>% filter(OFFICE == "MWA") %>% .$delay_hours_positive, digits = 2)
summary(df_merged %>% filter(OFFICE == "MZU") %>% .$delay_hours_positive, digits = 2)
summary(df_merged %>% filter(OFFICE == "SWE") %>% .$delay_hours_positive, digits = 2)

# View "Lane at selectivity" var
view(df_merged$`Lane At Selectivity`)
lane <- unique(df_merged$`Lane At Selectivity`)
# view(lane) # red, yellow, blue only, for example - this is to check

# Calculate mean by lane at selectivity
df_mean_by_lane <- df_merged %>%
  group_by(`Lane At Selectivity`) %>%
  summarise(Average_delay = mean(delay_hours_positive, na.rm = T))

# E$xport result to a latex table
xtable(df_mean_by_lane)

# Calculate mean by border (to see again)
df_mean_by_border <- df_merged %>%
  group_by(OFFICE) %>%
  summarise(Average_delay = mean(delay_hours_positive, na.rm = T))

# Pie chart for the average delay time by border
ggplot(df_mean_by_border, aes(x="", y=Average_delay, fill=OFFICE)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) +
  theme_void()







