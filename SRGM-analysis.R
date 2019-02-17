# - January 2018
#Master Sheet
#this version is for the master csv
# Copyright 2019 The MITRE Corporation. All Rights Reserved.
# This technical data was produced for the U. S. Government under Contract No. FA8702-18-C-0001, 
# and is subject to the Rights in Technical Data-Noncommercial Items Clause DFARS 252.227-7013 (JUN 2013)
# Approved for Public Release; Distribution Unlimited. Public Release Case Number 19-0034

# Author: Barry von Tobel, Principal Enginner, MITRE
#
# the purpose of this R Script is two-fold
# 1) using the JIRA export evaluate a Mean Time between Fault(failure) - MTBF and Mean Time to Fix Code(Repair) - MTTR
#    these values are used for determining contractor obligations on delivering code and for calculating A sub Zero (A0)
#    further effort is required to tweak these algorithms,but whatever the vaule, this code gives consistency to monthly reports
#     * the MTTR is the summary statistics of the data, coerced into a 50 bin histogram of the number of days between 
#      created and closed, created using data since inception
#     * the MTBF is a probability density function which calculates a cummulative value for the last 52 weeks, the density
#       is the (number of defects per work week) / (hours per work week)
# 2) Manipulate the JIRA export data to a form acceptable by the SFRAT, https://sasdlc.org/lab/projects/srt.html
#    

# Setting the working directory
# or else comment out the setwd and set it by selecting 'session/'set working directory'
setwd("/home/bvt/eng_science/R/wd/DF")

#Load required packages
#library(gridExtra)
library(moments) # needed for skewness
library(lubridate)  # needed for dates
library(dplyr) # part of the tidyverse modules
library(qwraps2) # needed for summary_table, Linux needs r-cran-devtools
library(ggplot2)
#options(qwraps2_markup = "markdown") qwraps may be removed, looking at making this a R Markdown product


#Import the DF data from JIRA export and manipulate it to a form for analysis
DF_Master <- read.csv("JiraNov30.csv", sep = ",", skip = 1, na.strings = " ")

# Create a data frame
data_summary <- data.frame(Key = DF_Master$Key, Status = DF_Master$Status, Type = DF_Master$Type,
                           closed = DF_Master$Resolved, created = DF_Master$Created, Labels = DF_Master$Labels,
                           priority_char = DF_Master$MST.Priority) 
# if you get an error in data.frame the last value imported, before the error may be variable mismatch

#add the year column and get the year from the created column
data_summary["year"] <- NA # Add the year column's name, this row creates the column's name

a=dmy(data_summary$created) # Lubridate function creates a 'a' value of array, i.e. "2017-06-02"
b=dmy(data_summary$closed)
 # add to the blank year column which is the year less the 2000 from the year extracted from 'a', i.e. "17"
year(a) - 2000 -> data_summary$year # https://stackoverflow.com/questions/36568070/extract-year-from-date
#add the week column
# get a numeric value for priority by extracting the first character
data_summary$priority <- substr(data_summary$priority_char, 1, 1) 
# add the week column
data_summary["week"] <- NA
# add to the blank week colum which is the week extracted from the 'a' value, i.e. "22"
#https://stackoverflow.com/questions/45061758/create-an-unique-week-variable-not-depending-on-the-calendar-in-r
week(a) -> data_summary$week
# add the yearweek column
data_summary["yearweek"] <- NA
# create a number which represents the year and week, i.e. "2017.22"
data_summary$yearweek <- year(a) + week(a)/100
# create the yearweek2 column and coerce it into a number
# create ordered levels
data_summary$yearweek2 <-as.numeric(as.factor(data_summary$yearweek))


#add the days to close column, https://cran.r-project.org/web/packages/lubridate/vignettes/lubridate.html
data_summary["Days2Close"] <- NA # create the header
# needed to use absolute value because some dates interval are negative
x <- abs(interval(a,b)) 
# troubleshooting - see if abs is needed
# bug <- (interval(a,b))
# round to nearest integer to arrive a a good guess on workdays by using 5/7
data_summary$Days2Close <- round(x / ddays(1) * 5/7) 
#
# MTTR and MTBF data analysis section
#
# create a "Days to Close" (MTTR) histogram of the days to close with 50 bins
h <- hist(data_summary$Days2Close, breaks = 50, col = NULL)
# plot the histogram and label appropiately
plot(h, main = " Days to Close Histogram \n for all years", xlab = "Number of Days", ylab = "Closed per Day")
# create a probability density function
d <- density(data_summary$Days2Close, na.rm=T) # if you need to compare - https://www.statmethods.net/graphs/density.html 
# plot the probability density function, the area under the curve = 1, a normalized distribution
# plot(d, main = " Probability Density Estimate in Days to Close \n for all years", xlab = "Number of Days", ylab = "Closed per Day")


ggplot(data_summary, aes(x=Days2Close)) +
    geom_density() +
    labs(title="Probability Density Estimate in Days to Close \n for all years",
     x = "Number of Days less outliers",
     y = "Density") +
     xlim (c(0,500)) #"remove outliers"

ggplot(data_summary, aes(x=Days2Close)) + stat_ecdf(geom = "line") +
    labs(title="Empirical Cummulative Density in Days to Close for all years",
    x = "Number of Days less outliers",
    y = "Density") +
    xlim (c(0,500)) #"remove outliers"

# Measure Central Tendency summary statistics
# using qwraps2 for presentation - https://cran.r-project.org/web/packages/qwraps2/vignettes/summary-statistics.html
# qwraps2 may be replaced in the future
our_summary1 <-
  list("Days to Close" =
      list("min" =~ min(Days2Close, na.rm = T), 
           "max" = ~ max(Days2Close, na.rm = T),
           "mean" = ~ mean(Days2Close, digits = 2, na.rm = T),
           "median" = ~ median(Days2Close, na.rm = T),
           "SD" = ~ sd(Days2Close, na.rm = T),
           "variance" = ~ var(Days2Close, na.rm = T),
           "IQR" = ~ IQR(Days2Close, na.rm = T),
           "q25" = ~ quantile(Days2Close, 0.25, na.rm = T),
           "q75" = ~ quantile(Days2Close, 0.75, na.rm = T),
           "skew" = ~ skewness(Days2Close, na.rm = T),
           "kurtosis" = ~ kurtosis(Days2Close, na.rm = T))
  )

x <- summary_table(data_summary, our_summary1)
# print out qwraps2_summary_table
as.data.frame(x)
#

# Fault density analysis (MTTR)
# filter for only the last 52 weeks find the latest week number and use this and the prior 51 weeks.
# determine the last week number
z <- max(data_summary$yearweek2, na.rm = TRUE)
# Populate the summary table to only include the data from the last 52 weeks
data_filtered <- filter(data_summary, (max(data_summary$yearweek2, na.rm = TRUE) - data_summary$yearweek2) <= 51)
# find the nuber of entries
rows.filtered <- nrow(data_filtered)
cat("number of rows in filtered data set = ", rows.filtered)
# filter only priority 2 faults, and anything else would go here
priority_filtered <- filter(data_filtered, priority == 2)
# now see the number of rows
num.rows <- nrow(priority_filtered)
cat("number of rows - priority filtered = ", num.rows)

# build a contingency table, first column is the week number, second is the frequency of priority 2s per week number
# this is similar to a spreadsheet's pivot table and shows the distribution, preferably you will eventually see
# a reduction in created defects
table1 <- table(priority_filtered$yearweek2, priority_filtered$priority)
# although table1 shows as numeric, the values shows ''table' int'', so the as.numeric fuction below gives 'num'
table1 <- as.numeric(table1)

# put back into dataframe
priority.df <- as.data.frame(table1)

# add back in the week number to the newly created data frame
priority.df$week <- 1:nrow(priority.df)
#add back in the yearweek, from March 2015
priority.df$yearweek <- (max(data_summary$yearweek2, na.rm = TRUE) - 52 + priority.df$week)
# need to automate the 'per week as' date
plot(priority.df$year, priority.df$table1, type ="h", col = "dark red", main = "Created Defects Per Week as of 30 November 18", xlab = "Last 52 weeks as of March 2015", ylab = "Defects per Week")

#create IEEE table for use in SFRAT, T = week, FC = faults, CFC = Cumulative Faults
priority.df <- rename(priority.df, "FC" = "table1", "T" = "week")
# caculate the cumulative sum
priority.df$CFC <- cumsum(priority.df$FC)
#change the order of columns, T, FC, CFC # this is the format needed for the SFRAT input
priority.df[c(2,1,4)] 
# caculate the hours per fault, cumulative density assuming 40 hour work week old method
# equation: (number per week * 40 hours per week)/(cummulative fautls for the 52 week period)
priority.df$density <- round((priority.df$T * 40) / priority.df$CFC,2)
# Summarize the results
summary(priority.df$density)
# save to file two csv files, one for the presentation, and the other for the SFRAT tool
write.csv(priority.df[c(2,1,4,5)], file = "presentation.csv") # create a table for the presentation
# before using SFRAT, the csv file needs the first column removed
write.csv(priority.df[c(2,1,4)], file = "SFRAT_Data.csv") # create a table for the SFRAT Tool

