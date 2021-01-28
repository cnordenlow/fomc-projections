#####################################################################################################
### Get Economic Data                                                                             ###
#####################################################################################################

### Source for PCE inflation and GDP data are BEA. BEA has an excellent API that is used. https://apps.bea.gov/API/signup/index.cfm
### Source for Unemployment data is BLS. BLS also has a great API but for the moment I´m using Rvest.
### Source for Fed Funds rate are my github. 


#https://www.bls.gov/developers/


#BLS.gov cannot vouch for the data or analyses derived from these data after the data have been retrieved from BLS.gov.

#####################################################################################################
### Libraries                                                                                     ###
#####################################################################################################

library(httr)
library(devtools)
library(bea.R)
library(tidyverse)
library(stringr)
library(lubridate)
library(rvest)


#####################################################################################################
### Get Fed Funds rate                                                                            ###
#####################################################################################################


fedFunds <- read.table(
  "https://raw.githubusercontent.com/cnordenlow/text-mining-fomc/main/Data/fedFundsRate.csv", 
  sep=",", header=TRUE)


#Change dates
fedFunds <- fedFunds %>%
  mutate(date = as.Date(gsub("\\D", "", date), format = "%Y%m%d"))


#Arrange
fedFunds <- arrange(fedFunds, date)

fedFunds <- fedFunds%>%
  filter(bound == "lower_bound")%>% ###Just using the lower bond in case the indata file have missing values for lower or upper bound. THe problem will be easier to handle with a fill if I only use the lower_bound and adding 0.125
  select(-bound)%>%
  mutate(rate = round(rate + 0.125,2))%>%
  mutate(LineDescription = "Federal funds rate")



#####################################################################################################
### Get Inflation and GDP data                                                                    ###
#####################################################################################################

#Individual code
beaKey <- 'Register at BEA'
beaSearch('Personal consumption expenditures (PCE)', beaKey)


#Inflation

beaSpecs <- list(
  'UserID' = beaKey ,
  'Method' = 'GetData',
  'datasetname' = 'NIPA',
  'TableName' = 'T20804',
  'Frequency' = 'M',
  'Year' = '2011, 2012,2013,2014,2015,2016,2017,2018,2019,2020',
  'ResultFormat' = 'json'
);
beaPayload <- beaGet(beaSpecs);




inflation_data <- beaPayload %>%
  select(-c("TableName", "SeriesCode", "LineNumber", "METRIC_NAME", "CL_UNIT", "UNIT_MULT"))%>%
  pivot_longer(!LineDescription,
               names_to = "des",
               values_to = "values")%>%
  #Fix dates  
  mutate(des = str_sub(des,-7,-1))%>%
  mutate(year = sub("\\M.*", "",des))%>%
  mutate(month = str_sub(des,-2,-1))%>%
  mutate(date = paste(year,month, sep="-"))%>%
  mutate(date = as.Date(paste(date,"-01",sep="")))%>% #convert to dateformat
  mutate(date = as.Date((date) + months(1) - days(1)))%>%  #change to end of month
  select(LineDescription, date, values) %>%
  #Calculate YoY chg
  arrange(date)%>%
  group_by(LineDescription) %>%
  mutate(rate = round((values - lag(values, n = 12, default = NA)) / lag(values, n = 12, default = NA) * 100,2)) %>% #YoY
  #mutate(MoM = round((values - lag(values, n = 1, default = NA)) / lag(values, n = 1, default = NA) * 100,2)) %>% #MoM
  #Select PCE
  filter(LineDescription %in% c("Personal consumption expenditures (PCE)", "PCE excluding food and energy"))%>%
  arrange(date)%>%
  select(-values)


# GDP


beaSpecs <- list(
  'UserID' = beaKey ,
  'Method' = 'GetData',
  'datasetname' = 'NIPA',
  'TableName' = 'T10111',
  'Frequency' = 'Q',
  'Year' = '2011, 2012,2013,2014,2015,2016,2017,2018,2019,2020',
  'ResultFormat' = 'json'
);
beaPayload <- beaGet(beaSpecs);




gdp_data <- beaPayload %>%
  select(-c("TableName", "SeriesCode", "LineNumber", "METRIC_NAME", "CL_UNIT", "UNIT_MULT"))%>%
  pivot_longer(!LineDescription,
               names_to = "des",
               values_to = "values")%>%
  #Fix dates  
  mutate(des = str_sub(des,-6,-1))%>%
  mutate(year =str_sub(des,1,4))%>%
  mutate(month = as.numeric(str_sub(des,-1,-1)) * 3)%>%
  mutate(date = paste(year, month, sep="-"))%>%
  mutate(date = as.Date(paste(date,"-01",sep="")))%>% #convert to dateformat
  mutate(date = as.Date(date) + months(1) - days(1))%>%  #change to end of month
  select(LineDescription, date, values) %>%
  rename(rate = values)%>%
  arrange(date)%>% #YoY
  filter(LineDescription %in% c("Gross domestic product (GDP)"))



#####################################################################################################
### Get Unemployment Data                                                                         ###
#####################################################################################################


url = "https://www.bls.gov/charts/employment-situation/civilian-unemployment-rate.htm"


### fomcprojections
parse_projections <- read_html(url)


labor_data <- parse_projections %>%
  html_nodes("table") %>%
  html_table(fill = TRUE)

labor_data <- labor_data[[1]]


labor_data <- labor_data %>%
  mutate(month = substr(Month, 1, 3))%>%
  mutate(year = str_sub(Month,-4,-1))%>%
  mutate(month = match(month,month.abb))%>%
  mutate(date = paste(year,month, sep="-"))%>%
  #mutate(date = paste(year,match(df$month,month.abb),sep="-"))%>% #change month abr to index
  mutate(date = as.Date(paste(date,"-01",sep="")))%>% #convert to dateformat
  mutate(date = as.Date(date) + months(1) - days(1))%>%  #change to end of month
  select(date, Total)%>%
  rename(rate = Total)%>%
  mutate(LineDescription = "Unemployment rate")


#####################################################################################################
### Merge Data tables                                                                             ###
#####################################################################################################


###Create dataframe  with dates from 2011
dataTable <- data.frame(
  date = seq(as.Date('2011-12-01'), Sys.Date(), by = 'days')
)


dataTable <- bind_rows(dataTable, fedFunds)
dataTable <- bind_rows(dataTable, inflation_data)
dataTable <- bind_rows(dataTable, gdp_data)
dataTable <- bind_rows(dataTable, labor_data)

dataTable <- arrange(dataTable, date)

dataTable <- dataTable %>%
  pivot_wider(date, names_from = LineDescription, values_from = rate)%>%
  select(-"NA")

dataTable <- dataTable %>% fill(names(dataTable))

dataTable <- dataTable %>%
  pivot_longer(!date, names_to = "LineDescription", values_to = "rate")


df <- df %>%
  mutate(date = as.Date(as.character(date)))


dataTable <- dataTable %>%
  filter(date > "2011-12-01")





write.table(dataTable, "data//dataTable.csv", sep=",")



