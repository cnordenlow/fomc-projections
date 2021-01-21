
#####################################################################################################
### Libraries                                                                                     ###
#####################################################################################################


library(rvest)
library(tidyverse)
library(stringr)



### special fomcprojections. need to calculate it yourself from 2014 and backwards

#####################################################################################################
### Function for parsing and calculating median                                                   ###
#####################################################################################################

parse_tables_specials <- function(projections) {
  
  base_url = "https://www.federalreserve.gov"
  url = paste(base_url, projections, sep="")
  parse_projections <- read_html(url)
  
  
  df <- parse_projections %>%
    html_nodes("table") %>%
    html_table(fill = TRUE)
  
  
  ####This depends on which minutes, year
  date = as.numeric(gsub("\\D", "", substr(projections, 1, 40))) ##hummz
  
  if (date == 20131218) {
    df <- df[[6]]
  }
  
  if (date == 20141217) {
    df <- df[[6]]
  }
  
  if (date == 20111213) {
    df <- df[[9]]
  }

  #calculate median, needs to ungroup it
  df <- df %>%
    pivot_longer(!1, names_to = "forecast_period", values_to = "values")%>%
    rename(rate = 1)%>%
    filter(!forecast_period %in% c("Longer run", "Longer Run"))%>%
    na.omit(values)%>%
    uncount(values)%>%
    group_by(forecast_period)%>%
    summarize(values = median(rate))
  
  
  return(df)
}

#####################################################################################################
### Links to parse                                                                                ###
#####################################################################################################


projection_links_specials = list(
  "/monetarypolicy/fomcprojtabl20131218.htm",
  "/monetarypolicy/fomcminutes20141217epa.htm#figure2", ###special
  "/monetarypolicy/files/FOMC20111213material.htm") ###even more special


#####################################################################################################
### Create an empty data frame                                                                    ###
#####################################################################################################

#create data frame with 0 rows and 5 columns
proj_table_specials <- data.frame(matrix(ncol = 5, nrow = 0))

#provide column names
colnames(proj_table_specials) <- c('variable', 'des', 'forecast_period', 'values', 'date')



#####################################################################################################
### Iterates all projection links and parse the tables                                            ###
#####################################################################################################


for (projections in projection_links_specials) {
    Sys.sleep(sample(3:9, 1, replace=T))
    df <- parse_tables_specials(projections)

    df <- df %>%
      mutate(date = as.numeric(gsub("\\D", "", substr(projections, 1, 40))),
             variable = "Federal funds rate",
             des = "Median1"
             )

    proj_table_specials <- bind_rows(proj_table_specials, df)

  }
  

#####################################################################################################
### Some additional cleaning and wrangling                                                        ###
#####################################################################################################


proj_table_specials <- proj_table_specials %>%
 # select(-.copy)%>%
  mutate(projection = date)%>%
  mutate(date = as.Date(gsub("\\D", "", date), format = "%Y%m%d"))%>%
  mutate(forecast_period = as.Date(paste(forecast_period, 12, 31, sep = "-")))%>%
  mutate(values = as.numeric(values))%>%
  mutate(meeting_month = months(date))%>%
  mutate(projection_year = substr(projection, 1, 4))

#####################################################################################################
### Save as csv                                                                                   ###
#####################################################################################################


write.table(proj_table_specials, "data//projections_table_specials.csv", sep=",")
