
#####################################################################################################
### Libraries                                                                                     ###
#####################################################################################################


library(rvest)
library(tidyverse)
library(stringr)

#####################################################################################################
### Function for parsing                                                                          ###
#####################################################################################################


parse_tables <- function(projections) {
  ### fomcprojections
  base_url = "https://www.federalreserve.gov"
  url = paste(base_url, projections, sep="")
  parse_projections <- read_html(url)

  df <- parse_projections %>%
    html_nodes("table") %>%
    html_table(fill = TRUE)

  ##Get table one
  df <- df[[1]]
  
  
  ##### due to that tables are a bit different.
  if (df[1,1] == "") {
    colnames(df) <- df[1,]
    df <- df[-1,]
  }
  
  #convert first row to shared columnname
  names(df) <- paste(names(df), df[1, ], sep = "_")
  
  #delete first row
  df <- df[-1,]
  
  #pivot longer
  df <- df %>%
    pivot_longer(!1, names_to = "names", values_to = "values")
  
  #divide the created column name
  df <- df %>% separate(names, c('des', 'forecast_period'), sep="_")
  
  #rename first column  
  df <- rename(df, variable = 1)
  
  return(df)
}





#####################################################################################################
### Parse all projection links                                                                    ###
#####################################################################################################

url_links = list(
   "https://www.federalreserve.gov/monetarypolicy/fomccalendars.htm"
  # "https://www.federalreserve.gov/monetarypolicy/fomchistorical2016.htm",

)

projection_links = character()

for (each in url_links) {
  
  parse_links <- read_html(each)
  
  
  links <- parse_links %>% html_nodes("a") %>% html_attr("href")
  links <- links[which(regexpr('.htm', links) >= 1)] 
  links <- links[which(regexpr('fomcprojtabl', links) >= 1)] 
  
  #print(each)
  projection_links <- c(projection_links, links)
}

####Add links for 2015 (not on same page as for others)
projection_links <- c(projection_links, "/monetarypolicy/fomcminutes20151216ep.htm")
projection_links <- c(projection_links, "/monetarypolicy/fomcminutes20150917ep.htm")


#####################################################################################################
### Create an empty data frame                                                                    ###
#####################################################################################################

proj_table <- data.frame(matrix(ncol = 5, nrow = 0))

#provide column names
colnames(proj_table) <- c('variable', 'des', 'forecast_period', 'values', 'date')



#####################################################################################################
### Iterates all projection links and parse the tables                                            ###
#####################################################################################################


for (projections in projection_links) {
  Sys.sleep(sample(3:9, 1, replace=T))
  df <- parse_tables(projections)

  df <- df %>%
    mutate(date = as.numeric(gsub("\\D", "", projections)))
  
  
  proj_table <- bind_rows(proj_table, df)
  
  #print(projections)
  }


#####################################################################################################
### Some additional cleaning and wrangling                                                        ###
#####################################################################################################

proj_table <- proj_table %>%
  select(-.copy)%>%
  mutate(projection = date)%>%
    mutate(date = as.Date(gsub("\\D", "", date), format = "%Y%m%d"))%>%
  mutate(forecast_period = as.Date(paste(forecast_period, 12, 31, sep = "-")))%>%
 mutate(values = as.numeric(values))%>%
  mutate(meeting_month = months(date))%>%
  mutate(projection_year = substr(projection, 1, 4))


#####################################################################################################
### Save as csv                                                                                   ###
#####################################################################################################


write.table(proj_table, "data//projections_table.csv", sep=",")





