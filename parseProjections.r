
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

  
  
  #########Normal tables: After sep 2015
  
  if (special_tables == 0) {
  
  
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
  }
  
  #########Special tables: 2011-2014
  
  if (special_tables == 1) {
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
    #filter(!forecast_period %in% c("Longer run", "Longer Run"))%>%
    na.omit(values)%>%
    uncount(values)%>%
    group_by(forecast_period)%>%
    summarize(values = median(rate))
  
  df$values <- as.character(df$values)
  
  }
  
  
  return(df)
}





#####################################################################################################
### Parse all projection links efter 2015                                                         ###
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


#projection_links <- c("/monetarypolicy/fomcprojtabl20131218.htm")

##Special links
projection_links_specials = list(
  "/monetarypolicy/fomcprojtabl20131218.htm",
  "/monetarypolicy/fomcminutes20141217epa.htm#figure2", ###special
  "/monetarypolicy/files/FOMC20111213material.htm") ###even more special



#####################################################################################################
### Create an empty data frame                                                                    ###
#####################################################################################################

proj_table <- data.frame(matrix(ncol = 5, nrow = 0))

#provide column names
colnames(proj_table) <- c('variable', 'des', 'forecast_period', 'values', 'date')



#####################################################################################################
### Iterates all projection links and parse the tables                                            ###
#####################################################################################################



#non specials (after 2015)
for (projections in projection_links) {
  special_tables = 0
  Sys.sleep(sample(3:9, 1, replace=T))
  df <- parse_tables(projections)

  df <- df %>%
    mutate(date = as.numeric(gsub("\\D", "", projections)))
  
  
  proj_table <- bind_rows(proj_table, df)
  
  #print(projections)
  }


#specials (before 2015)
for (projections in projection_links_specials) {
  special_tables = 1
  
  Sys.sleep(sample(3:9, 1, replace=T))
  df <- parse_tables(projections)
  
  df <- df %>%
    mutate(date = as.numeric(gsub("\\D", "", substr(projections, 1, 40))),
           variable = "Federal funds rate",
           des = "Median1"
    )
  
  proj_table <- bind_rows(proj_table, df)
  
}


#####################################################################################################
### Some additional cleaning and wrangling, write table                                           ###
#####################################################################################################


if (".copy" %in% names(proj_table)){
proj_table <- proj_table %>%
  select(-.copy)
}



write.table(proj_table, "data//projections_table.csv", sep=",")






