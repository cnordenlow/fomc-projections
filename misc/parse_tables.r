
library(rvest)
library(tidyverse)



### fomcprojections
parse_projections <- read_html(url)
tbls <- html_nodes(parse_projections, "table")
head(tbls)
tbls %>% html_attr("tablesubhead")



projections <- parse_projections %>%
  html_nodes("table") %>%
  # .[3:4] %>%
  html_table(fill = TRUE)#%>%
  #.[[1]]


##Get table one
df <- projections[[1]]

#convert first row to shared columnname
names(df) <- paste(names(df), df[1, ], sep = "_")

#delete first row
df <- df[-1,]

#pivot longer
df <- df %>%
  pivot_longer(!Variable_Variable, names_to = "names", values_to = "values")

#divide the created column name
df <- df %>% separate(names, c('des', 'period'), sep="_")





#########Get all links
#page <- read_html("http://www.yelp.com/search?find_loc=New+York,+NY,+USA")
parse_links <- read_html("https://www.federalreserve.gov/monetarypolicy/fomccalendars.htm")


links <- parse_links %>% html_nodes("a") %>% html_attr("href")
links <- links[which(regexpr('.htm', links) >= 1)] ##.htm
links <- links[which(regexpr('fomcprojtabl', links) >= 1)] ##.htm

as.data.frame(links)




#get heads
##head
head <- parse_projections %>% 
  html_nodes('.tablehead') %>%
  gsub(".*>(.+)<.*", "\\1", .)

##table sub head

subhead <- parse_projections %>% 
  html_nodes('.tablesubhead') %>%
  gsub(".*>(.+)<.*", "\\1", .)

#hej <- "<h6 class=tablesubhead id=xt4p1>PCE inflation</h6>"
#gsub(".*>(.+)<.*", "\\1", hej)





#test
web_bls <- read_html("http://www.bls.gov/web/empsit/cesbmart.htm")

tbls <- html_nodes(web_bls, "table")  #extract all table nodes that exist on the page.

head(tbls)


#To parse the HTML, we use html_table. In this example it creates
table_bls <- web_bls %>%
  html_nodes("table") %>%
  .[3:4] %>% ##determines which tables. In this case, table 3 and 4.
  html_table(fill = TRUE)

str(table_bls)

#Extract table 2, non-farm
head(table_bls[[2]], 4)

# remove row 1 that includes part of the headings. Not neccessary here
#table_bls[[2]] <- table_bls[[2]][-1,]


table_bls2 <-table_bls[[2]]