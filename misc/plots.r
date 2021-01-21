
library(tidyverse)

####Test
proj_table <- read.table(
  "projections_table.csv",  ##på detta sätt läser den ur aktuell mapp
  sep=",", header=TRUE)

proj_table_specials <- read.table(
  "projections_table_specials.csv",  ##på detta sätt läser den ur aktuell mapp
  sep=",", header=TRUE)


proj_table <- rbind(proj_table, proj_table_specials) 

###Fix df with dates
dates <- data.frame(
date = seq(as.Date('2011-12-01'), Sys.Date(), by = 'days'))
  




test <- proj_table %>%
  filter(variable %in% c("Federal funds rate"))%>%
 # filter(variable %in% c("Change in real GDP"))%>%
  #  filter(variable %in% c("Change in real GDP"))%>%
  #  filter(variable %in% c("Unemployment", "PCE inflation", "Change in real GDP","Federal funds rate"))%>%
  filter(des %in% c("Median 1", "Median1"))%>%
  na.omit()%>%
#  mutate(meeting_month = months(date))%>%
  filter(meeting_month %in% c("december"))%>%
  mutate(values = as.numeric(values))%>%
  mutate(projection = substr(projection, 1, 4))


test <- test %>%
#  mutate(projection = date)%>%
  select(forecast_period, values,projection)



############Fed funds

###fed funds rate


df2 <- read.table(
  "https://raw.githubusercontent.com/cnordenlow/text-mining-fomc/main/Data/fedFundsRate.csv", 
  sep=",", header=TRUE)


#Change dates
df2 <- df2 %>%
  mutate(date = as.Date(gsub("\\D", "", date), format = "%Y%m%d"))


#arrange
df2 <- arrange(df2, date)

df2 <- df2%>%
  filter(bound == "lower_bound")%>%
  select(-bound)

### merge with fed funds and date and projections
test2 <- merge(dates, df2, by = "date", all.x = TRUE)

test2 <- test2 %>%
  fill(rate, .direction = "down")

test2 <- merge(test2, test, by.x = "date", by.y ="forecast_period", all = TRUE)

#change na projections to ""
test2$projection[is.na(test2$projection)] <- ""




# Visualization
#ggplot(test2, aes(x = date, y = values, group = projection)) + 
 # geom_line(linetype="dashed")+
  #geom_line(aes(y=rate), size=1, color="blue")
  
#fixa max och min fÃ¶r fed  funds och projections
max_y = round(max(test$values) +0.5,1)
max_y2 = max(df2$rate)


# Visualization
p <- ggplot(test2, aes(x = date, y = rate)) + 
  geom_line(size = 1.1)+
  geom_area(fill = "lightgrey")+
  geom_line(aes(y=values, group = projection, color = projection), linetype="dashed", size=1.1)+
 # geom_line(aes(y=values, group = projection), linetype="dashed", size=1)+
  theme_minimal() +
  
  theme(legend.position="none",
        legend.title = element_blank(),
        plot.caption=element_text(hjust=0),
        plot.subtitle=element_text(face="italic"),
        plot.title=element_text(size=16,face="bold"))+

  labs(x="",y="Fed Funds rate",
       title="Fed Funds rate and projections",
       subtitle="c_subtitle",
       caption="Source = FOMC Projections")+
  ylim(0, max_y)+
  scale_fill_continuous(guide = guide_colourbar())+
  transition_reveal(date)



library(gganimate)

library(gifski)

#animate(p, height=400, width=600, end_pause = 30, renderer=gifski_renderer(loop = FALSE))
anim_save("test.gif",animate(p, height=400, width=600, end_pause = 30, renderer=gifski_renderer(loop = FALSE)))




#### FOMC projections
```{r, echo=FALSE, messages=FALSE, warning =FALSE}

temp <- proj_table %>%
  filter(variable %in% c("Federal funds rate"))%>%
  filter(meeting_month %in% c("december"))



temp <- merge(fedFunds, temp, by.x = "date", by.y ="forecast_period", all = TRUE)

#change na projections to ""
temp$projection_year[is.na(temp$projection_year)] <- ""




#fixa max och min fÃ¶r fed  funds och projections
max_y = round(max(temp$values) +0.5,1)
max_y2 = max(fedFunds$rate)


# Visualization
ggplot(temp, aes(x = date, y = rate)) + 
  geom_line(size = 1)+
  geom_area(fill = "lightgrey")+
  geom_line(aes(y=values, group = projection_year, color = projection_year), linetype="dashed", size=1)+
  #  geom_line(aes(y=values, group = projection_year), linetype="dashed", size=1)+
  theme_minimal() +
  
  theme(legend.position="none",
        #legend.position = "bottom",
        legend.title = element_blank(),
        plot.caption=element_text(hjust=0),
        plot.subtitle=element_text(face="italic"),
        plot.title=element_text(size=16,face="bold"))+
  
  labs(x="",y="Fed Funds rate",
       title="Fed Funds rate and projections",
       subtitle="c_subtitle",
       caption="Source = FOMC Projections")+
  ylim(0, max_y)+
  scale_fill_continuous(guide = guide_colourbar())




```

 