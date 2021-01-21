


temp <- proj_table %>%
  filter(variable %in% c("Federal funds rate"))%>%
  filter(meeting_month %in% c("december"))

#Max y
max_y = round(max(temp$values) +0.5,1)

temp <- merge(fedFunds, temp, by.x = "date", by.y ="forecast_period", all = TRUE)

#change na projections to ""
temp$projection_year[is.na(temp$projection_year)] <- ""






p <- ggplot(temp, aes(x = date, y = rate)) + 
  geom_line(size = 1)+
  geom_area(fill = "lightgrey")+
  geom_line(aes(y=values, group = projection_year, color = projection_year), linetype="dashed", size=1)+
  #  geom_line(aes(y=values, group = projection_year), linetype="dashed", size=1)+
  theme_minimal() +
  
  theme(legend.position="none",
      #  legend.position = "bottom",
        legend.title = element_blank(),
        plot.caption=element_text(hjust=0),
        plot.subtitle=element_text(face="italic"),
        plot.title=element_text(size=16,face="bold"))+

  labs(x="",y="Fed Funds rate",
       title="Fed Funds rate and projections",
       subtitle="Fed Funds rate and end of year projections (December meeting)",
       caption="Source = FOMC Projections")+
  
  scale_x_date(date_breaks = "2 year",
               date_labels = "%Y")+
  theme(
    panel.grid.minor = element_blank()
  )+
  
  ylim(0, max_y)+
  transition_reveal(date)

anim_save("animatedProjections.gif",animate(p, duration = 15, renderer=gifski_renderer(loop = FALSE)))
