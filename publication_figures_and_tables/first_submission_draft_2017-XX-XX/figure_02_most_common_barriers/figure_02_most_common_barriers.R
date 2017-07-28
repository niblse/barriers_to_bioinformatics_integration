#Summary of  Most Commonly Reported Barriers by Category (n=1231)

# load required libraries
require(ggplot2)
require(tidyverse)
require(ggthemes)
require(scales)


total.scored.cols <- read_csv("../../../analysis_scripts/top5_plots/ouput_tables/scored_values.csv")
sample.size <- 1231


reds<- c("#F6001D","#B40015","#68000C","#68000C","#FB697A")
greens <- c("#2EDB00","#135B00","#219E00","#4AE821","#80EE64")
yellows <- c("#FFC300", "#BC9000", "#6C5300", "#FFCC24", "#FFDC6B")
blues <- c("#766FDF","#1A10BA", "#120B83", "#08044C", "#4037D2" )
greys <- c("#252525", 
           "#636363", 
           "#969696", 
           "#bdbdbd", 
           "#d9d9d9", 
           "#f7f7f7")
           


#plot q33 scored by itself

label.df.s.33 <- list("q1.Q33" = "Q33: What do you think are the most important challenges currently facing those educating\n undergraduate life scientists in bioinformatics?")

facet_labeller.score.33 <- function(variable,value){
  return(label.df.s.33[value])
  
}



# filter out Q33 and order for plotting
total.scored.cols <- total.scored.cols%>%
  filter(question == "q1.Q33")%>%
  filter(percentage >= 0.05)%>%
  group_by(question)%>%
  arrange(.,desc(percentage))


total.scored.cols$barrier <- factor(total.scored.cols$barrier, levels = total.scored.cols$barrier[order(desc(total.scored.cols$percentage))])

color_values <- c(reds[1], greens[1:2], reds[2], blues[1:2])

total.scored.cols%>%
  ggplot()+
  aes(x = barrier, y=percentage, fill = barrier)+
  geom_bar(stat = "identity", position = "dodge")+
  facet_grid(~ question)+
  xlab("Scored barrier categories")+
  ylab("Percentage of respondents reporting barrier")+
  scale_fill_discrete(name="Barrier categories")+
  theme_fivethirtyeight(base_size = 25, base_family = "sans")+
  theme(panel.background = element_rect(fill = "white"))+
  theme(plot.background = element_rect(fill = "white"))+
  theme(legend.background = element_rect(fill = "white"))+
  theme(strip.text.y = element_blank())+
  theme(strip.background = element_blank(),strip.text.x = element_blank())+
  theme(axis.text.x = element_blank())+
  theme(axis.text.y = element_blank())+
  theme(legend.key.size = unit(1.5, 'lines'))+
  geom_text(aes(label = paste0(round(percentage *100), "%"), y = percentage),
            vjust =-.2, size = 10, color = "black")+
  scale_x_discrete(breaks = NULL)+
  guides(fill=guide_legend(ncol=2))+
  theme(axis.line.y = element_line(colour = "black"))+
  theme(axis.line.x = element_line(colour = "black"))+
  scale_fill_manual(values= greys, name= "Barriers")+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank())

  


ggsave( filename= "figure_02.png", 
        width = 18.8, 
        height = 8.81, 
        units = "in")





