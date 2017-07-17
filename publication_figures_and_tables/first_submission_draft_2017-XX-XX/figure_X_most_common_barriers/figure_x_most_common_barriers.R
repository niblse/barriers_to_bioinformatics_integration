#Summary of  Most Commonly Reported Barriers by Category (n=1231)

# load required libraries
require(ggplot2)
require(tidyverse)
require(ggthemes)
require(scales)


total.scored.cols <- read_csv("../../../analysis_scripts/top5_plots/ouput_tables/scored_values.csv")
sample.size <- 1231


faculty_colors <- c("#ff0000","#b30000","#ff4d4d","#800000","#ff9999","#330000")
#facilities_colors <- c(2)
#resources_colors <- c(3)
student_colors <- c("#00ffff", "#00b3b3", "#4dffff", "#006666", "#99ffff", "#003333")
curriculumn_colors <- c("#cc00cc", "#800080", "#4dffff", "#ff1aff", "#4d004d", "#ff99ff" )
#Institutional_colors <- c(2)

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

color_values <- c(faculty_colors[1], student_colors[1:2], faculty_colors[2], curriculumn_colors[1:2])

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
  theme(legend.key.size = unit(1.5, 'lines'))+
  geom_text(aes(label = paste0(round(percentage *100), "%"), y = percentage),
            vjust =-.2, size = 10, color = "black")+
  scale_x_discrete(breaks = NULL)+
  guides(fill=guide_legend(ncol=2)) 


ggsave( filename= "./top_barriers_plot.png", 
        width = 18.8, 
        height = 8.81, 
        units = "in")

