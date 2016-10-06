#some attempts at plots
library(ggplot2)
library(tidyverse)

#load dataframe

df <- read_csv("./summed_reduced_factors.csv")


#substitutuions - must be done before conerting to factor
df$Q14_Sex[df$Q14_Sex == "1"] <- "Male"
df$Q14_Sex[df$Q14_Sex == "2"] <- "Female"
df$Q14_Sex[df$Q14_Sex == "3"] <- "Unknown"

#change to factors
#df <- df %>% mutate_if(is.character,as.factor)

# change numerics to factors
#df <- df %>% mutate_if(is.numeric,as.factor)


data_to_plot <- data.frame(sum(df[,8]),
                          sum(df[,10]),
                          sum(df[,12]),
                          sum(df[,14]),
                          sum(df[,16]),
                          sum(df[,18]),
                          sum(df[,20]),
                          sum(df[,22]))
ggplot(df)+
  geom_bar(aes(Q14_Sex, y = df[,8]), stat = "identity")+
  geom_text(aes(Q14_Sex, y = df[,8]),label = sum(df[,8]))

