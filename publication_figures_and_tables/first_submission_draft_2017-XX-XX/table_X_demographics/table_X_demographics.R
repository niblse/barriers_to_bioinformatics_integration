# Create demographics tables / charts?

require(ggplot2)
require(stargazer)
require(tidyverse)
require(ggthemes)


#load unformatted demographics table 

data.df <- read_csv("../../../analysis_scripts/summarize_and_plot_demographic_information/output_tables/summary_table.csv")

# Create dataframe of desired demographics




table_sex <- data.frame("Demographic_category"=c("Female", 
                                                 "Male"),
                        "Demographic"=c("Sex", 
                                        "Sex"), 
                        "N"=c(as.numeric(data.df[5,3]), 
                              as.numeric(data.df[6,3])), 
                        "Percentage"=c(as.numeric(data.df[5,4]), 
                                       as.numeric(data.df[6,4])), 
                        stringsAsFactors = FALSE
                        )

table_ethnicity <-  data.frame("Demographic_category"=c("American Indian or Alaskan Native",
                                                        "Asian", 
                                                        "Black or African American", 
                                                        "Hispanic", 
                                                        "Native Hawaiian or Other Pacific Islander", 
                                                        "White"), 
                               "Demographic"=c("Ethnicity",
                                               "Ethnicity",
                                               "Ethnicity",
                                               "Ethnicity",
                                               "Ethnicity",
                                               "Ethnicity"
                                               ), 
                               "N"=c(as.numeric(data.df[15,3]),
                                     as.numeric(data.df[16,3]),
                                     as.numeric(data.df[17,3]),
                                     as.numeric(data.df[18,3]),
                                     as.numeric(data.df[19,3]),
                                     as.numeric(data.df[20,3])
                                     ), 
                               "Percentage"=c(as.numeric(data.df[15,4]),
                                              as.numeric(data.df[16,4]),
                                              as.numeric(data.df[17,4]),
                                              as.numeric(data.df[18,4]),
                                              as.numeric(data.df[19,4]),
                                              as.numeric(data.df[20,4])
                                              ),
                               stringsAsFactors = FALSE
                                     )
                                                        
                               
# combine tables
combined_tables <- dplyr::union(table_sex, 
                                table_ethnicity)



# group by demographic and order and convert percentages to rounded figures

combined_tables <- combined_tables%>%
  arrange(Demographic, desc(Percentage))%>%
  mutate(Percentage = round(Percentage*100))

# order for plotting

combined_tables$Demographic_category <- factor(combined_tables$Demographic_category, 
                                               levels = combined_tables$Demographic_category[order(desc(combined_tables$Percentage))]) 

#Generate plot

combined_tables%>%
  ggplot(aes(Demographic_category, Percentage))+
  geom_bar(stat = "Identity")+
  facet_wrap(~Demographic, scales = "free") +
  theme_tufte() +
  theme(text = element_text(size = 18))+
  geom_text(aes(label = paste0(Percentage, "%"), y = Percentage),
            vjust = 1.4, size = 4.5, color = "white") +
  theme(axis.text.x = element_text(angle = 45,  hjust = 1))+
  theme(axis.title.x=element_blank(),
        #axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  ggtitle("Respondant Demographics")

                               
                               
                           