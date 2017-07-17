# Create demographics tables / charts?

require(ggplot2)
require(stargazer)
require(tidyverse)
require(ggthemes)
require(gdata)

#load unformatted demographics table 

data.df <- read_csv("../../../analysis_scripts/summarize_and_plot_demographic_information/output_tables/summary_table.csv")
degree.df <- read_csv("../../../analysis_scripts/summarize_and_plot_demographic_information/output_tables/Q18.bin.degree.table.csv")
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

table_ethnicity <-  data.frame("Demographic_category"=c("American Indian/Alaskan Native",
                                                        "Asian", 
                                                        "Black/African American", 
                                                        "Hispanic", 
                                                        "Hawaiian/Pacific Islander", 
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
table_degree <- data.frame("Demographic_category"=c("Bachelors", 
                                                    "Masters",
                                                    "Doctoral", 
                                                    "Professional/Other"), 
                           "Demographic"=c("Terminal Degree", 
                                           "Terminal Degree",
                                           "Terminal Degree",
                                           "Terminal Degree"
                                           ), 
                           "N"=c(as.numeric(data.df[33,3]),
                                 as.numeric(data.df[34,3]),
                                 as.numeric(data.df[36,3]),
                                 (as.numeric(data.df[35,3])+as.numeric(data.df[37,3]))
                                 ), 
                           "Percentage"=c(as.numeric(data.df[33,4]),
                                          as.numeric(data.df[34,4]),
                                          as.numeric(data.df[36,4]),
                                          (as.numeric(data.df[35,4])+as.numeric(data.df[37,4]))
                                          ), 
                           stringsAsFactors = FALSE
                           )
table_carnegie <- data.frame("Demographic_category"=c("Associates", 
                                                      "Baccalaureate", 
                                                      "Masters", 
                                                      "Doctoral"), 
                             "Demographic"= c("Carnegie Classification",
                                              "Carnegie Classification",
                                              "Carnegie Classification",
                                              "Carnegie Classification"
                                              ), 
                             "N"=c(as.numeric(data.df[43,3]), 
                                   as.numeric(data.df[44,3]), 
                                   as.numeric(data.df[46,3]), 
                                   as.numeric(data.df[45,3]) 
                                   ), 
                             "Percentage"=c(as.numeric(data.df[43,4]), 
                                            as.numeric(data.df[44,4]), 
                                            as.numeric(data.df[46,4]), 
                                            as.numeric(data.df[45,4])
                                            ), 
                             stringsAsFactors = FALSE
                             )
table_msi <- data.frame("Demographic_category"=c("Minority Serving", 
                                                 "Non-minority Serving"), 
                        "Demographic"= c("MSI Designation", 
                                         "MSI Designation"), 
                        "N"=c(as.numeric(data.df[51,3]),
                              as.numeric(data.df[52,3])),
                        "Percentage"=c(as.numeric(data.df[51,4]),
                                       as.numeric(data.df[52,4])), 
                        stringsAsFactors = FALSE
                        )
table_enrollment <- data.frame("Demographic_category"=c("< 5,000", 
                                                        "5-15,000", 
                                                        "> 15,000"), 
                        "Demographic"= c("Undergraduate Enrollment", 
                                         "Undergraduate Enrollment", 
                                         "Undergraduate Enrollment"), 
                        "N"=c(as.numeric(data.df[55,3]), 
                              as.numeric(data.df[57,3]), 
                              as.numeric(data.df[56,3])),
                        "Percentage"=c(as.numeric(data.df[55,4]), 
                                       as.numeric(data.df[57,4]), 
                                       as.numeric(data.df[56,4])), 
                        stringsAsFactors = FALSE
                        )
table_training <- data.frame("Demographic_category"=c("Formal Training", 
                                                      "No Training", 
                                                      "Self-taught", 
                                                      "Workshops and Bootcamps"), 
                        "Demographic"= c("Bioinformatics Training",
                                         "Bioinformatics Training",
                                         "Bioinformatics Training",
                                         "Bioinformatics Training"), 
                        "N"=c(as.numeric(data.df[61,3]), 
                              as.numeric(data.df[62,3]), 
                              as.numeric(data.df[63,3]), 
                              as.numeric(data.df[64,3])),
                        "Percentage"=c(as.numeric(data.df[61,4]), 
                                       as.numeric(data.df[62,4]), 
                                       as.numeric(data.df[63,4]), 
                                       as.numeric(data.df[64,4])), 
                        stringsAsFactors = FALSE
                        )
table_decade <- data.frame("Demographic_category"=c("1980s", 
                                                    "1990s", 
                                                    "2000s", 
                                                    "2010s"),
                           "Demographic"=c("Decade of Degree",
                                           "Decade of Degree",
                                           "Decade of Degree",
                                           "Decade of Degree"), 
                           "N"=c(as.numeric(degree.df[2,3]), 
                                 as.numeric(degree.df[3,3]), 
                                 as.numeric(degree.df[4,3]), 
                                 as.numeric(degree.df[5,3])), 
                           "Percentage"=c(as.numeric(degree.df[2,4]), 
                                          as.numeric(degree.df[3,4]), 
                                          as.numeric(degree.df[4,4]), 
                                          as.numeric(degree.df[5,4])), 
                           stringsAsFactors = FALSE
                           )
                                                        
                               
# combine tables
combined_tables <- rbind(table_sex, 
                         table_ethnicity, 
                         table_degree, 
                         table_carnegie, 
                         table_msi, 
                         table_enrollment, 
                         table_training, 
                         table_decade)



# group by demographic and order and convert percentages to rounded figures

combined_tables <- combined_tables%>%
  arrange(Demographic, desc(Percentage))%>%
  mutate(Percentage = round(Percentage*100))

# order for plotting

combined_tables$Demographic_category <- factor(combined_tables$Demographic_category, 
                                               levels = combined_tables$Demographic_category[order(desc(combined_tables$Percentage))])

target_order <- c(
                  "Ethnicity",
                  "Ethnicity",
                  "Ethnicity",
                  "Ethnicity",
                  "Ethnicity",
                  "Ethnicity",
                  "Sex", 
                  "Sex", 
                  "Terminal Degree", 
                  "Terminal Degree", 
                  "Terminal Degree", 
                  "Terminal Degree", 
                  "Bioinformatics Training",
                  "Bioinformatics Training",
                  "Bioinformatics Training",
                  "Bioinformatics Training",
                  "Decade of Degree", 
                  "Decade of Degree", 
                  "Decade of Degree", 
                  "Decade of Degree", 
                  "MSI Designation", 
                  "MSI Designation", 
                  "Carnegie Classification", 
                  "Carnegie Classification",
                  "Carnegie Classification",
                  "Carnegie Classification",
                  "Undergraduate Enrollment",
                  "Undergraduate Enrollment",
                  "Undergraduate Enrollment"
                  )
combined_tables$Demographic <- reorder.factor(combined_tables$Demographic, new.order =  target_order)


#Generate plot

combined_tables%>%
  ggplot(aes(Demographic_category, Percentage))+
  geom_bar(stat = "Identity")+
  facet_wrap(~Demographic, scales = "free_x", nrow = 2) +
  theme_tufte() +
  theme(text = element_text(size = 20))+
  geom_text(aes(label = paste0(Percentage, "%"), y = Percentage),
            vjust =-.2, size = 4, color = "black") +
  theme(axis.text.x = element_text(angle = 45,  hjust = 1))+
  theme(axis.title.x=element_blank(),
        axis.ticks.x=element_blank())+
  theme(strip.text.x = element_text(size = 20))#+
  #theme(panel.background = element_rect(fill = NA, color = "black"))

ggsave("demographics_figure.png")                               
                               
                           