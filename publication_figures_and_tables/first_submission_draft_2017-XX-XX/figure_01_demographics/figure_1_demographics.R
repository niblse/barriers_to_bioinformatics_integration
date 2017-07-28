# Create demographics tables / charts?

require(ggplot2)
require(stargazer)
require(tidyverse)
require(ggthemes)
require(gdata)
require(stargazer)

#load unformatted demographics table 

data.df <- read_csv("../../../analysis_scripts/summarize_and_plot_demographic_information/output_tables/summary_demographics_w_NAs.csv")

# Create dataframe of desired demographics




table_sex <- data.frame("Demographic_category"=c("Female", 
                                                 "Male", 
                                                 "Unknown Sex"),
                        "Demographic"=c("Sex", 
                                        "Sex",
                                        "Sex"), 
                        "N"=c(as.numeric(data.df[47,3]), 
                              as.numeric(data.df[48,3]), 
                              as.numeric(data.df[49,3])
                              ),
                        "Percentage"=c(as.numeric(data.df[47,5]), 
                                       as.numeric(data.df[48,5]),
                                       as.numeric(data.df[49,5])
                                       ), 
                        stringsAsFactors = FALSE
                        )

table_ethnicity <-  data.frame("Demographic_category"=c("Am. Indian/Alaskan Native",
                                                        "Asian", 
                                                        "Black/African Am.", 
                                                        "Hispanic", 
                                                        "Hawaiian/Pacific Islander", 
                                                        "White", 
                                                        "Unknown Race/Ethnicity"), 
                               "Demographic"=c("Race/Ethnicity",
                                               "Race/Ethnicity",
                                               "Race/Ethnicity",
                                               "Race/Ethnicity",
                                               "Race/Ethnicity",
                                               "Race/Ethnicity",
                                               "Race/Ethnicity"
                                               ), 
                               "N"=c(as.numeric(data.df[17,3]),
                                     as.numeric(data.df[18,3]),
                                     as.numeric(data.df[19,3]),
                                     as.numeric(data.df[20,3]),
                                     as.numeric(data.df[21,3]),
                                     as.numeric(data.df[22,3]),
                                     as.numeric(data.df[23,3])
                                     ), 
                               "Percentage"=c(as.numeric(data.df[17,5]),
                                              as.numeric(data.df[18,5]),
                                              as.numeric(data.df[19,5]),
                                              as.numeric(data.df[20,5]),
                                              as.numeric(data.df[21,5]),
                                              as.numeric(data.df[22,5]),
                                              as.numeric(data.df[23,5])
                                              ),
                               stringsAsFactors = FALSE
                                     )
table_degree <- data.frame("Demographic_category"=c("Bachelors", 
                                                    "Masters",
                                                    "Doctoral", 
                                                    "Professional",
                                                    "Other", 
                                                    "Unknown Degree"
                                                    ), 
                           "Demographic"=c("Terminal Degree", 
                                           "Terminal Degree",
                                           "Terminal Degree",
                                           "Terminal Degree", 
                                           "Terminal Degree",
                                           "Terminal Degree"
                                           ), 
                           "N"=c(as.numeric(data.df[24,3]),
                                 as.numeric(data.df[25,3]),
                                 as.numeric(data.df[27,3]),
                                 as.numeric(data.df[28,3]),
                                 as.numeric(data.df[26,3]),
                                 as.numeric(data.df[29,3])
                                 ), 
                           "Percentage"=c(as.numeric(data.df[24,5]),
                                          as.numeric(data.df[25,5]),
                                          as.numeric(data.df[27,5]),
                                          as.numeric(data.df[28,5]),
                                          as.numeric(data.df[26,5]),
                                          as.numeric(data.df[29,5])
                                          ), 
                           stringsAsFactors = FALSE
                           )
table_carnegie <- data.frame("Demographic_category"=c("Associates", 
                                                      "Baccalaureate", 
                                                      "Masters", 
                                                      "Doctoral", 
                                                      "Unknown Classification"
                                                      ), 
                             "Demographic"= c("Carnegie Classification",
                                              "Carnegie Classification",
                                              "Carnegie Classification",
                                              "Carnegie Classification", 
                                              "Carnegie Classification"
                                              ), 
                             "N"=c(as.numeric(data.df[1,3]), 
                                   as.numeric(data.df[2,3]), 
                                   as.numeric(data.df[4,3]), 
                                   as.numeric(data.df[3,3]), 
                                   as.numeric(data.df[5,3]) 
                                   ), 
                             "Percentage"=c(as.numeric(data.df[1,5]), 
                                            as.numeric(data.df[2,5]), 
                                            as.numeric(data.df[4,5]), 
                                            as.numeric(data.df[3,5]), 
                                            as.numeric(data.df[5,5])
                                            ), 
                             stringsAsFactors = FALSE
                             )
table_msi <- data.frame("Demographic_category"=c("Minority Serving", 
                                                 "Non-minority Serving", 
                                                 "Unknown MSI Designation"
                                                 ), 
                        "Demographic"= c("MSI Designation", 
                                         "MSI Designation",
                                         "MSI Designation"
                                         ), 
                        "N"=c(as.numeric(data.df[44,3]),
                              as.numeric(data.df[45,3]),
                              as.numeric(data.df[46,3])
                              ),
                        "Percentage"=c(as.numeric(data.df[44,5]),
                                       as.numeric(data.df[45,5]),
                                       as.numeric(data.df[46,5])
                                       ), 
                        stringsAsFactors = FALSE
                        )
table_enrollment <- data.frame("Demographic_category"=c("< 5,000", 
                                                        "5-15,000", 
                                                        "> 15,000", 
                                                        "Unknown Enrollment"
                                                        ), 
                        "Demographic"= c("Undergraduate Enrollment", 
                                         "Undergraduate Enrollment", 
                                         "Undergraduate Enrollment",
                                         "Undergraduate Enrollment"
                                         ), 
                        "N"=c(as.numeric(data.df[50,3]), 
                              as.numeric(data.df[51,3]), 
                              as.numeric(data.df[52,3]),
                              as.numeric(data.df[53,3])
                              ),
                        "Percentage"=c(as.numeric(data.df[50,5]), 
                                       as.numeric(data.df[51,5]), 
                                       as.numeric(data.df[52,5]),
                                       as.numeric(data.df[53,5])
                                       ), 
                        stringsAsFactors = FALSE
                        )
table_training <- data.frame("Demographic_category"=c("Formal Training", 
                                                      "No Training", 
                                                      "Self-taught", 
                                                      "Workshops and Bootcamps", 
                                                      "Unknown Bioinformatics Training"
                                                      ), 
                        "Demographic"= c("Bioinformatics Training",
                                         "Bioinformatics Training",
                                         "Bioinformatics Training",
                                         "Bioinformatics Training",
                                         "Bioinformatics Training"
                                         ), 
                        "N"=c(as.numeric(data.df[39,3]), 
                              as.numeric(data.df[40,3]), 
                              as.numeric(data.df[41,3]), 
                              as.numeric(data.df[42,3]), 
                              as.numeric(data.df[43,3])
                              ),
                        "Percentage"=c(as.numeric(data.df[39,5]), 
                                       as.numeric(data.df[40,5]), 
                                       as.numeric(data.df[41,5]), 
                                       as.numeric(data.df[42,5]), 
                                       as.numeric(data.df[43,5])
                                       ), 
                        stringsAsFactors = FALSE
                        )
table_decade <- data.frame("Demographic_category"=c("Before 1970s", 
                                                    "1970s", 
                                                    "1980s", 
                                                    "1990s", 
                                                    "2000s", 
                                                    "2010s", 
                                                    "Unknown Decade of Degree"),
                           "Demographic"=c("Decade of Degree",
                                           "Decade of Degree",
                                           "Decade of Degree",
                                           "Decade of Degree", 
                                           "Decade of Degree",
                                           "Decade of Degree",
                                           "Decade of Degree"), 
                           "N"=c(as.numeric(data.df[59,3]), 
                                 as.numeric(data.df[54,3]), 
                                 as.numeric(data.df[55,3]), 
                                 as.numeric(data.df[56,3]),
                                 as.numeric(data.df[57,3]),
                                 as.numeric(data.df[58,3]),
                                 as.numeric(data.df[60,3])
                                 ), 
                           "Percentage"=c(as.numeric(data.df[59,5]), 
                                          as.numeric(data.df[54,5]), 
                                          as.numeric(data.df[55,5]), 
                                          as.numeric(data.df[56,5]),
                                          as.numeric(data.df[57,5]),
                                          as.numeric(data.df[58,5]),
                                          as.numeric(data.df[60,5])
                                          ), 
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
                                               levels = combined_tables$Demographic_category[order(combined_tables$Percentage)])

target_order <- c(
                  "Race/Ethnicity",
                  "Race/Ethnicity",
                  "Race/Ethnicity",
                  "Race/Ethnicity",
                  "Race/Ethnicity",
                  "Race/Ethnicity",
                  "Race/Ethnicity",
                  "Sex", 
                  "Sex",
                  "Sex", 
                  "Terminal Degree", 
                  "Terminal Degree", 
                  "Terminal Degree", 
                  "Terminal Degree", 
                  "Terminal Degree", 
                  "Terminal Degree", 
                  "Bioinformatics Training",
                  "Bioinformatics Training",
                  "Bioinformatics Training",
                  "Bioinformatics Training",
                  "Bioinformatics Training",
                  "Decade of Degree", 
                  "Decade of Degree", 
                  "Decade of Degree", 
                  "Decade of Degree", 
                  "Decade of Degree", 
                  "Decade of Degree", 
                  "Decade of Degree", 
                  "MSI Designation", 
                  "MSI Designation", 
                  "MSI Designation", 
                  "Carnegie Classification", 
                  "Carnegie Classification",
                  "Carnegie Classification",
                  "Carnegie Classification",
                  "Carnegie Classification",
                  "Undergraduate Enrollment",
                  "Undergraduate Enrollment",
                  "Undergraduate Enrollment",
                  "Undergraduate Enrollment"
                  )
combined_tables$Demographic <- reorder.factor(combined_tables$Demographic, new.order =  target_order)


#Generate plot

greys <- c("#252525", 
           "#636363", 
           "#969696", 
           "#bdbdbd", 
           "#d9d9d9", 
           "#f7f7f7")

combined_tables%>%
  ggplot(aes(Demographic_category, Percentage))+
  geom_bar(stat = "Identity")+
  coord_flip()+
  facet_wrap(~Demographic, scales = "free_y", ncol = 2) +
  theme_fivethirtyeight(base_size = 20, base_family = "sans")+
  theme(panel.background = element_rect(fill = "white"))+
  theme(plot.background = element_rect(fill = "white"))+
  theme(legend.background = element_rect(fill = "white"))+
  theme(strip.text.y = element_blank())+
  #theme(strip.background = element_rect,strip.text.x = element_blank())+
  theme(axis.text.x = element_blank())+
  geom_text(aes(label = paste0(Percentage, "%"), y = Percentage),
            vjust =0, nudge_y = 5, nudge_x = -.1, size = 5, color = "black") +
  theme(axis.title.x=element_blank(),
        axis.ticks.x=element_blank())+
  theme(strip.text.x = element_text(size = 14, face = "bold", vjust = 10 ))+
  theme( axis.line = element_line(colour = "black", 
                                  size = 0.5, linetype = "solid"))+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank())+
  theme(axis.line.x = element_blank())

  


ggsave("figure_01.png", 
       units = "in", 
       height = 15, 
       width = 18)                               
                               
combined_tables%>%
  select(Demographic, Demographic_category, N, Percentage)%>%
  stargazer(., type = "html", 
          summary = FALSE, 
          rownames = FALSE, 
          digits = 1,
          title = "Summary Demographics", 
          out = "./summary_gemographics.html",
          covariate.labels = c("Demographic", "Demographic Category", "n respondents", "Percentage of survey N"))                    