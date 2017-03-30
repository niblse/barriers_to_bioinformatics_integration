# this script creates a dataframe where level of bioinformatics training is grouped into custom bins

require(tidyverse)


############ LOAD THE PREPARED SURVEY DATA ###########################################
#read in cleaned dataframe "decoded_df.csv"
master.df <- read_csv("../04_decode_survey_responses/output/decoded_df.csv")    

#convert degree years to numeric values

##18
master.df$Q18_Year.of.highest.earned.degree.[master.df$Q18_Year.of.highest.earned.degree. == "1_2016"] <- "2016"
master.df$Q18_Year.of.highest.earned.degree.[master.df$Q18_Year.of.highest.earned.degree. == "2_2015"] <- "2015"
master.df$Q18_Year.of.highest.earned.degree.[master.df$Q18_Year.of.highest.earned.degree. == "3_2014"] <- "2014"
master.df$Q18_Year.of.highest.earned.degree.[master.df$Q18_Year.of.highest.earned.degree. == "4_2013"] <- "2013"
master.df$Q18_Year.of.highest.earned.degree.[master.df$Q18_Year.of.highest.earned.degree. == "5_2012"] <- "2012"
master.df$Q18_Year.of.highest.earned.degree.[master.df$Q18_Year.of.highest.earned.degree. == "6_2011"] <- "2011"
master.df$Q18_Year.of.highest.earned.degree.[master.df$Q18_Year.of.highest.earned.degree. == "7_2010"] <- "2010"
master.df$Q18_Year.of.highest.earned.degree.[master.df$Q18_Year.of.highest.earned.degree. == "8_2009"] <- "2009"
master.df$Q18_Year.of.highest.earned.degree.[master.df$Q18_Year.of.highest.earned.degree. == "9_2008"] <- "2008"
master.df$Q18_Year.of.highest.earned.degree.[master.df$Q18_Year.of.highest.earned.degree. == "10_2007"] <- "2007"
master.df$Q18_Year.of.highest.earned.degree.[master.df$Q18_Year.of.highest.earned.degree. == "11_2006"] <- "2006"
master.df$Q18_Year.of.highest.earned.degree.[master.df$Q18_Year.of.highest.earned.degree. == "12_2005"] <- "2005"
master.df$Q18_Year.of.highest.earned.degree.[master.df$Q18_Year.of.highest.earned.degree. == "13_2004"] <- "2004"
master.df$Q18_Year.of.highest.earned.degree.[master.df$Q18_Year.of.highest.earned.degree. == "14_2003"] <- "2003"
master.df$Q18_Year.of.highest.earned.degree.[master.df$Q18_Year.of.highest.earned.degree. == "15_2002"] <- "2002"
master.df$Q18_Year.of.highest.earned.degree.[master.df$Q18_Year.of.highest.earned.degree. == "16_2001"] <- "2001"
master.df$Q18_Year.of.highest.earned.degree.[master.df$Q18_Year.of.highest.earned.degree. == "17_2000"] <- "2000"
master.df$Q18_Year.of.highest.earned.degree.[master.df$Q18_Year.of.highest.earned.degree. == "18_1999"] <- "1999"
master.df$Q18_Year.of.highest.earned.degree.[master.df$Q18_Year.of.highest.earned.degree. == "19_1998"] <- "1998"
master.df$Q18_Year.of.highest.earned.degree.[master.df$Q18_Year.of.highest.earned.degree. == "20_1997"] <- "1997"
master.df$Q18_Year.of.highest.earned.degree.[master.df$Q18_Year.of.highest.earned.degree. == "21_1996"] <- "1996"
master.df$Q18_Year.of.highest.earned.degree.[master.df$Q18_Year.of.highest.earned.degree. == "22_1995"] <- "1995"
master.df$Q18_Year.of.highest.earned.degree.[master.df$Q18_Year.of.highest.earned.degree. == "23_1994"] <- "1994"
master.df$Q18_Year.of.highest.earned.degree.[master.df$Q18_Year.of.highest.earned.degree. == "24_1993"] <- "1993"
master.df$Q18_Year.of.highest.earned.degree.[master.df$Q18_Year.of.highest.earned.degree. == "25_1992"] <- "1992"
master.df$Q18_Year.of.highest.earned.degree.[master.df$Q18_Year.of.highest.earned.degree. == "26_1991"] <- "1991"
master.df$Q18_Year.of.highest.earned.degree.[master.df$Q18_Year.of.highest.earned.degree. == "27_1990"] <- "1990"
master.df$Q18_Year.of.highest.earned.degree.[master.df$Q18_Year.of.highest.earned.degree. == "28_1989"] <- "1989"
master.df$Q18_Year.of.highest.earned.degree.[master.df$Q18_Year.of.highest.earned.degree. == "29_1988"] <- "1989"
master.df$Q18_Year.of.highest.earned.degree.[master.df$Q18_Year.of.highest.earned.degree. == "30_1987"] <- "1987"
master.df$Q18_Year.of.highest.earned.degree.[master.df$Q18_Year.of.highest.earned.degree. == "31_1986"] <- "1986"
master.df$Q18_Year.of.highest.earned.degree.[master.df$Q18_Year.of.highest.earned.degree. == "32_1985"] <- "1985"
master.df$Q18_Year.of.highest.earned.degree.[master.df$Q18_Year.of.highest.earned.degree. == "33_1984"] <- "1984"
master.df$Q18_Year.of.highest.earned.degree.[master.df$Q18_Year.of.highest.earned.degree. == "34_1983"] <- "1983"
master.df$Q18_Year.of.highest.earned.degree.[master.df$Q18_Year.of.highest.earned.degree. == "35_1982"] <- "1982"
master.df$Q18_Year.of.highest.earned.degree.[master.df$Q18_Year.of.highest.earned.degree. == "36_1981"] <- "1981"
master.df$Q18_Year.of.highest.earned.degree.[master.df$Q18_Year.of.highest.earned.degree. == "37_1980"] <- "1980"
master.df$Q18_Year.of.highest.earned.degree.[master.df$Q18_Year.of.highest.earned.degree. == "38_1979"] <- "1979"
master.df$Q18_Year.of.highest.earned.degree.[master.df$Q18_Year.of.highest.earned.degree. == "39_1978"] <- "1978"
master.df$Q18_Year.of.highest.earned.degree.[master.df$Q18_Year.of.highest.earned.degree. == "40_1977"] <- "1977"
master.df$Q18_Year.of.highest.earned.degree.[master.df$Q18_Year.of.highest.earned.degree. == "41_1976"] <- "1976"
master.df$Q18_Year.of.highest.earned.degree.[master.df$Q18_Year.of.highest.earned.degree. == "42_1975"] <- "1975"
master.df$Q18_Year.of.highest.earned.degree.[master.df$Q18_Year.of.highest.earned.degree. == "43_1974"] <- "1974"
master.df$Q18_Year.of.highest.earned.degree.[master.df$Q18_Year.of.highest.earned.degree. == "44_1973"] <- "1973"
master.df$Q18_Year.of.highest.earned.degree.[master.df$Q18_Year.of.highest.earned.degree. == "45_1972"] <- "1972"
master.df$Q18_Year.of.highest.earned.degree.[master.df$Q18_Year.of.highest.earned.degree. == "46_1971"] <- "1971"
master.df$Q18_Year.of.highest.earned.degree.[master.df$Q18_Year.of.highest.earned.degree. == "47_1970"] <- "1970"
master.df$Q18_Year.of.highest.earned.degree.[master.df$Q18_Year.of.highest.earned.degree. == "48_1969"] <- "1969"
master.df$Q18_Year.of.highest.earned.degree.[master.df$Q18_Year.of.highest.earned.degree. == "49_1968"] <- "1968"
master.df$Q18_Year.of.highest.earned.degree.[master.df$Q18_Year.of.highest.earned.degree. == "50_1967"] <- "1967"
master.df$Q18_Year.of.highest.earned.degree.[master.df$Q18_Year.of.highest.earned.degree. == "51_1966"] <- "1966"
master.df$Q18_Year.of.highest.earned.degree.[master.df$Q18_Year.of.highest.earned.degree. == "52_1965"] <- "1965"
master.df$Q18_Year.of.highest.earned.degree.[master.df$Q18_Year.of.highest.earned.degree. == "53_1964"] <- "1964"
master.df$Q18_Year.of.highest.earned.degree.[master.df$Q18_Year.of.highest.earned.degree. == "54_1963"] <- "1963"
master.df$Q18_Year.of.highest.earned.degree.[master.df$Q18_Year.of.highest.earned.degree. == "55_1962"] <- "1962"
master.df$Q18_Year.of.highest.earned.degree.[master.df$Q18_Year.of.highest.earned.degree. == "56_1961"] <- "1961"
master.df$Q18_Year.of.highest.earned.degree.[master.df$Q18_Year.of.highest.earned.degree. == "57_1960"] <- "1960"
master.df$Q18_Year.of.highest.earned.degree.[master.df$Q18_Year.of.highest.earned.degree. == "58_1959"] <- "1959"
master.df$Q18_Year.of.highest.earned.degree.[master.df$Q18_Year.of.highest.earned.degree. == "59_1958"] <- "1958"
master.df$Q18_Year.of.highest.earned.degree.[master.df$Q18_Year.of.highest.earned.degree. == "60_1957"] <- "1957"
master.df$Q18_Year.of.highest.earned.degree.[master.df$Q18_Year.of.highest.earned.degree. == "61_1956"] <- "1956"
master.df$Q18_Year.of.highest.earned.degree.[master.df$Q18_Year.of.highest.earned.degree. == "62_1955"] <- "1955"
master.df$Q18_Year.of.highest.earned.degree.[master.df$Q18_Year.of.highest.earned.degree. == "63_1954"] <- "1954"
master.df$Q18_Year.of.highest.earned.degree.[master.df$Q18_Year.of.highest.earned.degree. == "64_1953"] <- "1953"
master.df$Q18_Year.of.highest.earned.degree.[master.df$Q18_Year.of.highest.earned.degree. == "65_1952"] <- "1952"
master.df$Q18_Year.of.highest.earned.degree.[master.df$Q18_Year.of.highest.earned.degree. == "66_1951"] <- "1951"
master.df$Q18_Year.of.highest.earned.degree.[master.df$Q18_Year.of.highest.earned.degree. == "67_1950"] <- "1950"
master.df$Q18_Year.of.highest.earned.degree.[master.df$Q18_Year.of.highest.earned.degree. == "68_Rather Not Say"] <- NA


master.df$Q18_Year.of.highest.earned.degree. <- as.numeric(master.df$Q18_Year.of.highest.earned.degree.)

master.df.adj <- master.df %>%
  mutate(
    bin_degree_yrs = ifelse(test = Q18_Year.of.highest.earned.degree. >= 2010,
                            yes = "2010-2016", 
                            no = ifelse(test = Q18_Year.of.highest.earned.degree. <2010 & Q18_Year.of.highest.earned.degree. >=2000,
                                        yes = "2000-2009", 
                                        no = ifelse(test = Q18_Year.of.highest.earned.degree. <2000 & Q18_Year.of.highest.earned.degree. >=1990,
                                                    yes = "1990-1999",
                                                    no = ifelse(test = Q18_Year.of.highest.earned.degree. <1990 & Q18_Year.of.highest.earned.degree. >=1980,
                                                                yes = "1980-1989", 
                                                                no = ifelse(test = Q18_Year.of.highest.earned.degree. <1980 & Q18_Year.of.highest.earned.degree. >=1970,
                                                                            yes = "1970-1979", 
                                                                            no = ifelse(test = Q18_Year.of.highest.earned.degree. <1970,
                                                                                        yes = "Before 1970", 
                                                                                        no = "NA"
                                                                              
                                                                            )
                                                                  
                                                                )
                                                      
                                                    )
                                          
                                        )
                              
                            )
      
    )
    
  )
    


# write dataframe for future refference

dir.create("./output")
write_csv(master.df.adj, "./output/decoded_df_w_bin_degree_years.csv")
  
