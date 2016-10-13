
require(tidyverse)
require(FactoMineR)

#have the tea dataset to compare
data("tea")


#Load survey data with catagorical responses decoded
df <- read_csv("decoded_df.csv")


#select all possible variables to be examined in this analysis

# remove any non-US respondants
countries <- c("United States","Puerto Rico")
df <- df%>%
  filter(Country_Country %in% countries )

factor_columns <- df %>%
        select(Q57_Please.select.the.statement.belOw.that.best.describes.yOu.,
          Q1_Please.select.the.statement.belOw.that.best.describes.yOur.current.teaching.Of.biOinfOrmatics.cOn...,
          Q5_In.yOur.OpiniOn..are.additiOnal.undergraduate.cOurses.with.biOinfOrmatics.cOntent.needed.at.yOur...,
          Q29_At.your.current.institution..do.you.face.any.technical.barriers.in.teaching.bioinformatics..e.g.....,
          Q14_Sex:Q17_Highest.earned.degree..If..other...please.explain.,
          Q3_Which.of.the.following.best.describes.your.level.of.bioinformatics.training.:Q24_What.is.the.total.number.of.undergraduate.students.at.your.institution.,
          Q26_How.many.full.time.faculty.are.in.your.department.unit...Do.not.include.part.time.faculty.or.adju...,
          Q27_How.many.undergraduate.students.are.in.your.department.unit..all.majors..,
          State_State,
          Region_Region)



#Analysis 1: analysis of factor columns

#create a df for this analysis

analysis_1_factors <- factor_columns

#transform df into a dataframe of factors

analysis_1_factors <- analysis_1_factors %>%
  mutate_if(is.character,as.factor)%>%
  mutate_if(is.numeric,as.factor)%>%
  data.frame()

#MCA of factors

analysis_1_factors <- na.omit(analysis_1_factors)




analysis_1_mca <- MCA(analysis_1_factors)
plot.MCA(analysis_1_mca, choix = "ind")



#examine variable categories starting with Q57
#Only select rows that are not NA

cat_df <- filter(df, !is.na(Q57_Please.select.the.statement.belOw.that.best.describes.yOu.))




#transform df into factors and into a dataframe (not tibble)
factor_cat_df <- cat_df %>% mutate_if(is.character,as.factor)
factor_cat_df  <- factor_cat_df  %>% mutate_if(is.numeric,as.factor)
factor_cat_df <- data.frame(factor_cat_df)
#Get descriptions of some categories
result <- catdes(factor_cat_df, num.var = 4)
catdes(tea, num.var = 1)



#goodies for later?
#dimdesc() requires MCA object
#graph.var() requires MCA object