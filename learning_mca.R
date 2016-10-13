library(tidyverse)

mca_df <- read_csv("decoded_df.csv")


barriers <- select(mca_df, Q29_At.your.current.institution..do.you.face.any.technical.barriers.in.teaching.bioinformatics..e.g....., 
                   Q21_What.is.the.Carnegie.classification.of.your.institution.)
barriers <- barriers %>% mutate_if(is.character,as.factor)



library(FactoMineR)


cats = apply(barriers, 2, function(x) nlevels(as.factor(x)))




mca1 = MCA(barriers, graph = FALSE)
plot.MCA(mca1, invisible = "Q29_At.your.current.institution..do.you.face.any.technical.barriers.in.teaching.bioinformatics..e.g.....")

library(ggplot2)

# data frame with variable coordinates
mca1_vars_df = data.frame(mca1$var$coord, Variable = rep(names(cats), cats))

# data frame with observation coordinates
mca1_obs_df = data.frame(mca1$ind$coord)

# plot of variable categories
ggplot(data=mca1_vars_df, 
       aes(x = Dim.1, y = Dim.2, label = rownames(mca1_vars_df))) +
  geom_hline(yintercept = 0, colour = "gray70") +
  geom_vline(xintercept = 0, colour = "gray70") +
  geom_text(aes(colour=Variable)) +
  ggtitle("MCA plot of variables using R package FactoMineR")