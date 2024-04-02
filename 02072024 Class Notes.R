# ANOVA work
# Danielle Weiss 
# 02072024

# load packages
library(palmerpenguins)
library(tidyverse)

# make dataframe
penguins
pens <- penguins

# explore data 
pens

# NULL HYPOTHESIS: There is no difference in flipper length by penguin species 

library(palmerpenguins)
library(tidyverse)

# Categorical + Numerical: anova [flipper_length_mm + species]
# 3 species - cannot do T-test + need ANOVA
# effect of species on flipper length 

# check how many species we have
unique(pens$species)

# ANOVA

aov1 <- aov(flipper_length_mm ~ species, data = pens)
aov1
summary(aov1)

# P-value less than 0.05 = significant 
# F-value 
# Small P-value + large f-value [flipper length differs by species]
# There is an effect of X on Y - we don't know the directionality of the effect

# graph that shows how flipper length may differ by species
pens

# boxplot with means + error bars 
# summarize

head(pens)

penguin_box <- ggplot(pens, aes(x=species, y=flipper_length_mm, fill=species)) + 
  geom_boxplot() + 
  geom_jitter(alpha=0.2, width=0.2) +
  labs(x = 'Species', y = 'Flipper Length', title = 'Flipper Length of Penguin Species') +
  theme_classic()

# view plot 
penguin_box

#####################################

# means + error + raw data + tidyverse pipeline [group_by() + summarize()]
  # we will use SE [standard error of mean] 
  # SE = sd / sqrt(sample size)

meanpen <- pens |>
  group_by(species) |>
  drop.na(flipper_length_mm |> # drop row + column with NAs
  summarize(mean=mean(flipper_length_mm), sd=sd(flipper_length_mm), n=n(), se= sd/sqrt(n))

meanpen # view data

# combine boxplot + means +  se
head(pens)
  
pens_updated <- ggplot(meanpen, aes(x=species, y=mean, color=species)) + #mean
    geom_jitter(data=pens, aes(x=species, y=flipper_length_mm), alpha=0.2, width=0.3) + #rawdata
    geom_point(color='black', size=2) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=0.3, color='black') + #error
  theme_classic() +
  labs(y='Flipper Length')
  scale_color_npg()
  
  summary(aov1)
  TukeyHSD(aov1)




