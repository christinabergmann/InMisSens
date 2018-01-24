#### Read in data and clean up ####

# This script was adapted from Christina Bergmann's script, which was
# written to create meta-analytic models to examine behavior mispronunciation sensitivity
# (segmenting artificial mini-languages based on statistics). 
# Original Author: Christina Bergmann
# chbergma'at'gmail.com
# Current Author: Katie Von Holzen
# last modified: January 17, 2018

library(tidyverse)
library(metafor)
library(meta)
library(pwr)
library(knitr)
library(wesanderson)

#### Data read in ####

#For now focusing on the eye tracking data
source("scripts/calculateES.R")

#### Preprocessing ####
db_ET <- db_ET %>%
  mutate(age.C = (mean_age_1-mean(mean_age_1, na.rm=TRUE))/30.44) %>%
  filter(mean_age_months < 31)


db_ET$collapse <- paste(db_ET$study_ID, db_ET$expt_num, db_ET$same_infant, sep = "_")

#Split into correct and MP database

db_ET_correct <- db_ET[db_ET$is_correct=="1",]
db_ET_MP <- db_ET[db_ET$is_mp=="1",]

#collapse over nonindependent MP rows for a *general* MP effect


collapse_rows <- function(db){
  db$collapse <- paste(db$study_ID, db$expt_num, db$same_infant, sep = "_")
  
  for(independent in unique(db$collapse)){
    if(length(db[db$collapse==independent])>1){
      sub = db[db$collapse==independent, ]
      sub$d_calc <- median(sub$d_calc)
      sub$d_var_calc <- median(sub$d_var_calc)
      sub$g_calc <- median(sub$g_calc)
      sub$g_var_calc <- median(sub$g_var_calc)
      sub$corr_imputed <- median(sub$corr_imputed)
      db <- db[!(db$collapse==independent),]
      db <- rbind(db, sub[1,])
    }
  }
  return(db)
}


#db_ET_correct = collapse_rows(db_ET_correct)

#db_ET_MP = collapse_rows(db_ET_MP)

#remove outliers, for now we have none, though
db_ET_MP$nooutlier = ifelse(db_ET_MP$g_calc > mean(db_ET_MP$g_calc, na.rm = TRUE) + 3*sd(db_ET_MP$g_calc, na.rm = TRUE) 
                            | db_ET_MP$g_calc < mean(db_ET_MP$g_calc, na.rm = TRUE) - 3*sd(db_ET_MP$g_calc, na.rm = TRUE),FALSE, TRUE)
db_ET_MP = db_ET_MP[db_ET_MP$nooutlier,]

db_ET_correct$nooutlier = ifelse(db_ET_correct$g_calc > mean(db_ET_correct$g_calc, na.rm = TRUE) + 3*sd(db_ET_correct$g_calc, na.rm = TRUE) 
                                 | db_ET_correct$g_calc < mean(db_ET_correct$g_calc, na.rm = TRUE) - 3*sd(db_ET_correct$g_calc, na.rm = TRUE),FALSE, TRUE)
db_ET_correct = db_ET_correct[db_ET_correct$nooutlier,]


#### Correct Object Identification ####
rma_correct = rma.mv(g_calc, g_var_calc, data = db_ET_correct, random = ~ collapse | short_cite)

summary(rma_correct)


rma_correct_age = rma.mv(g_calc, g_var_calc, mods = ~age.C, data = db_ET_correct, random = ~ collapse | short_cite)

summary(rma_correct_age)

#### Mispronounced Object Identification ####

rma_MP = rma.mv(g_calc, g_var_calc, data = db_ET_MP, random = ~ collapse | short_cite)

summary(rma_MP)


rma_MP_age = rma.mv(g_calc, g_var_calc, mods = ~age.C, data = db_ET_MP, random = ~ collapse | short_cite)

summary(rma_MP_age)


#### Mispronounciation Effect ####

db_ET_correct$condition <- 1
db_ET_MP$condition <- 0

dat <- bind_rows(db_ET_correct, db_ET_MP)

rma_MPeffect <- rma.mv(g_calc, g_var_calc, mods = ~condition, data = dat, random = ~ collapse | short_cite)

summary(rma_MPeffect)  

rma_MPeffect_1 <- rma.mv(g_calc, g_var_calc, mods = ~condition-1, data = dat, random = ~ collapse | short_cite)

summary(rma_MPeffect_1)  


rma_MPeffect_age <- rma.mv(g_calc, g_var_calc, mods = ~age.C*condition, data = dat, random = ~ collapse | short_cite)

summary(rma_MPeffect_age)  




#### Explore Funnel Plot Assymetry ####
# remove g higher than 2

db_ET_co2 <- subset(db_ET_correct, g_calc < 2)
db_ET_mp2 <- subset(db_ET_MP, g_calc < 1)
#dat_mpe2 <- subset(dat, g_calc < 2)

# Correct Object Identification

rma_co2 = rma.mv(g_calc, g_var_calc, data = db_ET_co2, random = ~ collapse | short_cite)

summary(rma_co2)

rma_co2_age = rma.mv(g_calc, g_var_calc, mods = ~age.C, data = db_ET_co2, random = ~ collapse | short_cite)

summary(rma_co2_age)

# Mispronounced Object Identification

rma_mp2 = rma.mv(g_calc, g_var_calc, data = db_ET_mp2, random = ~ collapse | short_cite)

summary(rma_mp2)

rma_mp2_age = rma.mv(g_calc, g_var_calc, mods = ~age.C, data = db_ET_mp2, random = ~ collapse | short_cite)

summary(rma_mp2_age)


# Mispronunciation Effect


