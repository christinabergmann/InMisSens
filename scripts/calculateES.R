#### Calculate Effect Sizes ####

# This script was written to calculate effect sizes in MetaLab compatible spreadsheets 
# chbergma'at'gmail.com
# last modified: Feb 1, 2017

#### Load libraries ####

#None needed right now

#### Get data ####

source("scripts/ReadIn.R")

#### calculate paired ES based on available data ####

# The formulae here are based on those in MetaLab
# Link https://github.com/langcog/metalab/blob/master/scripts/compute_es.R

# We only have 41 correlations. From those we take the variance, but set the mean to .5 (.16 seems awfully low) 
median_corr = .5
var_corr = .12 

set.seed(111)
db_ET$corr_imputed = rnorm(length(db_ET$n_1), mean = median_corr, sd = var_corr)

# Initiate the additional columns for d and d_var

db_ET$d_calc <- NA
db_ET$d_var_calc <- NA
db_ET$es_method <- "missing"

db_ET$corr = ifelse(is.na(db_ET$corr), db_ET$corr_imputed, db_ET$corr)

#Correlations are never perfect and cannot be higher than 1 or lower than 0, so fixing some possible imputation issues first. 
db_ET$corr = ifelse(db_ET$corr>.8, .8, db_ET$corr)
db_ET$corr = ifelse(db_ET$corr<0, .05, db_ET$corr)

#Now compute effect sizes based on the data we have, row by row. MetaLab solves this a bit differently, here I opt for readability.


#db_ET <- subset(db_ET, short_cite == "Tamasi (2016)")


for(line in 1:length(db_ET$n_1)){
  if(db_ET[line,]$participant_design == "within_two"){
    if (complete.cases(db_ET[line,]$x_1, db_ET[line,]$x_2, db_ET[line,]$SD_1, db_ET[line,]$SD_2)) {
      # Lipsey & Wilson, 3.14
      pooled_SD <- sqrt((db_ET[line,]$SD_1 ^ 2 + db_ET[line,]$SD_2 ^ 2) / 2)
      db_ET[line,]$d_calc <- (db_ET[line,]$x_2 - db_ET[line,]$x_1) / pooled_SD
      db_ET[line,]$es_method  <- "group_means_two"
    } else if (complete.cases(db_ET[line,]$t)) {
      #Dunlap et al., 1996, p.171
      wc <- sqrt(2 * (1 - db_ET[line,]$corr))
      db_ET[line,]$d_calc <- (db_ET[line,]$t / sqrt(db_ET[line,]$n_1)) * wc
      db_ET[line,]$es_method  <- "t_two"
    } else if (complete.cases(db_ET[line,]$x_1, db_ET[line,]$x_2, db_ET[line,]$SD_dif)) {
        within_SD <-db_ET[line,]$ SD_dif / sqrt(2 * (1 - db_ET[line,]$corr)) # Lipsey & Wilson (2001); Morris & DeShon (2002)
        db_ET[line,]$d_calc <- (db_ET[line,]$x_1 - db_ET[line,]$x_2) / within_SD # Lipsey & Wilson (2001)
        db_ET[line,]$es_method <- "group_means_two"
    }
    #Next step: effect size variance (needed for weighting the effect sizes)
    #Lipsey & Wilson (2001) 
    if (complete.cases(db_ET[line,]$n_1, db_ET[line,]$d_calc)) {
      #Previous version
      # db_ET[line,]$d_var_calc <- ((1 / db_ET[line,]$n_1) + (db_ET[line,]$d_calc ^ 2 / (2 * db_ET[line,]$n_1))) * 2 * (1 - db_ET[line,]$corr)
      # MetaLab Version (corrected by Sho), looks the same to me
      # d_var_calc <- (2 * (1 - corr)/ n_1) + (d_calc ^ 2 / (2 * n_1))
      db_ET[line,]$d_var_calc <-  (2 * (1 - db_ET[line,]$corr) / db_ET[line,]$n_1) + (db_ET[line,]$d_calc ^ 2 / (2 * db_ET[line,]$n_1))
    } 
  }else if(db_ET[line,]$participant_design == "within_one"){
    #This is super important, x2 is supposed to contain chance level where applicable, 0 where not. 
    if (complete.cases(db_ET[line,]$x_1, db_ET[line,]$x_2, db_ET[line,]$SD_1)) {
      db_ET[line,]$d_calc <- (db_ET[line,]$x_1 -db_ET[line,]$x_2) / db_ET[line,]$SD_1
      db_ET[line,]$es_method  <- "group_means_one"
    } else if (complete.cases(db_ET[line,]$t)) {
      db_ET[line,]$d_calc <- db_ET[line,]$t / sqrt(db_ET[line,]$n_1)
      db_ET[line,]$es_method  <- "t_one"
    }
    if (complete.cases(db_ET[line,]$n_1, db_ET[line,]$d_calc)) {
      db_ET[line,]$d_var_calc <- (2/db_ET[line,]$n_1) + (db_ET[line,]$d_calc ^ 2 / (2 * db_ET[line,]$n_1))
    }
  }
}

#### Remove missing values ####

db_ET = db_ET[!is.na(db_ET$d_calc),]


#### Compute Hedge's g based on Cohen's d ####

# Morris, 2010, p. 21
J <- 1 - 3 / (4 * (db_ET$n_1 - 1 - 1))
db_ET$g_calc <- db_ET$d_calc * J
db_ET$g_var_calc <- J ^ 2 * db_ET$d_var_calc

#### Add weights ####

db_ET$weights_g <-  1/(db_ET$g_var_calc)^2

db_ET$weights_d <-  1/(db_ET$d_var_calc)^2


#### Outlier removal ####


# Following InWordD/standard practice, we will remove effect sizes more than 3 SD away from the median effect (in both positive and negative directions)

db_ET$nooutlier = ifelse(db_ET$g_calc > mean(db_ET$g_calc, na.rm = TRUE) + 3*sd(db_ET$g_calc, na.rm = TRUE) 
                         | db_ET$g_calc < mean(db_ET$g_calc, na.rm = TRUE) - 3*sd(db_ET$g_calc, na.rm = TRUE),FALSE, TRUE)

#db_ET = db_ET[db_ET$nooutlier,]
#02.02. 3 outlier, not removed because it might be that it's correct vs MP
