#### Bias and Replicability Analyses ####

# This script was adapted from Christina Bergmann's script, which was
# written to analyze bias and replicability in statistical learning studies 
# (segmenting artificial mini-languages based on statistics). 
# Original Author: Christina Bergmann
# chbergma'at'gmail.com
# last modified: January 22, 2018
#   by Katie Von Holzen
#### Load libraries ####

library(metafor)
library(pwr)

#### get data ####

source("scripts/Meta_Analysis.R")

db_mpe <- dat
rm(dat)

#### get p curve code ####

#source("scripts/p_curve_app_code.R")


#### Publication Bias ####

# Publication bias can become visible in funnel plot asymmetry. 
# Metafor comes with several options to check this, I chose ranktest and regtest

# this is the model of the mispronunciation effect, without age
ranktest(rma_MPeffect)

# correct object identification
ranktest(rma_correct)

# mispronounced object identification
ranktest(rma_MP)


# from models where we removed g > 2
# correct object
ranktest(rma_co2)

# mispronounced object
ranktest(rma_mp2)

#No longer possible with switch from rma to rma.mv
#regtest(rma_all)
#regtest(rma_DR)

# All datasets seem to have significant asymmetry 
# See below for funnel plots

#### Export Data for p-curve ####
#The very pedestrian way because reasons. 

db <- db_ET_correct
#db <- db_ET_MP
#db <- db_mpe


# save text strings to paste into p-curve app
# http://www.p-curve.com/app4/

#sink("p_curve_app/p_curve_co.txt")
#sink("p_curve_app/p_curve_mp.txt")

for(line in 1:length(db$n_1)){
  if(!is.na(db[line,]$t)){
    newline = paste("t(", db[line,]$n_1-1, ")=",db[line,]$t, sep = "")
    #write(newline, file = textfile, append = TRUE)
    cat(newline)
    cat("\n")
    #write(newline, file = textfile, append = TRUE)    
  }
}

#sink()

#pcurve_app("p_curve_mp.txt","p_curve_app")
#pcurve_app("p_curve_co.txt","p_curve_app")

#### Power ####

db_test <- db_ET_correct
#db_test <- db_ET_MP
#db_test <- db_mpe

# What is the average power to detect an effect (to be compared with p-curve estimate)
d = rma.mv(g_calc, g_var_calc, data = db_test, random = ~ 1 | short_cite)$b[,1]
# Let's try to extract the ES automagically later, for now I enter the value I had when running fir model by hand

pwr.t.test(n = median(db_test$n_1, na.rm=TRUE), d = .95, sig.level = .05, type = "paired", alternative = "two.sided")

pwr.t.test(d = .228, sig.level = .05, type = "paired", power = .8, alternative = "two.sided")





#### Draw Funnel Plots ####
# Mispronunciation Effect
summary(rma_MPeffect)
ME_estimate = 0.5310
ME_se = 0.0356
mpe <- funnel.rma(rma_MPeffect, main="Standard Error",
           refline = ME_estimate, xlab = "Hedges' g")
abline(v = 0, col = "gray60", lty = 2)
abline(lm(x ~ y, data = mpe), lty = 3)

# Correct Object
summary(rma_correct)
CO_estimate = 0.9455
CO_se = 0.1067
co <- funnel(rma_correct, main="Standard Error",
       refline = CO_estimate, xlab = "Hedges' g")
abline(v = 0, col = "gray60", lty = 2)
abline(lm(x ~ y, data = co), lty = 3)

# Mispronunciation Object
summary(rma_MP)
MP_estimate = 0.2725
MP_se = 0.0569
mp <- funnel(rma_MP, main="Standard Error",
       refline = MP_estimate, xlab = "Hedges' g")
abline(v = 0, col = "gray60", lty = 2)
abline(lm(x ~ y, data = mp), lty = 3)


#### Plots from Sakaluk, 2016 ####
# https://sakaluk.wordpress.com/2016/02/16/7-make-it-pretty-plots-for-meta-analysis/

#### Forest Plots from Sakaluk, 2016 ####

#Call the ggplot2 package, if you haven't already done so
library(ggplot2)
library(grid)
library(gridExtra)

#Rename a bunch of things for ease
dat=db_mpe
dat <-
  dat %>% 
  rename(cite = short_cite,
         yi = g_calc,
         vi = g_var_calc)

#Reorder bibliographic info based on value of g (yi), so effect sizes can be plotted in descending order

dat <-
  dat %>% 
  select(cite, expt_num, same_infant, mean_age_1, yi, vi) %>% 
  mutate(study_ref = paste(cite, expt_num, same_infant, sep=',')) %>% 
  arrange(desc(yi))

#Get standard errors from variances
dat$se = sqrt(dat$vi)

#Calculate 95% CI values
dat$lowerci = (-1.96*dat$se)+dat$yi
dat$upperci = (1.96*dat$se)+dat$yi

#Themes and plot
apatheme=theme_bw()+
  theme(#panel.grid.major=element_blank(),
    #panel.grid.minor=element_blank(),
    #panel.border=element_blank(),
    axis.line=element_line(),
    text=element_text(family='Times', size=25),
    legend.position='none')
p=
  ggplot(dat, aes(y=reorder(study_ref, -yi), x=yi, xmin=lowerci, xmax=upperci))+
  #Add data points and color them black
  geom_point(color = 'black')+
  #Add 'special' points for the summary estimates, by making them diamond shaped
  #  geom_point(data=subset(dat, tester=='Summary'), color='black', shape=18, size=4)+
  #add the CI error bars
  geom_errorbarh(height=.1)+
  #Specify the limits of the x-axis and relabel it to something more meaningful
  scale_x_continuous(limits=c(-1.5,1.5), name='Standardized Mean Difference (g)')+
  #Give y-axis a meaningful label
  ylab('Reference')+
  #Add a vertical dashed line indicating an effect size of zero, for reference
  geom_vline(xintercept=0, color='grey', linetype='dashed')+
  #Add a vertical line to show mean effect
  geom_vline(xintercept=0.19, color='black', linetype='dashed')+
  #Create sub-plots (i.e., facets) based on levels of setting
  #And allow them to have their own unique axes (so authors don't redundantly repeat)
  #facet_grid(setting~., scales= 'free', space='free')+
  #Apply my APA theme
  apatheme

p

ggsave("figures/ForestPlot_DR.png", p)

###############################################################################
###############################################################################
#### Funnel Plots from Sakaluk, 2016 ####
###############################################################################
###############################################################################

# CORRECT PRONUNCIAITON OBJECT IDENFICATION

#Rename a bunch of things for ease
dat_co = db_ET_co2
#dat_co=db_ET_correct
dat_co <-
  dat_co %>% 
  rename(cite = short_cite,
         yi = g_calc,
         vi = g_var_calc)

#Reorder bibliographic info based on value of g (yi), so effect sizes can be plotted in descending order

dat_co <-
  dat_co %>% 
  select(cite, expt_num, same_infant, mean_age_1, yi, vi) %>% 
  mutate(study_ref = paste(cite, expt_num, same_infant, sep=',')) %>% 
  arrange(desc(yi))

#Get standard errors from variances
dat_co$se = sqrt(dat_co$vi)

#Calculate 95% CI values
dat_co$lowerci = (-1.96*dat_co$se)+dat_co$yi
dat_co$upperci = (1.96*dat_co$se)+dat_co$yi


#Store the meta-analytic estimate and its standard error from whatever model you run (substitute your own values)
summary(rma_correct)
estimate_co = 0.9455
se_co = 0.1067

#Store a vector of values that spans the range from 0
#to the max value of impression (standard error) in your dataset.
#Make the increment (the final value) small enough (I choose 0.001)
#to ensure your whole range of data is captured
se.seq_co=seq(0, max(dat_co$se), 0.001)

#Now, compute vectors of the lower-limit and upper limit values for
#the 95% CI region, using the range of SE that you generated in the previous step, 
#and the stored value of your meta-analytic estimate.
ll95_co = estimate_co-(1.96*se.seq_co)
ul95_co = estimate_co+(1.96*se.seq_co)

#You can do this for a 99% CI region too
ll99_co = estimate_co-(3.29*se.seq_co)
ul99_co = estimate_co+(3.29*se.seq_co)

#And finally, do the same thing except now calculating the confidence interval
#for your meta-analytic estimate based on the stored value of its standard error
meanll95_co = estimate_co-(1.96*se_co)
meanul95_co = estimate_co+(1.96*se_co)

#Now, smash all of those calculated values into one data frame (called 'dfCI').
#You might get a warning about '...row names were found from a short variable...'
#You can ignore it.
dfCI_co = data.frame(ll95_co, ul95_co, ll99_co,
                  ul99_co, se.seq_co, estimate_co, meanll95_co, meanul95_co)

#Now we can actually make the funnel plot.
#Using your original data-frame, map standard error to your x-axis (for now) and Zr to your y-axis
fp_co = ggplot(aes(x = se, y = yi), data = dat_co) +
  #Regression line for the FP asymmetry
  geom_smooth(aes(x = se, y = yi), method = "lm", colour = "darkgrey", alpha = .5, se = FALSE, data = dat_co) +
  #Add your data-points to the scatterplot
  geom_point(size = 2.5, colour="black") +
  #
  ggtitle("Correct")+
  #Give the x- and y- axes informative labels
  xlab('Standard Error') + ylab('Hedge\'s g')+
  # make sure both plots have same scale
  # still ylim because we haven't flipped yet
  #coord_cartesian(ylim = c(-1, 2.5))+
  #Now using the 'dfCI' data-frame we created, plot dotted lines corresponding
  #to the lower and upper limits of your 95% CI region
  #And dashed lines corresponding to your 99% CI region
  #Add lines corresponding to 0 and estimate
  geom_line(aes(x = se.seq_co, y = 0), linetype = 'solid', data = dfCI_co) +
  geom_line(aes(x = se.seq_co, y = estimate_co), linetype = 'dashed', data = dfCI_co) +
  geom_line(aes(x = se.seq_co, y = ll95_co), linetype = 'dotted', data = dfCI_co) +
  geom_line(aes(x = se.seq_co, y = ul95_co), linetype = 'dotted', data = dfCI_co) +
  #  geom_line(aes(x = se.seq, y = ll99), linetype = 'dashed', data = dfCI) +
  #  geom_line(aes(x = se.seq, y = ul99), linetype = 'dashed', data = dfCI) +
  #Now plot dotted lines corresponding to the 95% CI of your meta-analytic estimate
  #geom_segment(aes(x = min(se.seq), y = meanll95, xend = max(se.seq), yend = meanll95), linetype='dotted', data=dfCI) +
  #geom_segment(aes(x = min(se.seq), y = meanul95, xend = max(se.seq), yend = meanul95), linetype='dotted', data=dfCI) +
  #Reverse the x-axis ordering (se) so that the tip of the funnel will appear
  #at the top of the figure once we swap the x- and y-axes...
  scale_x_reverse()+
  #Specify the range and interval for the tick-marks of the y-axis (Zr);
  #Choose values that work for you based on your data
  #scale_y_continuous(breaks=seq(-.45,0.8,0.25))+
  #And now we flip the axes so that SE is on y- and Zr is on x-
  coord_flip()+
  #Finally, apply my APA-format theme (see code at end of post).
  #You could, alternatively, specify theme_bw() instead.
  apatheme

#Call the pretty funnel plot
#fp
#ggsave("figures/FunnelPlot_DR.pdf")

################################################################
  # MISPRONUNCIAITON OBJECT IDENFICATION
  
  #Rename a bunch of things for ease
dat_mp = db_ET_mp2  
#dat_mp=db_ET_MP
  dat_mp <-
    dat_mp %>% 
    rename(cite = short_cite,
           yi = g_calc,
           vi = g_var_calc)
  
  #Reorder bibliographic info based on value of g (yi), so effect sizes can be plotted in descending order
  
  dat_mp <-
    dat_mp %>% 
    select(cite, expt_num, same_infant, mean_age_1, yi, vi) %>% 
    mutate(study_ref = paste(cite, expt_num, same_infant, sep=',')) %>% 
    arrange(desc(yi))
  
  #Get standard errors from variances
  dat_mp$se = sqrt(dat_mp$vi)
  
  #Calculate 95% CI values
  dat_mp$lowerci = (-1.96*dat_mp$se)+dat_mp$yi
  dat_mp$upperci = (1.96*dat_mp$se)+dat_mp$yi
  
  
  #Store the meta-analytic estimate and its standard error from whatever model you run (substitute your own values)
  summary(rma_MP)
  estimate_mp = 0.2725
  se_mp = 0.0569
  
  #Store a vector of values that spans the range from 0
  #to the max value of impression (standard error) in your dataset.
  #Make the increment (the final value) small enough (I choose 0.001)
  #to ensure your whole range of data is captured
  se.seq_mp=seq(0, max(dat_mp$se), 0.001)
  
  #Now, compute vectors of the lower-limit and upper limit values for
  #the 95% CI region, using the range of SE that you generated in the previous step, 
  #and the stored value of your meta-analytic estimate.
  ll95_mp = estimate_mp-(1.96*se.seq_mp)
  ul95_mp = estimate_mp+(1.96*se.seq_mp)
  
  #You can do this for a 99% CI region too
  ll99_mp = estimate_mp-(3.29*se.seq_mp)
  ul99_mp = estimate_mp+(3.29*se.seq_mp)
  
  #And finally, do the same thing except now calculating the confidence interval
  #for your meta-analytic estimate based on the stored value of its standard error
  meanll95_mp = estimate_mp-(1.96*se_mp)
  meanul95_mp = estimate_mp+(1.96*se_mp)
  
  #Now, smash all of those calculated values into one data frame (called 'dfCI').
  #You might get a warning about '...row names were found from a short variable...'
  #You can ignore it.
  dfCI_mp = data.frame(ll95_mp, ul95_mp, ll99_mp,
                       ul99_mp, se.seq_mp, estimate_mp, meanll95_mp, meanul95_mp)
  
  #Now we can actually make the funnel plot.
  #Using your original data-frame, map standard error to your x-axis (for now) and Zr to your y-axis
  fp_mp = ggplot(aes(x = se, y = yi), data = dat_mp) +
    #Regression line for the FP asymmetry
    geom_smooth(aes(x = se, y = yi), method = "lm", colour = "darkgrey", alpha = .5, se = FALSE, data = dat_mp) +
    #Add your data-points to the scatterplot
    geom_point(size = 2.5, colour="black") +
    #
    ggtitle("Mispronunciation")+
  #Give the x- and y- axes informative labels
  xlab('Standard Error') + ylab('Hedge\'s g')+
    # make sure both plots have same scale
    # still ylim because we haven't flipped yet
    #coord_cartesian(ylim = c(-1, 2.5))+
    #Now using the 'dfCI' data-frame we created, plot dotted lines corresponding
    #to the lower and upper limits of your 95% CI region
    #And dashed lines corresponding to your 99% CI region
    #Add lines corresponding to 0 and estimate
    geom_line(aes(x = se.seq_mp, y = 0), linetype = 'solid', data = dfCI_mp) +
    geom_line(aes(x = se.seq_mp, y = estimate_mp), linetype = 'dashed', data = dfCI_mp) +
    geom_line(aes(x = se.seq_mp, y = ll95_mp), linetype = 'dotted', data = dfCI_mp) +
    geom_line(aes(x = se.seq_mp, y = ul95_mp), linetype = 'dotted', data = dfCI_mp) +
    #  geom_line(aes(x = se.seq, y = ll99), linetype = 'dashed', data = dfCI) +
    #  geom_line(aes(x = se.seq, y = ul99), linetype = 'dashed', data = dfCI) +
    #Now plot dotted lines corresponding to the 95% CI of your meta-analytic estimate
    #geom_segment(aes(x = min(se.seq), y = meanll95, xend = max(se.seq), yend = meanll95), linetype='dotted', data=dfCI) +
    #geom_segment(aes(x = min(se.seq), y = meanul95, xend = max(se.seq), yend = meanul95), linetype='dotted', data=dfCI) +
    #Reverse the x-axis ordering (se) so that the tip of the funnel will appear
    #at the top of the figure once we swap the x- and y-axes...
    scale_x_reverse()+
    #Specify the range and interval for the tick-marks of the y-axis (Zr);
    #Choose values that work for you based on your data
    #scale_y_mpntinuous(breaks=seq(-.45,0.8,0.25))+
    #And now we flip the axes so that SE is on y- and Zr is on x-
    coord_flip()+
    #Finally, apply my APA-format theme (see code at end of post).
    #You could, alternatively, specify theme_bw() instead.
    apatheme
  
  #Call the pretty funnel plot
  #fp
  #ggsave("figures/FunnelPlot_DR.pdf")
  
  
  jpeg(filename = "figures/Funnel_Plots_Object_Identification.jpg", 
       width = 600, height = 400, units = "px")
  
  p <-   grid.arrange(fp_co, fp_mp, nrow = 1, 
                      top = textGrob("Object Identification", gp=gpar(fontsize=25)))
                      #top = "Object Identification")

  
  dev.off()
  
  #ggsave("figures/Funnel_Plots_Object_Identification.jpg", p,height= 7,width= 6)
  
#This is the "classical" plot from the metafor package
#funnel(rma_DR)
