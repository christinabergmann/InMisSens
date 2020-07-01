#### Example Analysis ####

# This script creates paired t-test and one sample t-tests for the original data from Höhle et al., 2006
# Original Author: Katie Von Holzen
# katie.m.vonholzen'at'gmail.com
# last modified: June 25, 2020
#   by Katie Von Holzen
#### Load libraries ####

#setwd("~/Dropbox/MP_MA")

library(ggplot2)
library(wesanderson)
library(reshape)
library(doBy)
library(schoRsch)
library(dplyr)

std <- function(x) sd(x)/sqrt(length(x))

# Color Blind palette:
cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

set.seed(8)

correct_pre <- rnorm(100, mean=47, sd=8.854377448)
correct_post <- rnorm(100, mean=59, sd=12.01665511)

misp_pre <- rnorm(100, mean=51, sd=8.854377448)
misp_post <- rnorm(100, mean=51, sd=10.11928851)


cm <- data.frame(cbind(correct_pre, correct_post, misp_pre, misp_post))
cm$sub <- seq(1, nrow(cm))

cm$correct_diff <- cm$correct_post - cm$correct_pre
cm$misp_diff <- cm$misp_post - cm$misp_pre


# t-test pre vs. post correct
t_out(t.test(cm$correct_pre, cm$correct_post, paired =T))
# d = -0.80

# t-test pre vs. post misp
t_out(t.test(cm$misp_pre, cm$misp_post, paired =T))
# d = -0.07

# t-test diff correct
t_out(t.test(cm$correct_diff, mu = 0))
# d = 0.80

# t-test diff misp
t_out(t.test(cm$misp_diff, mu = 0))
# d = 0.07


# misp effect for difference
t_out(t.test(cm$misp_diff, cm$correct_diff, paired = T))
# d = -0.58

#######################################################
# ok,now how about if the aggregation is taking place at different steps?

sub <- 50
items <- c(1,2,3,4,5)

items <- rep(items, sub)
sub <- seq(1, sub)


d <- merge(sub, items)
names(d) <- c("sub", "items")
# no idea how to limit this properly, so do a dumb fix
d <- d[1:250,]

set.seed(8)


d$correct_pre <- rnorm(nrow(d), mean=47, sd=8.854377448)
d$correct_post <- rnorm(nrow(d), mean=59, sd=12.01665511)

d$misp_pre <- rnorm(nrow(d), mean=51, sd=8.854377448)
d$misp_post <- rnorm(nrow(d), mean=51, sd=10.11928851)

###################
# calculate difference first, then aggregate
d_diff <- d

d_diff$correct_diff <- d_diff$correct_post - d_diff$correct_pre
d_diff$misp_diff <- d_diff$misp_post - d_diff$misp_pre



d_diff <- d_diff %>%
  group_by(sub) %>%
  summarize(correct_diff = mean(correct_diff),
            misp_diff = mean(misp_diff))


# t-test diff correct
t_out(t.test(d_diff$correct_diff, mu = 0))
# d = 2.12

# t-test diff misp
t_out(t.test(d_diff$misp_diff, mu = 0))
# d = -0.01

# misp effect for difference
t_out(t.test(d_diff$misp_diff, d_diff$correct_diff, paired = T))
# d = -1.22

###################
# aggregate first, then calculate difference
d_agg <- d


d_agg <- d_agg %>%
  group_by(sub) %>%
  summarize(correct_pre = mean(correct_pre),
            correct_post = mean(correct_post),
            misp_pre = mean(misp_pre),
            misp_post = mean(misp_post))

d_agg$correct_diff <- d_agg$correct_post - d_agg$correct_pre
d_agg$misp_diff <- d_agg$misp_post - d_agg$misp_pre


# t-test diff correct
t_out(t.test(d_agg$correct_diff, mu = 0))
# d = 2.12

# t-test diff misp
t_out(t.test(d_agg$misp_diff, mu = 0))
# d = -0.01

# misp effect for difference
t_out(t.test(d_agg$misp_diff, d_agg$correct_diff, paired = T))
# d = -1.22


###################
# aggregate pre_post
d_pre_post <- d


d_pre_post <- d_pre_post %>%
  group_by(sub) %>%
  summarize(correct_pre = mean(correct_pre),
            correct_post = mean(correct_post),
            misp_pre = mean(misp_pre),
            misp_post = mean(misp_post))


# t-test pre vs. post correct
t_out(t.test(d_pre_post$correct_pre, d_pre_post$correct_post, paired =T))
# d = -2.12

# t-test pre vs. post misp
t_out(t.test(d_pre_post$misp_pre, d_pre_post$misp_post, paired =T))
# d = 0.01
