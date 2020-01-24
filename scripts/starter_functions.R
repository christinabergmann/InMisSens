
r2 <- function(x){ round(x, 3)}

psig <- function(x){
  ifelse(x < .001, "< .001",
         #ifelse(x < .001, "< .001",
         #ifelse(x < .01, "< .01",
         ifelse(x > .001 & x < .05, paste("=", r2(x), sep = " "), paste("=", r2(x), sep = " ")))#))
}

# function for creating 1 line paired t.test results
t.xtable <- function(x) as.data.frame(xtable(
  t_out(toutput=x, n.equal = TRUE, welch.df.exact = TRUE, welch.n = NA,
        d.corr = TRUE, print = TRUE)
))


g_SE <- function(x) paste0(r2(x$estimate), " (SE = ", r2(x$se), ")")

CI_p <- function(x) paste0("(CI [", r2(x$ci.lb), ", ", r2(x$ci.ub), "], *p* ", psig(x$pval), ")")

full_estimate <- function(x) paste0(" = ", r2(x$estimate), ", SE = ", r2(x$se), ", 95% CI[",  r2(x$ci.lb), ", ", r2(x$ci.ub),"], *p* ", psig(x$pval))


# moderator test
mod_test <- function(x) paste0("QM(", r2(x$m), ") = ", r2(x$QM), ", *p* ", psig(x$QMp))


# Create the function for mode.
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Plotting defaults

#Themes and plot
apatheme=theme_bw()+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.border=element_blank(),
        axis.line=element_line(),
        text=element_text(family='Times', size=25))

# Color Blind palette:
cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")