
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
cbPalette <- c("#F0E442","#0072B2","#CC79A7", "#E69F00", "#999999", "#56B4E9", "#009E73", "#0072B2", "#D55E00")



get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

grid_arrange_shared_legend <- function(..., ncol = length(list(...)), nrow = 1, position = c("bottom", "right")) {
  
  plots <- list(...)
  position <- match.arg(position)
  g <- ggplotGrob(plots[[1]] + theme(legend.position = position))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  lwidth <- sum(legend$width)
  gl <- lapply(plots, function(x) x + theme(legend.position="none"))
  gl <- c(gl, ncol = ncol, nrow = nrow)
  
  combined <- switch(position,
                     "bottom" = arrangeGrob(do.call(arrangeGrob, gl),
                                            legend,
                                            ncol = 1,
                                            heights = unit.c(unit(1, "npc") - lheight, lheight)),
                     "right" = arrangeGrob(do.call(arrangeGrob, gl),
                                           legend,
                                           ncol = 2,
                                           widths = unit.c(unit(1, "npc") - lwidth, lwidth)))
  
  grid.newpage()
  grid.draw(combined)
  
  # return gtable invisibly
  invisible(combined)
  
}



