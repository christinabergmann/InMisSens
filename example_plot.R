#setwd("~/Dropbox/MP_MA")

library(ggplot2)
library(wesanderson)
library(reshape)
library(doBy)

std <- function(x) sd(x)/sqrt(length(x))

correct <- rnorm(100, mean=0.730396429, sd=0.10471147)
mispronunciation <- rnorm(100, mean=0.612614286, sd=0.093135265)

cm <- data.frame(cbind(correct, mispronunciation))
cm$sub <- seq(1, nrow(cm))

dat.m2 <- melt(cm,
               id=c(3),
               measure=c(1:2),
               variable="Measure", na.rm=T)
names(dat.m2) <- c("sub", "condition", "PTL")

# Color Blind palette:
cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


box <- ggplot(data=dat.m2, aes(x=condition, y=PTL, fill=condition)) +
  geom_boxplot()+
  #scale_fill_manual(values=wes_palette(name="Darjeeling"))+
  scale_fill_manual(values=cbPalette)+
  theme_bw()+
  annotate("segment", x = .67, xend = .67, y = .7, yend = .5, colour="black", size=1, arrow=arrow())+
  annotate("segment", x = 1.2, xend = 2.2, y = .96, yend = .96, colour="black", size=1)+
  annotate("text", x = .5, y = .65, parse = TRUE, label = "recognition",angle = 90, size = 10)+
  annotate("text", x = 1.75, y = .99, parse = TRUE, label = "misp_sensitivity", size = 10)+
  geom_hline(yintercept = .5)+
  coord_cartesian(ylim = c(.35, 1))+
  theme(text = element_text(size=30),
        legend.position = "bottom",
        legend.title = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())+
  ylab("Proportion of Target Looks")

ggsave("figures/Example_boxplot.jpg", box)

d.p <- summaryBy(PTL~condition, data=dat.m2, FUN=c(mean, std))


limits <- aes(ymax = PTL.mean+PTL.std, ymin=PTL.mean-PTL.std)
dodge <- position_dodge(width=0.9)


ex <- ggplot(data=d.p, aes(x=condition, y=PTL.mean, fill=condition)) +
  geom_bar(stat = "identity", size=1.5, position=position_dodge(width = 0.9)) +
  geom_errorbar(limits, width=0.25, color="black", position=position_dodge(width = 0.9))+
  scale_fill_manual(values=wes_palette(name="Darjeeling"))+
  theme_bw()+
  annotate("segment", x = 1, xend = 1, y = .7, yend = .5, colour="black", size=1, arrow=arrow())+
  annotate("segment", x = 1, xend = 2, y = .79, yend = .79, colour="black", size=1)+
  annotate("text", x = .85, y = .6, parse = TRUE, label = "recognition",angle = 90, size = 7)+
  annotate("text", x = 1.5, y = .82, parse = TRUE, label = "misp_sensitivity", size = 7)+
      geom_hline(yintercept = .5)+
  coord_cartesian(ylim = c(.4, .85))+
  theme(text = element_text(size=25),
        legend.position = "bottom",
        legend.title = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())+
  ylab("Proportion of Target Looks")

ggsave("Example_plot.jpg", ex)