library(beeswarm) #beeswarm is used to draw data points onto of graphs
library(pheatmap)
library(ggplot2)
library(dplyr)
library(openxlsx)
library(ggpubr)
library(RColorBrewer)
library(cowplot)
require("ggrepel")
library(reshape2)


#shapiro function
myfun <- function(x) {
res <- shapiro.test(x)$p.value
signif(res, 3)
}

#KStest function
kstest <- function(x) {
  res <- ks.test(x, "pnorm", mean(x,na.rm=TRUE), sd(x, na.rm=TRUE))$p.value
  signif(res, 3)
}

#mapping function for data transformation
map <- function(y, y.lwr=0, y.upr=100, lwr=0, upr=1, f=0.25) {
  d <- f * min(p[p>0],na.rm=TRUE)
  (p-y.lwr) * (upr-lwr-2*d)/(y.upr-y.lwr) + d + lwr
}

#plot facet histograms
histo <- function(x) {
  nm <- deparse(substitute(x))#extacts the name of the df

  ggplot(x, aes(value, fill=Disease))+
    geom_histogram(alpha=0.6)+
    axis+
    labs(title = nm)+
    facet_wrap(~variable, scales = "free", ncol = 4)+
    theme(legend.position = c(0.9, 0.25))
}

#PCA function to faciliate analysis
PCA <- function(p) {
  df <- prcomp(p[,10:ncol(p)], scale = T, center = T) #cd45 data excluded to concentration on sub populations
  pcVar <- summary(df)
  plot (df)
  biplot(df, scale = 0)
  rota <- df$rotation
  varPC1 <- round(pcVar$importance[2,1], digits = 2)
  varPC2 <- round(pcVar$importance[2,2], digits = 2)
  varPC3 <- round(pcVar$importance[2,3], digits = 2)

  #print(names(p))
  nm <- deparse(substitute(p))#extacts the name of the df

  PC<-data.frame(df$x, Disease=data1.sqrt$Disease)
  ggplot(PC,aes(x=PC1,y=PC2,col=Disease))+
    geom_point(size=4,alpha=1)+
    labs(x = paste0("PC1 (", varPC1*100, "%)"), y = paste0("PC2 (", varPC2*100, "%)"), title = nm)

  ggsave(filename=paste(plotdir,"/", nm,"_1.png",sep=""), last_plot(),dpi = 300, width = 6, height = 5)
}
