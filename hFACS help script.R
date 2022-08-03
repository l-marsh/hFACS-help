library(beeswarm) #beeswarm is used to draw data points onto of graphs
library(pheatmap)
library(tidyverse)
library(openxlsx)
library(ggpubr)
library(RColorBrewer)
library(cowplot)
require("ggrepel")
library(reshape2)
library(readxl)
library(tidyr)


#Nice Pallettes
cpallette=c("#64B2CE", "#DA5724", "#74D944", "#CE50CA", "#C0717C", "#CBD588", "#5F7FC7",
            "#673770", "#D3D93E", "#8569D5", "#508578", "#D7C1B1", "#689030", "#AD6F3B", "#CD9BCD",
            "#D14285", "#6DDE88", "#652926", "#7FDCC0", "#C84248", "#8569D5", "#5E738F", "#D1A33D",
            "#8A7C64", "#599861")

col.pal3 <-   scale_fill_manual(values=c("Donor" = "#64B2CE",
                                         "COPD" = "#DA5724",
                                         "IPAH" = "#673770",
                                         "Fibrosis" = "#74D944"))

#color_palette for heatmaps
color_palette <- c(rev(colorRampPalette(brewer.pal(9, "Blues"))(48)), "#ffffff", "#ffffff" , colorRampPalette(brewer.pal(9, "Reds"))(48))

#some helpful filtering text

#i <- which(data1.Alllung$Diagnosis == "Donor")
#i <- c(which(data1.Alllung$Diagnosis == "Donor"), which(data1.Alllung$Diagnosis == "COPD"), which(data1.Alllung$Diagnosis == "Fibrosis"))

#can be used for heatmap subsetting e.g.
#pheatmap(t(data1.Alllung[i,4:ncol(data1.Alllung)]))

#Nice plot settings for width = 4, height = 4 plots
theme_set(theme_bw(base_size=16))

axis <- theme(axis.line.x = element_line(color="black", size = 0.5), axis.line.y = element_line(color="black", size = 0.5))+
  theme(plot.title = element_text(size = rel(1),colour="black"))+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.title.x = element_text(size = rel(0.6), hjust = 1))+
  theme(axis.title.y =element_text(margin=margin(0,10,0,0)))+
  theme(axis.text = element_text(colour="black"))+
  theme(panel.grid = element_blank())+
  theme(plot.caption = element_text(size = rel(0.6)))


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

#plot facet histograms v2
histo2 <- function(x) {
  nm <- deparse(substitute(x))#extacts the name of the df

  ggplot(x, aes(value, fill=Diagnosis))+
    geom_histogram(alpha=0.6)+
    axis+
    labs(title = nm)+
    facet_wrap(~variable, scales = "free", ncol = 4)+
    theme(legend.position = c(0.9, 0.05))
}


#gplot function
myPlot <- function(index) {
  ggplot(x, aes_string(x = "Diagnosis", y = index))+
    geom_boxplot(varwidth = TRUE, notch = FALSE, color="white", aes(fill=Diagnosis), alpha=0.4) +
    geom_dotplot(aes(fill=Diagnosis), dotsize = 0.75, binaxis="y", stackdir="center") +
    labs(x = "", y = "% cells", title = index)+
    col.pal3+
    axis+
    theme(axis.text.x = element_text(angle=90))+
    theme(legend.position="none")+
    facet_wrap(~Tissue, ncol = 4)+
    stat_compare_means(comparisons = my_comparisons, hide.ns = T, label = "p.signif")

  ggsave(filename=paste("Plots/TVcompare_",index,".png",sep=""), last_plot(),dpi = 300, width = 6, height = 4)
}


#PCA function to faciliate analysis
PCA <- function(p) {
  df <<- prcomp(p[,4:ncol(p)], scale = T, center = T) #cd45 data excluded to concentration on sub populations
  pcVar <- summary(df)
  plot (df)
  biplot(df, scale = 0)
  rota <<- df$rotation
  varPC1 <- round(pcVar$importance[2,1], digits = 2)
  varPC2 <- round(pcVar$importance[2,2], digits = 2)
  varPC3 <- round(pcVar$importance[2,3], digits = 2)

  #print(names(p))
  nm <- deparse(substitute(p))#extacts the name of the df

  PC<<-data.frame(df$x, Lung.Matchcode= p$Lung.Matchcode, Diagnosis=p$Diagnosis)
  p1 <<- ggplot(PC,aes(x=PC1,y=PC2,col=Diagnosis))+
    geom_point(size=4,alpha=1)+
    man.col+
    labs(x = paste0("PC1 (", varPC1*100, "%)"), y = paste0("PC2 (", varPC2*100, "%)"), title = nm)+
    theme(legend.position = "bottom")
plot(p1)
  #ggsave(filename=paste(plotdir,"/", nm,"_1.png",sep=""), last_plot(),dpi = 300, width = 6, height = 5)
}




# Ran plots -----------
#My plot function
myplot <- function(df, index) {
  print(
    ggplot(df, aes_string(x = "Diagnosis", y = index, fill = "Diagnosis"))+
      geom_violin(color="white", alpha=0.2, lwd= 1)+
      geom_dotplot(dotsize = 1.3, binaxis="y", stackdir="center") +
      stat_summary(fun = median, fun.min = median, fun.max = median,
                   geom = "crossbar", width = 0.5)+
      labs(x = "",
           y = expression(Units),
           title = index)+
      stat_compare_means(hide.ns = T)+ 
      man.fill+
      man.col+
      axis+
      theme(legend.position="none")+
      theme(plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"))
  )
}

#plot functions
scatter_fun = function(x, y) {
  ggplot(df, aes(x = .data[[x]], y = .data[[y]]) ) +
    geom_point(aes(color = Diagnosis)) +
    #col.col+
    geom_smooth(method = "lm", se = TRUE, color = "grey74") +
    theme_bw() +
    labs(x = x,
         y = y)+
    scale_x_log10()+ #needed for x conc
    scale_y_log10()+ #needed for y conc
    stat_cor(method = "spearman")+
    theme(legend.position = "none")
}

#ggplot corrplot
corr.gg = function (df, i) {
  df1 <- expand.grid(colnames(df)[i:ncol(df)], colnames(df)[i:ncol(df)])
  x <- df1$Var1 == df1$Var2
  df1 <- df1[!x,]
  df1$rho <- apply(df1, 1, function(v) cor.test(df[[ v[1] ]], df[[ v[2] ]], method = c("spearman"))$estimate)
  df1$p.value <- apply(df1, 1, function(v) cor.test(df[[ v[1] ]], df[[ v[2] ]], method = c("spearman"))$p.value)
  df1$p.value <- round(df1$p.value, 4)
  
  
  p <- ggplot(df1, aes(rho, -log(p.value))) + geom_point() +
    geom_label_repel(data = df1[df1$p.value <= 0.01,], aes(rho, -log(p.value), label = paste0(Var1, "_", Var2)))
  
  plot(p)
  
  df3 <- df1[df1$p.value <= 0.01,] 
  nrow(df3)
  hsize <<- ceiling(nrow(df3)/5)*3
  
  
  index1 <- as.character(df3$Var1)
  index2 <- as.character(df3$Var2)
  
  plot.list.all <- list()
  for(i in 1:length(index1)){
    plot.list.all[[i]] <- scatter_fun(index1[i], index2[i])
  }
  plot.list.all  <<- plot.list.all 
  cowplot::plot_grid(plotlist = plot.list.all, ncol = 5)
}















