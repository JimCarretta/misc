 library(ggplot2)
 rm(list=ls())

 setwd("c:/carretta/github/Stock_Assessments")

 data <- read.csv("Gray Whale Abund Time Series.csv")
 
 # function ʻannotate with ʻrectʻ applies a shaded box designating the gray whale unusual mortality events (UMEs)
 
 ggplot(data) + 
   annotate("rect", xmin = 1999, xmax = 2001, ymin = -Inf, ymax = max(data$UCL), fill = "gray", alpha = 0.4) +
   annotate("rect", xmin = 2019, xmax = 2023, ymin = -Inf, ymax = max(data$UCL), fill = "gray", alpha = 0.4) +
    geom_errorbar(data=data, aes(x=Year, y=Abundance, ymin=LCL, ymax=UCL, color=Method), width=1, size=2) +
     geom_point(aes(x=Year, y=Abundance, color=Method), size=5) +
      scale_x_continuous(breaks = data$Year, labels = data$Year) +
        ylim(9000,max(data$UCL)) +
         theme_classic() +
          theme(axis.text.x=element_text(angle=90,vjust=0))
      
 