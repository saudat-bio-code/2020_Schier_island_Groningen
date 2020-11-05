setwd("~/Downloads")
library(ggpubr)
library(ggplot2)
library(readxl)
library(bdscale)
library(scales)
library(lattice)
library(tidyverse)
library(dplyr)

My_Theme = theme(text = element_text(size = 21))

#Difference between sites:
sites = ggplot(dataset_df, aes(x = Site, y = Ratio)) +
  stat_summary(fun.data=mean_cl_boot, geom="errorbar", width=0.2) +
  stat_summary(fun=mean, geom="point", shape=21, size=5, fill = c("#F8766D","#00BCD8")) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1)) +
  My_Theme + labs(y = "Sex Ratio", x = c("Site A n=28", "Site B/n n=45")) +
  theme( panel.background = element_blank(), axis.line = element_line(colour = "black"))

group1 = c(1)
group2 = c(2)
pvalue =c(0.2) # change for every new pvalue!
df = data.frame(group1,group2, pvalue)

sites +
  stat_pvalue_manual(df, # only show sox effects
                     y.position=0.8, label = "Fisher's exact test,\n p < {pvalue}", label.size =5)

################################################################################################################################
#difference between methods
My_Theme = theme(text = element_text(size = 22))

  method_plot = ggplot(dataset_df) + 
    geom_point( aes(color=Period, y=Ratio, x=Type, size = Total)) +  
    labs(y = "Sex Ratio", x = "Method") +
    theme_bw() +  
    My_Theme +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 1)) 
    
  pvalue =c(0.001) # change for every new pvalue!
  df = data.frame(group1,group2, pvalue)
  
  method_plot +
    stat_pvalue_manual(df, # only show sox effects
                       y.position=01.25, label = "Fisher's exact test,\n p < {pvalue}", label.size =6) + My_Theme 
  ################################################################################################################################################
#difference between days
dataset_sweep = dataset_df %>% filter( Type == "Sweep netting")

days = ggplot(dataset_sweep, aes(x = Period, y = Ratio)) +
  stat_summary(fun.data=mean_cl_boot, geom="errorbar", width=0.2) +
  stat_summary(fun=mean, geom="point", shape=21, size=5, fill = c("#F8766D","#00BCD8")) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1)) +
  My_Theme + labs(y = "Sex Ratio", x = "Time (net sweeping)") +
  theme( panel.background = element_blank(), axis.line = element_line(colour = "black"))

pvalue =c(0.16) # change for every new pvalue!
df = data.frame(group1,group2, pvalue)

days +
  stat_pvalue_manual(df, # only show sox effects
                     y.position=0.8, label = "Fisher's exact test,\n p = {pvalue}", label.size =7) +My_Theme
################################################################################################################################################
#binomial visualization 
library(visualize)
visualize.binom(stat = 118, size = 288, prob = 0.5,
                  + strict = T)
visualize.binom(stat = 167, size = 251, prob = 0.5,
                  + strict = T)
visualize.binom(stat = 285, size = 539, prob = 0.5,
                + strict = T)

binomData = data.frame(Successes = rbinom(10000, 288, .5))
ggplot(binomData, aes(x = Successes)) + geom_histogram(binwidth = 1) 
