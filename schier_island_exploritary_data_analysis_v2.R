library("ggplot2")

setwd("~/Downloads")
library(readxl)
library(bdscale)
library(scales)
library(lattice)
library(tidyverse)

#Data prep and import
dataset <- read_excel("Final_data.xlsx", 
                      col_types = c("date", "text", "text", 
                                    "text", "text", "numeric", "numeric", 
                                    "numeric", "text", "text", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric"))
dataset_df = as.data.frame(dataset)
dts <- as.Date( c("2020-09-28", "2020-09-30", "2020-10-01", "2020-10-02"))
dataset_df$newdates = as.Date(dataset_df$Date)
dataset_df$experiment_name <- paste(dataset_df$Type,dataset_df$Site)
dataset_df$experiment_name_timing <- paste(dataset_df$Type,dataset_df$Period)


dataset_ltraps = dataset_df %>% filter( Type == "Light trapping")
dataset_sweep = dataset_df %>% filter( Type == "Sweep netting")

#sex ratio per day
ggplot(dataset_df, aes(fill=Site, y=Ratio, x= newdates)) + 
  geom_bar( position="dodge", stat="identity") +
  scale_x_bd(business.dates=dts, labels=date_format('%d%b'))


#compare methods - just two columns
method_plot = ggplot(dataset_df) + 
  geom_point( aes(color=Sample, y=Ratio, x=Type, size = Total))

method_plot +
  stat_pvalue_manual(df, # only show sox effects
                     y.position=1.10, label = "Fisher's exact test, p = {pvalue}")

#everything
ggplot(dataset_df) + 
  geom_point( aes(color=Type, y=Ratio, x=newdates, size = Total,shape = Site)) +
  scale_x_bd(business.dates=dts, labels=date_format('%d%b'))
My_Theme = theme(text = element_text(size = 20))

ggplot(dataset_df) + 
  geom_point( aes(color=Type, y=Total, x=newdates, shape = Period), size=4) +
  scale_x_bd(business.dates=dts, labels=date_format('%d%b'))  + theme_bw() 


My_Theme = theme(text = element_text(size = 20))


ggplot(dataset_df, aes(x = experiment_name, y = Ratio)) +
  stat_summary(fun.data=mean_cl_boot, geom="errorbar", width=0.2) +
  stat_summary(fun=mean, geom="point", shape=21, size=8, fill = c("red","red","blue","blue")) +
  scale_y_continuous(labels=percent_format(), limits=c(0,1)) +
  theme_bw() + theme_bw() + My_Theme + labs(y = "Sex Ratio", x = " ") +
  scale_x_discrete(labels=c("Light trapping A" = "Light Traps \n Site A", 
                            "Light trapping B" = "Light Traps \n Site B",
                            "Sweep netting A" = "Sweep netting\n Site A",
                            "Sweep netting B" = "Sweep netting\n Site B"))






ggplot(dataset_df, aes(x = Type, y = Ratio)) +
  stat_summary(fun.data=mean_cl_boot, geom="errorbar", width=0.2) +
  stat_summary(fun=mean, geom="point", shape=21, size=8, fill = "red") +
  scale_y_continuous(labels=percent_format(), limits=c(0,1)) +
  theme_bw() + My_Theme + labs(y = "Sex Ratio", x = " ")


#day versus night
ggplot(dataset_df, aes(x =experiment_name_timing, y = Ratio)) +
  stat_summary(fun.data=mean_cl_boot, geom="errorbar", width=0.2) +
  stat_summary(fun=mean, geom="point", shape=21, size=8, fill = "red") +
  scale_y_continuous(labels=percent_format(), limits=c(0,1)) +
  theme_bw() + My_Theme + labs(y = "Sex Ratio", x = " ")

ggplot(dataset_df, aes(x =experiment_name, y = Ratio)) +
  stat_summary(fun.data=mean_cl_boot, geom="errorbar", width=0.2) +
  stat_summary(fun=mean, geom="point", shape=21, size=4, fill = c("red","red", "blue", "blue" )) +
  scale_y_continuous(labels=percent_format(), limits=c(0,1)) +
  theme_bw() + My_Theme + labs(y = "Sex Ratio", x = " ")




My_Theme = theme(
  axis.title.x = element_text(size = 26),
  axis.text.x = element_text(size = 26),
  axis.title.y = element_text(size = 26))

#duration of every method by day
ggplot(dataset_df) + 
  geom_point( aes(color=Type, y=Minutes_duration, x=newdates, size = Total, position="dodge", stat="identity",shape = Period)) +
  scale_x_bd(business.dates=dts, labels=date_format('%d%b'))

#seperately four plots
ggplot(dataset_df) + My_Theme +
  geom_point( aes(color=Period, y=Ratio, x=Minutes_duration, position="dodge", stat="identity",shape = Site))  +
facet_wrap(factor(dataset_df$newdates), scales = 'free_x')

#plotting temperature
ggplot(dataset_df, aes(fill=Site, y=Wind, x= newdates)) + 
  geom_bar( position="dodge", stat="identity") +
  scale_x_bd(business.dates=dts, labels=date_format('%d%b'))
ggplot(dataset_df, aes(fill=Site, y=Humidity, x= newdates)) + 
  geom_bar( position="dodge", stat="identity") +
  scale_x_bd(business.dates=dts, labels=date_format('%d%b'))
ggplot(dataset_df, aes(fill=Site, y=Temperature, x= newdates)) + 
  geom_bar( position="dodge", stat="identity") +
  scale_x_bd(business.dates=dts, labels=date_format('%d%b'))





#days and method on the same axis
ggplot(dataset_df, aes(fill=Site, y=experiment_name, x= Total)) + 
  geom_bar(position="dodge", stat="identity")



---- Logistic model
dataset_df <- dataset_df %>%
mutate(Type_2 = recode(Type,
                       "Light trapping" = "0",
                       "Sweep netting" = "1"))
                       
type_2 = dataset_df$Type_2
type_2= as.factor(type_2)
ratio = dataset_df$Ratio
duration = dataset_df$Minutes_duration
mdl <-glm(type_2 ~ duration +ratio, family = binomial() )
slope <- coef(mdl)[2]/(-coef(mdl)[3])
intercept <- coef(mdl)[1]/(-coef(mdl)[3]) 

dataset_df <- dataset_df %>% 
  mutate(Type_2 = recode(Type, 
                         "Light trapping" = "0", 
                         "Sweep netting" = "1"))

GLM<-ggplot(dataset_df) + 
  geom_point( aes(color=Type, y=Ratio, x=Minutes_duration, size = Total, position="dodge", stat="identity")) +
  geom_abline(slope = slope, intercept =intercept)
print(GLM + ggtitle("Supervised Learning for Categorical Variables"))

hist(dataset_df$Ratio, breaks = 5)
hist(dataset_df$Ratio, breaks = 10)


pvalue =c(3.414e-09)
group1 = c(1)
group2 = c(2)

df = data.frame(group1,group2, pvalue)

---
prim_sec_ratio <- read_excel("prim_sec_ratio.xlsx", 
                             col_types = c("text", "text",  
                                           "numeric", "text"))

prim_sec_ratio = as.data.frame(prim_sec_ratio)
prim_sec_ratio$Source2 = factor(prim_sec_ratio$Source, levels  = levels)

prim_sec_ratio$Sex_Ratio = as.numeric(prim_sec_ratio$Sex_Ratio)

ggplot(prim_sec_ratio) + 
  geom_point( aes(x = factor(Source2), y =Sex_Ratio, color = Method),size = 5) +
  theme_bw() + theme_bw() + My_Theme  +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1)) +
  theme(axis.text.x = element_text(angle = 45,hjust = 1))
prim_sec_ratio_bias = prim_sec_ratio %>% filter( Method == "Activity Bias")
ggplot(prim_sec_ratio_bias) +
  geom_point( aes(x = factor(Source), y =Sex_Ratio, color = Method, shape = Method),size = 5) +
  theme_bw() + theme_bw() + My_Theme  +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1)) +
  theme(axis.text.x = element_text(angle = 45,hjust = 1))

  
levels  = c("Ronald, 1974",
            "Traynier, 1970",
            "Lovibond, 1937",
            "Pinchin, 1936",
            "Robertson, 1939", 
            "Jackson, 1975",
            "InsectTeam_traps, 2020",
            "Thompson, 1946", 
            "Jackson, 1972",
            "Coulson, 1962",
            "Barnes, 1937",
            "Jackson, 1973",
            "InsectTeam_nets, 2020")