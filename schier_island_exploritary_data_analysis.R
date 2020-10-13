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
dataset_df$experiment_name <- paste(dataset_df$Date,dataset_df$Type)
dataset_ltraps = dataset_df %>% filter( Type == "Light trapping")
dataset_sweep = dataset_df %>% filter( Type == "Sweep netting")

#sex ratio per day
ggplot(dataset_df, aes(fill=Site, y=Ratio, x= newdates)) + 
  geom_bar( position="dodge", stat="identity") +
  scale_x_bd(business.dates=dts, labels=date_format('%d%b'))

#compare methods - just two columns
ggplot(dataset_df) + 
  geom_point( aes(color=Period, y=Ratio, x=Type, size = Total)) 

#everything
ggplot(dataset_df) + 
  geom_point( aes(color=Type, y=Ratio, x=newdates, size = Total, position="dodge", stat="identity",shape = Site)) +
  scale_x_bd(business.dates=dts, labels=date_format('%d%b'))
#duration of every method by day
ggplot(dataset_df) + 
  geom_point( aes(color=Type, y=Minutes_duration, x=newdates, size = Total, position="dodge", stat="identity",shape = Period)) +
  scale_x_bd(business.dates=dts, labels=date_format('%d%b'))

#seperately four plots
ggplot(dataset_df) + 
  geom_point( aes(color=Type, y=Ratio, x=Minutes_duration, size = Total, position="dodge", stat="identity",shape = Site))  +
facet_wrap(factor(dataset_df$newdates), scales = 'free_x')

#plotting temperature
ggplot(dataset_df, aes(fill=Site, y=Wind, x= newdates)) + 
  geom_bar( position="dodge", stat="identity") +
  scale_x_bd(business.dates=dts, labels=date_format('%d%b'))
ggplot(dataset_df, aes(fill=Site, y=Humidity, x= newdates)) + 
  geom_bar( position="dodge", stat="identity") +
  scale_x_bd(business.dates=dts, labels=date_format('%d%b'))
ggplot(dataset_df, aes(fill=Site, y=Wind, x= newdates)) + 
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
