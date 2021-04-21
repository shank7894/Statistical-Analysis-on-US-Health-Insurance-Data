library(backports)
library(corrplot)
library(devtools)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(gridExtra)
library(gridGraphics)
library(gridtext)
library(Hmisc)
library(janitor)
library(lubridate)
library(magrittr)
library(maptools)
library(reshape2)
library(scales)
library(tidyverse)
library(treemapify)
library(usmap)
library(zeallot)
library(dplyr)
library(fitdistrplus)

US_Health_Insurance <- read.csv("C:/Users/saish/Downloads/US_Health_Insurance.csv")

#graph1
p1<-US_Health_Insurance %>% ggplot(mapping = aes(x=region, y=charges, fill=sex)) + geom_bar(stat = "identity", position="dodge")

p2<- US_Health_Insurance %>% ggplot(mapping = aes(x=sex, y=charges, fill=smoker)) + geom_bar(stat = "identity", position="dodge")

grid.arrange(p1,p2, ncol=2)

#graph 2

max(US_Health_Insurance$age)

#15-25, 25-35, 35-45, 45-55, 55-65

US_Health_Insurance$age_range <- cut_width(US_Health_Insurance$age,width=10,boundary=0)

US_Health_Insurance$Agegroup<-findInterval(US_Health_Insurance$age,c(0,25,35,45,55))

US_Health_Insurance$Agegroup<-as.factor(US_Health_Insurance$Agegroup)

levels(US_Health_Insurance$Agegroup)<-c("Below 25","25-34","35-44","45-54","55-64")

#grouping BMI

US_Health_Insurance$BMI_group<-findInterval(US_Health_Insurance$bmi,c(0,18.5,25,30))

US_Health_Insurance$BMI_group<-as.factor(US_Health_Insurance$BMI_group)

levels(US_Health_Insurance$BMI_group)<-c("Underweight","Normal","Overweight","Obesity")


p3<- US_Health_Insurance %>% ggplot(mapping= aes(x=Agegroup, y=charges, fill=sex)) +
  geom_bar(stat="identity", position="dodge")


p4<- US_Health_Insurance %>% ggplot(mapping= aes(x=children, y=charges, fill=sex)) +
  geom_bar(stat="identity")+ coord_flip()

grid.arrange(p3,p4, ncol=2)


#chart 3


US_Health_Insurance %>% ggplot(mapping= aes(x=BMI_group, y=charges, fill=smoker)) +
  geom_bar(stat="identity", position="dodge")



#try one box plot instead

#task 2

US_Health_Insurance$age_range <- cut_width(US_Health_Insurance$age,width=10,boundary=-1)


req_df<- US_Health_Insurance %>% group_by(age_range) %>%
  summarise(count = n())


req_df$age_pmf <- round(req_df$count/sum(req_df$count),3)
req_df$age_cdf <- round(cumsum(req_df$age_pmf),3)


req_df2<- US_Health_Insurance %>% group_by(children) %>%
  summarise(count = n())

req_df2$children_pmf <- round(req_df2$count/sum(req_df2$count),3)
req_df2$children_cdf <- round(cumsum(req_df2$children_pmf),3)

joint_freq <- outer(req_df$count, req_df2$count, FUN = "+")
joint_prob<- round(joint_freq/sum(joint_freq),3)
colnames(joint_freq) <- c('1','2','3','4','5','6')

#task 3

joint_df<- melt(joint_freq)

colnames(joint_df)<- c("age_ranges", "no_of_children", "frequency")


ggplot(data =joint_df,aes(x=age_ranges, y=no_of_children)) +
  geom_point(aes(size = frequency, color = frequency)) +
  labs(x = 'age_ranges', y = 'no of children') +
  scale_x_continuous("age_ranges", labels = as.character(joint_df$age_ranges),
                     breaks = joint_df$age_ranges) +
  scale_y_continuous("no of children", labels = as.character(joint_df$no_of_children),
                     breaks = joint_df$no_of_children)

#corelation co efficient
cor(req_df$count, req_df2$count)



#distribution fitting




#visualizing the BMI which is continuos data
ggplot(US_Health_Insurance, aes(bmi)) + geom_histogram(bins=20, colour='black', fill='steelblue')

# descriptive statistics
descdist(US_Health_Insurance$bmi)

#getting the parameter estimates accoring to the cullen and frey graph
#exponential
fit_nml<- fitdist(US_Health_Insurance$bmi, "norm")
summary(fit_nml)
#lognormal
fit_ln<- fitdist(US_Health_Insurance$bmi, "lnorm")
summary(fit_ln)
#gamma
fit_gm<- fitdist(US_Health_Insurance$bmi, "gamma")
summary(fit_gm)


#goodness of fit plots for gamma as the parameter estimates are indicative of a gamma distribution
par(mfrow=c(2,2))
plot.legend <- c("gamma")
denscomp(list(fit_gm), legendtext = plot.legend, xlab = 'Relative Humidity (x)', xlegend = 'topleft')
cdfcomp (list(fit_gm), legendtext = plot.legend, xlab = 'Relative Humidity (x)')
qqcomp  (list(fit_gm), legendtext = plot.legend, xlab = 'Relative Humidity (x)')
ppcomp  (list(fit_gm), legendtext = plot.legend, xlab = 'Relative Humidity (x)')

# visualize the Number of children which happens to be discrete data(y)
ggplot(US_Health_Insurance, aes(children)) +
  geom_histogram(bins = 20, color = 'Black', fill = 'steelblue')

# getting the paramter estimates for negative binomial distribution
fit_nb <- fitdist(US_Health_Insurance$children, 'nbinom')
summary(fit_nb)

# getting the paramter estimates for poisson distribution
fit_p <- fitdist(US_Health_Insurance$children, 'pois')
summary(fit_p)

# goodness-of-fit tests
gofstat(list(fit_nb, fit_p))


age_freq <- US_Health_Insurance %>%
  select(start.station.name, month, date, hour) %>%
  dplyr::filter(start.station.name == 'CSP' & hour == 8) %>%
  group_by(month, date, hour) %>%
  summarise(count = n()) %>%
  group_by(count) %>%
  summarise(num_days = n()) %>%
  mutate(pickup_pmf = num_days/sum(num_days)) %>%
  mutate(pickup_cdf = cumsum(pickup_pmf))






US_Health_Insurance %>% ggplot(mapping = aes(x=children, y=charges, color=sex)) + geom_point()

#ggplot(data=df2, aes(x=dose, y=len, fill=supp)) +
  geom_bar(stat="identity", position="dodge")
                               