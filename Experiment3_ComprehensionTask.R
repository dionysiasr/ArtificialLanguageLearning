#Install the appropriate packages
install.packages("car")
install.packages("readxl")
install.packages("lme4")


#Read the data file
library(readxl)
setwd("C:\\Users\\Dionysia\\Denise\\PhD\\2nd year 2017-2018\\Fall semester 2017\\Evidentiality\\paper\\paper_revisions")
Experiment3_cm = read_excel("Experiment3_cm.xlsx")
View(Experiment3_cm)

#Load packages we will need for the analysis
library(car)
library(lme4)
library(ggplot2)
library(dplyr)
#Inspect data
Experiment3_cm$System <- factor(Experiment3_cm$System, levels = c("Visual", "Inferential", "Reportative"))

Experiment3_sum = Experiment3_cm%>% group_by(ID, System) %>% summarize_at(c('Acc'), funs(mean, se=sd(.)/sqrt(n())))
#ggplot(Experiment3_sum, aes(y= mean, x=System))+geom_boxplot()


p <- ggplot(Experiment3_sum, aes(y= mean, x=System))+geom_boxplot(fatten = NULL) + stat_summary(fun.y = mean, geom = "errorbar", aes(ymax = ..y.., ymin = ..y..), width = 0.75, size = 1, linetype = "solid") + coord_cartesian(ylim = c(0, 1)) #+ stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.5)
p <- ggplot(Experiment3_sum, aes(y= mean, x=System))+geom_boxplot() + stat_summary(fun.y = mean, geom="point",colour="darkred", size=3) #stat_summary(fun.y = mean, geom = "errorbar", aes(ymax = ..y.., ymin = ..y..), width = 0.75, size = 1, linetype = "solid") + coord_cartesian(ylim = c(0, 1)) #+ stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.5)

print(p)
p <- p + ggtitle("Comprehension") + theme(plot.title = element_text(lineheight=.8, face="bold",  hjust=0.5, family = "serif")) +  xlab("System") + ylab("Accuracy") + theme(axis.title.x = element_text(family = "serif", size = 14)) + theme(axis.title.y = element_text(family = "serif", size = 14)) + theme(axis.text.x = element_text(size = 12))
p + ylim(0,1.0)

#Experiment1_sum2 = Experiment1%>% group_by(ID, List) %>% summarize_at(c(Acc), funs(mean, se=sd(.)/sqrt(n())))
#ggplot(Experiment3_sum, aes(y= mean, x=List))+geom_boxplot()

#Setting up my factors
Experiment3_cm$System <- factor(Experiment3_cm$System, levels = c("Reportative", "Inferential", "Visual"))
levels(Experiment3_cm$System)
Experiment3_cm$List = as.factor(Experiment3_cm$List)
Experiment3_cm$ID = as.factor(Experiment3_cm$ID)
Experiment3_cm$Event = as.factor(Experiment3_cm$Event)

#Contrasts coding
#System
contrasts(Experiment3_cm$System) = cbind(c(-.66, .33, .33), c(0, -.50, .50))

#setting contrasts for the exploratory factor List by deviation from the grand mean - not using a specific list as a reference point
contrasts(Experiment3_cm$List) = contr.sum(3)

#Building models

basic <- glmer(Acc ~ 1 + (1|ID), data = Experiment3_cm, family = "binomial", control=glmerControl("bobyqa",optCtrl=list(maxfun=100000)))
basic2 <- glmer(Acc ~ 1 + (1|ID) + (1|Event), data = Experiment3_cm, family = "binomial")
System <- glmer(Acc ~ System + (1|ID) + (1|Event), data = Experiment3_cm, family = "binomial")
List <- glmer(Acc ~ System + List + (1|ID) + (1|Event), data = Experiment3_cm, family = "binomial")

#Interactions
SystemxList <- glmer(Acc ~ System * List + (1|ID) + (1|Event), data = Experiment3_cm, family = "binomial", control=glmerControl("bobyqa",optCtrl=list(maxfun=100000)))

#Compare models
anova (basic, basic2, System, List)
anova (basic, basic2, System, List, SystemxList)
anova(System,SystemxList)

#Final Output
summary(System)

