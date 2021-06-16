#Install the appropriate packages
install.packages("car")
install.packages("readxl")
install.packages("lme4")


#Read the data file
library(readxl)
setwd("C:\\Users\\Dionysia\\Denise\\PhD\\2nd year 2017-2018\\Fall semester 2017\\Evidentiality\\paper\\paper_revisions")
Experiment1 = read_excel("Experiment1.xlsx")
View(Experiment1)

#Setting up my factors
Experiment1$System <- factor(Experiment1$System, levels = c("Reportative", "Inferential", "Visual"))
levels(Experiment1$System)
Experiment1$List = as.factor(Experiment1$List)
Experiment1$ID = as.factor(Experiment1$ID)
Experiment1$Event = as.factor(Experiment1$Event)

#reordering for List for second analysis
Experiment1$List <- factor(Experiment1$List, levels = c("3", "2","1"))
levels(Experiment1$List)


#Load packages we will need for the analysis
library(car)
library(lme4)
library(ggplot2)
library(dplyr)

#Inspect data

Experiment1$System <- factor(Experiment1$System, levels = c("Visual", "Inferential", "Reportative"))
Experiment1_sum = Experiment1%>% group_by(ID, System) %>% summarize_at(c('Acc'), funs(mean, se=sd(.)/sqrt(n())))

#p <- ggplot(Experiment1_sum, aes(y= mean, x=System))+geom_boxplot(fatten = NULL) + stat_summary(fun.y = mean, geom = "errorbar", aes(ymax = ..y.., ymin = ..y..), width = 0.75, size = 1, linetype = "solid") + coord_cartesian(ylim = c(0, 1)) #+ stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.5)
p <- ggplot(Experiment1_sum, aes(y= mean, x=System)) + geom_boxplot() + stat_summary(fun.y = mean, geom="point",colour="darkred", size=3)

p <- p + ggtitle("Comprehension") + theme(plot.title = element_text(lineheight=.8, face="bold",  hjust=0.5, family = "serif")) +  xlab("System" ) + ylab("Accuracy") + theme(axis.title.x = element_text(family = "serif", size = 14)) + theme(axis.title.y = element_text(family = "serif", size = 14)) + theme(axis.text.x = element_text(size = 12))

p + ylim(0,1.0)
Experiment1_sum2 = Experiment1%>% group_by(ID, List) %>% summarize_at(c('Acc'), funs(mean, se=sd(.)/sqrt(n())))
View(Experiment1_sum2)

#Contrasts coding
#System

contrasts(Experiment1$System) = cbind(c(.66, -.33, -.33), c(0, -.50, .50))

#setting contrasts for exploratory factor List by deviation from the grand mean - not using a specific list as a reference point
contrasts(Experiment1$List) = contr.sum(3)

#Building models

basic <- glmer(Acc ~ 1 + (1|ID), data = Experiment1, family = "binomial")
basic2 <- glmer(Acc ~ 1 + (1|ID) + (1|Event), data = Experiment1, family = "binomial")
System <- glmer(Acc ~ System + (1|ID) + (1|Event), data = Experiment1, family = "binomial")
List <- glmer(Acc ~ System + List + (1|ID) + (1|Event), data = Experiment1, family = "binomial")

#Interactions
SystemxList <- glmer(Acc ~ System * List + (1|ID) + (1|Event), data = Experiment1, family = "binomial", control=glmerControl("bobyqa",optCtrl=list(maxfun=100000)))

#Compare models
anova (basic, basic2,System,List)
anova (basic, basic2, System,List,SystemxList)
anova(System, SystemxList)

#Final Output
summary(System)
summary(SystemxList)
by(Experiment1$Acc, Experiment1$System, mean)

