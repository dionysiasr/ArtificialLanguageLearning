#Install the appropriate packages
install.packages("car")
install.packages("readxl")
install.packages("lme4")


#Read the data file
library(readxl)
setwd("C:\\Users\\Dionysia\\Denise\\PhD\\2nd year 2017-2018\\Fall semester 2017\\Evidentiality\\paper\\paper_revisions")
Experiment2_cm = read_excel("Experiment2_cm.xlsx")

#Setting up my factors
Experiment2_cm$System <- factor(Experiment2_cm$System, levels = c("Reportative", "Inferential", "Visual"))
levels(Experiment2_cm$System)
Experiment2_cm$List = as.factor(Experiment2_cm$List)
Experiment2_cm$ID = as.factor(Experiment2_cm$ID)
Experiment2_cm$Event = as.factor(Experiment2_cm$Event)

#reordering for List for second analysis
Experiment2_cm$List <- factor(Experiment2_cm$List, levels = c("3", "2","1"))
levels(Experiment2_cm$List)


#Load packages we will need for the analysis
library(car)
library(lme4)
library(ggplot2)
library(dplyr)

#Inspect data and create boxplot

Experiment2_cm$System <- factor(Experiment2_cm$System, levels = c("Visual", "Inferential", "Reportative"))

Experiment2_sum = Experiment2_cm%>% group_by(ID, System) %>% summarize_at(c('Acc'), funs(mean, se=sd(.)/sqrt(n())))

#p <- ggplot(Experiment2_sum, aes(y= mean, x=System))+geom_boxplot(fatten = NULL) + stat_summary(fun.y = mean, geom = "errorbar", aes(ymax = ..y.., ymin = ..y..), width = 0.75, size = 1, linetype = "solid") + coord_cartesian(ylim = c(0, 1)) #+ stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.5)

p <- ggplot(Experiment2_sum, aes(y= mean, x=System)) + geom_boxplot() + stat_summary(fun.y = mean, geom="point",colour="darkred", size=3)

#Edit the boxplot
p <- p + ggtitle("Comprehension") + theme(plot.title = element_text(lineheight=.8, face="bold",  hjust=0.5, family = "serif")) +  xlab("System" ) + ylab("Accuracy") + theme(axis.title.x = element_text(family = "serif", size = 14)) + theme(axis.title.y = element_text(family = "serif", size = 14)) + theme(axis.text.x = element_text(size = 12))

p + ylim(0,1.0)
#Contrasts coding

#System
contrasts(Experiment2_cm$System) = cbind(c(-.66, .33, .33), c(0, -.50, .50))

#Exploratory Factor List-setting contrasts by deviation from the grand mean - not using a specific list as a reference point
contrasts(Experiment2_cm$List) = contr.sum(3)


#Building models

basic <- glmer(Acc ~ 1 + (1|ID), data = Experiment2_cm, family = "binomial")
basic2 <- glmer(Acc ~ 1 + (1|ID) + (1|Event), data = Experiment2_cm, family = "binomial")
System <- glmer(Acc ~ System + (1|ID) + (1|Event), data = Experiment2_cm, family = "binomial")
List <- glmer(Acc ~ System + List + (1|ID) + (1|Event), data = Experiment2_cm, family = "binomial")

#Interactions
interactions <- glmer(Acc ~ 1 + System*List + (1|ID) + (1|Event), data = Experiment2_cm, family = "binomial", control=glmerControl("bobyqa",optCtrl=list(maxfun=100000)))

#Compare models
anova (basic, basic2, System,List) #Exploratory factor List does not significantly improve the model
anova (basic, basic2, System,List,interactions) #The interaction of the fixed predictors does not significantly improve the model
anova(System,SystemxList) # No significant difference obtained

#Final output
summary(System)
summary(List)
summary(interactions)
by(Experiment2_cm$Acc, Experiment2_cm$System, mean)
