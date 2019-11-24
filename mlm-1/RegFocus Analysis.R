
MGMT <- read.csv(file = "mgmt.csv", header = T)
data <- read.csv(file = "regfocus.csv", header = T)

library(dplyr)
library(tidyselect)
library(lattice)
library(survival)
library(Formula)
library(ggplot2)
library(effects)
library(Hmisc)
library(cowplot)
library(psych)
library(lme4)
library(nlme)
library(sjPlot)
library(stargazer)

#str(MGMT)
#head(MGMT)

#str(data)
data$Timepoint <- as.factor(data$Timepoint)
#str(data)

#table(data$Response[data$Survey=="CSE"])
#data$Response[data$Survey=="CSE"]

#B-S vars
CSE <- MGMT %>% select(contains("CSE"))
ProIdent <- MGMT %>% select(contains("ProIdent"))
Motivation <- MGMT %>% select(contains("Motivation"))
LMX <- MGMT %>% select(contains("LMX"))
Volunteer <- MGMT %>% select(contains("Volunteer"))
POFit <- MGMT %>% select(contains("POFit"))
WFC <- MGMT %>% select(contains("WFC"))

#W-S vars
PANAS <- MGMT %>% select(contains("PANAS"))
Stress <- MGMT %>% select(contains("Stress"))
Fatigue <- MGMT %>% select(contains("Fatigue"))
PrevFocus <- MGMT %>% select(contains("PrevFocus"))
PromFocus <- MGMT %>% select(contains("PromFocus"))                              
TaskPerf <- MGMT %>% select(contains("TaskPerf"))
OCB <- MGMT %>% select(contains("OCB"))
CWB <- MGMT %>% select(contains("CWB"))
ProPerf <- MGMT %>% select(contains("ProPerf"))

#--------------------------------------------------scoring (CSE and Fatigue need some reverse scoring; Stress only has one value per day)

fatigue.test <- Fatigue
rev.fatigue <- fatigue.test[,(c(seq(2,32,2)))]
fatigue.adj <- 6 - rev.fatigue
MGMT.test <- MGMT
MGMT.test[,c(seq(49,454,27))] <- fatigue.adj


CSE.test <- CSE
rev.CSE <- CSE.test[,(c(seq(2,12,2)))]
CSE.adj <- 6 - rev.CSE
MGMT.test[,c(seq(2,12,2))] <- CSE.adj
CSE[,c(seq(2,12,2))] <- CSE.adj

#--------------------------------- B-S vars

#CSE
CSE_means <- rowMeans(CSE, na.rm = T)
CSE_info <- describe(CSE_means)

#ProIdent
ProIdent_means <- rowMeans(ProIdent, na.rm = T)
ProIdent_info <- round(describe(ProIdent_means), digits = 2)

#Motivation
Motivation_means <- rowMeans(Motivation, na.rm =T)
Motivation_info <- describe(Motivation_means)

#LMX
LMX_means <- rowMeans(LMX, na.rm =T)
LMX_info <- describe(LMX_means)

#Volunteer
Volunteer_means <- rowMeans(Volunteer, na.rm =T)
Volunteer_info <- describe(Volunteer_means)

#POFit
POFit_means <- rowMeans(POFit, na.rm =T)
POFit_info <- describe(POFit_means)

#WFC
WFC_means <- rowMeans(WFC, na.rm =T)
WFC_info <- describe(WFC_means)

###----------------------- W-S vars
#---------------------------------------------------aggregation
ts.vars <- aggregate(data$Response, list(Survey = data$Survey, Timepoint = data$Timepoint, ID = data$ID), mean, na.rm = T)
colnames(ts.vars)[colnames(ts.vars)=="x"] <- "Score"


#--------------------------------------------------daily means

#PANAS
PANAS_daily <- ts.vars[ts.vars$Survey=="PANAS",]
PANAS_means <- with(PANAS_daily, aggregate(Score, list(ID = ID), mean, na.rm = T))
PANAS_means$ID <- NULL
colnames(PANAS_means)[colnames(PANAS_means)=="x"] <- "PANAS_means"
PANAS_info <- describe(PANAS_means)

#Stress
Stress_daily <- ts.vars[ts.vars$Survey=="Stress",]
Stress_means <- with(Stress_daily, aggregate(Score, list(ID = ID), mean, na.rm = T))
Stress_means$ID <- NULL
colnames(Stress_means)[colnames(Stress_means)=="x"] <- "Stress_means"
Stress_info <- describe(Stress_means)

#Fatigue
#rm(Fatigue_daily)

#PrevFocus
PrevFocus_daily <- ts.vars[ts.vars$Survey=="PrevFocus",]
PrevFocus_means <- with(PrevFocus_daily, aggregate(Score, list(ID = ID), mean, na.rm = T))
PrevFocus_means$ID <- NULL
colnames(PrevFocus_means)[colnames(PrevFocus_means)=="x"] <- "PrevFocus_means"
#PrevFocus_info <- describe(PrevFocus_means)

#PromFocus
PromFocus_daily <- ts.vars[ts.vars$Survey=="PromFocus",]
PromFocus_means <- with(PromFocus_daily, aggregate(Score, list(ID = ID), mean, na.rm = T))
PromFocus_means$ID <- NULL
colnames(PromFocus_means)[colnames(PromFocus_means)=="x"] <- "PromFocus_means"
PromFocus_info <- describe(PromFocus_means)

#TaskPerf
TaskPerf_daily <- ts.vars[ts.vars$Survey=="TaskPerf",]
TaskPerf_means <- with(TaskPerf_daily, aggregate(Score, list(ID = ID), mean, na.rm = T))
TaskPerf_means$ID <- NULL
colnames(TaskPerf_means)[colnames(TaskPerf_means)=="x"] <- "TaskPerf_means"
TaskPerf_info <- describe(TaskPerf_means)

#OCB
OCB_daily <- ts.vars[ts.vars$Survey=="OCB",]
OCB_means <- with(OCB_daily, aggregate(Score, list(ID = ID), mean, na.rm = T))
OCB_means$ID <- NULL
colnames(OCB_means)[colnames(OCB_means)=="x"] <- "OCB_means"
OCB_info <- describe(OCB_means)

#CWB
CWB_daily <- ts.vars[ts.vars$Survey=="CWB",]
CWB_means <- with(CWB_daily, aggregate(Score, list(ID = ID), mean, na.rm = T))
CWB_means$ID <- NULL
colnames(CWB_means)[colnames(CWB_means)=="x"] <- "CWB_means"
#CWB_info <- describe(CWB_means)

#ProPerf
ProPerf_daily <- ts.vars[ts.vars$Survey=="ProPerf",]
ProPerf_means <- with(ProPerf_daily, aggregate(Score, list(ID = ID), mean, na.rm = T))
ProPerf_means$ID <- NULL
colnames(ProPerf_means)[colnames(ProPerf_means)=="x"] <- "ProPerf_means"
ProPerf_info <- describe(ProPerf_means)


#-------------------------------------------------------Descriptives
descriptives <- rbind(CSE_info, ProIdent_info, Motivation_info, LMX_info, Volunteer_info, POFit_info, WFC_info, PANAS_info, Stress_info, PrevFocus_info, PromFocus_info, TaskPerf_info, OCB_info, CWB_info, ProPerf_info)
`.rowNamesDF<-`(descriptives, value = c("CSE_means", "ProIdent_means", "Motivation_means", "LMX_means", "Volunteer_means", "POFit_means", "WFC_means", "PANAS_means", "Stress_means", "PrevFocus_means", "PromFocus_means", "TaskPerf_means", "OCB_means", "CWB_means", "ProPerf_means"))

#for paper
PrevFocus_info <- round(describe(PrevFocus_daily$Score), digits = 2)
CWB_info <- round(describe(CWB_daily$Score), digits = 2)
paper.desc <- rbind(CWB_info, PrevFocus_info, ProIdent_info)
paper.desc[, c(1, 6, 7, 10:13)] <- list(NULL)
row.names(paper.desc) <- c("CWB", "Prevention Focus", "Prosocial Identity")
colnames(paper.desc) <- c("n", "Mean", "SD", "Median", "min", "max")
#---------------------------------------------------- correlation matrix

means <- cbind(CSE_means, ProIdent_means, Motivation_means, LMX_means, Volunteer_means, POFit_means, WFC_means, PANAS_means, Stress_means, PrevFocus_means, PromFocus_means, TaskPerf_means, OCB_means, CWB_means, ProPerf_means)
cor.mat <- rcorr(as.matrix(means))
means.coeff <- cor.mat$r
means.pvals <- cor.mat$P
#View(means.coeff)
#View(means.pvals)


sig.coeff <- as.data.frame(means.coeff)
sig.pvals <- as.data.frame(means.pvals)

sig.coeff$Motivation_means <- NULL
sig.coeff$Volunteer_means <- NULL
sig.coeff$Stress_means <- NULL
sig.coeff[c(3, 5, 9),] <- NA
sig.coeff <- na.omit(sig.coeff)


sig.pvals$Motivation_means <- NULL
sig.pvals$Volunteer_means <- NULL
sig.pvals$Stress_means <- NULL
#changing NA vals before omission
sig.pvals[c(3, 5, 9),] <- NA
#sig.pvals <- na.omit(sig.pvals)

#--------------------------------------------------------Reliability
alpha(PrevFocus)
alpha(PromFocus)
alpha(CWB)
ProIdent.a <- ProIdent
ProIdent.a$ProIdent2 <- NULL
alpha(ProIdent.a)

#--------------------------------------------------------TS plots
plot.var <- function (i, z) {
  i$ID <- as.factor(i$ID)
  i$Timepoint <- as.numeric(i$Timepoint)
  #graphs per person
  assign(paste0(z, ".graphs"),
         ggplot(i, aes(Timepoint, Score, color = ID)) + 
           geom_line() + 
           theme_bw() +
           facet_wrap(~ID) +
           ggtitle(z),
         .GlobalEnv
  )
  #trends
  assign(paste0(z, ".trend"),
         ggplot(i, aes(Timepoint, Score, color = ID)) + 
           geom_smooth(method = "lm", se = F, fullrange = T) + 
           theme_bw() +
           ggtitle(z),
         .GlobalEnv
  )
}

#plot.var <- function (i, z) {
 # i$ID <- as.factor(i$ID)
  #i$Timepoint <- as.numeric(i$Timepoint)
#assign(paste0(z, ".trend"),
 #      ggplot(i, aes(Timepoint, Score, group = ID)) + 
  #       geom_smooth(method = "lm", se = F, fullrange = T, alpha = 0.3) + 
   #      theme_bw() +
    #     ggtitle(z),
     #  .GlobalEnv
#  )
#}

plot.var(PANAS_daily, "PANAS")
plot.var(Stress_daily, "Stress")
plot.var(PrevFocus_daily, "PrevFocus")
plot.var(PromFocus_daily, "PromFocus")
plot.var(TaskPerf_daily, "TaskPerf")
plot.var(OCB_daily, "OCB")
plot.var(CWB_daily, "CWB")
plot.var(ProPerf_daily, "ProPerf")


#---------------------------------------------------- Multilevel Model

#-------------------------- df creation
ID <- rep(1:54, each = 16)
Timepoint <- rep(1:16, 54)
LMX_scores <- rep(LMX_means, each = 16)
ProIdent_scores <- rep(ProIdent_means, each = 16)
mod.data <- data.frame(ID, Timepoint, PromFocus_daily$Score, PrevFocus_daily$Score, OCB_daily$Score, CWB_daily$Score, TaskPerf_daily$Score, LMX_scores, ProIdent_scores)
paper.data <- data.frame(ID, Timepoint, CWB_daily$Score, PrevFocus_daily$Score, ProIdent_scores)
#-------------------------- centering
mod.data$PromFocus_daily.Score_c <- mod.data$PromFocus_daily.Score - mean(mod.data$PromFocus_daily.Score, na.rm=T)
mod.data$PrevFocus_daily.Score_c <- mod.data$PrevFocus_daily.Score - mean(mod.data$PrevFocus_daily.Score, na.rm=T)
mod.data$LMX_scores_c <- mod.data$LMX_scores - mean(mod.data$LMX_scores, na.rm=T)
mod.data$ProIdent_scores_c <- mod.data$ProIdent_scores - mean(mod.data$ProIdent_scores, na.rm=T)

#-------------------------- models


#f1 <- lme(OCB_daily.Score ~ PromFocus_daily.Score_c * LMX_scores_c, random=~1|ID, data=mod.data[!is.na(mod.data$OCB_daily.Score),])
#f2 <- lme(OCB_daily.Score ~ PromFocus_daily.Score_c * ProIdent_scores_c, random=~1|ID, data=mod.data[!is.na(mod.data$OCB_daily.Score),])
#f3 <- lme(CWB_daily.Score ~ PromFocus_daily.Score_c * ProIdent_scores_c, random=~1|ID, data=mod.data[!is.na(mod.data$CWB_daily.Score),])

#f4 <- lme(OCB_daily.Score ~ PrevFocus_daily.Score_c * LMX_scores_c, random=~1|ID, data=mod.data[!is.na(mod.data$OCB_daily.Score),])
#f5 <- lme(OCB_daily.Score ~ PrevFocus_daily.Score_c * ProIdent_scores_c, random=~1|ID, data=mod.data[!is.na(mod.data$OCB_daily.Score),])
f1 <- lme(CWB_daily.Score ~ PrevFocus_daily.Score_c * ProIdent_scores_c, random= ~1|ID, data=mod.data[!is.na(mod.data$CWB_daily.Score),])
f2 <- lmer(CWB_daily.Score ~ PrevFocus_daily.Score_c * ProIdent_scores_c + (1|ID), data=mod.data[!is.na(mod.data$CWB_daily.Score),])
#f7 <- lme(TaskPerf_daily.Score ~ PrevFocus_daily.Score_c * LMX_scores_c, random=~1|ID, data=mod.data[!is.na(mod.data$TaskPerf_daily.Score),])

#sig interaction of PrevFocus and ProIdent predicting CWB
#sig interaction of PrevFocus and LMX predicting TaskPerf

#baseline
f3 <- lmer(CWB_daily.Score ~ 1 + (1|ID), data=mod.data[!is.na(mod.data$CWB_daily.Score),])

#more models
f4 <- lmer(data = mod.data[!is.na(mod.data$CWB_daily.Score),], CWB_daily.Score ~ 1 + PrevFocus_daily.Score_c*ProIdent_scores_c + (1 +PrevFocus_daily.Score_c|ID),
                 control = lmerControl(optimizer = "bobyqa",
                                       optCtrl = list(maxfun = 200000)))
tab_model(f4, f2)

sqrt(1 + 0.62*((753/54) - 1))

#------------------------------------------------- Tables
#attributes(f1)
#f1$coefficients
#stargazer(f1)


paper.corr <- as.data.frame(means.coeff[c(2,10,14),c(2,10,14)])

#descriptives <- as.data.frame(descriptives)
#paper.desc <- round(descriptives[c(2,10,14), c(2,3,4)], digits = 2)
#`.rowNamesDF<-`(paper.desc, value = c("ProIdent_means", "PrevFocus_means", "CWB_means"))

#stargazer(paper.corr)
#stargazer(paper.desc)

tab1 <- tab_df(paper.desc, title = "Table 1", col.header = c("n", "M", "SD", "Median", "min", "max"))
tab2 <- sjt.corr(paper.data[,3:5], title = "Table 2", triangle = "lower", var.labels = c("CWB", "Prevention Focus", "Prosocial Identity"), p.numeric = T, string.diag = c("-", "-", "-"))

tab3 <- tab_model(f3, f2, show.ci = F, show.se = T, show.r2 = T, show.aic = T, show.loglik = T, title = "Table 3", pred.labels = c("(Intercept)", "Prevention Focus", "Prosocial Identity", "Prevention Focus:Prosocial Identity"), dv.labels = c("Baseline Model", "Interaction Model"))



