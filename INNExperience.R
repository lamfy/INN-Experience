################################################################################
#                                                                              #
#                                 INN Experience                               #
#                                                                              #
################################################################################

# ------------------------------------------------------------------------------
# ---------- Load Libraries ----------------------------------------------------
# ------------------------------------------------------------------------------

library(readxl)
library(dplyr)
library(ggplot2)
library(ggthemes)

# ------------------------------------------------------------------------------
# ---------- Load and Prepare Data ---------------------------------------------
# ------------------------------------------------------------------------------

# Set Working Directory

setwd("C:/Users/csclfya/Desktop/INN Experience"); dir()

# Load Data

data <- read_xlsx("INN_Experience_Dataset_Final.xlsx", sheet="Evaluation")

# Dichotomising Function: For a vector,
# 1. If the value is >= 4, convert it to 1; and
# 2. Otherwise, convert it to 0.

dich <- function(x) {
  x <- ifelse(is.na(x), NA,
              ifelse(x>=4, "1", "0"))
}

data <- mutate(data,
               IntentR=as.numeric(dich(data$Intent)),
               LearningR=as.numeric(dich(data$Learning)),
               ApplicationR=as.numeric(dich(data$Application)),
               PaceR=as.numeric(dich(data$Pace)),
               TrainerR=as.numeric(dich(data$Trainer)),
               EnhancementR=as.numeric(dich(data$Enhancement)),
               EnvironmentR=as.numeric(dich(data$Environment)),
               AdministrationR=as.numeric(dich(data$Administration)),
               SatisfactionR=as.numeric(dich(data$Satisfaction)),
               PerceptionR=as.numeric(dich(data$Perception)))

# ------------------------------------------------------------------------------
# ---------- Descriptive Statistics --------------------------------------------
# ------------------------------------------------------------------------------

# Obtain means and medians for each variable

mean1 <- list()
mean2 <- list()
medn1 <- list()
medn2 <- list()

for (i in c(2,4,5,6,7,8,9,10,11,12)) {
  
  mean1[i] <- mean(data[data$Location=="1",][[i]])
  mean2[i] <- mean(data[data$Location=="2",][[i]])
  medn1[i] <- median(data[data$Location=="1",][[i]])
  medn2[i] <- median(data[data$Location=="2",][[i]])

}

# Obtain proportions for each variable

prop1 <- list()
prop2 <- list()

for (i in 17:26) {
  
  prop1[i] <- mean(data[data$Location=="1",][[i]])
  prop2[i] <- mean(data[data$Location=="2",][[i]])

}

# Combine means, medians and proportions into a single dataframe

data.frame(Variable=names(data)[c(2,4,5,6,7,8,9,10,11,12)],
           Mean_CSC=round(unlist(mean1), 2),
           Mean_INN=round(unlist(mean2), 2),
           Median_CSC=unlist(medn1),
           Median_INN=unlist(medn2),
           Prop_CSC=round(unlist(prop1)*100, 0),
           Prop_INN=round(unlist(prop2)*100, 0))

# ------------------------------------------------------------------------------
# ---------- Mediation Analysis (Pearl, 2014) ----------------------------------
# ------------------------------------------------------------------------------

# Research Question:
# Does Enhancement mediate the relationship between Location and Learning?

# Direct Effect (0.0625)

((prop.table(table(data[data$EnhancementR==0,]$Location, 
                 data[data$EnhancementR==0,]$LearningR), 1)[2,2] - 
prop.table(table(data[data$EnhancementR==0,]$Location, 
                 data[data$EnhancementR==0,]$LearningR), 1)[1,2])*
prop.table(table(data$Location, data$EnhancementR), 1)[1,1])+
((prop.table(table(data[data$EnhancementR==1,]$Location, 
                 data[data$EnhancementR==1,]$LearningR), 1)[2,2]-
prop.table(table(data[data$EnhancementR==1,]$Location, 
                 data[data$EnhancementR==1,]$LearningR), 1)[1,2])*
prop.table(table(data$Location, data$EnhancementR), 1)[1,2])

(1-0.667)*0.3+(0.875-0.929)*0.7

# Indirect Effect (0.0494709)

(prop.table(table(data[data$EnhancementR==0,]$Location, 
                 data[data$EnhancementR==0,]$LearningR), 1)[1,2]*
(prop.table(table(data$Location, data$EnhancementR), 1)[2,1]-
prop.table(table(data$Location, data$EnhancementR), 1)[1,1]))+
(prop.table(table(data[data$EnhancementR==1,]$Location, 
                 data[data$EnhancementR==1,]$LearningR), 1)[1,2]*
(prop.table(table(data$Location, data$EnhancementR), 1)[2,2]-
prop.table(table(data$Location, data$EnhancementR), 1)[1,2]))

(0.889-0.7)*(0.929-0.667)

# Reverse Indirect Effect (0.02361111)

(prop.table(table(data[data$EnhancementR==0,]$Location, 
                  data[data$EnhancementR==0,]$LearningR), 1)[2,2]*
    (prop.table(table(data$Location, data$EnhancementR), 1)[1,1]-
       prop.table(table(data$Location, data$EnhancementR), 1)[2,1]))+
  (prop.table(table(data[data$EnhancementR==1,]$Location, 
                    data[data$EnhancementR==1,]$LearningR), 1)[2,2]*
     (prop.table(table(data$Location, data$EnhancementR), 1)[1,2]-
        prop.table(table(data$Location, data$EnhancementR), 1)[2,2]))

# Total Effect (0.03888889)

prop.table(table(data$Location, data$LearningR), 1)[2,2]-
prop.table(table(data$Location, data$LearningR), 1)[1,2]

(0.875*0.889)+(1-0.889)-((0.929*0.7)+(0.667*(1-0.7)))

# ------------------------------------------------------------------------------
# ---------- Data Visualisation ------------------------------------------------
# ------------------------------------------------------------------------------

# Histogram: Intent (CSC vs. INN)

ggplot(data, aes(x=Intent)) + 
  geom_bar() + 
  geom_text(stat="count", aes(label=..count..), vjust=-1) +
  labs(x="", y="Number of Participants\n") +
  scale_x_continuous(limits=c(0,6), breaks=c(1,2,3,4,5)) +
  scale_y_continuous(limits=c(0,15), breaks=seq(0, 15, 1)) +
  facet_grid(.~factor(Location, labels=c("CSC", "INN"))) +
  theme_bw() +
  theme(axis.line.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank())

# Histogram: Learning (CSC vs. INN)

ggplot(data, aes(x=Learning)) + 
  geom_bar() + 
  geom_text(stat="count", aes(label=..count..), vjust=-1) +
  labs(x="", y="Number of Participants\n") +
  scale_x_continuous(limits=c(0,6), breaks=c(1,2,3,4,5)) +
  scale_y_continuous(limits=c(0,15), breaks=seq(0, 15, 1)) +
  facet_grid(.~factor(Location, labels=c("CSC", "INN"))) +
  theme_bw() +
  theme(axis.line.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank())

# Histogram: Application (CSC vs. INN)

ggplot(data, aes(x=Application)) + 
  geom_bar() + 
  geom_text(stat="count", aes(label=..count..), vjust=-1) +
  labs(x="", y="Number of Participants\n") +
  scale_x_continuous(limits=c(0,6), breaks=c(1,2,3,4,5)) +
  scale_y_continuous(limits=c(0,15), breaks=seq(0, 15, 1)) +
  facet_grid(.~factor(Location, labels=c("CSC", "INN"))) +
  theme_bw() +
  theme(axis.line.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank())

# Histogram: Trainer (CSC vs. INN)

ggplot(data, aes(x=Trainer)) + 
  geom_bar() + 
  geom_text(stat="count", aes(label=..count..), vjust=-1) +
  labs(x="", y="Number of Participants\n") +
  scale_x_continuous(limits=c(0,6), breaks=c(1,2,3,4,5)) +
  scale_y_continuous(limits=c(0,15), breaks=seq(0, 15, 1)) +
  facet_grid(.~factor(Location, labels=c("CSC", "INN"))) +
  theme_bw() +
  theme(axis.line.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank())

# Histogram: Enhancement (CSC vs. INN)

ggplot(data, aes(x=Enhancement)) + 
  geom_bar() + 
  geom_text(stat="count", aes(label=..count..), vjust=-1) +
  labs(x="", y="Number of Participants\n") +
  scale_x_continuous(limits=c(0,6), breaks=c(1,2,3,4,5)) +
  scale_y_continuous(limits=c(0,15), breaks=seq(0, 15, 1)) +
  facet_grid(.~factor(Location, labels=c("CSC", "INN"))) +
  theme_bw() +
  theme(axis.line.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank())

# Histogram: Environment (CSC vs. INN)

ggplot(data, aes(x=Environment)) + 
  geom_bar() + 
  geom_text(stat="count", aes(label=..count..), vjust=-1) +
  labs(x="", y="Number of Participants\n") +
  scale_x_continuous(limits=c(0,6), breaks=c(1,2,3,4,5)) +
  scale_y_continuous(limits=c(0,15), breaks=seq(0, 15, 1)) +
  facet_grid(.~factor(Location, labels=c("CSC", "INN"))) +
  theme_bw() +
  theme(axis.line.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank())

# Histogram: Administration (CSC vs. INN)

ggplot(data, aes(x=Administration)) + 
  geom_bar() + 
  geom_text(stat="count", aes(label=..count..), vjust=-1) +
  labs(x="", y="Number of Participants\n") +
  scale_x_continuous(limits=c(0,6), breaks=c(1,2,3,4,5)) +
  scale_y_continuous(limits=c(0,15), breaks=seq(0, 15, 1)) +
  facet_grid(.~factor(Location, labels=c("CSC", "INN"))) +
  theme_bw() +
  theme(axis.line.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank())

# Histogram: Satisfaction (CSC vs. INN)

ggplot(data, aes(x=Satisfaction)) + 
  geom_bar() + 
  geom_text(stat="count", aes(label=..count..), vjust=-1) +
  labs(x="", y="Number of Participants\n") +
  scale_x_continuous(limits=c(0,6), breaks=c(1,2,3,4,5)) +
  scale_y_continuous(limits=c(0,15), breaks=seq(0, 15, 1)) +
  facet_grid(.~factor(Location, labels=c("CSC", "INN"))) +
  theme_bw() +
  theme(axis.line.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank())

# Histogram: Perception (CSC vs. INN)

ggplot(data, aes(x=Perception)) + 
  geom_bar() + 
  geom_text(stat="count", aes(label=..count..), vjust=-1) +
  labs(x="", y="Number of Participants\n") +
  scale_x_continuous(limits=c(0,6), breaks=c(1,2,3,4,5)) +
  scale_y_continuous(limits=c(0,15), breaks=seq(0, 15, 1)) +
  facet_grid(.~factor(Location, labels=c("CSC", "INN"))) +
  theme_bw() +
  theme(axis.line.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank())

