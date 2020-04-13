rm(list=ls()) 
options(scipen=999)
cat("\014")

library(readxl)
library(ggplot2)
library(Rmisc)
library(diptest)
library(BayesFactor)
library(tidyverse)
library(patchwork)
library(lmerTest)
library(emmeans)
library(stringr)

directory <- dirname(rstudioapi::getSourceEditorContext()$path) #Directory of where script is saved
dataFile <- paste0(directory, "/SimulatedData/SimulatedData.xlsx")
data <- read_excel(dataFile) #Read data from excel file
data$Latency <- as.numeric(data$Latency)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#============================= Means for each condition ==============================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

summary <- summarySEwithin(data, measurevar="Latency", withinvars = c("SCM", "Cond"), idvar="Subject")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#============================ Subsetting data for analysis ===========================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

dataFlex <- data[data$Cond == "BB_Flex", ]
dataPinch <- data[data$Cond == "FDI_Pinch", ]
dataBB_PinchFlex <- data[data$Cond == "BB_PinchFlex", ]
dataFDI_PinchFlex <- data[data$Cond == "FDI_PinchFlex", ]


######################################################################################################
###############################     SCM TRIAL SUMMARIES     ##########################################
######################################################################################################

# Here we will get average SCM+/- trial latencies which we will use later in analysis
# Also testing the difference between SCM+ and SCM- (paired subjects by SCM)
# This runs for each of the 4 tasks tested; T1:T4

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#============================ SCM trial summary - Task 1  ============================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

dataTask = dataFlex
# Calculate medians for each subject
dataMediansT1 <- ddply(dataTask, ~SCM*Subject, summarise, median = median(Latency), sd = sd(Latency)) 
names(dataMediansT1)[names(dataMediansT1) == "median"] <- "Latency"

# Summary stats for SCM+/- conds
Medians_SCM_PlusT1 <- dataMediansT1[dataMediansT1$SCM =='SCM+',]
Medians_SCM_MinusT1 <- dataMediansT1[dataMediansT1$SCM =='SCM-',]

Mean_SCM_PlusT1 <- mean(Medians_SCM_PlusT1$Latency)
SD_SCM_PlusT1 <- sd(Medians_SCM_PlusT1$Latency)

Mean_SCM_MinusT1 <- mean(Medians_SCM_MinusT1$Latency)
SD_SCM_MinusT1 <- sd(Medians_SCM_MinusT1$Latency)

# Difference between SCM+/-
differenceT1 <- Medians_SCM_PlusT1$Latency- Medians_SCM_MinusT1$Latency
mean(differenceT1)
sd(differenceT1)
t.test(Medians_SCM_PlusT1$Latency, Medians_SCM_MinusT1$Latency, paired = T, conf.level = 0.95)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#============================ SCM trial summary - Task 2  ============================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

dataTask = dataPinch
# Calculate medians for each subject
dataMediansT2 <- ddply(dataTask, ~SCM*Subject, summarise, median = median(Latency), sd = sd(Latency)) 
names(dataMediansT2)[names(dataMediansT2) == "median"] <- "Latency"

# Summary stats for SCM+/- conds
Medians_SCM_PlusT2 <- dataMediansT2[dataMediansT2$SCM =='SCM+',]
Medians_SCM_MinusT2 <- dataMediansT2[dataMediansT2$SCM =='SCM-',]

Mean_SCM_PlusT2 <- mean(Medians_SCM_PlusT2$Latency)
SD_SCM_PlusT2 <- sd(Medians_SCM_PlusT2$Latency)

Mean_SCM_MinusT2 <- mean(Medians_SCM_MinusT2$Latency)
SD_SCM_MinusT2 <- sd(Medians_SCM_MinusT2$Latency)

# Difference between SCM+/-
differenceT2 <- Medians_SCM_PlusT2$Latency- Medians_SCM_MinusT2$Latency
mean(differenceT2)
sd(differenceT2)
t.test(Medians_SCM_PlusT2$Latency, Medians_SCM_MinusT2$Latency, paired = T, conf.level = 0.95)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#============================ SCM trial summary - Task 3  ============================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

dataTask = dataBB_PinchFlex
# Calculate medians for each subject
dataMediansT3 <- ddply(dataTask, ~SCM*Subject, summarise, median = median(Latency), sd = sd(Latency)) 
names(dataMediansT3)[names(dataMediansT3) == "median"] <- "Latency"

# Summary stats for SCM+/- conds
Medians_SCM_PlusT3 <- dataMediansT3[dataMediansT3$SCM =='SCM+',]
Medians_SCM_MinusT3 <- dataMediansT3[dataMediansT3$SCM =='SCM-',]

Mean_SCM_PlusT3 <- mean(Medians_SCM_PlusT3$Latency)
SD_SCM_PlusT3 <- sd(Medians_SCM_PlusT3$Latency)

Mean_SCM_MinusT3 <- mean(Medians_SCM_MinusT3$Latency)
SD_SCM_MinusT3 <- sd(Medians_SCM_MinusT3$Latency)

# Difference between SCM+/-
differenceT3 <- Medians_SCM_PlusT3$Latency- Medians_SCM_MinusT3$Latency
mean(differenceT3)
sd(differenceT3)
t.test(Medians_SCM_PlusT3$Latency, Medians_SCM_MinusT3$Latency, paired = T, conf.level = 0.95)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#============================ SCM trial summary - Task 4  ============================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

dataTask = dataFDI_PinchFlex
# Calculate medians for each subject
dataMediansT4 <- ddply(dataTask, ~SCM*Subject, summarise, median = median(Latency), sd = sd(Latency)) 
names(dataMediansT4)[names(dataMediansT4) == "median"] <- "Latency"

# Summary stats for SCM+/- conds
Medians_SCM_PlusT4 <- dataMediansT4[dataMediansT4$SCM =='SCM+',]
Medians_SCM_MinusT4 <- dataMediansT4[dataMediansT4$SCM =='SCM-',]

Mean_SCM_PlusT4 <- mean(Medians_SCM_PlusT4$Latency)
SD_SCM_PlusT4 <- sd(Medians_SCM_PlusT4$Latency)

Mean_SCM_MinusT4 <- mean(Medians_SCM_MinusT4$Latency)
SD_SCM_MinusT4 <- sd(Medians_SCM_MinusT4$Latency)

# Difference between SCM+/-
differenceT4 <- Medians_SCM_PlusT4$Latency- Medians_SCM_MinusT4$Latency
mean(differenceT4)
sd(differenceT4)
t.test(Medians_SCM_PlusT4$Latency, Medians_SCM_MinusT4$Latency, paired = T, conf.level = 0.95)



######################################################################################################
#############################    RUNNING CDF FOR EACH TASK     #######################################
######################################################################################################

# Get 10 quantile values for each subject and place into data frame
# Average across participants to get mean latency at each quantile
# Find the percentiles that closest match average SCM+ and SCM- trial latencies

Ps <- levels(as.factor(dataTask$Subject)) #List of subject IDs
quantileRows<- c("5","15", "25", "35", "45", "55", "65", "75", "85", "95", "Subject") #Quantile col names

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#============================ Run CDF for task 1 =====================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

dataTask = dataFlex
storeQuantileT1 <-data.frame(matrix(0, nrow = NROW(Ps), ncol = 10)) #Empty structure to fill quantile values
storeQuantileT1$Subject <- Ps

for (i in 1:NROW(Ps)) {
  
  dataP <- dataTask[dataTask$Subject == Ps[i],] #Get trials of current participant
  valuesQ <- quantile(dataP$Latency,  probs = c(5,15, 25, 35, 45, 55, 65, 75, 85, 95)/100) #Quantile values for participant
  
  #Add new values to structure on each loop
  storeQuantileT1[i,1] <- valuesQ[[1]]
  storeQuantileT1[i,2] <- valuesQ[[2]]
  storeQuantileT1[i,3] <- valuesQ[[3]]
  storeQuantileT1[i,4] <- valuesQ[[4]]
  storeQuantileT1[i,5] <- valuesQ[[5]]
  storeQuantileT1[i,6] <- valuesQ[[6]]
  storeQuantileT1[i,7] <- valuesQ[[7]]
  storeQuantileT1[i,8] <- valuesQ[[8]]
  storeQuantileT1[i,9] <- valuesQ[[9]]
  storeQuantileT1[i,10] <- valuesQ[[10]]
  
}
colnames(storeQuantileT1) <- quantileRows

# vector of means at each quantile across participants
quantiMeansT1 <- rbind(mean(storeQuantileT1$`5`), 
                     mean(storeQuantileT1$`15`), 
                     mean(storeQuantileT1$`25`),
                     mean(storeQuantileT1$`35`),
                     mean(storeQuantileT1$`45`),
                     mean(storeQuantileT1$`55`),
                     mean(storeQuantileT1$`65`),
                     mean(storeQuantileT1$`75`), 
                     mean(storeQuantileT1$`85`), 
                     mean(storeQuantileT1$`95`))
MeansQuantT1 <-cbind(quantileRows[1:10], quantiMeansT1) #Bind quantiles with values
colnames(MeansQuantT1) <- c("Quantile", "Latency")
MeansQuantT1 <- as.data.frame(MeansQuantT1)

# Find percentile closest to mean of SCM+ trials; SCM+ percentile
match_rowPlusT1 <- which(abs(quantiMeansT1 - Mean_SCM_PlusT1)==min(abs(quantiMeansT1 - Mean_SCM_PlusT1)))
Plus_PercentileT1 <- MeansQuantT1[match_rowPlusT1, 1]
# Find percentile closest to mean of SCM- trials; SCM- percentile
match_rowMinusT1 <- which(abs(quantiMeansT1 - Mean_SCM_MinusT1)==min(abs(quantiMeansT1 - Mean_SCM_MinusT1)))
Minus_PercentileT1 <- MeansQuantT1[match_rowMinusT1, 1]

# Difference between SCM+ and SCM- percentiles
t.test(storeQuantileT1[,match_rowPlusT1], storeQuantileT1[,match_rowMinusT1], paired = TRUE, conf.level = 0.95)
difference <- (storeQuantileT1[,match_rowPlusT1]) - (storeQuantileT1[,match_rowMinusT1]) #SCM+ percentile - SCM- percentile for each subject
mean(difference) #average
sd(difference)

# Mean, sd of SCM+ percentile (across participants)
mean(storeQuantileT1[,match_rowPlusT1]) 
sd(storeQuantileT1[,match_rowPlusT1]) 
# Mean, sd of SCM- percentile
mean(storeQuantileT1[,match_rowMinusT1]) 
sd(storeQuantileT1[,match_rowMinusT1]) 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#============================ Run CDF for task 2 =====================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

dataTask = dataPinch
storeQuantileT2 <-data.frame(matrix(0, nrow = NROW(Ps), ncol = 10)) #Empty structure to fill quantile values
storeQuantileT2$Subject <- Ps

for (i in 1:NROW(Ps)) {
  
  dataP <- dataTask[dataTask$Subject == Ps[i],] #Get trials of current participant
  valuesQ <- quantile(dataP$Latency,  probs = c(5,15, 25, 35, 45, 55, 65, 75, 85, 95)/100) #Quantile values for participant
  
  #Add new values to structure on each loop
  storeQuantileT2[i,1] <- valuesQ[[1]]
  storeQuantileT2[i,2] <- valuesQ[[2]]
  storeQuantileT2[i,3] <- valuesQ[[3]]
  storeQuantileT2[i,4] <- valuesQ[[4]]
  storeQuantileT2[i,5] <- valuesQ[[5]]
  storeQuantileT2[i,6] <- valuesQ[[6]]
  storeQuantileT2[i,7] <- valuesQ[[7]]
  storeQuantileT2[i,8] <- valuesQ[[8]]
  storeQuantileT2[i,9] <- valuesQ[[9]]
  storeQuantileT2[i,10] <- valuesQ[[10]]
  
}
colnames(storeQuantileT2) <- quantileRows

# vector of means at each quantile across participants
quantiMeansT2 <- rbind(mean(storeQuantileT2$`5`), 
                       mean(storeQuantileT2$`15`), 
                       mean(storeQuantileT2$`25`),
                       mean(storeQuantileT2$`35`),
                       mean(storeQuantileT2$`45`),
                       mean(storeQuantileT2$`55`),
                       mean(storeQuantileT2$`65`),
                       mean(storeQuantileT2$`75`), 
                       mean(storeQuantileT2$`85`), 
                       mean(storeQuantileT2$`95`))
MeansQuantT2 <-cbind(quantileRows[1:10], quantiMeansT2) #Bind quantiles with values
colnames(MeansQuantT2) <- c("Quantile", "Latency")
MeansQuantT2 <- as.data.frame(MeansQuantT2)

# Find percentile closest to mean of SCM+ trials; SCM+ percentile
match_rowPlusT2 <- which(abs(quantiMeansT2 - Mean_SCM_PlusT2)==min(abs(quantiMeansT2 - Mean_SCM_PlusT2)))
Plus_PercentileT2 <- MeansQuantT2[match_rowPlusT2, 1]
# Find percentile closest to mean of SCM- trials; SCM- percentile
match_rowMinusT2 <- which(abs(quantiMeansT2 - Mean_SCM_MinusT2)==min(abs(quantiMeansT2 - Mean_SCM_MinusT2)))
Minus_PercentileT2 <- MeansQuantT2[match_rowMinusT2, 1]

# Difference between SCM+ and SCM- percentiles
t.test(storeQuantileT2[,match_rowPlusT2], storeQuantileT2[,match_rowMinusT2], paired = TRUE, conf.level = 0.95)
difference <- (storeQuantileT2[,match_rowPlusT2]) - (storeQuantileT2[,match_rowMinusT2]) #SCM+ percentile - SCM- percentile for each subject
mean(difference) #average
sd(difference)

# Mean, sd of SCM+ percentile (across participants)
mean(storeQuantileT2[,match_rowPlusT2]) 
sd(storeQuantileT2[,match_rowPlusT2]) 
# Mean, sd of SCM- percentile
mean(storeQuantileT2[,match_rowMinusT2]) 
sd(storeQuantileT2[,match_rowMinusT2]) 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#============================ Run CDF for task 3 =====================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

dataTask = dataBB_PinchFlex
storeQuantileT3 <-data.frame(matrix(0, nrow = NROW(Ps), ncol = 10)) #Empty structure to fill quantile values
storeQuantileT3$Subject <- Ps

for (i in 1:NROW(Ps)) {
  
  dataP <- dataTask[dataTask$Subject == Ps[i],] #Get trials of current participant
  valuesQ <- quantile(dataP$Latency,  probs = c(5,15, 25, 35, 45, 55, 65, 75, 85, 95)/100) #Quantile values for participant
  
  #Add new values to structure on each loop
  storeQuantileT3[i,1] <- valuesQ[[1]]
  storeQuantileT3[i,2] <- valuesQ[[2]]
  storeQuantileT3[i,3] <- valuesQ[[3]]
  storeQuantileT3[i,4] <- valuesQ[[4]]
  storeQuantileT3[i,5] <- valuesQ[[5]]
  storeQuantileT3[i,6] <- valuesQ[[6]]
  storeQuantileT3[i,7] <- valuesQ[[7]]
  storeQuantileT3[i,8] <- valuesQ[[8]]
  storeQuantileT3[i,9] <- valuesQ[[9]]
  storeQuantileT3[i,10] <- valuesQ[[10]]
  
}
colnames(storeQuantileT3) <- quantileRows

# vector of means at each quantile across participants
quantiMeansT3 <- rbind(mean(storeQuantileT3$`5`), 
                       mean(storeQuantileT3$`15`), 
                       mean(storeQuantileT3$`25`),
                       mean(storeQuantileT3$`35`),
                       mean(storeQuantileT3$`45`),
                       mean(storeQuantileT3$`55`),
                       mean(storeQuantileT3$`65`),
                       mean(storeQuantileT3$`75`), 
                       mean(storeQuantileT3$`85`), 
                       mean(storeQuantileT3$`95`))
MeansQuantT3 <-cbind(quantileRows[1:10], quantiMeansT3) #Bind quantiles with values
colnames(MeansQuantT3) <- c("Quantile", "Latency")
MeansQuantT3 <- as.data.frame(MeansQuantT3)

# Find percentile closest to mean of SCM+ trials; SCM+ percentile
match_rowPlusT3 <- which(abs(quantiMeansT3 - Mean_SCM_PlusT3)==min(abs(quantiMeansT3 - Mean_SCM_PlusT3)))
Plus_PercentileT3 <- MeansQuantT3[match_rowPlusT3, 1]
# Find percentile closest to mean of SCM- trials; SCM- percentile
match_rowMinusT3 <- which(abs(quantiMeansT3 - Mean_SCM_MinusT3)==min(abs(quantiMeansT3 - Mean_SCM_MinusT3)))
Minus_PercentileT3 <- MeansQuantT3[match_rowMinusT3, 1]

# Difference between SCM+ and SCM- percentiles
t.test(storeQuantileT3[,match_rowPlusT3], storeQuantileT3[,match_rowMinusT3], paired = TRUE, conf.level = 0.95)
difference <- (storeQuantileT3[,match_rowPlusT3]) - (storeQuantileT3[,match_rowMinusT3]) #SCM+ percentile - SCM- percentile for each subject
mean(difference) #average
sd(difference)

# Mean, sd of SCM+ percentile (across participants)
mean(storeQuantileT3[,match_rowPlusT3]) 
sd(storeQuantileT3[,match_rowPlusT3]) 
# Mean, sd of SCM- percentile
mean(storeQuantileT3[,match_rowMinusT3]) 
sd(storeQuantileT3[,match_rowMinusT3]) 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#============================ Run CDF for task 4 =====================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

dataTask = dataFDI_PinchFlex
storeQuantileT4 <-data.frame(matrix(0, nrow = NROW(Ps), ncol = 10)) #Empty structure to fill quantile values
storeQuantileT4$Subject <- Ps

for (i in 1:NROW(Ps)) {
  
  dataP <- dataTask[dataTask$Subject == Ps[i],] #Get trials of current participant
  valuesQ <- quantile(dataP$Latency,  probs = c(5,15, 25, 35, 45, 55, 65, 75, 85, 95)/100) #Quantile values for participant
  
  #Add new values to structure on each loop
  storeQuantileT4[i,1] <- valuesQ[[1]]
  storeQuantileT4[i,2] <- valuesQ[[2]]
  storeQuantileT4[i,3] <- valuesQ[[3]]
  storeQuantileT4[i,4] <- valuesQ[[4]]
  storeQuantileT4[i,5] <- valuesQ[[5]]
  storeQuantileT4[i,6] <- valuesQ[[6]]
  storeQuantileT4[i,7] <- valuesQ[[7]]
  storeQuantileT4[i,8] <- valuesQ[[8]]
  storeQuantileT4[i,9] <- valuesQ[[9]]
  storeQuantileT4[i,10] <- valuesQ[[10]]
  
}
colnames(storeQuantileT4) <- quantileRows

# vector of means at each quantile across participants
quantiMeansT4 <- rbind(mean(storeQuantileT4$`5`), 
                       mean(storeQuantileT4$`15`), 
                       mean(storeQuantileT4$`25`),
                       mean(storeQuantileT4$`35`),
                       mean(storeQuantileT4$`45`),
                       mean(storeQuantileT4$`55`),
                       mean(storeQuantileT4$`65`),
                       mean(storeQuantileT4$`75`), 
                       mean(storeQuantileT4$`85`), 
                       mean(storeQuantileT4$`95`))
MeansQuantT4 <-cbind(quantileRows[1:10], quantiMeansT4) #Bind quantiles with values
colnames(MeansQuantT4) <- c("Quantile", "Latency")
MeansQuantT4 <- as.data.frame(MeansQuantT4)

# Find percentile closest to mean of SCM+ trials; SCM+ percentile
match_rowPlusT4 <- which(abs(quantiMeansT4 - Mean_SCM_PlusT4)==min(abs(quantiMeansT4 - Mean_SCM_PlusT4)))
Plus_PercentileT4 <- MeansQuantT4[match_rowPlusT4, 1]
# Find percentile closest to mean of SCM- trials; SCM- percentile
match_rowMinusT4 <- which(abs(quantiMeansT4 - Mean_SCM_MinusT4)==min(abs(quantiMeansT4 - Mean_SCM_MinusT4)))
Minus_PercentileT4 <- MeansQuantT4[match_rowMinusT4, 1]

# Difference between SCM+ and SCM- percentiles
t.test(storeQuantileT4[,match_rowPlusT4], storeQuantileT4[,match_rowMinusT4], paired = TRUE, conf.level = 0.95)
difference <- (storeQuantileT4[,match_rowPlusT4]) - (storeQuantileT4[,match_rowMinusT4]) #SCM+ percentile - SCM- percentile for each subject
mean(difference) #average
sd(difference)

# Mean, sd of SCM+ percentile (across participants)
mean(storeQuantileT4[,match_rowPlusT4]) 
sd(storeQuantileT4[,match_rowPlusT4]) 
# Mean, sd of SCM- percentile
mean(storeQuantileT4[,match_rowMinusT4]) 
sd(storeQuantileT4[,match_rowMinusT4]) 



######################################################################################################
#############    LOOKING AT PROPORTION OF SCM IN FAST VS SLOW CATEGORIES   ###########################
######################################################################################################

#Group trials into fast (latency <= SCM+ Percentile) and slow (latency >= SCM- Percentile) categories
#Get number of SCM+ trials that occur in each category, calculate %
#Plot histogram of SCM distribution in each category
#Run Bayesian test of association - test dependence of percentile categorisation with SCM activity

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#============================ Distribution of SCM - Task 1 ===========================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# determining min and max of RT so we can automatically set x axis limits in histogram
x_min <- min(rbind(dataFlex$Latency, dataPinch$Latency, dataBB_PinchFlex$Latency, dataFDI_PinchFlex$Latency)) - 10
x_max <- max(rbind(dataFlex$Latency, dataPinch$Latency, dataBB_PinchFlex$Latency, dataFDI_PinchFlex$Latency)) + 10

# Function to round axis limits to nearest 10
Round <- function(x,y) {
  if((y - x %% y) <= x %% y) { x + (y - x %% y)}
  else { x - (x %% y)}
}

#Round to nearest 10
x_min <- Round(x_min, 10)
x_max <- Round(x_max, 10)

#Splitting data into fast and slow categories
dataTask <- dataFlex
dataFastT1 <- dataTask[(dataTask$Latency <= mean(storeQuantileT1[,match_rowPlusT1])),] #Get trials shorter than SCM+ percentile latency
dataFastT1$Cat <- "Fast"
dataSlowT1 <-dataTask[(dataTask$Latency >= mean(storeQuantileT1[,match_rowMinusT1])),] #Get trials longer than SCM- percentile latency
dataSlowT1$Cat <- "Slow"
dataT1 <- rbind(dataFastT1, dataSlowT1) #Hold both groups together

Group <- paste(dataT1$SCM, dataT1$Cat, sep = "_") #Creating group variable so one col can control grouping, legend
dataT1 <- cbind(dataT1, Group)

#Finding percentage of SCM+ trials in each category
percPlusFastT1 <- (nrow(dataFastT1[dataFastT1$SCM == "SCM+",])/nrow(dataFastT1))*100 #% SCM+ in Fast
percPlusSlowT1 <- (nrow(dataSlowT1[dataSlowT1$SCM == "SCM-",])/nrow(dataSlowT1))*100 #% SCM+ in Slow

# Plotting distribution of SCM
plotSCM_T1 <- ggplot(dataT1, aes(x=Latency, fill = Cat)) + 
  geom_histogram(binwidth=5, color = "black")+
  facet_grid(rows = vars(SCM))+
  labs(y="Frequency", x="Pre-motor reaction time (ms)")+
  scale_x_continuous(limits = c(x_min, x_max), breaks = seq(x_min, x_max, by = 30))+
  scale_y_continuous(limits = c(0,25), breaks = seq(0, 25, by = 10))+
  theme_bw()+
  scale_fill_discrete(name = "Categorory", labels = c("Startle", "Non-Startle"))+
  theme(
    legend.position = c(.7, .2),
    axis.title.x = element_blank(),
    axis.title.y =element_text(family="Arial", size=12)
  )
plotSCM_T1

############################ Bayesian test of association ############################################

crosstab <- xtabs(~ SCM + Cat, dataT1 ) #cross tabulate data
contingencyTableBF(crosstab, sampleType = "jointMulti" ) #Bayesian test of association 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#============================ Distribution of SCM - Task 2 ===========================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Splitting data into fast and slow categories
dataTask <- dataPinch
dataFastT2 <- dataTask[(dataTask$Latency <= mean(storeQuantileT2[,match_rowPlusT2])),] #Get trials shorter than SCM+ percentile latency
dataFastT2$Cat <- "Fast"
dataSlowT2 <-dataTask[(dataTask$Latency >= mean(storeQuantileT2[,match_rowMinusT2])),] #Get trials longer than SCM- percentile latency
dataSlowT2$Cat <- "Slow"
dataT2 <- rbind(dataFastT2, dataSlowT2) #Hold both groups together

Group <- paste(dataT2$SCM, dataT2$Cat, sep = "_") #Creating group variable so one col can control grouping, legend
dataT2 <- cbind(dataT2, Group)

percPlusFastT2 <- (nrow(dataFastT2[dataFastT2$SCM == "SCM+",])/nrow(dataFastT2))*100 #% SCM+ in Fast
percPlusSlowT2 <- (nrow(dataSlowT2[dataSlowT2$SCM == "SCM-",])/nrow(dataSlowT2))*100 #% SCM+ in Slow

# Plotting distribution of SCM
plotSCM_T2 <- ggplot(dataT2, aes(x=Latency, fill = Cat)) + 
  geom_histogram(binwidth=5, color = "black")+
  facet_grid(rows = vars(SCM))+
  labs(y="Frequency", x="Pre-motor reaction time (ms)")+
  scale_x_continuous(limits = c(x_min, x_max), breaks = seq(x_min, x_max, by = 30))+
  scale_y_continuous(limits = c(0,25), breaks = seq(0, 25, by = 10))+
  theme_bw()+
  scale_fill_discrete(name = "Categorory", labels = c("Startle", "Non-Startle"))+
  theme(
    legend.position = "none",
    axis.title.x =element_blank(),
    axis.title.y = element_blank()
  )
plotSCM_T2

############################ Bayesian test of association ############################################

crosstab <- xtabs(~ SCM + Cat, dataT2 ) #cross tabulate data
contingencyTableBF(crosstab, sampleType = "jointMulti" ) #Bayesian test of association 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#============================ Distribution of SCM - Task 3 ===========================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Splitting data into fast and slow categories
dataTask <- dataBB_PinchFlex
dataFastT3 <- dataTask[(dataTask$Latency <= mean(storeQuantileT3[,match_rowPlusT3])),] #Get trials shorter than SCM+ percentile latency
dataFastT3$Cat <- "Fast"
dataSlowT3 <-dataTask[(dataTask$Latency >= mean(storeQuantileT3[,match_rowMinusT3])),] #Get trials longer than SCM- percentile latency
dataSlowT3$Cat <- "Slow"
dataT3 <- rbind(dataFastT3, dataSlowT3) #Hold both groups together

Group <- paste(dataT3$SCM, dataT3$Cat, sep = "_") #Creating group variable so one col can control grouping, legend
dataT3 <- cbind(dataT3, Group)

percPlusFastT3 <- (nrow(dataFastT3[dataFastT3$SCM == "SCM+",])/nrow(dataFastT3))*100 #% SCM+ in Fast
percPlusSlowT3 <- (nrow(dataSlowT3[dataSlowT3$SCM == "SCM-",])/nrow(dataSlowT3))*100 #% SCM+ in Slow

# Plotting distribution of SCM
plotSCM_T3 <- ggplot(dataT3, aes(x=Latency, fill = Cat)) + 
  geom_histogram(binwidth=5, color = "black")+
  facet_grid(rows = vars(SCM))+
  labs(y="Frequency", x="Pre-motor reaction time (ms)")+
  scale_x_continuous(limits = c(x_min, x_max), breaks = seq(x_min, x_max, by = 30))+
  scale_y_continuous(limits = c(0,25), breaks = seq(0, 25, by = 10))+
  theme_bw()+
  scale_fill_discrete(name = "Categorory", labels = c("Startle", "Non-Startle"))+
  theme(
    legend.position = "none",
    axis.title.x =element_text(family="Arial", size=12),
    axis.title.y =element_text(family="Arial", size=12)
  )
plotSCM_T3

############################ Bayesian test of association ############################################

crosstab <- xtabs(~ SCM + Cat, dataT3 ) #cross tabulate data
contingencyTableBF(crosstab, sampleType = "jointMulti" ) #Bayesian test of association 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#============================ Distribution of SCM - Task 4 ===========================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Splitting data into fast and slow categories
dataTask <- dataFDI_PinchFlex
dataFastT4 <- dataTask[(dataTask$Latency <= mean(storeQuantileT4[,match_rowPlusT4])),] #Get trials shorter than SCM+ percentile latency
dataFastT4$Cat <- "Fast"
dataSlowT4 <-dataTask[(dataTask$Latency >= mean(storeQuantileT4[,match_rowMinusT4])),] #Get trials longer than SCM- percentile latency
dataSlowT4$Cat <- "Slow"
dataT4 <- rbind(dataFastT4, dataSlowT4) #Hold both groups together

Group <- paste(dataT4$SCM, dataT4$Cat, sep = "_") #Creating group variable so one col can control grouping, legend
dataT4 <- cbind(dataT4, Group)

percPlusFastT4 <- (nrow(dataFastT4[dataFastT4$SCM == "SCM+",])/nrow(dataFastT4))*100 #% SCM+ in Fast
percPlusSlowT4 <- (nrow(dataSlowT4[dataSlowT4$SCM == "SCM-",])/nrow(dataSlowT4))*100 #% SCM+ in Slow

# Plotting distribution of SCM
plotSCM_T4 <- ggplot(dataT4, aes(x=Latency, fill = Cat)) + 
  geom_histogram(binwidth=5, color = "black")+
  facet_grid(rows = vars(SCM))+
  labs(y="Frequency", x="Pre-motor reaction time (ms)")+
  scale_x_continuous(limits = c(x_min, x_max), breaks = seq(x_min, x_max, by = 30))+
  scale_y_continuous(limits = c(0,25), breaks = seq(0, 25, by = 10))+
  theme_bw()+
  scale_fill_discrete(name = "Categorory", labels = c("Startle", "Non-Startle"))+
  theme(
    legend.position = "none",
    axis.title.x = element_text(family="Arial", size=12),
    axis.title.y = element_blank()
  )
plotSCM_T4

############################ Bayesian test of association ############################################

crosstab <- xtabs(~ SCM + Cat, dataT4 ) #cross tabulate data
contingencyTableBF(crosstab, sampleType = "jointMulti" ) #Bayesian test of association 

########################### Putting plots together ###################################################

plotSCM <- plotSCM_T1 + plotSCM_T2 + plotSCM_T3 + plotSCM_T4 +
  plot_annotation(tag_levels = "A") & #Labels for plots
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    strip.background = element_rect(colour="black", fill="white"),
    axis.text.x = element_text(family="Arial", size=11), 
    axis.text.y = element_text(family="Arial", size=11),
    legend.text=element_text(family = "Arial", size=8),
    legend.title=element_blank()
  )
plotSCM



######################################################################################################
#############################    PLOTTING CDF FOR EACH TASK   ########################################
######################################################################################################

# Plot CDF results for each task, automatically annotating SCM+/- labels and y-axis limits
# Bind all plots together in one neat figure at the end
# Note the axis labels/legend differs for each task. Legend should be placed manually by theme(legend.position = c(x,y))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#============================ Plotting CDF - Task 1 ==================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

pd <- position_dodge(0) #dodge plot overlaps
quantile_means = rbind(MeansQuantT1, MeansQuantT2, MeansQuantT3, MeansQuantT4) #get mean quantile values for all tasks
y_axis_min <- min(as.numeric(as.vector(quantile_means[['Latency']]))) - 10 #set boundaries for y axis based on min and max values
y_axis_max <- max(as.numeric(as.vector(quantile_means[['Latency']]))) + 10 #adjust +-10 if you are cutting out se bars

# Round y axis to nearest 10
y_axis_min <- Round(y_axis_min, 10) #(round x to nearest 10)
y_axis_max <- Round(y_axis_max, 10) 

# Gather mean quantile values for each participant 
storeQuantileLongT1 <- gather(storeQuantileT1, quant, Latency,  '5':'95', factor_key=TRUE) 
# Means across participants for each quantile
summaryQuantilesT1 <- summarySEwithin(storeQuantileLongT1, measurevar="Latency", withinvars = c("quant"), idvar="Subject")
summaryQuantilesT1

# Automattically position text
text_diffT1 <- round(Mean_SCM_PlusT1 - Mean_SCM_MinusT1, digits = 1)
text_pos_plusT1 <- (Mean_SCM_PlusT1 -5)
text_pos_minusT1 <- (Mean_SCM_MinusT1 - 5)
text_diff_posT1 <- Mean_SCM_PlusT1 + (-text_diffT1/2)


plotT1 <- ggplot(summaryQuantilesT1, aes(x=quant, y=Latency))+
  geom_errorbar(aes(ymin=Latency-se, ymax=Latency+se),width=0) +
  geom_segment(aes(x = 0.5, y = Mean_SCM_PlusT1, xend = 11, yend = Mean_SCM_PlusT1), linetype="dashed", color="#F8766D", size = 1)+
  geom_segment(aes(x = 0.5, y = Mean_SCM_MinusT1, xend = 11, yend = Mean_SCM_MinusT1), linetype="dashed", color="#00BFC4", size = 1)+
  geom_point(stat="identity", size = 2, position= pd)+
  labs(y="Pre-motor reaction time (ms)", x="Percentile", fill="")+
  theme(legend.position="bottom")+
  scale_y_continuous(limits=c(y_axis_min, y_axis_max), breaks=seq(y_axis_min,y_axis_max,by=20))+
  geom_segment(aes(x = 8.5, y = Mean_SCM_PlusT1, xend = 8.5, yend = Mean_SCM_MinusT1), linetype="solid", color="black", size = 1, arrow = arrow(type = "open", angle = 30, length = unit(0.15, "inches")))+
  geom_segment(aes(x = 8.5, y = Mean_SCM_MinusT1, xend = 8.5, yend = Mean_SCM_PlusT1), linetype="solid", color="black", size = 1, arrow = arrow(type = "open", angle = 30, length = unit(0.15, "inches")))+
  annotate("text", x = 10.3, y = text_pos_plusT1, label = "SCM+", color="#F8766D")+
  annotate("text", x = 10.3, y = text_pos_minusT1, label = "SCM-", color="#00BFC4") +
  annotate("text", x = 9.2, y = text_diff_posT1, label = text_diffT1, color="black")+
  theme_classic() +
  theme(
    legend.position="none",
    axis.title.x = element_blank(),
    axis.title.y =element_text(family="Arial", size=12)
  )
plotT1

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#============================ Plotting CDF - Task 2 ==================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Gather mean quantile values for each participant 
storeQuantileLongT2 <- gather(storeQuantileT2, quant, Latency,  '5':'95', factor_key=TRUE) 
# Means across participants for each quantile
summaryQuantilesT2 <- summarySEwithin(storeQuantileLongT2, measurevar="Latency", withinvars = c("quant"), idvar="Subject")
summaryQuantilesT2

# Automattically position text
text_diffT2 <- round(Mean_SCM_PlusT2 - Mean_SCM_MinusT2, digits = 1)
text_pos_plusT2 <- (Mean_SCM_PlusT2 -5)
text_pos_minusT2 <- (Mean_SCM_MinusT2 - 5)
text_diff_posT2 <- Mean_SCM_PlusT2 + (-text_diffT2/2)


plotT2 <- ggplot(summaryQuantilesT2, aes(x=quant, y=Latency))+
  geom_errorbar(aes(ymin=Latency-se, ymax=Latency+se),width=0) +
  geom_segment(aes(x = 0.5, y = Mean_SCM_PlusT2, xend = 11, yend = Mean_SCM_PlusT2), linetype="dashed", color="#F8766D", size = 1)+
  geom_segment(aes(x = 0.5, y = Mean_SCM_MinusT2, xend = 11, yend = Mean_SCM_MinusT2), linetype="dashed", color="#00BFC4", size = 1)+
  geom_point(stat="identity", size = 2, position= pd)+
  labs(y="Pre-motor reaction time (ms)", x="Percentile", fill="")+
  scale_y_continuous(limits=c(y_axis_min, y_axis_max), breaks=seq(y_axis_min,y_axis_max,by=20))+
  geom_segment(aes(x = 8.5, y = Mean_SCM_PlusT2, xend = 8.5, yend = Mean_SCM_MinusT2), linetype="solid", color="black", size = 1, arrow = arrow(type = "open", angle = 30, length = unit(0.15, "inches")))+
  geom_segment(aes(x = 8.5, y = Mean_SCM_MinusT2, xend = 8.5, yend = Mean_SCM_PlusT2), linetype="solid", color="black", size = 1, arrow = arrow(type = "open", angle = 30, length = unit(0.15, "inches")))+
  annotate("text", x = 10.3, y = text_pos_plusT2, label = "SCM+", color="#F8766D")+
  annotate("text", x = 10.3, y = text_pos_minusT2, label = "SCM-", color="#00BFC4") +
  annotate("text", x = 9.2, y = text_diff_posT2, label = text_diffT2, color="black")+
  theme_classic() +
  theme(
    legend.position=c(.15,.8),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )
plotT2

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#============================ Plotting CDF - Task 3 ==================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Gather mean quantile values for each participant 
storeQuantileLongT3 <- gather(storeQuantileT3, quant, Latency,  '5':'95', factor_key=TRUE) 
# Means across participants for each quantile
summaryQuantilesT3 <- summarySEwithin(storeQuantileLongT3, measurevar="Latency", withinvars = c("quant"), idvar="Subject")
summaryQuantilesT3

# Automattically position text
text_diffT3 <- round(Mean_SCM_PlusT3 - Mean_SCM_MinusT3, digits = 1)
text_pos_plusT3 <- (Mean_SCM_PlusT3 -5)
text_pos_minusT3 <- (Mean_SCM_MinusT3 - 5)
text_diff_posT3 <- Mean_SCM_PlusT3 + (-text_diffT3/2)


plotT3 <- ggplot(summaryQuantilesT3, aes(x=quant, y=Latency))+
  geom_errorbar(aes(ymin=Latency-se, ymax=Latency+se),width=0) +
  geom_segment(aes(x = 0.5, y = Mean_SCM_PlusT3, xend = 11, yend = Mean_SCM_PlusT3), linetype="dashed", color="#F8766D", size = 1)+
  geom_segment(aes(x = 0.5, y = Mean_SCM_MinusT3, xend = 11, yend = Mean_SCM_MinusT3), linetype="dashed", color="#00BFC4", size = 1)+
  geom_point(stat="identity", size = 2, position= pd)+
  labs(y="Pre-motor reaction time (ms)", x="Percentile", fill="")+
  theme(legend.position="bottom")+
  scale_y_continuous(limits=c(y_axis_min, y_axis_max), breaks=seq(y_axis_min,y_axis_max,by=20))+
  geom_segment(aes(x = 8.5, y = Mean_SCM_PlusT3, xend = 8.5, yend = Mean_SCM_MinusT3), linetype="solid", color="black", size = 1, arrow = arrow(type = "open", angle = 30, length = unit(0.15, "inches")))+
  geom_segment(aes(x = 8.5, y = Mean_SCM_MinusT3, xend = 8.5, yend = Mean_SCM_PlusT3), linetype="solid", color="black", size = 1, arrow = arrow(type = "open", angle = 30, length = unit(0.15, "inches")))+
  annotate("text", x = 10.3, y = text_pos_plusT3, label = "SCM+", color="#F8766D")+
  annotate("text", x = 10.3, y = text_pos_minusT3, label = "SCM-", color="#00BFC4") +
  annotate("text", x = 9.2, y = text_diff_posT3, label = text_diffT3, color="black")+
  theme_classic() +
  theme(
    legend.position="none",
    axis.title.y =element_text(family="Arial", size=12), 
    axis.title.x =element_text(family="Arial", size=12)
  )
plotT3

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#============================ Plotting CDF - Task 4 ==================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Gather mean quantile values for each participant 
storeQuantileLongT4 <- gather(storeQuantileT4, quant, Latency,  '5':'95', factor_key=TRUE) 
# Means across participants for each quantile
summaryQuantilesT4 <- summarySEwithin(storeQuantileLongT4, measurevar="Latency", withinvars = c("quant"), idvar="Subject")
summaryQuantilesT4

# Automattically position text
text_diffT4 <- round(Mean_SCM_PlusT4 - Mean_SCM_MinusT4, digits = 1)
text_pos_plusT4 <- (Mean_SCM_PlusT4 -5)
text_pos_minusT4 <- (Mean_SCM_MinusT4 - 5)
text_diff_posT4 <- Mean_SCM_PlusT4 + (-text_diffT4/2)


plotT4 <- ggplot(summaryQuantilesT4, aes(x=quant, y=Latency))+
  geom_errorbar(aes(ymin=Latency-se, ymax=Latency+se),width=0) +
  geom_segment(aes(x = 0.5, y = Mean_SCM_PlusT4, xend = 11, yend = Mean_SCM_PlusT4), linetype="dashed", color="#F8766D", size = 1)+
  geom_segment(aes(x = 0.5, y = Mean_SCM_MinusT4, xend = 11, yend = Mean_SCM_MinusT4), linetype="dashed", color="#00BFC4", size = 1)+
  geom_point(stat="identity", size = 2, position= pd)+
  labs(y="Pre-motor reaction time (ms)", x="Percentile", fill="")+
  scale_y_continuous(limits=c(y_axis_min, y_axis_max), breaks=seq(y_axis_min,y_axis_max,by=20))+
  geom_segment(aes(x = 8.5, y = Mean_SCM_PlusT4, xend = 8.5, yend = Mean_SCM_MinusT4), linetype="solid", color="black", size = 1, arrow = arrow(type = "open", angle = 30, length = unit(0.15, "inches")))+
  geom_segment(aes(x = 8.5, y = Mean_SCM_MinusT4, xend = 8.5, yend = Mean_SCM_PlusT4), linetype="solid", color="black", size = 1, arrow = arrow(type = "open", angle = 30, length = unit(0.15, "inches")))+
  annotate("text", x = 10.3, y = text_pos_plusT4, label = "SCM+", color="#F8766D")+
  annotate("text", x = 10.3, y = text_pos_minusT4, label = "SCM-", color="#00BFC4") +
  annotate("text", x = 9.2, y = text_diff_posT4, label = text_diffT4, color="black")+
  theme_classic() +
  theme(
    legend.position="none",
    axis.title.y = element_blank(),
    axis.title.x =element_text(family="Arial", size=12)
  )
plotT4


########################### Putting plots together ################################################

plotCDFs <- plotT1 + plotT2 + plotT3 + plotT4 +
  plot_annotation(tag_levels = "A") & #Labels for plots
  theme(
    axis.text.x = element_text(family="Arial", size=11), 
    axis.text.y = element_text(family="Arial", size=11),
    panel.background = element_rect(fill = "transparent"), # bg of the panel
    plot.background = element_rect(fill = "transparent", color = NA) # bg of the plot
  )
plotCDFs


######################################################################################################
########################    RUNNING LINEAR MIXED-EFECTS MODELS   #####################################
######################################################################################################

# Split data into faster/slower onset trials - <= 45th percentile; >= 55th percentile
# Run lme on all data categorised via CDFs for each task
# Run lme on fast data 
# Run lme on (all/fast) data categorised via SCM for each task, compare methods
# Plot summary of data

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#=============================== Categorising data - Task 1 ==========================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

FastT1 <- c(storeQuantileT1$"5", storeQuantileT1$"15", storeQuantileT1$"25", storeQuantileT1$"35", storeQuantileT1$"45")
FastT1 <- data.frame(FastT1)
colnames(FastT1) <- "Latency"
FastT1$Percentile <- "Fast"
FastT1$Task <- "Single"
FastT1$Muscle <- "BB"
FastT1$Subject <- storeQuantileT1$Subject

SlowT1 <- c(storeQuantileT1$"55", storeQuantileT1$"65", storeQuantileT1$"75", storeQuantileT1$"85", storeQuantileT1$"95")
SlowT1 <- data.frame(SlowT1)
colnames(SlowT1) <- "Latency"
SlowT1$Percentile <- "Slow"
SlowT1$Task <- "Single"
SlowT1$Muscle <- "BB"
SlowT1$Subject <- storeQuantileT1$Subject

T1 = rbind(FastT1, SlowT1)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#=============================== Categorising data - Task 2 ==========================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

FastT2 <- c(storeQuantileT2$"5", storeQuantileT2$"15", storeQuantileT2$"25", storeQuantileT2$"35", storeQuantileT2$"45")
FastT2 <- data.frame(FastT2)
colnames(FastT2) <- "Latency"
FastT2$Percentile <- "Fast"
FastT2$Task <- "Single"
FastT2$Muscle <- "FDI"
FastT2$Subject <- storeQuantileT2$Subject

SlowT2 <- c(storeQuantileT2$"55", storeQuantileT2$"65", storeQuantileT2$"75", storeQuantileT2$"85", storeQuantileT2$"95")
SlowT2 <- data.frame(SlowT2)
colnames(SlowT2) <- "Latency"
SlowT2$Percentile <- "Slow"
SlowT2$Task <- "Single"
SlowT2$Muscle <- "FDI"
SlowT2$Subject <- storeQuantileT2$Subject

T2 = rbind(FastT2, SlowT2)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#=============================== Categorising data - Task 3 ==========================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

FastT3 <- c(storeQuantileT3$"5", storeQuantileT3$"15", storeQuantileT3$"25", storeQuantileT3$"35", storeQuantileT3$"45")
FastT3 <- data.frame(FastT3)
colnames(FastT3) <- "Latency"
FastT3$Percentile <- "Fast"
FastT3$Task <- "Combine"
FastT3$Muscle <- "BB"
FastT3$Subject <- storeQuantileT3$Subject

SlowT3 <- c(storeQuantileT3$"55", storeQuantileT3$"65", storeQuantileT3$"75", storeQuantileT3$"85", storeQuantileT3$"95")
SlowT3 <- data.frame(SlowT3)
colnames(SlowT3) <- "Latency"
SlowT3$Percentile <- "Slow"
SlowT3$Task <- "Combine"
SlowT3$Muscle <- "BB"
SlowT3$Subject <- storeQuantileT3$Subject

T3 = rbind(FastT3, SlowT3)
Muscle1 = rbind(T1, T3) #These tasks use the same muscle - collect all data for this muscle

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#=============================== Categorising data - Task 4 ==========================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

FastT4 <- c(storeQuantileT4$"5", storeQuantileT4$"15", storeQuantileT4$"25", storeQuantileT4$"35", storeQuantileT4$"45")
FastT4 <- data.frame(FastT4)
colnames(FastT4) <- "Latency"
FastT4$Percentile <- "Fast"
FastT4$Task <- "Combine"
FastT4$Muscle <- "FDI"
FastT4$Subject <- storeQuantileT4$Subject

SlowT4 <- c(storeQuantileT4$"55", storeQuantileT4$"65", storeQuantileT4$"75", storeQuantileT4$"85", storeQuantileT4$"95")
SlowT4 <- data.frame(SlowT4)
colnames(SlowT4) <- "Latency"
SlowT4$Percentile <- "Slow"
SlowT4$Task <- "Combine"
SlowT4$Muscle <- "FDI"
SlowT4$Subject <- storeQuantileT4$Subject

T4 = rbind(FastT4, SlowT4)
Muscle2 = rbind(T2, T4)  #These tasks use the same muscle - collect all data for this muscle

#####################################################################################################

#### Combine all categorised data for analysis ####
AllTs <- rbind(T1, T2, T3, T4)
FastAllTs <- rbind(FastT1, FastT2, FastT3, FastT4)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#================================== Run LME - All categorised data ===================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Full_model_lmer = lmer(formula = Latency ~ Percentile*Muscle*Task + (1|Subject), data = AllTs) #lme formula
anova(Full_model_lmer, ddf = "Kenward-Roger") #output
posthoc <- emmeans(Full_model_lmer, list(pairwise ~ Percentile*Task*Muscle), adjust = "Tukey") #adjust post hoc tests as you need

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#================================== Run LME - Fast categorised data ===================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Full_model_lmer = lmer(formula = Latency ~ Muscle*Task + (1|Subject), data = FastAllTs) #lme formula
anova(Full_model_lmer, ddf = "Kenward-Roger") #output
posthoc <- emmeans(Full_model_lmer, list(pairwise ~ Task*Muscle), adjust = "Tukey") #adjust post hoc tests as you need

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#================================== Run LME - SCM categorised data ===================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Analyse using traditional method of categorisation for comparison with CDF method

#Organise SCM data
data <- as.data.frame(data)
splitCond <- str_split_fixed(data$Cond, "_", 2) #Split Cond string to create 2 new cols
data$Muscle <- splitCond[,1]
data$Task <- splitCond[,2]
data$Task <- ifelse(data$Task == "PinchFlex", "Combined", "Single") #Replace PinchFlex string with Combined, else Single
dataM1 <- data[data$Muscle == "BB", ]
dataM2 <- data[data$Muscle == "FDI", ]

Full_model_lmer = lmer(formula = Latency ~ SCM*Muscle*Task + (1|Subject), data = data) #lme formula
anova(Full_model_lmer, ddf = "Kenward-Roger") #output
posthoc <- emmeans(Full_model_lmer, list(pairwise ~ SCM*Task*Muscle), adjust = "Tukey") #adjust post hoc tests as you need

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#================================== Run LME - SCM+ data ==============================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Get SCM+ data for comparison with Fast categorised data
dataPlus <- data[data$SCM == "SCM+", ]

Full_model_lmer = lmer(formula = Latency ~ Muscle*Task + (1|Subject), data = dataPlus) #lme formula
anova(Full_model_lmer, ddf = "Kenward-Roger") #output
posthoc <- emmeans(Full_model_lmer, list(pairwise ~ Task*Muscle), adjust = "Tukey") #adjust post hoc tests as you need

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#================================== Plot all categorised data ========================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Separating by muscle
summaryM1 <- summarySEwithin(Muscle1, measurevar="Latency", withinvars=c("Percentile", "Task"), idvar="Subject")
summaryM2 <- summarySEwithin(Muscle2, measurevar="Latency", withinvars=c("Percentile", "Task"), idvar="Subject")

ymin <- 0
ymax <- Round(max(rbind(summaryM1$Latency, summaryM2$Latency)) + 10, 10)

dodge = position_dodge(width=0.9)
plotM1<- ggplot(data=summaryM1,aes(y=Latency,x=Task,fill=Percentile))+ 
  geom_bar(position = position_dodge(),stat="identity", colour="black") +
  geom_errorbar(aes(ymin=Latency-se,ymax=Latency+se),position=dodge,width=.3)+
  labs(y="Reaction time (ms)", x="Task")+
  theme_classic()+
  scale_fill_manual(labels = c("Fast onset", "Slower onset"), values=c("#C6DBEF", "#2171B5")) +
  scale_y_continuous(limits=c(ymin, ymax), breaks=seq(ymin, ymax, by = 40))+
  theme(legend.position= c(.35, 1))
plotM1

plotM2<- ggplot(data=summaryM2,aes(y=Latency,x=Task,fill=Percentile))+ 
  geom_bar(position = position_dodge(),stat="identity", colour="black") +
  geom_errorbar(aes(ymin=Latency-se,ymax=Latency+se),position=dodge,width=.3)+
  labs(y="Reaction time (ms)", x="Task")+
  theme(legend.position="bottom")+
  theme_classic()+
  scale_fill_manual(labels = c("Fast onset", "Slower onset"), values=c("#C6DBEF", "#2171B5")) +
  scale_y_continuous(limits=c(ymin, ymax), breaks=seq(ymin, ymax, by = 40))+
  theme(legend.position= "none",
        axis.title.y = element_blank()
  )
plotM2

plotCat <- plotM1 + plotM2 +
  plot_annotation(tag_levels = "A") & #Labels for plots
  theme(axis.text.x = element_text(family="Arial", size=11), 
        axis.text.y = element_text(family="Arial", size=11)
  )
plotCat

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#================================== Plot Fast categorised data ========================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

summaryFastTs <- summarySEwithin(FastAllTs, measurevar="Latency", withinvars=c("Task", "Muscle"), idvar="Subject")

ymin <- 0
ymax <- Round(max(summaryFastTs$Latency) + 10, 10)


plotFastTs<- ggplot(data = summaryFastTs, aes(y= Latency, x = Task, fill = Muscle))+ 
  geom_bar(position = position_dodge(),stat="identity", colour="black") +
  geom_errorbar(aes(ymin=Latency-se,ymax=Latency+se),position=dodge,width=.3)+
  labs(y="Reaction time (ms)", x="Task")+
  theme_classic()+
  scale_fill_manual(labels = c("M1", "M2"), values=c("#C6DBEF", "#2171B5")) +
  scale_y_continuous(limits=c(ymin, ymax), breaks=seq(ymin, ymax, by = 40))+
  theme(legend.position= c(.2, .95),
        legend.title = element_blank()
        )
plotFastTs

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#================================== Plot SCM categorised data ========================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

summaryM1SCM <- summarySEwithin(dataM1, measurevar="Latency", withinvars=c("SCM", "Task"), idvar="Subject")
summaryM2SCM <- summarySEwithin(dataM2, measurevar="Latency", withinvars=c("SCM", "Task"), idvar="Subject")

ymin <- 0
ymax <- Round(max(rbind(summaryM1SCM$Latency, summaryM2SCM$Latency)) + 10, 10)

plotM1SCM <- ggplot(data = summaryM1SCM, aes(y= Latency, x = Task, fill = SCM))+ 
  geom_bar(position = position_dodge(),stat="identity", colour="black") +
  geom_errorbar(aes(ymin=Latency-se,ymax=Latency+se),position=dodge,width=.3)+
  labs(y="Reaction time (ms)", x="Task", fill="")+
  theme_classic()+
  scale_fill_manual(labels = c("M1", "M2"), values=c("#C6DBEF", "#2171B5")) +
  scale_y_continuous(limits=c(ymin, ymax), breaks=seq(ymin, ymax, by = 40))+
  theme(legend.position= c(.2, .95))
plotM1SCM

plotM2SCM <- ggplot(data = summaryM2SCM, aes(y= Latency, x = Task, fill = SCM))+ 
  geom_bar(position = position_dodge(),stat="identity", colour="black") +
  geom_errorbar(aes(ymin=Latency-se,ymax=Latency+se),position=dodge,width=.3)+
  labs(y="Reaction time (ms)", x="Task", fill="")+
  theme_classic()+
  scale_fill_manual(labels = c("M1", "M2"), values=c("#C6DBEF", "#2171B5")) +
  scale_y_continuous(limits=c(ymin, ymax), breaks=seq(ymin, ymax, by = 40))+
  theme(legend.position = "none",
        axis.title.y = element_blank())
plotM2SCM

plotSCM <- plotM1SCM + plotM2SCM +
  plot_annotation(tag_levels = "A") & 
  theme(axis.text.x = element_text(family="Arial", size=11), 
        axis.text.y = element_text(family="Arial", size=11)
  )
plotSCM

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#===================================== Plot SCM+ data ================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

summarySCMPlus <- summarySEwithin(dataPlus, measurevar="Latency", withinvars=c("Task", "Muscle"), idvar="Subject")

ymin <- 0
ymax <- Round(max(dataPlus$Latency) + 10, 10)


plotSCMPlus <- ggplot(data = summarySCMPlus, aes(y= Latency, x = Task, fill = Muscle))+ 
  geom_bar(position = position_dodge(),stat="identity", colour="black") +
  geom_errorbar(aes(ymin=Latency-se,ymax=Latency+se),position=dodge,width=.3)+
  labs(y="Reaction time (ms)", x="Task")+
  theme_classic()+
  scale_fill_manual(labels = c("M1", "M2"), values=c("#C6DBEF", "#2171B5")) +
  scale_y_continuous(limits=c(ymin, ymax), breaks=seq(ymin, ymax, by = 40))+
  theme(legend.position= c(.2, .95),
        legend.title = element_blank()
        )
plotSCMPlus
