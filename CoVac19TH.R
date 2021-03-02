# MODEL COVID-19 VACCINE THAILAND 
# Original COMO MODEL in R by Lisa White, Ricardo Aguas
# Adapted from COMO MODEL BY Nantasit Luangasanatip, Sompob Saralamba, Wirichada Pan-Ngum
#
#######
#######
#START CODE
#####

library("deSolve")
library("dplyr")

#########  INCIDENCE DATA
incdata_X<-read.csv("data/Thcovidcases.csv")
incdata_X[,1]<-as.Date(incdata_X[,1],"%d-%m-%y")

########## POPULATION Structure & POP AGEING
load('data/THpopstruct.RData')
load('data/mort_sever_default.Rda')

source("parameter/pop.R")

########## CONTACT DATA
load('data/THcontacts.RData')

#########   INITIALISE SIMULATION/INTERVENTION START TIMES
source("parameter/date.R")

# DEFINE Vaccine Strategy
######

#Define vaccination strategy based on 21 age group
NoVac<-rep(0,21)    #No vaccine
AgeAll<-rep(1,21)   #Vac all age gr
AgeHighI<-rep(0,21)
AgeHighI[5:8]<-1    #Vac age 20-39
AgeAdult<-rep(1,21)
AgeAdult[1:3]<-0    #Vac age >15
AgeElder<-rep(0,21)  
AgeElder[14:21]<-1  #Vac age >65

#choose Vaccination strategy
AgeVac<-NoVac   #5 Options; NoVac, AgeAll, AgeHighI, AgeAdult, AgeElder
######

#PARAMETERS
#####
source("parameter/parameter.R")

# ######
# the indices
source("parameter/indices.R")

# MODEL INITIAL CONDITIONS
###########################################################################
source("parameter/init.R")

# set progress bar
pb <- txtProgressBar(min = 0, max = length(times), style = 3)

######
# MODEL SOLVING EQUARIONS
#####
source("function/covid.R")

#create new model script with 1yr time frame
source("function/process_ode_outcome_1yr.R")

#function to run with differnt parameter inputs
source("function/VacTPP_all_scens_4.R")

#pop Elderly
sum(AgeAll*popstruc[,2])
sum(AgeAdult*popstruc[,2])
sum(AgeHighI*popstruc[,2])
sum(AgeElder*popstruc[,2])
#% for 1 yr
#Adu  (10M) <- 0.171
#HIGH (10M) <- 0.526
#Eld  (9M)  <- 0.92
#% for 0.5 yr
#Adu  (10M) <- 0.171
#HIGH (10M) <- 0.526
#Eld  (9M)  <- 0.92
#Change file name when save
sum(popstruc[1:4,2])
0.7/15.932143
sum(popstruc[5:8,2])
3.3/18.860035
sum(popstruc[9:13,2])
4.4/25.963304
sum(popstruc[14:21,2])
1.6/9.044496
#######TEST RUN#######
Baseline_w_sd_0_imp10<-VacTPP_all_scens_4(NoVac,0,0,0,0,1,2,0,4.8)
Baseline_w_sd_40_imp10<-VacTPP_all_scens_4(NoVac,0,0,0,0,1,2,0.4,4.8)
#Baseline_w_sd_30_imp10<-VacTPP_all_scens_4(NoVac,0,0,0,0,1,2,0.3,4.8)
Vac_Eff1_10M_AgeHig_70_sd40imp10<-VacTPP_all_scens_4(AgeHighI,0.52163,0.7,0,0,1,2,0.4,4.8)
Vac_Eff3_10M_AgeEld_70_sd40imp10<-VacTPP_all_scens_4(AgeElder,0.92172,0,0,0.7,1,2,0.4,4.8)
Vac_Eff3_10M_AgeHig_70_sd40imp10<-VacTPP_all_scens_4(AgeHighI,0.52163,0,0,0.7,1,2,0.4,4.8)
Vac_Eff1_10M_AgeEld_70_sd40imp10<-VacTPP_all_scens_4(AgeElder,0.92172,0.7,0,0,1,2,0.4,4.8)

Vac_Eff3_10M_AgeEld_99_sd40imp10<-VacTPP_all_scens_4(AgeElder,0.92172,0,0,0.999,1,2,0.4,4.8)

Vac_Eff1_10M_AgeHig_70_sd0imp10<-VacTPP_all_scens_4(AgeHighI,0.52163,0.7,0,0,1,2,0,4.8)

Vac_Eff1_10M_AgeHig_90_sd0imp10<-VacTPP_all_scens_4(AgeHighI,0.51,0.9,0,0,0.5,2,0,4.8)


plot(Baseline_w_sd_0_imp10[["time"]],Baseline_w_sd_0_imp10[["daily_incidence"]],xlab="time",ylab="daily_incidence",type="l", col="1",xlim = as.Date(c("2021-01-01","2023-01-01")))
lines(Baseline_w_sd_40_imp10[["time"]], Baseline_w_sd_40_imp10[["daily_incidence"]], col="2")

plot(Vac_Eff1_10M_AgeHig_70_sd40imp10[["time"]], Vac_Eff1_10M_AgeHig_70_sd40imp10[["daily_incidence"]], col="3",xlab="time",ylab="daily_incidence",type="l",xlim = as.Date(c("2021-01-01","2023-01-01")),ylim = c(0,16000))
lines(Vac_Eff3_10M_AgeEld_70_sd40imp10[["time"]], Vac_Eff3_10M_AgeEld_70_sd40imp10[["daily_incidence"]], col="4")
lines(Vac_Eff3_10M_AgeHig_70_sd40imp10[["time"]], Vac_Eff3_10M_AgeHig_70_sd40imp10[["daily_incidence"]], col="5")
lines(Vac_Eff1_10M_AgeEld_70_sd40imp10[["time"]], Vac_Eff1_10M_AgeEld_70_sd40imp10[["daily_incidence"]], col="6")
lines(Vac_Eff3_10M_AgeEld_99_sd40imp10[["time"]], Vac_Eff3_10M_AgeEld_99_sd40imp10[["daily_incidence"]], col="7")
lines(Vac_Eff1_10M_AgeHig_70_sd0imp10[["time"]], Vac_Eff1_10M_AgeHig_70_sd0imp10[["daily_incidence"]], col="8")
lines(Vac_Eff1_10M_AgeHig_90_sd0imp10[["time"]], Vac_Eff1_10M_AgeHig_90_sd0imp10[["daily_incidence"]], col="9")
library(ggplot2)

data <- data.frame(Baseline_w_sd_0_imp10[["time"]],Baseline_w_sd_0_imp10[["daily_incidence"]])

names(data)[1] <- "time"
names(data)[2] <- "daily_incidence"

ggplot(data, aes(x=time, y=daily_incidence))+ 
  geom_line()




