#########   INITIALISE SIMULATION/INTERVENTION START TIMES
startdate<-as.Date("2021-01-01") 
stopdate<-as.Date("2023-12-31")
day_start <- as.numeric(startdate-startdate)
day_stop <- as.numeric(stopdate-startdate)
times <- seq(day_start, day_stop)
times.date <- startdate+times
tin<-as.numeric(startdate-as.Date("2021-01-01"))/365.25
initP<-sum(popstruc[,2])       # population size 
ageindcase<-20                 # age of index case (years)
aci <- floor((ageindcase/5)+1) # age class of index case


######    THESE ARE JUST VARIABLE DEFINITIONS - PLEASE DO NOT CHANGE   #################################

# date to start cocooning the elderly
date_cocoon_on<-as.Date("2121-12-14")
# date to start screening
date_screen_on<-as.Date("2121-12-21")
# date to start voluntary quarantine
date_quarantine_on<-as.Date("2121-12-19")
# date to start lockdown low 
date_lockdown_low_on<-as.Date("2121-12-20")
# date to start lockdown high 
date_lockdown_high_on<-as.Date("2121-12-20")
# date to start lockdown mid 
date_lockdown_mid_on<-as.Date("2121-03-23")
#Selfisolation and social distancing as lockdown key intervention setting with different timing of implementation correspond to differnt magnitude
date_selfis_on<-as.Date("2121-08-16") 
date_work_on<-as.Date("2121-08-16")
# date to start the school closure
date_school_on<-as.Date("2121-08-16")

#######To adjust background intervention and vaccination timing########
date_hand_on<-as.Date("2021-01-01") #Hand washing as baseline intervnetion
date_dist_on<-as.Date("2021-07-02") #()Along the assigned timeline  as.Date("2021-07-15")
# date to start international travel ban
date_travelban_on<-as.Date("2021-01-01")
#Vaccination start date
date_vaccine_on<-as.Date("2021-04-01")
