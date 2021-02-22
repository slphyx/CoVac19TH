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
incdata_X<-read.csv("Thcovidcases.csv")
incdata_X[,1]<-as.Date(incdata_X[,1],"%d-%m-%y")

########## POPULATION Structure
load('THpopstruct.RData')
load('mort_sever_default.Rda')

popstruc <- THpop %>% 
  select(age_category, pop) %>% 
  rename(agefloor = age_category) %>% 
  as.data.frame()

popbirth <- THpop %>% 
  select(age_category, birth) %>% 
  as.data.frame() # unit should be per person per day

mort <- THpop %>% 
  pull(death) # unit should be per person per day

ihr <- mort_sever_default %>% 
  select(age_category, ihr) %>% 
  as.data.frame()
#to adjust IHR 
#ihr<-ihr*3 #3
#ihr[,2]<-mean(ihr[,2]*3)
ifr <- mort_sever_default %>% 
  select(age_category, ifr) %>% 
  as.data.frame()
#ifr[,2]<-mean(ifr[,2])

########## CONTACT DATA
load('THcontacts.RData')

########## POP AGEING
# per year ageing matrix
A <- length(age_categories)
dd<-seq(1:A)/seq(1:A)
ageing <- t(diff(diag(dd),lag=1)/(5*365.25))
ageing<-cbind(ageing,0*seq(1:A)) # no ageing from last compartment


#########   INITIALISE SIMULATION/INTERVENTION START TIMES
startdate<-as.Date("2021-01-01") 
# stopdate<-Sys.Date() # today
stopdate<-as.Date("2023-12-31")
#stopdate<-as.Date("2025-12-31") #defult ("2020-12-31)
# stopdate<-as.Date("2020-03-18")
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
#date_quarantine_on<-as.Date("2020-03-15")
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
######

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

# DEFINE PARAMETERS
#####
parameters <- c(
  # Transmission instrinsic
  p=0.028,           #0.025 probabilty of infection given a contact 
  rho = 10,          # 15 25 relative infectiousness of incubation phase (%) min 0 max 100 step 0.5 
  omega=2,         # average duration of immunity (years) min 0.5 max 100 step 0.5  (default 200)
  omegav=1,        # average duration of vaccinated immunity (years) min 0.5 max 100 step 0.5
  gamma=3.5,         # average incubation period (days) min 1 max 7 step 0.5 
  nui=4.5,             # average duration of symptomatic infection period (days) min 1 max 7 step 0.5
  report=2.5,          # percentage of all asymptomatic infections that are reported (%) min 0 max 100 step 1
  reportc=10,         # percentage of all symptomatic infections that are reported (%) min 0 max 100 step 1
  reporth=80,        # percentage of all infections requiring hospitalisation that are actually admitted to hospital (%) min 0 max 100 step 1
  beds_available = sum(popstruc[,2])*2.54/1000,#80000, # maximum number of hospital beds - numeric 
  icu_beds_available = sum(popstruc[,2])*6.6/10000,#8000, # maximum number of hospital beds - numeric 
  ventilators_available = 10000, # maximum number of ventilators - numeric
  give = 85 ,        # system capacity stressor  
  pdeath_h = 15,     #15 50 probability of dying when hospitalised 
  pdeath_hc = 15,    #50 50 probability of dying when denied hospitalisation 
  pdeath_icu = 30,   #30 70 probability of dying when admitted to ICU 
  pdeath_icuc = 40,  #75 70probability of dying when admission to ICU denied 
  pdeath_vent = 60,  #70 90 probability of dying when ventilated 
  pdeath_ventc = 70, #90 90 probability of dying when ventilator denied 
  pclin=10,          #70 default=15 probability upon infection of developing clinical symptoms
  prob_icu = 25,     # current=70 default= 25 probability upon hospitalisation of requiring icu admission   
  prob_vent = 40,    # current=80 default= 40 probability upon admission to the UCI of requiring a ventilator
  ihr_scaling = 1,   # scaling factor for infection hospitalisation rate
  nus = 12,          # duration of non-fatal hospitalised infection (days) min 1 max 20 step 0.5
  nusc = 12,         # duration of non-fatal denied hospitalisation infection (days) min 1 max 20 step 0.5
  nu_icu = 20,       # duration of non-fatal icu infection (days) min 1 max 20 step 0.5
  nu_icuc = 20,      # duration of non-fatal denied icu infection (days) min 1 max 20 step 0.5
  nu_vent = 20,      # duration of non-fatal ventilated infection (days) min 1 max 20 step 0.5
  nu_ventc = 20,     # duration of non-fatal denied ventilation infection (days) min 1 max 20 step 0.5
  rhos= 5,           # relative level of contacts from severely ill patients (%) min 0 max 100 step 1
  amp=0,            # relative amplitude of seasonal forcing (%) min 0 max 100 step 1
  phi=12,            # month of peak in seasonal forcing
  
  # INTERVENTIONS
  # vaccination
  vaccine_on= as.numeric(date_vaccine_on-startdate),
  vaccine_eff1=0,   # vaccine efficacy (%)- min 0 max 100 step 1; reduce infection
  vaccine_eff2=0,   # vaccine efficacy (%)- min 0 max 100 step 1; reduce transmission
  vaccine_eff3=0,   # vaccine efficacy (%)- min 0 max 100 step 1; reduce severity
  vaccine_cov=0,    # vaccine coverage (%)- min 0 max 100 step 1
  vac_campaign=8, # Number of weeks it takes to reach maximum coverage - min 1 max 8 step 1
  
  # self isolation
  selfis_on=as.numeric(date_selfis_on-startdate),
  selfis_dur=200,    # duration of self-isolation protocol (weeks) min 1 max 52 step 1
  selfis_cov=70,    # coverage of self isolation (%) min 0 max 100 step 1
  selfis_eff=70,    # adherence to self isolation (%) min 0 max 100 step 1
  # social distancing
  dist_on=as.numeric(date_dist_on-startdate),
  dist_dur=260,      # duration of social distancing protocol (weeks) min 1 max 52 step 1
  dist_cov=100,      # coverage of social distancing (%) min 0 max 100 step 1
  dist_eff=40,     # adherence to social distancing (%) min 0 max 100 step 1
  # hand washing
  hand_on=as.numeric(date_hand_on-startdate),
  hand_dur=500,      #300 duration of increased hand hygiene protocol (weeks) min 1 max 52 step 1
  hand_eff=30,       # efficacy of hand hygiene  (%) min 0 max 100 step 1 (default 5)
  # working at home
  work_on=as.numeric(date_work_on-startdate),
  work_dur=8,      # duration of working from home protocol (weeks) min 1 max 52 step 1
  work_cov=50,      # coverage of working from home (%) min 0 max 100 step 1
  work_eff=80,      # efficacy of working from home (%) min 0 max 100 step 1
  w2h = 10,         # work contacts that get attibuted to home when working from home (%) min 0 max 100 step 1
  # school closures
  school_on=as.numeric(date_school_on-startdate),
  school_dur=12,    # duration of school closure (weeks) min 1 max 52 step 1
  school_eff=90,    # efficacy of school closure (%) min 0 max 100 step 1
  s2h = 20,         # school contacts that get attibuted to home when school closes (%) min 0 max 100 step 1
  # cocooning the elderly
  cocoon_on = as.numeric(date_cocoon_on-startdate), 
  cocoon_dur=16,    # duration of elderly cocoon protocol (weeks) min 1 max 52 step 1
  cocoon_eff=35,    # efficacy of elderly cocoon (%) min 0 max 100 step 1
  cocoon_cov=75,    # coverage of elderly cocoon (%) min 0 max 100 step 1
  age_cocoon=70,    # minimum age for elderly cocoon min 0 max 100 step 5
  # imported cases 
  mean_imports = 4.8,           # user defined - mean number of infectious migrants per day (number) - min 0 max 500 step 1
  travelban_on= as.numeric(date_travelban_on-startdate),
  travelban_dur = 26,         # duration of internation travel restrictions (weeks) - min 1 max 52 step 1
  travelban_eff=100,          # travel restriction efficacy (%) - min 0 max 100 step 1
  import_dur = 500,            # 10 duration that travelban_eff=0% (days)
  travelban_dur2 = 1708,       # duration of the 2nd internation travel ban (days)
  # screening - increases the rate of isolation of infectious people in the model
  screen_on = as.numeric(date_screen_on-startdate), 
  screen_dur = 12,            # duration of intensified screening (week) - min 1 max 52 step 1
  screen_cov = 90,            # sensitivity of screening test min 25 max 100 step 1
  screen_overdispersion = 4,  # overdispersion of cases around index case. If  1 likelihood same as general population min 1 max 5 step 0.2 
  screen_contacts = 4,        # number of contacts screened per index case min 1 max 10 step 1
  # quarantine - This is the bi-product of increasing testing of suspected cases with a certain false positivity rate and voluntary home quarantining of people sharing a house with an infectious case
  quarantine_on = as.numeric(date_quarantine_on-startdate),
  quarantine_cov = 70,        # coverage of quarantine (%)- min 0 max 100 step 1
  quarantine_dur = 24,        # duration of quarantine (weeks) - min 1 max 52 step 1
  quarantine_days = 14,       # days in isolation for average person (days)  - min 5 max 21 step 1
  quarantine_effort = 2,      # days to implement maximum quarantine coverage - min 1 max 5
  quarantine_eff_home = 50,   # increase in the number of contacts at home when quarantined (%) - min 0 max 100 step 52
  quarantine_eff_other = 90,  # reduction in the number of other contacts when quarantined (%) - min 0 max 100 step 52
  # lockdown
  lockdown_low_on=as.numeric(date_lockdown_low_on-startdate),
  lockdown_low_dur = 16,
  lockdown_mid_on=as.numeric(date_lockdown_mid_on-startdate),
  lockdown_mid_dur = 16,
  lockdown_high_on=as.numeric(date_lockdown_high_on-startdate),
  lockdown_high_dur = 16,
  # mean household size
  household_size = 2          # mean household size (number) - min 1 max 10 step 1 
)


######

# Scale parameters to percentages/ rates
####################
parameters["rho"]<-parameters["rho"]/100
parameters["omega"]<-(1/(parameters["omega"]*365))
parameters["omegav"]<-(1/(parameters["omegav"]*365))
parameters["gamma"]<-1/parameters["gamma"]
parameters["nui"]<-1/parameters["nui"]
parameters["report"]<-parameters["report"]/100
parameters["reportc"]<-parameters["reportc"]/100
parameters["reporth"]<-parameters["reporth"]/100
parameters["nus"]<-1/parameters["nus"]
parameters["rhos"]<-parameters["rhos"]/100
parameters["amp"]<-parameters["amp"]/100
parameters["selfis_dur"]<-parameters["selfis_dur"]*7
parameters["selfis_cov"]<-parameters["selfis_cov"]/100
parameters["selfis_eff"]<-parameters["selfis_eff"]/100
parameters["dist_dur"]<-parameters["dist_dur"]*7
parameters["dist_cov"]<-parameters["dist_cov"]/100
parameters["dist_eff"]<-parameters["dist_eff"]/100
parameters["hand_dur"]<-parameters["hand_dur"]*7
parameters["hand_eff"]<-parameters["hand_eff"]/100
parameters["work_dur"]<-parameters["work_dur"]*7
parameters["work_cov"]<-parameters["work_cov"]/100
parameters["work_eff"]<-parameters["work_eff"]/100
parameters["w2h"]<-parameters["w2h"]/100
parameters["school_dur"]<-parameters["school_dur"]*7
parameters["schoolcov"]<-parameters["schoolcov"]/100
parameters["school_eff"]<-parameters["school_eff"]/100
parameters["s2h"]<-parameters["s2h"]/100
parameters["cocoon_dur"]<-parameters["cocoon_dur"]*7
parameters["cocoon_cov"]<-parameters["cocoon_cov"]/100
parameters["cocoon_eff"]<-parameters["cocoon_eff"]/100
parameters["age_cocoon"]<-floor((parameters["age_cocoon"]/5)+1)
parameters["travelban_eff"]<-parameters["travelban_eff"]/100
parameters["vaccine_eff1"]<-parameters["vaccine_eff1"]/100
parameters["vaccine_eff2"]<-parameters["vaccine_eff2"]/100
parameters["vaccine_eff3"]<-parameters["vaccine_eff3"]/100
parameters["vaccine_cov"]<-parameters["vaccine_cov"]/100
parameters["vac_campaign"]<-parameters["vac_campaign"]*7
parameters["travelban_dur"]<-parameters["travelban_dur"]*7
parameters["screen_dur"]<-parameters["screen_dur"]*7
parameters["screen_cov"]<-parameters["screen_cov"]/100
parameters["quarantine_cov"]<-parameters["quarantine_cov"]/100
parameters["quarantine_dur"]<-parameters["quarantine_dur"]*7
parameters["quarantine_days"]<-parameters["quarantine_days"]
parameters["quarantine_effort"]<-1/parameters["quarantine_effort"]
parameters["quarantine_eff_home"]<-parameters["quarantine_eff_home"]/-100
parameters["quarantine_eff_other"]<-parameters["quarantine_eff_other"]/100
parameters["give"]<-parameters["give"]/100
parameters["pdeath_h"]<-parameters["pdeath_h"]/100
parameters["pdeath_hc"]<-parameters["pdeath_hc"]/100
parameters["pdeath_icu"]<-parameters["pdeath_icu"]/100
parameters["pdeath_icuc"]<-parameters["pdeath_icuc"]/100
parameters["pdeath_vent"]<-parameters["pdeath_vent"]/100
parameters["pdeath_ventc"]<-parameters["pdeath_ventc"]/100
parameters["nusc"]<-1/parameters["nusc"]
parameters["nu_icu"]<-1/parameters["nu_icu"]
parameters["nu_icuc"]<-1/parameters["nu_icuc"]
parameters["nu_vent"]<-1/parameters["nu_vent"]
parameters["nu_ventc"]<-1/parameters["nu_ventc"]
parameters["pclin"]<-parameters["pclin"]/100
parameters["prob_icu"]<-parameters["prob_icu"]/100
parameters["prob_vent"]<-parameters["prob_vent"]/100
parameters["lockdown_low_dur"]<-parameters["lockdown_low_dur"]*7
parameters["lockdown_mid_dur"]<-parameters["lockdown_mid_dur"]*7
parameters["lockdown_high_dur"]<-parameters["lockdown_high_dur"]*7
#########

######
# Define the indices for each variable
###########################################################################
Sindex<-1:A
Eindex<-(A+1):(2*A)
Iindex<-(2*A+1):(3*A)
Rindex<-(3*A+1):(4*A)
Xindex<-(4*A+1):(5*A)
Hindex<-(5*A+1):(6*A)
HCindex<-(6*A+1):(7*A)
Cindex<-(7*A+1):(8*A)
CMindex<-(8*A+1):(9*A)

SVindex<-(9*A+1):(10*A)
EVindex<-(10*A+1):(11*A)
IVindex<-(11*A+1):(12*A)
CLVindex<-(12*A+1):(13*A)
HVindex<-(13*A+1):(14*A)
ICUVindex<-(14*A+1):(15*A)
VentVindex<-(15*A+1):(16*A)
RVindex<-(16*A+1):(17*A)
#dSVdt,dEVdt,dIVdt,dCLVdt,dHVdt,dICUVdt,dVentVdt,dRVdt

QSindex<-(17*A+1):(18*A)
QEindex<-(18*A+1):(19*A)
QIindex<-(19*A+1):(20*A)
QRindex<-(20*A+1):(21*A)
CLindex<-(21*A+1):(22*A)
QCindex<-(22*A+1):(23*A)
ICUindex<-(23*A+1):(24*A)
ICUCindex<-(24*A+1):(25*A)
Ventindex<-(25*A+1):(26*A)
VentCindex<-(26*A+1):(27*A)
CMCindex<-(27*A+1):(28*A)
VacPindex<-(28*A+1):(29*A)
######
# MODEL INITIAL CONDITIONS
###########################################################################

initI<-0*popstruc[,2]  # Infected and symptomatic
initE<-0*popstruc[,2]  # Incubating
initE[aci] <- 1
initR<-0.035*popstruc[,2]  # 0.035 Immune (seroprevalence from china 3.2-3.8% of the population already had immune)Xu X, Sun J, Nie S, et al. Seroprevalence of immunoglobulin M and G antibodies against SARS-CoV-2 in China [published online ahead of print, 2020 Jun 5]. Nat Med. 2020;10.1038/s41591-020-0949-6. doi:10.1038/s41591-020-0949-6
initX<-0*popstruc[,2]  # Isolated 
#initV<-0*popstruc[,2]  # Vaccinated 
initSV<-0*popstruc[,2]  # Vaccinated and susceptible
initEV<-0*popstruc[,2]  # Vaccinated cases
initIV<-0*popstruc[,2]  # Vaccinated asymptomatic cases
initCLV<-0*popstruc[,2]  # Vaccinated symptomatic cases
initHV<-0*popstruc[,2]  # Vaccinated hospitalised cases
initICUV<-0*popstruc[,2]  # Vaccinated hospi ICU cases
initVentV<-0*popstruc[,2]  # Vaccinated hosp ICU with ventilator
initRV<-0*popstruc[,2]  # Vaccinated infected and recovered

initQS<-0*popstruc[,2] # quarantined S 
initQE<-0*popstruc[,2] # quarantined E  
initQI<-0*popstruc[,2] # quarantined I  
initQR<-0*popstruc[,2] # quarantined R  
initH<-0*popstruc[,2]  # hospitalised 
initHC<-0*popstruc[,2] # hospital critical 
initC<-0*popstruc[,2]  # Cumulative cases (true)
initCM<-0*popstruc[,2] # Cumulative deaths (true)
initCL<-0*popstruc[,2] # symptomatic cases
initQC<-0*popstruc[,2] # quarantined C 
initICU<-0*popstruc[,2]   # icu
initICUC<-0*popstruc[,2]  # icu critical
initVent<-0*popstruc[,2]  # icu vent
initVentC<-0*popstruc[,2] # icu vent crit
initCMC<-0*popstruc[,2]   # Cumulative deaths (true)
initVacP<-0*popstruc[,2]
initS<-popstruc[,2]-initE-initI-initR-initX-initSV-initEV-initIV-initCLV-initHV-initICUV-initVentV-initRV-initH-initHC-initQS-initQE-initQI-initQR-initCL-initQC-initICU-initICUC-initVent-initVentC  # Susceptible (non-immune)
###############
# set progress bar
pb <- txtProgressBar(min = 0, max = length(times), style = 3)

######
# MODEL SOLVING EQUARIONS
#####
covid<-function(t, Y, parameters) 
{
  
  with(as.list(c(Y, parameters)),
       {
         S <- Y[Sindex]
         E <- Y[Eindex]
         I <- Y[Iindex]
         R <- Y[Rindex]
         X <- Y[Xindex]
         H <- Y[Hindex]
         HC <- Y[HCindex]
         C <- Y[Cindex]
         CM <- Y[CMindex]
         #V <- Y[Vindex]
         SV <- Y[SVindex]
         EV <- Y[EVindex]
         IV <- Y[IVindex]
         CLV <- Y[CLVindex]
         HV <- Y[HVindex]
         ICUV <- Y[ICUVindex]
         VentV <- Y[VentVindex]
         RV <- Y[RVindex]
         QS <- Y[QSindex]
         QE <- Y[QEindex]
         QI <- Y[QIindex]
         QR <- Y[QRindex]
         CL <- Y[CLindex]
         QC <- Y[QCindex]
         ICU <- Y[ICUindex]
         ICUC <- Y[ICUCindex]
         Vent <- Y[Ventindex]
         VentC <- Y[VentCindex]
         CMC <- Y[CMCindex]
         VacP <- Y[VacPindex]
         P <- (S+E+I+R+X+SV+EV+IV+CLV+HV+ICUV+VentV+RV+H+HC+QS+QE+QI+QR+CL+QC+ICU+ICUC+Vent+VentC)
         # print(sum(P))
         
         # health system performance
         f <- c(1,(1+give)/2,(1-give)/2,0)
         KH<-beds_available
         KICU<- icu_beds_available
         Kvent<- ventilators_available
         x.H <- c(0,(1+give)*KH/2,(3-give)*KH/2,2*KH)
         x.ICU <- c(0,(1+give)*KICU/2,(3-give)*KICU/2,2*KICU)
         x.Vent <- c(0,(1+give)*Kvent/2,(3-give)*Kvent/2,2*Kvent)
         fH <- splinefun(x.H, f, method = "hyman")
         fICU <- splinefun(x.ICU, f, method = "hyman")
         fVent<- splinefun(x.Vent, f, method = "hyman")
         critH<-min(1-fH(sum(H)+sum(ICUC))+(1-reporth),1)
         crit<-min(1-fICU(sum(ICU)+sum(Vent)+sum(VentC)),1)
         critV<-min(1-fVent(sum(Vent)),1)
         # print(fH(sum(H)))
         
         # interventions
         isolation<-(t>=selfis_on)*(t<=selfis_on+selfis_dur)
         distancing<-(t>=dist_on)*(t<=(dist_on+dist_dur))
         handwash<-(t>=hand_on)*(t<=(hand_on+hand_dur))
         workhome<-(t>=work_on)*(t<=(work_on+work_dur))
         schoolclose<-(t>=school_on)*(t<=(school_on+school_dur))
         cocoon<-(t>=cocoon_on)*(t<=(cocoon_on+cocoon_dur))*cocoon_cov
         vaccine<-(t>=(vaccine_on))*(t<=vaccine_on+vac_campaign)
         
         travelban<-(t>=travelban_on)*(t<=(travelban_on+travelban_dur))+(t>=travelban_on+travelban_dur+import_dur)*(t<=(travelban_on+travelban_dur+import_dur+travelban_dur2))
         # if(travelban==1){
         #   print(paste("t:",t))
         # }
         
         #travelban<-(t>=travelban_on)*(t<=(travelban_on+travelban_dur))
         screen<-(t>=screen_on)*(t<=(screen_on+screen_dur))
         quarantine<-(t>=quarantine_on)*(t<=(quarantine_on+quarantine_dur))
         lockdown_low<-(t>=lockdown_low_on)*(t<=(lockdown_low_on+lockdown_low_dur))
         lockdown_mid<-(t>=lockdown_mid_on)*(t<=(lockdown_mid_on+lockdown_mid_dur))
         lockdown_high<-(t>=lockdown_high_on)*(t<=(lockdown_high_on+lockdown_high_dur))
         
         screen_eff<-0
         selfis<-0
         school<-1
         dist<-1
         hand<-0
         vaccinate<-0
         trvban_eff<-0
         quarantine_rate<-0
         
         if (lockdown_low || lockdown_mid || lockdown_high){
           if(lockdown_low){
             selfis<-0.5
             dist<-0.25
             school<-0
             trvban_eff<-0
             quarantine_rate<-0
             work<-0
             cocoon<-0.95
             hand<-0.05
             vaccinate<-0
           }
           if(lockdown_mid){
             selfis<-0.5
             dist<-0.35 #default 0.35
             school<-0.85
             trvban_eff<-0
             quarantine_rate<-0.05
             work<-0.5
             cocoon<-0.95
             hand<-0.05 #defalt 0.05
             vaccinate<-0
           }
           if(lockdown_high){
             selfis<-0.95
             dist<-0.95
             school<-0.85
             trvban_eff<-0.95
             quarantine_rate<-0.9
             work<-0.75
             cocoon<-0.95
             hand<-0.075
             vaccinate<-0
           }
         }
         else{
           if (workhome){
             work<-work_cov*work_eff
           }else{work<-1}
           if (isolation){
             selfis<-selfis_cov
             if(screen){
               screen_eff<-min((report*I+reportc*(CL)+H+ICU+Vent+reporth*HC+ICUC+VentC)*screen_contacts*(screen_overdispersion*I/P)*screen_cov/P,1) 
             }
           }
           if (schoolclose){
             school<-school_eff
           }
           if(distancing){
             dist<-dist_cov*dist_eff
           }
           if(handwash){
             hand<-hand_eff
           }
           if(vaccine){
             vac_rate <- (-log(1-vaccine_cov)/vac_campaign)
             vaccinate <- vac_rate
           }
           
           if(travelban){
             trvban_eff<-travelban_eff
           }
           if(quarantine){
             quarantine_rate<-min(((I+CL+H+ICU+Vent+HC+ICUC+VentC)*(household_size-1)/P),1)*quarantine_cov*quarantine_effort
           }
         }
         
         
         # cocooning the elderly
         cocoon_mat<-matrix((1-cocoon_eff),nrow = length(popstruc$pop),ncol = length(popstruc$pop))
         cocoon_mat[1:(age_cocoon-1),1:(age_cocoon-1)]<-1
         
         
         # contact matrices
         cts<-(contact_home+distancing*(1-dist)*contact_other+(1-distancing)*contact_other
               +(1-schoolclose)*contact_school # school on
               +schoolclose*(1-school)*contact_school # school close
               +schoolclose*contact_home*school*s2h # inflating contacts at home when school closes
               +(1-workhome)*contact_work  # normal work
               +workhome*(1-work)*contact_work # people not working from home when homework is active
               +contact_home*workhome*work*w2h # inflating contacts at home when working from home
         )
         
         # Final transmission related parameters
         contacts <- (1-cocoon)*cts+cocoon*cts*cocoon_mat+cocoon*(1+school*(1-school_eff)+work*(1-work_eff))*contact_home*(1-cocoon_mat)
         #contacts<-matrix(5*0.1777628,nrow=21,ncol=21)
         
         seas <- 1+amp*cos(2*3.14*(t-(phi*365.25/12))/365.25)
         importation <- mean_imports*(1-trvban_eff)
         HH<-H+ICU+Vent
         HHC<-HC+ICUC+VentC
         
         
         lam <- (1-hand)*p*seas*(contacts%*%((rho*(E+(1-vaccine_eff2)*EV)+(I+CL+(1-vaccine_eff2)*IV+(1-vaccine_eff2)*CLV+importation)+(1-selfis_eff)*(X+HHC)+rhos*(HH))/P))
         # contacts under home quarantine
         lamq<-(1-hand)*p*seas*((1-quarantine_eff_home)*contact_home%*%(((1-selfis_eff)*(X+HHC))/P))+(1-hand)*p*seas*(1-quarantine_eff_other)*(contact_other%*%((rho*E+(I+CL+importation)+(1-selfis_eff)*(X+HHC)+rhos*(HH))/P))
         
         # birth/death
         b1<-sum(popbirth[,2]*popstruc[,2])
         birth<-0*popbirth[,2]
         birth[1]<-b1
         # ODE system
         dSdt <- -S*lam-S*vaccinate*AgeVac+omega*R+ageing%*%S-mort*S+birth-quarantine_rate*S +(1/quarantine_days)*QS+omegav*SV+omega*RV
         dEdt <- S*lam-gamma*E+ageing%*%E-mort*E-quarantine_rate*E+(1/quarantine_days)*QE 
         dIdt <- gamma*(1-pclin)*(1-screen_eff)*(1-ihr[,2])*E-nui*I+ageing%*%I-mort*I + (1/quarantine_days)*QI - quarantine_rate*I
         dCLdt<- gamma*pclin*(1-selfis)*(1-ihr[,2])*E-nui*CL+ageing%*%CL-mort*CL + (1/quarantine_days)*QC
         dRdt <- nui*I-omega*R+nui*X+nui*CL+ageing%*%R-mort*R + (1/quarantine_days)*QR + nus*(1-pdeath_h*ifr[,2])*H + (1-pdeath_icu*ifr[,2])*nu_icu*ICU + (1-pdeath_icuc*ifr[,2])*nu_icuc*ICUC + (1-pdeath_hc*ifr[,2])*nusc*HC + (1-pdeath_vent*ifr[,2])*nu_vent*Vent+ (1-pdeath_ventc*ifr[,2])*nu_ventc*VentC - vaccinate*R*AgeVac + omegav*RV
         dXdt <- gamma*selfis*pclin*(1-ihr[,2])*E+gamma*(1-pclin)*screen_eff*(1-ihr[,2])*E-nui*X+ageing%*%X-mort*X 
         ############
         #dVdt <- vaccinate*S -(1-vaccine_eff)*lam*V +ageing%*%V - mort*V
         
         #to add terms (vaccine_eff1=%reduction in infection, vaccine_eff2=%reduction in duration of infection, vaccine_eff3=%reduction in risk of sevrerity, hospitalisation in this case, but we have hospi, ICU and ICUVent [need to clarify the effect of vac])
         #SV, EV, IV, CL,HV,ICUV, VentV, RV set initial condition
         #
         #if (vaccine){cf
         #Add vaccine compartment - add (AgeVac*)S*vaccinate to dSdt and dSVdt
         dSVdt <- S*vaccinate*AgeVac - (1-vaccine_eff1)*SV*lam + ageing%*%SV-mort*SV-omegav*SV #Assuming the lam is the same as general population
         
         dEVdt <- (1-vaccine_eff1)*SV*lam - gamma*EV +ageing%*%EV-mort*EV
         dIVdt <- gamma*(1-pclin*(1-vaccine_eff3))*(1-(ihr[,2]*(1-vaccine_eff3)))*EV-nui*IV+ageing%*%IV - mort*IV
         dCLVdt<- gamma*(pclin*(1-vaccine_eff3))*(1-(ihr[,2]*(1-vaccine_eff3)))*EV-nui*CLV+ageing%*%CLV - mort*CLV
         dHVdt <- gamma*ihr[,2]*(1-vaccine_eff3)*(1-prob_icu)*EV-nus*HV+ ageing%*%HV - mort*HV
         dICUVdt <- gamma*ihr[,2]*(1-vaccine_eff3)*prob_icu*(1-prob_vent)*EV-nu_icu*ICUV +ageing%*%ICUV - mort*ICUV
         dVentVdt <- gamma*ihr[,2]*(1-vaccine_eff3)*prob_icu*prob_vent*EV-nu_vent*VentV +ageing%*%VentV - mort*VentV
         dRVdt <- nui*IV + nui*CLV +nus*HV+nu_icu*ICUV+nu_vent*VentV + ageing%*%RV-mort*RV
         + nus*(1-pdeath_h*ifr[,2])*HV + (1-pdeath_icu*ifr[,2])*nu_icu*ICU + (1-pdeath_icuc*ifr[,2])*nu_icuc*ICUC + (1-pdeath_hc*ifr[,2])*nusc*HC + (1-pdeath_vent*ifr[,2])*nu_vent*Vent+ (1-pdeath_ventc*ifr[,2])*nu_ventc*VentC-omega*RV + vaccinate*R*AgeVac - omegav*RV
         #modified to include Cumulative cases and deaths for those with vaccination
         dVacPdt <- S*vaccinate*AgeVac+vaccinate*R*AgeVac 
         ############ 
         dQSdt <- quarantine_rate*S+ ageing%*%QS-mort*QS - (1/quarantine_days)*QS - lamq*QS
         dQEdt <- quarantine_rate*E - gamma*QE + ageing%*%QE-mort*QE - (1/quarantine_days)*QE + lamq*QS 
         dQIdt <- quarantine_rate*I + gamma*(1-ihr[,2])*(1-pclin)*QE-nui*QI+ageing%*%QI-mort*QI - (1/quarantine_days)*QI
         dQCdt <- gamma*(1-ihr[,2])*pclin*QE-nui*QC+ageing%*%QC-mort*QC - (1/quarantine_days)*QC
         dQRdt <- nui*QI+nui*QC+ageing%*%QR-mort*QR - (1/quarantine_days)*QR
         
         dHdt <- gamma*ihr[,2]*(1-prob_icu)*(1-critH)*E + gamma*ihr[,2]*(1-prob_icu)*(1-critH)*QE - nus*H + ageing%*%H-mort*H  # all pdeath have to be lower than
         dHCdt <- gamma*ihr[,2]*(1-prob_icu)*critH*E + gamma*ihr[,2]*(1-prob_icu)*critH*QE - nusc*HC + ageing%*%HC-mort*HC 
         dICUdt <- gamma*ihr[,2]*prob_icu*(1-crit)*(1-prob_vent)*E + gamma*ihr[,2]*prob_icu*(1-crit)*(1-prob_vent)*QE - nu_icu*ICU +ageing%*%ICU - mort*ICU 
         dICUCdt <- gamma*ihr[,2]*prob_icu*crit*(1-prob_vent)*E + gamma*ihr[,2]*prob_icu*crit*(1-prob_vent)*QE - nu_icuc*ICUC +ageing%*%ICUC - mort*ICUC 
         dVentdt <- gamma*ihr[,2]*prob_icu*(1-crit)*(1-critV)*prob_vent*E + gamma*ihr[,2]*prob_icu*(1-crit)*(1-critV)*prob_vent*QE + (1-critV)*VentC*1/2 - nu_vent*Vent +ageing%*%Vent - mort*Vent 
         dVentCdt <- gamma*ihr[,2]*prob_icu*prob_vent*(1-crit)*critV*E +gamma*ihr[,2]*prob_icu*prob_vent*crit*E+
           gamma*ihr[,2]*prob_icu*prob_vent*(1-crit)*critV*QE + gamma*ihr[,2]*prob_icu*prob_vent*crit*QE - 
           (1-critV)*VentC*1/2-nu_ventc*VentC +ageing%*%VentC - mort*VentC
         
         #Add Terms on accumulate case and death from vaccine compartments       
         dCdt <- report*gamma*(1-pclin)*(1-ihr[,2])*(E+QE)+reportc*gamma*pclin*(1-ihr[,2])*(E+QE)+ 
           gamma*ihr[,2]*(1-critH)*(1-prob_icu)*(E+QE)+gamma*ihr[,2]*critH*reporth*(1-prob_icu)*(E+QE)+gamma*ihr[,2]*prob_icu*(E+QE)+
           report*gamma*(1-pclin)*(1-ihr[,2])*(1-vaccine_eff3)*EV+reportc*gamma*(pclin)*(1-ihr[,2]*(1-vaccine_eff3))*EV+
           gamma*ihr[,2]*(1-vaccine_eff3)*(1-prob_icu)*EV+gamma*ihr[,2]*(1-vaccine_eff3)*prob_icu*(1-prob_vent)*EV+gamma*ihr[,2]*(1-vaccine_eff3)*prob_icu*prob_vent*EV
         
         dCMdt<- nus*pdeath_h*ifr[,2]*H + nusc*pdeath_hc*ifr[,2]*HC + nu_icu*pdeath_icu*ifr[,2]*ICU + nu_icuc*pdeath_icuc*ifr[,2]*ICUC +  nu_vent*pdeath_vent*ifr[,2]*Vent + nu_ventc*pdeath_ventc*ifr[,2]*VentC + 
           mort*H + mort*HC + mort*ICU + mort*ICUC + mort*Vent + mort*VentC+
           nus*pdeath_h*ifr[,2]*HV + nu_icu*pdeath_icu*ifr[,2]*ICUV + nu_vent*pdeath_vent*ifr[,2]*VentV + mort*HV + mort*ICUV + mort*VentV
         
         dCMCdt <- nusc*pdeath_hc*ifr[,2]*HC+nu_icuc*pdeath_icuc*ifr[,2]*ICUC + nu_ventc*pdeath_ventc*ifr[,2]*VentC + 
           mort*HC + mort*ICUC + mort*VentC
         
         setTxtProgressBar(pb, t)
         
         # return the rate of change
         list(c(dSdt,dEdt,dIdt,dRdt,dXdt,dHdt,dHCdt,dCdt,dCMdt,dSVdt,dEVdt,dIVdt,dCLVdt,dHVdt,dICUVdt,dVentVdt,dRVdt,dQSdt,dQEdt,dQIdt,dQRdt,dCLdt,dQCdt,dICUdt,dICUCdt,dVentdt,dVentCdt,dCMCdt,dVacPdt),lam=lam,P=P)
       }
  ) 
}

#create new model script with 1yr time frame
process_ode_outcome_1yr <- function(out){
  # total population
  pop1<-out[,(Sindex+1)]+out[,(Eindex+1)]+out[,(Iindex+1)]+out[,(CLindex+1)]+out[,(Rindex+1)]+out[,(Xindex+1)]+
    out[,(SVindex+1)]+out[,(EVindex+1)]+out[,(IVindex+1)]+out[,(CLVindex+1)]+out[,(HVindex+1)]+out[,(ICUVindex+1)]+out[,(VentVindex+1)]+out[,(RVindex+1)]+
    out[,(QSindex+1)]+out[,(QEindex+1)]+out[,(QIindex+1)]+out[,(QCindex+1)]+out[,(QRindex+1)]+
    out[,(Hindex+1)]+out[,(HCindex+1)]+out[,(ICUindex+1)]+out[,(ICUCindex+1)]+out[,(Ventindex+1)]+out[,(VentCindex+1)] 
  tpop1<-rowSums(pop1)
  
  time<-as.Date(out[,1]+startdate)
  # daily incidence (combind age gr)
  inc1 <- parameters["report"]*parameters["gamma"]*(1-parameters["pclin"])*out[,(Eindex+1)]%*%(1-ihr[,2])+
    parameters["reportc"]*parameters["gamma"]*parameters["pclin"]*out[,(Eindex+1)]%*%(1-ihr[,2])+
    parameters["report"]*parameters["gamma"]*(1-parameters["pclin"])*out[,(QEindex+1)]%*%(1-ihr[,2])+
    parameters["reportc"]*parameters["gamma"]*parameters["pclin"]*out[,(QEindex+1)]%*%(1-ihr[,2])+
    parameters["report"]*parameters["gamma"]*(1-parameters["pclin"]*(1-parameters["vaccine_eff3"]))*out[,(EVindex+1)]%*%(1-ihr[,2]*(1-parameters["vaccine_eff3"]))+
    parameters["reportc"]*parameters["gamma"]*parameters["pclin"]*(1-parameters["vaccine_eff3"])*out[,(EVindex+1)]%*%(1-ihr[,2]*(1-parameters["vaccine_eff3"]))
  
  inc1h<- parameters["gamma"]*out[,(Eindex+1)]%*%ihr[,2]*parameters["reporth"]*(1-parameters["prob_icu"])+
    parameters["gamma"]*out[,(QEindex+1)]%*%ihr[,2]*parameters["reporth"]*(1-parameters["prob_icu"])+
    parameters["gamma"]*out[,(Eindex+1)]%*%ihr[,2]*parameters["prob_icu"]+
    parameters["gamma"]*out[,(QEindex+1)]%*%ihr[,2]*parameters["prob_icu"]+
    parameters["gamma"]*out[,(EVindex+1)]%*%ihr[,2]*(1-parameters["vaccine_eff3"])*(1-parameters["prob_icu"])+
    parameters["gamma"]*out[,(EVindex+1)]%*%ihr[,2]*(1-parameters["vaccine_eff3"])*parameters["prob_icu"]
  
  #Overall incidence
  dailyinc1<-rowSums(inc1)+rowSums(inc1h)      # daily incidence
  cuminc1<-colSums(inc1)+colSums(inc1h)        # cumulative incidence
  
  
  # daily incidence (by age gr)
  inc1_ag <- parameters["report"]*parameters["gamma"]*(1-parameters["pclin"])*out[,(Eindex+1)]*(1-ihr[,2])+
    parameters["reportc"]*parameters["gamma"]*parameters["pclin"]*out[,(Eindex+1)]*(1-ihr[,2])+
    parameters["report"]*parameters["gamma"]*(1-parameters["pclin"])*out[,(QEindex+1)]*(1-ihr[,2])+
    parameters["reportc"]*parameters["gamma"]*parameters["pclin"]*out[,(QEindex+1)]*(1-ihr[,2])
  
  inc1h_ag <- parameters["gamma"]*out[,(Eindex+1)]*ihr[,2]*parameters["reporth"]*(1-parameters["prob_icu"])+
    parameters["gamma"]*out[,(QEindex+1)]*ihr[,2]*parameters["reporth"]*(1-parameters["prob_icu"])+
    parameters["gamma"]*out[,(Eindex+1)]*ihr[,2]*parameters["prob_icu"]+
    parameters["gamma"]*out[,(QEindex+1)]*ihr[,2]*parameters["prob_icu"]
  
  inc1v_ag <-  parameters["report"]*parameters["gamma"]*(1-parameters["pclin"]*(1-parameters["vaccine_eff3"]))*out[,(EVindex+1)]*(1-ihr[,2]*(1-parameters["vaccine_eff3"]))+
    parameters["reportc"]*parameters["gamma"]*parameters["pclin"]*(1-parameters["vaccine_eff3"])*out[,(EVindex+1)]*(1-ihr[,2]*(1-parameters["vaccine_eff3"]))
  
  inc1hv_ag <- parameters["gamma"]*out[,(EVindex+1)]*ihr[,2]*(1-parameters["vaccine_eff3"])*(1-parameters["prob_icu"])+
    parameters["gamma"]*out[,(EVindex+1)]*ihr[,2]*(1-parameters["vaccine_eff3"])*parameters["prob_icu"]
  
  
  #Overall incidence
  dailyinc1_gr<-inc1_ag+inc1h_ag+inc1v_ag+inc1hv_ag  
  dailyinc1_ag<-rowSums(inc1_ag)+rowSums(inc1h_ag)+rowSums(inc1v_ag)+rowSums(inc1hv_ag)      # daily incidence
  cuminc1_agegr<-colSums(inc1_ag)+colSums(inc1h_ag)+colSums(inc1v_ag)+colSums(inc1hv_ag)        # cumulative incidence
  cuminc1_sum<-sum(colSums(inc1_ag)+colSums(inc1h_ag)+colSums(inc1v_ag)+colSums(inc1hv_ag)) 
  
  
  #by 21 age group
  inc1Asym1<- parameters["report"]*parameters["gamma"]*(1-parameters["pclin"])*out[,(Eindex+1)]*(1-ihr[,2])+
    parameters["report"]*parameters["gamma"]*(1-parameters["pclin"])*out[,(QEindex+1)]*(1-ihr[,2])+
    parameters["report"]*parameters["gamma"]*(1-parameters["pclin"]*(1-parameters["vaccine_eff3"]))*out[,(EVindex+1)]*(1-(ihr[,2]*(1-parameters["vaccine_eff3"]))) #Asymptomatic
  
  inc1Mild1<- parameters["reportc"]*parameters["gamma"]*parameters["pclin"]*out[,(Eindex+1)]*(1-ihr[,2])+
    parameters["reportc"]*parameters["gamma"]*parameters["pclin"]*out[,(QEindex+1)]*(1-ihr[,2])+
    parameters["reportc"]*parameters["gamma"]*parameters["pclin"]*(1-parameters["vaccine_eff3"])*out[,(EVindex+1)]*(1-(ihr[,2]*(1-parameters["vaccine_eff3"]))) #non-severe (mild)
  
  inc1Host1<- parameters["gamma"]*out[,(Eindex+1)]*ihr[,2]*parameters["reporth"]*(1-parameters["prob_icu"])+
    parameters["gamma"]*out[,(QEindex+1)]*ihr[,2]*parameters["reporth"]*(1-parameters["prob_icu"])+
    parameters["gamma"]*out[,(EVindex+1)]*ihr[,2]*(1-parameters["prob_icu"])*(1-parameters["vaccine_eff3"]) #non-severe (moderate)
  
  inc1ICU1<- parameters["gamma"]*out[,(Eindex+1)]*ihr[,2]*parameters["prob_icu"]*(1-parameters["prob_vent"])+
    parameters["gamma"]*out[,(QEindex+1)]*ihr[,2]*parameters["prob_icu"]*(1-parameters["prob_vent"])+
    parameters["gamma"]*out[,(EVindex+1)]*ihr[,2]*parameters["prob_icu"]*(1-parameters["prob_vent"])*(1-parameters["vaccine_eff3"]) #ICU(wo vent) (severe)
  
  inc1ICUv1<- parameters["gamma"]*out[,(Eindex+1)]*ihr[,2]*parameters["prob_icu"]*parameters["prob_vent"]+
    parameters["gamma"]*out[,(QEindex+1)]*ihr[,2]*parameters["prob_icu"]*parameters["prob_vent"]+
    parameters["gamma"]*out[,(EVindex+1)]*ihr[,2]*parameters["prob_icu"]*parameters["prob_vent"]*(1-parameters["vaccine_eff3"]) #ICU(with vent) (severe)
  
  inc1All0<-inc1_ag +inc1h_ag +inc1v_ag +inc1hv_ag 
  inc1All1<-inc1Asym1 +inc1Mild1 +inc1Host1 +inc1ICU1+ inc1ICUv1
  
  #by age group
  Daily_inc_Asym_ag1   <-inc1Asym1
  Daily_inc_MildMod_ag1<-inc1Mild1+inc1Host1
  Daily_inc_ICU_ag1    <-inc1ICU1
  Daily_inc_Vent_ag1   <-inc1ICUv1
  Daily_inc_all_ag1    <-inc1All1 #Daily_inc_Asym_ag+Daily_inc_MildMod_ag+Daily_inc_ICU_ag+Daily_inc_Vent_ag
  
  #Vacinated population
  Vac_pop<-sum(AgeVac*popstruc[,2])*parameters["vaccine_cov"]
  VacP<-out[,(VacPindex+1)]
  VacPsum<-rowSums(out[,(VacPindex+1)])
  #New mortality
  Death_oth<-out[,631:651]*mort
  #Death_oth<-out.vaceff_0[,631:651]*mort
  ##########################    CALCULATE MORTALITY 
  cinc_mort_H1 <- cumsum(rowSums(parameters["nus"]*parameters["pdeath_h"]*(out[,(Hindex+1)]%*%ifr[,2])))#+ out[,(Hindex+1)]%*%mort))
  + cumsum(rowSums(parameters["nus"]*parameters["pdeath_h"]*(out[,(HVindex+1)]%*%ifr[,2]))) #+ out[,(HVindex+1)]%*%mort))
  cinc_mort_HC1 <- cumsum(rowSums(parameters["nusc"]*parameters["pdeath_hc"]*(out[,(HCindex+1)]%*%ifr[,2]))) #+ out[,(HCindex+1)]%*%mort))
  cinc_mort_ICU1 <- cumsum(rowSums(parameters["nu_icu"]*parameters["pdeath_icu"]*out[,(ICUindex+1)]%*%ifr[,2])) #+ out[,(ICUindex+1)]%*%mort))+
  + cumsum(rowSums(parameters["nu_icu"]*parameters["pdeath_icu"]*out[,(ICUVindex+1)]%*%ifr[,2])) #+ out[,(ICUVindex+1)]%*%mort))
  cinc_mort_ICUC1 <- cumsum(rowSums(parameters["nu_icuc"]*parameters["pdeath_icuc"]*out[,(ICUCindex+1)]%*%ifr[,2])) #+ out[,(ICUCindex+1)]%*%mort
  cinc_mort_Vent1 <- cumsum(rowSums(parameters["nu_vent"]*parameters["pdeath_vent"]*out[,(Ventindex+1)]%*%ifr[,2])) #+ out[,(Ventindex+1)]%*%mort
  + cumsum(rowSums(parameters["nu_vent"]*parameters["pdeath_vent"]*out[,(VentVindex+1)]%*%ifr[,2])) # + out[,(VentVindex+1)]%*%mort
  cinc_mort_VentC1 <- cumsum(rowSums(parameters["nu_ventc"]*parameters["pdeath_ventc"]*out[,(VentCindex+1)]%*%ifr[,2] )) #+ out[,(VentCindex+1)]%*%mort
  
  cinc_mort_Iv1 <- 0 #cumsum(rowSums(out[,(IVindex+1)]%*%mort)) #only genpop death
  cinc_mort_Clv1 <-  0 #cumsum(rowSums(out[,(CLVindex+1)]%*%mort)) #only genpop death
  cinc_mort_Hv1 <- cumsum(rowSums(parameters["nus"]*parameters["pdeath_h"]*(out[,(HVindex+1)]%*%ifr[,2]))) #+ out[,(HVindex+1)]%*%mort
  cinc_mort_ICUv1 <- cumsum(rowSums(parameters["nu_icu"]*parameters["pdeath_icu"]*out[,(ICUVindex+1)]%*%ifr[,2])) #+ out[,(ICUVindex+1)]%*%mort
  cinc_mort_Ventv1 <-cumsum(rowSums(parameters["nu_vent"]*parameters["pdeath_vent"]*out[,(VentVindex+1)]%*%ifr[,2])) # + out[,(VentVindex+1)]%*%mort
  
  cinc_mort_all <-  cinc_mort_H1+cinc_mort_HC1+cinc_mort_ICU1+cinc_mort_ICUC1+cinc_mort_Vent1+cinc_mort_VentC1+cinc_mort_Hv1+cinc_mort_ICUv1+cinc_mort_Ventv1
  
  dailydeath_H1 <-diff(cinc_mort_H1)
  dailydeath_HC1<-diff(cinc_mort_HC1)
  dailydeath_ICU1<-diff(cinc_mort_ICU1)
  dailydeath_ICUC1<-diff(cinc_mort_ICUC1)
  dailydeath_Vent1<-diff(cinc_mort_Vent1)
  dailydeath_VentC1<-diff(cinc_mort_VentC1)
  
  dailydeath_Iv1<-diff(cinc_mort_Iv1)
  dailydeath_Clv1<-diff(cinc_mort_Clv1)
  dailydeath_Hv1<-diff(cinc_mort_Hv1)
  dailydeath_ICUv1<-diff(cinc_mort_ICUv1)
  dailydeath_Ventv1<-diff(cinc_mort_Ventv1)
  
  dailydeath_Asym<-dailydeath_Iv1
  dailydeath_Mild_Mod<-dailydeath_H1+dailydeath_HC1+dailydeath_Clv1+dailydeath_Hv1
  dailydeath_ICU<-dailydeath_ICU1+dailydeath_ICUC1+dailydeath_ICUv1
  dailydeath_Vent<-dailydeath_Vent1+dailydeath_VentC1+dailydeath_Ventv1
  #total death (no subgroup)
  dailydeath_all<-c(0,dailydeath_H1+dailydeath_HC1+dailydeath_ICU1+dailydeath_ICUC1+dailydeath_Vent1+dailydeath_VentC1+dailydeath_Hv1+dailydeath_ICUv1+dailydeath_Ventv1)
  total_death<-sum(dailydeath_all)
  
  #death by age group (to export)
  Daily_mort_Asym_ag   <- out[,(IVindex+1)]*0
  
  Daily_mort_MildMod_ag<-parameters["nus"]*parameters["pdeath_h"]*(out[,(Hindex+1)]%*%diag(ifr[,2])) +
    parameters["nusc"]*parameters["pdeath_hc"]*(out[,(HCindex+1)]%*%diag(ifr[,2]))+ out[,(CLVindex+1)]*0+
    parameters["nus"]*parameters["pdeath_h"]*out[,(HVindex+1)]%*%diag(ifr[,2])
  
  Daily_mort_ICU_ag    <- parameters["nu_icu"]*parameters["pdeath_icu"]*out[,(ICUindex+1)]%*%diag(ifr[,2])+ # + out[,(ICUindex+1)]*mort
    parameters["nu_icuc"]*parameters["pdeath_icuc"]*out[,(ICUCindex+1)]%*%diag(ifr[,2])+ # + out[,(ICUCindex+1)]*mort
    parameters["nu_icu"]*parameters["pdeath_icu"]*out[,(ICUVindex+1)]%*%diag(ifr[,2])# + out[,(ICUVindex+1)]*mort
  
  Daily_mort_Vent_ag   <- parameters["nu_vent"]*parameters["pdeath_vent"]*out[,(Ventindex+1)]%*%diag(ifr[,2])+ #+ out[,(Ventindex+1)]*mort
    parameters["nu_ventc"]*parameters["pdeath_ventc"]*out[,(VentCindex+1)]%*%diag(ifr[,2])+ #+ out[,(VentCindex+1)]*mort
    parameters["nu_vent"]*parameters["pdeath_vent"]*out[,(VentVindex+1)]%*%diag(ifr[,2])# + out[,(VentVindex+1)]*mort
  
  Daily_mort_all_ag    <-Daily_mort_Asym_ag+Daily_mort_MildMod_ag+Daily_mort_ICU_ag+Daily_mort_Vent_ag
  
  #out[401:405,(Ventindex+1)]        
  #out[401:405,(Ventindex+1)]*ifr[,2]
  #out[401:405,(Ventindex+1)]%*%diag(ifr[,2])
  
  # Export in a cohesive format ----
  results <- list()
  results$time <- startdate + times  # dates
  results$daily_incidence <- round(dailyinc1)  # daily incidence (Reported)
  results$dailyinc1_gr <- dailyinc1_gr
  results$daily_death <-round(dailydeath_all)
  results$tpop1
  results$pop1
  results$Death_oth<- rowSums(Death_oth)
  results$lam<-rowSums(out[,610:631])
  results$Outcomes_death_oth <- matrix(NA,21,4) #  Vac_pop + 6 outputs (vac_pop, asym, sym, hosp, icu, icuven, death) * 5 years (2020-2024) = 31 in total
  results$Outcomes_death_oth[1:21,1] <- rowSums(t(round(Death_oth[,])))
  results$Outcomes_death_oth[1:21,2] <- rowSums(t(round(Death_oth))[,1:365])
  results$Outcomes_death_oth[1:21,3] <- rowSums(t(round(Death_oth))[,1+365:730])
  results$Outcomes_death_oth[1:21,4] <- rowSums(t(round(Death_oth))[,731:1095])
  
  
  #Outcomes to export
  results$Outcomes_mean1 <- matrix(NA,22,7) #  Vac_pop + 6 outputs (vac_pop, asym, sym, hosp, icu, icuven, death) * 5 years (2020-2024) = 31 in total
  #index 589-609
  #results$Outcomes_mean1[1,1] <- last(round(VacPsum[,1]))
  results$Outcomes_mean1[1,1] <- last(round(VacP[,1]))
  results$Outcomes_mean1[2,1] <- last(round(VacP[,2]))
  results$Outcomes_mean1[3,1] <- last(round(VacP[,3]))
  results$Outcomes_mean1[4,1] <- last(round(VacP[,4]))
  results$Outcomes_mean1[5,1] <- last(round(VacP[,5]))
  results$Outcomes_mean1[6,1] <- last(round(VacP[,6]))
  results$Outcomes_mean1[7,1] <- last(round(VacP[,7]))
  results$Outcomes_mean1[8,1] <- last(round(VacP[,8]))
  results$Outcomes_mean1[9,1] <- last(round(VacP[,9]))
  results$Outcomes_mean1[10,1] <- last(round(VacP[,10]))
  results$Outcomes_mean1[11,1] <- last(round(VacP[,11]))
  results$Outcomes_mean1[12,1] <- last(round(VacP[,12]))
  results$Outcomes_mean1[13,1] <- last(round(VacP[,13]))
  results$Outcomes_mean1[14,1] <- last(round(VacP[,14]))
  results$Outcomes_mean1[15,1] <- last(round(VacP[,15]))
  results$Outcomes_mean1[16,1] <- last(round(VacP[,16]))
  results$Outcomes_mean1[17,1] <- last(round(VacP[,17]))
  results$Outcomes_mean1[18,1] <- last(round(VacP[,18]))
  results$Outcomes_mean1[19,1] <- last(round(VacP[,19]))
  results$Outcomes_mean1[20,1] <- last(round(VacP[,20]))
  results$Outcomes_mean1[21,1] <- last(round(VacP[,21]))
  results$Outcomes_mean1[22,1] <- last(round(VacP[,1:21]))
  
  results$Outcomes_mean1[1:21,2] <- rowSums(t(round(Daily_inc_Asym_ag1))[,91:455]) #round(previcureq0)
  results$Outcomes_mean1[1:21,3] <- rowSums(t(round(Daily_inc_MildMod_ag1))[,91:455]) #round(previcureq01) 
  results$Outcomes_mean1[1:21,4] <- rowSums(t(round(Daily_inc_ICU_ag1))[,91:455])   #round(previcureq1)
  results$Outcomes_mean1[1:21,5] <- rowSums(t(round(Daily_inc_Vent_ag1))[,91:455])        #round(previcureq21)
  results$Outcomes_mean1[1:21,6] <- rowSums(t(round(Daily_inc_all_ag1))[,91:455])  #total cases
  results$Outcomes_mean1[1:21,7] <- rowSums(t(round(Daily_mort_all_ag))[,91:455]) #total deaths
  
  results$Outcomes_mean1[22,] <- colSums(results$Outcomes_mean1[1:21,])
  
  colnames(results$Outcomes_mean1) <- c("Vac_pop","Asym_Y1","MildMod_Y1","ICU_Y1","ICUVent_Y1","TotCase","TotalDeath")
  rownames(results$Outcomes_mean1) <- c("0-4 y.o.","5-9 y.o.","10-14 y.o.","15-19 y.o.","20-24 y.o.","25-29 y.o.","30-34 y.o.","35-39 y.o.","40-44 y.o.","45-49 y.o.","50-54 y.o.","55-59 y.o.","60-64 y.o.","65-69 y.o.","70-74 y.o.","75-79 y.o.","80-84 y.o.","85-89 y.o.","90-94 y.o.","95-99 y.o.",">=100 y.o.","Total")
  
  
  #Classified to 4 age groups: 0-19 (bin 1-4), 20-39 (bin 5-8), 40-64 (bin 9-13), 65+ (bin 14-21) with different year
  results$Outcomes_mean_4gr1yr <- matrix(NA,5,7) #  Vac_pop + 6 outputs (vac_pop, asym, sym, hosp, icu, icuven, death) * 5 years (2020-2024) = 31 in total
  #results$Outcomes_mean_4gr1yr[1,1] <-last(round(VacPsum))
  print(sum(results$Outcomes_mean1[1:4,1]))
  
  results$Outcomes_mean_4gr1yr[1,1] <- sum(results$Outcomes_mean1[1:4,1]) 
  results$Outcomes_mean_4gr1yr[2,1] <- sum(results$Outcomes_mean1[5:8,1]) 
  results$Outcomes_mean_4gr1yr[3,1] <- sum(results$Outcomes_mean1[9:13,1]) 
  results$Outcomes_mean_4gr1yr[4,1] <- sum(results$Outcomes_mean1[14:21,1])
  results$Outcomes_mean_4gr1yr[5,1] <- sum(results$Outcomes_mean1[1:21,1])
  
  results$Outcomes_mean_4gr1yr[1,2] <- sum(results$Outcomes_mean1[1:4,2])   
  results$Outcomes_mean_4gr1yr[2,2] <- sum(results$Outcomes_mean1[5:8,2])   
  results$Outcomes_mean_4gr1yr[3,2] <- sum(results$Outcomes_mean1[9:13,2])  
  results$Outcomes_mean_4gr1yr[4,2] <- sum(results$Outcomes_mean1[14:21,2])  
  
  results$Outcomes_mean_4gr1yr[1,3] <- sum(results$Outcomes_mean1[1:4,3])  
  results$Outcomes_mean_4gr1yr[2,3] <- sum(results$Outcomes_mean1[5:8,3])  
  results$Outcomes_mean_4gr1yr[3,3] <- sum(results$Outcomes_mean1[9:13,3]) 
  results$Outcomes_mean_4gr1yr[4,3] <- sum(results$Outcomes_mean1[14:21,3])
  
  results$Outcomes_mean_4gr1yr[1,4] <- sum(results$Outcomes_mean1[1:4,4])    
  results$Outcomes_mean_4gr1yr[2,4] <- sum(results$Outcomes_mean1[5:8,4])    
  results$Outcomes_mean_4gr1yr[3,4] <- sum(results$Outcomes_mean1[9:13,4])   
  results$Outcomes_mean_4gr1yr[4,4] <- sum(results$Outcomes_mean1[14:21,4])  
  
  results$Outcomes_mean_4gr1yr[1,5] <- sum(results$Outcomes_mean1[1:4,5])    
  results$Outcomes_mean_4gr1yr[2,5] <- sum(results$Outcomes_mean1[5:8,5])    
  results$Outcomes_mean_4gr1yr[3,5] <- sum(results$Outcomes_mean1[9:13,5])   
  results$Outcomes_mean_4gr1yr[4,5] <- sum(results$Outcomes_mean1[14:21,5])    
  
  results$Outcomes_mean_4gr1yr[1,6] <- sum(results$Outcomes_mean1[1:4,6])    
  results$Outcomes_mean_4gr1yr[2,6] <- sum(results$Outcomes_mean1[5:8,6])    
  results$Outcomes_mean_4gr1yr[3,6] <- sum(results$Outcomes_mean1[9:13,6])   
  results$Outcomes_mean_4gr1yr[4,6] <- sum(results$Outcomes_mean1[14:21,6])   
  
  results$Outcomes_mean_4gr1yr[1,7] <- sum(results$Outcomes_mean1[1:4,7])    
  results$Outcomes_mean_4gr1yr[2,7] <- sum(results$Outcomes_mean1[5:8,7])    
  results$Outcomes_mean_4gr1yr[3,7] <- sum(results$Outcomes_mean1[9:13,7])   
  results$Outcomes_mean_4gr1yr[4,7] <- sum(results$Outcomes_mean1[14:21,7])   
  
  
  results$Outcomes_mean_4gr1yr[5,] <- colSums(results$Outcomes_mean_4gr1yr[1:4,])
  
  
  colnames(results$Outcomes_mean_4gr1yr) <- c("Vac_pop","Asymtomatic","MildModerate","ICU","ICUVent","TotCase","TotalDeath")
  rownames(results$Outcomes_mean_4gr1yr) <- c("0-19 y.o.","20-39 y.o.","40-64 y.o.",">=65 y.o.","Total")
  
  
  #Classified to 4 age groups: 0-19 (bin 1-4), 20-39 (bin 5-8), 40-64 (bin 9-13), 65+ (bin 14-21)
  results$Outcomes_mean_4gr1 <- matrix(NA,5,7) #  Vac_pop + 6 outputs (vac_pop, asym, sym, hosp, icu, icuven, death) * 5 years (2020-2024) = 31 in total
  #results$Outcomes_mean_4gr1[1,1] <-last(round(VacPsum))
  results$Outcomes_mean_4gr1[1,1] <- sum(results$Outcomes_mean1[1:4,1])
  results$Outcomes_mean_4gr1[2,1] <- sum(results$Outcomes_mean1[5:8,1])
  results$Outcomes_mean_4gr1[3,1] <- sum(results$Outcomes_mean1[9:13,1])
  results$Outcomes_mean_4gr1[4,1] <- sum(results$Outcomes_mean1[14:21,1])
  results$Outcomes_mean_4gr1[5,1] <- sum(results$Outcomes_mean1[1:21,1])
  
  results$Outcomes_mean_4gr1[1,2] <- sum(results$Outcomes_mean1[1:4,2])  
  results$Outcomes_mean_4gr1[2,2] <- sum(results$Outcomes_mean1[5:8,2])  
  results$Outcomes_mean_4gr1[3,2] <- sum(results$Outcomes_mean1[9:13,2]) 
  results$Outcomes_mean_4gr1[4,2] <- sum(results$Outcomes_mean1[14:21,2]) 
  
  results$Outcomes_mean_4gr1[1,3] <- sum(results$Outcomes_mean1[1:4,3])  
  results$Outcomes_mean_4gr1[2,3] <- sum(results$Outcomes_mean1[5:8,3])  
  results$Outcomes_mean_4gr1[3,3] <- sum(results$Outcomes_mean1[9:13,3]) 
  results$Outcomes_mean_4gr1[4,3] <- sum(results$Outcomes_mean1[14:21,3])
  
  results$Outcomes_mean_4gr1[1,4] <- sum(results$Outcomes_mean1[1:4,4])    
  results$Outcomes_mean_4gr1[2,4] <- sum(results$Outcomes_mean1[5:8,4])    
  results$Outcomes_mean_4gr1[3,4] <- sum(results$Outcomes_mean1[9:13,4])   
  results$Outcomes_mean_4gr1[4,4] <- sum(results$Outcomes_mean1[14:21,4])     
  
  
  results$Outcomes_mean_4gr1[1,5] <- sum(results$Outcomes_mean1[1:4,5])    
  results$Outcomes_mean_4gr1[2,5] <- sum(results$Outcomes_mean1[5:8,5])    
  results$Outcomes_mean_4gr1[3,5] <- sum(results$Outcomes_mean1[9:13,5])   
  results$Outcomes_mean_4gr1[4,5] <- sum(results$Outcomes_mean1[14:21,5])   
  
  
  results$Outcomes_mean_4gr1[,6] <-  rowSums(results$Outcomes_mean_4gr1[,2:5])
  results$Outcomes_mean_4gr1[1,7] <- sum(results$Outcomes_mean1[1:4,7])
  results$Outcomes_mean_4gr1[2,7] <- sum(results$Outcomes_mean1[5:8,7])
  results$Outcomes_mean_4gr1[3,7] <- sum(results$Outcomes_mean1[9:13,7])
  results$Outcomes_mean_4gr1[4,7] <- sum(results$Outcomes_mean1[14:21,7])
  results$Outcomes_mean_4gr1[5,] <- colSums(results$Outcomes_mean_4gr1[1:4,])
  
  colnames(results$Outcomes_mean_4gr1) <- c("Vac_pop","Asym","MildMod","ICU","ICUVent","TotCase","TotDeath")
  rownames(results$Outcomes_mean_4gr1) <- c("0-19 y.o.","20-39 y.o.","40-64 y.o.",">=65 y.o.","Total")
  
  
  return(results)
}
#function to run with differnt parameter inputs
VacTPP_all_scens_4 <- function(par1, par2, par3, par4, par5, par6, par7, par8, par9){
  AgeVac<<-par1  #AgeAll, AgeAdult, AgeHighI, AgeElder
  parameters["vaccine_cov"] <- par2
  parameters["vaccine_eff1"] <- par3
  parameters["vaccine_eff2"] <- par4 #reduce viral shredding 0, 0.5,0.7
  parameters["vaccine_eff3"] <- par5 # reduce severity 0, 0.5,0.7
  parameters["omegav"]<- 1/(par6*365) # vaccine duration 0.5, 1
  parameters["omega"]<- 1/(par7*365)   # average duration of immunity (years) min 0.5 max 100 step 0.5  (default 200)
  parameters["dist_eff"]<-par8    # adherence to social distancing (%) min 0 max 100 step 1 , (0, 0.2, 0.4)
  parameters["mean_imports"]<-par9 #0,10,100
  #print(parameters)
  out.vaceff_2 <- ode(y = Y, times = times, func = covid, parms = parameters)
  results.vaceff_2 <- process_ode_outcome_1yr(out.vaceff_2)
  #
  return(results.vaceff_2)
}

#ReportA :InitR 3.5%
initR<-0.035*popstruc[,2] #3 values: 5%, 10%, 15% (now is 3.5% ref Wuhan paper)
Y<-c(initS,initE,initI,initR,initX,initH,initHC,initC,initCM,initSV,initEV,initIV,initCLV,initHV,initICUV,initVentV,initRV, initQS, initQE, initQI,initQR, initCL, initQC, initICU, initICUC, initVent, initVentC, initCMC, initVacP) # initial conditions for the main solution vector
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

