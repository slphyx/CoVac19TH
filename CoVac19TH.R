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

########## POPULATION Structure
load('data/THpopstruct.RData')
load('data/mort_sever_default.Rda')

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
load('data/THcontacts.RData')

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
source("function/covid.R")

#create new model script with 1yr time frame
source("function/process_ode_outcome_1yr.R")

#function to run with differnt parameter inputs
source("function/VacTPP_all_scens_4.R")

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


plot(Baseline_w_sd_0_imp10[["time"]],Baseline_w_sd_0_imp10[["daily_incidence"]],xlab="time",ylab="daily_incidence",type="l", col="1",xlim = as.Date(c("2021-01-01","2023-01-01")))
lines(Baseline_w_sd_40_imp10[["time"]], Baseline_w_sd_40_imp10[["daily_incidence"]], col="2")

plot(Vac_Eff1_10M_AgeHig_70_sd40imp10[["time"]], Vac_Eff1_10M_AgeHig_70_sd40imp10[["daily_incidence"]], col="3",xlab="time",ylab="daily_incidence",type="l",xlim = as.Date(c("2021-01-01","2023-01-01")),ylim = c(0,16000))
lines(Vac_Eff3_10M_AgeEld_70_sd40imp10[["time"]], Vac_Eff3_10M_AgeEld_70_sd40imp10[["daily_incidence"]], col="4")
lines(Vac_Eff3_10M_AgeHig_70_sd40imp10[["time"]], Vac_Eff3_10M_AgeHig_70_sd40imp10[["daily_incidence"]], col="5")
lines(Vac_Eff1_10M_AgeEld_70_sd40imp10[["time"]], Vac_Eff1_10M_AgeEld_70_sd40imp10[["daily_incidence"]], col="6")
lines(Vac_Eff3_10M_AgeEld_99_sd40imp10[["time"]], Vac_Eff3_10M_AgeEld_99_sd40imp10[["daily_incidence"]], col="7")
lines(Vac_Eff1_10M_AgeHig_70_sd0imp10[["time"]], Vac_Eff1_10M_AgeHig_70_sd0imp10[["daily_incidence"]], col="8")
lines(Vac_Eff1_10M_AgeHig_90_sd0imp10[["time"]], Vac_Eff1_10M_AgeHig_90_sd0imp10[["daily_incidence"]], col="9")

