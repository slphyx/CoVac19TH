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