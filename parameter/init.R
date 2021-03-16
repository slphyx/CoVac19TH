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
#ReportA :InitR 3.5%
initR<-0.035*popstruc[,2] #3 values: 5%, 10%, 15% (now is 3.5% ref Wuhan paper)

# initial conditions for the main solution vector
(Y<-c(initS,initE,initI,initR,initX,initH,initHC,initC,initCM,initSV,initEV,initIV,initCLV,initHV,initICUV,initVentV,initRV, initQS, initQE, initQI,initQR, initCL, initQC, initICU, initICUC, initVent, initVentC, initCMC, initVacP))