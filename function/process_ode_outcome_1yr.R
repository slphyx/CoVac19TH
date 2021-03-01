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