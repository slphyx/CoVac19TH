#function to run with different parameter inputs

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
  out.vaceff_2 <- ode(y = Y, times = times, func = covid, parms = parameters)
  results.vaceff_2 <- process_ode_outcome_1yr(out.vaceff_2)
  return(results.vaceff_2)
}