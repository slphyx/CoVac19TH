# Function
This app has three function
1.VacTPP_all_scens_4()
2.covid()
3.process_ode_outcome_1yr()

## VacTPP_all_scens_4 Function

This is a main function which use in the CoVac19TH.R to run Example.

This is the code
```R
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
```

This function require 9 parameter
1. par1 is AgeVac which has AgeAll, AgeAdult, AgeHighI and AgeElder
2. par2 is vaccine coverage
3. par3 is vaccine efficacy 1
4. par4 is vaccine efficacy 2 (reduce viral shredding 0, 0.5,0.7)
5. par5 is vaccine efficacy 3 (reduce severity 0, 0.5,0.7)
6. par6 is vaccine duration 0.5, 1
7. par7 is average duration of immunity (years) min 0.5 max 100 step 0.5  (default 200)
8. par8 is adherence to social distancing (%) min 0 max 100 step 1 , (0, 0.2, 0.4)
9. par9 is mean imports (0,10,100)

## covid Function
This function is used in VacTPP_all_scens_4 function to solving equations of the model

code in VacTPP_all_scens_4.R
```R
  out.vaceff_2 <- ode(y = Y, times = times, func = covid, parms = parameters)
```

## process_ode_outcome_1yr Function

This function is used in VacTPP_all_scens_4 function to running the model script with 1yr time frame

code in VacTPP_all_scens_4.R
```R
  results.vaceff_2 <- process_ode_outcome_1yr(out.vaceff_2)
```
