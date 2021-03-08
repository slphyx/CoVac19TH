# CoVac19TH
COVID19 Vaccine Modelling for Thailand
# Abstract
Thailand is facing the dilemma of which groups to prioritise for the limited first tranche of vaccinations in 2021. A mathematical modelling analysis was performed to compare the potential short-term impact of allocating the available doses to either the high-risk group (over 65-year-olds) or the high incidence group (aged 20-39). Vaccinating the high incidence group with a vaccine with sufficiently high protection against infection (more than 50%) could provide enough herd effects to delay the expected epidemic peak, resulting in fewer deaths within the 12-month time horizon compared to vaccinating the same number of the high-risk group. After 12 months, if no further vaccination or other interventions were deployed, this strategy would lead to more deaths. With the right vaccine efficacy profile, targeting the high incidence groups could be a viable short-term component of the Thai vaccination strategy. These results and emerging evidence on vaccines and susceptibility suggest prioritisation guidelines should be more nuanced.
# Usage
```r
Baseline_w_sd_0_imp10<-VacTPP_all_scens_4(NoVac,0,0,0,0,1,2,0,4.8)
Baseline_w_sd_40_imp10<-VacTPP_all_scens_4(NoVac,0,0,0,0,1,2,0.4,4.8)

Vac_Eff1_10M_AgeHig_70_sd40imp10<-VacTPP_all_scens_4(AgeHighI,0.52163,0.7,0,0,1,2,0.4,4.8)
Vac_Eff1_10M_AgeEld_70_sd40imp10<-VacTPP_all_scens_4(AgeElder,0.92172,0.7,0,0,1,2,0.4,4.8)
Vac_Eff1_10M_AgeHig_70_sd0imp10<-VacTPP_all_scens_4(AgeHighI,0.52163,0.7,0,0,1,2,0,4.8)

Vac_Eff3_10M_AgeEld_99_sd40imp10<-VacTPP_all_scens_4(AgeElder,0.92172,0,0,0.999,1,2,0.4,4.8)
Vac_Eff3_10M_AgeEld_70_sd40imp10<-VacTPP_all_scens_4(AgeElder,0.92172,0,0,0.7,1,2,0.4,4.8)
Vac_Eff3_10M_AgeHig_70_sd40imp10<-VacTPP_all_scens_4(AgeHighI,0.52163,0,0,0.7,1,2,0.4,4.8)
```
