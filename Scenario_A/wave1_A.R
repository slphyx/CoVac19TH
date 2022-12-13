library(ggplot2)
library(dplyr)

setwd("D:/Work/como/wave1_2022/Scenario_A")
charToDate <- function(data){
  data[,1] <-as.Date(data[,1])
  return(data)
}

CumuHos <-function(data){
  data  %>% 
    mutate(Daily_Hospital = Hospital.Surge.Beds+ICU.Beds+Ventilators,Cumulative_Hospital = cumsum(Hospital.Surge.Beds+ICU.Beds+Ventilators))%>% 
    select(DateTime,Daily_Hospital,Cumulative_Hospital)
}

diff_percent <- function(base,data){
  percent <- (data-base)/base*100
  return(percent)
}

Base_A_Death <- charToDate(read.csv("severity_A/data/ScenarioA-cumulative-deat.csv"))
Base_A_Case <- charToDate(read.csv("severity_A/data/ScenarioA-daily-cases.csv"))
Base_A_Hos <- CumuHos(charToDate(read.csv("severity_A/data/ScenarioA-hospital-occupa.csv")))

#Scenario_A_Severity_high_inc_70
Eff1_A_high_inc_70_Death <- charToDate(read.csv("severity_A/data/severity_A_high_inc_70_1year-cumulative-deat.csv"))
Eff1_A_high_inc_70_Case <- charToDate(read.csv("severity_A/data/severity_A_high_inc_70_1year-daily-cases.csv"))
Eff1_A_high_inc_70_Hos <- CumuHos(charToDate(read.csv("severity_A/data/severity_A_high_inc_70_1year-hospital-occupa.csv")))
Eff1_A_high_inc_70_Death_Eff <- diff_percent(tail(Base_A_Death,1)[,3],tail(Eff1_A_high_inc_70_Death,1)[,3])
Eff1_A_high_inc_70_Case_Eff <- diff_percent(sum(Base_A_Case[,3]),sum(Eff1_A_high_inc_70_Case[,3]))
Eff1_A_high_inc_70_Hos_Eff <- diff_percent(sum(Base_A_Hos[,3]),sum(Eff1_A_high_inc_70_Hos[,3]))

#Scenario_A_Severity_high_inc_90
Eff1_A_high_inc_90_Death <- charToDate(read.csv("severity_A/data/severity_A_high_inc_90_HF-cumulative-deat.csv"))
Eff1_A_high_inc_90_Case <- charToDate(read.csv("severity_A/data/severity_A_high_inc_90_HF-daily-cases.csv"))
Eff1_A_high_inc_90_Hos <- CumuHos(charToDate(read.csv("severity_A/data/severity_A_high_inc_90_HF-hospital-occupa.csv")))
Eff1_A_high_inc_90_Death_Eff <- diff_percent(tail(Base_A_Death,1)[,3],tail(Eff1_A_high_inc_90_Death,1)[,3])
Eff1_A_high_inc_90_Case_Eff <- diff_percent(sum(Base_A_Case[,3]),sum(Eff1_A_high_inc_90_Case[,3]))
Eff1_A_high_inc_90_Hos_Eff <- diff_percent(sum(Base_A_Hos[,3]),sum(Eff1_A_high_inc_90_Hos[,3]))

#Scenario_A_Severity_high_risk_70
Eff1_A_high_risk_70_Death <- charToDate(read.csv("severity_A/data/severity_A_high_risk_70_1year-cumulative-deat.csv"))
Eff1_A_high_risk_70_Case <- charToDate(read.csv("severity_A/data/severity_A_high_risk_70_1year-daily-cases.csv"))
Eff1_A_high_risk_70_Hos <- CumuHos(charToDate(read.csv("severity_A/data/severity_A_high_risk_70_1year-hospital-occupa.csv")))
Eff1_A_high_risk_70_Death_Eff <- diff_percent(tail(Base_A_Death,1)[,3],tail(Eff1_A_high_risk_70_Death,1)[,3])
Eff1_A_high_risk_70_Case_Eff <- diff_percent(sum(Base_A_Case[,3]),sum(Eff1_A_high_risk_70_Case[,3]))
Eff1_A_high_risk_70_Hos_Eff <- diff_percent(sum(Base_A_Hos[,3]),sum(Eff1_A_high_risk_70_Hos[,3]))

#Scenario_A_Severity_high_risk_90
Eff1_A_high_risk_90_Death <- charToDate(read.csv("severity_A/data/severity_A_high_risk_90_HF-cumulative-deat.csv"))
Eff1_A_high_risk_90_Case <- charToDate(read.csv("severity_A/data/severity_A_high_risk_90_HF-daily-cases.csv"))
Eff1_A_high_risk_90_Hos <- CumuHos(charToDate(read.csv("severity_A/data/severity_A_high_risk_90_HF-hospital-occupa.csv")))
Eff1_A_high_risk_90_Death_Eff <- diff_percent(tail(Base_A_Death,1)[,3],tail(Eff1_A_high_risk_90_Death,1)[,3])
Eff1_A_high_risk_90_Case_Eff <- diff_percent(sum(Base_A_Case[,3]),sum(Eff1_A_high_risk_90_Case[,3]))
Eff1_A_high_risk_90_Hos_Eff <- diff_percent(sum(Base_A_Hos[,3]),sum(Eff1_A_high_risk_90_Hos[,3]))

#Scenario_A_susceptibility_high_inc_70
Eff2_A_high_inc_70_Death <- charToDate(read.csv("susceptibility_A/data/susceptibility_A_high_inc_70_1year-cumulative-deat.csv"))
Eff2_A_high_inc_70_Case <- charToDate(read.csv("susceptibility_A/data/susceptibility_A_high_inc_70_1year-daily-cases.csv"))
Eff2_A_high_inc_70_Hos <- CumuHos(charToDate(read.csv("susceptibility_A/data/susceptibility_A_high_inc_70_1year-hospital-occupa.csv")))
Eff2_A_high_inc_70_Death_Eff <- diff_percent(tail(Base_A_Death,1)[,3],tail(Eff2_A_high_inc_70_Death,1)[,3])
Eff2_A_high_inc_70_Case_Eff <- diff_percent(sum(Base_A_Case[,3]),sum(Eff2_A_high_inc_70_Case[,3]))
Eff2_A_high_inc_70_Hos_Eff <- diff_percent(sum(Base_A_Hos[,3]),sum(Eff2_A_high_inc_70_Hos[,3]))

#Scenario_A_susceptibility_high_inc_90
Eff2_A_high_inc_90_Death <- charToDate(read.csv("susceptibility_A/data/susceptibility_A_high_inc_90_HF-cumulative-deat.csv"))
Eff2_A_high_inc_90_Case <- charToDate(read.csv("susceptibility_A/data/susceptibility_A_high_inc_90_HF-daily-cases.csv"))
Eff2_A_high_inc_90_Hos <- CumuHos(charToDate(read.csv("susceptibility_A/data/susceptibility_A_high_inc_90_HF-hospital-occupa.csv")))
Eff2_A_high_inc_90_Death_Eff <- diff_percent(tail(Base_A_Death,1)[,3],tail(Eff2_A_high_inc_90_Death,1)[,3])
Eff2_A_high_inc_90_Case_Eff <- diff_percent(sum(Base_A_Case[,3]),sum(Eff2_A_high_inc_90_Case[,3]))
Eff2_A_high_inc_90_Hos_Eff <- diff_percent(sum(Base_A_Hos[,3]),sum(Eff2_A_high_inc_90_Hos[,3]))

#Scenario_A_susceptibility_high_risk_70
Eff2_A_high_risk_70_Death <- charToDate(read.csv("susceptibility_A/data/susceptibility_A_high_risk_70_1year-cumulative-deat.csv"))
Eff2_A_high_risk_70_Case <- charToDate(read.csv("susceptibility_A/data/susceptibility_A_high_risk_70_1year-daily-cases.csv"))
Eff2_A_high_risk_70_Hos <- CumuHos(charToDate(read.csv("susceptibility_A/data/susceptibility_A_high_risk_70_1year-hospital-occupa.csv")))
Eff2_A_high_risk_70_Death_Eff <- diff_percent(tail(Base_A_Death,1)[,3],tail(Eff2_A_high_risk_70_Death,1)[,3])
Eff2_A_high_risk_70_Case_Eff <- diff_percent(sum(Base_A_Case[,3]),sum(Eff2_A_high_risk_70_Case[,3]))
Eff2_A_high_risk_70_Hos_Eff <- diff_percent(sum(Base_A_Hos[,3]),sum(Eff2_A_high_risk_70_Hos[,3]))

#Scenario_A_susceptibility_high_risk_90
Eff2_A_high_risk_90_Death <- charToDate(read.csv("susceptibility_A/data/susceptibility_A_high_risk_90_HF-cumulative-deat.csv"))
Eff2_A_high_risk_90_Case <- charToDate(read.csv("susceptibility_A/data/susceptibility_A_high_risk_90_HF-daily-cases.csv"))
Eff2_A_high_risk_90_Hos <- CumuHos(charToDate(read.csv("susceptibility_A/data/susceptibility_A_high_risk_90_HF-hospital-occupa.csv")))
Eff2_A_high_risk_90_Death_Eff <- diff_percent(tail(Base_A_Death,1)[,3],tail(Eff2_A_high_risk_90_Death,1)[,3])
Eff2_A_high_risk_90_Case_Eff <- diff_percent(sum(Base_A_Case[,3]),sum(Eff2_A_high_risk_90_Case[,3]))
Eff2_A_high_risk_90_Hos_Eff <- diff_percent(sum(Base_A_Hos[,3]),sum(Eff2_A_high_risk_90_Hos[,3]))

df_A_diff <- data.frame(Type = c("E1_A_inc_70",
                               "E1_A_inc_90",
                               "E1_A_risk_70",
                               "E1_A_risk_90",
                               "E2_A_inc_70",
                               "E2_A_inc_90",
                               "E2_A_risk_70",
                               "E2_A_risk_90"),
                      Case=c(Eff1_A_high_inc_70_Case_Eff,
                             Eff1_A_high_inc_90_Case_Eff,
                             Eff1_A_high_risk_70_Case_Eff,
                             Eff1_A_high_risk_90_Case_Eff,
                             Eff2_A_high_inc_70_Case_Eff,
                             Eff2_A_high_inc_90_Case_Eff,
                             Eff2_A_high_risk_70_Case_Eff,
                             Eff2_A_high_risk_90_Case_Eff),
                      Death = c(Eff1_A_high_inc_70_Death_Eff,
                                Eff1_A_high_inc_90_Death_Eff,
                                Eff1_A_high_risk_70_Death_Eff,
                                Eff1_A_high_risk_90_Death_Eff,
                                Eff2_A_high_inc_70_Death_Eff,
                                Eff2_A_high_inc_90_Death_Eff,
                                Eff2_A_high_risk_70_Death_Eff,
                                Eff2_A_high_risk_90_Death_Eff),
                      Hospitalised = c(Eff1_A_high_inc_70_Hos_Eff,
                                Eff1_A_high_inc_90_Hos_Eff,
                                Eff1_A_high_risk_70_Hos_Eff,
                                Eff1_A_high_risk_90_Hos_Eff,
                                Eff2_A_high_inc_70_Hos_Eff,
                                Eff2_A_high_inc_90_Hos_Eff,
                                Eff2_A_high_risk_70_Hos_Eff,
                                Eff2_A_high_risk_90_Hos_Eff)
                      )

cbPalette <- c("#A52A2A", "#0000E1", "#006400", "#A9A9A9","#000000")

ggplot()+
  geom_line(data = Base_A_Case,aes(x=DateTime,y=Predicted.Reported,colour ="No vaccine"))+
  geom_line(data = Eff1_A_high_inc_70_Case,aes(x=DateTime,y=Predicted.Reported,colour ="Eff1_A_high_inc_70_1year"))+
  geom_line(data = Eff1_A_high_inc_90_Case,aes(x=DateTime,y=Predicted.Reported,colour ="Eff1_A_high_inc_90_0.5year"))+
  geom_line(data = Eff1_A_high_risk_70_Case,aes(x=DateTime,y=Predicted.Reported,colour ="Eff1_A_high_risk_70_1year"))+
  geom_line(data = Eff1_A_high_risk_90_Case,aes(x=DateTime,y=Predicted.Reported,colour ="Eff1_A_high_risk_90_0.5year"))+
  scale_color_manual(values=cbPalette)+
  xlab("Time") +
  ylab("Case") +
  theme(legend.position = c(0.75, 0.6),
        legend.title = element_blank(),
        legend.text = element_text(size = 6),
        )

ggsave(file = "Scenario_A_Severity_Case.png",width = 1920 , height =1080,units ="px",dpi = 300)

ggplot()+
  geom_line(data = Base_A_Death,aes(x=DateTime,y=Predicted.Reported,colour ="No vaccine"))+
  geom_line(data = Eff1_A_high_inc_70_Death,aes(x=DateTime,y=Predicted.Reported,colour ="Eff1_A_high_inc_70_1year"))+
  geom_line(data = Eff1_A_high_inc_90_Death,aes(x=DateTime,y=Predicted.Reported,colour ="Eff1_A_high_inc_90_0.5year"))+
  geom_line(data = Eff1_A_high_risk_70_Death,aes(x=DateTime,y=Predicted.Reported,colour ="Eff1_A_high_risk_70_1year"))+
  geom_line(data = Eff1_A_high_risk_90_Death,aes(x=DateTime,y=Predicted.Reported,colour ="Eff1_A_high_risk_90_0.5year"))+
  scale_color_manual(values=cbPalette)+
  xlab("Time") +
  ylab("Death") +
  theme(legend.position = c(0.75, 0.4),
        legend.title = element_blank(),
        legend.text = element_text(size = 6),
  )

ggsave(file = "Scenario_A_Severity_Death.png",width = 1920 , height =1080,units ="px",dpi = 300)

ggplot()+
  geom_line(data = Base_A_Hos,aes(x=DateTime,y=Cumulative_Hospital,color ="No vaccine"))+
  geom_line(data = Eff1_A_high_inc_70_Hos,aes(x=DateTime,y=Cumulative_Hospital,colour ="Eff1_A_high_inc_70_1year"))+
  geom_line(data = Eff1_A_high_inc_90_Hos,aes(x=DateTime,y=Cumulative_Hospital,colour ="Eff1_A_high_inc_90_0.5year"))+
  geom_line(data = Eff1_A_high_risk_70_Hos,aes(x=DateTime,y=Cumulative_Hospital,colour ="Eff1_A_high_risk_70_1year"))+
  geom_line(data = Eff1_A_high_risk_90_Hos,aes(x=DateTime,y=Cumulative_Hospital,colour ="Eff1_A_high_risk_90_0.5year"))+
  scale_color_manual(values=cbPalette)+
  xlab("Time") +
  ylab("Hospitalised") +
  theme(legend.position = c(0.75, 0.4),
        legend.title = element_blank(),
        legend.text = element_text(size = 6),
  )

ggsave(file = "Scenario_A_Severity_Hos.png",width = 1920 , height =1080,units ="px",dpi = 300)

###########Susceptibility###########

ggplot()+
  geom_line(data = Base_A_Case,aes(x=DateTime,y=Predicted.Reported,colour ="No vaccine"))+
  geom_line(data = Eff2_A_high_inc_70_Case,aes(x=DateTime,y=Predicted.Reported,colour ="Eff2_A_high_inc_70_1year"))+
  geom_line(data = Eff2_A_high_inc_90_Case,aes(x=DateTime,y=Predicted.Reported,colour ="Eff2_A_high_inc_90_0.5year"))+
  geom_line(data = Eff2_A_high_risk_70_Case,aes(x=DateTime,y=Predicted.Reported,colour ="Eff2_A_high_risk_70_1year"))+
  geom_line(data = Eff2_A_high_risk_90_Case,aes(x=DateTime,y=Predicted.Reported,colour ="Eff2_A_high_risk_90_0.5year"))+
  scale_color_manual(values=cbPalette)+
  xlab("Time") +
  ylab("Case") +
  theme(legend.position = c(0.75, 0.6),
        legend.title = element_blank(),
        legend.text = element_text(size = 6),
  )

ggsave(file = "Scenario_A_Susceptibility_Case.png",width = 1920 , height =1080,units ="px",dpi = 300)




ggplot()+
  geom_line(data = Base_A_Death,aes(x=DateTime,y=Predicted.Reported,colour ="No vaccine"))+
  geom_line(data = Eff2_A_high_inc_70_Death,aes(x=DateTime,y=Predicted.Reported,colour ="Eff2_A_high_inc_70_1year"))+
  geom_line(data = Eff2_A_high_inc_90_Death,aes(x=DateTime,y=Predicted.Reported,colour ="Eff2_A_high_inc_90_0.5year"))+
  geom_line(data = Eff2_A_high_risk_70_Death,aes(x=DateTime,y=Predicted.Reported,colour ="Eff2_A_high_risk_70_1year"))+
  geom_line(data = Eff2_A_high_risk_90_Death,aes(x=DateTime,y=Predicted.Reported,colour ="Eff2_A_high_risk_90_0.5year"))+
  scale_color_manual(values=cbPalette)+
  xlab("Time") +
  ylab("Death") +
  theme(legend.position = c(0.75, 0.4),
        legend.title = element_blank(),
        legend.text = element_text(size = 6),
  )

ggsave(file = "Scenario_A_Susceptibility_Death.png",width = 1920 , height =1080,units ="px",dpi = 300)

ggplot()+
  geom_line(data = Base_A_Hos,aes(x=DateTime,y=Cumulative_Hospital,color ="No vaccine"))+
  geom_line(data = Eff2_A_high_inc_70_Hos,aes(x=DateTime,y=Cumulative_Hospital,colour ="Eff2_A_high_inc_70_1year"))+
  geom_line(data = Eff2_A_high_inc_90_Hos,aes(x=DateTime,y=Cumulative_Hospital,colour ="Eff2_A_high_inc_90_0.5year"))+
  geom_line(data = Eff2_A_high_risk_70_Hos,aes(x=DateTime,y=Cumulative_Hospital,colour ="Eff2_A_high_risk_70_1year"))+
  geom_line(data = Eff2_A_high_risk_90_Hos,aes(x=DateTime,y=Cumulative_Hospital,colour ="Eff2_A_high_risk_90_0.5year"))+
  scale_color_manual(values=cbPalette)+
  xlab("Time") +
  ylab("Hospitalised") +
  theme(legend.position = c(0.75, 0.4),
        legend.title = element_blank(),
        legend.text = element_text(size = 6),
  )

ggsave(file = "Scenario_A_Susceptibility_Hos.png",width = 1920 , height =1080,units ="px",dpi = 300)

ggplot(df_A_diff, aes(fill=Type,x = Type,y=Case)) +
  geom_bar(stat = "identity")+
  theme(legend.title = element_blank())+
  geom_text(aes(y= ifelse(Case >=0,Case+0.5,Case-0.5),label = paste0(round(Case,2)," %")))+ 
  scale_x_discrete(guide = guide_axis(n.dodge = 2))

ggsave(file = "Scenario_A_Case_diff.png",width = 1920 , height =1080,units ="px",dpi = 300)

ggplot(df_A_diff, aes(fill=Type,x = Type,y=Death )) +
  geom_bar(stat = "identity")+
  theme(legend.title = element_blank())+
  geom_text(aes(y= ifelse(Death >=0,Death+0.5,Death-0.5),label = paste0(round(Death,2)," %")))+ 
  scale_x_discrete(guide = guide_axis(n.dodge = 2))

ggsave(file = "Scenario_A_Death_diff.png",width = 1920 , height =1080,units ="px",dpi = 300)

ggplot(df_A_diff, aes(fill=Type,x = Type,y=Hospitalised )) +
  geom_bar(stat = "identity")+
  theme(legend.title = element_blank())+
  geom_text(aes(y= ifelse(Hospitalised >=0,Hospitalised+0.1,Hospitalised-0.1),label = paste0(round(Hospitalised,2)," %")))+ 
  scale_x_discrete(guide = guide_axis(n.dodge = 2))

ggsave(file = "Scenario_A_Hos_diff.png",width = 1920 , height =1080,units ="px",dpi = 300)

