library(ggplot2)
library(dplyr)
# set path
# setwd("D:/Work/CoVac19TH/Scenario_A")
# Change datatype character to date
charToDate <- function(data){
  data[,1] <-as.Date(data[,1])
  return(data)
}

# Sum daily hospital and cumulative of hospital occupancy
CumuHos <-function(data){
  data  %>% 
    mutate(Daily_Hospital = Hospital.Surge.Beds+ICU.Beds+Ventilators,Cumulative_Hospital = cumsum(Hospital.Surge.Beds+ICU.Beds+Ventilators))%>% 
    select(DateTime,Daily_Hospital,Cumulative_Hospital)
}

# different value between baseline and Scenario
diff_percent <- function(base,data){
  percent <- (data-base)/base*100
  return(percent)
}

# Read data
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

# data frame of all different between baseline and each Scenario
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

# 70% VE (severity), 1 year duration, HT group
# 70% VE (severity, 1 year duration, HS group
# “High transmission” (= high incidence) 
# and “high severity” (= high risk).
        
# color Palette for line plot
cbPalette <- c("#A52A2A", "#0000E1", "#006400", "#A9A9A9","#000000")
# order for ploting 
order_color_severity <- c('70% VE (severity), 1 year duration, HT group', 
            '90% VE (severity), 0.5 year duration, HT group', 
            '70% VE (severity), 1 year duration, HS group', 
            '90% VE (severity), 0.5 year duration, HS group', 
            'No vaccine')    
order_color_Susceptibility <- c('70% VE (susceptibility), 1 year duration, HT group', 
                               '90% VE (susceptibility), 0.5 year duration, HT group', 
                               '70% VE (susceptibility), 1 year duration, HS group', 
                               '90% VE (susceptibility), 0.5 year duration, HS group', 
                               'No vaccine')    
# plot daily case
ggplot()+
  geom_line(data = Base_A_Case,aes(x=DateTime,y=Predicted.Reported,colour ="No vaccine"))+
  geom_line(data = Eff1_A_high_inc_70_Case,aes(x=DateTime,y=Predicted.Reported,colour ="70% VE (severity), 1 year duration, HT group"))+
  geom_line(data = Eff1_A_high_inc_90_Case,aes(x=DateTime,y=Predicted.Reported,colour ="90% VE (severity), 0.5 year duration, HT group"))+
  geom_line(data = Eff1_A_high_risk_70_Case,aes(x=DateTime,y=Predicted.Reported,colour ="70% VE (severity), 1 year duration, HS group"))+
  geom_line(data = Eff1_A_high_risk_90_Case,aes(x=DateTime,y=Predicted.Reported,colour ="90% VE (severity), 0.5 year duration, HS group"))+
  scale_color_manual(values=cbPalette,
                    breaks=order_color_severity)+
  # ggtitle("Against Severity")+
  xlab("Time") +
  ylab("Cases") +
  theme(legend.position = c(0.75, 0.6),
        legend.title = element_blank(),
        legend.text = element_text(size = 6),
        )
# save pic 
ggsave(file = "Scenario_A_Severity_Case.png",width = 1920 , height =1080,units ="px",dpi = 300)

# plot cumulative death
ggplot()+
  geom_line(data = Base_A_Death,aes(x=DateTime,y=Predicted.Reported,colour ="No vaccine"))+
  geom_line(data = Eff1_A_high_inc_70_Death,aes(x=DateTime,y=Predicted.Reported,colour ="70% VE (severity), 1 year duration, HT group"))+
  geom_line(data = Eff1_A_high_inc_90_Death,aes(x=DateTime,y=Predicted.Reported,colour ="90% VE (severity), 0.5 year duration, HT group"))+
  geom_line(data = Eff1_A_high_risk_70_Death,aes(x=DateTime,y=Predicted.Reported,colour ="70% VE (severity), 1 year duration, HS group"))+
  geom_line(data = Eff1_A_high_risk_90_Death,aes(x=DateTime,y=Predicted.Reported,colour ="90% VE (severity), 0.5 year duration, HS group"))+
  scale_color_manual(values=cbPalette,
                     breaks=order_color_severity)+
  xlab("Time") +
  ylab("Death") +
  theme(legend.position = c(0.75, 0.4),
        legend.title = element_blank(),
        legend.text = element_text(size = 6),
  )
# save pic
ggsave(file = "Scenario_A_Severity_Death.png",width = 1920 , height =1080,units ="px",dpi = 300)

# plot cumulative hospital occupancy
ggplot()+
  geom_line(data = Base_A_Hos,aes(x=DateTime,y=Cumulative_Hospital,color ="No vaccine"))+
  geom_line(data = Eff1_A_high_inc_70_Hos,aes(x=DateTime,y=Cumulative_Hospital,colour ="70% VE (severity), 1 year duration, HT group"))+
  geom_line(data = Eff1_A_high_inc_90_Hos,aes(x=DateTime,y=Cumulative_Hospital,colour ="90% VE (severity), 0.5 year duration, HT group"))+
  geom_line(data = Eff1_A_high_risk_70_Hos,aes(x=DateTime,y=Cumulative_Hospital,colour ="70% VE (severity), 1 year duration, HS group"))+
  geom_line(data = Eff1_A_high_risk_90_Hos,aes(x=DateTime,y=Cumulative_Hospital,colour ="90% VE (severity), 0.5 year duration, HS group"))+
  scale_color_manual(values=cbPalette,
                     breaks=order_color_severity)+
  xlab("Time") +
  ylab("Hospitalised") +
  theme(legend.position = c(0.75, 0.4),
        legend.title = element_blank(),
        legend.text = element_text(size = 6),
  )
# save pic
ggsave(file = "Scenario_A_Severity_Hos.png",width = 1920 , height =1080,units ="px",dpi = 300)

###########Susceptibility###########
# plot daily case
ggplot()+
  geom_line(data = Base_A_Case,aes(x=DateTime,y=Predicted.Reported,colour ="No vaccine"))+
  geom_line(data = Eff2_A_high_inc_70_Case,aes(x=DateTime,y=Predicted.Reported,colour ="70% VE (susceptibility), 1 year duration, HT group"))+
  geom_line(data = Eff2_A_high_inc_90_Case,aes(x=DateTime,y=Predicted.Reported,colour ="90% VE (susceptibility), 0.5 year duration, HT group"))+
  geom_line(data = Eff2_A_high_risk_70_Case,aes(x=DateTime,y=Predicted.Reported,colour ="70% VE (susceptibility), 1 year duration, HS group"))+
  geom_line(data = Eff2_A_high_risk_90_Case,aes(x=DateTime,y=Predicted.Reported,colour ="90% VE (susceptibility), 0.5 year duration, HS group"))+
  scale_color_manual(values=cbPalette,
                     breaks=order_color_Susceptibility)+
  xlab("Time") +
  ylab("Cases") +
  theme(legend.position = c(0.75, 0.6),
        legend.title = element_blank(),
        legend.text = element_text(size = 6),
  )
# save pic
ggsave(file = "Scenario_A_Susceptibility_Case.png",width = 1920 , height =1080,units ="px",dpi = 300)

# plot cumulative death
ggplot()+
  geom_line(data = Base_A_Death,aes(x=DateTime,y=Predicted.Reported,colour ="No vaccine"))+
  geom_line(data = Eff2_A_high_inc_70_Death,aes(x=DateTime,y=Predicted.Reported,colour ="70% VE (susceptibility), 1 year duration, HT group"))+
  geom_line(data = Eff2_A_high_inc_90_Death,aes(x=DateTime,y=Predicted.Reported,colour ="90% VE (susceptibility), 0.5 year duration, HT group"))+
  geom_line(data = Eff2_A_high_risk_70_Death,aes(x=DateTime,y=Predicted.Reported,colour ="70% VE (susceptibility), 1 year duration, HS group"))+
  geom_line(data = Eff2_A_high_risk_90_Death,aes(x=DateTime,y=Predicted.Reported,colour ="90% VE (susceptibility), 0.5 year duration, HS group"))+
  scale_color_manual(values=cbPalette,
                     breaks=order_color_Susceptibility)+
  xlab("Time") +
  ylab("Death") +
  theme(legend.position = c(0.75, 0.4),
        legend.title = element_blank(),
        legend.text = element_text(size = 6),
  )
# save pic
ggsave(file = "Scenario_A_Susceptibility_Death.png",width = 1920 , height =1080,units ="px",dpi = 300)
# plot cumulative hospital occupancy
ggplot()+
  geom_line(data = Base_A_Hos,aes(x=DateTime,y=Cumulative_Hospital,color ="No vaccine"))+
  geom_line(data = Eff2_A_high_inc_70_Hos,aes(x=DateTime,y=Cumulative_Hospital,colour ="70% VE (susceptibility), 1 year duration, HT group"))+
  geom_line(data = Eff2_A_high_inc_90_Hos,aes(x=DateTime,y=Cumulative_Hospital,colour ="90% VE (susceptibility), 0.5 year duration, HT group"))+
  geom_line(data = Eff2_A_high_risk_70_Hos,aes(x=DateTime,y=Cumulative_Hospital,colour ="70% VE (susceptibility), 1 year duration, HS group"))+
  geom_line(data = Eff2_A_high_risk_90_Hos,aes(x=DateTime,y=Cumulative_Hospital,colour ="90% VE (susceptibility), 0.5 year duration, HS group"))+
  scale_color_manual(values=cbPalette,
                     breaks=order_color_Susceptibility)+
  xlab("Time") +
  ylab("Hospitalised") +
  theme(legend.position = c(0.75, 0.4),
        legend.title = element_blank(),
        legend.text = element_text(size = 6),
  )
# save pic
ggsave(file = "Scenario_A_Susceptibility_Hos.png",width = 1920 , height =1080,units ="px",dpi = 300)

# bar plot different Case
ggplot(df_A_diff, aes(fill=Type,x = Type,y=Case)) +
  geom_bar(stat = "identity")+
  theme(legend.title = element_blank())+
  geom_text(aes(y= ifelse(Case >=0,Case+0.5,Case-0.5),label = paste0(round(Case,2)," %")))+ 
  scale_x_discrete(guide = guide_axis(n.dodge = 2))
# save pic
ggsave(file = "Scenario_A_Case_diff.png",width = 1920 , height =1080,units ="px",dpi = 300)

# bar plot different deaths
ggplot(df_A_diff, aes(fill=Type,x = Type,y=Death )) +
  geom_bar(stat = "identity")+
  theme(legend.title = element_blank())+
  geom_text(aes(y= ifelse(Death >=0,Death+0.5,Death-0.5),label = paste0(round(Death,2)," %")))+ 
  scale_x_discrete(guide = guide_axis(n.dodge = 2))
# save pic
ggsave(file = "Scenario_A_Death_diff.png",width = 1920 , height =1080,units ="px",dpi = 300)

# bar plot different Hospitalised
ggplot(df_A_diff, aes(fill=Type,x = Type,y=Hospitalised )) +
  geom_bar(stat = "identity")+
  theme(legend.title = element_blank())+
  geom_text(aes(y= ifelse(Hospitalised >=0,Hospitalised+0.1,Hospitalised-0.1),label = paste0(round(Hospitalised,2)," %")))+ 
  scale_x_discrete(guide = guide_axis(n.dodge = 2))
# save pic
ggsave(file = "Scenario_A_Hos_diff.png",width = 1920 , height =1080,units ="px",dpi = 300)

