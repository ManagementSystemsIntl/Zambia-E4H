#Load Packages


#source("C:/Users/NyimbiliShida/Documents/MSI/GIS & Visuals/R Data/Scripts/r prep.R")
source("scripts/r prep.r")


##Malaria 2020

ip <- read_xls("C:/Users/NyimbiliShida/Documents/MSI/GIS & Visuals/R Data/Data PMI/malrain.xls")


ip$period <- as.Date(ip$period)  

ip1 <- ip[order(ip$period), ]

ip1

#plot

ot_plt <- ggplot(ip1,aes(x=period))+
  geom_line(aes(y=Malaria_Cases, color="Malaria Cases"), size=1) +
  geom_line(aes(y=Rainfall*200, color="Rainfall(mm)"), size=1) + 
  geom_point(aes(y=Rainfall*200, color="Rainfall(mm)"), size=3)+
  scale_x_date(date_labels="%b %Y",date_breaks="1 month")+
  scale_y_continuous(sec.axis=sec_axis(trans = ~ .*0.005, labels=comma, name = "Rainfall (mm)"))+
  labs(color="Legend:", title="Malaria Cases: A focus on 2020", x="Period", y="Malaria Cases") + 
  scale_color_manual(values = c("Malaria Cases"="#BA0C2F", "Rainfall(mm)"="#002A6C")) + base

ot_plt

ggsave("C:/Users/NyimbiliShida/Documents/MSI/GIS & Visuals/R Data/Visuals Exports/cases and rainfall.png",
       device="png",
       type="cairo",
       height = 7,
       width = 12)










##Death and Artesunate stockouts
ats <- read_xls("C:/Users/NyimbiliShida/Documents/MSI/GIS & Visuals/R Data/Data PMI/ArtusunateSs.xls")

ats

ats$ReportingMonth <- as.Date(ats$ReportingMonth)  

ats1 <- ats[order(ats$ReportingMonth), ]

# ats1 <- melt(ats1, id = "ReportingMonth")

ats1


id_plt <- ggplot(ats1) +
  geom_bar(aes(x=ReportingMonth, y=Stockout,  fill=commodity), stat="identity", position = "dodge") +
  geom_line(aes(x=ReportingMonth, y=Deaths, fill="Deaths") ,color="#BA0C2F", size=1)+
  scale_y_continuous(sec.axis=sec_axis(trans = ~ .*1, name = "Malaria Deaths")) +
  scale_x_date(date_labels="%b %Y",date_breaks="2 months")+
  labs(fill="Stockouts", title="Malaria Deaths And Commodity Stockouts - Nchelenge", x="Period", y="Number of stockout days") + 
  scale_fill_manual(values = c("#85C1E9", "#BA0C2F", "#002F6C", "#EF7D00")) + base

id_plt
ggsave("C:/Users/NyimbiliShida/Documents/MSI/GIS & Visuals/R Data/Visuals Exports/Death and commodity.png",
       device="png",
       type="cairo",
       height = 7,
       width = 12)

  
###IPT ITNS
library(gganimate)
ip <- read_xls("C:/Users/NyimbiliShida/Documents/MSI/GIS & Visuals/R Data/Data PMI/iptpreg.xls")

# ip <- melt(data = ip, id.vars = c("periodname","IPT_1ANC", "ITN_Preganant_Women","Malaria_In_Pregnancy"), variable.name = "periodname")


ip$periodname <- as.Date(ip$periodname)  

ip1 <- ip[order(ip$periodname), ]

ip1

#plot

ot_plt <- ggplot(ip1,aes(x=periodname))+
  geom_line(aes(y=IPT_1ANC, color="IPT 1ANC"), size=1) +
  geom_point(aes(y=IPT_1ANC, color="IPT 1ANC"), size=3) +
  geom_line(aes(y=ITN_Preganant_Women, color="ITN Preganant Women"), size=1)+
  geom_point(aes(y=ITN_Preganant_Women, color="ITN Preganant Women"), size=3)+
  geom_line(aes(y=Malaria_In_Pregnancy, color="Malaria In Pregnancy"), size=1)+
  geom_point(aes(y=Malaria_In_Pregnancy, color="Malaria In Pregnancy"), size=3)+
  scale_x_date(date_labels="%b %Y",date_breaks="3 months")+
  labs(color="Legend:", title="Malaria in pregnancy, IPT and ITN provided to pregnant woman at ANC coverage visit - Nchelenge", x="Period", y="Count") + 
  scale_color_manual(values = c("IPT 1ANC"="#002A6C", "ITN Preganant Women"="#AEB6BF", "Malaria In Pregnancy"="#BA0C2F")) + 
  transition_reveal(periodname) + base

ot_plt
animate(ot_plt, height = 800, width =1000)

anim_save("C:/Users/NyimbiliShida/Documents/MSI/GIS & Visuals/R Data/Visuals Exports/iptitn.gif",
          device="gif",
          type="cairo",
          height = 800,
          width = 1000)


ot_plt
ggsave("C:/Users/NyimbiliShida/Documents/MSI/GIS & Visuals/R Data/Visuals Exports/iptitn.png",
       device="png",
       type="cairo",
       height = 7,
       width = 12)

ot_plt <- ggplot(ip1) +
  geom_line(aes(x=periodname, y=IPT_1ANC, fill="IPT 1st AN"), color="#002A6C", position="dodge", size=1, stat='identity')+
  geom_line(aes(x=periodname, y=ITN_Preganant_Women, fill="ITN Preganant Women"), position="dodge", color= "#CFCDC9", size=1, stat='identity') +
  geom_line(aes(x=periodname, y=Malaria_In_Pregnancy, fill="Malaria In Pregnancy"), color="#A7C6ED",size=1, stat='identity', position="dodge") +
  scale_color_manual(values=c("#002A6C", "#CFCDC9", "#A7C6ED"))+
  scale_x_date(date_labels="%b %Y",date_breaks="4 months")+
  labs(fill="Legend:", title="RDT confirmed overlayed with Malaria diagnostic positivity rate - Nchelenge",
       x="Period",
       y="Diagnostic Positivity(%)") + base
ot_plt





###OTTS
ot <- read_xls("C:/Users/NyimbiliShida/Documents/MSI/GIS & Visuals/R Data/Data PMI/OTSS_ALL.xls")


ot$Periods <- as.Date(ot$Periods)  

ot1 <- ot[order(ot$Periods), ]

ot1
#plot
ot_plt <- ggplot(ot1) +
  geom_line(aes(x=Periods, y=Adherence_to_Negative_Test_Results, fill=positive), color="#002A6C", size=1, lty="longdash") +
  geom_line(aes(x=Periods, y=Adherence_to_Positive_Test_Results), color="#A569BD", size=1, lty="longdash") +
  geom_line(aes(x=Periods, y=Testing_Prior_to_Treatment_Score), colour="#F5B041",size=1) +
  geom_line(aes(x=Periods, y=Clinical_Observation_Overall), size=1) +
  geom_line(aes(x=Periods, y=RDT100_RDT_Observation), size=1) +
  geom_line(aes(x=Periods, y=Overall_Laboratory_Score), size=1) + 
  scale_color_manual(values=c("#002A6C", "#CFCDC9", "#A7C6ED", "#A7C6ED", "#A7C6ED", "#A7C6ED"))+
  labs(fill="Legend:", title="RDT confirmed overlayed with Malaria diagnostic positivity rate - Nchelenge",
       x="Period",
       y="Diagnostic Positivity(%)") + base
ot_plt

##RDT
rdt <- read_xls("C:/Users/NyimbiliShida/Documents/MSI/GIS & Visuals/R Data/Data PMI/rdtpositivity.xls")


rdt$period <- as.Date(rdt$period)  

rdt1 <- rdt[order(rdt$period), ]

rdt1

#Plot
rdt_plt <- ggplot(rdt1) + 
  geom_bar(aes(x=period, y=RDT1/5, fill=RDT), stat="identity") +
  scale_fill_manual(values=c("#BA0C2F", "#002F6C")) +
  geom_line(aes(x=period, y=positivity_rate, fill=positive), color="#BA0C2F", size=1)+
  #geom_point(aes(x=period, y=positivity_rate), stat="identity",color="#BA0C2F",size=2, alpha=0.5) +
  scale_x_date(date_labels="%b %Y",date_breaks="4 months")+
  scale_y_continuous(sec.axis=sec_axis(trans = ~ .*5, labels=comma, name = "RDT"))+
  labs(fill="Legend:", title="RDT confirmed overlayed with Malaria diagnostic positivity rate - Nchelenge",
       x="Period",
       y="Diagnostic Positivity(%)") + base
# theme(legend.key=element_blank(), legend.title=element_blank(), 
#       legend.box="horizontal",legend.position = "bottom")

# mallbs <- c("97,227",
#             "88,475",
#             "55,123",
#             "71,173",
#             "66,514",
#             "47,015",
#             "137,986",
#             "109,894")

rdt_plt
ggsave("C:/Users/NyimbiliShida/Documents/MSI/GIS & Visuals/R Data/Visuals Exports/RDT.png",
       device="png",
       type="cairo",
       height = 7,
       width = 12)



##indoor resting density
source("scripts/r prep.r")
insecty <- read_xls("C:/Users/NyimbiliShida/Documents/MSI/GIS & Visuals/R Data/Data PMI/insecticidetype.xls")


# ggplot(dat1, aes(monthyr, Malaria_Confirmed_Cases)) + geom_line()
#insecty <- melt(data = indens, id.vars = c("mnth","gambia", "Site","funestus"), variable.name = "mnth")
insecty$mont <- as.Date(insecty$mont)  

insecty1 <- insecty[order(insecty$mont), ]

insecty1

# insecty$mont <- as.Date(insecty$mont) origin="2014"
# insect1 <- insecty[order(insecty$mont), ]
# # 
# indens1

#Calculation sd
# sd(indens1$funestus)

id_plt <- ggplot(insecty1) + 
  geom_bar(aes(x=mont, y=Coverage, fill=Insecticide), stat="identity", position = "dodge") +
  scale_fill_manual(values=c("#002A6C", "#CFCDC9", "#CB4335","#A7C6ED")) +
  geom_line(aes(x=mont, y=Malaria/1200, fill="Malaria Cases") ,color="#BA0C2F", size=1) + 
  geom_point(aes(x=mont, y=Malaria/1200), stat="identity",color="#BA0C2F",size=3) +
  scale_x_date(date_labels="%Y",date_breaks="1 year", limits = NULL)+
  scale_y_continuous(sec.axis = sec_axis(trans = ~ .*1200, labels=comma, name = "Malaria cases"))+
  labs(fill="Legend:", title="Malaria Cases and Insecticide Types Used Over Time - Nchelenge",
       x=" Period",
       y="Coverage(%)") + base
  # theme(legend.key=element_blank(), legend.title=element_blank(),
  #       legend.box="horizontal",legend.position = "bottom")

mallbs <- c("97,227",
            "88,475",
            "55,123",
            "71,173",
            "66,514",
            "47,015",
            "137,986",
            "109,894")
        
id_plt

# id_plt + geom_label(aes(x=mont, y=Malaria/1200),
#                     label=mallbs, color = c("#002A6C","#002A6C","#002A6C","#002A6C","#002A6C", "#CFCDC9","#CFCDC9", "#A7C6ED"), fontface = "bold", hjust=0.5, vjust = 1.6)

ggsave("C:/Users/NyimbiliShida/Documents/MSI/GIS & Visuals/R Data/Visuals Exports/Malaria & Insecticide withot labels.png",
       device="png",
       type="cairo",
       height = 7,
       width = 12)




##indoor resting density
indens <- read_xls("C:/Users/NyimbiliShida/Documents/MSI/GIS & Visuals/R Data/Data PMI/inddensity.xls")

indens

indens <- melt(data = indens, id.vars = c("mnth","gambia", "Site","funestus"), variable.name = "mnth")

indens

indens$mnth <- as.Date(indens$mnth)  

indens1 <- indens[order(indens$mnth), ]

indens1


id_plt <- ggplot(indens1, aes(x=mnth, y=gambia, fill = Site)) +
  geom_bar(stat="identity", position = "dodge") + 
  # geom_errorbar( aes(x=mnth, ymin=gambia-sd, ymax=gambia+sd), width=0.4, colour="orange", alpha=0.9, size=0.9) +
  scale_x_date(date_labels="%b %Y",date_breaks="3 months", limits = NULL) + 
  scale_fill_manual(values=c("#A7C6ED","#C2113A")) +
  labs(fill="Legend:", title="Indoor Resting Density An.gambia s.I",
       x=" Period",
       y="Indoor density (No of vectors per house per night)") + base

id_plt

ggsave("C:/Users/NyimbiliShida/Documents/MSI/GIS & Visuals/R Data/Visuals Exports/resting density an An.gambia.png",
       device="png",
       type="cairo",
       height = 7,
       width = 12)



##Animated trends Malaria
library(gganimate)

iptm <- read_xls("C:/Users/NyimbiliShida/Documents/MSI/GIS & Visuals/R Data/Data PMI/ipt.xls")


iptm_data$period <- as.Date(as.character(iptm$period), format = "%Y")

iptm_long <- melt(iptm_data, id = "period")
iptm_long

# Plot
iptm_plt <- ggplot(iptm_long, aes(x=period, y=value,  color=variable)) +
  geom_line(size=1, alpha=0.5) +
  geom_point(size=3) +
  # scale_color_viridis(discrete = TRUE) +
  scale_x_date(date_labels="%Y",date_breaks="1 year") +
  scale_color_manual(values=c("#002A6C","#C2113A", "#EF7D00"))+
  scale_y_continuous(labels=comma) +
  labs(color="Legend:", title="Nchelenge Confirmed Cases",
       x="",
       y="cases") +
  transition_reveal(period) + base
animate(iptm_plt, height = 800, width =1000)

# #anim_save("C:/Users/NyimbiliShida/Documents/MSI/GIS & Visuals/R Data/Visuals Exports/smooth-animation-Severe-Malaria.gif",
#           device="gif",
#           type="cairo",
#           height = 7,
#           width = 15)

anim_save("viz/Malaria/severe.gif",
       plot = iptm_plt,
       device="gif",
       type="cairo",
       height=6,
       width=13)


##Malaria In Pregnancy

iptm <- read_xls("C:/Users/NyimbiliShida/Documents/MSI/GIS & Visuals/R Data/Data PMI/ipt.xls")

iptm

iptm_data$period <- as.Date(as.character(iptm$period), format = "%Y")      #Date type in R is always a combination of year, month and day (not necessarily in this order). You cannot have a Date type with only the year

class(period)

sapply(iptm_data, mode)

str(iptm_data)

iptm_long <- melt(iptm_data, id = "period")
gfg_plot <- ggplot(iptm_long,aes(x = period,y = value, color = variable)) +  geom_line(size=1, alpha=0.5) + geom_point(size=3)+
  #scale_x_continuous(date_breaks(width="4 months")) +
  scale_color_manual(values=c("#002A6C","#C2113A", "#EF7D00"))+
  scale_y_continuous(labels=comma) +
  labs(color="Legend:", title="Nchelenge Confirmed Cases",
       x="",
       y="cases") +
  base
gfg_plot

ggsave("C:/Users/NyimbiliShida/Documents/MSI/GIS & Visuals/R Data/Visuals Exports/Malaria in pregnanc2y.png",
       device="png",
       type="cairo",
       height = 7,
       width = 12)



## Severe Malaria and Deaths
source("scripts/r prep.r")

malsv <- read_xls("C:/Users/NyimbiliShida/Documents/MSI/GIS & Visuals/R Data/Data PMI/severemalaria.xls")

#lapply(malsv, na.omit)
malsv

malsv <- melt(malsv[c(1, 2, 3)], id = 'period')

malsv$period <- as.Date(malsv$period)                 
malsv1 <- malsv[order(malsv$period), ]

#Campaigns
dt <- as.Date("2017-10-01")
dt1 <- as.Date("2018-10-01")
dt2 <- as.Date("2020-12-01")

malsv <- ggplot(malsv1, aes(x=period, y=value, fill = variable)) +
  geom_area(alpha=.7)+
  scale_fill_manual(values=c("#A7C6ED","#C2113A")) +
  scale_y_continuous(labels=comma) +
  scale_x_date(date_labels="%b %Y",date_breaks="4 month", limits = NULL) +
  labs(fill="Legend:", title="Severe Malaria and Deaths",
       x="",
       y="Cases") +
  base +
  geom_vline(xintercept = c(dt, dt1, dt2) ,color=c("#EF7D00","#198a00ff", "#198a00ff") ,lty=c("solid","dotted", "dotted") ,size=c(2,1,1), alpha=1)+
  annotate("text", x = dt, y = 0, label = substitute(paste(bold('Integrated community case management: Oct'))), size=4, angle=90, hjust =-0.3, vjust=-1, color="#EF7D00")+
  annotate("text", x = dt1, y = 0, label = substitute(paste(bold('ITNS Mass Dist.Campaigns: Oct'))), size=4, angle=90, hjust =-1, vjust=-0.7, color="#198a00ff")+
  annotate("text", x = dt2, y = 0, label = substitute(paste(bold('ITNS Mass Dist.Campaigns: Dec'))), size=4, angle=90, hjust =-1, vjust=-0.5, color="#198a00ff")


malsv

ggsave("C:/Users/NyimbiliShida/Documents/MSI/GIS & Visuals/R Data/Visuals Exports/Severe & Death Cases.png",
       device="png",
       type="cairo",
       height=7,
       width=13)


##Malaria Cases -5 & 5+

source("C:/Users/NyimbiliShida/Documents/MSI/GIS & Visuals/R Data/Scripts/r prep.R")
source("scripts/r prep.r")
library(gganimate)

mal5 <- read_xls("C:/Users/NyimbiliShida/Documents/MSI/GIS & Visuals/R Data/Data PMI/malariacasesfives.xls")

lapply(mal5, na.omit)

mal5 <- melt(mal5[, c(1, 2,3)], id = 'year')

as.Date(as.Date.numeric(mal5$year,origin = "2016-10-01"),format = "%Y%m%d")

mal5plt <- ggplot(mal5, aes(x=year, y=value, fill = variable)) +
  geom_area(alpha=.7)+
  scale_fill_manual(values=c("#A7C6ED","#C2113A")) +
  scale_y_continuous(labels=comma) +
  #scale_x_date(date_labels="%Y-%m",date_breaks="6 months")+
  labs(fill="Legend", title="Malaria Cases by Age groups",
    x="",
    y="Cases") +
  geom_vline(xintercept = c(as.POSIXct("2016-10-01"), as.POSIXct("2017-10-01"), as.POSIXct("2017-09-01"), as.POSIXct("2018-10-01"), as.POSIXct("2019-10-01"), as.POSIXct("2020-10-01"), as.POSIXct("2020-12-01"),  as.POSIXct("2021-10-01")),
             color=c("#EF7D00","#198a00ff","#EF7D00","#EF7D00","#EF7D00","#EF7D00","#198a00ff","#EF7D00"),
             lty=c("1343","solid", "1343", "1343", "1343", "1343", "solid",  "1343") ,
             size=c(1,1.5,1,1,1,1,1.5,1),
             alpha=1) +
  annotate("text", x = as.POSIXct("2016-10-01"), y = 0, label = substitute(paste(bold('IRS Campaigns: Oct'))), size=4, angle=90, hjust =-2.5, vjust=-0.6, color="#EF7D00") +
  annotate("text", x = as.POSIXct("2017-10-01"), y = 0, label = substitute(paste(bold('ITNS Mass Dist.Campaigns: Oct'))), size=4, angle=90, hjust =-1, vjust=1.5, color="#198a00ff")+
  annotate("text", x = as.POSIXct("2020-12-01"), y = 0, label = substitute(paste(bold('ITNS Mass Dist.Campaigns: Dec'))), size=4, angle=90, hjust =-1, vjust=2.5, color="#198a00ff") 

# + transition_reveal(year) + base
# animate(mal5plt, height = 800, width =1000)

mal5plt

# anim_save("C:/Users/NyimbiliShida/Documents/MSI/GIS & Visuals/R Data/Visuals Exports/smooth-animation-Malaria-age-group.gif",
#           device="gif",
#           type="cairo",
#           height = 800,
#           width=1000)

ggsave("C:/Users/NyimbiliShida/Documents/MSI/GIS & Visuals/R Data/Visuals Exports/Nchelenge confirmed By Age group.png",
        device="png",
        type="cairo",
        height=7,
        width=13)

##Malaria Incidence Nchelenge

malin <- read_xls("C:/Users/NyimbiliShida/Documents/MSI/GIS & Visuals/R Data/Data PMI/Malariaincidence.xls")

malin$year <- as.Date(malin$year)

malin_data <- malin

sapply(malin, mode)
malin

malin %>% 
  convert(int(Incidence_all_ages, Incidence_rate_under5, Incidence_rate_above5))

inc <- ggplot(malin,aes(x=year))+
  geom_line(aes(y=Incidence_rate_under5, color="Incidence rate under5"), size=1)+
  geom_point(aes(y=Incidence_rate_under5, color="Incidence rate under5"), size=3)+
  geom_line(aes(y=Incidence_all_ages, color="Incidence all ages"), size=1)+
  geom_point(aes(y=Incidence_all_ages, color="Incidence all ages"), size=3)+
  geom_line(aes(y=Incidence_rate_above5, color="Incidence rate above5"), size=1)+
  geom_point(aes(y=Incidence_rate_above5, color="Incidence rate above5"), size=3)+
  scale_x_date(date_labels="%b %Y",date_breaks="6 months")+
  labs(color="Legend",title = "Malaria Incidence Rates - Nchelenge" , x="Months", y="Rate(%)") + 
  scale_color_manual(values = c("Incidence rate under5"="#BA0C2F", "Incidence all ages"="#205493", "Incidence rate above5"="#EF7D00"))+ base + transition_reveal(year)

inc
animate(inc, height = 800, width =1000)
# 
#Save Animation
anim_save("C:/Users/NyimbiliShida/Documents/MSI/GIS & Visuals/R Data/Visuals Exports/smooth-animation-Severe-Malaria.gif",
          device="gif",
          type="cairo",
          height = 700,
          width = 1000)

##Save Static image
ggsave("C:/Users/NyimbiliShida/Documents/MSI/GIS & Visuals/R Data/Visuals Exports/Nchelenge Malaria Incidence Rates.png",
       device="png",
       type="cairo",
       height = 7,
       width = 12)




##Heatmap Luapula Malaria Deaths By Districts

#ht <- read_xls("C:/Users/NyimbiliShida/Documents/MSI/GIS & Visuals/R Data/Data PMI/districtdeathsluapula.xls")
dt1 <- "C:/Users/NyimbiliShida/Documents/MSI/GIS & Visuals/R Data/Data PMI/districtdeathsluapula.csv"
ht <- read.csv(dt1, row.names = 1)

#d3heatmap(ht, scale = "row", dendrogram = "none",colors=usaid_palette6) #using d3heatmap

htplt <-pheatmap(ht,
         treeheight_row=0, 
         treeheight_col=0,  
         cutree_rows = 3,
         cluster_cols=FALSE,
         #main ="Nchelenge leads in Malaria Deaths",
         axis.title=element_text(size=12, family="Gill Sans Mt"),
         axis.text=element_text(size=12, family="Gill Sans Mt"),
         axis.ticks = element_blank(),
         color = colorRampPalette(brewer.pal(8,"Reds"))(400))

ggsave("C:/Users/NyimbiliShida/Documents/MSI/GIS & Visuals/R Data/Visuals Exports/heatmap Malaria Deaths1.png",
       plot = htplt,
       device="png",
       type="cairo",
       height=8,
       width=13)


ggsave("viz/Malaria/heatmap Malaria Deaths.png",
       plot = htplt,
       device="png",
       type="cairo",
       height=6,
       width=13)



##Malaria Deaths trendline
source("C:/Users/NyimbiliShida/Documents/MSI/GIS & Visuals/R Data/Scripts/r prep.R")
source("scripts/r prep.r")
library(gganimate)


dth <- read_xls("C:/Users/NyimbiliShida/Documents/MSI/GIS & Visuals/R Data/Data PMI/deaths.xls")
#dth <- melt(dth[, c(1, 2,3)], id.vars = 'Year')
dth$Year <- as.Date(dth$Year)

sapply(dth, mode)
dth

ggplot(dth,aes(x=Year))+
  geom_line(aes(y=Death_above_5, color="Death_above_5"))+
  geom_point(aes(y=Death_above_5, color="Death_above_5"))+
  geom_line(aes(y=Death_under_5, color="Death_under_5"))+
  geom_point(aes(y=Death_under_5, color="Death_under_5"))+
  scale_x_date(date_labels="%Y-%m",date_breaks="6 months")+
  labs(title="Malaria Deaths",
      x="Months",
      y="cases") + base + 
  transition_reveal(Year)
animate(iptm_plt, height = 800, width =1000)
  

ggsave("C:/Users/NyimbiliShida/Documents/MSI/GIS & Visuals/R Data/Visuals Exports/Nchelenge confirmed death11s.png",
       device="png",
       type="cairo",
       height=6,
       width=13)


##Malaria Nchelenge trend line 2014-2021 with campaigns

dat <- read_xls("C:/Users/NyimbiliShida/Documents/MSI/GIS & Visuals/R Data/Data PMI/Nchelenge Confirmed Cases.xls")
pmi_data <- dat %>%
  mutate(name2=name)
pmi_data
pmi_data %>%
  ggplot(aes(x=Year, y=Confirmed_Cases,group=name))+
  geom_line( dat=pmi_data %>% dplyr::select(-name), aes(group=name2), color="#BA0C2F", size=1, alpha=0.6) +
  geom_point(aes(color = name, group = name), color="#BA0C2F", size=3, alpha=6 ) +
  geom_vline(xintercept = c(vline1, vline2, vline3, vline4),color=c("#212721","#EF7D00", "#0067B9","#EF7D00") ,lty=c("solid", "solid", "solid", "solid") ,size=c(5,5,5,5), alpha=0.5)+
  annotate("text", x = vline1, y = 0, label = "SBC", size=2, angle=90, hjust =-10) +
  annotate("text", x = vline2, y = 0, label = "Mass ITNS Distribution", size=2, angle=90, hjust =-2)+
  annotate("text", x = vline3, y = 0, label = "ICCM", size=2, angle=90, hjust =-8) +
  annotate("text", x = vline4, y = 0, label = "Mass ITN's Distribution", size=2, angle=90, hjust =-2)+
  scale_x_continuous(breaks=2014:2021) +
  scale_y_continuous(labels=comma) + #faceted
  labs(title="Nchelenge Confirmed Cases",
       x="",
       y="cases") +
  base
  
  ggsave("C:/Users/NyimbiliShida/Documents/MSI/GIS & Visuals/R Data/Visuals Exports/Nchelenge confirmed cases trendline with IRS.png",
         device="png",
         type="cairo",
         height=4,
         width=7)
  

#Malaria Cases in Nchelenge as bars 2014-2021
  
pmi_data %>%
  ggplot(aes(x=Year, y=Confirmed_Cases)) + 
  geom_col(width=.4, fill=usaid_blue, alpha=7) +
  #geom_text(aes(label=Confirmed_Cases), vjust = 1.5, color="white", size=2) +
  scale_x_continuous(breaks=2014:2021) +
  scale_y_continuous(labels=comma) +
  labs(title="Nchelenge Confirmed Cases",
       x="",
       y="Confirmed cases") +
  base


# ggsave("viz/Malaria/Nchelenge confirmed cases bar.png",
#         device="png",
#         type="cairo",
#        height=6,
#         width=10)
ggsave("C:/Users/NyimbiliShida/Documents/MSI/GIS & Visuals/R Data/Visuals Exports/Nchelenge confirmed cases bar.png",
       device="png",
       type="cairo",
       height=4,
       width=7)


#Malaria Nchelenge Monthly trend line 2014-2021
source("scripts/r prep.r")

dat1 <- read_xls("C:/Users/NyimbiliShida/Documents/MSI/GIS & Visuals/R Data/Data PMI/Nchelenge Confirmed Cases Monthly 2014-2021.xls")

ggplot(dat1, aes(period, Malaria_Confirmed_Cases)) + geom_line()
monthly_data <- dat1 %>%

geom_line( dat1=monthly_data %>% dplyr::select(-name), aes(group=name2), color="#BA0C2F", size=1, alpha=0.6) +
geom_point(aes(color = name, group =-name), color="#BA0C2F", size=3, alpha=6 ) +
geom_vline(xintercept = c(vline1, vline2, vline3, vline4, vline5),color=c("#212721","#EF7D00", "#0067B9","#EF7D00","#198a00ff") ,lty=c("solid", "solid", "solid", "solid", "dotted") ,size=5, alpha=0.5)+
annotate("text", x = vline1, y = 0, label = "SBC", size=2, angle=90, hjust =-10) +
annotate("text", x = vline2, y = 0, label = "Mass ITNS Distribution", size=2, angle=90, hjust =-2)+
annotate("text", x = vline3, y = 0, label = "ICCM", size=2, angle=90, hjust =-8) +
annotate("text", x = vline4, y = 0, label = "Mass ITN's Distribution", size=2, angle=90, hjust =-2)+
scale_x_continuous(breaks=2014:2021) +
scale_y_continuous(labels=comma) + #faceted
labs(title="Nchelenge Confirmed Cases",
     x="",
     y="Confirmed cases") +
  base

ggsave("C:/Users/NyimbiliShida/Documents/MSI/GIS & Visuals/R Data/Visuals Exports/Nchelenge confirmed cases trendline with IRS campaigns.png",
       device="png",
       type="cairo",
       height=4,
       width=7)


##########Malaria Nchelenge Monthly trend line 2014-2021
source("scripts/r prep.r")

dat2 <- read_xls("C:/Users/NyimbiliShida/Documents/MSI/GIS & Visuals/R Data/Data PMI/Nchelenge Confirmed Cases Monthly 2014-2021.xls")
ani <- read_xls("C:/Users/NyimbiliShida/Documents/MSI/GIS & Visuals/R Data/Data PMI/insecticide.xls")

#Date conversion
dat2$period <- as.Date(dat2$period)  

da <- dat2[order(dat2$period), ]

da

ani$Start <- as.Date(ani$Start) 
ani$End <- as.Date(ani$End) 

ani1 <- ani[order(ani$Start), ]
ani1 <- ani[order(ani$End), ]


#campaigns
cm <- as.Date("2014-10-01")
cm1 <- as.Date("2020-10-01")
cm2 <- as.Date("2017-10-01")
cm3 <- as.Date("2020-12-01")
cm4 <- as.Date("2018-10-01")
cm5 <- as.Date("2016-04-01")
Start <- as.Date(NULL)
End <- as.Date(NULL)

da
sapply(da,mode)

#plot
ggplot(da,aes(x=period))+
  geom_area(aes(y=Malaria_Cases, fill="Malaria_Cases"), alpha=0.3, color="#CFCDC9")+
  geom_rect(data=ani1, aes(NULL, NULL, xmin=Start, xmax=End, fill=Inserticide),
            ymin=0,ymax=13000, colour="#CFCDC9", size=0.6, alpha=0.6, lty="twodash") +
  geom_vline(xintercept = c(cm2, cm3,cm4,cm5),color=c("#198a00ff","#198a00ff","#198a00ff","#198a00ff"), lty=c("solid", "solid","solid", "solid") ,size=3, alpha=0.5) +
  # geom_point(aes(y=Malaria_Confirmed_Cases, color="Malaria_Confirmed_Cases"))+
  scale_x_date(date_labels="%b %Y",date_breaks="8 months") +
  scale_y_continuous(labels=comma) +
  scale_fill_manual(values=c("#002A6C", "#F5B041","#6C6463", "#A7C6ED")) +
    labs(fill="IRS & Insecticide Used", title="Nchelenge Monthly Confirmed Cases with Campaigns",
         x="Period",
         y="Total Confimred Malaria Cases") + base +
  # annotate("text", x = cm, y = 0, label = substitute(paste(bold('Indoor Residual Spraying'))), size=4, angle=90, hjust =-3, vjust=-2) +
  # annotate("text", x = cm1, y = 0, label = substitute(paste(bold('Indoor Residual Spraying'))), size=4, angle=90, hjust =-3, vjust=-2) +
  annotate("text", x = cm2, y = 0, label = substitute(paste(bold('ITNS Mass Dist.Campaigns:Oct'))), size=4, angle=90, hjust =-1.1, vjust=1.5) +
  annotate("text", x = cm3, y = 0, label = substitute(paste(bold('ITNS Mass Dist.Campaigns: Dec'))), size=4, angle=90, hjust =-1.1, vjust=-0.5)+
  annotate("text", x = cm4, y = 0, label = substitute(paste(bold('Integrated community case management'))), size=4, angle=90, hjust =-0.8, vjust=1.5) +
  annotate("text", x = cm5, y = 0, label = substitute(paste(bold('Behaviour Change Communication'))), size=4, angle=90, hjust =-1, vjust=-0.5)

ggsave("C:/Users/NyimbiliShida/Documents/MSI/GIS & Visuals/R Data/Visuals Exports/Nchelenge confirmed cases trendline with monthly1.png",
       device="png",
       type="cairo",
       height=8,
       width=14)

#Malaria Area Plot wiith Campaigns

source("C:/Users/NyimbiliShida/Documents/MSI/GIS & Visuals/R Data/Scripts/r prep.R")
source("scripts/r prep.r")
library(cowplot)
library(geomtextpath)
library(writexl)


anf <- read_xls("C:/Users/NyimbiliShida/Documents/MSI/GIS & Visuals/R Data/Data PMI/hlb.xls")
ani <- read_xls("C:/Users/NyimbiliShida/Documents/MSI/GIS & Visuals/R Data/Data PMI/insecticide.xls")
ang <- read_xls("C:/Users/NyimbiliShida/Documents/MSI/GIS & Visuals/R Data/Data PMI/shakall.xls")
rf <- read_xls("C:/Users/NyimbiliShida/Documents/MSI/GIS & Visuals/R Data/Data PMI/rainfall.xls")
#dat5 <- read_xls("C:/Users/NyimbiliShida/Documents/MSI/GIS & Visuals/R Data/Data PMI/Manchene indoors.xls")
#dat6 <- read_xls("C:/Users/NyimbiliShida/Documents/MSI/GIS & Visuals/R Data/Data PMI/Manchene outdoors.xls")
#dat3<- read_xls(here("data/Malaria/shakall.xls"))
#dat2 <- read_xls(here("data/Malaria/hlb.xls"))


# Nchelenge HLC Shikapande FvG

anf <- melt(anf[, c(1, 2, 3)], id = 'period')

anf$period <- as.Date(anf$period)  

anf1 <- anf[order(anf$period), ]


ani$Start <- as.Date(ani$Start) 
ani$End <- as.Date(ani$End) 

ani1 <- ani[order(ani$Start), ]
ani1 <- ani[order(ani$End), ]


rf <- rf[order(rf$period), ]
rf1 <- rf[order(rf$period), ]


#campaigns
cm <- as.Date("2019-10-01")
cm1 <- as.Date("2020-10-01")
Start <- as.Date(NULL)
End <- as.Date(NULL)

str(anf1$period)

sapply(anf, mode)

class(period)
ani1
anf1
rf1

# write_xlsx(new_anf1,"C:/Users/NyimbiliShida/Documents/MSI/GIS & Visuals/R Data/Data PMI/dfdemo.xlsx")

pmi_1 <- ggplot(data=anf1, aes(x = period, y = value, fill = variable)) + 
  geom_area(alpha=.7) +
  #geom_line(data=rf1, aes(x = period, y = Rainfall)) + 
  geom_rect(data=ani1, aes(NULL, NULL, xmin=Start, xmax=End, fill=Inserticide),
            ymin=c(62.125, 84.3), ymax=c(220, 220), colour="#CFCDC9", size=0.6, alpha=0.5, lty="twodash") +
  scale_fill_manual(values=c("#A7C6ED","#C2113A", "#EF7D00",  "#EF7D00", "#198a00ff")) +
  scale_x_date(date_breaks="3 months", date_labels="%b %Y") +
  labs(fill="Legend:", title="Number of bites per person per Night in Sprayed and Unsprayed Areas - indoor",
       x="",
       y="Human Bite Rate") + 
  base +
  annotate("text", x = cm, y = 0, label = substitute(paste(bold('Indoor Residual Spraying'))), size=5, angle=90, hjust =-1.3, vjust=-1) +
  annotate("text", x = cm1, y = 0, label = substitute(paste(bold('Indoor Residual Spraying'))), size=5, angle=90, hjust =-1.3, vjust=-0.7)

pmi_1


ang <- melt(ang[, c(1, 2, 3)], id.vars = 'period')
ang

ang$period <- as.Date(ang$period)  

ang1 <- ang[order(ang$period), ]


pmi_2 <-ggplot(ang1, aes(x=period, y=value, fill = variable)) + 
  geom_area(alpha=.7) + 
  geom_rect(data=ani, aes(NULL,NULL,xmin=Start,xmax=End,fill=Inserticide),
           ymin=c(1.25,0.7) ,ymax=c(60,60), colour="#CFCDC9", size=0.6, alpha=0.5, lty="twodash") +
  scale_fill_manual(values=c("#A7C6ED","#C2113A", "#EF7D00", "#198a00ff")) +
  scale_x_date(date_breaks="3 months", date_labels="%b %Y")+
  labs(fill="Legend:", #title="Number of bites per person per Night in Sprayed and Unsprayed Areas",
    x="",
    y="Human Bite Rate") +
  base +
  annotate("text", x = cm, y = 0, label = substitute(paste(bold('Indoor Residual Spraying'))), size=5, angle=90, hjust =-1.1, vjust=-1) +
  annotate("text", x = cm1, y = 0, label = substitute(paste(bold('Indoor Residual Spraying'))), size=5, angle=90, hjust =-1.1, vjust=-0.7)



pmi_2


plot_grid(pmi_1, pmi_2, nrows = 2, ncol = 1)

ggsave("C:/Users/NyimbiliShida/Documents/MSI/GIS & Visuals/R Data/Visuals Exports/Nchelenge HBR All1 outdoors.png",
       #ggsave("viz/Malaria/Nchelenge HLC Manchene fg.png",
       device="png",
       #type="cairo",
       height=22,
       width=18)



#Nchelenge HLC for  Manchene FvG

dat5<- melt(dat5[, c(1, 2, 3)], id.vars = 'Year')

dat5

sapply(dat5, mode)


pmi_4 <- ggplot(dat5, aes(x=Year, y=value, fill = variable)) + 
  geom_area(alpha=.7)+
  scale_fill_manual(values=c("#002A6C","#C2113A")) +
  labs(fill="Legend:", #title="Number of bites per person per Night at Manchene (Not Sprayed)",
    x="",
    y="Human Bite Rate") +
  theme(
    axis.title.y=element_text(angle=0, hjust=2, vjust=6),
    plot.title=element_text(size=10, hjust=1.5)) + 
  #  faceted # don't use this if the plot doesn't have facet_wrap
  geom_vline(xintercept = c(as.POSIXct("2019-10-01"), as.POSIXct("2020-10-01")),
             color=c("#EF7D00","#EF7D00"),
             lty=c("1343", "1343") ,
             size=c(1,1), 
             alpha=1) + 
  #annotate("text", x = as.POSIXct("2019-10-01"), y = 0, label = substitute(paste(bold('ITNS Mass Dist. Campaigns:2019-10'))), size=3, angle=90, hjust =-0.8, vjust=-2) +
  #annotate("text", x = as.POSIXct("2020-10-01"), y = 0, label = substitute(paste(bold('ITNS Mass Dist. Campaigns:2020-10'))), size=3, angle=90, hjust =-0.7, vjust=2)+
  faceted
pmi_4

dat6 <- melt(dat6[, c(1, 2, 3)], id.vars = 'Year')

pmi_3 <-ggplot(dat6, aes(x=Year, y=value, fill = variable)) + 
  geom_area(alpha=.7)+
  scale_fill_manual(values=c("#002A6C","#C2113A")) +
  labs(fill="Legend:", title="Number of bites per person per Night at Manchene (Not Sprayed)",
       x="",
       y="Human Bite Rate") +
  theme(
    axis.title.y=element_text(angle=0, hjust=2, vjust=6),
    plot.title=element_text(size=10, hjust=1.5)) +
  #  faceted # don't use this if the plot doesn't have facet_wrap
  geom_vline(xintercept = c(as.POSIXct("2019-10-01"), as.POSIXct("2020-10-01")),
             color=c("#EF7D00","#EF7D00"),
             lty=c("1343", "1343"),
             size=c(1,1),
             alpha=1) +
  annotate("text", x = as.POSIXct("2019-10-01"), y = 0, label = substitute(paste(bold('ITNS Mass Dist. Campaigns:2019-10'))), size=3.4, angle=90, hjust =-0.9, vjust=-0.5) +
  annotate("text", x = as.POSIXct("2020-10-01"), y = 0, label = substitute(paste(bold('ITNS Mass Dist. Campaigns:2020-10'))), size=3.4, angle=90, hjust =-0.9, vjust=-0.5) +
  faceted

pmi_3

plot_grid(pmi_3, pmi_4,
          nrows = 2,
          ncol = 1
          )

ggsave("C:/Users/NyimbiliShida/Documents/MSI/GIS & Visuals/R Data/Visuals Exports/Nchelenge HLC Manchene fg.png",
#ggsave("viz/Malaria/Nchelenge HLC Manchene fg.png",
       device="png",
       #type="cairo",
       height=16,
       width=18)



