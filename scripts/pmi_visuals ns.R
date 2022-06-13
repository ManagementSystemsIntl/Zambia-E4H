#Packages

source("C:/Users/NyimbiliShida/Documents/MSI/GIS & Visuals/R Data/Scripts/r prep.R")
source("scripts/r prep.r")

#Malaria Deaths
dth <- read_xls("C:/Users/NyimbiliShida/Documents/MSI/GIS & Visuals/R Data/Data PMI/deaths.xls")
#dth <- melt(dth[, c(1, 2,3)], id.vars = 'Year')
dth$Year <- as.Date(dth$Year)             

dt<-dth
dt %>%
  ggplot() +
  scale_x_date(date_labels="%Y-%m",date_breaks="6 months")+
  geom_line(aes(x=Year, y=Death_above_5),size=1, color="#A7C6ED", alpha=2) +
  geom_point(aes(x=Year, y=Death_above_5), size=2, color="#0067B9", alpha=0.8)+
  geom_line(aes(x=Year, y=Death_under_5),size=1, color="#BA0C2F", alpha=0.6)+
  geom_point(aes(x=Year, y=Death_under_5),size=2, color="#BA0C2F", alpha=0.8)+
  labs(title="Malaria Deaths",
       x="",
       y="cases")+ base

ggsave("C:/Users/NyimbiliShida/Documents/MSI/GIS & Visuals/R Data/Visuals Exports/Nchelenge confirmed deaths.png",
       device="png",
       type="cairo",
       height=4,
       width=9)


#Malaria Nchelenge trend line 2014-2021 with campaigns

dat <- read_xls("C:/Users/NyimbiliShida/Documents/MSI/GIS & Visuals/R Data/Data PMI/Nchelenge Confirmed Cases.xls")
pmi_data <- dat %>%
  mutate(name2=name)

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

dat1 <- read_xls("C:/Users/NyimbiliShida/Documents/MSI/GIS & Visuals/R Data/Data PMI/Nchelenge Confirmed Cases Monthly 2014-2021.xls")

monthly_data <- dat1 %>%
  
geom_line( dat1=monthly_data %>% dplyr::select(-name), aes(group=name2), color="#BA0C2F", size=1, alpha=0.6) +
geom_point(aes(color = name, group =-name), color="#BA0C2F", size=3, alpha=6 ) +
# geom_vline(xintercept = c(vline1, vline2, vline3, vline4, vline5),color=c("#212721","#EF7D00", "#0067B9","#EF7D00","#198a00ff") ,lty=c("solid", "solid", "solid", "solid", "dotted") ,size=5, alpha=0.5)+
# annotate("text", x = vline1, y = 0, label = "SBC", size=2, angle=90, hjust =-10) +
# annotate("text", x = vline2, y = 0, label = "Mass ITNS Distribution", size=2, angle=90, hjust =-2)+
# annotate("text", x = vline3, y = 0, label = "ICCM", size=2, angle=90, hjust =-8) +
# annotate("text", x = vline4, y = 0, label = "Mass ITN's Distribution", size=2, angle=90, hjust =-2)+
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


#Malaria Nchelenge Monthly trend line 2014-2021

dat2 <- read_xls("C:/Users/NyimbiliShida/Documents/MSI/GIS & Visuals/R Data/Data PMI/Nchelenge Confirmed Cases Monthly 2014-2021.xls")
dat2 <- melt(dat1[, c(1, 2)], id.vars = 'monthyr')

dat1$monthyr <- as.Date(dat1$monthyr)                 
dat2 <- dat2[order(dat1$monthyr), ]

dat2
sapply(dat1,mode)

# dat1$monthyr <- as.Date(as.yearmon(dat1$monthyr))
# df$monthyr <- as.Date(as.yearqtr(df$monthyr))

#monthyr = as.Date.character(monthyr,"%Y/%m/%d")
ggplot(dat2, aes(x=monthyr, y=value)) +
  geom_line(size=1, color="#A7C6ED", alpha=0.6) +
  geom_point(size=3, color="#0067B9", alpha=0.8)+
  #geom_vline()+
  scale_x_date(date_labels="%Y-%m",date_breaks="6 months")+
  #geom_text(aes(label=Malaria_Confirmed_Cases), vjust = 1.5, color="white", size=2) +
  # scale_x_continuous(breaks = "monthyr") +
  scale_y_continuous(labels=comma) +
  labs(title="Nchelenge Trend Monthly Confirmed Cases",
       x="",
       y="Cases") +
  base +
  #annotate("text", x = as.POSIXct("2019-10-01"), y = 0, label = substitute(paste(bold('ITNS Mass Dist. Campaigns:2019-10'))), size=3, angle=90, hjust =-0.8, vjust=-2) +
  #annotate("text", x = as.POSIXct("2020-10-01"), y = 0, label = substitute(paste(bold('ITNS Mass Dist. Campaigns:2020-10'))), size=3, angle=90, hjust =-0.7, vjust=2)+
  #faceted

ggsave("C:/Users/NyimbiliShida/Documents/MSI/GIS & Visuals/R Data/Visuals Exports/Nchelenge confirmed cases trendline with monthly1.png",
       device="png",
       type="cairo",
       height=6,
       width=14)

#Malaria Area Plot wiith Campaigns

dat3 <- read_xls("C:/Users/NyimbiliShida/Documents/MSI/GIS & Visuals/R Data/Data PMI/hlb.xls")
dat4 <- read_xls("C:/Users/NyimbiliShida/Documents/MSI/GIS & Visuals/R Data/Data PMI/shakall.xls")
dat5 <- read_xls("C:/Users/NyimbiliShida/Documents/MSI/GIS & Visuals/R Data/Data PMI/Manchene indoors.xls")
dat6 <- read_xls("C:/Users/NyimbiliShida/Documents/MSI/GIS & Visuals/R Data/Data PMI/Manchene outdoors.xls")
#dat3<- read_xls(here("data/Malaria/shakall.xls"))
#dat2 <- read_xls(here("data/Malaria/hlb.xls"))

dat3
dat4
dat5
dat6

# dat <- dat2 %>%
#   pivot_longer(2:3,
#                names_to="variable",
#                values_to="value")

# Nchelenge HLC Shikapande FvG

dat3<- melt(dat3[, c(1, 2, 3)], id.vars = 'Year')

dat3

sapply(dat3, mode)


pmi_1 <- ggplot(dat3, aes(x=Year, y=value, fill = variable)) + 
  geom_area(alpha=.7)+
  scale_fill_manual(values=c("#002A6C","#C2113A")) +
  labs(#title="Number of bites per person per Night at Manchene (Not Sprayed)",
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
pmi_1

dat4 <- melt(dat4[, c(1, 2, 3)], id.vars = 'Year')

pmi_2 <-ggplot(dat4, aes(x=Year, y=value, fill = variable)) + 
  geom_area(alpha=.7)+
  scale_fill_manual(values=c("#002A6C","#C2113A")) +
  labs(title="Number of bites per person per Night at Shikapande(Sprayed)",
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
  annotate("text", x = as.POSIXct("2019-10-01"), y = 0, label = substitute(paste(bold('ITNS Mass Dist. Campaigns:2019-10'))), size=3.4, angle=90, hjust =-0.9, vjust=-0.5) +
  annotate("text", x = as.POSIXct("2020-10-01"), y = 0, label = substitute(paste(bold('ITNS Mass Dist. Campaigns:2020-10'))), size=3.4, angle=90, hjust =-0.9, vjust=-0.5)+
  faceted

pmi_2

plot_grid(pmi_2, pmi_1, nrows= 2, ncol= 1)

ggsave("C:/Users/NyimbiliShida/Documents/MSI/GIS & Visuals/R Data/Visuals Exports/Nchelenge HLC Shikapande fg.png",
       #ggsave("viz/Malaria/Nchelenge HLC Manchene fg.png",
       device="png",
       #type="cairo",
       height=16,
       width=18)



#Nchelenge HLC for  Manchene FvG

dat5<- melt(dat5[, c(1, 2, 3)], id.vars = 'Year')

dat5

sapply(dat5, mode)


pmi_4 <- ggplot(dat5, aes(x=Year, y=value, fill = variable)) + 
  geom_area(alpha=.7)+
  scale_fill_manual(values=c("#002A6C","#C2113A")) +
  labs(#title="Number of bites per person per Night at Manchene (Not Sprayed)",
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
  labs(title="Number of bites per person per Night at Manchene (Not Sprayed)",
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
  annotate("text", x = as.POSIXct("2019-10-01"), y = 0, label = substitute(paste(bold('ITNS Mass Dist. Campaigns:2019-10'))), size=3.4, angle=90, hjust =-0.9, vjust=-0.5) +
  annotate("text", x = as.POSIXct("2020-10-01"), y = 0, label = substitute(paste(bold('ITNS Mass Dist. Campaigns:2020-10'))), size=3.4, angle=90, hjust =-0.9, vjust=-0.5)+
  faceted

pmi_3

plot_grid(pmi_3, pmi_4, nrows= 2, ncol= 1)

ggsave("C:/Users/NyimbiliShida/Documents/MSI/GIS & Visuals/R Data/Visuals Exports/Nchelenge HLC Manchene fg.png",
#ggsave("viz/Malaria/Nchelenge HLC Manchene fg.png",
       device="png",
       #type="cairo",
       height=16,
       width=18)



