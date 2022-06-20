#Load Packages

source("C:/Users/NyimbiliShida/Documents/MSI/GIS & Visuals/R Data/Scripts/r prep.R")
source("scripts/r prep.r")




##Animated trends Malaria

iptm <- read_xls("C:/Users/NyimbiliShida/Documents/MSI/GIS & Visuals/R Data/Data PMI/ipt.xls")



# # Keep only 3 names
# don <- babynames %>% 
#   filter(name %in% c("Ashley", "Patricia", "Helen")) %>%
#   filter(sex=="F")



iptm_data$period <- as.Date(as.character(iptm$period), format = "%Y")

iptm_long <- melt(iptm_data, id = "period")
iptm_long

# Plot
iptm_plt <- ggplot(iptm_long, aes(x=period, y=value,  color=variable)) +
  geom_line(size=1, alpha=0.5) +
  geom_point(size=3) +
  scale_color_viridis(discrete = TRUE) +
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

iptm_long <- melt(iptm_data, id = "period")
gfg_plot <- ggplot(iptm_long,aes(x = period,y = value, color = variable)) +  geom_line(size=1, alpha=0.5) + geom_point(size=3)+
  scale_x_date(date_labels="%Y",date_breaks="1 year") +
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

malsv <- read_xls("C:/Users/NyimbiliShida/Documents/MSI/GIS & Visuals/R Data/Data PMI/severemalaria.xls")

lapply(malsv, na.omit)
malsv

malsv <- melt(malsv[c(1, 2,4)], id = 'period')
# 
# as.character(malsv$period)  #Fix this
# 
# str(malsv$period)
# 
# as.Date(as.character(malsv$period),format="%Y%m%d")
# as.Date(as.Date.numeric(malsv$period),origin = "2016-01-01", format = "%Y%m%d")

malsv <- ggplot(malsv, aes(x=period, y=value, fill = variable)) +
  geom_area(alpha=.7)+
  scale_fill_manual(values=c("#002A6C","#C2113A")) +
  scale_y_continuous(labels=comma) +
  scale_x_date(date_labels="%Y-%b",date_breaks="6 months")+
  labs(fill="Legend:", title="Severe Malaria and Deaths",
       x="",
       y="Cases") +
  base +
  geom_vline(xintercept = c(as.POSIXct("2018-10-01"), as.POSIXct("2017-10-01"), as.POSIXct("2020-10-01")),
             color=c("#EF7D00","#198a00ff", "#198a00ff"),
             lty=c("solid","dotted", "dotted") ,
             size=c(2,1,1),
             alpha=1) +
  annotate("text", x = as.POSIXct("2018-10-01"), y = 0, label = substitute(paste(bold('Integrated community case management: Oct'))), size=4, angle=90, hjust =-0.3, vjust=-1, color="#EF7D00")+
  annotate("text", x = as.POSIXct("2017-10-01"), y = 0, label = substitute(paste(bold('ITNS Mass Dist.Campaigns: Oct'))), size=4, angle=90, hjust =-1, vjust=-0.7, color="#198a00ff")+
  annotate("text", x = as.POSIXct("2020-12-01"), y = 0, label = substitute(paste(bold('ITNS Mass Dist.Campaigns: Dec'))), size=4, angle=90, hjust =-1, vjust=-0.5, color="#198a00ff")


malsv
ggsave("C:/Users/NyimbiliShida/Documents/MSI/GIS & Visuals/R Data/Visuals Exports/Severe & Death Cases.png",
       device="png",
       type="cairo",
       height=7,
       width=13)


##Malaria Cases -5 & 5+
mal5 <- read_xls("C:/Users/NyimbiliShida/Documents/MSI/GIS & Visuals/R Data/Data PMI/malariacasesfives.xls")

lapply(mal5, na.omit)

mal5 <- melt(mal5[, c(1, 2,3)], id = 'year')

as.Date(as.Date.numeric(mal5$year,origin = "2016-10-01"),format = "%Y%m%d")

mal5plt <- ggplot(mal5, aes(x=year, y=value, fill = variable)) +
  geom_area(alpha=.7)+
  scale_fill_manual(values=c("#002A6C","#C2113A")) +
  scale_y_continuous(labels=comma) +
  #scale_x_date(date_labels="%Y-%m",date_breaks="6 months")+
  labs(title="Malaria Cases by Age groups",
    x="",
    y="Cases") +
  faceted + 
  geom_vline(xintercept = c(as.POSIXct("2016-10-01"), as.POSIXct("2017-10-01"), as.POSIXct("2017-09-01"), as.POSIXct("2018-10-01"), as.POSIXct("2019-10-01"), as.POSIXct("2020-10-01"), as.POSIXct("2020-12-01"),  as.POSIXct("2021-10-01")),
             color=c("#EF7D00","#198a00ff","#EF7D00","#EF7D00","#EF7D00","#EF7D00","#198a00ff","#EF7D00"),
             lty=c("1343","solid", "1343", "1343", "1343", "1343", "solid",  "1343") ,
             size=c(1,1.5,1,1,1,1,1.5,1),
             alpha=1) +
  annotate("text", x = as.POSIXct("2016-10-01"), y = 0, label = substitute(paste(bold('IRS Campaigns: Oct'))), size=4, angle=90, hjust =-2.5, vjust=-0.6, color="#EF7D00") +
  annotate("text", x = as.POSIXct("2017-10-01"), y = 0, label = substitute(paste(bold('ITNS Mass Dist.Campaigns: Oct'))), size=4, angle=90, hjust =-1, vjust=1.5, color="#198a00ff")+
  annotate("text", x = as.POSIXct("2020-12-01"), y = 0, label = substitute(paste(bold('ITNS Mass Dist.Campaigns: Dec'))), size=4, angle=90, hjust =-1, vjust=2.5, color="#198a00ff")

mal5plt

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

malin %>% 
  convert(int(Incidence_all_ages, Incidence_rate_under5, Incidence_rate_above5))

ggplot(malin,aes(x=year))+
  geom_line(aes(y=Incidence_rate_under5, color="Incidence_rate_under5"))+
  geom_point(aes(y=Incidence_rate_under5, color="Incidence_rate_under5"))+
  geom_line(aes(y=Incidence_all_ages, color="Incidence_all_ages"))+
  geom_point(aes(y=Incidence_all_ages, color="Incidence_all_ages"))+
  geom_line(aes(y=Incidence_rate_above5, color="Incidence_rate_above5"))+
  geom_point(aes(y=Incidence_rate_above5, color="Incidence_rate_above5"))+
  scale_x_date(date_labels="%Y-%m",date_breaks="6 months")+
  labs(x="Months", y="Rate") + 
  scale_color_manual(values = c("Incidence_rate_under5"="#BA0C2F", "Incidence_all_ages"="#205493", "Incidence_rate_above5"="#EF7D00"))+
  faceted

ggsave("C:/Users/NyimbiliShida/Documents/MSI/GIS & Visuals/R Data/Visuals Exports/Nchelenge Malaria Incidence Rates all.png",
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
      y="cases")+
  faceted
  

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
# dat2 <- melt(dat1[, c(1, 2)], id.vars = 'monthyr')

# dat2$monthyr <- as.Date(dat2$monthyr)                 
# dat2 <- dat2[order(dat1$monthyr), ]

dat2
sapply(dat2,mode)

ggplot(dat2,aes(x=monthyr))+
  geom_line(aes(y=Malaria_Confirmed_Cases, color="Malaria_Confirmed_Cases"))+
  geom_point(aes(y=Malaria_Confirmed_Cases, color="Malaria_Confirmed_Cases"))+
  #scale_x_date(date_labels="%Y-%m",date_breaks="6 months")+
  scale_color_manual(values ="#205493")+
    labs(title="Nchelenge Trend Monthly Confirmed Cases with Campaigns",
         x="Months",
         y="cases")+
  faceted +
geom_vline(xintercept = c(as.POSIXct("2014-10-01"), as.POSIXct("2015-10-01"), as.POSIXct("2016-10-01"), as.POSIXct("2017-10-01"), as.POSIXct("2017-09-01"), as.POSIXct("2018-10-01"), as.POSIXct("2019-10-01"), as.POSIXct("2020-10-01"), as.POSIXct("2020-12-01"),  as.POSIXct("2021-10-01")),
           color=c("#EF7D00","#EF7D00","#EF7D00","#198a00ff","#EF7D00","#EF7D00","#EF7D00","#EF7D00","#198a00ff","#EF7D00"),
           lty=c("1343","1343","1343","solid", "1343", "1343", "1343", "1343", "solid",  "1343") ,
           size=c(1,1,1,1.5,1,1,1,1,1.5,1),
           alpha=1) +
  annotate("text", x = as.POSIXct("2014-10-01"), y = 0, label = substitute(paste(bold('IRS Campaigns: Oct'))), size=4, angle=90, hjust =-2.5, vjust=-0.6, color="#EF7D00") +
  annotate("text", x = as.POSIXct("2017-10-01"), y = 0, label = substitute(paste(bold('ITNS Mass Dist.Campaigns: Oct'))), size=4, angle=90, hjust =-1, vjust=1.5, color="#198a00ff")+
  annotate("text", x = as.POSIXct("2020-12-01"), y = 0, label = substitute(paste(bold('ITNS Mass Dist.Campaigns: Dec'))), size=4, angle=90, hjust =-1, vjust=2.5, color="#198a00ff")

# ggplot(dat2, aes(x=monthyr, y=value)) +
#   geom_line(size=1, color="#A7C6ED", alpha=0.6) +
#   geom_point(size=3, color="#0067B9", alpha=0.8)+
#   scale_x_date(date_labels="%Y-%m",date_breaks="6 months")+
#   #scale_x_continuous(breaks = "") +
#   scale_y_continuous(labels=comma) +
#   labs(title="Nchelenge Trend Monthly Confirmed Cases",
#        x="Month",
#        y="Cases") +
#   base
  #geom_vline(xintercept = c(as.POSIXct("2014-10-01"), as.POSIXct("2020-10-01")),color=c("#EF7D00","#EF7D00"), lty=c("solid", "solid") , size=c(1,1),  alpha=1)
            
  #annotate("text", x = as.POSIXct("2019-10-01"), y = 0, label = substitute(paste(bold('ITNS Mass Dist. Campaigns:2019-10'))), size=3, angle=90, hjust =-0.8, vjust=-2) +
  #annotate("text", x = as.POSIXct("2020-10-01"), y = 0, label = substitute(paste(bold('ITNS Mass Dist. Campaigns:2020-10'))), size=3, angle=90, hjust =-0.7, vjust=2)+
  #faceted

ggsave("C:/Users/NyimbiliShida/Documents/MSI/GIS & Visuals/R Data/Visuals Exports/Nchelenge confirmed cases trendline with monthly1.png",
       device="png",
       type="cairo",
       height=8,
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
dat4

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

plot_grid(pmi_2, pmi_1, nrows=2, ncol=1)

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



