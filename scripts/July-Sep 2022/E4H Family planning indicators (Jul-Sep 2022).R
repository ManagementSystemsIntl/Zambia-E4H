# Zambia E4 Health
# Family planning indicators
# July - September 2022

# prep ---- 

source("scripts/r prep.r")

fp <- read_excel("data/July-Sep 2022/Family planning/Family Planning (July 2018 - Sep 2022 quarterly).xlsx",
                 sheet="export",
                 range="A2:AY21") %>%
  mutate(yrqtr = as.yearqtr(date),
         yrqtr = yearquarter(date, fiscal_start=10),
         qtr = round(quarter(date, with_year = T),2),
         year=year(date),
         fisc=fiscal_year(yrqtr))


?ymd

names(fp)
str(fp)

frq(fp$qtr)
frq(fp$fisc)

?fiscal_year

fp[,4:51] <- map(fp[,4:51], as.numeric)

varlabs <- read_xlsx("data/July-Sep 2022/Family Planning (July 2018 - Sep 2022 quarterly).xlsx",
                     sheet="export",
                     range='A1:AY1',
                     col_names = F) %>%
  t()

varlabs


fpDict <- data.frame(var=names(fp),
                      varlab = c(varlabs, "Date", "Quarter","Year", "Fiscal year")) %>%
  mutate(class=lapply(fp, class)) %>%
  remove_rownames(.)

fp_prov <- read_excel("data/July-Sep 2022/Family planning/Family Planning Monthly data by prov.xlsx",
                 sheet="export",
                 range="A2:AO132") %>%
  mutate(date=mdy(paste(month, "01", year, sep=""))) %>%
  filter(year==2022)

names(fp_prov)

# Women visited by CHA (visited) ---- 

describe(fp$visited)
frq(fp$fisc)


ggplot(fp, aes(yrqtr, visited)) + 
  geom_point(color=usaid_blue, size=2) + 
  geom_line(color=usaid_blue, size=.4) +
  stat_smooth(data=filter(fp, fisc==2022), 
              aes(y=visited), 
              method="lm", se=F, color=usaid_blue, alpha=.4) +
  scale_x_yearquarter(date_labels="%Y") +
  scale_y_continuous(limits=c(6273,8.0315e4),
                     breaks=seq(0,8e4, 1e4),
                     labels=comma,
                     sec.axis = dup_axis()) +
  labs(x="\nJan 2018 - Sep 2022",
       y="CHA\nvisits\n",
       title="FY2022 on an increasing trend") +
  theme(axis.title.y.left=element_text(angle=0, vjust=.5),
        axis.title.y.right=element_blank(),
        axis.text.y.left=element_blank())


ggsave("viz/Jul-Sep 2022/Family planning/CHA visits (Jan 2018 - Sep 2022).png",
       height=5.3,
       width=7.1)

# Women on modern family planning method (fp) ---- 

describe(fp$fp)
frq(fp$fisc)

ggplot(fp, aes(yrqtr, fp)) + 
  geom_point(color=usaid_blue, size=2) + 
  geom_line(color=usaid_blue, size=.4) +
  stat_smooth(data=filter(fp, fisc==2022), 
              aes(y=fp), 
              method="lm", se=F, color=usaid_blue, alpha=.4) +
  scale_x_yearquarter(date_labels="%Y") +
  scale_y_continuous(limits=c(2862,4.2e4),
                     breaks=seq(0,4.2e4, .5e4),
                     labels=comma,
                     sec.axis = dup_axis()) +
  labs(x="\nJan 2018 - Sep 2022",
       y="Modern\nfamily\nplanning\nmethod",
       title="FY2022 on a flat/decreasing trend") +
  theme(axis.title.y.left=element_text(angle=0, vjust=.5),
        axis.title.y.right=element_blank(),
        axis.text.y.left=element_blank())


ggsave("viz/Jul-Sep 2022/Family planning/Women on modern family planning method (Jan 2018 - Sep 2022).png",
       height=5.3,
       width=7.1)


# Clients accessing contraceptives (contra) ---- 

describe(fp$contra)

ggplot(fp, aes(yrqtr, contra/100)) + 
  geom_point(color=usaid_blue, size=2) + 
  geom_line(color=usaid_blue, size=.4) +
  stat_smooth(data=filter(fp, fisc==2022), 
              aes(y=contra/100), 
              method="lm", se=F, color=usaid_blue, alpha=.4) +
  scale_x_yearquarter(date_labels="%Y") +
  scale_y_continuous(limits=c(.025,.15),
                     breaks=seq(.025,.15, .025),
                     labels=percent,
                     sec.axis = dup_axis()) +
  labs(x="\nJan 2018 - Sep 2022",
       y="Accessing\ncontraceptives",
       title="FY2022 on a flat/decreasing trend") +
  theme(axis.title.y.left=element_text(angle=0, vjust=.5),
        axis.title.y.right=element_blank(),
        axis.text.y.left=element_blank())


ggsave("viz/Jul-Sep 2022/Family planning/Women accessing contraceptives (Jan 2018 - Sep 2022).png",
       height=5.3,
       width=7.1)


# New family planning acceptors (acceptors) ---- 

describe(fp$acceptors)

ggplot(fp, aes(yrqtr, acceptors/100)) + 
  geom_point(color=usaid_blue, size=2) + 
  geom_line(color=usaid_blue, size=.4) +
  stat_smooth(data=filter(fp, fisc==2022), 
              aes(y=acceptors/100), 
              method="lm", se=F, color=usaid_blue, alpha=.4) +
  scale_x_yearquarter(date_labels="%Y") +
  scale_y_continuous(limits=c(0,.3),
                     breaks=seq(0,.3, .05),
                     labels=percent,
                     sec.axis = dup_axis()) +
  labs(x="\nJan 2018 - Sep 2022",
       y="Accessing\ncontraceptives",
       title="FY2022 on a flat/decreasing trend") +
  theme(axis.title.y.left=element_text(angle=0, vjust=.5),
        axis.title.y.right=element_blank(),
        axis.text.y.left=element_blank())


ggsave("viz/Jul-Sep 2022/Family planning/Women accessing contraceptives (Jan 2018 - Sep 2022).png",
       height=5.3,
       width=7.1)




# fp removed ---- 

rem <- fp %>%
  select(yrqtr, fisc,
         iucd_rem:impl_rem) %>%
  pivot_longer(3:4,
               names_to="type",
               values_to="num")

rem

describe(rem$num)

labs <- c("Implants removed", "IUCDs removed")
names(labs) <- c("impl_rem","iucd_rem")

labs

ggplot(rem, aes(yrqtr, num, color=type)) + 
  geom_point(size=2) + 
  geom_line(size=.4) +
  stat_smooth(data=filter(rem, fisc==2022), 
              aes(y=num), 
              method="lm", se=F, alpha=.4) +
  scale_x_yearquarter(date_labels="%Y") +
  scale_y_continuous(sec.axis = dup_axis(),
                     labels=comma) +
  facet_wrap(~type, ncol=1, 
             scales="free_y",
             labeller=labeller(type=labs)) + 
  faceted  +
  theme(legend.position="none",
        axis.text.y.right=element_blank(),
        axis.ticks.y.right=element_blank(),
        axis.title.y.left=element_blank(),
        axis.title.y.right=element_text(angle=0, vjust=.5)) +
  labs(x="\nJan 2018 - Sep 2022",
       y="Number\nremoved",
       title="Implants removed on an increasing trend\nIUCDs removed on a decreasing trend")


ggsave("viz/Jul-Sep 2022/Family planning/Contraceptives removed (Jan 2018 - Sep 2022).png",
       height=5.3,
       width=7.1)



#implant_remove2

describe(fp_prov$implant_remove2)


ggplot(fp_prov, aes(date, implant_remove2)) + 
  geom_point(size=.8) + 
  geom_line(size=.4, alpha=.4) + 
  stat_smooth(method="lm", se=F, alpha=.8) + 
  facet_wrap(~prov, ncol=5) + 
  scale_color_viridis_d() +
  faceted +
  theme(legend.position="none",
        axis.title.y.left=element_blank(),
        axis.title.y.right=element_text(angle=0, vjust=.5)) +
  #axis.text.y.left=element_blank(),
  #axis.ticks.y.left=element_blank()) +
  scale_x_date(date_labels="%b") +
  scale_y_continuous(breaks=seq(0,1000,200),
                     sec.axis = dup_axis()) +
  labs(x="2022",
       y="",
       #y="Coverage\nrate",
       title=#"Number of Implants Removed" 
         "Increasing trend in Central, Copperbelt, Eastern, Luapula, Lusaka, Northern, Southern
       No trend in Muchinga, Western, Northwestern",
       caption="Number of Implants Removed")

ggsave("viz/Jul-Sep 2022/Family planning/Implants removed (2022).png",
       height=5.3,
       width=7)


#fp_accept_1519


describe(fp_prov$fp_accept_1519)


ggplot(fp_prov, aes(date, fp_accept_1519)) + 
  geom_point(size=.8) + 
  geom_line(size=.4, alpha=.4) + 
  stat_smooth(method="lm", se=F, alpha=.8) + 
  facet_wrap(~prov, ncol=5) + 
  scale_color_viridis_d() +
  faceted +
  theme(legend.position="none",
        axis.title.y.left=element_blank(),
        axis.title.y.right=element_text(angle=0, vjust=.5)) +
  #axis.text.y.left=element_blank(),
  #axis.ticks.y.left=element_blank()) +
  scale_x_date(date_labels="%b") +
  scale_y_continuous(breaks=seq(0,2400,400),
                     sec.axis = dup_axis()) +
  labs(x="2022",
       y="",
       #y="Coverage\nrate",
       title=#"New Acceptors (15-19 years old)" 
         "Increasing trend in Central, Copperbelt, Eastern, Luapula, Lusaka, 
         Muchinga, Northern, Southern, Western
       Decreasing trend in Northwestern",
       caption="New Acceptors (15-19 years old)")

ggsave("viz/Jul-Sep 2022/Family planning/New acceptors (15-19 years old).png",
       height=5.3,
       width=7)


#fp_accept_2024


describe(fp_prov$fp_accept_2024)


ggplot(fp_prov, aes(date, fp_accept_2024)) + 
  geom_point(size=.8) + 
  geom_line(size=.4, alpha=.4) + 
  stat_smooth(method="lm", se=F, alpha=.8) + 
  facet_wrap(~prov, ncol=5) + 
  scale_color_viridis_d() +
  faceted +
  theme(legend.position="none",
        axis.title.y.left=element_blank(),
        axis.title.y.right=element_text(angle=0, vjust=.5)) +
  #axis.text.y.left=element_blank(),
  #axis.ticks.y.left=element_blank()) +
  scale_x_date(date_labels="%b") +
  scale_y_continuous(breaks=seq(0,3000,600),
                     sec.axis = dup_axis()) +
  labs(x="2022",
       y="",
       #y="Coverage\nrate",
       title=#"New Acceptors (20-24 years old)" 
         "Increasing trend in Central, Copperbelt, Luapula, Lusaka, Northern, Southern, Western
        No trend in Eastern, Muchinga
       Decreasing trend in Northwestern",
       caption="New Acceptors (20-24 years old)")

ggsave("viz/Jul-Sep 2022/Family planning/New acceptors (20-24 years old).png",
       height=5.3,
       width=7)


#fp_accept_ov25

describe(fp_prov$fp_accept_ov25)


ggplot(fp_prov, aes(date, fp_accept_ov25)) + 
  geom_point(size=.8) + 
  geom_line(size=.4, alpha=.4) + 
  stat_smooth(method="lm", se=F, alpha=.8) + 
  facet_wrap(~prov, ncol=5) + 
  scale_color_viridis_d() +
  faceted +
  theme(legend.position="none",
        axis.title.y.left=element_blank(),
        axis.title.y.right=element_text(angle=0, vjust=.5)) +
  #axis.text.y.left=element_blank(),
  #axis.ticks.y.left=element_blank()) +
  scale_x_date(date_labels="%b") +
  scale_y_continuous(breaks=seq(0,4000,600),
                     sec.axis = dup_axis()) +
  labs(x="2022",
       y="",
       #y="Coverage\nrate",
       title=#"New Acceptors (Over 25 years old)" 
         "Increasing trend in Central, Copperbelt, Lusaka, Southern, Western
        No trend in Muchinga, Northern
       Decreasing trend in Eastern, Luapula, Northwestern",
       caption="New Acceptors (Over 25 years old)")

ggsave("viz/Jul-Sep 2022/Family planning/New acceptors (Over 25 years old).png",
       height=5.3,
       width=7)


#fp_accept_u15

describe(fp_prov$fp_accept_u15)


ggplot(fp_prov, aes(date, fp_accept_u15)) + 
  geom_point(size=.8) + 
  geom_line(size=.4, alpha=.4) + 
  stat_smooth(method="lm", se=F, alpha=.8) + 
  facet_wrap(~prov, ncol=5) + 
  scale_color_viridis_d() +
  faceted +
  theme(legend.position="none",
        axis.title.y.left=element_blank(),
        axis.title.y.right=element_text(angle=0, vjust=.5)) +
  #axis.text.y.left=element_blank(),
  #axis.ticks.y.left=element_blank()) +
  scale_x_date(date_labels="%b") +
  scale_y_continuous(breaks=seq(0,320,80),
                     sec.axis = dup_axis()) +
  labs(x="2022",
       y="",
       #y="Coverage\nrate",
       title=#"New Acceptors (Under 15 years old)" 
         "Increasing trend in Southern, Western
        No trend in Eastern, Luapula, Lusaka, Muchinga, Northwestern
       Decreasing trend in Central, Copperbelt, Northern",
       caption="New Acceptors (Under 15 years old)")

ggsave("viz/Jul-Sep 2022/Family planning/New acceptors (Under 15 years old).png",
       height=5.3,
       width=7)


#iucd_remove

describe(fp_prov$iucd_remove)


ggplot(fp_prov, aes(date, iucd_remove)) + 
  geom_point(size=.8) + 
  geom_line(size=.4, alpha=.4) + 
  stat_smooth(method="lm", se=F, alpha=.8) + 
  facet_wrap(~prov, ncol=5) + 
  scale_color_viridis_d() +
  faceted +
  theme(legend.position="none",
        axis.title.y.left=element_blank(),
        axis.title.y.right=element_text(angle=0, vjust=.5)) +
  #axis.text.y.left=element_blank(),
  #axis.ticks.y.left=element_blank()) +
  scale_x_date(date_labels="%b") +
  scale_y_continuous(breaks=seq(0,250,50),
                     sec.axis = dup_axis()) +
  labs(x="2022",
       y="",
       #y="Coverage\nrate",
       title=#"IUCDs Removed" 
         "Increasing trend in Copperbelt, Lusaka
        No trend in Central, Eastern, Luapula, Muchinga, Northwestern, Southern
       Decreasing trend in Northern, Western",
       caption="IUCDs Removed")

ggsave("viz/Jul-Sep 2022/Family planning/IUCDs Removed.png",
       height=5.3,
       width=7)


#hts_screen

describe(fp_prov$hts_screen)


ggplot(fp_prov, aes(date, hts_screen)) + 
  geom_point(size=.8) + 
  geom_line(size=.4, alpha=.4) + 
  stat_smooth(method="lm", se=F, alpha=.8) + 
  facet_wrap(~prov, ncol=5) + 
  scale_color_viridis_d() +
  faceted +
  theme(legend.position="none",
        axis.title.y.left=element_blank(),
        axis.title.y.right=element_text(angle=0, vjust=.5)) +
  #axis.text.y.left=element_blank(),
  #axis.ticks.y.left=element_blank()) +
  scale_x_date(date_labels="%b") +
  scale_y_continuous(breaks=seq(0,25000,5000),
                     sec.axis = dup_axis()) +
  labs(x="2022",
       y="",
       #y="Coverage\nrate",
       title=#"Number Screened for HTS Needs" 
         "Increasing trend in Lusaka
        No trend in Central, Copperbelt, Luapula, Muchinga, 
        Northern, Northwestern, Southern, Western
       Decreasing trend in Eastern",
       caption="Number Screened for HTS Needs")

ggsave("viz/Jul-Sep 2022/Family planning/Screened for HTS Needs.png",
       height=5.3,
       width=7)



#prop_restart

describe(fp_prov$prop_restart)


ggplot(fp_prov, aes(date, prop_restart/100)) + 
  geom_point(size=.8) + 
  geom_line(size=.4, alpha=.4) + 
  stat_smooth(method="lm", se=F, alpha=.8) + 
  facet_wrap(~prov, ncol=5) + 
  scale_color_viridis_d() +
  faceted +
  theme(legend.position="none",
        axis.title.y.left=element_blank(),
        axis.title.y.right=element_text(angle=0, vjust=.5)) +
  #axis.text.y.left=element_blank(),
  #axis.ticks.y.left=element_blank()) +
  scale_x_date(date_labels="%b") +
  scale_y_continuous(breaks=seq(0,.6,.1),
                     labels=percent,
                     sec.axis = dup_axis()) +
  labs(x="\n2022",
       y="",
       #y="Coverage\nrate",
       title=#"Proportion of Clients Restarting Family Planning" 
         "No trend in Copperbelt, Luapula, Muchinga, Northern, Northwestern, Western
       Decreasing trend in Central, Eastern, Lusaka, Southern",
       caption="Proportion of Clients Restarting Family Planning (considering outliers)")

ggsave("viz/Jul-Sep 2022/Family planning/Proportion Restarting FP.png",
       height=5.3,
       width=7)



#prop_new_1519

describe(fp_prov$prop_new_1519)


ggplot(fp_prov, aes(date, prop_new_1519/100)) + 
  geom_point(size=.8) + 
  geom_line(size=.4, alpha=.4) + 
  stat_smooth(method="lm", se=F, alpha=.8) + 
  facet_wrap(~prov, ncol=5) + 
  scale_color_viridis_d() +
  faceted +
  theme(legend.position="none",
        axis.title.y.left=element_blank(),
        axis.title.y.right=element_text(angle=0, vjust=.5)) +
  #axis.text.y.left=element_blank(),
  #axis.ticks.y.left=element_blank()) +
  scale_x_date(date_labels="%b") +
  scale_y_continuous(breaks=seq(0,.75,.15),
                     labels=percent,
                     sec.axis = dup_axis()) +
  labs(x="\n2022",
       y="",
       #y="Coverage\nrate",
       title=#"Proportion of New Acceptors (15-19 years old)" 
         "Increasing trend in Central, Copperbelt, Eastern, Luapula, Muchinga, Southern, Western
        No trend in Lusaka, Northern
       Decreasing trend in Northwestern",
       caption="Proportion of New Acceptors (15-19 years old) (considering outliers)")

ggsave("viz/Jul-Sep 2022/Family planning/Proportion New Acceptors (15-19 years).png",
       height=5.3,
       width=7)



#new_accept

describe(fp_prov$new_accept)


ggplot(fp_prov, aes(date, new_accept/100)) + 
  geom_point(size=.8) + 
  geom_line(size=.4, alpha=.4) + 
  stat_smooth(method="lm", se=F, alpha=.8) + 
  facet_wrap(~prov, ncol=5) + 
  scale_color_viridis_d() +
  faceted +
  theme(legend.position="none",
        axis.title.y.left=element_blank(),
        axis.title.y.right=element_text(angle=0, vjust=.5)) +
  #axis.text.y.left=element_blank(),
  #axis.ticks.y.left=element_blank()) +
  scale_x_date(date_labels="%b") +
  scale_y_continuous(breaks=seq(0,.60,.10),
                     labels=percent,
                     sec.axis = dup_axis()) +
  labs(x="\n2022",
       y="",
       #y="Coverage\nrate",
       title=#"Percentage of New Acceptors" 
         "No trend in Central, Copperbelt, Eastern, Luapula, Lusaka, Northern, Southern, Western
       Decreasing trend in Muchinga, Northwestern",
       caption="Percentage New Acceptors")

ggsave("viz/Jul-Sep 2022/Family planning/Percentage New Acceptors.png",
       height=5.3,
       width=7)




#copper_insert_1519

describe(fp_prov$copper_insert_1519)


ggplot(fp_prov, aes(date, copper_insert_1519)) + 
  geom_point(size=.8) + 
  geom_line(size=.4, alpha=.4) + 
  stat_smooth(method="lm", se=F, alpha=.8) + 
  facet_wrap(~prov, ncol=5) + 
  scale_color_viridis_d() +
  faceted +
  theme(legend.position="none",
        axis.title.y.left=element_blank(),
        axis.title.y.right=element_text(angle=0, vjust=.5)) +
  #axis.text.y.left=element_blank(),
  #axis.ticks.y.left=element_blank()) +
  scale_x_date(date_labels="%b") +
  scale_y_continuous(breaks=seq(0,500,100),
                     sec.axis = dup_axis()) +
  labs(x="2022",
       y="",
       #y="Coverage\nrate",
       title=#"Copper IUCD Inserted (15-19 years old)" 
         "Increasing trend in Copperbelt
         No trend in Eastern, Lusaka, Muchinga, Northern, Northwestern, Southern, Western
       Decreasing trend in Central, Luapula",
       caption="Copper IUCD Inserted (15-19 years old)")

ggsave("viz/Jul-Sep 2022/Family planning/Copper IUCD Inserted (15-19).png",
       height=5.3,
       width=7)



#copper_insert_2024

describe(fp_prov$copper_insert_2024)


ggplot(fp_prov, aes(date, copper_insert_2024)) + 
  geom_point(size=.8) + 
  geom_line(size=.4, alpha=.4) + 
  stat_smooth(method="lm", se=F, alpha=.8) + 
  facet_wrap(~prov, ncol=5) + 
  scale_color_viridis_d() +
  faceted +
  theme(legend.position="none",
        axis.title.y.left=element_blank(),
        axis.title.y.right=element_text(angle=0, vjust=.5)) +
  #axis.text.y.left=element_blank(),
  #axis.ticks.y.left=element_blank()) +
  scale_x_date(date_labels="%b") +
  scale_y_continuous(breaks=seq(0,700,100),
                     sec.axis = dup_axis()) +
  labs(x="2022",
       y="",
       #y="Coverage\nrate",
       title=#"Copper IUCD Inserted (20-24 years old)" 
         "No trend in Eastern, Northwestern, Southern, Western
       Decreasing trend in Central, Copperbelt, Luapula, Lusaka, Muchinga, Northern",
       caption="Copper IUCD Inserted (20-24 years old)")

ggsave("viz/Jul-Sep 2022/Family planning/Copper IUCD Inserted (20-24).png",
       height=5.3,
       width=7)



#copper_insert_ov25

describe(fp_prov$copper_insert_ov25)


ggplot(fp_prov, aes(date, copper_insert_ov25)) + 
  geom_point(size=.8) + 
  geom_line(size=.4, alpha=.4) + 
  stat_smooth(method="lm", se=F, alpha=.8) + 
  facet_wrap(~prov, ncol=5) + 
  scale_color_viridis_d() +
  faceted +
  theme(legend.position="none",
        axis.title.y.left=element_blank(),
        axis.title.y.right=element_text(angle=0, vjust=.5)) +
  #axis.text.y.left=element_blank(),
  #axis.ticks.y.left=element_blank()) +
  scale_x_date(date_labels="%b") +
  scale_y_continuous(breaks=seq(0,1800,300),
                     sec.axis = dup_axis()) +
  labs(x="2022",
       y="",
       #y="Coverage\nrate",
       title=#"Copper IUCD Inserted (Over 25 years old)" 
         "No trend in Central, Eastern, Lusaka, Northwestern, Southern, Western
       Decreasing trend in Copperbelt, Luapula, Muchinga, Northern",
       caption="Copper IUCD Inserted (Over 25 years old)")

ggsave("viz/Jul-Sep 2022/Family planning/Copper IUCD Inserted (Over 25).png",
       height=5.3,
       width=7)



#copper_insert_u15

describe(fp_prov$copper_insert_u15)


ggplot(fp_prov, aes(date, copper_insert_u15)) + 
  geom_point(size=.8) + 
  geom_line(size=.4, alpha=.4) + 
  stat_smooth(method="lm", se=F, alpha=.8) + 
  facet_wrap(~prov, ncol=5) + 
  scale_color_viridis_d() +
  faceted +
  theme(legend.position="none",
        axis.title.y.left=element_blank(),
        axis.title.y.right=element_text(angle=0, vjust=.5)) +
  #axis.text.y.left=element_blank(),
  #axis.ticks.y.left=element_blank()) +
  scale_x_date(date_labels="%b") +
  scale_y_continuous(breaks=seq(0,350,50),
                     sec.axis = dup_axis()) +
  labs(x="2022",
       y="",
       #y="Coverage\nrate",
       title=#"Copper IUCD Inserted (Under 15 years old)" 
         "Increasing trend in Copperbelt
         No trend in Central, Muchinga, Northern, Northwestern, Southern, Western
       Decreasing trend in Eastern, Luapula, Lusaka",
       caption="Copper IUCD Inserted (Under 15 years old) (considering outliers)")

ggsave("viz/Jul-Sep 2022/Family planning/Copper IUCD Inserted (Under 15).png",
       height=5.3,
       width=7)


#hormone_insert_1519

describe(fp_prov$hormone_insert_1519)


ggplot(fp_prov, aes(date, hormone_insert_1519)) + 
  geom_point(size=.8) + 
  geom_line(size=.4, alpha=.4) + 
  stat_smooth(method="lm", se=F, alpha=.8) + 
  facet_wrap(~prov, ncol=5) + 
  scale_color_viridis_d() +
  faceted +
  theme(legend.position="none",
        axis.title.y.left=element_blank(),
        axis.title.y.right=element_text(angle=0, vjust=.5)) +
  #axis.text.y.left=element_blank(),
  #axis.ticks.y.left=element_blank()) +
  scale_x_date(date_labels="%b") +
  scale_y_continuous(breaks=seq(0,30,5),
                     sec.axis = dup_axis()) +
  labs(x="\n2022",
       y="",
       #y="Coverage\nrate",
       title=#"Hormone IUCD Inserted (15-19 years old)" 
         "Increasing trend in Copperbelt
         No trend in Eastern, Lusaka, Muchinga, Northern, Northwestern, Western
       Decreasing trend in Central, Luapula, Southern",
       caption="Hormone IUCD Inserted (15-19 years old)")

ggsave("viz/Jul-Sep 2022/Family planning/Hormone IUCD Inserted (15-19).png",
       height=5.3,
       width=7)



#hormone_insert_2024

describe(fp_prov$hormone_insert_2024)


ggplot(fp_prov, aes(date, hormone_insert_2024)) + 
  geom_point(size=.8) + 
  geom_line(size=.4, alpha=.4) + 
  stat_smooth(method="lm", se=F, alpha=.8) + 
  facet_wrap(~prov, ncol=5) + 
  scale_color_viridis_d() +
  faceted +
  theme(legend.position="none",
        axis.title.y.left=element_blank(),
        axis.title.y.right=element_text(angle=0, vjust=.5)) +
  #axis.text.y.left=element_blank(),
  #axis.ticks.y.left=element_blank()) +
  scale_x_date(date_labels="%b") +
  scale_y_continuous(breaks=seq(0,125,25),
                     sec.axis = dup_axis()) +
  labs(x="\n2022",
       y="",
       #y="Coverage\nrate",
       title=#"Hormone IUCD Inserted (20-24 years old)" 
         "No trend in Copperbelt, Luapula, Northwestern, Southern, Western
       Decreasing trend in Central, Eastern, Lusaka, Muchinga, Northern",
       caption="Hormone IUCD Inserted (20-24 years old) (considering outliers)")

ggsave("viz/Jul-Sep 2022/Family planning/Hormone IUCD Inserted (20-24).png",
       height=5.3,
       width=7)



#hormone_insert_ov25

describe(fp_prov$hormone_insert_ov25)


ggplot(fp_prov, aes(date, hormone_insert_ov25)) + 
  geom_point(size=.8) + 
  geom_line(size=.4, alpha=.4) + 
  stat_smooth(method="lm", se=F, alpha=.8) + 
  facet_wrap(~prov, ncol=5) + 
  scale_color_viridis_d() +
  faceted +
  theme(legend.position="none",
        axis.title.y.left=element_blank(),
        axis.title.y.right=element_text(angle=0, vjust=.5)) +
  #axis.text.y.left=element_blank(),
  #axis.ticks.y.left=element_blank()) +
  scale_x_date(date_labels="%b") +
  scale_y_continuous(breaks=seq(0,275,50),
                     sec.axis = dup_axis()) +
  labs(x="\n2022",
       y="",
       #y="Coverage\nrate",
       title=#"Hormone IUCD Inserted (Over 25 years old)" 
         "No trend in Copperbelt, Eastern, Luapula, Lusaka, Northern, Northwestern, Southern, Western
       Decreasing trend in Central, Muchinga",
       caption="Hormone IUCD Inserted (Over 25 years old) (considering outliers)")

ggsave("viz/Jul-Sep 2022/Family planning/Hormone IUCD Inserted (Over 25).png",
       height=5.3,
       width=7)



#hormone_insert_u15

describe(fp_prov$hormone_insert_u15)


ggplot(fp_prov, aes(date, hormone_insert_u15)) + 
  geom_point(size=.8) + 
  geom_line(size=.4, alpha=.4) + 
  stat_smooth(method="lm", se=F, alpha=.8) + 
  facet_wrap(~prov, ncol=5) + 
  scale_color_viridis_d() +
  faceted +
  theme(legend.position="none",
        axis.title.y.left=element_blank(),
        axis.title.y.right=element_text(angle=0, vjust=.5)) +
  #axis.text.y.left=element_blank(),
  #axis.ticks.y.left=element_blank()) +
  scale_x_date(date_labels="%b") +
  scale_y_continuous(breaks=seq(0,43,10),
                     sec.axis = dup_axis()) +
  labs(x="\n2022",
       y="",
       #y="Coverage\nrate",
       title=#"Hormone IUCD Inserted (Under 15 years old)" 
         "Increasing trend in Copperbelt
         No trend in Central, Eastern, Luapula, Muchinga, Northern, Northwestern, Southern, Western
       Decreasing trend in Lusaka",
       caption="Hormone IUCD Inserted (Under 15 years old)")

ggsave("viz/Jul-Sep 2022/Family planning/Hormone IUCD Inserted (Under 15).png",
       height=5.3,
       width=7)


#implanon_1519

describe(fp_prov$implanon_1519)


ggplot(fp_prov, aes(date, implanon_1519)) + 
  geom_point(size=.8) + 
  geom_line(size=.4, alpha=.4) + 
  stat_smooth(method="lm", se=F, alpha=.8) + 
  facet_wrap(~prov, ncol=5) + 
  scale_color_viridis_d() +
  faceted +
  theme(legend.position="none",
        axis.title.y.left=element_blank(),
        axis.title.y.right=element_text(angle=0, vjust=.5)) +
  #axis.text.y.left=element_blank(),
  #axis.ticks.y.left=element_blank()) +
  scale_x_date(date_labels="%b") +
  scale_y_continuous(breaks=seq(0,425,50),
                     sec.axis = dup_axis()) +
  labs(x="\n2022",
       y="",
       #y="Coverage\nrate",
       title=#"Implant Implanon (15-19 years old)" 
         "Increasing trend in Central, Copperbelt, Eastern, Luapula, Southern
         No trend in Lusaka, Muchinga, Northern, Northwestern, Western",
       caption="Implant Implanon (15-19 years old)")

ggsave("viz/Jul-Sep 2022/Family planning/Implant Implanon (15-19).png",
       height=5.3,
       width=7)



#implanon_2024

describe(fp_prov$implanon_2024)


ggplot(fp_prov, aes(date, implanon_2024)) + 
  geom_point(size=.8) + 
  geom_line(size=.4, alpha=.4) + 
  stat_smooth(method="lm", se=F, alpha=.8) + 
  facet_wrap(~prov, ncol=5) + 
  scale_color_viridis_d() +
  faceted +
  theme(legend.position="none",
        axis.title.y.left=element_blank(),
        axis.title.y.right=element_text(angle=0, vjust=.5)) +
  #axis.text.y.left=element_blank(),
  #axis.ticks.y.left=element_blank()) +
  scale_x_date(date_labels="%b") +
  scale_y_continuous(breaks=seq(0,900,150),
                     sec.axis = dup_axis()) +
  labs(x="\n2022",
       y="",
       #y="Coverage\nrate",
       title=#"Implant Implanon (20-24 years old)" 
         "Increasing trend in Central, Eastern, Luapula
         No trend in Copperbelt, Lusaka, Muchinga, Northern, Northwestern, Southern, Western",
       caption="Implant Implanon (20-24 years old)")

ggsave("viz/Jul-Sep 2022/Family planning/Implant Implanon (20-24).png",
       height=5.3,
       width=7)


#implanon_ov25

describe(fp_prov$implanon_ov25)


ggplot(fp_prov, aes(date, implanon_ov25)) + 
  geom_point(size=.8) + 
  geom_line(size=.4, alpha=.4) + 
  stat_smooth(method="lm", se=F, alpha=.8) + 
  facet_wrap(~prov, ncol=5) + 
  scale_color_viridis_d() +
  faceted +
  theme(legend.position="none",
        axis.title.y.left=element_blank(),
        axis.title.y.right=element_text(angle=0, vjust=.5)) +
  #axis.text.y.left=element_blank(),
  #axis.ticks.y.left=element_blank()) +
  scale_x_date(date_labels="%b") +
  scale_y_continuous(breaks=seq(0,1700,250),
                     sec.axis = dup_axis()) +
  labs(x="\n2022",
       y="",
       #y="Coverage\nrate",
       title=#"Implant Implanon (Over 25 years old)" 
         "Increasing trend in Central, Eastern, Lusaka
         No trend in Muchinga, Northern, Northwestern, Southern, Western
       Decreasing trend in Copperbelt, Luapula",
       caption="Implant Implanon (Over 25 years old)")

ggsave("viz/Jul-Sep 2022/Family planning/Implant Implanon (Over 25).png",
       height=5.3,
       width=7)


#implanon_u15

describe(fp_prov$implanon_u15)


ggplot(fp_prov, aes(date, implanon_u15)) + 
  geom_point(size=.8) + 
  geom_line(size=.4, alpha=.4) + 
  stat_smooth(method="lm", se=F, alpha=.8) + 
  facet_wrap(~prov, ncol=5) + 
  scale_color_viridis_d() +
  faceted +
  theme(legend.position="none",
        axis.title.y.left=element_blank(),
        axis.title.y.right=element_text(angle=0, vjust=.5)) +
  #axis.text.y.left=element_blank(),
  #axis.ticks.y.left=element_blank()) +
  scale_x_date(date_labels="%b") +
  scale_y_continuous(breaks=seq(0,350,50),
                     sec.axis = dup_axis()) +
  labs(x="\n2022",
       y="",
       #y="Coverage\nrate",
       title=#"Implant Implanon (Under 15 years old)" 
         "Increasing trend in Copperbelt
         No trend in Northern, Northwestern, Southern, Western
       Decreasing trend in Central, Eastern, Luapula, Lusaka, Muchinga",
       caption="Implant Implanon (Under 15 years old)")

ggsave("viz/Jul-Sep 2022/Family planning/Implant Implanon (Under 15).png",
       height=5.3,
       width=7)


#jadelle_1519

describe(fp_prov$jadelle_1519)


ggplot(fp_prov, aes(date, jadelle_1519)) + 
  geom_point(size=.8) + 
  geom_line(size=.4, alpha=.4) + 
  stat_smooth(method="lm", se=F, alpha=.8) + 
  facet_wrap(~prov, ncol=5) + 
  scale_color_viridis_d() +
  faceted +
  theme(legend.position="none",
        axis.title.y.left=element_blank(),
        axis.title.y.right=element_text(angle=0, vjust=.5)) +
  #axis.text.y.left=element_blank(),
  #axis.ticks.y.left=element_blank()) +
  scale_x_date(date_labels="%b") +
  scale_y_continuous(breaks=seq(0,525,75),
                     sec.axis = dup_axis()) +
  labs(x="\n2022",
       y="",
       #y="Coverage\nrate",
       title=#"Implant Jadelle (15-19 years old)" 
         "Increasing trend in Central, Copperbelt, Eastern
         No trend in Luapula, Lusaka, Northwestern, Southern, Western
       Decreasing trend in Muchinga, Northern",
       caption="Implant Jadelle (15-19 years old)")

ggsave("viz/Jul-Sep 2022/Family planning/Implant Jadelle (15-19).png",
       height=5.3,
       width=7)


#jadelle_2024

describe(fp_prov$jadelle_2024)


ggplot(fp_prov, aes(date, jadelle_2024)) + 
  geom_point(size=.8) + 
  geom_line(size=.4, alpha=.4) + 
  stat_smooth(method="lm", se=F, alpha=.8) + 
  facet_wrap(~prov, ncol=5) + 
  scale_color_viridis_d() +
  faceted +
  theme(legend.position="none",
        axis.title.y.left=element_blank(),
        axis.title.y.right=element_text(angle=0, vjust=.5)) +
  #axis.text.y.left=element_blank(),
  #axis.ticks.y.left=element_blank()) +
  scale_x_date(date_labels="%b") +
  scale_y_continuous(breaks=seq(0,1475,225),
                     sec.axis = dup_axis()) +
  labs(x="\n2022",
       y="",
       #y="Coverage\nrate",
       title=#"Implant Jadelle (20-24 years old)" 
         "Increasing trend in Central
         No trend in Copperbelt, Eastern, Luapula, Lusaka, Muchinga, Northern, 
       Northwestern, Southern, Western",
       caption="Implant Jadelle (20-24 years old)")

ggsave("viz/Jul-Sep 2022/Family planning/Implant Jadelle (20-24).png",
       height=5.3,
       width=7)


#jadelle_ov25

describe(fp_prov$jadelle_ov25)


ggplot(fp_prov, aes(date, jadelle_ov25)) + 
  geom_point(size=.8) + 
  geom_line(size=.4, alpha=.4) + 
  stat_smooth(method="lm", se=F, alpha=.8) + 
  facet_wrap(~prov, ncol=5) + 
  scale_color_viridis_d() +
  faceted +
  theme(legend.position="none",
        axis.title.y.left=element_blank(),
        axis.title.y.right=element_text(angle=0, vjust=.5)) +
  #axis.text.y.left=element_blank(),
  #axis.ticks.y.left=element_blank()) +
  scale_x_date(date_labels="%b") +
  scale_y_continuous(breaks=seq(0,2600,400),
                     sec.axis = dup_axis()) +
  labs(x="\n2022",
       y="",
       #y="Coverage\nrate",
       title=#"Implant Jadelle (Over 25 years old)" 
         "Increasing trend in Central, Lusaka
         No trend in Eastern, Luapula, Muchinga, Northern, Northwestern, Southern, Western
       Decreasing trend in Copperbelt",
       caption="Implant Jadelle (Over 25 years old)")

ggsave("viz/Jul-Sep 2022/Family planning/Implant Jadelle (Over 25).png",
       height=5.3,
       width=7)


#jadelle_u15

describe(fp_prov$jadelle_u15)


ggplot(fp_prov, aes(date, jadelle_u15)) + 
  geom_point(size=.8) + 
  geom_line(size=.4, alpha=.4) + 
  stat_smooth(method="lm", se=F, alpha=.8) + 
  facet_wrap(~prov, ncol=5) + 
  scale_color_viridis_d() +
  faceted +
  theme(legend.position="none",
        axis.title.y.left=element_blank(),
        axis.title.y.right=element_text(angle=0, vjust=.5)) +
  #axis.text.y.left=element_blank(),
  #axis.ticks.y.left=element_blank()) +
  scale_x_date(date_labels="%b") +
  scale_y_continuous(breaks=seq(0,700,100),
                     sec.axis = dup_axis()) +
  labs(x="\n2022",
       y="",
       #y="Coverage\nrate",
       title=#"Implant Jadelle (Under 15 years old)" 
         "No trend in Central, Copperbelt, Luapula, Northern, Northwestern, Southern, Western
       Decreasing trend in Eastern, Lusaka, Muchinga",
       caption="Implant Jadelle (Under 15 years old) (considering outliers)")

ggsave("viz/Jul-Sep 2022/Family planning/Implant Jadelle (Under 15).png",
       height=5.3,
       width=7)






