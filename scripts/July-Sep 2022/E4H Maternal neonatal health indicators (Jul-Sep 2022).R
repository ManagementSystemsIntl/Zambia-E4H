# Zambia E4 Health
# Maternal neonatal health indicators
# Calendar 2022

source("scripts/r prep.r")

matp <- read_excel("data/July-Sep 2022/Reproductive maternal/Reproductive Health monthly prov.xlsx",
                  sheet="export",
                  range="A2:U132") %>%
  mutate(date=mdy(paste(month, "01", year, sep=""))) %>%
  filter(year==2022)



labs <- read_excel("data/July-Sep 2022/Child health/Child health monthly prov 2022.xlsx",
                   sheet="export",
                   range="A1:P1",
                   col_names = F)

names(matp)

#ANC Coverage 1st Trimester ----


describe(matp$anc_1st)

ggplot(matp, aes(date, anc_1st, color=province)) + 
  geom_point(size=.8, alpha=.6) + 
  geom_line(alpha=.4) + 
  stat_smooth(method="lm", se=F, alpha=.8, size=.6) + 
  facet_wrap(~province, ncol=5) + 
  scale_color_viridis_d() +
  faceted +
  theme(legend.position="none",
        axis.title.y.left=element_blank(),
        axis.title.y.right=element_text(angle=0, vjust=.5)) +
  #        plot.title=element_markdown()) +
  #axis.text.y.left=element_blank(),
  #axis.ticks.y.left=element_blank()) +
  scale_x_date(date_labels="%b") +
  scale_y_continuous(breaks=seq(0,60,10),
                     sec.axis = dup_axis()) +
  labs(x="2022",
       y="",
       #y="Coverage\nrate",
      title= "
       Decreasing trend in Eastern, Luapula, Muchinga , Northwestern, Western
       No trend in Central, Copperbelt, Northern, Southern
      Increasing trend in Lusaka",
       caption = "ANC Coverage 1st Trimester")

ggsave("viz/Jul-Sep 2022/Reproductive maternal/ANC Coverage 1st Trimester (2022 by province).png",
       height=5.3,
       width=7)

#ANC Coverage All Trimesters ----

describe(matp$anc_all)

ggplot(matp, aes(date, anc_all, color=province)) + 
  geom_point(size=.8, alpha=.6) + 
  geom_line(alpha=.4) + 
  stat_smooth(method="lm", se=F, alpha=.8, size=.6) + 
  facet_wrap(~province, ncol=5) + 
  scale_color_viridis_d() +
  faceted +
  theme(legend.position="none",
        axis.title.y.left=element_blank(),
        axis.title.y.right=element_text(angle=0, vjust=.5)) +
  #        plot.title=element_markdown()) +
  #axis.text.y.left=element_blank(),
  #axis.ticks.y.left=element_blank()) +
  scale_x_date(date_labels="%b") +
  scale_y_continuous(breaks=seq(0,150,15),
                     sec.axis = dup_axis()) +
  labs(x="2022",
       y="",
       #y="Coverage\nrate",
       title= "
       Decreasing trend in Eastern, Western, Muchinga, Northwestern
       No trend in Copperbelt, Luapula, Lusaka, Southern
      Increasing trend in Central, Northern",
       caption = "ANC Coverage All Trimesters")

ggsave("viz/Jul-Sep 2022/Reproductive maternal/ANC Coverage All Trimesters (2022 by province).png",
       height=5.3,
       width=7)


#ANC Visits Under 20 ----

describe(matp$anc_1st_und20)

ggplot(matp, aes(date, anc_1st_und20, color=province)) + 
  geom_point(size=.8, alpha=.6) + 
  geom_line(alpha=.4) + 
  stat_smooth(method="lm", se=F, alpha=.8, size=.6) + 
  facet_wrap(~province, ncol=5) + 
  scale_color_viridis_d() +
  faceted +
  theme(legend.position="none",
        axis.title.y.left=element_blank(),
        axis.title.y.right=element_text(angle=0, vjust=.5)) +
  #        plot.title=element_markdown()) +
  #axis.text.y.left=element_blank(),
  #axis.ticks.y.left=element_blank()) +
  scale_x_date(date_labels="%b") +
  scale_y_continuous(breaks=seq(0,60,10),
                     sec.axis = dup_axis()) +
  labs(x="2022",
       y="",
       #y="Coverage\nrate",
       title= "
       Decreasing trend in Luapula, Muchinga, Northern, Southern
       No trend in Copperbelt, Eastern, Lusaka, Northwestern, Western
      Increasing trend in Central",
       caption = "ANC Visits in 1st Trimester Visits Under 20")

ggsave("viz/Jul-Sep 2022/Reproductive maternal/ANC 1st Trimester U20 (2022 by province).png",
       height=5.3,
       width=7)

# Folic Acid ----

describe(matp$folic)

ggplot(matp, aes(date, folic, color=province)) + 
  geom_point(size=.8, alpha=.6) + 
  geom_line(alpha=.4) + 
  stat_smooth(method="lm", se=F, alpha=.8, size=.6) + 
  facet_wrap(~province, ncol=5) + 
  scale_color_viridis_d() +
  faceted +
  theme(legend.position="none",
        axis.title.y.left=element_blank(),
        axis.title.y.right=element_text(angle=0, vjust=.5)) +
  #        plot.title=element_markdown()) +
  #axis.text.y.left=element_blank(),
  #axis.ticks.y.left=element_blank()) +
  scale_x_date(date_labels="%b") +
  scale_y_continuous(breaks=seq(0,90,10),
                     sec.axis = dup_axis()) +
  labs(x="2022",
       y="",
       #y="Coverage\nrate",
       title= "
       Decreasing trend in Central, Copperbelt, Muchinga, Northern,
       Northwestern
       No trend in Luapula, Lusaka, Southern
      Increasing trend in Eastern, Western",
       caption = "Folic Acid Supplementation During ANC Visits")

ggsave("viz/Jul-Sep 2022/Reproductive maternal/Folic Acid Supplementation During ANC Visits (2022 by province).png",
       height=5.3,
       width=7)

#Iron ----

describe(matp$iron)

ggplot(matp, aes(date, iron, color=province)) + 
  geom_point(size=.8, alpha=.6) + 
  geom_line(alpha=.4) + 
  stat_smooth(method="lm", se=F, alpha=.8, size=.6) + 
  facet_wrap(~province, ncol=5) + 
  scale_color_viridis_d() +
  faceted +
  theme(legend.position="none",
        axis.title.y.left=element_blank(),
        axis.title.y.right=element_text(angle=0, vjust=.5)) +
  #        plot.title=element_markdown()) +
  #axis.text.y.left=element_blank(),
  #axis.ticks.y.left=element_blank()) +
  scale_x_date(date_labels="%b") +
  scale_y_continuous(breaks=seq(0,90,10),
                     sec.axis = dup_axis()) +
  labs(x="2022",
       y="",
       #y="Coverage\nrate",
       title= "
       Decreasing trend in Copperbelt, Luapula, Muchinga, Northern,
       Northwestern
       No trend in Central, Lusaka, Western, Southern
      Increasing trend in Eastern",
       caption = "Iron Supplementation During ANC Visits")

ggsave("viz/Jul-Sep 2022/Reproductive maternal/Iron Supplementation During ANC Visits (2022 by province).png",
       height=5.3,
       width=7)

#ANC High Risk ----

describe(matp$anc_highrisk)

ggplot(matp, aes(date, anc_highrisk, color=province)) + 
  geom_point(size=.8, alpha=.6) + 
  geom_line(alpha=.4) + 
  stat_smooth(method="lm", se=F, alpha=.8, size=.6) + 
  facet_wrap(~province, ncol=5) + 
  scale_color_viridis_d() +
  faceted +
  theme(legend.position="none",
        axis.title.y.left=element_blank(),
        axis.title.y.right=element_text(angle=0, vjust=.5)) +
  #        plot.title=element_markdown()) +
  #axis.text.y.left=element_blank(),
  #axis.ticks.y.left=element_blank()) +
  scale_x_date(date_labels="%b") +
  scale_y_continuous(breaks=seq(0,40,5),
                     sec.axis = dup_axis()) +
  labs(x="2022",
       y="",
       #y="Coverage\nrate",
       title= "
      No trend in Central, Eastern, Muchinga, Southern, Western
      Increasing trend in Copperbelt, Luapula, Lusaka, Northern, Northwestern",
       caption = "ANC High Risk Pregnancies")

ggsave("viz/Jul-Sep 2022/Reproductive maternal/High Risk Pregnance at 1st ANC (2022 by province).png",
       height=5.3,
       width=7)


# Maternal deaths ---- 

frq(matp$mat_dths)

ggplot(matp, aes(date, mat_dths, color=province)) + 
  geom_point(size=.8, alpha=.6) + 
  geom_line(alpha=.4) + 
  stat_smooth(method="lm", se=F, alpha=.8, size=.6) + 
  facet_wrap(~province, ncol=5) + 
  scale_color_viridis_d() +
  faceted +
  theme(legend.position="none",
        axis.title.y.left=element_blank(),
        axis.title.y.right=element_text(angle=0, vjust=.5)) +
#        plot.title=element_markdown()) +
#axis.text.y.left=element_blank(),
#axis.ticks.y.left=element_blank()) +
scale_x_date(date_labels="%b") +
  scale_y_continuous(breaks=seq(0,30,5),
                     sec.axis = dup_axis()) +
  labs(x="2022",
       y="",
       #y="Coverage\nrate",
       title="Maternal deaths 
       Decreasing trend in Central, Copperbelt, Muchinga, Northwestern
       No trend in Central, Luapula, Lusaka, Western
       Increasing trend in Eastern, Southern")

ggsave("viz/Jul-Sep 2022/Reproductive maternal/Maternal deaths (2022 by province).png",
       height=5.3,
       width=7)



ggplot(matp, aes(date, mat_dths_audit, color=province)) + 
  geom_point(size=.8, alpha=.6) + 
  geom_line(alpha=.4) + 
  stat_smooth(method="lm", se=F, alpha=.8, size=.6) + 
  facet_wrap(~province, ncol=5) + 
  scale_color_viridis_d() +
  faceted +
  theme(legend.position="none",
        axis.title.y.left=element_blank(),
        axis.title.y.right=element_text(angle=0, vjust=.5)) +
  #        plot.title=element_markdown()) +
  #axis.text.y.left=element_blank(),
  #axis.ticks.y.left=element_blank()) +
  scale_x_date(date_labels="%b") +
  scale_y_continuous(breaks=seq(0,30,5),
                     sec.axis = dup_axis()) +
  labs(x="2022",
       y="",
       #y="Coverage\nrate",
       title="Maternal deaths, audited 
       Decreasing trend in Central, Copperbelt, Lusaka, Southern
       No trend in Luapula, Northern, Northwestern, Western
       Increasing trend in Eastern, Muchinga")

ggsave("viz/Jul-Sep 2022/Reproductive maternal/Maternal deaths audited (2022 by province).png",
       height=5.3,
       width=7)









mat <- read_xls("data/Jan- Jun 2022/Reproductive Maternal Data_National Level(Monthly).xls")
mat  <- mat  %>%
  mutate(month_chr = str_sub(periodname,
                             start=1,
                             end=nchar(periodname)-5),
         month = factor(month_chr,
                        levels=c("January","February","March","April","May","June","July","August","September","October","November","December")),
         month_code = as.numeric(month), 
         year = str_sub(periodname, 
                        start=nchar(periodname)-4,
                        end=nchar(periodname)),
         monyr = paste(month_code, year, sep="-"),
         mnthyr = my(monyr))

sum(mat$month_chr!=mat$month) # expecting 0 if vars same

pnctrgts <- read_xls("data/Jan- Jun 2022/PNC Targets.xls")
Start <- as.Date(NULL)
End <- as.Date(NULL)


matq <- read_xls("data/Jan- Jun 2022/Reproductive Maternal Data_National Level(Quarterly).xls")
matqp <- read_xls("data/Jan-Mar 2022/Reproductive Maternal Data_Provincial Level(Quarterly).xls")
mat_prov <- read_xls("data/Jan- Jun 2022/Reproductive Maternal Data_Provincial Level(Monthly).xls")

mat_prov
mat_prov  <- mat_prov  %>%
  mutate(month_chr = str_sub(periodname,
                             start=1,
                             end=nchar(periodname)-5),
         month = factor(month_chr,
                        levels=c("January","February","March","April","May","June","July","August","September","October","November","December")),
         month_code = as.numeric(month), 
         year = str_sub(periodname, 
                        start=nchar(periodname)-4,
                        end=nchar(periodname)),
         monyr = paste(month_code, year, sep="-"),
         mnthyr = my(monyr))

sum(mat_prov$month_chr!=mat_prov$month) # expecting 0 if vars same


#'*Maternal ---- *

#'* 1. Client: ANC coverage ----*

mat <- mat %>%
  rename(ancc = 3,
         anc1 = 4,
         anc1u20 = 5) %>%
  mutate(anccp = ancc/100,
         anc1p = anc1/100,
         anc1u20p = anc1u20/100)

#'*set anccp to 100 for all values >100*
mat <- mat %>% 
  dplyr::mutate(ancc = ifelse(ancc > 100, 100, ancc)) %>% 
  dplyr::mutate(anccp = ancc/100)

#'*To create legend, gather method for including a legend --*

mat <- gather(mat, key = subpop , value = rate, c(anccp, anc1p,anc1u20p))
mat$subpop <- factor(mat$subpop, levels = unique(mat$subpop)) # transform into factor
levels(mat$subpop)

#view(mat)

ggplot(mat, aes(x = mnthyr, y = rate, group = subpop, colour = subpop)) +
  geom_point(alpha=.6, size=1.9) + 
  #geom_line(size=1) +
  geom_smooth(method = loess, size = .8, se=FALSE) +
  scale_y_continuous(limits = c(0,1),
                     labels = percent,
                     breaks = c(.1,.2,.3,.4,.5,.6,.7,.8,.9, 1)) +
  scale_x_date(date_labels="%b %Y",date_breaks="4 months") +
  xlab("") + 
  ylab("") +
  ggtitle("Proportion of expected pregnancies receiving antenatal care (ANC), 2018-2022") +
  scale_color_manual(name ="",
                     values = usaid_palette,
                     labels = c("1st ANC coverage (all trimesters)", "1st ANC Coverage (1st Trimester)", 
                                "1st ANC visits in the 1st trimester: Women <20 yrs")
  ) + 
 base

ggsave("viz/Apr-Jun 2022/Reproductive Maternal & Neontal/Proportion of expected pregnancies receiving antenatal care geom-line ns1.png",
       device="png",
       type="cairo",
       height = 5.5,
       width = 10)


#'* 2. ANC coverage all PROVINCES* ----
names(mat_prov)

anc_prov <- mat_prov %>%
  rename(prov = 1,
         ancall =3,
         anc1 = 4,
         anc1u20 = 5) %>%
  mutate(anc1p = anc1/100,
         ancallp= ancall/100,
         anc1u20p = anc1u20/100) %>% 


  #  select(prov, mnthyr, anc1p, anc1hrp) %>% 
  #  select(prov, mnthyr, anc1p, anc1u20p, anc1hrp) %>% 
  select(prov, mnthyr, ancallp, anc1p) %>% 
  mutate(prov = factor(prov),
         ip = case_when(prov=="Northern" |
                          prov =="Central" |
                          prov =="Luapula" |
                          prov =="Muchinga" |
                          prov =="Southern" |
                          prov =="Eastern" ~ "ip",
                        TRUE ~ "non-ip"))



table(anc_prov$ip, anc_prov$prov)
frq(anc_prov$ip) #sjmisc

levels(anc_prov$prov)

anc_prov_l <- anc_prov %>% 
  # gather(key = subpop , value = rate, c(anc1p, anc1hrp)) %>% 
  gather(key = subpop , value = rate, c(ancallp, anc1p)) %>% 
  
  mutate(ip = factor(ip),
         subpop = factor(subpop))

levels(anc_prov_l$ip)
levels(anc_prov_l$subpop)

ggplot(anc_prov_l, aes(x = mnthyr, y = rate, group = subpop, colour = subpop)) +
  # geom_point(alpha=.6, size=.6) + 
  geom_smooth(method = loess, size = .7, se=FALSE)  +
  facet_wrap(~prov) +
  faceted +
  scale_y_continuous(limits = c(0,1),
                     labels = percent,
                     breaks = c(.2,.4,.6,.8, 1)) +
  labs(x ="", y="", caption = "USAID supports Northern, Central, Luapula, Muchinga, Southern and Eastern") +
  ggtitle("Proportion of expected pregnancies receiving 1st ANC visit <br> at first trimester, 2018-2022") +
  scale_color_manual(name= "", values = (usaid_palette), labels = c("1st ANC Coverage (1st Trimester )", "1st ANC coverage (all trimesters)", "Pregnancies of women <20 years")) + base
  # theme(# plot.title = element_text(size = 12), 
  #   plot.title=element_markdown(),
  #   plot.caption = element_text(size=10),
  #   axis.title.x = element_text(size = 10),
  #   axis.title.y = element_text(size = 10),
  #   axis.text.x = element_text(size = 7),
  #   axis.text.y = element_text(size = 7),
  #   legend.text = element_text(size = 10),
  #   legend.title=element_blank(),
  #   legend.position=c(.75,.15),
  #   strip.text=element_text(size=10, family="Gill Sans Mt"),
  # )

# ggsave("viz/(1.1) ANC by province faceted.png",
#        device="png",
#        type="cairo",
#        height=4,
#        width=7)


#'*2.2 ANC coverage PROV SEPARATED BY USAID SUPPORTED AND NONE* ----

names(anc_prov)
table(anc_prov$ip, anc_prov$prov)
frq(anc_prov$ip) #sjmisc

#'*redraw for all USAID-funded provinces*

ipyes <- anc_prov %>%
  filter(ip =="ip")

levels(anc_prov$prov)

ipyes_l <- ipyes %>% 
  gather(key = subpop , value = rate, c(ancallp,anc1p)) %>% 
  mutate(ip = factor(ip),
         subpop = factor(subpop))

levels(ipyes_l$ip)
levels(ipyes_l$subpop)

ipprov <- ggplot(ipyes_l, aes(x = mnthyr, y = rate, group = subpop, colour = subpop)) +
  geom_point(alpha=.6, size=.6) + 
  #geom_line(size=1) +
  geom_smooth(method = loess, size = .7, se=FALSE)  +
  facet_wrap(~prov) +
  faceted +
  scale_y_continuous(limits = c(0,1),
                     labels = percent,
                     breaks = c(.2,.4,.6,.8, 1)) +
  labs(x ="", y="", caption = "") +
  ggtitle("USAID-supported provinces") +
  scale_color_manual(name= "", values = (usaid_palette)) + baseX

#'*redraw for all NON USAID-funded provinces*

#'*define non IP provinces*

ipno <- anc_prov %>%
  filter(ip =="non-ip")

levels(ipno$prov)

ipno_l <- ipno %>% 
  gather(key = subpop , value = rate, c(ancallp, anc1p)) %>% 
  mutate(ip = factor(ip),
         subpop = factor(subpop))

levels(ipno_l$subpop)


nonipprov <- ggplot(ipno_l, aes(x = mnthyr, y = rate, group = subpop, colour = subpop)) +
  geom_point(alpha=.6, size=.6) + 
  #geom_line(size=1) +
  geom_smooth(method = loess, size = .7, se=FALSE)  +
  facet_wrap(~prov) +
  faceted + 
  scale_y_continuous(limits = c(0,1),
                     labels = percent,
                     breaks = c(.2,.4,.6,.8, 1)) +
  labs(x ="", y="", caption = "") +
  ggtitle("Non USAID-supported provinces") +
  scale_color_manual(name= "", values = (usaid_palette), 
                     labels = c("1st ANC Coverage (1st Trimester )", "1st ANC coverage (all trimesters)", "Pregnancies of women <20 years", "All pregnancies", "Women <20 yrs")) + basey

ipprov
nonipprov


all_prov <- grid.arrange(ipprov, nonipprov, ncol = 2)


ggsave("viz/Apr-Jun 2022/Reproductive Maternal & Neontal/ANC by province faceted smooth ns1.png",
       plot = all_prov,
       device="png",
       type="cairo",
       height=7,
       width=12)





#'*4. Inst delivery coverage ----*
inst <- mat  %>%
  rename(instdel = 8) %>%
  mutate(instdelp = instdel/100,
         month_chr = str_sub(periodname,
                             start=1,
                             end=nchar(periodname)-5),
         month = factor(month_chr,
                        levels=c("January","February","March","April","May","June","July","August","September","October","November","December")),
         month_code = as.numeric(month), 
         year = str_sub(periodname, 
                        start=nchar(periodname)-4,
                        end=nchar(periodname)),
         monyr = paste(month_code, year, sep="-"),
         mnthyr = my(monyr))

sum(inst$month_chr!=inst$month) # expecting 0 if vars same
names(mat_prov)

names(inst)

instd_prov <- mat_prov %>%
  rename(prov = 1,
         instdel = 8) %>%
  # mutate(prov = case_when(prov == 'Northwestern' ~ 'NW'))#
  select(prov, mnthyr, instdel) %>% 
  mutate(prov = factor(prov),
         ip = case_when(prov=="Northern" |
                          prov =="Central" |
                          prov =="Luapula" |
                          prov =="Muchinga" |
                          prov =="Southern" |
                          prov =="Eastern" ~ "ip",
                        TRUE ~ "non-ip"))

levels(instd_prov$prov)

inst <- mat  %>%
  rename(instdel = 8) %>%
  mutate(instdelp = instdel/100,
         month_chr = str_sub(periodname,
                             start=1,
                             end=nchar(periodname)-5),
         month = factor(month_chr,
                        levels=c("January","February","March","April","May","June","July","August","September","October","November","December")),
         month_code = as.numeric(month), 
         year = str_sub(periodname, 
                        start=nchar(periodname)-4,
                        end=nchar(periodname)),
         monyr = paste(month_code, year, sep="-"),
         mnthyr = my(monyr))
inst


sum(inst$month_chr!=inst$month) # expecting 0 if vars same

ggplot(inst, aes(x=mnthyr, y=instdelp, fill=year)) + 
  geom_bar(alpha=0.7, position = "dodge", stat="identity", color=light_grey) +
  geom_smooth(color= dark_grey, se=F, size=1.1, alpha=.8) +
  scale_y_continuous(limits=c(0,1),
                     labels = percent,
                     breaks = c(.1,.2,.3,.4,.5,.6,.7,.8,.9, 1)) +
  scale_fill_manual(values = c("#CFCDC9", "#A7C6ED","#002F6C","#000000","#BA0C2F")) +
  scale_x_date(date_labels="%b %Y",date_breaks="3 months") +
  labs(x="",
       y="",
       fill="", title="Proportion of expected deliveries occurring in health facilities increased \nfrom 70% to slight above 80% in 2018, then decreased back to below 70% by June 2022") +
  non_base

ggsave("viz/Apr-Jun 2022/Reproductive Maternal & Neontal/Expected deliveries bars & smooth ns.png",
       device="png",
       type="cairo",
       height=7,
       width=12)



#'*4.1 Inst delivery PROV SEPARATED BY USAID SUPPORTED AND NONE ----*
names(inst)

instd_prov <- mat_prov %>%
  rename(prov = 1,
         instdel = 8) %>%

  select(prov, mnthyr, instdel) %>% 
  mutate(prov = factor(prov),
         ip = case_when(prov=="Northern" |
                          prov =="Central" |
                          prov =="Muchinga" |
                          prov == "Luapula" |
                          prov =="Southern" |
                          prov =="Eastern" ~ "ip",
                        TRUE ~ "non-ip"))

names(instd_prov)
table(instd_prov$ip, instd_prov$prov)
frq(instd_prov$ip) #sjmisc

# redraw inst deliveries for all USAID-funded provinces

ipyes <- instd_prov %>%
  filter(ip =="ip")

instd.ipvz <- ggplot(ipyes, aes(x = mnthyr, y = instdel, colour = ip)) +
  geom_point(alpha=.5, size=.8, position = "dodge", stat = "identity", fill=light_blue) + 
  geom_smooth(color=usaid_blue,method = loess, size = .8, se=FALSE)  +
  facet_wrap(~prov) +
  faceted +
  scale_y_continuous(limits = c(0,100),
                     breaks = c(20, 40, 60, 80)) +
  xlab("") + 
  ylab("") +
  ggtitle("USAID-supported provinces") +
  scale_color_manual(name= "", values = (usaid_palette)) + baseX


# inst deliveries for non-usaid provinces 

ipno <- instd_prov %>%
  filter(ip =="non-ip")


instd.nonipvz <- ggplot(ipno, aes(x = mnthyr, y = instdel, colour = ip)) +
  geom_point(alpha=.3, size=.8, color=usaid_red, position = "dodge", stat = "identity", fill=usaid_red) + 
  geom_smooth(color=usaid_red, method = loess, size = .8, se=FALSE)  +
  facet_wrap(~prov) +
  faceted +
  scale_y_continuous(limits = c(0,100),
                     breaks = c(20, 40, 60, 80)) +
  xlab("") + 
  ylab("") +
  ggtitle("Non USAID-supported provinces") +
  scale_color_manual(name= "", values = (usaid_palette)) + baseX

instd.ipvz
instd.nonipvz
sprt <- grid.arrange(instd.ipvz, instd.nonipvz, ncol=2)

ggsave("viz/Apr-Jun 2022/Reproductive Maternal & Neontal/Expected deliveries by province faceted ns.png",
       plot=sprt,
       device="png",
       type="cairo",
       height=7,
       width=12)


#'* POSTNATAL CARE NATIONAL*

pnc  <- mat  %>%
  rename(pnc = 11) %>%
  mutate(pncp = pnc/100)


pnctrgts$Start <- as.Date(pnctrgts$Start) 
pnctrgts$End <- as.Date(pnctrgts$End) 
Start <- as.Date(NULL)
End <- as.Date(NULL)


pnc_plt <- ggplot(pnc, aes(mnthyr, pncp)) + 
  geom_point(color= web_blue, alpha=.6, size=.8, fill=web_blue) + 
  geom_rect(data=pnctrgts, aes(NULL,NULL,xmin=Start,xmax=End,fill=PNCTargets),
            ymin=c(.74,.79,.85,.9) ,ymax=c(.75,.80,.86,.91), colour=light_grey, size=0.8, alpha=0.8, lty="solid", fill=usaid_red) +
  # geom_line(color= usaid_blue, alpha=.4) +
  scale_y_continuous(limits = c(0,1),
                     labels = percent,
                     breaks = c(.1, .2, .3, .4, .5, .6, .7, .8, .9, 1)) +
  stat_smooth(color= web_blue, se=F, size=1.1, alpha=.8) +
  scale_x_date(date_labels="%b %Y",date_breaks="4 months") +
  labs(x="",
       y="",
       title=""
  ) + base +
  annotate(geom = "text", x=trgt1, y = 0.74, family="Gill Sans Mt", colour = rich_black, label=substitute(paste(bold("National Targets"))), size= 4,  hjust =0, vjust=-9) +
  annotate(geom = "text", x=trgt1, y = 0.74, family="Gill Sans Mt", colour = rich_black, label=substitute(paste(bold("74%"))), size= 4,  hjust =-2.5, vjust=-1.8) +
  annotate(geom = "text", x=trgt1, y = 0.79, family="Gill Sans Mt", colour = rich_black, label=substitute(paste(bold("79%"))), size= 4,  hjust =-9.5, vjust=-1) +
  annotate(geom = "text", x=trgt1, y = 0.85, family="Gill Sans Mt", colour = rich_black, label=substitute(paste(bold("85%"))), size= 4,  hjust =-17.5, vjust=-1) +
  annotate(geom = "text", x=trgt1, y = 0.9, family="Gill Sans Mt", colour = rich_black, label=substitute(paste(bold("90%"))), size= 4,  hjust =-25.5, vjust=-1)



pnc_plt 


ggsave("viz/Apr-Jun 2022/Reproductive Maternal & Neontal/postnatal care within 48 hrs stat_smooth ns.png",
       device="png",
       type="cairo",
       height=7,
       width=11)


#'* POSTNATAL CARE NATIONAL SEPERATED BY USAID SUPPORTED AND NONE*
names(mat_prov)

pnc_prov <- mat_prov %>%
  rename(prov = 1,
         pncr  = 11) %>%
  select(prov, mnthyr, pncr) %>% 
  mutate(prov = factor(prov),
         ip = case_when(prov=="Northern" |
                          prov =="Central" |
                          prov =="Luapula" |
                          prov =="Muchinga" |
                          prov =="Southern" |
                          prov =="Eastern" ~ "ip",
                        TRUE ~ "non-ip"))

levels(pnc_prov$prov)

ipyes <- pnc_prov %>%
  filter(ip=="ip")

pnc.ipvz <- ggplot(ipyes, aes(x = mnthyr, y = pncr, colour = ip)) +
  geom_area(alpha=.3, size=.8,color=usaid_blue, fill=light_blue) + 
  #stat_smooth(method = loess, size = .7, se=FALSE, color=light_blue)  +
  facet_wrap(~prov) +
  faceted +
  scale_y_continuous(limits = c(0,100),
                     breaks = c(20, 40, 60, 80)) +
  xlab("") + 
  ylab("") +
  ggtitle("USAID-supported provinces") +
  scale_color_manual(name= "", values = (usaid_palette), labels = c("Supported by FHN, MOMENT and G2G", "Not supported")) +
  baseX


ipno <- pnc_prov %>%
  filter(ip=="non-ip")

pnc.ipnonvz <- ggplot(ipno, aes(x = mnthyr, y = pncr, colour = ip)) +
  geom_area(alpha=.3, size=.8, color=usaid_red, fill=usaid_red) + 
  #stat_smooth(method = loess, size = .7, se=FALSE, color=usaid_red)  +
  facet_wrap(~prov) +
  faceted +
  scale_y_continuous(limits = c(0,100),
                     breaks = c(20, 40, 60, 80)) +
  xlab("") + 
  ylab("") +
  ggtitle("Non USAID-supported provinces") +
  scale_color_manual(name= "", values = (usaid_palette), labels = c("Supported by FHN, MOMENT and G2G", "Not supported")) +
  baseX

pnc.ipvz 
pnc.ipnonvz 

pnc.all <- grid.arrange(pnc.ipvz, pnc.ipnonvz, ncol=2)

ggsave("viz/Apr-Jun 2022/Reproductive Maternal & Neontal/postnatal care within 48 by province faceted area ns.png",
       plot=pnc.all,
       device="png",
       type="cairo",
       height=7,
       width=12)



#'*FRESH STILL & MACERATED STILLBIRTH NATIONAL*

names(mat)

mat_smb <- mat  %>%
  rename( prov = 1,
          sbr = 15,
          msbr = 19,
          fsbr = 20
  ) %>% 
  select(prov, mnthyr, sbr, msbr, fsbr) 

# [pivot_longer syntax] ----

mat_smb1 <- mat_smb %>%
  pivot_longer(c(sbr, msbr, fsbr), names_to= "sbtype", values_to = "sbrate") %>% 
  mutate(sbtype = factor(sbtype),
         year=year(mnthyr))

levels(mat_smb1$sbtype)
mat_smb1

fms_plt <- ggplot(mat_smb1, aes(x = mnthyr, y = sbrate/100, group = sbtype, colour = sbtype)) +
  geom_point(alpha=.6, size=.8) + 
  #  geom_line(alpha=.4) +
  stat_smooth(se=T, size=.8, alpha=.8) +
  scale_y_continuous(limits=c(0,.2),
                     breaks=seq(0,.2,.02),
                     labels=percent_format(accuracy=1)) +
  scale_x_date(date_labels="%b %Y",date_breaks="4 months") +
  labs(x="",
       y="",
       title=paste("Stillbirth rates on a ", declining, "trend since March 2022"),,
       #title="Stillbirth rates on a <span style='color:#BA0C2F;'>**declining**</span> trend since March 2022",
       #title=     "The proportion of Fresh stillbirths and Macerated stillbirths has been decreasing since the first quarter of #2022 \n this decrease trend started fourth quarter of 2021",
       caption= "Stillbirth rates expressed per 1000 total births") +
  scale_color_manual(name= "",values = c(medium_grey,usaid_blue,usaid_red), labels = c("Fresh stillbirth rate","Macerated stillbirth rate","Stillbirth rate (MSB + FSB)")) +
  base +
  theme(plot.title.position="plot",
        plot.title=element_markdown())

fms_plt 

ggsave("viz/Apr-Jun 2022/Reproductive Maternal & Neontal/fresh & macerated stillbirths ns ggtext.png",
       device="png",
       type="cairo",
       height=7,
       width=13)



#'*FRESH STILL & MACERATED STILLBIRTH PROVINCIAL SEPERATED BY USAID SUPPORTED AND NONE*
names(mat_prov)

mat_smb2 <- mat_prov  %>%
  rename( prov = 1,
          sbr = 15,
          msbr = 19,
          fsbr = 20
  ) %>% 
  select(prov, mnthyr, sbr, msbr, fsbr) %>%
  mutate(prov = factor(prov),
         ip = case_when(prov =="Northern" |
                          prov =="Central" |
                          prov =="Luapula" |
                          prov =="Muchinga" |
                          prov =="Southern" |
                          prov =="Eastern" ~ "ip",
                        TRUE ~ "non-ip"))

levels(mat_smb2$prov)

# [pivot_longer syntax] ----

mat_smb2_2 <- mat_smb2 %>% 
  pivot_longer(c(sbr, msbr, fsbr), names_to= "sbtype", values_to = "sbrate") %>% 
  mutate(sbtype = factor(sbtype),
         year=year(mnthyr))

levels(matsb_l$sbtype)


nt_sprtd <- ggplot(mat_smb2_2, aes(x = mnthyr, y = sbrate, group = sbtype, colour = sbtype)) +
  geom_point(alpha=.6, size=.8) + 
  #  geom_line(alpha=.4) +
  stat_smooth(se=F, size=.8, alpha=.8) +
  scale_y_continuous(limits=c(0,18)) + 
  facet_wrap(~prov) +
  faceted +
  labs(x="",
       y="",
       title="The proportion of stillbirths that are macerated stillbirths has slightly increased \nin the last two years",
       caption= "Stillbirth rates expressed per 1000 total births") +
  scale_color_manual(name= "",values = c(medium_grey,usaid_blue,usaid_red), labels = c("Fresh stillbirth rate","Macerated stillbirth rate","Stillbirth rate (MSB + FSB)")) +
  basey

nt_sprtd

#'*-----------------------------Separating Provinces by USAID supported and non supported*

spprtd <- mat_smb2_2 %>%
  filter(ip=="ip")

spprtd

spprtd_plt <- ggplot(spprtd, aes(x = mnthyr, y = sbrate, group = sbtype, colour = sbtype)) +
  geom_point(alpha=.3, size=.8) + 
  stat_smooth(method = loess, size = .9, se=FALSE)  +
  facet_wrap(~prov) +
  faceted +
  scale_y_continuous(limits = c(0,18)) +
  xlab("") + 
  ylab("") +
  ggtitle("USAID-supported provinces") +
  scale_color_manual(name= "", values = c(medium_grey,usaid_blue,usaid_red)) +
  baseX
spprtd_plt


non_spprtd <- mat_smb2_2 %>%
  filter(ip=="non-ip")

non_spprtd_plt <- ggplot(non_spprtd, aes(x = mnthyr, y = sbrate, group = sbtype, colour = sbtype)) +
  geom_point(alpha=.3, size=.8) + 
  stat_smooth(method = loess, size = .9, se=FALSE)  +
  facet_wrap(~prov) +
  faceted +
  scale_y_continuous(limits = c(0,18)) +
  xlab("") + 
  ylab("") +
  ggtitle("USAID-supported provinces") +
  scale_color_manual(name= "", values = c(medium_grey,usaid_blue,usaid_red), labels = c("Fresh stillbirth rate","Macerated stillbirth rate","Stillbirth rate (MSB + FSB)")) +
  basey

non_spprtd_plt
spprtd_plt

sbr.allplt <- grid.arrange(spprtd_plt, non_spprtd_plt, ncol=2)

ggsave("viz/Apr-Jun 2022/Reproductive Maternal & Neontal/Province fresh & macerated stillbirths faceted ns.png",
       plot=sbr.allplt,
       device="png",
       type="cairo",
       height=7,
       width=13)


#'*MATERNAL DEATHS NATIONAL*
mmr <- mat %>%
  rename(mmfr = 16,
         mmf = 13,
         mmc = 22
  )
colnames(mat)
pnctrgts$Start <- as.Date(pnctrgts$Start) 
pnctrgts$End <- as.Date(pnctrgts$End) 
Start <- as.Date(NULL)
End <- as.Date(NULL)


mmr_1 <- gather(mmr, key = mmtype , value = deaths, c(mmfr, mmf, mmc))
mmr_1
colnames(mmr_1)

mmr_1$mmtypef <- factor(mmr_1$mmtype, levels = unique(mmr_1$mmtype))
levels(mmr_1$mmtypef)

names(mmr_1)
colnames(mmr_1)

mmr_plt <- ggplot(mmr_1, aes(x = mnthyr, y = deaths , colour =   mmtype, linetype=mmtype)) + 
  geom_point(alpha=.5, size=.7) + 
  geom_rect(data=pnctrgts, mapping=aes(NULL,NULL,xmin=Start,xmax=End,fill=PNCTargets),
            ymin=c(245,200,150,100) ,ymax=c(247,202,152,102), colour=light_grey, size=0.3, alpha=0.4, lty="solid", fill=usaid_red) +
  geom_smooth(method = loess, size=.9, se=F) +
  scale_y_continuous(limits = c(0,260),
                     breaks = c(20,60,100,140,180,220,260),
                     labels = c("20","60","100","140","180","220", "260")) +
  scale_x_date(date_breaks = "4 months", date_labels = "%b %y")+
  scale_linetype_manual(name="",
                        labels= c("Community deaths","Health facility deaths", "Maternal mortality facility ratio (per 100,000 live births)"), 
                                  values=c("solid","solid", "dashed"))+
  xlab("") +
  ylab("") +
  #ggtitle("Maternal deaths and mortality ratio in facilities have increased since February 2020, \nwhile community deaths numbers have decreased") +
  scale_colour_manual(name = "",
                      labels= c("Community deaths","Health facility deaths", "Maternal mortality facility ratio (per 100,000 live births)"),
                      values = c(usaid_blue, medium_grey, usaid_red)) +
  base + 
  annotate(geom = "text", x=trgt1, y = 250, family="Gill Sans Mt", colour = rich_black, label=substitute(paste(italic("National Targets"))), size= 3.5,  hjust =-.0, vjust=-2) +
  annotate(geom = "text", x=trgt1, y = 245, family="Gill Sans Mt", colour = rich_black, label=substitute(paste(bold("245"))), size= 3,  hjust =-5.2, vjust=-.8) +
  annotate(geom = "text", x=trgt1, y = 200, family="Gill Sans Mt", colour = rich_black, label=substitute(paste(bold("200"))), size= 3,  hjust =-19.5, vjust=-1) +
  annotate(geom = "text", x=trgt1, y = 150, family="Gill Sans Mt", colour = rich_black, label=substitute(paste(bold("150"))), size= 3,  hjust =-30.5, vjust=-1) +
  annotate(geom = "text", x=trgt1, y = 100, family="Gill Sans Mt", colour = rich_black, label=substitute(paste(bold("100"))), size= 3,  hjust =-45.5, vjust=-1)


mmr_plt

ggsave("viz/Apr-Jun 2022/Reproductive Maternal & Neontal/maternal deaths national ns.png",
       device="png",
       type="cairo",
       height=8,
       width=13)


#'*Maternal MORTALITY PROVINCe SEPERATED BY USAID SUPPORTED AND NONE*

names(mat_prov)

mmp_prov <- mat_prov %>%
  rename(prov = 1,
         fmmr = 16,
  ) %>% 
  select(prov, fmmr, mnthyr) %>% 
  mutate(prov = factor(prov),
         ip = case_when(prov=="Northern" |
                          prov =="Central" |
                          prov =="Luapula" |
                          prov =="Muchinga" |
                          prov =="Southern" |
                          prov =="Eastern" ~ "ip",
                        TRUE ~ "non-ip"))

levels(mmp_prov$prov)

spprtd_mm <- mmp_prov %>%
  filter(ip=="ip")

spprtd_mm_plt <- ggplot(spprtd_mm, aes(x = mnthyr, y = fmmr, colour = ip)) +
  geom_point(alpha=.6, size=1) + 
  stat_smooth(method = loess, size = .9, se=FALSE)  +
  facet_wrap(~prov) +
  faceted +
  scale_y_continuous(limits = c(0,500),
                     breaks = c(100, 200, 300, 400)) +
  xlab("") + 
  ylab("") +
  ggtitle("USAID-supported provinces") +
  scale_color_manual(name= "", values = usaid_blue, labels = c("Supported by FHN, MOMENT and G2G", "Not supported")) +
  baseX

non_spprtd_mm <- matd_prov %>%
  filter(ip=="non-ip")

non_spprtd_mm_plt <- ggplot(non_spprtd_mm, aes(x = mnthyr, y = fmmr, colour = ip)) +
  geom_point(alpha=.6, size=1) + 
  stat_smooth(method = loess, size = .9, se=FALSE)  +
  facet_wrap(~prov) +
  faceted +
  scale_y_continuous(limits = c(0,500),
                     breaks = c(100, 200, 300, 400)) +
  xlab("") + 
  ylab("") +
  ggtitle("Non USAID-supported provinces") +
  scale_color_manual(name= "", values = usaid_red, labels = c("Supported by FHN, MOMENT and G2G", "Not supported")) +
  baseX

spprtd_mm_plt
non_spprtd_mm_plt

mmr.allplt <- grid.arrange(spprtd_mm_plt,non_spprtd_mm_plt, ncol=2)

ggsave("viz/Apr-Jun 2022/Reproductive Maternal & Neontal/Maternal mortality by Provincial faceted ns.png",
       plot=mmr.allplt,
       device="png",
       type="cairo",
       height=7,
       width=13)

#'*STILLBIRTH RATE PER 1000 LIVE BIRTH NATIONAL*

names(mat)

mat_smb <- mat  %>%
  rename( prov = 1,
          sbr = 15
  ) %>% 
  select(prov, mnthyr, sbr) 

pnctrgts$Start <- as.Date(pnctrgts$Start) 
pnctrgts$End <- as.Date(pnctrgts$End) 
Start <- as.Date(NULL)
End <- as.Date(NULL)

# [pivot_longer syntax] ----

  
  pivot_longer(sbr, names_to= "sbtype", values_to = "sbrate") %>% 
  mutate(sbtype = factor(sbtype),
         year=year(mnthyr))

levels(mat_smb1$sbtype)
mat_smb1

fms_plt <- ggplot(mat_smb1, aes(x = mnthyr, y = sbrate, colour = sbtype)) +
  geom_point(alpha=.6, size=.8) + 
  geom_rect(data=pnctrgts, mapping=aes(NULL,NULL,xmin=Start,xmax=End,fill=PNCTargets),
            ymin=c(10,8,6,4) ,ymax=c(9.8,7.8,5.8,3.8), colour=light_grey, size=0.8, alpha=0.6, lty="solid", fill=usaid_red) +
  #  geom_line(alpha=.4) +
  stat_smooth(se=F, size=1, alpha=.8) +
  scale_y_continuous(limits=c(0,18))+
  scale_x_date(date_labels="%b %Y",date_breaks="4 months") +
  labs(x="",
       y="",
       #title="The proportion of Fresh stillbirths and Macerated stillbirths has been decreasing since the first quarter of 2022 \n this decrease trend started fourth quarter of 2021",
       caption= "Stillbirth rates expressed per 1000 total births") +
  scale_color_manual(name= "",values = c(medium_grey,usaid_blue,usaid_red), labels = c("Stillbirth rate (MSB + FSB)")) +
  base +
  annotate(geom = "text", x=trgt1, y = 13, family="Gill Sans Mt", colour = rich_black, label=substitute(paste(italic("Achieved"))), size= 4,  hjust =-20.5, vjust=-2.5) +
  annotate(geom = "text", x=trgt1, y = 2, family="Gill Sans Mt", colour = rich_black, label=substitute(paste(italic("National Targets"))), size= 4,  hjust =-10.5, vjust=-3.5) +
  annotate(geom = "text", x=trgt1, y = 10, family="Gill Sans Mt", colour = rich_black, label=substitute(paste(bold("10"))), size= 3,  hjust =-11.2, vjust=-1) +
  annotate(geom = "text", x=trgt1, y = 8, family="Gill Sans Mt", colour = rich_black, label=substitute(paste(bold("8"))), size= 3,  hjust =-60.5, vjust=-1) +
  annotate(geom = "text", x=trgt1, y = 6, family="Gill Sans Mt", colour = rich_black, label=substitute(paste(bold("6"))), size= 3,  hjust =-92.5, vjust=-1) +
  annotate(geom = "text", x=trgt1, y = 4, family="Gill Sans Mt", colour = rich_black, label=substitute(paste(bold("4"))), size= 3,  hjust =-130.5, vjust=-1)

fms_plt


ggsave("viz/Apr-Jun 2022/Reproductive Maternal & Neontal/Stillbirth Rate National ns.png",
       device="png",
       type="cairo",
       height=7,
       width=13)



#'*NEONATAL AND PERINATAL DEATHS NATIONAL*
names(mat)

npd <- mat  %>%
  rename( prov = 1,
          nd = 26,
          pd = 27
  ) %>% 
  select(prov, mnthyr, nd, pd) 


npd1 <- npd %>% 
  pivot_longer(c(nd, pd), names_to= "sbtype", values_to = "sbrate") %>% 
  mutate(sbtype = factor(sbtype),
         year=year(mnthyr))

levels(npd1$sbtype)


nt_sprtd <- ggplot(npd1, aes(x = mnthyr, y = sbrate, group = sbtype, colour = sbtype)) +
  geom_point(alpha=.6, size=.9) + 
  #  geom_line(alpha=.4) +
  geom_smooth(se=F, size=.8, alpha=.9) +
  scale_x_date(date_labels = "%b %y",date_breaks = "4 months") +
  # scale_y_continuous(limits=c(0,18)) + 
  # faceted +
  labs(x="",
       y="",
       title="The Perinatal deaths ahve been on the decline for the last two years , \nwhile Neontal deaths has been on the rise since 2018",
       caption= "Stillbirth rates expressed per 1000 total births") +
  scale_color_manual(name= "",values = c(usaid_blue,usaid_red), labels = c("Neonatl Deaths","Perinatal Deaths")) +
  basey

nt_sprtd

ggsave("viz/Apr-Jun 2022/Reproductive Maternal & Neontal/Neonatl and Perinatal deaths ns.png",
       device="png",
       type="cairo",
       height=8,
       width=13)



#'* NEONATAL DEATHS COUNTS BY PRVONCE SEPERATED BY USAID SUPPORT AND NON*


names(mat_prov)

npd_prov <- mat_prov %>%
  rename(prov = 1,
         ndc = 26,
  ) %>% 
  select(prov, ndc, mnthyr) %>% 
  mutate(prov = factor(prov),
         ip = case_when(prov=="Northern" |
                          prov =="Central" |
                          prov =="Luapula" |
                          prov =="Muchinga" |
                          prov =="Southern" |
                          prov =="Eastern" ~ "ip",
                        TRUE ~ "non-ip"))

levels(npd_prov$prov)

spprtd_ndp <- npd_prov %>%
  filter(ip=="ip")


levels(spprtd_ndp$prov)

spprtd_ndp

nd_spprtd_plt <- ggplot(spprtd_ndp, aes(x = mnthyr, y = ndc, colour = ip)) +
  geom_point(alpha=.4, size=.9) + 
  geom_smooth(method = loess, size = 1, se=FALSE)  +
  facet_wrap(~prov) +
  faceted +
  scale_y_continuous(limits = c(0,30)) +
  xlab("") + 
  ylab("") +
  ggtitle("USAID-supported provinces") +
  scale_color_manual(name= "", values = usaid_blue) +
  baseX
nd_spprtd_plt


non_spprtd_ndp <- npd_prov %>%
  filter(ip=="non-ip")

nd_spprtd_plt1 <- ggplot(non_spprtd_ndp, aes(x = mnthyr, y = ndc, colour = ip)) +
  geom_point(alpha=.4, size=.9) + 
  geom_smooth(method = loess, size = 1, se=FALSE)  +
  facet_wrap(~prov) +
  faceted +
  scale_y_continuous(limits = c(0,30)) +
  xlab("") + 
  ylab("") +
  ggtitle("USAID-supported provinces") +
  scale_color_manual(name= "", values = usaid_red, labels = "Neontal Deaths") +
  basey

nd_spprtd_plt
nd_spprtd_plt1

nd.allplt <- grid.arrange(nd_spprtd_plt, nd_spprtd_plt1, ncol=2)

ggsave("viz/Apr-Jun 2022/Reproductive Maternal & Neontal/Neonatl and Perinatal deaths by province faceted ns.png",
       plot = nd.allplt,
       device="png",
       type="cairo",
       height=8,
       width=13)
