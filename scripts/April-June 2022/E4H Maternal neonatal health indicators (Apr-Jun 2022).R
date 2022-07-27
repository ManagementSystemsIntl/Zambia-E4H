#'*Zambia E4 Health*
#'*Maternal neonatal health indicators*
#'*April - June 2022*


source("scripts/r prep2.r")

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

view(mat)

ggplot(mat, aes(x = mnthyr, y = rate, group = subpop, colour = subpop)) +
  geom_point(alpha=.6, size=1.9) + 
  geom_line(size=1) +
  #geom_smooth(method = loess, size = .8, se=FALSE) +
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
  geom_area(alpha=.5, size=.8, position = "dodge", stat = "identity", fill=light_blue) + 
  #geom_smooth(color=usaid_blue,method = loess, size = .8, se=FALSE)  +
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
  geom_area(alpha=.3, size=.8, color=usaid_red, position = "dodge", stat = "identity", fill=usaid_red) + 
  #geom_smooth(color=usaid_blue, method = loess, size = .8, se=FALSE)  +
  facet_wrap(~prov) +
  faceted +
  scale_y_continuous(limits = c(0,100),
                     breaks = c(20, 40, 60, 80)) +
  xlab("") + 
  ylab("") +
  ggtitle("Non USAID-supported provinces") +
  scale_color_manual(name= "", values = (usaid_palette)) + basey

instd.ipvz
instd.nonipvz
sprt <- grid.arrange(instd.ipvz, instd.nonipvz, ncol=2)

ggsave("viz/Apr-Jun 2022/Reproductive Maternal & Neontal/Expected deliveries by province bars faceted area ns.png",
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


trgt1 <- as.Date("2018-01-01")
trgt2 <- as.Date("2019-01-01")
trgt3 <- as.Date("2020-01-01")
trgt4 <- as.Date("2021-01-01")

# # target2018 <- data.frame(x1= as.Date("2018-10-01"), x2=as.Date("2019-01-01"), y1=.74, y2=.74)
# # target2019 <- data.frame(x1=as.Date("2019-10-01"), x2=as.Date("2020-01-01"), y1=.79, y2=.79)
# # target2020 <- data.frame(x1=as.Date("2020-10-01"), x2=as.Date("2021-01-01"), y1=.85, y2=.85)
# # target2021 <- data.frame(x1=as.Date("2021-10-01"), x2=as.Date("2022-01-01"), y1=.81, y2=.81)
# # targets <- bind_rows(target2018, target2019, target2020, target2021)


pnc_vz <- ggplot(pnc, aes(mnthyr, pncp)) + 
  geom_point(color= usaid_blue, alpha=.6, size=.8) + 
  geom_rect(data=pnctrgts, aes(NULL,NULL,xmin=Start,xmax=End,fill=PNCTargets),
            ymin=c(.74,.79,.85,.9) ,ymax=c(.75,.80,.86,.91), colour=light_grey, size=0.8, alpha=0.8, lty="solid", fill=usaid_red) +
  # geom_line(color= usaid_blue, alpha=.4) +
  scale_y_continuous(limits = c(0,1),
                     labels = percent,
                     breaks = c(.1, .2, .3, .4, .5, .6, .7, .8, .9, 1)) +
  stat_smooth(color= usaid_blue, se=F, size=1.1, alpha=.8) +
  scale_x_date(date_labels="%b %Y",date_breaks="4 months") +
  labs(x="",
       y="",
       title=""
  ) + base +
  annotate(geom = "text", x=trgt1, y = 0.74, family="Gill Sans Mt", colour = "black", label=substitute(paste(bold("National Targets"))), size= 4,  hjust =0, vjust=-9) +
  annotate(geom = "text", x=trgt1, y = 0.74, family="Gill Sans Mt", colour = "black", label=substitute(paste(bold("74%"))), size= 4,  hjust =-2.5, vjust=-1.8) +
  annotate(geom = "text", x=trgt1, y = 0.79, family="Gill Sans Mt", colour = "black", label=substitute(paste(bold("79%"))), size= 4,  hjust =-9.5, vjust=-1) +
  annotate(geom = "text", x=trgt1, y = 0.85, family="Gill Sans Mt", colour = "black", label=substitute(paste(bold("85%"))), size= 4,  hjust =-17.5, vjust=-1) +
  annotate(geom = "text", x=trgt1, y = 0.9, family="Gill Sans Mt", colour = "black", label=substitute(paste(bold("90%"))), size= 4,  hjust =-25.5, vjust=-1)



pnc_vz 


ggsave("viz/Apr-Jun 2022/Reproductive Maternal & Neontal/postnatal care within 48 hrs stat_smooth ns.png",
       device="png",
       type="cairo",
       height=7,
       width=11)


  # theme(plot.title = element_text(size = 14), 
  #       axis.title.x = element_text(size = 12),
  #       axis.title.y = element_text(size = 12),
  #       axis.text = element_text(size = 9),
  #       legend.title = element_text(size = 12), 
  #       legend.text = element_text(size = 11)
  # ) 
# pnc_vz +  annotate(geom="text", x=as.Date("2018-01-01"), y=.75, colour = usaid_red, label="National Targets", size= 3) +
#   annotate(geom="text", x=as.Date("01-06-2021", format = "%d-%m-%Y"), y=.1, label="*home deliveries included", size =3, fontface = 'italic')


#yearn <- lapply(mat$year, as.numeric)
# 
# target2018 <- data.frame(x1= as.Date("2018-10-01"), x2=as.Date("2019-01-01"), y1=.74, y2=.74)
# target2019 <- data.frame(x1=as.Date("2019-10-01"), x2=as.Date("2020-01-01"), y1=.79, y2=.79)
# target2020 <- data.frame(x1=as.Date("2020-10-01"), x2=as.Date("2021-01-01"), y1=.85, y2=.85)
# target2021 <- data.frame(x1=as.Date("2021-10-01"), x2=as.Date("2022-01-01"), y1=.81, y2=.81)
# targets <- bind_rows(target2018, target2019, target2020, target2021)

# #targets <- data.frame(x1=c("2018-12-01", "2019-12-01", .., x2 = .., x3=..)


  annotate(geom="text", x=as.Date("15-5-2018", format = "%d-%m-%Y"), y=.75, colour = usaid_red, label="National Targets", size= 3) +
  annotate(geom="text", x=as.Date("01-06-2021", format = "%d-%m-%Y"), y=.1, label="*home deliveries included", size =3, fontface = 'italic')

