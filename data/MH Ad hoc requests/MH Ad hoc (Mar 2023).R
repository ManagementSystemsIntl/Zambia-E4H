source("C:/Users/PIMPA.SAWULU/Desktop/R project doc_E4H/E4H-Zambia/scripts/r prep3.r")


mtnlAd_prov <- read_xls("C:/Users/PIMPA.SAWULU/Desktop/R project doc_E4H/E4H-Zambia/data/MH Ad hoc requests/Maternal Health data_Provincial level yearly.xls")
names(mtnlAd_prov)
mtnlAd_prov1 <- mtnlAd_prov %>%
  select(1,2,6)
mtnlAd_prov1
mtnlAd_prov2 <- mtnlAd_prov1 %>%
  rename(prov=1,
         yr=2,
         ins.de.cvrg=3) %>%
  mutate(ins.de.cvrgP = ins.de.cvrg/100)
mtnlAd_prov2

mtnlAd_prov3 <- mtnlAd_prov2 %>%
select(1,2,4)
mtnlAd_prov3

# mtnlAd_prov3 <- mtnlAd_prov2 %>%
#   gather(key = subRt , value = rate, c(yr, ins.de.cvrgP))
# 
# mtnlAd_prov3
  

prov_plt <- ggplot(mtnlAd_prov3, aes(x = yr, y = ins.de.cvrgP)) +
  geom_point(alpha=.6, size=.8, color=usaid_blue) + 
  #geom_area(alpha=.3, size=.8, color=usaid_blue, fill=light_blue) + 
  #geom_line(size=1) +
  stat_smooth(method = "loess", size = .8, se=FALSE) + facet_wrap(~prov) + faceted +
  scale_y_continuous(limits = c(0,1),
                     labels = percent,
                     breaks = c(.2,.4,.6,.8, 1)) +
  labs(x ="", y="", caption = "Data Source:HMIS") +
  ggtitle("Institutional delivery Coverage") +
  scale_color_manual(name= "", values = usaid_red) + baseX

prov_plt
ggsave("C:/Users/PIMPA.SAWULU/Desktop/R project doc_E4H/E4H-Zambia/graphs/Province Instituitional delivery coverage.png",
       device="png",
       type="cairo",
       height = 7.5,
       width = 12)


#'*Facility births SBA midwife/obstetrician*
mtnlAd_prov
colnames(mtnlAd_prov)
mtnlAd_prov1 <- mtnlAd_prov %>%
  select(1,2,3)
mtnlAd_prov1
mtnlAd_prov2 <- mtnlAd_prov1 %>%
  rename(prov=1,
         yr=2,
         sba.mdwf.obs=3) %>%
  mutate(sba.mdwf.obsP = sba.mdwf.obs/100)
mtnlAd_prov2

mtnlAd_prov3 <- mtnlAd_prov2 %>%
  select(1,2,4)
mtnlAd_prov3

# mtnlAd_prov3 <- mtnlAd_prov2 %>%
#   gather(key = subRt , value = rate, c(yr, ins.de.cvrgP))
# 
# mtnlAd_prov3


prov_plt <- ggplot(mtnlAd_prov3, aes(x = yr, y = sba.mdwf.obsP)) +
  geom_point(alpha=.6, size=.8, color=usaid_blue) + 
  #geom_area(alpha=.3, size=.8, color=usaid_blue, fill=light_blue) + 
  #geom_line(size=1) +
  stat_smooth(method = "loess", size = .8, se=FALSE) + facet_wrap(~prov) + faceted +
  scale_y_continuous(limits = c(0,1),
                     labels = percent,
                     breaks = c(.2,.4,.6,.8, 1)) +
  labs(x ="", y="", caption = "Data Source:HMIS") +
  ggtitle("SBA - Midwife/Obstetrician") +
  scale_color_manual(name= "", values = usaid_red) + baseX

prov_plt
ggsave("C:/Users/PIMPA.SAWULU/Desktop/R project doc_E4H/E4H-Zambia/graphs/Provincial SBA Midwife or Obs.png",
       device="png",
       type="cairo",
       height = 7.5,
       width = 12)



#'*Facility births SBA Doctor/Nurse/CO/ML*
mtnlAd_prov
colnames(mtnlAd_prov)
mtnlAd_prov1 <- mtnlAd_prov %>%
  select(1,2,4)
mtnlAd_prov1
mtnlAd_prov2 <- mtnlAd_prov1 %>%
  rename(prov=1,
         yr=2,
         sba.drnr.coml=3) %>%
  mutate(sba.drnr.comlP = sba.drnr.coml/100)
mtnlAd_prov2

mtnlAd_prov3 <- mtnlAd_prov2 %>%
  select(1,2,4)
mtnlAd_prov3

# mtnlAd_prov3 <- mtnlAd_prov2 %>%
#   gather(key = subRt , value = rate, c(yr, ins.de.cvrgP))
# 
# mtnlAd_prov3


prov_plt <- ggplot(mtnlAd_prov3, aes(x = yr, y = sba.drnr.comlP)) +
  geom_point(alpha=.6, size=.8, color=usaid_blue) + 
  #geom_area(alpha=.3, size=.8, color=usaid_blue, fill=light_blue) + 
  #geom_line(size=1) +
  stat_smooth(method = "loess", size = .8, se=FALSE) + facet_wrap(~prov) + faceted +
  scale_y_continuous(limits = c(0,1),
                     labels = percent,
                     breaks = c(.2,.4,.6,.8, 1)) +
  labs(x ="", y="", caption = "Data Source:HMIS") +
  ggtitle("SBA - Doctor/Nurse/CO/ML") +
  scale_color_manual(name= "", values = usaid_red) + baseX

prov_plt
ggsave("C:/Users/PIMPA.SAWULU/Desktop/R project doc_E4H/E4H-Zambia/graphs/Provincial SBA Doctor-Nurse-CO-ML.png",
       device="png",
       type="cairo",
       height = 7.5,
       width = 12)


#'*Skilled Personnel as % of Institutional DEL*
mtnlAd_prov
colnames(mtnlAd_prov)
mtnlAd_prov1 <- mtnlAd_prov %>%
  select(1,2,5)
mtnlAd_prov1
mtnlAd_prov2 <- mtnlAd_prov1 %>%
  rename(prov=1,
         yr=2,
         skld.prsnl.instdel=3) %>%
  mutate(skld.prsnl.instdelP = skld.prsnl.instdel/100)
mtnlAd_prov2

mtnlAd_prov3 <- mtnlAd_prov2 %>%
  select(1,2,4)
mtnlAd_prov3

# mtnlAd_prov3 <- mtnlAd_prov2 %>%
#   gather(key = subRt , value = rate, c(yr, ins.de.cvrgP))
# 
# mtnlAd_prov3


prov_plt <- ggplot(mtnlAd_prov3, aes(x = yr, y = skld.prsnl.instdelP)) +
  geom_point(alpha=.6, size=.8, color=usaid_blue) + 
  #geom_area(alpha=.3, size=.8, color=usaid_blue, fill=light_blue) + 
  #geom_line(size=1) +
  stat_smooth(method = "loess", size = .8, se=FALSE) + facet_wrap(~prov) + faceted +
  scale_y_continuous(limits = c(0,1),
                     labels = percent,
                     breaks = c(.2,.4,.6,.8, 1)) +
  labs(x ="", y="", caption = "Data Source:HMIS") +
  ggtitle("Skilled  Personnel as a % of Institutional DEL") +
  scale_color_manual(name= "", values = usaid_red) + baseX

prov_plt
ggsave("C:/Users/PIMPA.SAWULU/Desktop/R project doc_E4H/E4H-Zambia/graphs/Provincial Skilled Personnel to instituitional DEL.png",
       device="png",
       type="cairo",
       height = 7.5,
       width = 12)


#'*4th+ to Total ANC Attendance*
mtnlAd_prov
colnames(mtnlAd_prov)
mtnlAd_prov1 <- mtnlAd_prov %>%
  select(1,2,7)
mtnlAd_prov1
mtnlAd_prov2 <- mtnlAd_prov1 %>%
  rename(prov=1,
         yr=2,
         frthplus.anc=3) %>%
  mutate(frthplus.ancP = frthplus.anc/100)
mtnlAd_prov2

mtnlAd_prov3 <- mtnlAd_prov2 %>%
  select(1,2,4)
mtnlAd_prov3

# mtnlAd_prov3 <- mtnlAd_prov2 %>%
#   gather(key = subRt , value = rate, c(yr, ins.de.cvrgP))
# 
# mtnlAd_prov3


prov_plt <- ggplot(mtnlAd_prov3, aes(x = yr, y = frthplus.ancP)) +
  geom_point(alpha=.6, size=.8, color=usaid_blue) + 
  #geom_area(alpha=.3, size=.8, color=usaid_blue, fill=light_blue) + 
  #geom_line(size=1) +
  stat_smooth(method = "loess", size = .8, se=FALSE) + facet_wrap(~prov) + faceted +
  scale_y_continuous(limits = c(0,1),
                     labels = percent,
                     breaks = c(.2,.4,.6,.8, 1)) +
  labs(x ="", y="", caption = "Data Source:HMIS") +
  ggtitle("4th+ To Total ANC Attendances") +
  scale_color_manual(name= "", values = usaid_red) + baseX

prov_plt
ggsave("C:/Users/PIMPA.SAWULU/Desktop/R project doc_E4H/E4H-Zambia/graphs/Provincial 4th+ ANCs Attendances.png",
       device="png",
       type="cairo",
       height = 7.5,
       width = 12)


#'*Early PNC Coverage within 48hrs*
mtnlAd_prov
colnames(mtnlAd_prov)
mtnlAd_prov1 <- mtnlAd_prov %>%
  select(1,2,8)
mtnlAd_prov1
mtnlAd_prov2 <- mtnlAd_prov1 %>%
  rename(prov=1,
         yr=2,
         pnc.frtyeghthrs=3) %>%
  mutate(pnc.frtyeghthrsP = pnc.frtyeghthrs/100)
mtnlAd_prov2

mtnlAd_prov3 <- mtnlAd_prov2 %>%
  select(1,2,4)
mtnlAd_prov3

# mtnlAd_prov3 <- mtnlAd_prov2 %>%
#   gather(key = subRt , value = rate, c(yr, ins.de.cvrgP))
# 
# mtnlAd_prov3


prov_plt <- ggplot(mtnlAd_prov3, aes(x = yr, y = pnc.frtyeghthrsP)) +
  geom_point(alpha=.6, size=.8, color=usaid_blue) + 
  #geom_area(alpha=.3, size=.8, color=usaid_blue, fill=light_blue) + 
  #geom_line(size=1) +
  stat_smooth(method = "loess", size = .8, se=FALSE) + facet_wrap(~prov) + faceted +
  scale_y_continuous(limits = c(0,1),
                     labels = percent,
                     breaks = c(.2,.4,.6,.8, 1)) +
  labs(x ="", y="", caption = "Data Source:HMIS") +
  ggtitle("Maternal Postnatal Care within 48 hours After Delivery") +
  scale_color_manual(name= "", values = usaid_red) + baseX

prov_plt
ggsave("C:/Users/PIMPA.SAWULU/Desktop/R project doc_E4H/E4H-Zambia/graphs/Provincial Early PNC coverage (48 hours).png",
       device="png",
       type="cairo",
       height = 7.5,
       width = 12)


#'*Maternal Mortality at Facility and Community*
mtnlAd_prov
colnames(mtnlAd_prov)
mtnlAd_prov1 <- mtnlAd_prov %>%
  select(1,2,10,11)
mtnlAd_prov1
mmr <- mtnlAd_prov1 %>%
  rename(prov = 1,
         yr = 2,
         mmf = 3,
         mmc = 4
  )
colnames(mmr)

mmr_1 <- gather(mmr, key = mmtype , value = deaths, c(mmf, mmc))
mmr_1
colnames(mmr_1)

mmr_1$mmtypef <- factor(mmr_1$mmtype, levels = unique(mmr_1$mmtype))
levels(mmr_1$mmtypef)

names(mmr_1)
colnames(mmr_1)

mmr_plt <- ggplot(mmr_1, aes(x = yr, y = deaths , colour =   mmtype, linetype=mmtype)) + 
  geom_point(alpha=.6, size=.8) + 
  geom_smooth(method = "loess", size=.8, se=F) + facet_wrap(~prov) + faceted +
  scale_y_continuous(limits = c(0,220),
                     breaks = c(20,60,100,140,180,220),
                     labels = c("20","60","100","140","180","220")) +
  # scale_x_date(date_breaks = "", date_labels = "%Y")+
  scale_linetype_manual(name="",
                        labels= c("Community deaths","Health facility deaths"), 
                        values=c("solid","solid"))+
  labs(x ="", y="", caption = "Data Source:HMIS") +
  # xlab("") +
  # ylab("", caption = "Data Source:HMIS") + 
  ggtitle("Overall Maternal Mortality at Facility and Community") +
  scale_colour_manual(name = "",
                      labels= c("Community deaths","Health facility deaths"),
                      values = c(usaid_blue, medium_grey)) +
  base  
mmr_plt
ggsave("C:/Users/PIMPA.SAWULU/Desktop/R project doc_E4H/E4H-Zambia/graphs/Provincial Maternal Mortality.png",
       device="png",
       type="cairo",
       height = 7.5,
       width = 12)



#'*National Maternal Mortality Ratio*
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

mmr <- mat %>%
  rename(mmfr = 16,
         mmf = 13,
         mmc = 22
  )
colnames(mat)
# pnctrgts$Start <- as.Date(pnctrgts$Start) 
# pnctrgts$End <- as.Date(pnctrgts$End) 
# Start <- as.Date(NULL)
# End <- as.Date(NULL)

mmr_1 <- gather(mmr, key = mmtype , value = deaths, c(mmfr, mmf, mmc))
mmr_1
colnames(mmr_1)

mmr_1$mmtypef <- factor(mmr_1$mmtype, levels = unique(mmr_1$mmtype))
levels(mmr_1$mmtypef)

names(mmr_1)
colnames(mmr_1)

mmr_plt <- ggplot(mmr_1, aes(x = mnthyr, y = deaths , colour =   mmtype, linetype=mmtype)) + 
  geom_point(alpha=.5, size=.7) + 
  # geom_rect(data=pnctrgts, mapping=aes(NULL,NULL,xmin=Start,xmax=End,fill=PNCTargets),
  #           ymin=c(245,200,150,100) ,ymax=c(247,202,152,102), colour=light_grey, size=0.3, alpha=0.4, lty="solid", fill=usaid_red) +
  geom_smooth(method = loess, size=.9, se=F) +
  scale_y_continuous(limits = c(0,170),
                     breaks = c(20,60,100,140,170),
                     labels = c("20","60","100","140","170")) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")+
  scale_linetype_manual(name="",
                        labels= c("Community deaths","Health facility deaths", "Maternal mortality facility ratio (per 100,000 live births)"), 
                        values=c("solid","solid", "dashed"))+
  xlab("") +
  ylab("") +
  ggtitle("Maternal deaths and mortality ratio in facilities have increased since 2020, \nwhile community deaths numbers have decreased") +
  scale_colour_manual(name = "",
                      labels= c("Community deaths","Health facility deaths", "Maternal mortality facility ratio (per 100,000 live births)"),
                      values = c(usaid_blue, medium_grey, usaid_red)) +
  base
mmr_plt
ggsave("C:/Users/PIMPA.SAWULU/Desktop/R project doc_E4H/E4H-Zambia/graphs/National Maternal Mortality Ratio23.png",
       device="png",
       type="cairo",
       height = 7.5,
       width = 12)


#'*Provincial Maternal Mortality Ratio and Reporting Rates*
source("scripts/r prep2.r")
matprv1 <- read_xls("C:/Users/PIMPA.SAWULU/Desktop/R project doc_E4H/E4H-Zambia/data/MH Ad hoc requests/Maternal Mortality Ratio and Reporting rates_ (2019 to 2022).xls")


matprv  <- matprv1  %>%
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

matprv

names(matprv)


matprv
matprv1 <- matprv %>%
  select(1,3,4,10)

matprv1
matprv3 <- matprv1 %>%
  rename(prov = 1,
         yr = 4,
         mr = 2,
         hrr = 3)

matprv3

matprv4 <- gather(matprv3, key = variable , value = rate, c(mr, hrr))
matprv4$variable <- factor(matprv4$variable, levels = unique(matprv4$variable)) # transform into factor
levels(matprv4$variable)

matprv4

#write_xlsx(matprv3,"C:/Users/SNyimbili/OneDrive - Right to Care/Documents/RTCZ/matprv3.xlsx")


mt_plt <- ggplot(matprv4, aes(x = yr, y = rate, group = variable, colour = variable)) +
  geom_point(alpha=.9, size=1.3) +
  stat_smooth(method = "loess", size=.9, se=T) + facet_wrap(~prov) +
  faceted +
  scale_y_continuous(sec.axis = sec_axis(~ .*0.005, labels = scales::label_percent())) +
  xlab("") + 
  ylab("") +
  ggtitle("Maternal mortality Ratio and reporting rates, 2019-2022") +
  scale_color_manual(name ="",
                     values = usaid_palette,
                     labels = c("Facility Maternal mortality ratio per 100 000 deliveries", "HIA2 Reporting rate (%)")
  ) + basem

mt_plt
ggsave("C:/Users/PIMPA.SAWULU/Desktop/R project doc_E4H/E4H-Zambia/graphs/Maternal mortality Ratio and reporting rates.png",
       device="png",
       type="cairo",
       height = 7.5,
       width = 14)

#'*Bar Plot Provincial Maternal Mortality Ratio and Reporting Rates
ggplot(data=matprv3, aes(x=yr, y=mr)) +
  geom_bar(stat="identity", position=position_dodge(), fill=usaid_blue, width=119, alpha=.9) +
  geom_line(aes(x = yr, y = hrr, color=usaid_red)) +
  geom_label(aes(label=hrr)) +
  facet_wrap(~prov) +
  faceted +
  scale_y_continuous(sec.axis = sec_axis(~ .*0.006, labels = scales::label_percent())) +
  labs(x ="", y="", caption = "Data Source:HMIS") +
  # xlab("") +
  # ylab("") +
  ggtitle("Maternal mortality Ratio and reporting rates, 2019-2022") +
  scale_color_manual(name ="",
                     values = usaid_red,
                     labels = c("Facility Maternal mortality ratio per 100 000 deliveries", "HIA2 Reporting rate (%)")) +
  basem
ggsave("C:/Users/PIMPA.SAWULU/Desktop/R project doc_E4H/E4H-Zambia/graphs/Maternal mortality Ratio and reporting rates.png",
       device="png",
       type="cairo",
       height = 7.5,
       width=14)




#'*---------------FAMILY HEALTH APRIL 2023 REQUEST*'
source("scripts/r prep2.r")
source("scripts/r prep3.r")

inst.del_prov <- read_xls("data/MC Health April 2023/Institutional delivery coverage by province.xls")
inst.del_prov  <- inst.del_prov  %>%
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

inst.del_prov
colnames(inst.del_prov)

inst.del_prov1 <- inst.del_prov %>%
  select(1,2,3)
inst.del_prov1
inst.del_prov2 <- inst.del_prov1 %>%
  rename(prov=1,
         yr=2,
         ins.de.cvrg=3) %>%
  mutate(ins.de.cvrgP = ins.de.cvrg/100)
inst.del_prov2

inst.del_prov3 <- inst.del_prov2 %>%
  select(1,2,4)
inst.del_prov3



indel_plt <- ggplot(inst.del_prov3, aes(x = mnthyr, y = ins.de.cvrgP)) +
  geom_smooth(method="loess", color=usaid_blue, size=0.7,se=F) +
  geom_point(alpha=.6, size=.8, color=usaid_blue) +
  # stat_smooth(method = "loess", size = .8, se=FALSE) +
  facet_wrap(~prov) + faceted +
  scale_y_continuous(limits = c(0,1),
                     labels = percent,
                     breaks = c(.2,.4,.6,.8, 1)) +
  labs(x ="", y="", caption = "Data Source:HMIS") +
  ggtitle("Institutional delivery Coverage") +
  scale_color_manual(name= "", values = usaid_red) + baseX

# indel_plt <- ggplot(inst.del_prov3, aes(x=yr, y=ins.de.cvrgP), alpha=0.5)+ 
#   geom_smooth(method="loess", color=usaid_red, size=0.7,se=F) + 
#   stat_smooth(method = "loess", size = .8, se=FALSE) +
#   geom_point(color=usaid_red) + faceted +
#   facet_wrap(~prov) + ##scales="free_y" tom allow for independ y axis variables
#   # scale_x_date(date_labels="%b %y",date_breaks="") + 
#   labs(fill="Legend:", title="Provincial Institutional delivery Coverage",
#        x="",
#        y="", caption = "Data Source:HMIS")
indel_plt

ggsave("C:/Users/PIMPA.SAWULU/Desktop/R project doc_E4H/E4H-Zambia/graphs/Province Instituitional delivery coverage.png",
       device="png",
       type="cairo",
       height = 7.5,
       width = 12)


#'*COVERAGE OF MODERN FAMILY PLANNING ADOPTION*

fam <- read_xls("data/MC Health April 2023/Family Planning National_monthly.xls")
fam  <- fam  %>%
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

sum(fam$month_chr!=fam$month) # expecting 0 if vars same

# pnctrgts <- read_xls("data/Jan- Jun 2022/PNC Targets.xls")
# Start <- as.Date(NULL)
# End <- as.Date(NULL)



fam_prov <- read_xls("data/MC Health April 2023/Family Planning Provincial_monthly.xls")
names(fam_prov)
fam_prov
fam_prov  <- fam_prov  %>%
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

sum(fam_prov$month_chr!=fam_prov$month) # expecting 0 if vars same



#'*______COVERAGE OF MODERN FAMILY PLANNING ADOPTION Redone*


fam <- fam %>%
  rename(wmn.mfp=4,
         wmn.vstd=3) %>%
  mutate(cvrg_fp = wmn.mfp/wmn.vstd) #%>%
# relocate(cvrg_fp, .after=wmn.mfp)
colnames(fam)
crvg_plt <- ggplot(fam, aes(x=mnthyr, y=cvrg_fp, colour=usaid_blue)) + 
  geom_point(alpha=.6, size=1.5) + 
  #geom_line(size=1) +
  geom_smooth(method = loess, size = .8, se=FALSE) +
  scale_y_continuous(limits = c(0,.8),
                     labels = percent,
                     breaks = c(.1,.2,.3,.4,.5,.6,.7,.8)) +
  scale_x_date(date_labels="%b %y",date_breaks="4 months") +
  labs(x="", y="", caption="Data Source: HMIS", title="Coverage of modern family planning use among women of reproductive \nage has been increasing since 2019") +
  scale_color_manual(name ="",
                     values = usaid_blue,
                     labels ="Coverage of modern family planning adoption") + 
  baseX

crvg_plt
ggsave("viz/Apr-Jun 2022/Family Planning/Coverage of modern family planning adoption smooth PS.png",
       device="png",
       type="cairo",
       height = 6.5,
       width = 11)


#'*COVERAGE OF MODERN FAMILY PLANNING ADOPTION BY PROVINCE*

names(fam_prov)
names(fam_prov)
fam_prov <- fam_prov %>%
  rename(prov=2,
         wmn.mfp=4,
         wmn.vstd=3) %>%
  mutate(cvrg_fp = wmn.mfp/wmn.vstd)
ggplot(fam_prov, aes(x=mnthyr, y=cvrg_fp)) + 
  geom_point(size=.5, alpha=.5, colour=usaid_blue) + 
  stat_smooth(se=F, size=.8, alpha=.6, colour=usaid_blue) +
  scale_y_continuous(limits = c(0,1),
                     labels = percent,
                     breaks = c(.2,.4,.6,.8,1)) +
  labs(x ="", y="", caption = "Data Source: HMIS") +labs(x ="", y="", caption = "Data Source: HMIS") +
  ggtitle("Provincial Coverage of modern family planning utilization, 2019-2023") +
  facet_wrap(~prov, ncol=4) +
  faceted +
  scale_color_manual(values=usaid_blue) + basey

ggsave("viz/Apr-Jun 2022/Family Planning/Coverage faceted PS.png",
       device="png",
       type="cairo",
       height = 6.5,
       width = 11)

#'*________1st ANC COVERAGE 1ST TRIMESTER*

anc1_prov <- read_xls("data/MC Health April 2023/1st ANC coverage 1st trimester_Provinciall level monthly.xls")
names(anc1_prov)
anc1_prov
anc1_prov  <- anc1_prov  %>%
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

sum(fam_prov$month_chr!=fam_prov$month) # expecting 0 if vars same


names(anc1_prov)
names(anc1_prov)
anc1_prov <- anc1_prov %>%
  rename(prov=1,
         fANC.fTm=3) %>%
  mutate(fANC.fTmP = fANC.fTm/100)
ggplot(anc1_prov, aes(x=mnthyr, y=fANC.fTmP)) + 
  geom_point(size=.5, alpha=.5, colour=usaid_blue) + 
  stat_smooth(se=F, size=.8, alpha=.6, colour=usaid_blue) +
  scale_y_continuous(limits = c(0,1),
                     labels = percent,
                     breaks = c(.2,.4,.6,.8,1)) +
  labs(x ="", y="", caption = "Data Source: HMIS") +labs(x ="", y="", caption = "Data Source: HMIS") +
  ggtitle("Provincial 1st ANC Coverage (1st Trimester), 2019-2023") +
  facet_wrap(~prov, ncol=4) +
  faceted +
  scale_color_manual(values=usaid_blue) + basey

ggsave("viz/Apr-Jun 2022/Family Planning/1st ANC TM1 Coverage faceted PS.png",
       device="png",
       type="cairo",
       height = 6.5,
       width = 11)

#'*________4th+ TO TOTAL ANC ATTENDANCES*

frthPlusANC_prov <- read_xls("data/May 2023 FHDR/4th+ to Total ANC Attendance_provincial level.xls")
names(frthPlusANC_prov)
frthPlusANC_prov
frthPlusANC_prov  <- frthPlusANC_prov  %>%
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

sum(fam_prov$month_chr!=fam_prov$month) # expecting 0 if vars same


names(frthPlusANC_prov)
names(frthPlusANC_prov)
frthPlusANC_prov <- frthPlusANC_prov %>%
  rename(prov=1,
         frth.anc=3) %>%
  mutate(frth.ancP = frth.anc/100)
ggplot(frthPlusANC_prov, aes(x=mnthyr, y=frth.ancP)) + 
  geom_point(size=.5, alpha=.5, colour=usaid_blue) + 
  stat_smooth(se=F, size=.8, alpha=.6, colour=usaid_blue) +
  scale_y_continuous(limits = c(0,1),
                     labels = percent,
                     breaks = c(.2,.4,.6,.8,1)) +
  labs(x ="", y="", caption = "Data Source: HMIS") +labs(x ="", y="", caption = "Data Source: HMIS") +
  ggtitle("4th+ to Total ANC attendances, 2019 - 2023") +
  facet_wrap(~prov, ncol=4) +
  faceted +
  scale_color_manual(values=usaid_blue) + basey

ggsave("viz/May 2023 data review/4th+ to Total ANC attendances faceted.png",
       device="png",
       type="cairo",
       height = 6.5,
       width = 11)



#'*_______Redraw for National Level*

frthPlusANC <- read_xls("data/May 2023 FHDR/4th+ to Total ANC Attendance_National level.xls")
names(frthPlusANC)
frthPlusANC
frthPlusANC  <- frthPlusANC  %>%
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


names(frthPlusANC)
frthPlusANC <- frthPlusANC %>%
  rename(frth.anc=3) %>%
  mutate(frth.ancP = frth.anc/100)
ggplot(frthPlusANC, aes(x=mnthyr, y=frth.ancP)) + 
  geom_point(size=.7, alpha=.5, colour=usaid_blue) + 
  stat_smooth(se=F, size=.9, alpha=.6, colour=usaid_blue) +
  scale_y_continuous(limits = c(0,1),
                     labels = percent,
                     breaks = c(.2,.4,.6,.8,1)) +
  labs(x ="", y="", caption = "Data Source: HMIS") +labs(x ="", y="", caption = "Data Source: HMIS") +
  ggtitle("The Trends for the 4th+ to Total ANC attendances has been below 30% but above 20% \nat National Level") +
  scale_color_manual(values=usaid_blue) + basey

ggsave("viz/May 2023 data review/National 4th+ to Total ANC attendances.png",
       device="png",
       type="cairo",
       height = 6.5,
       width = 12)




#'*________MATERNAL POSTNATAL CARE WITHIN 48HRS*

MatPNC_prov <- read_xls("data/May 2023 FHDR/Reproductive Maternal Health_Provincial level monthly.xls")
names(MatPNC_prov)
MatPNC_prov
MatPNC_prov  <- MatPNC_prov  %>%
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

names(MatPNC_prov)
MatPNC_prov <- MatPNC_prov %>%
  rename(prov=1,
         MatPNC=11) %>%
  mutate(MatPNCP = MatPNC/100)
ggplot(MatPNC_prov, aes(x=mnthyr, y=MatPNCP)) + 
  geom_point(size=.5, alpha=.5, colour=usaid_blue) + 
  stat_smooth(se=F, size=.8, alpha=.6, colour=usaid_blue) +
  scale_y_continuous(limits = c(0,1),
                     labels = percent,
                     breaks = c(.2,.4,.6,.8,1)) +
  labs(x ="", y="", caption = "Data Source: HMIS") +labs(x ="", y="", caption = "Data Source: HMIS") +
  ggtitle("Maternal Postnatal Care within 48 hours After Delivery, 2019 - 2023") +
  facet_wrap(~prov, ncol=4) +
  faceted +
  scale_color_manual(values=usaid_blue) + basey

ggsave("viz/May 2023 data review/Maternal Postnatal 48 faceted.png",
       device="png",
       type="cairo",
       height = 6.5,
       width = 11)

#'*_______Redraw for National Level*


MatPNC <- read_xls("data/May 2023 FHDR/Reproductive Maternal Health_National level monthly.xls")
names(MatPNC)
MatPNC
MatPNC  <- MatPNC  %>%
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

names(MatPNC)
MatPNC <- MatPNC %>%
  rename(MatPNC=11) %>%
  mutate(MatPNCP = MatPNC/100)
ggplot(MatPNC, aes(x=mnthyr, y=MatPNCP)) + 
  geom_point(size=.7, alpha=.5, colour=usaid_blue) + 
  stat_smooth(se=F, size=.9, alpha=.6, colour=usaid_blue) +
  scale_y_continuous(limits = c(0,1),
                     labels = percent,
                     breaks = c(.2,.4,.6,.8,1)) +
  labs(x ="", y="", caption = "Data Source: HMIS") +labs(x ="", y="", caption = "Data Source: HMIS") +
  ggtitle("Maternal Postnatal Care within 48 hours After Delivery has been on an upward \ntrajectory since 2019 and now at a record high of above 63% in 2023!") +
  scale_color_manual(values=usaid_blue) + basey

ggsave("viz/May 2023 data review/National Maternal Postnatal 48hr Care.png",
       device="png",
       type="cairo",
       height = 6.5,
       width = 11)



#'*________INSTITUTIONAL DELIVERY COVERAGE*

InstDel_prov <- read_xls("data/May 2023 FHDR/Reproductive Maternal Health_Provincial level monthly.xls")
names(InstDel_prov)
InstDel_prov
InstDel_prov  <- InstDel_prov  %>%
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

# sum(fam_prov$month_chr!=fam_prov$month) # expecting 0 if vars same


names(InstDel_prov)
names(InstDel_prov)
InstDel_prov <- InstDel_prov %>%
  rename(prov=1,
         InstDel=8) %>%
  mutate(InstDelP = InstDel/100)
ggplot(InstDel_prov, aes(x=mnthyr, y=InstDelP)) + 
  geom_point(size=.5, alpha=.5, colour=usaid_blue) + 
  stat_smooth(se=F, size=.8, alpha=.6, colour=usaid_blue) +
  scale_y_continuous(limits = c(0,1),
                     labels = percent,
                     breaks = c(.2,.4,.6,.8,1)) +
  labs(x ="", y="", caption = "Data Source: HMIS") +labs(x ="", y="", caption = "Data Source: HMIS") +
  ggtitle("Institutional Delivery Coverage, 2019 - 2023") +
  facet_wrap(~prov, ncol=4) +
  faceted +
  scale_color_manual(values=usaid_blue) + basey

ggsave("viz/May 2023 data review/Provincial Institutional delivery coverage.png",
       device="png",
       type="cairo",
       height = 6.5,
       width = 11)

#'*_______Redraw for National Level*

InstDel <- read_xls("data/May 2023 FHDR/Reproductive Maternal Health_National level monthly.xls")
names(InstDel)
InstDel
InstDel  <- InstDel  %>%
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


names(InstDel)
InstDel <- InstDel %>%
  rename(InstDel=8) %>%
  mutate(InstDelP = InstDel/100)
ggplot(InstDel, aes(x=mnthyr, y=InstDelP)) + 
  geom_point(size=.5, alpha=.5, colour=usaid_blue) + 
  stat_smooth(se=F, size=.8, alpha=.6, colour=usaid_blue) +
  scale_y_continuous(limits = c(0,1),
                     labels = percent,
                     breaks = c(.1,.2,.3,.4,.5,.6,.7,.8,.9,1)) +
  labs(x ="", y="", caption = "Data Source: HMIS") +labs(x ="", y="", caption = "Data Source: HMIS") +
  ggtitle("Institutional Delivery Coverage has been below 80% since mid 2019 at National level.") +
  scale_color_manual(values=usaid_blue) + basey

ggsave("viz/May 2023 data review/National Institutional delivery coverage.png",
       device="png",
       type="cairo",
       height = 6.5,
       width = 11)


#'*__________CAESAREAN SECTION RATE*

csection_prov <- read_xls("data/May 2023 FHDR/Reproductive Maternal Health_Provincial level monthly.xls")
names(csection_prov)
csection_prov
csection_prov  <- csection_prov  %>%
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


names(csection_prov)
csection_prov <- csection_prov %>%
  rename(prov=1,
         csectrate=9) %>%
  mutate(csectrateP = csectrate/100)
ggplot(csection_prov, aes(x=mnthyr, y=csectrateP)) + 
  geom_point(size=.5, alpha=.5, colour=usaid_blue) + 
  stat_smooth(se=F, size=.7, alpha=.6, colour=usaid_blue) +
  scale_y_continuous(limits = c(0,.6),
                     labels = percent,
                     breaks = c(.1,.2,.3,.4,.5,.6)) +
  labs(x ="", y="", caption = "Data Source: HMIS") +labs(x ="", y="", caption = "Data Source: HMIS") +
  ggtitle("Caesarean Section Rate (%), 2019 - 2023") +
  facet_wrap(~prov, ncol=4) +
  faceted +
  scale_color_manual(values=usaid_blue) + basey

ggsave("viz/May 2023 data review/Provincial Caesarean Rate.png",
       device="png",
       type="cairo",
       height = 6.5,
       width = 11)


#'*_______Redraw for National Level*

Csect <- read_xls("data/May 2023 FHDR/Reproductive Maternal Health_National level monthly.xls")
names(Csect)
Csect
Csect  <- Csect  %>%
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


names(Csect)
Csect <- Csect %>%
  rename(Csect.rate=9) %>%
  mutate(Csect.rateP = Csect.rate/100)
ggplot(Csect, aes(x=mnthyr, y=Csect.rateP)) + 
  geom_point(size=.5, alpha=.5, colour=usaid_blue) + 
  stat_smooth(se=F, size=.8, alpha=.6, colour=usaid_blue) +
  scale_y_continuous(limits = c(0,.6),
                     labels = percent,
                     breaks = c(.1,.2,.3,.4,.5,.6)) +
  labs(x ="", y="", caption = "Data Source: HMIS") +labs(x ="", y="", caption = "Data Source: HMIS") +
  ggtitle("Caesarean Section Rate at National level has been steadily increasing each year.") +
  scale_color_manual(values=usaid_blue) + basey

ggsave("viz/May 2023 data review/National CSection rate.png",
       device="png",
       type="cairo",
       height = 6.5,
       width = 11)


#'*__________FOLIC ACID SUPPLEMENTATION (%) DURING ANC VISITS*

folicsup_prov <- read_xls("data/May 2023 FHDR/Reproductive Maternal Health_Provincial level monthly.xls")
names(folicsup_prov)
folicsup_prov
folicsup_prov  <- folicsup_prov  %>%
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


names(folicsup_prov)
folicsup_prov <- folicsup_prov %>%
  rename(prov=1,
         folic=6) %>%
  mutate(folicP = folic/100)
ggplot(folicsup_prov, aes(x=mnthyr, y=folicP)) + 
  geom_point(size=.5, alpha=.5, colour=usaid_blue) + 
  stat_smooth(se=F, size=.7, alpha=.6, colour=usaid_blue) +
  scale_y_continuous(limits = c(0,1),
                     labels = percent,
                     breaks = c(.1,.2,.3,.4,.5,.6,.7,.8,.9,1)) +
  labs(x ="", y="", caption = "Data Source: HMIS") +labs(x ="", y="", caption = "Data Source: HMIS") +
  ggtitle("Folic Acid Supplementation (%) during ANC Visits shows that there was a dip between 2019 and 2022\n across the provinces except Northern, but shows a general steady increase into 2023.") +
  facet_wrap(~prov, ncol=4) +
  faceted +
  scale_color_manual(values=usaid_blue) + basey

ggsave("viz/May 2023 data review/Folic Acid Supplementation.png",
       device="png",
       type="cairo",
       height = 7,
       width = 12)


#'*_______Redraw for National Level*

folicAcid <- read_xls("data/May 2023 FHDR/Reproductive Maternal Health_National level monthly.xls")
names(folicAcid)
folicAcid
folicAcid  <- folicAcid  %>%
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


names(folicAcid)
folicAcid <- folicAcid %>%
  rename(folicSupp=6) %>%
  mutate(folicSuppP = folicSupp/100)
ggplot(folicAcid, aes(x=mnthyr, y=folicSuppP)) + 
  geom_point(size=.6, alpha=.5, colour=usaid_blue) + 
  stat_smooth(se=F, size=.8, alpha=.6, colour=usaid_blue) +
  scale_y_continuous(limits = c(0,1),
                     labels = percent,
                     breaks = c(.1,.2,.3,.4,.5,.6,.7,.8,.9,1)) +
  labs(x ="", y="", caption = "Data Source: HMIS") +labs(x ="", y="", caption = "Data Source: HMIS") +
  ggtitle("Folic Acid Supplementation (%) during ANC Visits, 2019 - 2023.") +
  scale_color_manual(values=usaid_blue) + basey

ggsave("viz/May 2023 data review/National Folic Acid Sup.png",
       device="png",
       type="cairo",
       height = 6.5,
       width = 11)



#'*__________HIGH RISK PREGNANCIES AT 1st ANC (%)*

highRpreg_prov <- read_xls("data/May 2023 FHDR/Reproductive Maternal Health_Provincial level monthly.xls")
names(highRpreg_prov)
highRpreg_prov
highRpreg_prov  <- highRpreg_prov  %>%
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


names(highRpreg_prov)
highRpreg_prov <- highRpreg_prov %>%
  rename(prov=1,
         risk.preg=12) %>%
  mutate(risk.pregP = risk.preg/100)
ggplot(highRpreg_prov, aes(x=mnthyr, y=risk.pregP)) + 
  geom_point(size=.5, alpha=.5, colour=usaid_red) + 
  stat_smooth(se=F, size=.7, alpha=.6, colour=usaid_red) +
  scale_y_continuous(limits = c(0,.8),
                     labels = percent,
                     breaks = c(.1,.2,.3,.4,.5,.6,.7,.8)) +
  labs(x ="", y="", caption = "Data Source: HMIS") +labs(x ="", y="", caption = "Data Source: HMIS") +
  ggtitle("High-risk Pregnancies at 1st ANC (%), 2019 - 2023.") +
  facet_wrap(~prov, ncol=4) +
  faceted +
  scale_color_manual(values=usaid_red) + basey

ggsave("viz/May 2023 data review/High Risk pregnancies.png",
       device="png",
       type="cairo",
       height = 7,
       width = 12)



#'*_______Redraw for National Level*

highRpreg <- read_xls("data/May 2023 FHDR/Reproductive Maternal Health_National level monthly.xls")
names(highRpreg)
highRpreg
highRpreg  <- highRpreg  %>%
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


names(highRpreg)
highRpreg <- highRpreg %>%
  rename(hr.preg=12) %>%
  mutate(hr.pregP = hr.preg/100)
ggplot(highRpreg, aes(x=mnthyr, y=hr.pregP)) + 
  geom_point(size=.6, alpha=.5, colour=usaid_red) + 
  stat_smooth(se=F, size=.8, alpha=.6, colour=usaid_red) +
  scale_y_continuous(limits = c(0,.8),
                     labels = percent,
                     breaks = c(.1,.2,.3,.4,.5,.6,.7,.8)) +
  labs(x ="", y="", caption = "Data Source: HMIS") +labs(x ="", y="", caption = "Data Source: HMIS") +
  ggtitle("Number of High-Risk pregancies reported at 1st ANC visit\n have been increasing each year and now stand at 15% in 2023.") +
  scale_color_manual(values=usaid_red) + basey

ggsave("viz/May 2023 data review/National High risk pregnancies.png",
       device="png",
       type="cairo",
       height = 6.5,
       width = 11)






#'*Maternal neonatal health indicators*
#'*April 2023*
#'*ANC All Trimesters 2019 - 2023*

mat <- read_xls("data/May 2023 FHDR/Reproductive Maternal Health_National level monthly.xls")
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

# pnctrgts <- read_xls("data/Jan- Jun 2022/PNC Targets.xls")
# Start <- as.Date(NULL)
# End <- as.Date(NULL)


# matq <- read_xls("data/Jan- Jun 2022/Reproductive Maternal Data_National Level(Quarterly).xls")
# matqp <- read_xls("data/Jan-Mar 2022/Reproductive Maternal Data_Provincial Level(Quarterly).xls")
mat_prov <- read_xls("data/May 2023 FHDR/Reproductive Maternal Health_Provincial level monthly.xls")

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
  geom_point(alpha=.6, size=1.5) + 
  #geom_line(size=1) +
  geom_smooth(method = loess, size = .8, se=FALSE) +
  scale_y_continuous(limits = c(0,1),
                     labels = percent,
                     breaks = c(.1,.2,.3,.4,.5,.6,.7,.8,.9, 1)) +
  scale_x_date(date_labels="%b %Y",date_breaks="3 months") +
  labs(x ="", y="", caption = "Data Source: HMIS") +labs(x ="", y="", caption = "Data Source: HMIS") +
  # xlab("", caption = "Data Source: HMIS") + 
  # ylab("") +
  ggtitle("Proportion of expected pregnancies receiving Antenatal Care (ANC), 2019 - 2023") +
  scale_color_manual(name ="",
                     values = usaid_palette,
                     labels = c("1st ANC coverage (all trimesters)", "1st ANC Coverage (1st Trimester)", 
                                "1st ANC visits in the 1st trimester: Women <20 yrs")
  ) + 
  base

ggsave("viz/May 2023 data review/National Proportion of expected pregnancies receiving antenatal care.png",
       device="png",
       type="cairo",
       height = 6.5,
       width = 12)

#'*______________________Child Health INDICATORS*


chldH <- read_xls("data/MC Health April 2023/Child Heath national level_monthly.xls")
chldH  <- chldH  %>%
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

# sum(chldH$month_chr!=chldH$month) # expecting 0 if vars same

# pnctrgts <- read_xls("data/Jan- Jun 2022/PNC Targets.xls")
# Start <- as.Date(NULL)
# End <- as.Date(NULL)


# chldHq <- read_xls("data/Jan- Jun 2022/Child Health Data_National Level(Quarterly).xls")
# chldHqp <- read_xls("data/Jan-Mar 2022/Child Health Data_Provincial Level(Quarterly).xls")
chldH_prov <- read_xls("data/MC Health April 2023/Child Heath provincial level_monthly.xls")
names(chldH_prov)
# chldH_provpimpa <- chldH_prov %>%
#   select(1,2,21)
# chldH_provpimpa <- chldH_provpimpa %>%
#   rename(me=1,
#          you=2,
#          there=3)
# 
# chldH_provpimpa
# 
# chldH_provpimpa 
chldH_prov  <- chldH_prov  %>%
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

sum(chldH_prov$month_chr!=chldH_prov$month) # expecting 0 if vars same




#'*IMMUNIZATION*

#'* MEASLES 1 & 2*
names(chldH)
# view(chldH)
chldH <- chldH %>%
  rename(msles1 = 11,
         msles2 = 12
  ) %>%
  
  mutate(msles1p = msles1/100,
         msles2p = msles2/100)

#'*set msles1p & msles2p to 100 for all values >100*
chldH <- chldH %>% 
  dplyr::mutate(ancc = ifelse(msles1 > 100, 100, msles1)) %>% 
  dplyr::mutate(msles1p = msles1/100)

#'*To create legend, gather method for including a legend --*

chldH <- gather(chldH, key = subpop , value = rate, c(msles1p, msles2p))
chldH$subpop <- factor(chldH$subpop, levels = unique(chldH$subpop)) # transform into factor
levels(chldH$subpop)

# view(chldH)

msles_plt <- ggplot(chldH, aes(x = mnthyr, y = rate, group = subpop, colour = subpop)) +
  geom_point(alpha=.6, size=.6) + 
  #geom_line(size=1) +
  geom_smooth(method = loess, size = .8, se=FALSE) +
  scale_y_continuous(limits = c(0,1),
                     labels = percent,
                     breaks = c(.1,.2,.3,.4,.5,.6,.7,.8,.9, 1)) +
  scale_x_date(date_labels="%b %y",date_breaks="4 months") +
  labs(x="", y="", caption="Data Source: HMIS", title="Coverage of the first measles vaccine dose has slightly decreased since June 2021, \nwhile the coverage the 2nd dose has slightly increased") +
  scale_color_manual(name ="",
                     values = usaid_palette,
                     labels = c("Measles under 1", "Measles under 2")
  ) + 
  basem
msles_plt
ggsave("viz/Apr-Jun 2022/Child Health/Proportion of infacts receiving Measles Vaccines 1and2.png",
       device="png",
       type="cairo",
       height = 5.5,
       width = 9)


#'* MEASLES 1 & 2 SEPERATED BY PROVINCES* ----
names(chldH_prov)

chldH_prov <- chldH_prov %>%
  rename(prov = 1,
         msles1 = 11,
         msles2 = 12
  ) %>%
  
  mutate(msles1p = msles1/100,
         msles2p = msles2/100) %>%
  
  #'*set msles1p & msles2p to 100 for all values >100*
  chldH_prov <- chldH_prov %>% 
  dplyr::mutate(ancc = ifelse(msles1 > 100, 100, msles1)) %>% 
  dplyr::mutate(msles1p = msles1/100)

#'*To create legend, gather method for including a legend --*

chldH_prov <- gather(chldH_prov, key = subpop , value = rate, c(msles1p, msles2p))
chldH_prov$subpop <- factor(chldH_prov$subpop, levels = unique(chldH_prov$subpop)) # transform into factor
levels(chldH_prov$subpop)

# view(chldH)

mslesProv_plt <- ggplot(chldH_prov, aes(x = mnthyr, y = rate, group = subpop, colour = subpop)) +
  geom_point(alpha=.6, size=.6) + 
  #geom_line(size=1) +
  geom_smooth(method = loess, size = .8, se=FALSE) +
  facet_wrap(~prov) +
  faceted +
  scale_y_continuous(limits = c(0,1),
                     labels = percent,
                     breaks = c(.1,.2,.3,.4,.5,.6,.7,.8,.9, 1)) +
  scale_x_date(date_labels="%b %y",date_breaks="4 months") +
  labs(x="", y="", caption="Data Source: HMIS", title="Coverage of the first and second measles vaccines") +
  scale_color_manual(name ="",
                     values = usaid_palette,
                     labels = c("Measles under 1", "Measles under 2")
  ) + 
  basem
mslesProv_plt

ggsave("viz/Apr-Jun 2022/Child Health/Provincial Proportion of infants receiving Measles Vaccines faceted PS.png",
       device="png",
       type="cairo",
       height = 6.5,
       width = 12)

#'*Re-look at the above section*

#'*___________FULLY IMMUNIZED Coverage*
names(chldH)
# view(chldH)
fullyimz <- chldH %>%
  rename(fic = 7) %>%
  
  mutate(ficp = fic/100)

#'*setting values of dpt 1st dose to 100 for all values >100*
fullyimz1 <- fullyimz %>% 
  dplyr::mutate(fic = ifelse(fic > 100, 100, fic)) %>% 
  dplyr::mutate(ficp = fic/100)

#'*To create legend, gather method for including a legend --*

fullyimz1 <- gather(fullyimz1, key = subpop , value = rate, c(fic))
fullyimz1$subpop <- factor(fullyimz1$subpop, levels = unique(fullyimz1$subpop)) # transform into factor
levels(fullyimz1$subpop)

full_plt <- ggplot(fullyimz1, aes(x = mnthyr, y = ficp, colour=usaid_blue )) +
  geom_point(alpha=.4, size=1.9) + 
  #geom_line(size=1) +
  geom_smooth(method = loess, size = .8, se=FALSE) +
  scale_y_continuous(limits = c(0,1),
                     labels = percent,
                     breaks = c(.1,.2,.3,.4,.5,.6,.7,.8,.9, 1)) +
  scale_x_date(date_labels="%b %y",date_breaks="4 months") +
  labs(x="", y="", caption="Data Source: HMIS", title="The proportion of infants that are fully immunized has been constant in the \npast years, but we are seeing a slight decline begining late 2022") +
  scale_color_manual(name ="",
                     values = usaid_blue,
                     labels = "Fully Immunized") + 
  basem 
  
full_plt

ggsave("viz/Apr-Jun 2022/Child Health/Fully immunised coverage PS23.png",
       device="png",
       type="cairo",
       height = 5.5,
       width = 9)


#'* EBF @6MONTHS & INITIATION ON BREASTMILK WITHIN ONE HOUR OF BIRTH*

names(chldH)
# view(chldH)
breastfeed <- chldH %>%
  rename(brst1hr = 18,
         ebf = 19
  ) %>%
  
  mutate(brst1hrp = brst1hr/100,
         ebfp = ebf/100)

#'*set EBF & BREASTFEED 1HOUR to 100 for all values >100*
breastfeed <- breastfeed %>% 
  dplyr::mutate(brst1hr = ifelse(brst1hr > 100, 100, brst1hr)) %>% 
  dplyr::mutate(brst1hrp = brst1hr/100)

#'*To create legend, gather method for including a legend --*

breastfeed <- gather(breastfeed, key = subpop , value = rate, c(brst1hrp, ebfp))
breastfeed$subpop <- factor(breastfeed$subpop, levels = unique(breastfeed$subpop)) # transform into factor
levels(breastfeed$subpop)

# pnctrgts$Start <- as.Date(pnctrgts$Start) 
# pnctrgts$End <- as.Date(pnctrgts$End) 
# Start <- as.Date(NULL)
# End <- as.Date(NULL)
# colnames(breastfeed)
# view(chldH)

brstfeeding_plt <- ggplot(breastfeed, aes(x = mnthyr, y = rate, group = subpop, colour = subpop)) +
  # geom_rect(data=pnctrgts, aes(NULL,NULL,xmin=Start,xmax=End,fill=PNCTargets),
  #           ymin=c(.79,.85,.90,.96) ,ymax=c(.80,.86,.91,.97), colour=light_grey, size=0.8, alpha=0.8, lty="solid", fill=usaid_red) +
  #geom_area(alpha=.3, size=.8,color=usaid_blue, fill=light_blue) +
  geom_point(alpha=.6, size=1.5) + 
  #geom_line(size=1) +
  geom_smooth(method = loess, size = .8, se=FALSE) +
  scale_y_continuous(limits = c(0,1),
                     labels = percent,
                     breaks = c(.1,.2,.3,.4,.5,.6,.7,.8,.9, 1)) +
  scale_x_date(date_labels="%b %y",date_breaks="4 months") +
  labs(x="", y="", caption="Data Source: HMIS", title="Proportion of infants breastfed within 1 hour of birth has slightly increased \nin the last years, reaching 90% in June 2022") +
  scale_color_manual(name ="",
                     values = usaid_palette,
                     labels = c("Initiation on breastmilk with one hour of birth", "Infants on EBF at 6 months")) + basey
#     basey +
#     annotate(geom = "text", x=trgt1, y = 0.79, family="Gill Sans Mt", colour = usaid_red, label=substitute("Targets Breastfeeding within 1 hour
# of birth"), size= 4,  hjust =0, vjust=-4.5) +
#     annotate(geom = "text", x=trgt1, y = 0.79, family="Gill Sans Mt", colour = rich_black, label=substitute(paste(bold("79%"))), size= 4,  hjust =-2.5, vjust=-.2) +
#     annotate(geom = "text", x=trgt1, y = 0.85, family="Gill Sans Mt", colour = rich_black, label=substitute(paste(bold("85%"))), size= 4,  hjust =-9.5, vjust=-1) +
#     annotate(geom = "text", x=trgt1, y = 0.90, family="Gill Sans Mt", colour = rich_black, label=substitute(paste(bold("90%"))), size= 4,  hjust =-15.5, vjust=-1) +
#     annotate(geom = "text", x=trgt1, y = 0.96, family="Gill Sans Mt", colour = rich_black, label=substitute(paste(bold("96%"))), size= 4,  hjust =-22, vjust=-1)
#   

brstfeeding_plt

ggsave("viz/Apr-Jun 2022/Child Health/National EBF and 1hr BF PS.png",
       device="png",
       type="cairo",
       height = 5.5,
       width = 9.5)


#'*___________CHILD STUNTING LEVELS*
names(chldH)
# view(chldH)
chldstunt <- chldH %>%
  rename(csl = 16) %>%
  
  mutate(cslp = csl/100)

#'*setting values of stunting to 100 for all values >100*
chldstunt1 <- chldstunt %>% 
  dplyr::mutate(csl = ifelse(csl > 100, 100, csl)) %>% 
  dplyr::mutate(cslp = csl/100)

#'*To create legend, gather method for including a legend --*

chldstunt1 <- gather(chldstunt1, key = subpop , value = rate, c(csl))
chldstunt1$subpop <- factor(chldstunt1$subpop, levels = unique(chldstunt1$subpop)) # transform into factor
levels(chldstunt1$subpop)

stunt_plt <- ggplot(chldstunt1, aes(x = mnthyr, y = cslp, colour=usaid_blue )) +
  geom_point(alpha=.4, size=1.9) + 
  #geom_line(size=1) +
  geom_smooth(method = loess, size = .8, se=FALSE) +
  scale_y_continuous(limits = c(0,1),
                     labels = percent,
                     breaks = c(.1,.2,.3,.4,.5,.6,.7,.8,.9, 1)) +
  scale_x_date(date_labels="%b %y",date_breaks="4 months") +
  labs(x="", y="", caption="Data Source: HMIS", title="The Stunting Rates in Under 5s at Facility have been below 2%") +
  scale_color_manual(name ="",
                     values = usaid_blue,
                     labels = "Stunting Rates") + 
  basem 
stunt_plt

ggsave("viz/Apr-Jun 2022/Child Health/National Stunting Levels PS23.png",
       device="png",
       type="cairo",
       height = 5.5,
       width = 9)

