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
ggsave("C:/Users/PIMPA.SAWULU/Desktop/R project doc_E4H/E4H-Zambia/graphs/National Maternal Mortality Ratio.png",
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

inst.del_prov <- read_xls("data/MC Health April 2023/Institutional delivery coverage by province.xlsx")
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

prov_plt <- ggplot(inst.del_prov3, aes(x = yr, y = ins.de.cvrgP)) +
  geom_point(alpha=.6, size=.8, color=usaid_blue) + 
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
