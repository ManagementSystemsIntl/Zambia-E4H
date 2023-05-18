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
  geom_point(alpha=.5, size=.7) + 
  geom_smooth(method = loess, size=.9, se=F) + facet_wrap(~prov) + faceted +
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
  ggtitle("Overall Maternal Mortality") +
  scale_colour_manual(name = "",
                      labels= c("Community deaths","Health facility deaths"),
                      values = c(usaid_blue, medium_grey)) +
  base  
mmr_plt
ggsave("C:/Users/PIMPA.SAWULU/Desktop/R project doc_E4H/E4H-Zambia/graphs/Provincial Maternal Mortality Ratio.png",
       device="png",
       type="cairo",
       height = 7.5,
       width = 12)




#'*National Maternal Mortality Ratio*
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

mmr <- mat %>%
  rename(mmfr = 16
  )
colnames(mat)

mmr_1 <- gather(mmr, key = mmtype , value = deaths, mmfr)
mmr_1
colnames(mmr_1)

mmr_1$mmtypef <- factor(mmr_1$mmtype, levels = unique(mmr_1$mmtype))
levels(mmr_1$mmtypef)

names(mmr_1)
colnames(mmr_1)

mmr_plt <- ggplot(mmr_1, aes(x = mnthyr, y = deaths , colour =   mmtype, linetype=mmtype)) + 
  geom_point(alpha=.5, size=.7) + 
  geom_smooth(method = loess, size=.9, se=F) +
  scale_y_continuous(limits = c(0,170),
                     breaks = c(20,60,100,140,170),
                     labels = c("20","60","100","140","170")) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")+
  scale_linetype_manual(name="",
                        labels= ("Maternal mortality facility ratio (per 100,000 live births)"), 
                        values=("solid"))+
  xlab("") +
  ylab("") +
  ggtitle("Maternal deaths and mortality ratio in facilities have increased since 2020, \nwhile community deaths numbers have decreased") +
  scale_colour_manual(name = "",
                      labels= "Maternal mortality facility ratio (per 100,000 live births",
                      values =  usaid_red) +
  baseX
mmr_plt
ggsave("C:/Users/PIMPA.SAWULU/Desktop/R project doc_E4H/E4H-Zambia/graphs/National Maternal Mortality Ratio23.png",
       device="png",
       type="cairo",
       height = 7.5,
       width = 12)

