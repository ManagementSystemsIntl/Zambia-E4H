
source("scripts/r prep2.r")
source("scripts/r prep3.r")



#'*Perinatal deaths, Fresh Still & Macerated Stillbirths*
pr.mr.st <- read_xlsx("data/Aug 2023 MHDR/provincial FSB MSB_monthly.xlsx")

pr.mr.st

pr.mr.st$Month <- as.Date(pr.mr.st$Month)

pr.mr.st


pr.mr.st2 <- pr.mr.st %>%
  rename(prov=1,
         mth=2,
         frsh.stlbrth.Rt=7,
         mcrtd.brth.Rt=6)

pr.mr.st2

pr.mr.st2
frsh.stillmacerbirth <- pr.mr.st2 %>%
  select(1,2,6,7)

frsh.stillmacerbirth


frsh.stillmacerbirth <- frsh.stillmacerbirth %>% 
  gather(key = subRt , value = rate, c(frsh.stlbrth.Rt, mcrtd.brth.Rt))

frsh.stillmacerbirth

ggplot(frsh.stillmacerbirth, aes(x = mth, y = rate, group = subRt, fill = subRt), alpha=0.6) +
  geom_area(alpha=.8, position = position_dodge()) +
  scale_y_continuous(limits = c(0,8),
                     breaks = c(0,2,4,6,8)) +
  xlab("") + 
  ylab("Rate") +
  ggtitle("Fresh stillbirth and Macerated stillbirth per 1000 live births (Oct 2022 - Jul 2023.)") +
  # facet_wrap(~prov, ncol=4) +
  # faceted +
  scale_x_date(date_labels="%b %y",date_breaks="1 month") +
  scale_fill_manual(name ="",
                    values = c(usaid_red,usaid_blue),labels = c("Macerated Stillbirth","Fresh Stillbirth")) + base

ggsave("viz/Aug 23 FHDR/stillbirths Sept National.png",
       device="png",
       type="cairo",
       height = 6.5,
       width = 12)








names(fam)
newAccpt <- fam %>%
  select(24:27,56) %>%
  na.omit() 

newAccpt
colnames(newAccpt)
newAccpt1 <- newAccpt[, c(4,1,2,3,5)]
colnames(newAccpt1)
newAccpt1

newAccpt1

newAccpt2 <- newAccpt1 %>%
  select(5,1,2,3,4) %>%
  na.omit()
names(newAccpt2)


newAccpt3 <- reshape2::melt(newAccpt2, id = "mnthyr")

Accpt_plt <- ggplot(newAccpt3,aes(x=mnthyr, y=value, color=variable))+
  geom_point(alpha=.6, size=1.4) +
  geom_smooth(method =loess,se=F, size=1.1, alpha=.8) +
  scale_x_date(date_labels="%b %y",date_breaks="2 months") +
  scale_y_continuous(labels=comma) +
  labs(x="",
       y="",
       caption="Data Source: HMIS",
       title="Family Planning New Acceptors Starting FP disaggregated by Age (Jan 2020 - Sept 2023).") +
  basey + scale_color_manual(name ="",
                             values =c(light_blue,light_grey,usaid_blue, usaid_red),
                             labels = c("under 15yrs","15-19yrs","20-24yrs","above 25yrs"))
Accpt_plt


ggsave("viz/Dec 23 FHDR/New Acceptors Starting FP disaggs.png",
       device="png",
       type="cairo",
       height = 6.5,
       width = 11)






#'*FAMILY PLANNING NEW ACCEPTORS............without age disaggs*


FPNewaccpt <- read_xls("data/Dec 2023 MHDR/FP New Acceptors_National monthly.xls")
FPNewaccpt  <- FPNewaccpt  %>%
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

names(FPNewaccpt)

naccfp <- FPNewaccpt %>%
  rename(family.plan = 3,
         new.accptors = 4
  ) %>%
  
  mutate(naccfp.ab = family.plan + 
           new.accptors)

naccfp

FPA_plt <- ggplot(naccfp, aes(x=mnthyr, y=naccfp.ab)) + 
  geom_point(color= usaid_blue, alpha=.6, size=1) + 
  geom_smooth(method = loess,color= usaid_blue, se=F, size=1.1, alpha=.8) +
  scale_x_date(date_labels="%b %y",date_breaks="3 months") +
  scale_y_continuous(labels=comma,
                     limits=c(0, 70000),
                     breaks = c(5000, 10000, 15000, 20000, 25000, 30000, 35000, 40000, 45000, 50000, 55000, 60000, 65000, 70000)) +
  labs(x="",
       y="",
       caption="Data Source: HMIS",
       title="Family Planning New Acceptors Jan 2021 - Sept 2023.") + 
  baseX

FPA_plt

ggsave("viz/Dec 23 FHDR/National FP new Acceptors.png",
       device="png",
       type="cairo",
       height = 6.5,
       width = 12)














#'*COVERAGE OF MODERN FAMILY PLANNING ADOPTION Private Clinics......*

famPrivate <- read_xls("data/Dec 2023 MHDR/FP in private facilities_national monthly.xls")
famPrivate  <- famPrivate  %>%
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

sum(famPrivate$month_chr!=famPrivate$month) # expecting 0 if vars same



famPrivate_prov <- read_xls("data/Dec 2023 MHDR/FP in private facilities_provincial monthly.xls")
names(famPrivate_prov)
famPrivate_prov
famPrivate_prov  <- famPrivate_prov  %>%
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

sum(famPrivate_prov$month_chr!=famPrivate_prov$month) # expecting 0 if vars same


#'*______COVERAGE OF MODERN FAMILY PLANNING ADOPTION private clinics*

famPrivate <- famPrivate %>%
  rename(wmn.mfp=4,
         wmn.vstd=3) %>%
  mutate(cvrg_fp = wmn.mfp/wmn.vstd) #%>%
# relocate(cvrg_fp, .after=wmn.mfp)
colnames(famPrivate)
crvg_plt <- ggplot(famPrivate, aes(x=mnthyr, y=cvrg_fp, colour=usaid_blue)) + 
  geom_point(alpha=.6, size=1.5) + 
  #geom_line(size=1) +
  geom_smooth(method = loess, linewidth = .8, se=FALSE) +
  scale_y_continuous(limits = c(0,.8),
                     labels = percent,
                     breaks = c(.1,.2,.3,.4,.5,.6,.7,.8)) +
  scale_x_date(date_labels="%b %y",date_breaks="3 months") +
  labs(x="", y="", caption="Data Source: HMIS", title="Coverage of Modern Family Planning use among women of reproductive age \nin PRIVATELY OWNED FACILITIES (Jan 2020 - Dec 2023).") +
  scale_color_manual(name ="",
                     values = usaid_blue,
                     labels ="Coverage of modern family planning adoption") + 
  baseX

crvg_plt
ggsave("viz/Dec 23 FHDR/Private Coverage of modern family planning adoption.png",
       device="png",
       type="cairo",
       height = 6.5,
       width = 12)




#'*______COVERAGE OF MODERN FAMILY PLANNING ADOPTION private clinics...FACETS*



names(famPrivate_prov)
famPrivate_prov <- famPrivate_prov %>%
  rename(prov=1,
         wmn.mfp=4,
         wmn.vstd=3) %>%
  mutate(cvrg_fp = wmn.mfp/wmn.vstd)

famPrivate_prov

fp_plt_prty <- ggplot(famPrivate_prov, aes(x = mnthyr, y = cvrg_fp, colour = usaid_blue)) +
  geom_point(alpha=.9, size=1.3) +
  stat_smooth(method = loess, size=.9, se=T) + 
  scale_y_continuous(limits = c(0,1),
                     labels = percent,
                     breaks = c(.2,.4,.6,.8,1)) +
  facet_wrap(~prov) +
  faceted +
  labs(x ="", y="", caption = "Data Source: HMIS") + labs(x ="", y="", caption = "Data Source: HMIS") +
  ggtitle("Provincial Coverage of Modern Family Planning utilization in PRIVATELY OWNED FACILITIES (Jan 2020 - Dec 2023).") +
  scale_color_manual(name ="",
                     values = usaid_palette,
                     labels = c("Family Planning Utilization")
  ) + basem

fp_plt_prty

ggsave("viz/Dec 23 FHDR/private Modern FP utilization facets.png",
       device="png",
       type="cairo",
       height = 6.5,
       width = 12)







#'*CLIENTS ACCESSING LARC.........private*

names(famPrivate)

larc <- famPrivate %>%
  rename(iucd.inserted = 11,
         implant.inserted = 12
  ) %>%
  
  mutate(larc.ab = iucd.inserted + 
           implant.inserted)

lrc_plt_pvty <- ggplot(larc, aes(x=mnthyr, y=larc.ab)) + 
  geom_point(color= usaid_blue, alpha=.6, size=1) + 
  geom_smooth(method = loess,color= usaid_blue, se=F, size=1.1, alpha=.8) +
  scale_x_date(date_labels="%b %y",date_breaks="3 months") +
  scale_y_continuous(labels=comma,
                     limits=c(0, 3000),
                     breaks = c(500, 1000, 1500, 2000, 2500, 3000)) +
  labs(x="",
       y="",
       caption="Data Source: HMIS",
       title="The Number of clients accessing LARCs (implants and IUDs) in Private-owned \nfacilities has been below 1000 until July 2023 when the picture begins to improve.") + 
  baseX

lrc_plt_pvty

ggsave("viz/Dec 23 FHDR/ private clinics Accessing LARCS.png",
       device="png",
       type="cairo",
       height = 6.5,
       width = 12)





#'*____________CLIENTS ACCESSING LARC........private*

names(famPrivate_prov)

larc <- famPrivate_prov %>%
  rename(iucd.inserted = 11,
         implant.inserted = 12
  ) %>%
  
  mutate(larc.ab = iucd.inserted + 
           implant.inserted)

lrc_prov_pvty <- ggplot(larc, aes(x=mnthyr, y=larc.ab)) + 
  geom_point(color= usaid_blue, alpha=.6, size=1) + 
  geom_smooth(method = loess,color= usaid_blue, se=F, size=1.1, alpha=.8) +
  #scale_x_date(date_labels="%b %y",date_breaks="3 months") +
  facet_wrap(~prov, ncol=4) +
  faceted +
  scale_y_continuous(labels=comma,
                     limits=c(0, 2000),
                     breaks = c(500, 1000, 1500, 2000)) +
  labs(x="",
       y="",
       caption="Data Source: HMIS",
       title="The Number of clients accessing LARCs (implants and IUDs) seems to be improving in Lusaka, \nbut static in the other provinces with PRIVATELY-OWNED facility reports.") + 
  baseX

lrc_prov_pvty

ggsave("viz/Dec 23 FHDR/Accessing LARCS Facets_private.png",
       device="png",
       type="cairo",
       height = 6.5,
       width = 12)




#'*PERCENTAGE OF DISCONTINUNING LARC......private*
names(famPrivate)

larc <- famPrivate %>%
  rename(iucd.inserted = 11,
         implant.inserted = 12,
         iucd.removed = 8,
         implant.removed = 9
  ) %>%
  
  mutate(larc.dis.p = (iucd.removed + implant.removed) / (iucd.inserted + implant.inserted))


larc.dis.p_plt <- ggplot(larc, aes(x=mnthyr, y=larc.dis.p)) + 
  geom_point(color= usaid_blue, alpha=.6, size=1) + 
  geom_smooth(method =loess, color= usaid_blue, se=F, size=1.1, alpha=.8) +
  scale_x_date(date_labels="%b %y",date_breaks="3 months") +
  scale_y_continuous(limits=c(0,.5),
                     labels=percent,
                     breaks = c(.1,.2,.3,.4,.5)) +
  labs(x="",
       y="",
       caption="Data Source: HMIS",
       title="Percentage of clients discontinuing LARCs from privately-owned facilities \nhas been on a downward trend and now stands at below 20%!.") + 
  baseX

larc.dis.p_plt

ggsave("viz/Dec 23 FHDR/Discontinuing LARCS_privately..png",
       device="png",
       type="cairo",
       height = 6.5,
       width = 12)








#'*PERCENTAGE OF DISCONTINUNING LARC.....privately*
names(famPrivate_prov)

larc <- famPrivate_prov %>%
  rename(iucd.inserted = 11,
         implant.inserted = 12,
         iucd.removed = 8,
         implant.removed = 9
  ) %>%
  
  mutate(larc.dis.p = (iucd.removed + implant.removed) / (iucd.inserted + implant.inserted))


larc.disc.p_plt <- ggplot(larc, aes(x=mnthyr, y=larc.dis.p)) + 
  geom_point(color= usaid_blue, alpha=.6, size=1) + 
  geom_smooth(method =loess, color= usaid_blue, se=F, size=1.1, alpha=.8) +
  #scale_x_date(date_labels="%b %y",date_breaks="3 months") +
  facet_wrap(~prov, ncol=4) +
  faceted +
  scale_y_continuous(limits=c(0,.5),
                     labels=percent,
                     breaks = c(.1,.2,.3,.4,.5)) +
  labs(x="",
       y="",
       caption="Data Source: HMIS",
       title="Percentage of clients discontinuing LARCs (Jan 2020 - Dec 2023) from privately-owned facilities.") + 
  baseX

larc.disc.p_plt

ggsave("viz/Dec 23 FHDR/Discontinuing LARCS Facets_pty facilities.png",
       device="png",
       type="cairo",
       height = 6.5,
       width = 12)




#'*______________________Child Health INDICATORS......Private facilities*


chldH_pty <- read_xls("data/Dec 2023 MHDR/Child Heath national level_monthly_privately.xls")
chldH_pty  <- chldH_pty  %>%
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


chldH_prov_pty <- read_xls("data/Dec 2023 MHDR/Child Heath provincial level_monthly_private.xls")
names(chldH_prov_pty)

chldH_prov_pty  <- chldH_prov_pty  %>%
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

sum(chldH_prov_pty$month_chr!=chldH_prov_pty$month) # expecting 0 if vars same





#'* FULLY IMMUNIZED COMBINED WITH DPT.....private*
names(chldH_pty)

chldH_pty <- chldH_pty %>%
  rename(fullyimunized = 7,
         dpt.undr1 = 13
  ) %>%
  
  mutate(fullyimunizedP = fullyimunized/100,
         dpt.undr1P = dpt.undr1/100)

#'*set msles1p & msles2p to 100 for all values >100*
chldH_pty <- chldH_pty %>% 
  dplyr::mutate(ancc = ifelse(fullyimunized > 100, 100, fullyimunized)) %>% 
  dplyr::mutate(fullyimunizedP = fullyimunized/100)

#'*To create legend, gather method for including a legend --*

chldH_pty <- gather(chldH_pty, key = subpop , value = rate, c(fullyimunizedP, dpt.undr1P))
chldH_pty$subpop <- factor(chldH_pty$subpop, levels = unique(chldH$subpop)) # transform into factor
levels(chldH_pty$subpop)


dptfull_plt_pty <- ggplot(chldH_pty, aes(x = mnthyr, y = rate, group = subpop, colour = subpop)) +
  geom_point(alpha=.5, size=.5) + 
  #geom_line(size=1) +
  geom_smooth(method = loess, linewidth = .8, se=FALSE) +
  scale_y_continuous(limits = c(0,1),
                     labels = percent,
                     breaks = c(.1,.2,.3,.4,.5,.6,.7,.8,.9, 1)) +
  scale_x_date(date_labels="%b %y",date_breaks="3 months") +
  labs(x="", y="", caption="Data Source: HMIS", title="Coverage of the Fully Immunized and DPT Under 1 (Jan 2020 - Dec 2023.") +
  scale_color_manual(name ="",
                     values = usaid_palette,
                     labels = c("Fully Immunized (%) under 1", "DPT 1st dose Coverage (%) under 1")
  ) + 
  basem
dptfull_plt_pty
ggsave("viz/Dec 23 FHDR/Proportion of infants fully immunized and DPT given_privateNat.png",
       device="png",
       type="cairo",
       height = 6.5,
       width = 12)


#'*________FULLY IMMUNIZED COMBINED WITH DPT.....privately*

fulldpt_prov_pty <- read_xls("data/Dec 2023 MHDR/Child Heath provincial level_monthly_private.xls")
names(fulldpt_prov_pty)
fulldpt_prov_pty
fulldpt_prov_pty  <- fulldpt_prov_pty  %>%
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

sum(fulldpt_prov_pty$month_chr!=fulldpt_prov_pty$month) # expecting 0 if vars same


names(fulldpt_prov_pty)
names(fulldpt_prov_pty)
fulldpt_prov_pty <- fulldpt_prov_pty %>%
  rename(prov=1,
         fully.imnzd=7,
         dpt1=13) %>%
  mutate(fully.imnzdP = fully.imnzd/100,
         dpt1P = dpt1/100)

#'*set dpt and imnized to 100 for all values >100*
fulldpt_prov_pty <- fulldpt_prov_pty %>% 
  dplyr::mutate(fully.imnzd = ifelse(fully.imnzd > 100, 100, fully.imnzd)) %>% 
  dplyr::mutate(fully.imnzdP = fully.imnzd/100)

#'*To create legend, gather method for including a legend --*

fulldpt_prov_pty <- gather(fulldpt_prov_pty, key = subpop , value = rate, c(fully.imnzdP, dpt1P))
fulldpt_prov_pty$subpop <- factor(fulldpt_prov_pty$subpop, levels = unique(fulldpt_prov_pty$subpop)) # transform into factor
levels(fulldpt_prov_pty$subpop)

provdpt.imnzd_pltPty <- ggplot(fulldpt_prov_pty, aes(x = mnthyr, y = rate, group = subpop, colour = subpop)) +
  geom_point(size=.5, alpha=.5) + 
  geom_smooth(method = loess, linewidth = .7, se=FALSE) +
  scale_y_continuous(limits = c(0,1),
                     labels = percent,
                     breaks = c(.2,.4,.6,.8,1)) +
  labs(x ="", y="", caption = "Data Source: HMIS") +labs(x ="", y="", caption = "Data Source: HMIS") +
  ggtitle("Provincial Coverage of the Fully Immunized (%) and DPT 1st dose Under 1 (%) \n(Jan 2020 - Dec 2023) - PRIVATE-OWNED.") +
  facet_wrap(~prov, ncol=4) +
  faceted +
  scale_color_manual(name ="",
                     values = usaid_palette,
                     labels = c("Fully Immunized (%) under 1", "DPT 1st dose Coverage (%) under 1")) + basey

provdpt.imnzd_pltPty

ggsave("viz/Dec 23 FHDR/Provincial fully immunized and DPT1_privately.png",
       device="png",
       type="cairo",
       height = 6.5,
       width = 12)









#'*Nutrition (Severe acute and moderate malnutrition) ----private*

chldMalt <- read_xls("data/Dec 2023 MHDR/Nutrition data national_monthly_privately.xls")
chldMalt  <- chldMalt  %>%
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

chldMalt <- chldMalt %>%
  rename(chacute = 3,
         chsevere = 4,
         chobesse = 5) %>%
  mutate(chacuteP = chacute/100,
         chsevereP = chsevere/100,
         chobesseP = chobesse/100)

#'*set chacuteP to 100 for all values >100*
chldMalt <- chldMalt %>% 
  dplyr::mutate(chacute = ifelse(chacute > 100, 100, chacute)) %>% 
  dplyr::mutate(chacuteP = chacute/100)

#'*To create legend, gather method for including a legend --*

chldMalt <- gather(chldMalt, key = subpop , value = rate, c(chacuteP, chsevereP,chobesseP))
chldMalt$subpop <- factor(chldMalt$subpop, levels = unique(chldMalt$subpop)) # transform into factor
levels(chldMalt$subpop)

ggplot(chldMalt, aes(x = mnthyr, y = rate, group = subpop, colour = subpop)) +
  geom_point(alpha=.6, size=1.5) + 
  geom_smooth(method = loess, linewidth = .8, se=FALSE) +
  scale_y_continuous(limits = c(0,1),
                     labels = percent,
                     breaks = c(.1,.2,.3,.4,.5,.6,.7,.8,.9, 1)) +
  scale_x_date(date_labels="%b %y",date_breaks="3 months") +
  labs(x ="", y="", caption = "Data Source: HMIS") +labs(x ="", y="", caption = "Data Source: HMIS") +
  ggtitle("Proportion of Child Malnutrition Admission Rates (%), January 2021 - December 2023 - Private Owned Sites.") +
  scale_color_manual(name ="",
                     values = usaid_palette,
                     labels = c("Child Acute Malnutrition (Moderate)", "Child Acute Malnutrition (Severe)", 
                                "Child Overweight/Obesse")
  ) + 
  base

ggsave("viz/Dec 23 FHDR/National child malnutrion admission rates_privately.png",
       device="png",
       type="cairo",
       height = 6.5,
       width = 12)




#'*REDRAW FOR PROVINCIAL LELVEL..................*

chldMalt_prov <- read_xls("data/Dec 2023 MHDR/Nutrition data provincial_monthly_privately.xls")
chldMalt_prov  <- chldMalt_prov  %>%
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

chldMalt_prov <- chldMalt_prov %>%
  rename(prov = 1,
         chacute = 3,
         chsevere = 4,
         chobesse = 5) %>%
  mutate(chacuteP = chacute/100,
         chsevereP = chsevere/100,
         chobesseP = chobesse/100)

#'*set chacuteP to 100 for all values >100*
chldMalt_prov <- chldMalt_prov %>% 
  dplyr::mutate(chacute = ifelse(chacute > 100, 100, chacute)) %>% 
  dplyr::mutate(chacuteP = chacute/100)

#'*To create legend, gather method for including a legend --*

chldMalt_prov <- gather(chldMalt_prov, key = subpop , value = rate, c(chacuteP, chsevereP,chobesseP))
chldMalt_prov$subpop <- factor(chldMalt_prov$subpop, levels = unique(chldMalt_prov$subpop)) # transform into factor
levels(chldMalt_prov$subpop)

ggplot(chldMalt_prov, aes(x = mnthyr, y = rate, group = subpop, colour = subpop)) +
  geom_point(alpha=.6, size=1.5) + 
  geom_smooth(method = loess, linewidth = .8, se=FALSE) +
  scale_y_continuous(limits = c(0,1),
                     labels = percent,
                     breaks = c(.1,.2,.3,.4,.5,.6,.7,.8,.9, 1)) +
  #scale_x_date(date_labels="%b %y",date_breaks="3 months") +
  labs(x ="", y="", caption = "Data Source: HMIS") +labs(x ="", y="", caption = "Data Source: HMIS") +
  facet_wrap(~prov, ncol=4) +
  faceted +
  ggtitle("Proportion of Child Malnutrition admission rates (%), (Jan 2021 - Dec 2023) - Private sites") +
  scale_color_manual(name ="",
                     values = usaid_palette,
                     labels = c("Child Acute Malnutrition (Moderate)", "Child Acute Malnutrition (Severe)", 
                                "Child Overweight/Obesse")
  ) + 
  base

ggsave("viz/Dec 23 FHDR/Child malnutrition admission rates_facets_privae sites.png",
       device="png",
       type="cairo",
       height = 6.5,
       width = 12)




#'*______NATIONAL VITAMIN A SUPPLEMENT COVERAGE_Private*

names(chldH_pty)
NatVitA <- chldH_pty %>%
  rename(vitSup = 14) %>%
  
  mutate(vitSupP = vitSup/100)

#'*setting values of Vitamin A to 100 for all values >100*
NatVitA <- NatVitA %>% 
  dplyr::mutate(vitSup = ifelse(vitSup > 100, 100, vitSup)) %>% 
  dplyr::mutate(vitSupP = vitSup/100)

#'*To create legend, gather method for including a legend --*

NatVitA <- gather(NatVitA, key = subpop , value = rate, c(vitSup))
NatVitA$subpop <- factor(NatVitA$subpop, levels = unique(NatVitA$subpop)) # transform into factor
levels(NatVitA$subpop)

NatVitA_plt <- ggplot(NatVitA, aes(x = mnthyr, y = vitSupP, colour=usaid_blue )) +
  geom_point(alpha=.5, size=.7) + 
  geom_smooth(method = loess, size = .8, se=FALSE) +
  scale_y_continuous(limits = c(0,.8),
                     labels = percent,
                     breaks = c(.1,.2,.3,.4,.5,.6,.7,.8,.9, 1)) +
  scale_x_date(date_labels="%b %y",date_breaks="3 months") +
  labs(x="", y="", caption="Data Source: HMIS", title="The Proportion of infants given Vitamin A supplement (6-11 months semester coverage)\n in Private owned facilities is above 50% begining December 2022!") +
  scale_color_manual(name ="",
                     values = usaid_blue,
                     labels = "Vitamin A supplement coverage (%)") + 
  basem 

NatVitA_plt

ggsave("viz/Dec 23 FHDR/National Vitamin A supp_private.png",
       device="png",
       type="cairo",
       height = 6.5,
       width = 12)




#'*______VITAMIN A SUPPLEMENT COVERAGE............private*

vitA_prov <- read_xls("data/Dec 2023 MHDR/Child Heath provincial level_monthly_private.xls")
names(vitA_prov)
vitA_prov
vitA_prov  <- vitA_prov  %>%
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

names(vitA_prov)
vitA_prov <- vitA_prov %>%
  rename(prov=1,
         vitA.supp=14) %>%
  mutate(vitA.suppP = vitA.supp/100)

#'*set Vitamin A supplement to 100 for all values >100*
vitA_prov <- vitA_prov %>% 
  dplyr::mutate(vitA.supp = ifelse(vitA.supp > 100, 100, vitA.supp)) %>% 
  dplyr::mutate(vitA.suppP = vitA.supp/100)

#'*To create legend, gather method for including a legend --*

vitA_prov <- gather(vitA_prov, key = subpop , value = rate, c(vitA.supp))
vitA_prov$subpop <- factor(vitA_prov$subpop, levels = unique(vitA_prov$subpop)) # transform into factor
levels(vitA_prov$subpop)

vitA_prov_plt <- ggplot(vitA_prov, aes(x = mnthyr, y = vitA.suppP, colour=usaid_blue )) +
  geom_point(alpha=.4, size=.7) + 
  #geom_line(size=1) +
  geom_smooth(method = loess, size = .8, se=FALSE) +
  scale_y_continuous(limits = c(0,.5),
                     labels = percent,
                     breaks = c(.1,.2,.3,.4,.5)) +
  
  labs(x="", y="", caption="Data Source: HMIS", title="Vitamin A Supplement at provincial level in private owned sites.") +
  facet_wrap(~prov, ncol=4) +
  faceted +
  scale_color_manual(name ="",
                     values = usaid_blue,
                     labels = "Vitamin A Supplement Coverage") + 
  basem 

vitA_prov_plt

ggsave("viz/Dec 23 FHDR/Vitamin A supplementation facets_private.png",
       device="png",
       type="cairo",
       height = 6.5,
       width = 12)



#'*________4th+ TO TOTAL ANC ATTENDANCES.........Private*

frthPlusANC_prov <- read_xls("data/Dec 2023 MHDR/4th+ to Total ANC Attendance_provincial level_private.xls")
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


names(frthPlusANC_prov)
names(frthPlusANC_prov)
frthPlusANC_prov <- frthPlusANC_prov %>%
  rename(prov=1,
         frth.anc=3) %>%
  mutate(frth.ancP = frth.anc/100)
ggplot(frthPlusANC_prov, aes(x=mnthyr, y=frth.ancP)) + 
  geom_point(size=.5, alpha=.5, colour=usaid_blue) + 
  stat_smooth(se=F, size=.8, alpha=.6, colour=usaid_blue) +
  scale_y_continuous(limits = c(0,.5),
                     labels = percent,
                     breaks = c(.1,.2,.3,.4,.5)) +
  labs(x ="", y="", caption = "Data Source: HMIS") +labs(x ="", y="", caption = "Data Source: HMIS") +
  ggtitle("4th+ to Total ANC attendances in PRIVATELY-OWNED sites shows a similar trend where there is a deep \nbegining 2022, with Central Province having a sharp deep starting mid 2021.") +
  facet_wrap(~prov, ncol=4) +
  faceted +
  scale_color_manual(values=usaid_blue) + basey

ggsave("viz/Dec 23 FHDR/4th+ to Total ANC attendances faceted_private.png",
       device="png",
       type="cairo",
       height = 6.5,
       width = 12)



#'*Causes Perinatal Deaths*
cod <- read_xlsx("data/Prematurity Jan 2024/Perinatal Deaths and cause by quarter.xlsx")

# cod$causes <- as.Date(cod$causes)

cod
cod <- reshape2::melt(cod[c(1, 2, 3, 4, 5, 6, 7)], id = 'causes')

cod

cod1 <- ggplot(cod, aes(x=causes, y=value, fill=variable), alpha=0.6)+ 
  geom_bar(alpha=.7,stat="identity", position="dodge") +
  scale_fill_manual(values=c( usaid_palette6)) +
  scale_y_continuous(labels=comma) +
  labs(fill="Legend:",  caption="Data Source: PDSR", title="Causes of Perinatal Deaths, quarters 4 (2019 - 2023).",
       x="",
       y="Number of cases") + base

cod1
ggsave("viz/Prematurity viz jan 24/ qtr 4 causes 2023.png",
       device="png",
       type="cairo",
       height = 6.0,
       width = 13)




#'*________4th+ TO TOTAL ANC ATTENDANCES*

frthPlusANC_prov <- read_xls("data/June 2024 Ad Hoc/4th+ to Total ANC Attendance_provincial level.xls")
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

#sum(fam_prov$month_chr!=fam_prov$month) # expecting 0 if vars same


names(frthPlusANC_prov)
names(frthPlusANC_prov)
frthPlusANC_prov <- frthPlusANC_prov %>%
  rename(prov=1,
         frth.anc=3) %>%
  mutate(frth.ancP = frth.anc/100)
ggplot(frthPlusANC_prov, aes(x=mnthyr, y=frth.ancP)) + 
  geom_point(size=.5, alpha=.5, colour=usaid_blue) + 
  stat_smooth(se=F, size=.8, alpha=.6, colour=usaid_blue) +
  scale_y_continuous(limits = c(0,.5),
                     labels = percent,
                     breaks = c(.1,.2,.3,.4,.5)) +
  labs(x ="", y="", caption = "Data Source: HMIS") +labs(x ="", y="", caption = "Data Source: HMIS") +
  ggtitle("4th+ to Total ANC attendances paints a similar trend across the four provinces, (Jan 2021 - Apr 2024).") +
  facet_wrap(~prov, ncol=4) +
  faceted +
  scale_color_manual(values=usaid_blue) + basey

ggsave("viz/Ad hoc Jun 2024/4th+ to Total ANC attendances CLMN.png",
       device="png",
       type="cairo",
       height = 6.5,
       width = 12)







#'*_______Redraw for National Level*

frthPlusANC <- read_xls("data/June 2024 Ad Hoc/4th+ to Total ANC Attendance_National level.xls")
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
  scale_y_continuous(limits = c(0,.5),
                     labels = percent,
                     breaks = c(.1,.2,.3,.4,.5,.6)) +
  labs(x ="", y="", caption = "Data Source: HMIS") +labs(x ="", y="", caption = "Data Source: HMIS") +
  ggtitle("The Trends for the 4th+ to Total ANC attendances has been below 30% but above 20%\n at National Level except for 2023 where it is on a downward trend!") +
  scale_color_manual(values=usaid_blue) + basey

ggsave("viz/Ad hoc Jun 2024/National 4th+ to Total ANC attendances.png",
       device="png",
       type="cairo",
       height = 6.5,
       width = 12)








#'*________4th+ TO TOTAL ANC ATTENDANCES BY DISTRICT*

frthPlusANC_prov <- read_xls("data/June 2024 Ad Hoc/4th+ ANC visits_Muchinga province districts.xls")
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

#sum(fam_prov$month_chr!=fam_prov$month) # expecting 0 if vars same


names(frthPlusANC_prov)
names(frthPlusANC_prov)
frthPlusANC_prov <- frthPlusANC_prov %>%
  rename(prov=1,
         frth.anc=3) %>%
  mutate(frth.ancP = frth.anc/100)
ggplot(frthPlusANC_prov, aes(x=mnthyr, y=frth.ancP)) + 
  geom_point(size=.5, alpha=.5, colour=usaid_blue) + 
  stat_smooth(se=F, size=.8, alpha=.6, colour=usaid_blue) +
  scale_y_continuous(limits = c(0,.5),
                     labels = percent,
                     breaks = c(.1,.2,.3,.4,.5)) +
  labs(x ="", y="", caption = "Data Source: HMIS") +labs(x ="", y="", caption = "Data Source: HMIS") +
  ggtitle("The 4th+ ANC attendances for Muchinga Province districts show a similar trend across,\n but with a special pattern from Nakonde district!.") +
  facet_wrap(~prov, ncol=4) +
  faceted +
  scale_color_manual(values=usaid_blue) + basey

ggsave("viz/Ad hoc Jun 2024/4th+ ANC visits_Muchinga districts.png",
       device="png",
       type="cairo",
       height = 6.5,
       width = 12.5)






#'*_____________Neonatal mortality rates by district*

neo.mr <- read_xlsx("data/June 2024 Ad Hoc/Neonatal mortality rate_Western.xlsx")
names(neo.mr)

neo.mr1 <- neo.mr %>%
  rename(dist=1,
         neonatal.mortRate=2,
         yr=3)

neo.mr1

nmr_plt <- ggplot(neo.mr1, aes(x=yr, y=neonatal.mortRate, colour=usaid_red)) + 
  geom_point(alpha=.6, size=.9) + 
  geom_smooth(method = loess, linewidth = .8, se=FALSE) + facet_wrap(~dist) + faceted +
  scale_y_continuous(labels=comma) +
  # scale_x_date(date_labels="") +
  labs(x="", y="", caption="Data Source: HMIS", title="Neonatal Mortality Rate per 1,000 live births, Western province districts (2018 - 2023).") +
  scale_color_manual(name ="",
                     values = usaid_red,
                     labels ="Neonatal Mortality Rates") + 
  basey

nmr_plt
ggsave("viz/Ad hoc Jun 2024/Neonata mortality rates_western districts.png",
       device="png",
       type="cairo",
       height = 7.0,
       width = 12.5)







#'*_____________Maternal mortality ratios by district (Lua.Much.North.NWP.West..........*

mat.mratio <- read_xlsx("data/June 2024 Ad Hoc/Maternal mortality ratio_Western.xlsx")
names(mat.mratio)

mat.mratio1 <- mat.mratio %>%
  rename(dist=1,
         maternal.mortRatio=2,
         yr=3)

mat.mratio1

Matmratio_plt <- ggplot(mat.mratio1, aes(x=yr, y=maternal.mortRatio, colour=usaid_red)) + 
  geom_point(alpha=.6, size=.9) + 
  geom_smooth(method = loess, linewidth = .8, se=FALSE) + facet_wrap(~dist) + faceted +
  scale_y_continuous(labels=comma) +
  # scale_x_date(date_labels="") +
  labs(x="", y="", caption="Data Source: HMIS", title="Maternal Mortality Ratio per 100, 000 live births, Western Province Districts (2018 - 2023).") +
  scale_color_manual(name ="",
                     values = usaid_red,
                     labels ="Maternal Mortality Ratio Per 100, 000 Deliveries.") + 
  basey

Matmratio_plt
ggsave("viz/Ad hoc Jun 2024/Mat mortality ratio_Western districts.png",
       device="png",
       type="cairo",
       height = 7.0,
       width = 12.5)



#'*........National Maternal Mortality Ratio*
#'
#'
mat <- read_xls("data/Nov 2024 MHDR/Reproductive Maternal Health_National level monthly.xls")
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
  geom_smooth(method = loess, linewidth=.9, se=F) +
  scale_y_continuous(limits = c(0,170),
                     breaks = c(20,60,100,140,170),
                     labels = c("20","60","100","140","170")) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")+
  scale_linetype_manual(name="",
                        labels= ("Maternal mortality facility ratio (per 100,000 live births)"), 
                        values=("solid"))+
  labs(x="", y="", caption="Data Source: HMIS", title="Maternal mortality facility ratio (per 100,000 live births) had been increasing since mid 2020, \nand has since shown a somewhat constant trend beginning 2022 through Jan 2023 \nwhere it is on a downward trend into 2024! (Jan 2020 - Sept 2024).") +
  scale_color_manual(name ="",
                     values = usaid_red) + 
  baseX

mmr_plt
ggsave("viz/Nov 2024 FHDR/National Maternal Mortality Ratio.png",
       device="png",
       type="cairo",
       height = 6.5,
       width = 12)




#'*........National Maternal Mortality Ratio_USAID Supported provinces*
#'
#'
mat <- read_xls("data/Dec 2023 MHDR/Reproductive Maternal Health_National level monthly_USAID.xls")
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
  geom_smooth(method = loess, linewidth=.9, se=F) +
  scale_y_continuous(limits = c(0,170),
                     breaks = c(20,60,100,140,170),
                     labels = c("20","60","100","140","170")) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")+
  scale_linetype_manual(name="",
                        labels= ("Maternal mortality facility ratio (per 100,000 live births)"), 
                        values=("solid"))+
  labs(x="", y="", caption="Data Source: HMIS", title="Maternal mortality facility ratio (per 100,000 live births) in USAID Supported Provinces had been on an upward \ntrajectory since Jan 2020, and has since been a constant trend from early 2022 through into 2024!.") +
  scale_color_manual(name ="",
                     values = usaid_blue) + 
  baseX

mmr_plt
ggsave("viz/Dec 23 FHDR/National Maternal Mortality Ratio_USAID supported.png",
       device="png",
       type="cairo",
       height = 6.5,
       width = 12)







#'*________1st ANC, and 4th+ ANC BY DISTRICT*

frthPlusANC_prov <- read_xls("data/Aug 2024 MHDR/ANC PNC_Central province districts.xls")
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


names(frthPlusANC_prov)
frthPlusANC_prov <- frthPlusANC_prov %>%
  rename(dstrct=1,
         frst.anc=3,
         frth.anc=4,
         pnc=5) %>%
  mutate(frst.ancP = frst.anc/100,
         frth.ancP = frth.anc/100,
         pncP = pnc/100
         )

#'*set frst.ancP & frth.ancP to 100 for all values >100*
frthPlusANC_prov <- frthPlusANC_prov %>% 
  dplyr::mutate(frst.anc = ifelse(frst.anc > 100, 100, frst.anc)) %>% 
  dplyr::mutate(frst.ancP = frst.anc/100)

#'*To create legend, gather method for including a legend --*

frthPlusANC_prov <- gather(frthPlusANC_prov, key = subpop , value = rate, c(frst.ancP, frth.ancP))
frthPlusANC_prov$subpop <- factor(frthPlusANC_prov$subpop, levels = unique(frthPlusANC_prov$subpop)) # transform into factor
levels(frthPlusANC_prov$subpop)

ggplot(frthPlusANC_prov, aes(x = mnthyr, y = rate, group = subpop, colour = subpop )) + 
  geom_point(size=.5, alpha=.5, colour=c(light_blue,light_grey, usaid_red)) + 
  stat_smooth(se=F, linewidth=.8, alpha=.6, colour=c(light_blue,light_grey, usaid_red)) +
  scale_y_continuous(limits = c(0,1),
                     labels = percent,
                     breaks = c(.2,.4,.6,.8,1)) +
  labs(x ="", y="", caption = "Data Source: HMIS") +labs(x ="", y="", caption = "Data Source: HMIS") +
  ggtitle("The 4th+ ANC attendances for Muchinga Province districts show a similar trend across,\n but with a special pattern from Nakonde district!.") +
  facet_wrap(~dstrct, ncol=4) +
  faceted +
  scale_color_manual(name ="",
                     values =c(light_blue,light_grey, usaid_red),
                     labels = c("under 15yrs","15-19yrs","20-24yrs","above 25yrs")) + basey

ggsave("viz/Aug 2024 FHDR/4th+ ANC visits_Muchinga districts.png",
       device="png",
       type="cairo",
       height = 6.5,
       width = 12.5)









#'*........National Neonatal deaths Count absolute numbers*
#'
#'
neonat <- read_xls("data/Nov 2024 MHDR/National Neonatal death count Jan 2020_Sept 2024.xls")
neonat  <- neonat  %>%
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

ndc <- neonat %>%
  rename(nnc = 3
  )
colnames(neonat)

ndc_1 <- gather(ndc, key = mmtype , value = deaths, nnc)
ndc_1
colnames(ndc_1)

ndc_1$mmtypef <- factor(ndc_1$mmtype, levels = unique(ndc_1$mmtype))
levels(ndc_1$mmtypef)

names(ndc_1)
colnames(ndc_1)

ndc_plt <- ggplot(ndc_1, aes(x = mnthyr, y = deaths , colour =   mmtype, linetype=mmtype)) + 
  geom_point(alpha=.5, size=.7) + 
  geom_smooth(method = loess, linewidth=.9, se=F) +
  scale_y_continuous(limits = c(0,600),
                     breaks = c(50,100,150,200,250,300,350,400,450,500,550,600),
                     labels = c("50","100","150","200","250","300","350","400","450","500","550","600")) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")+
  scale_linetype_manual(name="",
                        labels= ("Neonatal Deaths Count (absolute numbers"), 
                        values=("solid"))+
  labs(x="", y="", caption="Data Source: HMIS", title="The Neonatal deaths were on a downward trend since January 2020 up until late 2021 where it\nstarted going up and has since maintained a perormance of between 300 and 320 per month (Jan 2020 - Sept 2024).") +
  scale_color_manual(name ="",
                     values = usaid_red) + 
  baseX

ndc_plt
ggsave("viz/Nov 2024 FHDR/National neaonatal death count.png",
       device="png",
       type="cairo",
       height = 6.5,
       width = 12)







#'*........Neonatal deaths percentage Trends Jan 2020 to Sept 2024*
#'
#'
neo.perctge <- read_xls("data/Nov 2024 MHDR/Neonatal deaths (percentage) 2020_Sept 2024.xls")

neo.perctge$period <- as.Date(neo.perctge$period)

neo.perctge

neo.perctge2 <- neo.perctge %>%
  rename( neo.perctge = 2) %>%
  mutate(neo.perctge.prt = neo.perctge/100)



neo.perctge2 <- neo.perctge2 %>%
  select(1,3)

neo.perctge2

neo.perctge3 <- neo.perctge2 %>%
  gather(key = subRt , value = rate, c(neo.perctge.prt))

neo.perctge3

ggplot(neo.perctge3, aes(x = period, y = rate, group = subRt, colour = subRt)) +
  geom_point(alpha=.6, size=1.9) + 
  #geom_line(size=1) +
  geom_smooth(method = loess, linewidth = .8, se=FALSE) +
  scale_y_continuous(limits = c(0,.1),
                     labels = percent,
                     breaks = c(.01,.02,.03,.04,.05,.06,.07,.08,.09,.1)) +
  scale_x_date(date_labels="%b %y",date_breaks="3 months") +
  labs(x="", y="", caption="Data Source: PDSR", title="Neonatal Deaths as a Percentage have been below 1%, (January 2020 - September 2024).") +
  scale_color_manual(name ="",
                     values = usaid_red) + 
  baseX

ggsave("viz/Nov 2024 FHDR/national neonatal deaths percentage Jan.png",
       device="png",
       type="cairo",
       height = 6.5,
       width = 12)






#'*______National Level Under 1 and Under 5 Mortality rates*

under1.5 <- read_xls("data/Nov 2024 MHDR/Under 1 and under 5 mortality rate_national monthly.xls")
under1.5  <- under1.5  %>%
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

under1.5

under15.mort <- under1.5 %>%
  select(3,4,9)

under15.mort

# peri.mr2 <- peri.mr1 %>%
#   rename(perinatal.mortRate=1)

under15.mort

under15.mort2 <- reshape2::melt(under15.mort, id = "monyr")

nat_u1andu5 <- ggplot(under15.mort2, aes(x=monyr, y=value, colour=variable)) + 
  geom_point(alpha=.6, size=.9) + 
  geom_smooth(method = loess, linewidth = .8, se=FALSE) +
  scale_y_continuous(labels=comma,
                     limits=c(9,21)) +
  scale_x_date(date_labels="%b %y",date_breaks="3 months") +
  labs(x="", y="", caption="Data Source: HMIS", title="Perinatal Mortality Rate per 1,000 live births, had been on a downward trend from January 2020, \nbut has begun to rise begining Jan 2023.") +
  scale_color_manual(name ="",
                     values = c(usaid_red,usaid_blue),
                     labels = c("Facility mortality under 1 year rate","Facility mortality under 5 years rate")) + 
  basey

nat_u1andu5

ggsave("viz/Nov 2024 FHDR/National facility mortality U1 & U5 Jan 2020-Sept 2024.png",
       device="png",
       type="cairo",
       height = 6.5,
       width = 12)





























