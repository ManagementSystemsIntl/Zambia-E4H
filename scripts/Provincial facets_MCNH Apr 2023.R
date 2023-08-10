
source("scripts/r prep2.r")
source("scripts/r prep3.r")


#'*________PROPORTION BREASTFED WITHIN AN HOUR PROVINCIAL FACETED*

bfhr_prov <- read_xls("data/May 2023 FHDR/Child Heath provincial level_monthly.xls")
names(bfhr_prov)
bfhr_prov
bfhr_prov  <- bfhr_prov  %>%
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

sum(bfhr_prov$month_chr!=bfhr_prov$month) # expecting 0 if vars same


names(bfhr_prov)
names(bfhr_prov)
bfhr_prov <- bfhr_prov %>%
  rename(prov=1,
         bfhr=18,
         ebf=19) %>%
  mutate(bfhrP = bfhr/100,
         ebfP = ebf/100)

#'*set EBF & BREASTFEED 1HOUR to 100 for all values >100*
bfhr_prov <- bfhr_prov %>% 
  dplyr::mutate(bfhr = ifelse(bfhr > 100, 100, bfhr)) %>% 
  dplyr::mutate(bfhrP = bfhr/100)

#'*To create legend, gather method for including a legend --*

bfhr_prov <- gather(bfhr_prov, key = subpop , value = rate, c(bfhrP, ebfP))
bfhr_prov$subpop <- factor(bfhr_prov$subpop, levels = unique(bfhr_prov$subpop)) # transform into factor
levels(bfhr_prov$subpop)

bfhr_plt <- ggplot(bfhr_prov, aes(x = mnthyr, y = rate, group = subpop, colour = subpop)) +
  geom_point(size=.5, alpha=.5) + 
  geom_smooth(method = loess, size = .8, se=FALSE) +
  # stat_smooth(se=F, size=.8, alpha=.6, colour=usaid_blue) +
  scale_y_continuous(limits = c(0,1),
                     labels = percent,
                     breaks = c(.2,.4,.6,.8,1)) +
  labs(x ="", y="", caption = "Data Source: HMIS") +labs(x ="", y="", caption = "Data Source: HMIS") +
  ggtitle("Proportion of Infants Breastfed within 1 hour of birth \nand those on EBF at 6 months, 2019 - 2023") +
  facet_wrap(~prov, ncol=4) +
  faceted +
  scale_color_manual(name ="",
                     values = usaid_palette,
                     labels = c("Initiation on breastmilk within one hour of birth", "Infants on EBF at 6 months")) + basey

bfhr_plt

ggsave("viz/May 2023 data review/Breastfed within 1 hour of birth and EBF facets.png",
       device="png",
       type="cairo",
       height = 6.5,
       width = 11)



#'*________1st and 2nd MEASLES VACCINE COVERAGE PROVINCIAL FACETED*

mslsvac_prov <- read_xls("data/May 2023 FHDR/Child Heath provincial level_monthly.xls")
names(mslsvac_prov)
mslsvac_prov
mslsvac_prov  <- mslsvac_prov  %>%
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

sum(mslsvac_prov$month_chr!=mslsvac_prov$month) # expecting 0 if vars same


names(mslsvac_prov)
names(mslsvac_prov)
mslsvac_prov <- mslsvac_prov %>%
  rename(prov=1,
        mslse1=11,
        mslse2=12) %>%
  mutate(mslse1P = mslse1/100,
         mslse2P = mslse2/100)

#'*set Measles 1 & Measles 2 to 100 for all values >100*
mslsvac_prov <- mslsvac_prov %>% 
  dplyr::mutate(mslse1 = ifelse(mslse1 > 100, 100, mslse1)) %>% 
  dplyr::mutate(mslse1P = mslse1/100)

#'*To create legend, gather method for including a legend --*

mslsvac_prov <- gather(mslsvac_prov, key = subpop , value = rate, c(mslse1P, mslse2P))
mslsvac_prov$subpop <- factor(mslsvac_prov$subpop, levels = unique(mslsvac_prov$subpop)) # transform into factor
levels(mslsvac_prov$subpop)

msles_plt <- ggplot(mslsvac_prov, aes(x = mnthyr, y = rate, group = subpop, colour = subpop)) +
  geom_point(size=.5, alpha=.5) + 
  geom_smooth(method = loess, size = .8, se=FALSE) +
  # stat_smooth(se=F, size=.8, alpha=.6, colour=usaid_blue) +
  scale_y_continuous(limits = c(0,1),
                     labels = percent,
                     breaks = c(.2,.4,.6,.8,1)) +
  labs(x ="", y="", caption = "Data Source: HMIS") +labs(x ="", y="", caption = "Data Source: HMIS") +
  ggtitle("Vaccine Coverage for 1st and 2nd doses of Measles, 2019 - 2023") +
  facet_wrap(~prov, ncol=4) +
  faceted +
  scale_color_manual(name ="",
                     values = usaid_palette,
                     labels = c("Measles 1 coverage", "Measles 2 coverage")) + basey

msles_plt

ggsave("viz/May 2023 data review/Provincial Measles Vaccines.png",
       device="png",
       type="cairo",
       height = 6.5,
       width = 12)


#'*________FULLY IMMUNIZED PROVINCIAL FACETED*

fulmunized_prov <- read_xls("data/May 2023 FHDR/Child Heath provincial level_monthly.xls")
names(fulmunized_prov)
fulmunized_prov
fulmunized_prov  <- fulmunized_prov  %>%
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

sum(fulmunized_prov$month_chr!=fulmunized_prov$month) # expecting 0 if vars same


names(fulmunized_prov)
names(fulmunized_prov)
fulmunized_prov <- fulmunized_prov %>%
  rename(prov=1,
         fiu1=7) %>%
  mutate(fiu1P = fiu1/100)

#'*set immunization to 100 for all values >100*
fulmunized_prov <- fulmunized_prov %>% 
  dplyr::mutate(fiu1 = ifelse(fiu1 > 100, 100, fiu1)) %>% 
  dplyr::mutate(fiu1P = fiu1/100)

#'*To create legend, gather method for including a legend --*

fulmunized_prov <- gather(fulmunized_prov, key = subpop , value = rate, c(fiu1))
fulmunized_prov$subpop <- factor(fulmunized_prov$subpop, levels = unique(fulmunized_prov$subpop)) # transform into factor
levels(fulmunized_prov$subpop)

fulmunized_plt <- ggplot(fulmunized_prov, aes(x = mnthyr, y = fiu1P, colour=usaid_blue )) +
  geom_point(alpha=.4, size=.7) + 
  #geom_line(size=1) +
  geom_smooth(method = loess, size = .8, se=FALSE) +
  scale_y_continuous(limits = c(0,1),
                     labels = percent,
                     breaks = c(.1,.2,.3,.4,.5,.6,.7,.8,.9, 1)) +
  # scale_x_date(date_labels="%b %y",date_breaks="4 months") +
  labs(x="", y="", caption="Data Source: HMIS", title="Fully Immunized Coverage (%) Under 1, 2019 - 2023") +
  facet_wrap(~prov, ncol=4) +
  faceted +
  scale_color_manual(name ="",
                     values = usaid_blue,
                     labels = "Fully Immunized Coverage Under 1") + 
  basem 

fulmunized_plt

# fulmunized_plt <- ggplot(mslsvac_prov, aes(x = mnthyr, y = rate, group = subpop, colour = subpop)) +
#   geom_point(size=.5, alpha=.5) + 
#   geom_smooth(method = loess, size = .8, se=FALSE) +
#   # stat_smooth(se=F, size=.8, alpha=.6, colour=usaid_blue) +
#   scale_y_continuous(limits = c(0,1),
#                      labels = percent,
#                      breaks = c(.2,.4,.6,.8,1)) +
#   labs(x ="", y="", caption = "Data Source: HMIS") +labs(x ="", y="", caption = "Data Source: HMIS") +
#   ggtitle("Vaccine Coverage for 1st and 2nd doses of Measles, 2019 - 2023") +
  # facet_wrap(~prov, ncol=4) +
  # faceted +
#   scale_color_manual(name ="",
#                      values = usaid_palette,
#                      labels = c("Measles 1 coverage", "Measles 2 coverage")) + basey
# 
# fulmunized_plt

ggsave("viz/May 2023 data review/Fully Imunnized facets PS.png",
       device="png",
       type="cairo",
       height = 6.5,
       width = 12)


#'*______BCG COVERAGE UNDER 1*

BCG_prov <- read_xls("data/May 2023 FHDR/Child Heath provincial level_monthly.xls")
names(BCG_prov)
BCG_prov
BCG_prov  <- BCG_prov  %>%
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

names(BCG_prov)
BCG_prov <- BCG_prov %>%
  rename(prov=1,
         bcg.coverage=10) %>%
  mutate(bcg.coverageP = bcg.coverage/100)

#'*set BCG to 100 for all values >100*
BCG_prov <- BCG_prov %>% 
  dplyr::mutate(bcg.coverage = ifelse(bcg.coverage > 100, 100, bcg.coverage)) %>% 
  dplyr::mutate(bcg.coverageP = bcg.coverage/100)

#'*To create legend, gather method for including a legend --*

BCG_prov <- gather(BCG_prov, key = subpop , value = rate, c(bcg.coverage))
BCG_prov$subpop <- factor(BCG_prov$subpop, levels = unique(BCG_prov$subpop)) # transform into factor
levels(BCG_prov$subpop)

bcg_plt <- ggplot(BCG_prov, aes(x = mnthyr, y = bcg.coverageP, colour=usaid_blue )) +
  geom_point(alpha=.4, size=.7) + 
  #geom_line(size=1) +
  geom_smooth(method = loess, size = .8, se=FALSE) +
  scale_y_continuous(limits = c(.2,1),
                     labels = percent,
                     breaks = c(.2,.4,.6,.8,1)) +
  # scale_x_date(date_labels="%b %y",date_breaks="4 months") +
  labs(x="", y="", caption="Data Source: HMIS", title="BCG Coverage (%) Under 1, 2019 - 2023") +
  facet_wrap(~prov, ncol=4) +
  faceted +
  scale_color_manual(name ="",
                     values = usaid_blue,
                     labels = "BCG coverage (%) under 1") + 
  basem 

bcg_plt

ggsave("viz/May 2023 data review/BCG Under 1 facets.png",
       device="png",
       type="cairo",
       height = 6.5,
       width = 12)


#'*______DPT 1st DOSE COVERAGE UNDER 1*

dpt1_prov <- read_xls("data/May 2023 FHDR/Child Heath provincial level_monthly.xls")
names(dpt1_prov)
dpt1_prov
dpt1_prov  <- dpt1_prov  %>%
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

names(dpt1_prov)
dpt1_prov <- dpt1_prov %>%
  rename(prov=1,
         dptu1.coverage=13) %>%
  mutate(dptu1.coverageP = dptu1.coverage/100)

#'*set DPT 1st dose to 100 for all values >100*
dpt1_prov <- dpt1_prov %>% 
  dplyr::mutate(dptu1.coverage = ifelse(dptu1.coverage > 100, 100, dptu1.coverage)) %>% 
  dplyr::mutate(dptu1.coverageP = dptu1.coverage/100)

#'*To create legend, gather method for including a legend --*

dpt1_prov <- gather(dpt1_prov, key = subpop , value = rate, c(dptu1.coverage))
dpt1_prov$subpop <- factor(dpt1_prov$subpop, levels = unique(dpt1_prov$subpop)) # transform into factor
levels(dpt1_prov$subpop)

prov.dpt_plt <- ggplot(dpt1_prov, aes(x = mnthyr, y = dptu1.coverageP, colour=usaid_blue )) +
  geom_point(alpha=.4, size=.7) + 
  #geom_line(size=1) +
  geom_smooth(method = loess, size = .8, se=FALSE) +
  scale_y_continuous(limits = c(.2,1),
                     labels = percent,
                     breaks = c(.2,.4,.6,.8,1)) +

  labs(x="", y="", caption="Data Source: HMIS", title="DPT 1st dose Coverage (%) Under 1, 2019 - 2023") +
  facet_wrap(~prov, ncol=4) +
  faceted +
  scale_color_manual(name ="",
                     values = usaid_blue,
                     labels = "DPT 1st dose coverage (%) under 1") + 
  basem 

prov.dpt_plt

ggsave("viz/May 2023 data review/DPT 1st dose Under 1 facets.png",
       device="png",
       type="cairo",
       height = 6.5,
       width = 12)



#'*______VITAMIN A SUPPLEMENT COVERAGE*

vitA_prov <- read_xls("data/May 2023 FHDR/Child Heath provincial level_monthly.xls")
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
  scale_y_continuous(limits = c(0,.8),
                     labels = percent,
                     breaks = c(.1,.2,.3,.4,.5,.6,.7,.8)) +
  
  labs(x="", y="", caption="Data Source: HMIS", title="Vitamin A Supplement Coverage, 2019 - 2023") +
  facet_wrap(~prov, ncol=4) +
  faceted +
  scale_color_manual(name ="",
                     values = usaid_blue,
                     labels = "Vitamin A Supplement Coverage") + 
  basem 

vitA_prov_plt

ggsave("viz/May 2023 data review/Vitamin A supplementation facets.png",
       device="png",
       type="cairo",
       height = 6.5,
       width = 12)


#'*______PROVINCIAL STUNTING RATES in Under 5s*

stunting_prov <- read_xls("data/May 2023 FHDR/Child Heath provincial level_monthly.xls")
names(stunting_prov)
stunting_prov
stunting_prov  <- stunting_prov  %>%
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

names(stunting_prov)
stunting_prov <- stunting_prov %>%
  rename(prov=1,
         stunt.rate=16) %>%
  mutate(stunt.rateP = stunt.rate/100)

#'*set Vitamin A supplement to 100 for all values >100*
stunting_prov <- stunting_prov %>% 
  dplyr::mutate(stunt.rate = ifelse(stunt.rate > 100, 100, stunt.rate)) %>% 
  dplyr::mutate(stunt.rateP = stunt.rate/100)

#'*To create legend, gather method for including a legend --*

stunting_prov <- gather(stunting_prov, key = subpop , value = rate, c(stunt.rate))
stunting_prov$subpop <- factor(stunting_prov$subpop, levels = unique(stunting_prov$subpop)) # transform into factor
levels(stunting_prov$subpop)

stunt_prov_plt <- ggplot(stunting_prov, aes(x = mnthyr, y = stunt.rateP, colour=usaid_blue )) +
  geom_point(alpha=.4, size=.7) + 
  #geom_line(size=1) +
  geom_smooth(method = loess, size = .8, se=FALSE) +
  scale_y_continuous(limits = c(0,.05),
                     labels = percent,
                     breaks = c(.01,.02,.03,.04,.05)) +
  
  labs(x="", y="", caption="Data Source: HMIS", title="Stunting rate (%) in under 5s at facility, 2019 - 2023") +
  facet_wrap(~prov, ncol=4) +
  faceted +
  scale_color_manual(name ="",
                     values = usaid_blue,
                     labels = "Stunting Rates") + 
  basem 

stunt_prov_plt

ggsave("viz/May 2023 data review/Stunting rates facets.png",
       device="png",
       type="cairo",
       height = 6.5,
       width = 12)





#'*______PROVINCIAL WASTING RATES in Under 5s*

wasting_prov <- read_xls("data/May 2023 FHDR/Child Heath provincial level_monthly.xls")
names(wasting_prov)
wasting_prov
wasting_prov  <- wasting_prov  %>%
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

names(wasting_prov)
wasting_prov <- wasting_prov %>%
  rename(prov=1,
         waste.rate=17) %>%
  mutate(waste.rateP = waste.rate/100)

#'*set Vitamin A supplement to 100 for all values >100*
wasting_prov <- wasting_prov %>% 
  dplyr::mutate(waste.rate = ifelse(waste.rate > 100, 100, waste.rate)) %>% 
  dplyr::mutate(waste.rateP = waste.rate/100)

#'*To create legend, gather method for including a legend --*

wasting_prov <- gather(wasting_prov, key = subpop , value = rate, c(waste.rate))
wasting_prov$subpop <- factor(wasting_prov$subpop, levels = unique(wasting_prov$subpop)) # transform into factor
levels(wasting_prov$subpop)

waste_prov_plt <- ggplot(wasting_prov, aes(x = mnthyr, y = waste.rateP, colour=usaid_blue )) +
  geom_point(alpha=.4, size=.7) + 
  #geom_line(size=1) +
  geom_smooth(method = loess, size = .8, se=FALSE) +
  scale_y_continuous(limits = c(0,.05),
                     labels = percent,
                     breaks = c(.01,.02,.03,.04,.05)) +
  
  labs(x="", y="", caption="Data Source: HMIS", title="Wasting rate (%) in under 5s at facility, 2019 - 2023") +
  facet_wrap(~prov, ncol=4) +
  faceted +
  scale_color_manual(name ="",
                     values = usaid_blue,
                     labels = "Wasting Rates") + 
  basem 

waste_prov_plt

ggsave("viz/May 2023 data review/wasting rates facets.png",
       device="png",
       type="cairo",
       height = 6.5,
       width = 12)





#'*______PROVINCIAL CHILD DEWORMING RATES*

deworming_prov <- read_xls("data/May 2023 FHDR/Child Heath provincial level_monthly.xls")
names(deworming_prov)
deworming_prov
deworming_prov  <- deworming_prov  %>%
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

names(deworming_prov)
deworming_prov <- deworming_prov %>%
  rename(prov=1,
         deworm.rate=15) %>%
  mutate(deworm.rateP = deworm.rate/100)

#'*set deworming to 100 for all values >100*
deworming_prov <- deworming_prov %>% 
  dplyr::mutate(deworm.rate = ifelse(deworm.rate > 100, 100, deworm.rate)) %>% 
  dplyr::mutate(deworm.rateP = deworm.rate/100)

#'*To create legend, gather method for including a legend --*

deworming_prov <- gather(deworming_prov, key = subpop , value = rate, c(deworm.rate))
deworming_prov$subpop <- factor(deworming_prov$subpop, levels = unique(deworming_prov$subpop)) # transform into factor
levels(deworming_prov$subpop)

dw_prov_plt <- ggplot(deworming_prov, aes(x = mnthyr, y = deworm.rateP, colour=usaid_blue )) +
  geom_point(alpha=.4, size=.7) + 
  #geom_line(size=1) +
  geom_smooth(method = loess, size = .8, se=FALSE) +
  scale_y_continuous(limits = c(0,.5),
                     labels = percent,
                     breaks = c(.1,.2,.3,.4,.5)) +
  
  labs(x="", y="", caption="Data Source: HMIS", title="Child Deworming Rate, 2019 - 2023") +
  facet_wrap(~prov, ncol=4) +
  faceted +
  scale_color_manual(name ="",
                     values = usaid_blue,
                     labels = "Child Deworming Rate") + 
  basem 

dw_prov_plt

ggsave("viz/May 2023 data review/Deworming rates facets.png",
       device="png",
       type="cairo",
       height = 6.5,
       width = 12)



#'*_________ANC VISITS - PROVINCIAL*

mat_prov <- read_xls("data/Aug 2023 MHDR/Reproductive Maternal Health_Provincial level monthly.xls")

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


#'* ___ANC coverage ----*

mat_prov <- mat_prov %>%
  rename(prov = 1,
         ancc = 3,
         anc1 = 4,
         anc1u20 = 5) %>%
  mutate(anccp = ancc/100,
         anc1p = anc1/100,
         anc1u20p = anc1u20/100)

#'*set anccp to 100 for all values >100*
mat_prov <- mat_prov %>% 
  dplyr::mutate(ancc = ifelse(ancc > 100, 100, ancc)) %>% 
  dplyr::mutate(anccp = ancc/100)

#'*To create legend, gather method for including a legend --*

mat_prov <- gather(mat_prov, key = subpop , value = rate, c(anccp, anc1p,anc1u20p))
mat_prov$subpop <- factor(mat_prov$subpop, levels = unique(mat_prov$subpop)) # transform into factor
levels(mat_prov$subpop)


ggplot(mat_prov, aes(x = mnthyr, y = rate, group = subpop, colour = subpop)) +
  geom_point(alpha=.6, size=.6) + 
  #geom_line(size=1) +
  geom_smooth(method = loess, size = .8, se=FALSE) +
  scale_y_continuous(limits = c(0,1),
                     labels = percent,
                     breaks = c(.2,.4,.6,.8, 1)) +
  # scale_x_date(date_labels="%b %Y",date_breaks="4 months") +
  labs(x ="", y="", caption = "Data Source: HMIS") +labs(x ="", y="", caption = "Data Source: HMIS") +
  facet_wrap(~prov, ncol=4) +
  faceted +
  ggtitle("Proportion of expected pregnancies receiving Antenatal Care (ANC), 2019 - 2023") +
  scale_color_manual(name ="",
                     values = usaid_palette,
                     labels = c("1st ANC coverage (all trimesters)", "1st ANC Coverage (1st Trimester)", 
                                "1st ANC visits in the 1st trimester: Women <20 yrs")
  ) + 
  base

ggsave("viz/Aug 23 FHDR/Provincial ANCs PS.png",
       device="png",
       type="cairo",
       height = 6.5,
       width = 12)




#'*________________________FAMILY PLANNING INDICATORS*


fam_prov <- read_xls("data/May 2023 FHDR/Family Planning data_Provincial level monthly.xls")
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




#'*WOMEN OF REPRODUCTIVE AGE VISITED BY CHA's*
names(fam_prov)
fam_prov <- fam_prov %>%
  rename(prov=2,
         wmn.vstd=3)

chavst_plt <- ggplot(fam_prov, aes(x=mnthyr, y=wmn.vstd, colour=usaid_blue)) + 
  #geom_bar(stat="identity") +
  geom_point(alpha=.6, size=.7) +
  geom_smooth(method = loess, size = .8, se=FALSE) +
  scale_y_continuous(labels=comma) +
  labs(x="", y="", caption="Data Source: HMIS", title="Number of Women in reproductive age visited by CHA.") +
  facet_wrap(~prov, ncol=4) +
  faceted +
  scale_color_manual(name ="",
                     values = usaid_blue,
                     labels ="Women of Reproductive age visited by CHA") +  
  base

chavst_plt
ggsave("viz/May 2023 data review/Women of reproductive age visited by CHA.png",
       device="png",
       type="cairo",
       height = 6.5,
       width = 11)


#'*____________CLIENTS ACCESSING LARC*

names(fam_prov)

larc <- fam_prov %>%
  rename(iucd.inserted = 11,
         implant.inserted = 12
  ) %>%
  
  mutate(larc.ab = iucd.inserted + 
           implant.inserted)

lrc_plt <- ggplot(larc, aes(x=mnthyr, y=larc.ab)) + 
  geom_point(color= usaid_blue, alpha=.6, size=1) + 
  geom_smooth(method = loess,color= usaid_blue, se=F, size=1.1, alpha=.8) +
  #scale_x_date(date_labels="%b %y",date_breaks="3 months") +
  facet_wrap(~prov, ncol=4) +
  faceted +
  scale_y_continuous(labels=comma,
                     limits=c(0, 8000),
                     breaks = c(1000, 2000, 3000, 4000, 5000, 6000, 7000, 8000)) +
  labs(x="",
       y="",
       caption="Data Source: HMIS",
       title="Number of clients accessing LARCs (implants and IUDs) shows a downward trend in Central, \nCopperbelt, Eastern and Muchinga Provinces while other provinces show no significant trends.") + 
  baseX

lrc_plt

ggsave("viz/May 2023 data review/Accessing LARCS Facets.png",
       device="png",
       type="cairo",
       height = 6.5,
       width = 12)




#'*PERCENTAGE OF DISCONTINUNING LARC*
names(fam_prov)

larc <- fam_prov %>%
  rename(iucd.inserted = 11,
         implant.inserted = 12,
         iucd.removed = 8,
         implant.removed = 9
  ) %>%
  
  mutate(larc.dis.p = (iucd.removed + implant.removed) / (iucd.inserted + implant.inserted))


larc.dis.p_plt <- ggplot(larc, aes(x=mnthyr, y=larc.dis.p)) + 
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
       title="Percentage of clients discontinuing LARC begining mid-2021, show a sharp increase in all \nthe provinces except Luapula and  Western provinces.") + 
  baseX

larc.dis.p_plt

ggsave("viz/May 2023 data review/Discontinuing LARCS Facets.png",
       device="png",
       type="cairo",
       height = 6.5,
       width = 11)



mat_prov <- read_xls("data/May 2023 FHDR/Reproductive Maternal Health_Provincial level monthly.xls")
names(fam_prov)
fam_prov <- fam_prov %>%
  rename(wmn.vstd=3)

chavst_plt <- ggplot(fam_prov, aes(x=mnthyr, y=wmn.vstd, colour=usaid_blue)) + 
  #geom_bar(stat="identity") +
  geom_point(alpha=.6, size=.7) +
  geom_smooth(method = loess, size = .8, se=FALSE) +
  scale_y_continuous(labels=comma) +
  labs(x="", y="", caption="Data Source: HMIS", title="Number of Women in reproductive age visited by CHA.") +
  facet_wrap(~prov, ncol=4) +
  faceted +
  scale_color_manual(name ="",
                     values = usaid_blue,
                     labels ="Women of Reproductive age visited by CHA") +  
  base

chavst_plt
ggsave("viz/May 2023 data review/Women of reproductive age visited by CHA.png",
       device="png",
       type="cairo",
       height = 6.5,
       width = 11)



#'*_____PROVINCIAL FP METHODS -TYPE DISAGGS*

fpmethod_prov <- read_xls("data/May 2023 FHDR/IUCD_Implant_Injectables_Provincial monthly.xls")
fpmethod_prov  <- fpmethod_prov  %>%
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

sum(fpmethod_prov$month_chr!=fpmethod_prov$month) # expecting 0 if vars same

names(fpmethod_prov)
fpmethod_prov1 <- fpmethod_prov %>%
  select(1:5,11) %>%
  rename(prov=1)
  #na.omit() 

fpmethod_prov1
colnames(fpmethod_prov1)

fpmethod_prov1

fpmethod_prov2 <- fpmethod_prov1 %>%
  select(1,3,4,5,6) %>%
  na.omit()
  names(fpmethod_prov2)

#fpmethod_prov3 <- melt(fpmethod_prov2, id = "mnthyr")

method_prov_plt <- ggplot(fpmethod_prov2, aes(x=mnthyr, y=value, color=variable))+
  geom_point(alpha=.6, size=.6) +
  geom_smooth(method =loess,se=F, size=.9, alpha=.8) +
  #scale_x_date(date_labels="%b %y",date_breaks="3 months") +
  scale_y_continuous(labels=comma) +
  labs(x="",
       y="",
       caption="Data Source: HMIS",
       title="Family Planning Methods and their usage/consumption, 2019 - 2023") +
  facet_wrap(~prov, ncol=4) +
  faceted +
  basey + scale_color_manual(name ="",
                             values =c(light_blue,light_grey,usaid_blue),
                             labels = c("IUCDs","Implants","Injectables"))
method_prov_plt


ggsave("viz/May 2023 data review/Provincial Family planning methods facets.png",
       device="png",
       type="cairo",
       height = 6.5,
       width = 11)


#'*__Relook at the code above!!!*'

#'*____New Acceptors Starting FP*

names(fam_prov)
nfaccpt <- fam_prov %>%
  select(2,24:27,56) %>%
  rename(prov=1) %>%
  na.omit() 

nfaccpt
colnames(nfaccpt)
nfaccpt1 <- nfaccpt[, c(1,2,3,4,5,6)]
colnames(nfaccpt1)
nfaccpt1

nfaccpt1

nfaccpt2 <- nfaccpt1 %>%
  select(6,1,2,3,4,5) %>%
  na.omit()

names(nfaccpt2)


nfaccpt3 <- reshape2::melt(nfaccpt2, id = "mnthyr")

plt <- ggplot(nfaccpt2, aes(x=mnthyr, y=value, color=variable))+
  geom_point(alpha=.6, size=.6) +
  geom_smooth(method =loess,se=F, size=.9, alpha=.8) +
  #scale_x_date(date_labels="%b %y",date_breaks="3 months") +
  scale_y_continuous(labels=comma) +
  labs(x="",
       y="",
       caption="Data Source: HMIS",
       title="Family Planning New Acceptors (Starting FP), 2019 - 2023") +
  facet_wrap(~prov, ncol=4) +
  faceted +
  basey + scale_color_manual(name ="",
                             values =c(light_blue,light_grey,usaid_blue),
                             labels = c("under 15yrs","15-19yrs","20-24yrs","above 25yrs"))
plt


ggsave("viz/May 2023 data review/Provincial Family planning New Acceptors facets.png",
       device="png",
       type="cairo",
       height = 6.5,
       width = 11)


#'*Provincial Maternal Mortality Ratio and Reporting Rates*

matprv1 <- read_xls("data/May 2023 FHDR/Maternal MR and RR_provincial (2019-2023).xls")


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
#Bars & lines

matprv3
ggplot(matprv3, aes(x=yr, y=mr)) +
  geom_col(stat="identity", position=position_dodge(), fill=usaid_blue) +
  geom_line(aes(x = yr, y = hrr*3.34, color=usaid_red)) +
  # geom_point(aes(aes(x= yr, y= hrr*2.2),color=usaid_red, size=3)) +
  facet_wrap(~prov) +
  faceted +
  scale_y_continuous(sec.axis = sec_axis(trans = ~ .*0.0030,name = "Reporting rate", labels = scales::label_percent())) +
  labs(x="", y="Mortality Ratio", caption="Data Source: HMIS",title="Maternal Mortality Ratio and Reporting rates - Quarters 1, 2019-2023") +
  scale_color_manual(name ="",
                     values = usaid_red,
                     labels = c("HIA2 Reporting rate (%)")) + 
  basem + geom_label(aes( x= yr, y = hrr*3.34,label=hrr), fontface = "bold", hjust=0.6, vjust = 0.7)

ggsave("viz/May 2023 data review/MMR and HIA2 RR.png",
       device="png",
       type="cairo",
       height = 6.5,
       width=12.5)





#'*_______SYPHILIS AND ANC SCREENING COVERAGE*
#'*Code under construction!!*

syphanc.nmr_prov <- read_xls("data/May 2023 FHDR/quarterly ANC and Syphilis Screening Coverage_Provincial.xls")
syphanc.nmr_prov  <- syphanc.nmr_prov  %>%
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

sum(syphanc.nmr_prov$month_chr!=syphanc.nmr_prov$month) # expecting 0 if vars same


names(syphanc.nmr_prov)
syphanc.nmr_prov <- syphanc.nmr_prov %>%
  rename(prov = 1,
         anccov = 3,
         syph = 4,
         nnmr = 5
  ) %>%
  
  mutate(anccovP = anccov/100,
         syphP = syph/100)

#'*set anccovP & syphP to 100 for all values >100*
syphanc.nmr_prov <- syphanc.nmr_prov %>% 
  dplyr::mutate(anccov = ifelse(anccov > 100, 100, anccov)) %>% 
  dplyr::mutate(anccovP = anccov/100)

#'*To create legend, gather method for including a legend --*

syphanc.nmr_prov <- gather(syphanc.nmr_prov, key = subpop , value = rate, c(anccovP, syphP, nnmr))
syphanc_prov$subpop <- factor(syphanc.nmr_prov$subpop, levels = unique(syphanc.nmr_prov$subpop)) # transform into factor
levels(syphanc.nmr_prov$subpop)

syphanc.nnmr_plt <- ggplot(syphanc.nmr_prov, aes(x = mnthyr, y = rate, group = subpop, colour = subpop)) +
  # #geom_point(alpha=.5, size=.5) + 
  # #geom_line(size=1) +
  # geom_smooth(method = loess, size = .8, se=FALSE) +
  # scale_y_continuous(limits = c(0,1),
  #                    labels = percent,
  #                    breaks = c(.1,.2,.3,.4,.5,.6,.7,.8,.9, 1)) +
  # #scale_x_date(date_labels="%b %y",date_breaks="3 months") +
  scale_y_continuous(sec.axis = sec_axis(trans = ~ .*0.045,name = "Perinatal Mortality Rate", labels = scales::label_value)) +
  labs(x="", y="", caption="Data Source: HMIS", title="Coverage of ANC and Syphilis Screening during ANC visits, 2019 - 2023 Q1") +
  facet_wrap(~prov, ncol=4) +
  faceted +
  scale_color_manual(name ="",
                     values = usaid_palette,
                     labels = c("1st ANC coverage (all trimesters)", "Syphilis screening rates (%) at 1st ANC", "Perinatal Mortality Rate")
  ) + 
  basem

syphanc.nnmr_plt

ggsave("viz/May 2023 data review/Syphilis and ANC Coverage facets.png",
       device="png",
       type="cairo",
       height = 6.5,
       width = 12)




