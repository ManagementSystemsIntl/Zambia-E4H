
source("scripts/r prep2.r")
source("scripts/r prep3.r")


#'*________PROPORTION BREASTFED WITHIN AN HOUR PROVINCIAL FACETED*

bfhr_prov <- read_xls("data/MC Health April 2023/Child Heath provincial level_monthly.xls")
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
  ggtitle("Proportion of Infants Breastfed within 1 hour of birth \nand those on EBF at 6 months, 2019-2023") +
  facet_wrap(~prov, ncol=4) +
  faceted +
  scale_color_manual(name ="",
                     values = usaid_palette,
                     labels = c("Initiation on breastmilk within one hour of birth", "Infants on EBF at 6 months")) + basey

bfhr_plt

ggsave("viz/Apr-Jun 2022/Child Health/Breastfed within 1 hour of birth and EBF faceted PS.png",
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

#'*set EBF & BREASTFEED 1HOUR to 100 for all values >100*
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


#'*_________ANC VISITS - PROVINCIAL*

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

ggsave("viz/May 2023 data review/Provincial ANCs PS.png",
       device="png",
       type="cairo",
       height = 6.5,
       width = 12)

