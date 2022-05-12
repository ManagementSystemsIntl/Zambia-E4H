
source("scripts/r prep.R")
remotes::install_github("AllanCameron/geomtextpath", quiet = T)
library(geomtextpath)

#get data
dat_immun <- readxl::read_xls("data/Jan-Mar 2022/Child Health Data_National Level(Monthly).xls")

glimpse(dat_immun)

#clean var names
library(janitor)

dat_immun <- dat_immun %>% clean_names() #not great, but I don't need too many so not a big deal

#select only the immunization columns

dat_immun2 <- dat_immun %>% 
  select(periodname
         , imm1 = fully_immunised_coverage_percent_under_1
         , imm2 = fully_immunised_coverage_percent_under_2_years
         , bcg1 = bcg_coverage_percent_under_1
         , measles1 = measles_1_coverage_percent_under_1
         , measles2 = measles_2_coverage_percent_under_2
         , dpt_hib_hep1 = dpt_hib_hep_1st_dose_coverage_percent_under_1) %>% 
  mutate(
    imm1 = imm1/100
    , imm2 = imm2/100
    , bcg1 = bcg1/100
    , measles1 = measles1/100
    , measles2 = measles2/100
    , dpt_hib_hep1 = dpt_hib_hep1/100
    , month_chr = str_sub(periodname
                             , start = 1
                             , end=nchar(periodname)-5)
         , month = factor(month_chr
                          , levels=c("January"
                                     ,"February"
                                     ,"March"
                                     ,"April"
                                     ,"May"
                                     ,"June"
                                     ,"July"
                                     ,"August"
                                     ,"September"
                                     ,"October"
                                     ,"November"
                                     ,"December"))
         , month_code = as.numeric(month) 
         , year = str_sub(periodname
                          , start=nchar(periodname)-4
                          , end=nchar(periodname))
         , monyr = paste(month_code, year, sep="-")
         , mnthyr = my(monyr)
  )

dat_immun2 <- pivot_longer(dat_immun2
                     , names_to = "subpop"
                     , values_to = "rate"
                     , cols = c(imm1,imm2,bcg1, measles1, measles2, dpt_hib_hep1))

#add rate_fix as a new column that
# with a max rate of 1 

dat_immun2 <-  dat_immun2 %>% 
  mutate(rate_fix = case_when(rate > 1 ~ 1
                                , rate <= 1 ~ rate))


#basic line chart of immunization data
ggplot(dat_immun2, aes(x = mnthyr
                       , y = rate_fix
                       , group = subpop
                       , color = subpop)) +
  geom_point(alpha = .6, size = 1) + 
  geom_line(size = .5, alpha = .6) +
  annotate(geom = "text"
           , x = as.Date(c("2018-01-01"))
           , y = 0
           , hjust = 0
           , vjust = 0
           , label = "Spring campaign") +
  scale_y_continuous(limits = c(0,1),
                     labels = percent) +
  labs(title = "Immunization Rates (2018-2022)"
       , subtitle = "Immunization rates rise during spring and fall campaigns"
       , x = ""
       , y = ""
       , caption = "Source: Zambia Ministry of Health") +
  scale_color_manual(name = "",
                      labels = c("BCG under 1"
                                 ,"dpt, hib, hep under 1"
                                 ,"Fully immunized under 1"
                                 , "Fully immunized under 2"
                                 , "Measles coverage under 1"
                                 , "Measles coverage under 2")
                      , values = usaid_palette6) +
  theme(plot.title.position = "plot",
        plot.title = element_text(size = 14),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text = element_text(size = 9),
        legend.title = element_text(size = 12), 
        legend.text = element_text(size = 11)
  ) 
#save the viz
ggsave("viz/Immunizations.png",
       device="png",
       type="cairo",
       height=4,
       width=7)


#Now use the child health data for nutrition

#Select only the columns needed
dat_nutri <- dat_immun %>% 
  select(date = periodname
         , vitA = 14
         , deworm = 15
         , stunt_u5 = 16
         , waste_u5 = 17
         , breastmilk_1h = 18
         , breastfed_6m = 19
         , pneu_5 = 20
         , dia_no_blood_5 = 21
         , dia_dehyd_5 = 22
         , opd_1 = 23
         , ebf_6m = 24) %>% 
  mutate(vitA = vitA/100
         , deworm = deworm/100
         , stunt_u5 = stunt_u5/100
         , waste_u5 = waste_u5/100
         , breastmilk_1h = breastmilk_1h/100
         , breastfed_6m = breastfed_6m/100
         , pneu_5 = pneu_5/100
         , dia_no_blood_5 = dia_no_blood_5/100
         , dia_dehyd_5 = dia_dehyd_5/100
         , ebf_6m = ebf_6m/100
         , month_chr = str_sub(date
                             , start = 1
                             , end=nchar(date)-5)
         , month = factor(month_chr
                          , levels=c("January"
                                     ,"February"
                                     ,"March"
                                     ,"April"
                                     ,"May"
                                     ,"June"
                                     ,"July"
                                     ,"August"
                                     ,"September"
                                     ,"October"
                                     ,"November"
                                     ,"December"))
         , month_code = as.numeric(month) 
         , year = str_sub(date
                          , start=nchar(date)-4
                          , end=nchar(date))
         , monyr = paste(month_code, year, sep="-")
         , mnthyr = my(monyr)
  ) 

#pivot the data 
dat_nutri <- pivot_longer(dat_nutri
                           , names_to = "subpop"
                           , values_to = "rate"
                           , cols = c(vitA,deworm,stunt_u5,waste_u5
                                      , breastmilk_1h, breastfed_6m
                                      , pneu_5, dia_no_blood_5, dia_dehyd_5
                                      , opd_1, ebf_6m))

#basic line chart of immunization data
ggplot(dat_nutri, aes(x = mnthyr
                       , y = rate
                       , group = subpop
                       , color = subpop
                      , label = subpop)) +
  geom_point(alpha = .6, size = 1) + 
  geom_line(size = .5, alpha = .6) +
  scale_y_continuous(limits = c(0,4),
                     labels = percent) +
  facet_wrap(vars(subpop)
             , nrow = 3
             , scales = "free_y") +
  labs(title = "Child Health (2018-2022)"
       , subtitle = "Subtitle"
       , x = ""
       , y = ""
       , caption = "Source: Zambia Ministry of Health") +
  scale_color_viridis_d(name = "",)+
                        #labels = c(
  theme(plot.title.position = "plot",
        plot.title = element_text(size = 14, hjust = 0),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text = element_text(size = 9),
        legend.title = element_text(size = 12), 
        legend.text = element_text(size = 11)
        , legend.position = "none"
  ) 
#save the viz
ggsave("viz/Nutrition.png",
       device="png",
       type="cairo",
       height=4,
       width=7)


