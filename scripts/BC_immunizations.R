
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

#A bcg object
dat_immun2_bcg <- dat_immun2 %>% 
  filter(subpop == "bcg1")

#bcg target
bcg_target <- read_xls("data/Processed data.xls",
                             sheet="Child Health",
                             range="B19:E20") %>%
  pivot_longer(1:4,
               names_to="year"
               , values_to="target") %>%
  #rename(type=1) %>%
  mutate(year=as.numeric(year),
         value=as.numeric(target),
         mnthyr=ymd(paste(year, "-12-01")))

#bcg chart
ggplot(dat_immun2_bcg, aes(x = mnthyr
                       , y = rate_fix
                       , group = subpop
                       , color = subpop)) +
  geom_point(alpha = .6, size = 1) + 
  geom_smooth(size = .7
              , se = FALSE) +
  #geom_line(data=bcg_target, aes(x=mnthyr
       #                          , y=value)
       #     , color="maroon") +
  #geom_label(data=bcg_target
          #   , aes(x=mnthyr
         #          , y=value
         #         , label=paste(value*100, "%", sep=""))
         #    , color="maroon") +
  scale_y_continuous(limits = c(0,1),
                     labels = percent) +
  labs(title = "Proportion of infants who received the Bacille \nCalmette-Guérin (BCG) vaccine within 1 year (2018-2022)"
       #, subtitle = "Immunization rates rise during spring and fall campaigns"
       , x = ""
       , y = ""
       , caption = "Source: Zambia Ministry of Health") +
  scale_color_manual(name = "",
                     values = usaid_palette) +
  theme(plot.title.position = "plot"
        , plot.title = element_text(size = 14)
        , axis.title.x = element_text(size = 12)
        , axis.title.y = element_text(size = 12)
        , axis.text = element_text(size = 9)
        , legend.title = element_text(size = 12) 
        , legend.text = element_text(size = 7)
        , legend.position = "none"
  ) 

#save the viz
ggsave("viz/BCG.png",
       device="png",
       type="cairo",
       height=4,
       width=7)


#DPT object
dat_immun2_dpt <- dat_immun2 %>% 
  filter(subpop == "dpt_hib_hep1")

#DPT chart
ggplot(dat_immun2_dpt, aes(x = mnthyr
                           , y = rate_fix
                           , group = subpop
                           , color = subpop)) +
  geom_point(alpha = .6, size = 1) + 
  geom_smooth(size = .7
              
              , se = FALSE) +
  scale_y_continuous(limits = c(0,1),
                     labels = percent) +
  labs(title = "Proportion of infants who received the DPT-hib-hep \nvaccine within 1 year (2018-2022)"
       #, subtitle = "Immunization rates rise during spring and fall campaigns"
       , x = ""
       , y = ""
       , caption = "Source: Zambia Ministry of Health") +
  scale_color_manual(name = "",
                     values = usaid_palette) +
  theme(plot.title.position = "plot"
        , plot.title = element_text(size = 14)
        , axis.title.x = element_text(size = 12)
        , axis.title.y = element_text(size = 12)
        , axis.text = element_text(size = 9)
        , legend.title = element_text(size = 12) 
        , legend.text = element_text(size = 7)
        , legend.position = "none"
  ) 

#save the viz
ggsave("viz/DPT.png",
       device="png",
       type="cairo",
       height=4,
       width=7)


#Measles 1 and 2 object and viz
dat_immun2_mea <- dat_immun2 %>% 
  filter(subpop %in% c("measles1"
                       , "measles2"))

#For some reason this breaks my plot
#dat_immun2_mea$subpop <- dat_immun2_mea$subpop %>% 
#  recode_factor("measles1" = "Oneyear"  
#         , "measles2" = "Twoyears")

pop <- paste0(c("1 year"
               , "2 year"))

#DPT chart
ggplot(dat_immun2_mea, aes(x = mnthyr
                           , y = rate_fix
                           , group = subpop
                           , color = subpop)) +
  geom_point(alpha = .6, size = 1) + 
  geom_textsmooth(size = 5
                  , label = pop) +
              #, se = FALSE) +
  scale_y_continuous(limits = c(0,1),
                     labels = percent) +
  labs(title = "Proportion of infants who received the measles \nvaccine within 1 and 2 years (2018-2022)"
       #, subtitle = "Immunization rates rise during spring and fall campaigns"
       , x = ""
       , y = ""
       , caption = "Source: Zambia Ministry of Health") +
  scale_color_manual(name = "",
                     values = usaid_palette) +
  theme(plot.title.position = "plot"
        , plot.title = element_text(size = 14)
        , axis.title.x = element_text(size = 12)
        , axis.title.y = element_text(size = 12)
        , axis.text = element_text(size = 9)
        , legend.title = element_text(size = 12) 
        , legend.text = element_text(size = 7)
        , legend.position = "bottom") 

#save the viz
ggsave("viz/measles.png",
       device="png",
       type="cairo",
       height=4,
       width=7)


#basic line chart of immunization data
ggplot(dat_immun2, aes(x = mnthyr
                       , y = rate_fix
                       , group = subpop
                       , color = subpop)) +
  geom_point(alpha = .6, size = 1) + 
  geom_line(size = .5, alpha = .6) +
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
  theme(plot.title.position = "plot"
        , plot.title = element_text(size = 14)
        , axis.title.x = element_text(size = 12)
        , axis.title.y = element_text(size = 12)
        , axis.text = element_text(size = 9)
        , legend.title = element_text(size = 12) 
        , legend.text = element_text(size = 7)
        , legend.position = "bottom"
  ) 

#save the viz
ggsave("viz/Immunizations.png",
       device="png",
       type="cairo",
       height=4,
       width=7)


#Same viz with a geom_smooth() and not a geom_line()
ggplot(dat_immun2, aes(x = mnthyr
                       , y = rate_fix
                       , group = subpop
                       , color = subpop)) +
  geom_smooth(method = lm
              , size = .7
              , se = FALSE
              , alpha = .4) +
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
  theme(plot.title.position = "plot"
        , plot.title = element_text(size = 14)
        , axis.title.x = element_text(size = 12)
        , axis.title.y = element_text(size = 12)
        , axis.text = element_text(size = 9)
        , legend.title = element_text(size = 12) 
        , legend.text = element_text(size = 9)
        , legend.position = "bottom"
  ) 

#save the viz
ggsave("viz/Immunizations_smooth.png",
       device="png",
       type="cairo",
       height=4,
       width=7)
#Now use the child health data for nutrition

#Select only the columns needed
dat_nutri <- dat_immun %>% 
  select(date = periodname
         , vitA = 13
         , breastmilk_1h = 17
         , ebf_6m = 20) %>% 
  mutate(vitA = vitA/100
         , breastmilk_1h = breastmilk_1h/100
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
                           , cols = c(vitA
                                      , breastmilk_1h
                                      , ebf_6m))

dat_nutri <- dat_nutri %>% 
  mutate(rate_fix = case_when(rate > 1 ~ 1
                              , rate <= 1 ~ rate))

nut_names = c("breastmilk within 1 hour"
              , "exclusively breastfed at 6 months"
              , "Vitamin A at 6 and 11 months")

color = c("#205493"
          , "#BA0C2F"
          , "#A7C6ED")                      
                      
subtitle <- "Proportion of infants <span style = 'color:#205493;'>breastfed within an hour after birth </span>,<br>
 <span style = 'color:#BA0C2F;'> exclusively breasfed until 6 months</span> and who received <span style = 'color:#A7C6ED;'>Vitamin A at 6 and 11 months</span>"

#basic line chart of immunization data
ggplot(dat_nutri, aes(x = mnthyr
                       , y = rate_fix
                       , group = subpop
                       , color = subpop
                      , label = subpop)) +
  geom_point(alpha = .6, size = .5)+
  geom_line(size = .7
              , alpha = .6) +
  scale_y_continuous(limits = c(0,1),
                     labels = percent) +
  labs(title = "Child Health, 2018-2022"
       , subtitle = subtitle
       , x = ""
       , y = ""
       , caption = "Source: Zambia Ministry of Health") +
  scale_color_manual(name = ""
                     , labels = c("Infants receiving breastmilk <= 1 hour"
                                  , "Infants exclusively breastfed at 6 months" 
                                  , "Vitamin A coverage")
                     , values = usaid_palette) +
  theme(plot.title.position = "plot",
        plot.title = element_text(size = 14, hjust = 0),
        plot.subtitle = ggtext::element_markdown(),
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

#smooth chart of immunization data
ggplot(dat_nutri, aes(x = mnthyr
                      , y = rate_fix
                      , group = subpop
                      , color = subpop
                      , label = subpop)) +
  geom_smooth(method = lm
              , size = .7
              , se = FALSE
              , alpha = .6) +
  scale_y_continuous(limits = c(0,1),
                     labels = percent) +
  labs(title = "Child Health, 2018-2022"
       , subtitle = subtitle
       , x = ""
       , y = ""
       , caption = "Source: Zambia Ministry of Health") +
  scale_color_manual(name = ""
                     , labels = c("Infants receiving breastmilk <= 1 hour"
                                  , "Infants exclusively breastfed at 6 months" 
                                  , "Vitamin A coverage")
                     , values = usaid_palette) +
  theme(plot.title.position = "plot",
        plot.title = element_text(size = 14, hjust = 0),
        plot.subtitle = ggtext::element_markdown(),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text = element_text(size = 9),
        legend.title = element_text(size = 12), 
        legend.text = element_text(size = 11)
        , legend.position = "none"
  ) 

#save the viz
ggsave("viz/Nutrition_smooth.png",
       device="png",
       type="cairo",
       height=4,
       width=7)


####Mapping immunizations

dat_immun_reg <- readxl::read_xls("data/Jan-Mar 2022/Child Health Data_Provincial Level(Quarterly).xls") 

#clean var names
library(janitor)

dat_immun_reg <- dat_immun_reg %>% clean_names() #not great, but I don't need too many so not a big deal

#select only the immunization columns

dat_immun_reg2 <- dat_immun_reg %>% 
  dplyr::select(organisationunitname
         , periodname
         , imm1 = fully_immunised_coverage_percent_under_1
         , imm2 = fully_immunised_coverage_percent_under_2_years
         , bcg1 = bcg_coverage_percent_under_1
         , measles1 = measles_1_coverage_percent_under_1
         , measles2 = measles_2_coverage_percent_under_2
         , dpt_hib_hep1 = dpt_hib_hep_1st_dose_coverage_percent_under_1) %>% 
  dplyr::mutate(
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
    , quarter = zoo::as.yearqtr(mnthyr)
  )

dat_immun_reg2 <- pivot_longer(dat_immun_reg2
                           , names_to = "subpop"
                           , values_to = "rate"
                           , cols = c(imm1,imm2,bcg1, measles1, measles2, dpt_hib_hep1))  

dat_immun_reg3 <- dat_immun_reg2 %>% 
  select(organisationunitname
         , year
         , quarter
         , subpop
         , rate) %>% 
  group_by(quarter)

#add rate_fix as a new column that
# with a max rate of 1 

dat_immun_reg3 <-  dat_immun_reg3 %>% 
  mutate(rate_fix = case_when(rate > 1 ~ 1
                              , rate < 1 ~ rate))

dat_immun_reg3$hjust <- dat_immun_reg3$rate_fix*.25

hjust <- c(.1, .2, .3, .4, .5, .6)

#smooth chart of immunization data
ggplot(dat_immun_reg3, aes(x = quarter
                      , y = rate_fix
                      , group = subpop
                      , color = subpop
                      , label = subpop)) +
  geom_textsmooth(aes(label = subpop)
                  , size = 4
                  , text_smoothing = 30
                  , method = "loess"
                  , linewidth = 1
                  , hjust = rep(c(.05, .15, .3, .5, .7, .9), length.out = 480)
                  , vjust = 0
                  , gap = TRUE
                  , alpha = .8)+
  scale_y_continuous(limits = c(0,1),
                     labels = percent) +
  labs(title = "Child Health, 2018-2022"
       , subtitle = "blah"
       , x = ""
       , y = ""
       , caption = "Source: Zambia Ministry of Health") +
  scale_color_manual(name = ""
                     , values = usaid_palette6) +
  theme(plot.title.position = "plot",
        plot.title = element_text(size = 14, hjust = 0),
        plot.subtitle = ggtext::element_markdown(),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text = element_text(size = 9)
        , legend.position = "none") 

#save the viz
ggsave("viz/immun_region_smooth.png",
       device="png",
       type="cairo",
       height=4,
       width=7)



library(rgeoboundaries)

zam <- geoboundaries(country = "Zambia"
                     , adm_lvl = 1) %>% 
  select(shapeName)

zam$shapeName

dat_immun_geo <- left_join(dat_immun_reg3
                            , zam
                            , by = c("organisationunitname" = "shapeName")) %>% 
  sf::st_as_sf()




ggplot(dat_immun_geo
       , aes(geometry = geometry
             , fill = rate_fix)) +
  geom_sf()+
  facet_grid(~year)
