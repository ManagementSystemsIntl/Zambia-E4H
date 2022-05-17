

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

#A bcg object ----
dat_immun2_bcg <- dat_immun2 %>% 
  filter(subpop == "bcg1")

#bcg target
bcg_target <- read_xls("data/Processed data.xls",
                             sheet="Child Health",
                             range="B20:E20") %>%
  pivot_longer(2:5,
               names_to="year") %>%
  rename(type=1) %>%
  mutate(year=as.numeric(year),
         value=as.numeric(value),
         mnthyr=ymd(paste(year, "-12-01")))

#BCG chart ----
ggplot(dat_immun2_bcg, aes(x = mnthyr
                       , y = rate_fix
                       , group = subpop
                       , color = subpop)) +
  geom_point(alpha = .6, size = 1) + 
  geom_smooth(size = .7
              
              , se = FALSE) +
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
        , legend.position = "bottom"
  ) 

#save the viz
ggsave("viz/BCG.png",
       device="png",
       type="cairo",
       height=4,
       width=7)


#DPT object ----
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
  labs(title = "Proportion of infants who received the DPT-Hib-Hep 1st dose\nvaccine within 1 year (2018-2022)"
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
        , legend.position = "bottom"
  ) 

#save the viz
ggsave("viz/DPT.png",
       device="png",
       type="cairo",
       height=4,
       width=7)


#Measles 1 and 2 object and viz ----
dat_immun2_mea <- dat_immun2 %>% 
  filter(subpop %in% c("measles1"
                       , "measles2"))

#DPT chart ----
ggplot(dat_immun2_mea, aes(x = mnthyr
                           , y = rate_fix
                           , group = subpop
                           , color = subpop)) +
  geom_point(alpha = .6, size = 1) + 
  geom_smooth(size = .7
              
              , se = FALSE) +
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
        , legend.position = "bottom"
  ) 

#save the viz
ggsave("viz/measles.png",
       device="png",
       type="cairo",
       height=4,
       width=7)

#fully immunized 1 year
#Measles 1 and 2 object and viz ----
dat_immun2_ful <- dat_immun2 %>% 
  filter(subpop %in% c("imm1"
                       , "imm2"))

targets <- data.frame(year = c(2018, 2019, 2020, 2021)
                      , x1 = c(as.Date("2018-10-01")
                               , as.Date("2019-10-01")
                               , as.Date("2020-10-01")
                               , as.Date("2021-10-01"))
                      , x2 = c(as.Date("2019-01-01")
                               , as.Date("2020-01-01")
                               , as.Date("2021-01-01")
                               , as.Date("2022-01-01"))
                      , y1 = c(as.numeric(.79)
                              , as.numeric(.85)
                              , as.numeric(.9)
                              , as.numeric(.96))
                      , y2 = c(as.numeric(.79)
                               , as.numeric(.85)
                               , as.numeric(.9)
                               , as.numeric(.96)))

                      
                    
#fully immunized year 1 & 2----
ggplot(filter(dat_immun2_ful
              , subpop == "imm1")
       , aes(x = mnthyr, y = rate_fix
            , group = subpop
            , color = subpop)) +
  geom_point(alpha = .6, size = 1) + 
  geom_smooth(size = .7
              , se = FALSE) +
  geom_segment(data = targets
               , aes(x = x1
                     , xend = x2
                     , y = y1
                     , yend = y2)
               , color = usaid_red
               , inherit.aes = FALSE) +
  geom_text(data = targets
            , aes(x = x2
                  , y = y1
                  , label = paste0(y1*100, "%"))
            , color = usaid_red
            , vjust = -.4
            , size = 3
            , inherit.aes = FALSE)+
  scale_y_continuous(limits = c(0,1),
                     labels = percent) +
  labs(title = "Proportion of infants who are fully vaccinated within 1 year, 2018-2022"
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
  ) +
  annotate(geom="text", x=as.Date("15-5-2018", format = "%d-%m-%Y"), y=.75, colour = usaid_red, label="National Targets", size= 3)
  #annotate(geom="text", x=as.Date("01-06-2021", format = "%d-%m-%Y"), y=.1, label="*home deliveries included", size =3, fontface = 'italic')

#save the viz
ggsave("viz/fully imm 1 year.png",
       device="png",
       type="cairo",
       height=4,
       width=7)

#fully immunized year 2
ggplot(filter(dat_immun2_ful
              , subpop == "imm2")
       , aes(x = mnthyr, y = rate_fix
             , group = subpop
             , color = subpop)) +
  geom_point(alpha = .6, size = 1) + 
  geom_smooth(size = .7
              , se = FALSE) +
  #geom_segment(data = targets
   #            , aes(x = x1
   #                  , xend = x2
    #                 , y = y1
    #                 , yend = y2)
      #         , color = usaid_red
      #         , inherit.aes = FALSE) +
  #geom_text(data = targets
     #       , aes(x = x2
      #            , y = y1
      #            , label = paste0(y1*100, "%"))
      #     , color = usaid_red
       #     , vjust = -.4
       #     , size = 3
       #     , inherit.aes = FALSE)+
  scale_y_continuous(limits = c(0,1),
                     labels = percent) +
  labs(title = "Proportion of infants who are fully vaccinated within 2 years, 2018-2022"
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
  ) #+
# annotate(geom="text", x=as.Date("15-5-2018", format = "%d-%m-%Y"), y=.75, colour = usaid_red, label="National Targets", size= 3)
#annotate(geom="text", x=as.Date("01-06-2021", format = "%d-%m-%Y"), y=.1, label="*home deliveries included", size =3, fontface = 'italic')

#save the viz
ggsave("viz/fully imm 2 year.png",
       device="png",
       type="cairo",
       height=4,
       width=7)

dat_immun2_imm <- dat_immun2 %>% 
  filter(subpop %in% c("imm1", "imm2")) 

dat_immun2_imm$subpop <- dat_immun2_imm$subpop %>% 
  recode("imm1" = "Within 1 year"
         , "imm2" = "Within 2 years")

#smooth chart of imm1 and imm2 ---
ggplot(dat_immun2_imm
              , aes(x = mnthyr
                       , y = rate_fix
                       , group = subpop
                       , color = subpop)) +
  geom_point(alpha = .6, size = 1) + 
  geomtextpath::geom_textsmooth(size = 5
                                , aes(label = subpop)
                                  , alpha = .6) +
  scale_y_continuous(limits = c(0,1),
                     labels = percent) +
  labs(title = "Immunization Rates (2018-2022)"
       , subtitle = "Immunization rates rise during spring and fall campaigns"
       , x = ""
       , y = ""
       , caption = "Source: Zambia Ministry of Health") +
  scale_color_manual(name = "",
                      , values = usaid_palette6) +
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
ggsave("viz/Fully Immunized 1 2.png",
       device="png",
       type="cairo",
       height=4,
       width=7)

#fully immunized year 2 ----
ggplot(filter(dat_immun2_ful
              , subpop == "imm2")
       , aes(x = mnthyr
             , y = rate_fix
             , group = subpop
             , color = subpop)) +
  geom_point(alpha = .6, size = 1) + 
  geom_smooth(size = .7
              , se = FALSE) +
#  geom_segment(data = targets
 #              , aes(x = x1
  #                   , xend = x2
   #                  , y = y1
    #                 , yend = y2)
     #          , color = "maroon"
      #         , inherit.aes = FALSE) +
  #geom_text(data = targets
   #         , aes(x = x2
    #              , y = y1
     #             , label = paste0(y1*100, "%"))
      #      , color = "maroon"
       #     , vjust = -.4
        #    , size = 3
         #   , inherit.aes = FALSE)+
  scale_y_continuous(limits = c(0,1),
                     labels = percent) +
  labs(title = "Proportion of infants who are fully vaccinated within 2 years, (2018-2022)"
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
ggsave("viz/fully imm 2 year.png",
       device="png",
       type="cairo",
       height=4,
       width=7)
#Same viz with a geom_smooth() and not a geom_line() ----
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

#Now use the child health data for nutrition  ----

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
 <span style = 'color:#BA0C2F;'> exclusively breastfed until 6 months</span> and who received <span style = 'color:#A7C6ED;'>Vitamin A at 6 and 11 months</span>"

#basic line chart of child health data  ----
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

#smooth chart of immunization data  ----
ggplot(dat_nutri, aes(x = mnthyr
                      , y = rate_fix
                      , group = subpop
                      , color = subpop
                      , label = subpop)) +
  geom_smooth(method = loess
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

#targets breastfeeding
targets_bf <- data.frame(year = c(2018, 2019, 2020, 2021)
                      , x1 = c(as.Date("2018-10-01")
                               , as.Date("2019-10-01")
                               , as.Date("2020-10-01")
                               , as.Date("2021-10-01"))
                      , x2 = c(as.Date("2019-01-01")
                               , as.Date("2020-01-01")
                               , as.Date("2021-01-01")
                               , as.Date("2022-01-01"))
                      , y1 = c(as.numeric(.79)
                               , as.numeric(.86)
                               , as.numeric(.93)
                               , as.numeric(1))
                      , y2 = c(as.numeric(.79)
                               , as.numeric(.86)
                               , as.numeric(.93)
                               , as.numeric(1)))



#Breastmilk within 1 hour chart
#smooth chart of immunization data  ----
ggplot(filter(dat_nutri, subpop == "breastmilk_1h")
              , aes(x = mnthyr
                      , y = rate_fix
                      , group = subpop
                      , color = subpop
                      , label = subpop)) +
  geom_point(alpha = .6, size = 1)+
  geom_smooth(method = loess
              , size = .7
              , se = FALSE
              , alpha = .6) +
  geom_segment(data = targets_bf
               , aes(x = x1
                     , xend = x2
                     , y = y1
                     , yend = y2)
               , color = "maroon"
               , size = .7
               , inherit.aes = FALSE)+
  geom_text(data = targets_bf
            , aes(x = x2
                  , y = y1
                  , label = paste0(y1*100, "%"))
            , color = "maroon"
            , vjust = -.4
            , size = 3
            , inherit.aes = FALSE)+
  scale_y_continuous(limits = c(0,1),
                     labels = percent) +
  labs(title = "Proportion of infants breastfeeding within 1 hour, 2018-2022"
       , x = ""
       , y = ""
       , caption = "Source: Zambia Ministry of Health") +
  scale_color_manual(name = ""
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
  ) +
  annotate(geom="text", x=as.Date("15-5-2018", format = "%d-%m-%Y"), y=.75, colour = usaid_red, label="National Targets", size= 3)

#save the viz
ggsave("viz/breastfed within 1 hour.png",
       device="png",
       type="cairo",
       height=4,
       width=7)


####Mapping immunizations ----

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
         , periodname
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
                  , scale_hjust_manual = c(.1, .7, .5, .9, .1, .3)                 , vjust = 0
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

#Shapes to make maps
library(rgeoboundaries)

zam <- geoboundaries(country = "Zambia"
                     , adm_lvl = 1) %>% 
  select(shapeName)

zam$shapeName <- recode(zam$shapeName
                        , "North Western" = "Northwestern")

#Quick map of fully immune year 1
#the statistic showed is an annual mean of the quarterly rates
#devtools::install_github("yutannihilation/ggsflabel")
library(ggsflabel) #provides geom_sf_text_repel

#map colors
#install.packages("rcartocolor")
library(rcartocolor)

map_colors <- carto_pal(name = "Sunset")

dat_immun_geo <- dat_immun_reg3 %>%
  filter(periodname != "April 2022") %>% 
  group_by(year, organisationunitname, subpop) %>% 
  summarise(value = mean(rate_fix)) %>% 
  mutate(target = case_when(as.numeric(year) == 2018 ~ .79
                            , as.numeric(year) == 2019 ~ .86
                            , as.numeric(year) == 2020 ~ .9
                            , as.numeric(year) == 2021 ~ .95))
                            
                                    
                                    
immun_geo <- left_join(dat_immun_geo
                           , zam
                           , by = c("organisationunitname" = "shapeName")) %>% 
  sf::st_as_sf()

#Map of fully immune year 1
ggplot(filter(immun_geo, subpop == "imm1")
       , aes(geometry = geometry
             , fill = value)) +
  geom_sf()+
  ggsflabel::geom_sf_text(aes(label = organisationunitname)
                                , size = 2)+
  facet_wrap(~year) +
  scale_fill_carto_c(name="Proportion of\ninfants vaccinated"
                     , palette = "Sunset")+
  labs(title = "Proportion of infants fully immunized by region, 2018-2022"
       , subtitle = "Darker colors represent a higher proportion of vaccinated infants") +
  theme_void()+
  theme(plot.title.position = "plot"
        , plot.title = element_text(size = 14)
        , legend.title = element_text(size = 12) 
        , legend.text = element_text(size = 9)
        , legend.position = "right")

ggsave("viz/map immun 1 year.png",
       device="png",
       type="cairo",
       height=4,
       width=7)






#To add targets to a map
#case_when(value >= target ~ "#f3e79b"
 #         , value >= target *.8 ~ "#f8a07e"
  #        , value < target ~ "#a059a0")))


#### Functions for plots
#Here we're going to map 6 plots. I'd use facet_grid(), but I don't know how to make the charts reorder for each facet. So, I iterate over a list of six ggplots using map()

#My type vector
immun_geo$subpop <- immun_geo$subpop %>% 
  recode("bcg1" = "BCG1"  
         , "dpt_hib_hep1" = "DPT_HIB_HEP1" 
         , "imm1" = "Fully Immunized Within 1 Year"
         , "imm2" = "Fully Immunized Within 2 Years"
         , "measles1" = "Measles Vaccine (under 1)" 
         , "measles2" = "Measles Vaccine (under 2)")

type <- unique(immun_geo$subpop)

#My function for filtering by type
type_fun <- function(x){
  immun_geo %>% 
    filter(subpop == {{x}}) %>% 
    select(1:4, 6) 
    
}

#Now run it for real
z <-map(type, ~type_fun(.x))

#Plot the object and organize it by Count. The facet_wrap function generates six charts, one for each of the event types
plot_fun <- function(x) {
  ggplot(x
         , aes(geometry = geometry
                , fill = value)) +
    geom_sf()+
    ggsflabel::geom_sf_text(aes(label = organisationunitname)
                            , size = 2)+
    facet_wrap(~year) +
    scale_fill_carto_c(name="Proportion of\ninfants vaccinated"
                       , palette = "Sunset") +
    labs(title = x[[3]]
         ,subtitle = "Darker colors represent a higher proportion of vaccinated infants") +
    theme_void()+
    theme(plot.title.position = "plot"
          , plot.title = element_text(size = 14)
          , legend.title = element_text(size = 8) 
          , legend.text = element_text(size = 7)
          , legend.position = "right")
  
}

  
six_plot <- map(z, ~plot_fun(.x))

bcg_map <- six_plot[[1]]

ggsave(plot = bcg_map
       , "viz/bcg_map.png"
       , device="png"
       , type="cairo"
       , height=6
       , width=9)

dpt_map <- six_plot[[2]]

ggsave(plot = dpt_map
       , "viz/dpt_map.png"
       , device="png"
       , type="cairo"
       , height=6
       , width=9)

imm1_map <- six_plot[[3]]
ggsave(plot = imm1_map
       , "viz/imm1_map.png"
       , device="png"
       , type="cairo"
       , height=6
       , width=9)

imm2_map <- six_plot[[4]]

ggsave(plot = imm2_map
       , "viz/imm2_map.png"
       , device="png"
       , type="cairo"
       , height=6
       , width=9)

meas1_map <- six_plot[[5]]

ggsave(plot = meas1_map
       , "viz/meas1_map.png"
       , device="png"
       , type="cairo"
       , height=6
       , width=9)


meas2_map <- six_plot[[6]]
ggsave(plot = meas2_map
       , "viz/meas2_map.png"
       , device="png"
       , type="cairo"
       , height=6
       , width=9)




#This works
plot_row <- cowplot::plot_grid(plotlist = six_plot
                               #, nrow = 3
                               #, ncol = 3
                               , label_size = 14
                               , label_fontface = "plain"
                               , label_fontfamily = "Open sans")

title <- cowplot::ggdraw() + 
  cowplot::draw_label(
    "Proportions"
    , fontface = 'plain'
    , fontfamily = "Open sans"
    , size = 26
    , x = 0
    , hjust = 0
  ) +
  theme(
    # add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 7)
  )

caption <- cowplot::ggdraw() +
  cowplot::draw_label("Source: Zambia Ministry of Health"
                      , fontface = "plain"
                      , fontfamily = "Corbel"
                      , size = 10
                      , hjust = -.5)

plot_grid(
  title, plot_row, caption)
  # rel_heights values control vertical title margins

