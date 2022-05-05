
source("scripts/r prep.R")
remotes::install_github("AllanCameron/geomtextpath", quiet = T)
library(geomtextpath)

#get data
dat_immun <- readxl::read_xls("data/Downlaod Extract Childhealth Monthly At National.xls")

glimpse(dat_immun)

#clean var names
library(janitor)

dat_immun <- dat_immun %>% clean_names() #not great, but I don't need too many so not a big deal

#select only the immunization columns

dat_immun2 <- dat_immun %>% 
  select(organisationunitname
         , periodname
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

#basic line chart
ggplot(dat_immun2, aes(x = mnthyr
                       , y = rate
                       , group = subpop
                       , color = subpop)) +
  geom_point(alpha = .6, size = 1) + 
  geom_line(size = .5, alpha = .6) +
  annotate(geom = "text"
           , x = min(dat_immun2$mnthyr)
           , y = max(dat_immun2$rate)
           , label = "Can someone check on these data?\n
           Seems like a lot of % over 100 to me."
           , hjust = 0
           , vjust = 1
           , size = 5
           , color = "red")+
  scale_y_continuous(limits = c(0,5),
                     labels = percent) +
  labs(title = "Immunization Rates (2018-2022)"
       , subtitle = "Immunization rates are fairly consistent across categories"
       , x = ""
       , y = ""
       , caption = "Source: Zambia Ministry of Health") +
  scale_color_viridis_d(name = "",
                      labels = c("BCG under 1"
                                 ,"dpt, hib, hep under 1"
                                 ,"Fully immunized under 1"
                                 , "Fully immunized under 2"
                                 , "Measles coverage under 1"
                                 , "Measles coverage under 2"))+#,
                      #values = c(light_blue
                       #          , medium_grey
                        #         , usaid_blue
                         #        , usaid_red
                          #       , medium_grey
                           #      , rich_black)) +
  theme(plot.title.position = "plot",
        plot.title = element_text(size = 14),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text = element_text(size = 9),
        legend.title = element_text(size = 12), 
        legend.text = element_text(size = 11)
  ) 

ggsave("viz/Immunizations.png",
       device="png",
       type="cairo",
       height=4,
       width=7)
