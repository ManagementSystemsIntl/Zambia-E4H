
# packages ---- 

# packages <- c("arm", "BMA", "brms", "corrplot", "dummies","DescTools", "estimatr","extrafont", "extrafontdb", "janitor",
#               "reshape2","tidyr","broom", "caret", "haven", "HH","Hmisc","lubridate","knitr", "margins", "magrittr", "plotrix",
#               "scales","survey", "srvyr", "sysfonts", "foreign","car", "ICC", "openxlsx", "ggrepel", "readr",
#               "readxl", "sjmisc", "sjPlot", "sjstats", "sjlabelled", "skimr","labelled", "texreg", "janitor","psych","dplyr",
#               "tidyverse", "viridis", "here", "ggridges", "ggthemes", "DT", "jtools", "huxtable", "stringi", "gghighlight",
#               "plm", "brms", "rstan", "rstanarm","tidybayes","texreg","gt","gtsummary","huxtable","stargazer", "gsynth",
#               "panelView", "assertr", "pointblank", "validate", "sandwich", "workflowr", "here", "missForest", "ltm")

# zam_packages <- c("tidyverse", "here", "gt","gtsummary","gghighlight","stringi","DT", "ggthemes","ggridges","viridis",
#               "psych","labelled","readxl", "skimr","sjlabelled","sjstats","sjPlot", "sjmisc","readr","ggrepel","openxlsx",
#               "sysfonts","scales","magrittr","knitr","lubridate","Hmisc","haven","tidyr","reshape2","DescTools","extrafont",
#               "extrafontdb")

# zam_packages2 <- c("tidyverse", "here", 
#                   #"gt","gtsummary",
#                   "gghighlight","stringi","DT", "ggthemes","ggridges","viridis",
#                   "psych","labelled","readxl", "skimr","sjlabelled","sjstats","sjPlot", "sjmisc","readr","ggrepel","openxlsx",
#                   "sysfonts","scales","magrittr","knitr","lubridate","Hmisc","haven","tidyr","reshape2","DescTools","extrafont",
#                   "extrafontdb")

zam_packages <- c("tidyverse", "here", "gt","gtsummary","gghighlight","stringi","DT", "ggthemes","ggridges","viridis",
              "psych","labelled","readxl", "skimr","sjlabelled","sjstats","sjPlot", "sjmisc","readr","ggrepel","openxlsx",
              "sysfonts","scales","magrittr","knitr","lubridate","Hmisc","haven","tidyr","reshape2","DescTools","extrafont",
              "patchwork", "extrafontdb", "ggtext", "geomtextpath", "zoo", "psych","cowplot", "kableExtra")



# lapply(zam_packages, install.packages, character.only=T)

#lapply(zam_packages, library, character.only=T)

# font_import()
# loadfonts(device="win")
# windwsFonts()


# formatting ---- 

#font_add_google("Open Sans", "sans-serif")

options(digits=4, scipen=8)
#options(digits=8, scipen=9)

# set default
base <- theme_bw() + theme(panel.grid.minor.x=element_blank(),
                           panel.grid.minor.y=element_blank(),
                           plot.title=element_text(face="bold",
                                                   size=14, 
                                                   hjust=.5, 
                                                   family = "Gill Sans Mt"),
                           plot.subtitle = element_text(size=12, family="Gill Sans Mt"),
                           plot.caption=element_text(size=12, family="Gill Sans Mt"),
                           axis.title=element_text(size=12, family="Gill Sans Mt"),
                           axis.text=element_text(size=9, family="Gill Sans Mt"),
                           legend.text=element_text(size=11, family="Gill Sans Mt"),
                           strip.text=element_text(size=12, family="Gill Sans Mt"),
                           panel.border=element_blank(),
                           axis.ticks = element_blank()) 

#scale_color_discrete <- usaid_palette

#opts <- options(ggplot2.discrete.color = usaid_palette)

theme_set(base)

faceted <- theme_bw() +
  theme(panel.grid.minor.x=element_blank(),
        panel.grid.minor.y=element_blank(),
        plot.title=element_text(face="bold",
                                size=14, 
                                hjust=.5, 
                                family = "Gill Sans Mt"),
        plot.subtitle = element_text(size=12, family="Gill Sans Mt"),
        plot.caption=element_text(size=12, family="Gill Sans Mt"),
        axis.title=element_text(size=12, family="Gill Sans Mt"),
        axis.text=element_text(size=9, family="Gill Sans Mt"),
        legend.text=element_text(size=11, family="Gill Sans Mt"),
        strip.text=element_text(size=12, family="Gill Sans Mt"))


# USAID colors

usaid_blue <- "#002F6C"
usaid_red <- "#BA0C2F"
rich_black <- "#212721"
medium_blue <- "#0067B9"
light_blue <- "#A7C6ED"
web_blue <- "#205493"
dark_red <- "#651D32"
dark_grey <- "#6C6463"
medium_grey <- "#8C8985"
light_grey <- "#CFCDC9"

usaid_palette <- c(web_blue, usaid_red, light_blue, dark_red, usaid_blue)
usaid_palette

usaid_palette6 <- c(web_blue
                    , usaid_red
                    , light_blue
                    , dark_red
                    , usaid_blue
                    , medium_grey)
scale_colour_discrete <- function(...) scale_colour_manual(..., values = usaid_palette)

# palette(usaid_palette)
# 
#  data(mtcars)
# # head(mtcars)
#  str(mtcars)
# frq(mtcars$carb)
# # 
#  ggplot(mtcars, aes(mpg, hp, color=as.factor(carb))) + 
#    geom_point() 
#  
#  +
#    scale_color_brewer(palette="Set2")
# #   
#   
#   scale_color_discrete()
# 
# ?scale_color_discrete
#   
#   
# 
#   + 
#   scale_color_manual(values=usaid_palette)

options(ggplot2.discrete.color = usaid_palette)

# Zambia colors

zamGreen <- "#198a00ff"
zamRed <- "#de2010ff"
zamOrange <- "#EF7D00"
zamBlack <- "#000000"

#Here's a USAID color scheme to apply to scale_color_manual()
#of all the plotting functions. To use it replace the existing color scale with
#scale_color_manual(values = colors, labels=get_labels(variable))

# colors = c("#002F6C", "#BA0C2F", "#0067B9", "#6C6463", "#651D32", "#A7C6ED", "#8C8985")


high_blue <- "<span style='color:#205493;'>**declined**</span>"
  
  

# labels ---- 


qlabs <- c("2018-Q1",
           "2018-Q2",
           "2018-Q3",
           "2018-Q4",
           "2019-Q1",
           "2019-Q2",
           "2019-Q3",
           "2019-Q4",
           "2020-Q1",
           "2020-Q2",
           "2020-Q3",
           "2020-Q4",
           "2021-Q1",
           "2021-Q2",
           "2021-Q3",
           "2021-Q4",
           "2022-Q1")

#qlabs

qdate <- c("2018-03-01", 
           "2018-06-01",
           "2018-09-01",
           "2018-12-01",
           "2019-03-01",
           "2019-06-01",
           "2019-09-01",
           "2019-12-01",
           "2020-03-01",
           "2020-06-01",
           "2020-09-01",
           "2020-12-01",
           "2021-03-01",
           "2021-06-01",
           "2021-09-01",
           "2021-12-01",
           "2022-03-01")

qkey <- data.frame(q_num=1:17,
                   qlab=qlabs,
                   qdate=qdate,
                   year=c(rep(2018:2021, each=4), 2022))
#Malaria Campaigns
vline1 <- 2016
vline2 <- 2017
vline3 <- 2018
vline4 <- 2020

vline5 <- 2016
vline6 <- 2017-10-01
vline7 <- 2018-10-01
vline8 <- 2020-10-01


#qkey


# functions ---- 





