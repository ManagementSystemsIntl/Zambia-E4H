
# packages ---- 

# packages <- c("arm", "BMA", "brms", "corrplot", "dummies","DescTools", "estimatr","extrafont", "extrafontdb", "janitor",
#               "reshape2","tidyr","broom", "caret", "haven", "HH","Hmisc","lubridate","knitr", "margins", "magrittr", "plotrix",
#               "scales","survey", "srvyr", "sysfonts", "foreign","car", "ICC", "openxlsx", "ggrepel", "readr",
#               "readxl", "sjmisc", "sjPlot", "sjstats", "sjlabelled", "skimr","labelled", "texreg", "janitor","psych","dplyr",
#               "tidyverse", "viridis", "here", "ggridges", "ggthemes", "DT", "jtools", "huxtable", "stringi", "gghighlight",
#               "plm", "brms", "rstan", "rstanarm","tidybayes","texreg","gt","gtsummary","huxtable","stargazer", "gsynth",
#               "panelView", "assertr", "pointblank", "validate", "sandwich", "workflowr", "here", "missForest", "ltm")

zam_packages <- c("tidyverse", "here", "gt","gtsummary","gghighlight","stringi","DT", "ggthemes","ggridges","viridis",
              "psych","labelled","readxl", "skimr","sjlabelled","sjstats","sjPlot", "sjmisc","readr","ggrepel","openxlsx",
              "sysfonts","scales","magrittr","knitr","lubridate","Hmisc","haven","tidyr","reshape2","DescTools","extrafont",
              "extrafontdb")


# lapply(zam_packages, install.packages, character.only=T)

lapply(zam_packages, library, character.only=T)

# font_import()
# loadfonts(device="win")
# windwsFonts()


# formatting ---- 

font_add_google("Open Sans", "sans-serif")

options(digits=4, scipen=8)
#options(digits=8, scipen=9)

# set default
base <- theme_bw() + theme(panel.grid.minor.x=element_blank(),
                           panel.grid.minor.y=element_blank(),
                           plot.title=element_text(face="bold",size=16, hjust=.5, family = "Gill Sans Mt"),
                           plot.subtitle = element_text(size=16, family="Gill Sans Mt"),
                           plot.caption=element_text(size=12, family="Gill Sans Mt"),
                           axis.title=element_text(size=16, family="Gill Sans Mt"),
                           axis.text=element_text(size=14, family="Gill Sans Mt"),
                           legend.text=element_text(size=14, family="Gill Sans Mt"),
                           strip.text=element_text(size=14, family="Gill Sans Mt"),
                           panel.border=element_blank(),
                           axis.ticks = element_blank())

theme_set(base)

faceted <- theme_bw() +
  theme(panel.grid.minor.x=element_blank(),
        panel.grid.minor.y=element_blank(),
        plot.title=element_text(face="bold",size=18, hjust=.5, family = "Gill Sans Mt"),
        plot.subtitle = element_text(size=16, family="Gill Sans Mt"),
        plot.caption=element_text(size=12, family="Gill Sans Mt"),
        axis.title=element_text(size=16, family="Gill Sans Mt"),
        axis.text=element_text(size=14, family="Gill Sans Mt"),
        legend.text=element_text(size=14, family="Gill Sans Mt"),
        strip.text=element_text(size=14, family="Gill Sans Mt"))


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

#Here's a USAID color scheme to apply to scale_color_manual()
#of all the plotting functions. To use it replace the existing color scale with
#scale_color_manual(values = colors, labels=get_labels(variable))

# colors = c("#002F6C", "#BA0C2F", "#0067B9", "#6C6463", "#651D32", "#A7C6ED", "#8C8985")


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
           "2021-Q4")

#qlabs

qkey <- data.frame(q_num=1:16,
                   qlab=qlabs,
                   year=rep(2018:2021, each=4))

#qkey


# functions ---- 





