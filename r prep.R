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


