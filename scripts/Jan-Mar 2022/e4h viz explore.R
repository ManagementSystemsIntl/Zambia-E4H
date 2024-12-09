# Zambia 4 Health

# Prep ---- 

setwd("C:/Users/yashin.lin/Dropbox/0 Current Work/R R for work/Zambia-E4H Git")

source("scripts/r prep.R")

#ggplot_shiny()

mat <- read_xls("data/Jan-Mar 2022/Reproductive Maternal Data_National Level(Monthly) updated.xls") 
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

sum(mat$month_chr!=mat$month) # expecting 0 if vars same

matq <- read_xls("data/Jan-Mar 2022/Reproductive Maternal Data_National Level(Quarterly).xls")
mat_prov <- read_xls("data/Jan-Mar 2022/Reproductive Maternal Data_Provincial Level(Monthly).xls")
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

# Maternal ---- 

#* (1) Client: ANC coverage ----

mat <- mat %>%
  rename(ancc = 3,
         anc1 = 4,
         anc1u20 = 5,
         anc1hr = 12) %>%
  mutate(anccp = ancc/100,
    anc1p = anc1/100,
    anc1u20p = anc1u20/100,
    anc1hrp = anc1hr/100)

# set anccp to 100 for all values >100
mat <- mat %>% 
  dplyr::mutate(ancc = ifelse(ancc > 100, 100, ancc)) %>% 
  dplyr::mutate(anccp = ancc/100)

# To create legend, gather method for including a legend --

mat <- gather(mat, key = subpop , value = rate, c(anccp, anc1p,anc1u20p,anc1hrp))
mat$subpop <- factor(mat$subpop, levels = unique(mat$subpop)) # transform into factor
levels(mat$subpop)

ggplot(mat, aes(x = mnthyr, y = rate, group = subpop, colour = subpop)) +
  geom_point(alpha=.6, size=1) + 
  geom_smooth(method = loess, size = .7, se=FALSE) +
  scale_y_continuous(limits = c(0,1),
                     labels = percent,
                     breaks = c(.1,.2,.3,.4,.5,.6,.7,.8,.9, 1)) +
  xlab("") + 
  ylab("") +
  ggtitle("Proportion of expected pregnancies receiving antenatal care (ANC), 2018-2022") +
  scale_color_manual(name ="",
                     values = usaid_palette,
                     labels = c("1st ANC, all TMs", "ANC at TM1", "ANC at TM1: Women <20 yrs", "ANC at TM1: High risk pregs")
  ) +
    theme(plot.title = element_text(size = 14), 
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text = element_text(size = 9),
        legend.position = "bottom", 
        legend.text = element_text(size = 9)
  ) 

ggsave("viz/(1) Proportion of expected pregnancies receiving ANC.png",
       device="png",
       type="cairo",
       height=4,
       width=7)

#* (1.1) Client: ANC coverage PROVINCIAL ----

names(mat_prov)

anc_prov <- mat_prov %>%
  rename(prov = 1,
         anc1 = 4,
#         anc1u20 = 5,
         anc1hr = 12) %>%
  mutate(anc1p = anc1/100,
#         anc1u20p = anc1u20/100,
         anc1hrp = anc1hr/100,
         ) %>% 
   select(prov, mnthyr, anc1p, anc1hrp) %>% 
#  select(prov, mnthyr, anc1p, anc1u20p, anc1hrp) %>% 
  mutate(prov = factor(prov),
         ip = case_when(prov=="Northern" |
                          prov =="Central" |
                          prov =="Muchinga" |
                          prov =="Southern" |
                          prov =="Eastern" ~ "ip",
                        TRUE ~ "non-ip"))

levels(anc_prov$prov)

anc_prov_l <- anc_prov %>% 
  gather(key = subpop , value = rate, c(anc1p, anc1hrp)) %>% 
# gather(key = subpop , value = rate, c(anc1p,anc1u20p,anc1hrp)) %>% 
  mutate(ip = factor(ip),
         subpop = factor(subpop))
  
levels(anc_prov_l$ip)
levels(anc_prov_l$subpop)

ggplot(anc_prov_l, aes(x = mnthyr, y = rate, group = subpop, colour = subpop)) +
#  geom_point(alpha=.6, size=.6) + 
  geom_smooth(method = loess, size = .7, se=FALSE)  +
  facet_wrap(~prov) +
  faceted +
  scale_y_continuous(limits = c(0,1),
                     labels = percent,
                     breaks = c(.2,.4,.6,.8, 1)) +
  labs(x ="", y="", caption = "Provinces supported by FHN, MOMENT and G2G:<br>	Northern, Central, Luapula, Muchinga Southern and Eastern") +
  ggtitle("Proportion of expected pregnancies receiving ANC at first trimester, 2018-2022") +
  scale_color_manual(name= "", values = (usaid_palette), labels = c("High risk pregnancies", "All", "Pregnancies of women <20 years")) +
  theme(# plot.title = element_text(size = 12), 
    plot.title=element_markdown(),
    axis.title.x = element_text(size = 10),
    axis.title.y = element_text(size = 10),
    axis.text.x = element_text(size = 7),
    axis.text.y = element_text(size = 7),
    legend.text = element_text(size = 10),
    legend.title=element_blank(),
    legend.position=c(.75,.15),
    strip.text=element_text(size=7, family="Gill Sans Mt"),
    )
 
ggsave("viz/(1.1) ANC by province faceted.png",
       device="png",
       type="cairo",
       height=4,
       width=7)


# -ANC care (Folic acid + Fe) ----

# Assign each value of folic = 100 if >100

mat <- mat %>% 
  rename(folic = 6,
         fe = 7) %>% 
  dplyr::mutate(folic = ifelse(folic > 100, 100, folic)) %>% 
  dplyr::mutate(folicp = folic/100,
                fep = fe/100) 

str(mat) # 48 rows x 25 columns

# To create legend, gather method for including a legend --

mat <- gather(mat, key = vit, value = rate, c(folicp, fep)) # 96 rows x 25

# Create mnthyr date var for y axis

mat <- mat %>%
  dplyr::mutate(month_chr = str_sub(periodname,
                             start=1,
                             end=nchar(periodname)-5
                             ),
         month = factor(month_chr,
                        levels=c("January","February","March","April","May","June","July","August","September","October","November","December")
                        ),
         month_code = as.numeric(month), 
         year = str_sub(periodname, 
                        start=nchar(periodname)-4,
                        end=nchar(periodname)
                        ),
         monyr = paste(month_code, year, sep="-"),
         mnthyr = my(monyr)
         )

suppressMessages(expr, classes = "message")

ggplot(mat, aes(x = mnthyr, y = rate, group = vit, colour = vit)) +
  geom_point(alpha=.6, size=1) + 
  geom_smooth(method = loess, se= FALSE) +
  scale_y_continuous(limits = c(0,1),
                     labels = percent,
                     breaks = c(.1,.2,.3,.4,.5,.6,.7,.8,.9, 1)) +
  xlab("") + 
  ylab("Proportion of women \noffered iron or folic acid") +
  ggtitle("ANC quality: Folic Acid and Iron Supplementation \nduring ANC (2018-2022)") +
  scale_colour_manual(name = "Supplement",
                      labels = c("Iron","Folic Acid"),
                      values = c(usaid_red, usaid_blue)) +
  theme(plot.title = element_text(size = 14), 
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text = element_text(size = 9),
        legend.title = element_text(size = 12), 
        legend.text = element_text(size = 11),
  ) 

#? cannot save viz

ggsave("viz/Folic Acid and Iron Supplementation during ANC.png",
       device="png",
       type="cairo",
       height=4,
       width=7)

#* (2) Client: Institutional delivery coverage ----

inst <- mat  %>%
  rename(instdel = 8) %>%
  mutate(instdelp = instdel/100,
         month_chr = str_sub(periodname,
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

sum(inst$month_chr!=inst$month) # expecting 0 if vars same

ggplot(inst, aes(x=mnthyr, y=instdelp)) + 
  geom_point(color= usaid_blue, alpha=.6, size=.8) + 
# geom_line(color= usaid_blue, alpha=.4) +
  geom_smooth(color= usaid_blue, se=F, size=1.1, alpha=.8) +
  scale_y_continuous(limits=c(0,1),
                     labels = percent,
                     breaks = c(.1,.2,.3,.4,.5,.6,.7,.8,.9, 1)) +
  labs(x="",
       y="",
       title="Proportion of expected deliveries occurring in health facilities") +
  theme(plot.title = element_text(size = 14), 
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text = element_text(size = 9),
        legend.title = element_text(size = 12), 
        legend.text = element_text(size = 11)
        )

ggsave("viz/(2) Prop of expected deliveries occuring in health facilities.png",
       device="png",
       type="cairo",
       height=4,
       width=7)

# -F/C: Cesarean rate ====

mat  <- mat  %>%
rename(cesar = 9) %>%
  mutate(cesarp = cesar/100,
         month_chr = str_sub(periodname,
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

sum(mat$month_chr!=mat$month) # expecting 0 if vars same

ggplot(mat, aes(mnthyr, cesarp)) + 
  geom_point(color= usaid_blue, alpha=.6, size=.8) + 
  geom_line(color= usaid_blue, alpha=.4) +
  scale_y_continuous(limits = c(0,.1),
                     labels = percent,
                     breaks = c(.01, .02, .03, .04, .05, .06, .07, .08, .09, .1)) +
  stat_smooth(method="lm", color= usaid_blue, se=F, size=1.1, alpha=.8) +
  labs(x="",
       y="",
       title="Proportion of expected pregnancies delivered through Cesarean Section") +
    theme(plot.title = element_text(size = 14), 
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text = element_text(size = 9),
        legend.title = element_text(size = 12), 
        legend.text = element_text(size = 11))

ggsave("viz/Cesarean section.png",
       device="png",
       type="cairo",
       height=4,
       width=7)

#* Outcome: Stillbirths

mat  <- mat  %>%
  rename(sbr = 15) %>%
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

sum(mat$month_chr!=mat$month) # expecting 0 if vars same

mat %>% 
ggplot(aes(x = mnthyr, y =sbr)) + 
  geom_point(color= usaid_blue, alpha=.6, size=.8) + 
  geom_line(color= usaid_blue, alpha=.4) +
  scale_y_continuous(limits = c(0,50),
                     breaks = c(10, 20, 30, 40, 50)) +
  stat_smooth(color= usaid_blue, se=FALSE, size=1.1, alpha=.8) +
  labs(x="",
       y="",
       title="Number of fetal deaths at health facilities \nout of 1000 expected births") +
  theme(plot.title = element_text(size = 14), 
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text = element_text(size = 9),
        legend.title = element_text(size = 12), 
        legend.text = element_text(size = 11))

ggsave("viz/Fetal deaths.png",
       device="png",
       type="cairo",
       height=4,
       width=7)

#* (3) Facility: Postnatal care ====

mat  <- mat  %>%
  rename(pnc = 11) %>%
  mutate(pncp = pnc/100)

mat <- ggplot(mat, aes(mnthyr, pncp)) + 
  geom_point(color= usaid_blue, alpha=.6, size=.8) + 
  geom_line(color= usaid_blue, alpha=.4) +
  scale_y_continuous(limits = c(0,1),
                     labels = percent,
                     breaks = c(.1, .2, .3, .4, .5, .6, .7, .8, .9, 1)) +
  stat_smooth(color= usaid_blue, se=F, size=1.1, alpha=.8) +
  labs(x="",
       y="",
       title="Proportion of expected deliveries \nthat receive postnatal care within 48 hours after delivery has doubled since 2018*"
       ) +
  theme(plot.title = element_text(size = 14), 
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text = element_text(size = 9),
        legend.title = element_text(size = 12), 
        legend.text = element_text(size = 11)
        )

mat + annotate(geom="text", x=as.Date("01-06-2021", format = "%d-%m-%Y"), y=.1, label="*home deliveries included", size =4, fontface = 'italic')

ggsave("viz/(3) Proportion of expected deliveries receiving PNC within 48 hrs.png",
       device="png",
       type="cairo",
       height=4,
       width=7)

#* (4) Outcome: Maternal deaths ----
 
matmm <- mat %>%
  rename(mmfr = 16,
         mmf = 13,
         mmc = 22
         ) 

matmm <- gather(matmm, key = mmtype , value = deaths, c(mmfr, mmf, mmc)) 

matmm$mmtypef <- factor(matmm$mmtype, levels = unique(matmm$mmtype))
levels(matmm$mmtypef)

ggplot(matmm, aes(x = mnthyr, y = deaths, group = mmtypef, colour = mmtypef)) + 
  geom_point(alpha=.6, size=1) + 
  geom_line(size=.7) +
  scale_y_continuous(limits = c(0,200),
                     breaks = c(20,40,60,80,100,120,140,160,180,200),
                     labels = c("20","40","60","80","100","120","140","160","180","200")) +
  xlab("") +
  ylab("Number of maternal deaths") +
  ggtitle("Maternal deaths occurring at health facilities \nand in the community, 2018-2022") +
  scale_colour_manual(name = "",
                    labels= c( "Maternal mortality facility ratio \n(per 10 000 live births)", "Health facility deaths", "Community deaths"),
                    values = c(usaid_blue, medium_grey, usaid_red)) +

# [scale_colour_manual]: How to assign correct colour to correct label: ----
# [general rule: r looks at data in  alphabetical order of variables or variable values
# [values: assigns colour to data [in alphabetical order of variable name
# [labels: assigns legend labels over data [by alphabetical order of var name
  
  theme(plot.title = element_text(size = 14), 
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text = element_text(size = 9.5),
        legend.position = "bottom", 
        legend.text = element_text(size = 10))
        
ggsave("viz/(4) Maternal deaths.png",
       device="png",
       type="cairo",
       height=4,
       width=7)

#* Maternal Provincial ---- 

#* (4.1) Outcome: Maternal mortality PROVINCIAL ----

names(mat_prov)

matd_prov <- mat_prov %>%
  select(1, 16, mnthyr) %>% 
  rename(prov = 1,
         mmrate = 2
         ) %>% 
  mutate(prov = factor(prov),
         ip = case_when(prov=="Northern" |
                          prov =="Central" |
                          prov =="Muchinga" |
                          prov =="Southern" |
                          prov =="Eastern" ~ "ip",
                          TRUE ~ "non-ip"))

levels(matd_prov$ip)

ggplot(matd_prov, aes(x = mnthyr, y = mmrate, colour = ip)) +
  geom_point(alpha=.6, size=1) + 
  geom_smooth(method = loess, size = .7, se=FALSE)  +
  facet_wrap(~prov) +
  faceted +
  scale_y_continuous(limits = c(0,500),
                     breaks = c(100, 200, 300, 400)) +
  xlab("") + 
  ylab("") +
  ggtitle("Facility maternal mortality ratio, 2018-2022") +
  scale_color_manual(name= "", values = (usaid_palette), labels = c("Supported by FHN, MOMENT and G2G", "Not supported")) +
  theme(# plot.title = element_text(size = 12), 
        plot.title=element_markdown(),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        axis.text.x = element_text(size = 7),
        axis.text.y = element_text(size = 7),
        legend.text = element_text(size = 10),
        legend.title=element_blank(),
        legend.position=c(.75,.15))
  ) 

ggsave("viz/(7) Maternal mortality ratio by province.png",
       device="png",
       type="cairo",
       height=4,
       width=7)

# Child Health ---- 

names(ch)

#* (6) Outcome: Stillbirths  ---- 

names(mat)

matsb <- mat  %>%
  rename( sbh = 15,
          sbm = 19,
          sbf = 20
          ) %>% 
  select(1, mnthyr, sbh, sbm, sbf) 

# [pivot_longer syntax] ----

matsb_l <- matsb %>% 
  pivot_longer(c(sbh, sbm, sbf), names_to= "sbtype", values_to = "sbrate") %>% 
  mutate(sbtype = factor(sbtype))
levels(matsb_l$sbtype)

# ancv <- ggplot(mat, aes(x = mnthyr, y = rate, group = subpop, colour = subpop)) +

ggplot(matsb_l, aes(x = mnthyr, y = sbrate, group = sbtype, colour = sbtype)) +
  geom_point(alpha=.6, size=.8) + 
  geom_line(alpha=.4) +
  stat_smooth(se=F, size=.8, alpha=.8) +
  scale_y_continuous(limits=c(0,18)) +
  labs(x="",
       y="",
       title="The proportion of stillbirths  that are macerated stillbirths has slightly increased \nin the last two years",
       caption= "Stillbirth rates expressed per 1000 total births") +
  scale_color_manual(name= "",values = usaid_palette, labels = c("Fresh stillbirth rate", "Stillbirth rate (MSB + FSB)", "Macerated stillbirth rate")) +
  theme(plot.title = element_text(size = 14), 
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text = element_text(size = 9),
        legend.title = element_text(size = 12), 
        legend.position = "bottom", 
        legend.text = element_text(size = 11)
        ) 

ggsave("viz/(6) Stillbirths.png",
       device="png",
       type="cairo",
       height=4,
       width=7)

#* (5.1) Outcome: Neonatal/perinatal deaths ----

names(mat)

matnp <- mat %>%
  rename(neod = 14,
         perid = 26) %>% 
  select(mnthyr, neod, perid) 

matnp_l <- matnp %>% 
  pivot_longer(c(neod, perid), names_to= "npdtype", values_to = "npdrate") %>% 
  mutate(npdtype = factor(npdtype))
  levels(matnp_l$npdtype)

#  scale_y_continuous(limits = c(0,1),
#                     labels = percent,
#                     breaks = c(.1,.2,.3,.4,.5,.6,.7,.8,.9, 1)) +

neoperid_vz <- ggplot(matnp_l, aes(x=mnthyr, y=npdrate, group = npdtype, colour = npdtype)) + 
  geom_point(alpha=.6, size= 1.2) + 
  geom_line(alpha=.6, size = .6) +
  scale_y_continuous(limits = c(0,1000)) +
  labs(x="",
       y="",
       title="Neonatal and perinatal deaths") +
  scale_color_manual(name= "", values = (usaid_palette), labels = c("Neonatal", "Perinatal")) +
  theme(plot.title = element_text(size = 14), 
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text = element_text(size = 9),
        legend.title = element_text(size = 12), 
        legend.position = "bottom", 
        legend.text = element_text(size = 11)
  ) 

ggsave("viz/(5) Neonatal and perinatal deaths.png",
       device="png",
       type="cairo",
       height=4,
       width=7)


#*(5) Outcome: Neonatal rates ----

names(mat)

matnpr <- mat %>%
  rename(neod = 14,
         peridr = 29,
         lb = 28) %>% 
  select(mnthyr, neod, peridr, lb) %>% 
  mutate(neodr = neod/lb * 1000)

matnpr_l <- matnpr %>% 
  pivot_longer(c(neodr, peridr), names_to= "drtype", values_to = "nprrate") %>% 
  mutate(drtype = factor(drtype))

levels(matnpr_l$drtype)

glimpse(matnp_l$npdrate)

ggplot(matnpr_l, aes(x= mnthyr, y= nprrate, group=drtype, colour=drtype)) + 
  geom_point(alpha=.6, size=2) + 
  geom_smooth(method = "lm", alpha=.4, size = 1, se = F) +
#  stat_smooth(color= usaid_blue, se=F, size=1.1, alpha=.8) +
  scale_y_continuous(limits=c(0,22)) +
  labs(x="",
       y="",
       title="<span style='color:#205493;'>**Neonatal**</span> and <span style='color:#BA0C2F;'>**perinatal**</span> mortality rates") +
  scale_colour_manual(name = "",
                      values = c(usaid_blue, usaid_red),
                      labels= c("Neonatal", "Perinatal")) +
  theme(#plot.title = element_text(size = 14), 
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 9),
      legend.title = element_text(size = 12), 
      legend.text = element_text(size = 11),
      legend.position = "bottom",
      plot.title.position="plot",
      plot.title=element_markdown()
      )
      
title="CHA worker visits <span style='color:#205493;'>**declined**</span> in Jan-Mar 2022,\ncontinuing a trend beginning in Q3 2020") +
  annotate("text", x=as.Date("2018-12-01"), y=45000, label="SUNTA\nlaunch", color="grey60", size=4) +
  theme(plot.title.position="plot",
        plot.title=element_markdown())


ggsave("viz/Neonatal and perinatal mortality rates.png",
       device="png",
       type="cairo",
       height=4,
       width=7)

# Analysis:
# In last quarter's graph, the presentation showed there was a 
# slight decrease in 2021--we will need to understand why current 
# graph shows increase instead

#* maternal postnatal care within 48 hrs

postnatal_target <- read_xls("data/Processed data.xls",
                             sheet="Maternal",
                             range="A64:E66") %>%
  pivot_longer(2:5,
               names_to="year") %>%
  rename(type=1) %>%
  mutate(year=as.numeric(year),
         value=as.numeric(value),
         mnthyr=ymd(paste(year, "-12-01")))


ggplot(ch, aes(mnthyr, postnatal_care2)) + 
  geom_point(color="dodgerblue", alpha=.6) + 
  geom_line(color="dodgerblue", alpha=.4) +
  stat_smooth(method="lm", color="dodgerblue", se=F, size=1.2, alpha=.8) +
  geom_line(data=filter(postnatal_target, type=="Target"), aes(x=mnthyr, y=value), color="maroon") +
  geom_label(aes(x=mnthyr, y=value, label=paste(value*100, "%", sep="")), color="maroon", data=filter(postnatal_target, type=="Target")) +
  scale_y_continuous(limits=c(0,1),
                     labels=percent_format(accuracy=1)) +
  labs(x="",
       y="",
       title="Maternal postnatal care within 48 hours",
       caption="Annual targets in red")

ggsave("viz/maternal postnatal care overally month with targets.png",
       device="png",
       type="cairo",
       height=4,
       width=7)





