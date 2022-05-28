# Zambia 4 Health


# Prep ---- 

#setwd("C:/Users/yashin.lin/Dropbox/0 Current Work/R R for work/Zambia-E4H")

source("scripts/r prep.R")

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
         mnthyr = my(monyr)
         )

sum(mat$month_chr!=mat$month) # expecting 0 if vars same

matq <- read_xls("data/Jan-Mar 2022/Reproductive Maternal Data_National Level(Quarterly).xls")

mat_prov <- read_xls("data/Jan-Mar 2022/Reproductive Maternal Data_Provincial Level(Monthly).xls")
mat_prov  <- mat_prov  %>%
  mutate(
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
         mnthyr = my(monyr)
        )

ch_prov <- readxl::read_xls("data/Jan-Mar 2022/Child Health Data_Provincial Level(Quarterly).xls")

ch_prov  <- ch_prov  %>%
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

#* (1) ANC coverage ----

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
  geom_point(alpha=.5, size=1) + 
  geom_smooth(method = loess, size = .7, se=FALSE) +
  scale_y_continuous(limits = c(0,1),
                     labels = percent,
                     breaks = c(.1,.2,.3,.4,.5,.6,.7,.8,.9, 1)) +
  xlab("") + 
  ylab("") +
  ggtitle("Proportion of expected pregnancies receiving antenatal care at all trimesters \nand at 1st trimester (TM1), 2018-2022") +
  scale_color_manual(name ="",
                     values = usaid_palette,
                     labels = c("1st ANC, all trimestres", "1st ANC at TM1", "1st ANC at TM1: Women <20 yrs", "ANC at TM1: Hi risk pregnancy")
  ) +
    theme(plot.title = element_text(size = 14), 
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text = element_text(size = 9),
        legend.position = "bottom", 
        legend.text = element_text(size = 9)
  ) 

ggsave("viz/(1) Proportion of expected pregnancies receiving 1st ANC.png",
       device="png",
       type="cairo",
       height=4,
       width=8)

#* (1.1) Client: ANC coverage PROVINCIAL ----

names(mat_prov)

anc_prov <- mat_prov %>%
  rename(prov = 1,
         anc1 = 4,
         anc1u20 = 5,
         anc1hr = 12) %>%
  mutate(anc1p = anc1/100,
         anc1u20p = anc1u20/100,
         anc1hrp = anc1hr/100,
         ) %>% 
#  select(prov, mnthyr, anc1p, anc1hrp) %>% 
#  select(prov, mnthyr, anc1p, anc1u20p, anc1hrp) %>% 
   select(prov, mnthyr, anc1p, anc1hrp) %>% 
    mutate(prov = factor(prov),
         ip = case_when(prov=="Northern" |
                          prov =="Central" |
                          prov =="Muchinga" |
                          prov =="Southern" |
                          prov =="Eastern" ~ "ip",
                        TRUE ~ "non-ip"))

table(anc_prov$ip, anc_prov$prov)
frq(anc_prov$ip) #sjmisc

levels(anc_prov$prov)

anc_prov_l <- anc_prov %>% 
# gather(key = subpop , value = rate, c(anc1p, anc1hrp)) %>% 
  gather(key = subpop , value = rate, c(anc1p,anc1u20p,anc1hrp)) %>% 

  mutate(ip = factor(ip),
         subpop = factor(subpop))
  
levels(anc_prov_l$ip)
levels(anc_prov_l$subpop)

ggplot(anc_prov_l, aes(x = mnthyr, y = rate, group = subpop, colour = subpop)) +
# geom_point(alpha=.6, size=.6) + 
  geom_smooth(method = loess, size = .7, se=FALSE)  +
  facet_wrap(~prov) +
  faceted +
  scale_y_continuous(limits = c(0,1),
                     labels = percent,
                     breaks = c(.2,.4,.6,.8, 1)) +
  labs(x ="", y="", caption = "USAID supports Northern, Central, Luapula, Muchinga, Southern and Eastern") +
  ggtitle("Proportion of expected pregnancies receiving 1st ANC visit <br> at first trimester, 2018-2022") +
  scale_color_manual(name= "", values = (usaid_palette), labels = c("High risk pregnancies", "All", "Pregnancies of women <20 years")) +
  theme(# plot.title = element_text(size = 12), 
    plot.title=element_markdown(),
    plot.caption = element_text(size=10),
    axis.title.x = element_text(size = 10),
    axis.title.y = element_text(size = 10),
    axis.text.x = element_text(size = 7),
    axis.text.y = element_text(size = 7),
    legend.text = element_text(size = 10),
    legend.title=element_blank(),
    legend.position=c(.75,.15),
    strip.text=element_text(size=10, family="Gill Sans Mt"),
    )
 
ggsave("viz/(1.1) ANC by province faceted.png",
       device="png",
       type="cairo",
       height=4,
       width=7)

#* (1.2) ANC coverage PROV SEPARATED ----

names(anc_prov)
table(anc_prov$ip, anc_prov$prov)
frq(anc_prov$ip) #sjmisc

# redraw for all USAID-funded provinces

ipyes <- anc_prov %>%
  filter(ip=="ip")

levels(anc_prov$prov)

ipyes_l <- ipyes %>% 
  gather(key = subpop , value = rate, c(anc1p,anc1hrp)) %>% 
  mutate(ip = factor(ip),
         subpop = factor(subpop))

levels(ipyes_l$ip)
levels(ipyes_l$subpop)

ipprov <- ggplot(ipyes_l, aes(x = mnthyr, y = rate, group = subpop, colour = subpop)) +
  geom_smooth(method = loess, size = .7, se=FALSE)  +
  facet_wrap(~prov) +
  faceted +
  scale_y_continuous(limits = c(0,1),
                     labels = percent,
                     breaks = c(.2,.4,.6,.8, 1)) +
  labs(x ="", y="", caption = "") +
  ggtitle("USAID-supported provinces") +
  scale_color_manual(name= "", values = (usaid_palette)) +
  theme(# plot.title = element_text(size = 12), 
    plot.title=element_markdown(),
    plot.caption = element_text(size=10),
    axis.title.x = element_text(size = 10),
    axis.title.y = element_text(size = 10),
    axis.text.x = element_text(size = 7),
    axis.text.y = element_text(size = 7),
    legend.text = element_text(size = 10),
    legend.title=element_blank(),
    legend.position="none",
    strip.text=element_text(size=10, family="Gill Sans Mt"),
  )

# redraw for all NON USAID-funded provinces

# define non IP provinces

ipno <- anc_prov %>%
  filter(ip=="non-ip")

levels(ipno$prov)

ipno_l <- ipno %>% 
  gather(key = subpop , value = rate, c(anc1p, anc1hrp)) %>% 
  mutate(ip = factor(ip),
         subpop = factor(subpop))

levels(ipno_l$subpop)

nonipprov <- ggplot(ipno_l, aes(x = mnthyr, y = rate, group = subpop, colour = subpop)) +
  # geom_point(alpha=.6, size=.6) + 
  geom_smooth(method = loess, size = .7, se=FALSE)  +
  facet_wrap(~prov) +
  faceted +
  scale_y_continuous(limits = c(0,1),
                     labels = percent,
                     breaks = c(.2,.4,.6,.8, 1)) +
  labs(x ="", y="", caption = "") +
  ggtitle("Non USAID-supported provinces") +
  scale_color_manual(name= "", values = (usaid_palette), labels = c("High risk pregnancies", "All pregnancies", "Women <20 yrs")) +
  theme(# plot.title = element_text(size = 12), 
    plot.title=element_markdown(),
    plot.caption = element_text(size=10),
    axis.title.x = element_text(size = 10),
    axis.title.y = element_text(size = 10),
    axis.text.x = element_text(size = 7),
    axis.text.y = element_text(size = 7),
    legend.text = element_text(size = 8),
    legend.title=element_blank(),
    legend.position=c("bottom"),
    strip.text=element_text(size=10, family="Gill Sans Mt"),
  )


ipprov
nonipprov

ipprov + nonipprov + 
  plot_annotation(title="ANC coverage for high risk pregnancies has increased in last two years independently \nof USAID support. No other pattern apparent.")

ggsave("viz/(1.2) ANC by province faceted partitioned.png",
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
  geom_point(color= usaid_blue, alpha=.6, size=.7) + 
# geom_line(color= usaid_blue, alpha=.4) +
  geom_smooth(color= usaid_blue, se=F, size=1.1, alpha=.8) +
  scale_y_continuous(limits=c(0,1),
                     labels = percent,
                     breaks = c(.1,.2,.3,.4,.5,.6,.7,.8,.9, 1)) +
  labs(x="",
       y="",
       title="Proportion of expected deliveries occurring in health facilities increased \nfrom 70% to 80% in 2018, then decreased back to 70% by 2022") +
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

# (2.1) Client: Institutional delivery coverage bar chart ----

ggplot(inst, aes(x=mnthyr, y=instdelp)) + 
  geom_col(fill = usaid_red) + 
  geom_smooth(color= usaid_blue, se=F, size=1.1, alpha=.8) +
  scale_y_continuous(limits=c(0,1),
                     labels = percent,
                     breaks = c(.1,.2,.3,.4,.5,.6,.7,.8,.9, 1)) +
  labs(x="",
       y="",
       title="Proportion of expected deliveries occurring in health facilities increased \nfrom 70% to 80% in 2018, then decreased back to 70% by 2022") +
  theme(plot.title = element_text(size = 14), 
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text = element_text(size = 9),
        legend.title = element_text(size = 12), 
        legend.text = element_text(size = 11)
  )

ggsave("viz/(2.1) Inst deliveries bar chart.png",
       device="png",
       type="cairo",
       height=4,
       width=7)


# (2.2) Institutional delivery coverage PROVINCIAL ====

names(mat_prov)

instd_prov <- mat_prov %>%
  rename(prov = 1,
         instdel = 8) %>%
  select(prov, mnthyr, instdel) %>% 
  mutate(prov = factor(prov),
         ip = case_when(prov=="Northern" |
                          prov =="Central" |
                          prov =="Muchinga" |
                          prov =="Southern" |
                          prov =="Eastern" ~ "ip",
                        TRUE ~ "non-ip"))

levels(instd_prov$prov)

ggplot(instd_prov, aes(x = mnthyr, y = instdel, colour = ip)) +
  geom_point(alpha=.5, size=.8) + 
  geom_smooth(method = loess, size = .8, se=FALSE)  +
  facet_wrap(~prov) +
  faceted +
  scale_y_continuous(limits = c(0,100),
                     breaks = c(20, 40, 60, 80)) +
  xlab("") + 
  ylab("") +
  ggtitle("Proportion expected deliveries occurring in health facilities, 2018-2022") +
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

ggsave("viz/(2.1) Institutional deliveries by province faceted.png",
       device="png",
       type="cairo",
       height=4,
       width=7)

# (2.3) Inst delivery PROV SEPARATED ----

names(instd_prov)
table(instd_prov$ip, instd_prov$prov)
frq(instd_prov$ip) #sjmisc

# redraw for all USAID-funded provinces

ipyes <- instd_prov %>%
  filter(ip=="ip")

insdelyeschart

<- ggplot(ipyes, aes(x = mnthyr, y = instdel, colour = ip)) +
  geom_point(alpha=.5, size=.8) + 
  geom_smooth(method = loess, size = .8, se=FALSE)  +
  facet_wrap(~prov) +
  faceted +
  scale_y_continuous(limits = c(0,100),
                     breaks = c(20, 40, 60, 80)) +
  xlab("") + 
  ylab("") +
# ggtitle("Proportion expected deliveries occurring in health facilities, 2018-2022") +
  scale_color_manual(name= "", values = (usaid_palette)) +
  theme(# plot.title = element_text(size = 12), 
    plot.title=element_markdown(),
    axis.title.x = element_text(size = 10),
    axis.title.y = element_text(size = 10),
    axis.text.x = element_text(size = 7),
    axis.text.y = element_text(size = 7),
    legend.text = element_text(size = 10),
    legend.title=element_blank(),
    legend.position=c("none"))

ipno <- instd_prov %>%
  filter(ip=="non-ip")

ipnochart <- ggplot(ipno, aes(x = mnthyr, y = instdel, colour = ip)) +
  geom_point(alpha=.5, size=.8) + 
  geom_smooth(method = loess, size = .8, se=FALSE)  +
  facet_wrap(~prov) +
  faceted +
  scale_y_continuous(limits = c(0,100),
                     breaks = c(20, 40, 60, 80)) +
  xlab("") + 
  ylab("") +
  scale_color_manual(name= "", values = (usaid_palette)) +
  theme(# plot.title = element_text(size = 12), 
    plot.title=element_markdown(),
    axis.title.x = element_text(size = 10),
    axis.title.y = element_text(size = 10),
    axis.text.x = element_text(size = 7),
    axis.text.y = element_text(size = 7),
    legend.text = element_text(size = 10),
    legend.title=element_blank(),
    legend.position=c("none"))

ipyeschart
ipnochart

ipyeschart + ipnochart + 
  plot_annotation(title="ANC coverage for high risk pregnancies has increased in last two years independently \nof USAID support. No other pattern apparent.")

ggsave("viz/(1.2) ANC by province faceted partitioned.png",
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

pnc  <- mat  %>%
  rename(pnc = 11) %>%
  mutate(pncp = pnc/100)

pnc_vz <- ggplot(pnc, aes(mnthyr, pncp)) + 
  geom_point(color= usaid_blue, alpha=.6, size=.8) + 
# geom_line(color= usaid_blue, alpha=.4) +
  scale_y_continuous(limits = c(0,1),
                     labels = percent,
                     breaks = c(.1, .2, .3, .4, .5, .6, .7, .8, .9, 1)) +
  stat_smooth(color= usaid_blue, se=F, size=1.1, alpha=.8) +
  labs(x="",
       y="",
       title="The proportion of expected deliveries that receive postnatal care within 48 hrs \nafter delivery decreased in the last quarter, following a decreasing trend \nthat started in Aug 2021"
       ) +
  theme(plot.title = element_text(size = 14), 
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text = element_text(size = 9),
        legend.title = element_text(size = 12), 
        legend.text = element_text(size = 11)
        ) 

#yearn <- lapply(mat$year, as.numeric)

target2018 <- data.frame(x1= as.Date("2018-10-01"), x2=as.Date("2019-01-01"), y1=.74, y2=.74)
target2019 <- data.frame(x1=as.Date("2019-10-01"), x2=as.Date("2020-01-01"), y1=.79, y2=.79)
target2020 <- data.frame(x1=as.Date("2020-10-01"), x2=as.Date("2021-01-01"), y1=.85, y2=.85)
target2021 <- data.frame(x1=as.Date("2021-10-01"), x2=as.Date("2022-01-01"), y1=.81, y2=.81)
targets <- bind_rows(target2018, target2019, target2020, target2021)

#targets <- data.frame(x1=c("2018-12-01", "2019-12-01", .., x2 = .., x3=..)

pnc_vz + geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2), colour = usaid_red, size=.8, data = targets) +
 annotate(geom="text", x=as.Date("15-5-2018", format = "%d-%m-%Y"), y=.75, colour = usaid_red, label="National Targets", size= 3) +
 annotate(geom="text", x=as.Date("01-06-2021", format = "%d-%m-%Y"), y=.1, label="*home deliveries included", size =3, fontface = 'italic')

ggsave("viz/(3) Proportion of expected deliveries receiving PNC within 48 hrs.png",
       device="png",
       type="cairo",
       height=4,
       width=7)

#* (3.1) PNC PROVINCIAL FACETED

names(mat_prov)

pnc_prov <- mat_prov %>%
  rename(prov = 1,
         pncr  = 11) %>%
  select(prov, mnthyr, pncr) %>% 
  mutate(prov = factor(prov),
         ip = case_when(prov=="Northern" |
                          prov =="Central" |
                          prov =="Muchinga" |
                          prov =="Southern" |
                          prov =="Eastern" ~ "ip",
                        TRUE ~ "non-ip"))

levels(instd_prov$prov)

ggplot(pnc_prov, aes(x = mnthyr, y = pncr, colour = ip)) +
  geom_point(alpha=.5, size=.8) + 
  geom_smooth(method = loess, size = .7, se=FALSE)  +
  facet_wrap(~prov) +
  faceted +
  scale_y_continuous(limits = c(0,100),
                     breaks = c(20, 40, 60, 80)) +
  xlab("") + 
  ylab("") +
  ggtitle("Proportion of expected deliveries receiving postnatal care within 48 hrs increased,<br>both in provinces supported and not supported by USAID activities, 2018-2022") +
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

ggsave("viz/(3.1) PNC by province.png",
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

mm_vz <- ggplot(matmm, aes(x = mnthyr, y = deaths, group = mmtypef, colour = mmtypef)) + 
  geom_point(alpha=.6, size=.7) + 
# geom_line(size=.7) +
  geom_smooth(method = loess, size=.7, se=F) +
  scale_y_continuous(limits = c(0,260),
                     breaks = c(20,40,60,80,100,120,140,160,180,200, 220, 240),
                     labels = c("20","40","60","80","100","120","140","160","180","200", "220", "240")) +
  xlab("") +
  ylab("") +
  ggtitle("Maternal deaths occurring at health facilities and in the community, 2018-2022") +
  scale_colour_manual(name = "",
                    labels= c( "Maternal mortality facility ratio \n(per 10 000 live births)", "Health facility deaths", "Community deaths"),
                    values = c(usaid_blue, medium_grey, usaid_red)) +
  theme(plot.title = element_text(size = 14), 
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text = element_text(size = 9.5),
        legend.position = "bottom", 
        legend.text = element_text(size = 10))

# [scale_colour_manual]: How to assign correct colour to correct label: ----
# [general rule: r looks at data in  alphabetical order of variables or variable values
# [values: assigns colour to data [in alphabetical order of variable name
# [labels: assigns legend labels over data [by alphabetical order of var name

target2018 <- data.frame(x1= as.Date("2018-10-01"), x2=as.Date("2019-01-01"), y1 = 250, y2 = 250)
target2019 <- data.frame(x1=as.Date("2019-10-01"), x2=as.Date("2020-01-01"), y1 = 200, y2 = 200)
target2020 <- data.frame(x1=as.Date("2020-10-01"), x2=as.Date("2021-01-01"), y1 = 150, y2 = 150)
target2021 <- data.frame(x1=as.Date("2021-10-01"), x2=as.Date("2022-01-01"), y1 = 100, y2 = 100)
targets <- bind_rows(target2018, target2019, target2020, target2021)

#targets <- data.frame(x1=c("2018-12-01", "2019-12-01", .., x2 = .., x3=..)

mm_vz + geom_segment(aes(x = x1
                         , y = y1
                         , xend = x2
                         , yend = y2)
                     , colour = usaid_blue
                     , size=.8
                     , data = targets
                     , inherit.aes = FALSE) +
# geom_text(data = targets
#          , aes(x = x2
#                , y = y1
#                , label = paste0(y1)
#          , color = "maroon"
#          , vjust = -.4
#          , size = 3)
#          , inherit.aes = FALSE) #+ 
  annotate(geom="text", x=as.Date("15-5-2018", format = "%d-%m-%Y"), y=250, colour = usaid_blue, label="National Targets", size= 3)

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
         mmrate = 16,
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
#  geom_line(alpha=.4) +
  stat_smooth(se=F, size=.8, alpha=.8) +
  scale_y_continuous(limits=c(0,18)) +
  labs(x="",
       y="",
       title="The proportion of stillbirths that are macerated stillbirths has slightly increased \nin the last two years",
       caption= "Stillbirth rates expressed per 1000 total births") +
  scale_color_manual(name= "",values = usaid_palette, labels = c("Fresh stillbirth rate", "Stillbirth rate (MSB + FSB)", "Macerated stillbirth rate")) +
  theme(plot.title = element_text(size = 14), 
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text = element_text(size = 9),
        legend.title = element_text(size = 12), 
        legend.position = "bottom", 
        legend.text = element_text(size = 11),
        plot.caption = element_text(size=10)
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

glimpse(matnpr_l$npdrate)

ggplot(matnpr_l, aes(x= mnthyr, y= nprrate, group=drtype, colour=drtype)) + 
  geom_point(alpha=.6, size=.5) + 
  geom_smooth(method = "lm", alpha=.4, size = 1, se = F) +
# stat_smooth(color= usaid_blue, se=F, size=1.1, alpha=.8) +
  scale_y_continuous(limits=c(0,22)) +
  labs(x="",
       y="",
       title="The <span style='color:#205493;'>**neonatal**</span> mortality rate has slightly increased since 2018, and <br>the <span style='color:#BA0C2F;'>**perinatal**</span> mortality rate has decreased since 2020") +
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
      
ggsave("viz/(5) Neonatal and perinatal mortality rates.png",
       device="png",
       type="cairo",
       height=4,
       width=7)

# Analysis:
# In last quarter's graph, the presentation showed there was a 
# slight decrease in 2021--we will need to understand why current 
# graph shows increase instead

#* (5.2) Outcome: Neonatal mortality by PROVINCE FACETED

names(mat_prov)

nm_prov <- mat_prov %>%
  rename(prov = 1,
         nd  = 14) %>%
  select(prov, mnthyr, pncr) %>% 
  mutate(prov = factor(prov),
         ip = case_when(prov=="Northern" |
                          prov =="Central" |
                          prov =="Muchinga" |
                          prov =="Southern" |
                          prov =="Eastern" ~ "ip",
                        TRUE ~ "non-ip"))

levels(instd_prov$prov)

ggplot(pnc_prov, aes(x = mnthyr, y = pncr, colour = ip)) +
  geom_point(alpha=.5, size=.8) + 
  geom_smooth(method = loess, size = .7, se=FALSE)  +
  facet_wrap(~prov) +
  faceted +
  scale_y_continuous(limits = c(0,100),
                     breaks = c(20, 40, 60, 80)) +
  xlab("") + 
  ylab("") +
  ggtitle("Proportion of expected deliveries receiving postnatal care within 48 hrs increased,<br>both in provinces supported and not supported by USAID activities, 2018-2022") +
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

ggsave("viz/(3.1) PNC by province.png",
       device="png",
       type="cairo",
       height=4,
       width=7)

#* (20) BF within an hour CHILD FACETED ----

names(ch_prov)

bf1hr_prov <- ch_prov %>%
  rename(prov = 1,
         bf1 = 18) %>%
  select(prov, mnthyr, bf1) %>% 
  mutate(prov = factor(prov),
         ip = case_when(prov=="Northern" |
                          prov =="Central" |
                          prov =="Muchinga" |
                          prov =="Southern" |
                          prov =="Eastern" ~ "ip",
                        TRUE ~ "non-ip"))

levels(bf1hr_prov$prov)

ggplot(bf1hr_prov, aes(x = mnthyr, y = bf1, colour = ip)) +
  geom_point(alpha=.5, size=.8) + 
  geom_smooth(method = loess, size = .7, se=FALSE)  +
  facet_wrap(~prov) +
  faceted +
  scale_y_continuous(limits = c(0,100),
                     breaks = c(20, 40, 60, 80)) +
  xlab("") + 
  ylab("") +
  ggtitle("Proportion of neonates breastfed within 1 hour by province, 2018-2022") +
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

ggsave("viz/(10) Breastfed within 1hr by province.png",
       device="png",
       type="cairo",
       height=4,
       width=7)

#* <maternal postnatal care within 48 hrs other target method? ----

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


# (11)*FP: Number of clients accessing LARC----

source("scripts/r prep.R")

fam_mnth <- read_xls("data/Jan-Mar 2022/Family Planning Data_National Level(Monthly) updated 2022.05.16.xls")   %>%
  mutate(month_chr = str_sub(periodname,
                             start=1,
                             end=nchar(periodname)-5),
         month = factor(month_chr,
                        levels=c("January","February","March","April","May","June","July","August","September","October","November","December")),
         month_code = as.numeric(month), 
         year = str_sub(periodname, 
                        start=nchar(periodname)-4,
                        end=nchar(periodname)),
         year = as.numeric(year),
         monyr = paste(month_code, year, sep="-"),
         mnthyr = my(monyr)) %>%
  relocate(mnthyr, .after=periodname)

names(fam_mnth)

larc <- fam_mnth %>%
  rename(larc = 14) %>% 
  select(mnthyr, larc) 

frq(larc$larc)

ggplot(larc, aes(x=mnthyr, y=larc)) + 
  geom_point(color= usaid_blue, alpha=.6, size=1) + 
  geom_smooth(color= usaid_blue, se=F, size=1.1, alpha=.8) +
  scale_y_continuous(limits=c(0, 35000),
                     breaks = c(5000, 10000, 15000, 20000, 25000, 30000)) +
  labs(x="",
       y="", 
       title="Number of clients accessing LARCs (implants and IUDs) has also increased \nsince 2018, though variance has also increased in the last two years") +
  theme(plot.title = element_text(size = 14), 
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text = element_text(size = 9),
        legend.title = element_text(size = 12), 
        legend.text = element_text(size = 11)
  )

ggsave("viz/(11) LARC.png",
       device="png",
       type="cairo",
       height=4,
       width=7)

# (12)*FP: Percentage of clients discontinuing LARC----

names(fam_mnth)

<<<<<<< HEAD
larcoff <- fam_mnth %>%
  rename(larcoff = 15) %>% 
  select(mnthyr, larcoff) 
=======
fam_mnth <- fam_mnth %>%
  rename(iucd_remove = 9,
         implant_remove=10,
         iucd_insert=12,
         implant_insert=13,
         larcoff = 15) %>% 
#  select(mnthyr, larcoff) %>%
  mutate(discont_larc = (iucd_remove + implant_remove) / (iucd_insert + implant_insert)) %>%
  relocate(discont_larc, .after=implant_insert)
>>>>>>> 84907b4993b8e5ddf4bf2f855b27803ca584bed0

frq(larc$larcoff)
frq(fam_mnth$iucd_remove)
frq(fam_mnth$discont_larc)


# 

ggplot(fam_mnth, aes(x=mnthyr, y=larcoff)) + 
  geom_point(color= usaid_blue, alpha=.6, size=1) + 
  geom_smooth(color= usaid_blue, se=F, size=1.1, alpha=.8, method="lm") +
  scale_y_continuous(limits=c(0,1),
                     labels=percent) +
                     #breaks = c(5000, 10000, 15000, 20000, 25000, 30000)) +
  labs(x="",
       y="", 
       title="Number of clients accessing LARCs (implants\nand IUDs) has remained consistent since 2018") +
  theme(plot.title = element_text(size = 14), 
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text = element_text(size = 9),
        legend.title = element_text(size = 12), 
        legend.text = element_text(size = 11)
  )

ggsave("viz/Jan-Mar 2022/Family planning/(11) discontinue LARC.png",
       device="png",
       type="cairo",
       height=4,
       width=7)


