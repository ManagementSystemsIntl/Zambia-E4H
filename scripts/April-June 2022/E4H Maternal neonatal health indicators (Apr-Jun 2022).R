#'*Zambia E4 Health*
#'*Maternal neonatal health indicators*
#'*April - June 2022*


source("scripts/r prep2.r")

mat <- read_xls("data/Jan- Jun 2022/Reproductive Maternal Data_National Level(Monthly).xls")
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

matq <- read_xls("data/Jan- Jun 2022/Reproductive Maternal Data_National Level(Quarterly).xls")
mat_prov <- read_xls("data/Jan- Jun 2022/Reproductive Maternal Data_Provincial Level(Monthly).xls")
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



#'*Maternal ---- *

#'* 1. Client: ANC coverage ----*

mat <- mat %>%
  rename(ancc = 3,
         anc1 = 4,
         anc1u20 = 5,
         anc1hr = 12) %>%
  mutate(anccp = ancc/100,
         anc1p = anc1/100,
         anc1u20p = anc1u20/100,
         anc1hrp = anc1hr/100)

#'*set anccp to 100 for all values >100*
mat <- mat %>% 
  dplyr::mutate(ancc = ifelse(ancc > 100, 100, ancc)) %>% 
  dplyr::mutate(anccp = ancc/100)

#'*To create legend, gather method for including a legend --*

mat <- gather(mat, key = subpop , value = rate, c(anccp, anc1p,anc1u20p,anc1hrp))
mat$subpop <- factor(mat$subpop, levels = unique(mat$subpop)) # transform into factor
levels(mat$subpop)

view(mat)

ggplot(mat, aes(x = mnthyr, y = rate, group = subpop, colour = subpop)) +
  geom_point(alpha=.6, size=1.5) + 
  geom_smooth(method = loess, size = .8, se=FALSE) +
  scale_y_continuous(limits = c(0,1),
                     labels = percent,
                     breaks = c(.1,.2,.3,.4,.5,.6,.7,.8,.9, 1)) +
  scale_x_date(date_labels="%b %Y",date_breaks="4 months") +
  xlab("") + 
  ylab("") +
  ggtitle("Proportion of expected pregnancies receiving antenatal care (ANC), 2018-2022") +
  scale_color_manual(name ="",
                     values = usaid_palette,
                     labels = c("1st ANC, all TMs", "1st ANC at TM1", 
                                "1st ANC at TM1: Women <20 yrs", "ANC at TM1: High risk pregs")
  ) + base

ggsave("viz/Apr-Jun 2022/Reproductive Maternal & Neontal/Proportion of expected pregnancies receiving antenatal care ns.png",
       device="png",
       type="cairo",
       height = 5.5,
       width = 10)


#'* 2. ANC coverage all PROVINCES* ----
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
                          prov =="Luapula" |
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

# ggsave("viz/(1.1) ANC by province faceted.png",
#        device="png",
#        type="cairo",
#        height=4,
#        width=7)


#'*2.2 ANC coverage PROV SEPARATED BY USAID SUPPORTED AND NONE* ----

names(anc_prov)
table(anc_prov$ip, anc_prov$prov)
frq(anc_prov$ip) #sjmisc

#'*redraw for all USAID-funded provinces*

ipyes <- anc_prov %>%
  filter(ip =="ip")

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
  scale_color_manual(name= "", values = (usaid_palette)) + baseX

#'*redraw for all NON USAID-funded provinces*

#'*define non IP provinces*

ipno <- anc_prov %>%
  filter(ip =="non-ip")

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
  scale_color_manual(name= "", values = (usaid_palette), 
                     labels = c("High risk pregnancies", "All pregnancies", "Women <20 yrs")) + basey


ipprov
nonipprov

all_prov <- grid.arrange(ipprov, nonipprov, ncol = 2)


ggsave("viz/Apr-Jun 2022/Reproductive Maternal & Neontal/ANC by province faceted ns1.png",
       plot = all_prov,
       device="png",
       type="cairo",
       height=7,
       width=12)


#'* 3. ANC care (Folic acid + Fe) ----*

#'*Assign each value of folic = 100 if >100*

mat <- mat %>% 
  rename(folic = 6,
         fe = 7) %>% 
  dplyr::mutate(folic = ifelse(folic > 100, 100, folic)) %>% 
  dplyr::mutate(folicp = folic/100,
                fep = fe/100) 

str(mat) # 48 rows x 25 columns

#'*To create legend, gather method for including a legend --*

mat <- gather(mat, key = vit, value = rate, c(folicp, fep)) # 96 rows x 25

#'*Create mnthyr date var for y axis*

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
  #geom_point(alpha=.6, size=1) + 
  geom_smooth(method = loess, se= FALSE) +
  scale_y_continuous(limits = c(0,1),
                     labels = percent,
                     breaks = c(.1,.2,.3,.4,.5,.6,.7,.8,.9, 1))+
  scale_x_date(date_labels="%b %Y",date_breaks="4 months") +
  xlab("") + 
  ylab("Proportion of women \noffered iron or folic acid") +
  ggtitle("ANC quality: Folic Acid and Iron Supplementation \nduring ANC (2018-2022)") +
  scale_colour_manual(name = "",
                      labels = c("Iron","Folic Acid"),
                      values = c(usaid_red, usaid_blue)) + base

ggsave("viz/Apr-Jun 2022/Reproductive Maternal & Neontal/iron & folic acid supplements smoothline ns.png",
       device="png",
       type="cairo",
       height=7,
       width=12)


#'*4. Inst delivery coverage ----*

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
inst


sum(inst$month_chr!=inst$month) # expecting 0 if vars same

ggplot(inst, aes(x=mnthyr, y=instdelp, fill=year)) + 
  geom_bar(alpha=0.7, position = "dodge", stat="identity", color=light_grey) +
  geom_smooth(color= dark_grey, se=F, size=1.1, alpha=.8) +
  scale_y_continuous(limits=c(0,1),
                     labels = percent,
                     breaks = c(.1,.2,.3,.4,.5,.6,.7,.8,.9, 1)) +
  scale_fill_manual(values = c("#CFCDC9", "#A7C6ED","#002F6C","#000000","#BA0C2F")) +
  scale_x_date(date_labels="%b %Y",date_breaks="4 months") +
  labs(x="",
       y="",
       fill="", title="Proportion of expected deliveries occurring in health facilities increased \nfrom 70% to slight above 80% in 2018, then decreased back to below 70% by June 2022") +
  non_base

ggsave("viz/Apr-Jun 2022/Reproductive Maternal & Neontal/Expected deliveries ns.png",
       device="png",
       type="cairo",
       height=7,
       width=12)



#'*4.1 Inst delivery PROV SEPARATED BY USAID SUPPORTED AND NONE ----*
names(inst)

instd_prov <- mat_prov %>%
  rename(prov = 1,
         instdel = 8) %>%
  # mutate(prov = case_when(prov == 'Northwestern' ~ 'NW'))#
  select(prov, mnthyr, instdel) %>% 
  mutate(prov = factor(prov),
         ip = case_when(prov=="Northern" |
                          prov =="Central" |
                          prov =="Muchinga" |
                          prov == "Luapula" |
                          prov =="Southern" |
                          prov =="Eastern" ~ "ip",
                        TRUE ~ "non-ip"))


names(instd_prov)
table(instd_prov$ip, instd_prov$prov)
frq(instd_prov$ip) #sjmisc

# redraw inst deliveries for all USAID-funded provinces

ipyes <- instd_prov %>%
  filter(ip =="ip")

instd.ipvz <- ggplot(ipyes, aes(x = mnthyr, y = instdel, colour = ip)) +
  geom_bar(alpha=.5, size=.8, position = "dodge", stat = "identity") + 
  geom_smooth(color=usaid_blue,method = loess, size = .8, se=FALSE)  +
  facet_wrap(~prov) +
  faceted +
  scale_y_continuous(limits = c(0,100),
                     breaks = c(20, 40, 60, 80)) +
  xlab("") + 
  ylab("") +
  ggtitle("USAID-supported provinces") +
  scale_color_manual(name= "", values = (usaid_palette)) + baseX


# inst deliveries for non-usaid provinces 

ipno <- instd_prov %>%
  filter(ip =="non-ip")


instd.nonipvz <- ggplot(ipno, aes(x = mnthyr, y = instdel, colour = ip)) +
  geom_histogram(alpha=.3, size=.8, color=usaid_red, position = "dodge", stat = "identity", fill=usaid_red) + 
  geom_smooth(color=usaid_blue, method = loess, size = .8, se=FALSE)  +
  facet_wrap(~prov) +
  faceted +
  scale_y_continuous(limits = c(0,100),
                     breaks = c(20, 40, 60, 80)) +
  xlab("") + 
  ylab("") +
  ggtitle("Non USAID-supported provinces") +
  scale_color_manual(name= "", values = (usaid_palette)) + basey

instd.ipvz
instd.nonipvz
sprt <- grid.arrange(instd.ipvz, instd.nonipvz, ncol=2)

ggsave("viz/Apr-Jun 2022/Reproductive Maternal & Neontal/Expected deliveries by province bars faceted ns.png",
       plot=sprt,
       device="png",
       type="cairo",
       height=7,
       width=12)
