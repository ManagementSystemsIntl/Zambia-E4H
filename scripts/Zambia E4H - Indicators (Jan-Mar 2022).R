# Zambia E4 Health
# Indicators
# Jan - Mar 2022

# prep ---- 

source("scripts/r prep.R")

chld_mnth <- read_xls("data/Jan-Mar 2022/Child Health Data_National Level(Monthly).xls")

chld_mnth_labs <- data.frame(var_lab=names(chld_mnth))

chld <- read_xls("data/Jan-Mar 2022/Child Health Data_National Level(Quarterly).xls")

chld_labs <- data.frame(var_lab=names(chld))


chld_prov <- read_xls("data/Jan-Mar 2022/Child Health Data_Provincial Level(Quarterly).xls")

chld_prov_labs <- data.frame(var_lab=names(chld_prov))




  # mat



  # Family planning

fam_mnth <- read_xls("data/Jan-Mar 2022/Family Planning Data_National Level(Monthly).xls")   %>%
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

frq(fam_mnth$year)

fam_mnth_labs <- data.frame(var_lab=names(fam_mnth))
write_csv(fam_mnth_labs, "data/Jan-Mar 2022/fam_mnth_labs.csv")

fam <- read_xls("data/Jan-Mar 2022/Family Planning Data_National Level(Quarterly).xls") %>%
  mutate(qrtr = 1:17,
         qlab = qlabs,
         qdate=as.Date(qdate)) %>%
  relocate(c(qrtr, qlab, qdate), .after=periodname)

str(fam)

fam_labs <- data.frame(var_lab=names(fam))
write_csv(fam_labs, "data/Jan-Mar 2022/fam_labs.csv")


frq(fam_prov_mnth$province)

fam_prov_mnth <- read_xls("data/Jan-Mar 2022/Family Planning Data_Provincial Level(Monthly).xls") %>%
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
         mnthyr = my(monyr),
         sunta = case_when(province=="Central" |
                             province=="Copperbelt" |
                             province=="Luapula" |
                             province=="Northern" ~ "SUNTA",
         TRUE ~ "non-SUNTA")) %>%
  relocate(mnthyr, .after=periodname) 

fam_prov_mnth_labs <- data.frame(var_lab=names(fam_prov_mnth))
frq(fam_prov_mnth$sunta)


# Family Planning Ind 1: Women of reproductive age visited by CHA ---- 

fam <- fam %>%
  rename(cha_visit=6)

ggplot(fam, aes(x=qdate, y=cha_visit)) + 
  geom_vline(aes(xintercept=qdate[5]), size=1.2, color="darkgoldenrod2", alpha=.6) +
  geom_line(color=medium_blue, size=1, alpha=.6) + 
  geom_point(color=medium_blue, size=2.5) + 
#  stat_smooth(color=medium_blue) +
  scale_x_date(#limits=c(as.Date("2018-01-01"), as.Date("2021-12-31")),
               date_breaks="1 year",
               date_labels="%Y") +
  scale_y_continuous(labels=comma) +
  labs(x="",
       y="",
       title="CHA worker visits <span style='color:#205493;'>**declined**</span> in Jan-Mar 2022,\ncontinuing a trend beginning in Q3 2020") +
  annotate("text", x=as.Date("2018-12-01"), y=45000, label="SUNTA\nlaunch", color="grey60", size=4) +
  theme(plot.title.position="plot",
        plot.title=element_markdown())

ggsave("viz/Jan-Mar 2022/Family planning/FP ind1 Women of reproductive age visited by CHA (Jan-Mar 2022).png",
       device="png",
       type="cairo",
       height=4,
       width=7)

#"This is a test for <span style='color:#205493;'>blue</span> and a test for <span style='color:#BA0C2F;'>red
#</span>") +
  

  # by province

fam_prov_mnth <- fam_prov_mnth %>%
  rename(cha_visit=4)

ggplot(fam_prov_mnth, aes(mnthyr, cha_visit, color=sunta)) + 
  geom_point(size=.5, alpha=.5) + 
#  geom_line() +
  stat_smooth(se=F, size=.8, alpha=.6) +
  facet_wrap(~province, ncol=4) +
  faceted +
  scale_color_manual(values=c(usaid_red, medium_blue)) +
  scale_x_date(#limits=c(as.Date("2018-01-01"), as.Date("2021-12-31")),
    date_breaks="2 years",
    date_labels="%Y") +
  scale_y_continuous(labels=comma) +
  labs(x="",
       y="",
       title="CHA worker visits are <span style='color:#205493;'>**higher**</span> in SUNTA provinces, <br>trend in both program and non-program areas is <span style='color:#205493;'>**declining**</span>") +
  theme(plot.title.position="plot",
        plot.title=element_markdown(),
        legend.title=element_blank(),
        legend.position=c(.75,.15))

ggsave("viz/Jan-Mar 2022/Family planning/FP ind1 Women of reproductive age visited by CHA province (Jan-Mar 2022).png",
         device="png",
         type="cairo",
         height=5.5,
         width=7)


  # by program area

fam_prog <- fam_prov_mnth %>%
  group_by(mnthyr, sunta) %>%
  summarise(cha_visit=sum(cha_visit)) %>%
  mutate(cha_visit=ifelse(is.na(cha_visit), 0, cha_visit),
         qtr = as.yearqtr(mnthyr))

fam_prog

ggplot(fam_prog, aes(mnthyr, cha_visit, color=sunta)) + 
  geom_point() + 
#  geom_line() +
  stat_smooth(se=F) +
  scale_color_manual(values=c(usaid_red, medium_blue)) 

+ 
  theme(legend.title="none")

fam_prog_qtr <- fam_prog %>%
  group_by(qtr, sunta) %>%
  summarise(cha_visit=sum(cha_visit)) %>%
  as.data.frame()

fam_prog_qtr
str(fam_prog_qtr)


ggplot(fam_prog_qtr, aes(qtr, cha_visit, color=sunta)) + 
  geom_vline(aes(xintercept=qtr[9]), size=1, color="darkgoldenrod2", alpha=.8) + 
  stat_smooth(se=F, size=1.4) +
  scale_color_manual(values=c(usaid_red, medium_blue)) +
  scale_y_continuous(labels=comma) +
  theme(legend.title=element_blank(),
        #legend.position="bottom",
        legend.position=c(.7,.2)) +
  labs(x="",
       y="",
       title="Women of reproductive age visited\nby Community Health Association") +
  annotate("text", x=fam_prog_qtr$qtr[8], y=20000, label="SUNTA\nlaunch", color="grey60", size=4)

ggsave("viz/Jan-Mar 2022/Family planning/FP ind1 Women of reproductive age visited by CHA, by program area legend (Jan-Mar 2022).png",
       device="png",
       type="cairo",
       height=4,
       width=7)



?geom_labelsmooth

ggplot(fam_prog_qtr, aes(qtr, cha_visit, color=sunta)) + 
  geom_vline(aes(xintercept=qtr[9]), size=1, color="darkgoldenrod2", alpha=.8) + 
# geom_point() + 
#  geom_line() +
  stat_smooth(se=F, size=1) +
  geom_labelsmooth(aes(label=sunta), 
                   text_smoothing=20, 
                   method="loess",
                   hjust="ymax") +
  scale_color_manual(values=c(usaid_red, medium_blue)) +
  scale_y_continuous(labels=comma) +
  labs(x="",
       y="",
       title="Women of reproductive age visited by Community Health Assistant",
       caption="Provincial data, aggregated by quarter\nand to program or non-program areas") +
  annotate("text", x=fam_prog_qtr$qtr[8], y=20000, label="SUNTA\nlaunch", color="grey60", size=4) +
  theme(legend.position="none",
        plot.title.position="plot")


ggsave("viz/Jan-Mar 2022/Family planning/ FP ind1 Women of reproductive age visited by CHA, by program area textpath (Jan-Mar 2022).png",
       device="png",
       type="cairo",
       height=4,
       width=7)





names(fam_mnth)

fam_mnth <- fam_mnth %>%
  rename(cha_visit=4)


ggplot(fam_mnth, aes(x=mnthyr, y=cha_visit)) + 
#  geom_line(color=medium_blue, size=1, alpha=.6) + 
  geom_point(color=medium_blue, size=2.5) + 
  stat_smooth(color=medium_blue) +
  scale_x_date(limits=c(as.Date("2018-01-01"), as.Date("2021-12-31")),
               date_breaks="1 year",
               date_labels="%Y") +
  scale_y_continuous(labels=comma) +
  labs(x="",
       y="",
       title="Women of reproductive age visited\nby Community Health Association")

ggsave("viz/Women of reproductive age visited by CHA (Jan-Mar 2022).png",
       device="png",
       type="cairo",
       height=4,
       width=7)

?scale_x_date



#  stat_smooth(se=F)
  scale_x_continuous(breaks=1:16,
                     labels=qlabs)
  

# Family Planning Ind 2: Women of reproductive age use modern family planning ---- 

  
names(fam)  

fam <- fam %>%
    rename(cha_fp=7)

describe(fam$cha_fp)

ggplot(fam, aes(x=qdate, y=cha_fp)) + 
  geom_vline(aes(xintercept=qdate[5]), size=1.2, color="darkgoldenrod2", alpha=.6) +
  geom_line(color=web_blue, size=1, alpha=.6) + 
  geom_point(color=web_blue, size=2.5) + 
  #  stat_smooth(color=medium_blue) +
  scale_x_date(#limits=c(as.Date("2018-01-01"), as.Date("2021-12-31")),
    date_breaks="1 year",
    date_labels="%Y") +
  scale_y_continuous(labels=comma) +
  labs(x="",
       y="",
       title="Women adopting modern family planning method <span style='color:#205493;'>**declined**</span><br>in Jan-Mar 2022, continuing a trend starting in Q4 2020") +
  annotate("text", x=as.Date("2018-12-01"), y=25000, label="SUNTA\nlaunch", color="grey60", size=4) +
  theme(plot.title.position="plot",
        plot.title=element_markdown())

ggsave("viz/Jan-Mar 2022/Family planning/FP ind2 Women of reproductive age adopt modern family planning (Jan-Mar 2022).png",
       device="png",
       type="cairo",
       height=4,
       width=7)


  # by province

names(fam_prov_mnth)

frq(fam_prov_mnth$mnthyr)

fam_prov_mnth <- fam_prov_mnth %>%
  rename(cha_fp=5)

ggplot(fam_prov_mnth, aes(mnthyr, cha_fp, color=sunta)) + 
  geom_point(size=.5, alpha=.5) + 
  #geom_line() +
  stat_smooth(se=F, size=.8, alpha=.6) +
  facet_wrap(~province, ncol=4) +
  faceted +
  scale_color_manual(values=c(usaid_red, medium_blue)) +
  scale_x_date(#limits=c(as.Date("2018-01-01"), as.Date("2021-12-31")),
    date_breaks="2 years",
    date_labels="%Y") +
  scale_y_continuous(labels=comma) +
  labs(x="",
       y="",
       title="Women adopting modern family planning is <span style='color:#205493;'>**higher**</span> in SUNTA provinces<br>
       trend in both program and non-program areas is <span style='color:#205493;'>**declining**</span><br>",
       caption="Jan 2018-Mar 2022") +
  theme(plot.title.position="plot",
        plot.title=element_markdown(),
        legend.title=element_blank(),
        legend.position=c(.75,.15))

ggsave("viz/Jan-Mar 2022/Family planning/FP ind2 Women of reproductive adopt modern family planning province (Jan-Mar 2022).png",
       device="png",
       type="cairo",
       height=5.5,
       width=7)


# by program area

fam_prog <- fam_prov_mnth %>%
  group_by(mnthyr, sunta) %>%
  summarise(cha_fp=sum(cha_fp, na.rm=T)) %>%
  mutate(#cha_fp=ifelse(is.na(cha_fp), 0, cha_fp),
         qtr = as.yearqtr(mnthyr))

fam_prog

ggplot(fam_prog, aes(mnthyr, cha_fp, color=sunta)) + 
#  geom_point() + 
#  geom_line() +
  stat_smooth(se=F) +
  scale_color_manual(values=c(usaid_red, medium_blue)) 

+ 
  theme(legend.title="none")

fam_prog_qtr <- fam_prog %>%
  group_by(qtr, sunta) %>%
  summarise(cha_fp=sum(cha_fp)) %>%
  as.data.frame()

fam_prog_qtr
str(fam_prog_qtr)


ggplot(fam_prog_qtr, aes(qtr, cha_fp, color=sunta)) + 
  geom_vline(aes(xintercept=qtr[9]), size=1, color="darkgoldenrod2", alpha=.8) + 
  #geom_line() +
  stat_smooth(se=F, size=1.4) +
  scale_color_manual(values=c(usaid_red, medium_blue)) +
  scale_y_continuous(labels=comma) +
  theme(plot.title.position="plot",
        plot.title=element_markdown(),
        legend.title=element_blank(),
        #legend.position="bottom",
        legend.position=c(.7,.2)) +
  labs(x="",
       y="",
       title="CHA worker visits are <span style='color:#205493;'>**higher**</span> in SUNTA provinces, <br>trend in both program and non-program areas is <span style='color:#205493;'>**declining**</span>") +
  annotate("text", x=fam_prog_qtr$qtr[8], y=20000, label="SUNTA\nlaunch", color="grey60", size=4)

#"Women adopting modern family planning method\ndeclined in Jan-Mar 2022, continuing a trend starting in Q3 2020


ggsave("viz/Jan-Mar 2022/Family planning/FP ind2 Women adopting modern family planning method, by program area legend (Jan-Mar 2022).png",
       device="png",
       type="cairo",
       height=4,
       width=7)



ggplot(fam_prog_qtr, aes(qtr, cha_fp, color=sunta)) + 
  #geom_vline(aes(xintercept=qtr[9]), size=1, color=zamOrange, alpha=.8) + 
  # geom_point() + 
  #  geom_line() +
  stat_smooth(se=F, size=1) +
  geom_labelsmooth(aes(label=sunta), 
                   text_smoothing=20, 
                   method="loess",
                   hjust="ymax") +
  scale_color_manual(values=c(usaid_red, web_blue)) +
  scale_y_continuous(labels=comma) +
  labs(x="",
       y="",
       title="CHA worker visits are <span style='color:#205493;'>**higher**</span> in SUNTA provinces, <br>trend in both program and non-program areas is <span style='color:#205493;'>**declining**</span>",
       caption="Provincial data, aggregated by quarter\nand to program or non-program areas") +
#  annotate("text", x=fam_prog_qtr$qtr[8], y=20000, label="SUNTA\nlaunch", color="grey60", size=4) +
  theme(plot.title.position="plot",
        plot.title=element_markdown(),
        legend.position="none")

#"Women adopting modern family planning declined\nin Jan-Mar 2022, continuing a trend beginning in 2021"

ggsave("viz/Jan-Mar 2022/Family planning/FP ind2 Women adopting modern family planning, by program area textpath (Jan-Mar 2022).png",
       device="png",
       type="cairo",
       height=4,
       width=7)


# Family Planning Ind 3: Coverage of modern FP initiation among women visited by CHA ----  

names(fam_mnth)

fam_mnth <- fam_mnth %>%
  rename(cha_visit=4,
         cha_fp=5) %>%
  mutate(fp_prop = cha_fp/cha_visit) %>%
  relocate(fp_prop, .after=cha_fp)

fam_mnth[1:10,1:7]

ggplot(fam_mnth, aes(x=mnthyr, y=fp_prop)) +
#  geom_point(color=medium_blue) + 
#  geom_line(color=medium_blue) + 
  stat_smooth(color=medium_blue, se=F) +
  scale_x_date(#limits=c(as.Date("2018-01-01"), as.Date("2021-12-31")),
    date_breaks="1 year",
    date_labels="%Y") +
  scale_y_continuous(labels=) +
  labs(x="",
       y="",
       title="Women visited by CHA and adopting modern\nfamily planning method declined in Jan-Mar 2022") 


ggsave("viz/Jan-Mar 2022/Women visited by CHA and adopt modern family planning (Jan-Mar 2022).png",
       device="png",
       type="cairo",
       height=4,
       width=7)

# by quarter

names(fam)

fam <- fam %>%
  mutate(fp_prop = cha_fp/cha_visit) %>%
  relocate(fp_prop, .after=cha_fp)

fam[1:10,1:8]

a <- ggplot(fam, aes(qdate, fp_prop)) +
  geom_point(color="darkgoldenrod2", size=1.5) + 
  #geom_line(color=medium_blue, size=1) +
  stat_smooth(color="darkgoldenrod2", method="lm", size=.8, se=F) +
  #  geom_ribbon(aes(ymin=lower, ymax=upper), fill="lightgrey") +
  #  stat_smooth(aes(y=lower), linetype="dotdash") + 
  #  stat_smooth(aes(y=upper), linetype="dotdash") +
#  geom_label(aes(label=paste(round(fp_prop*100,1),"%", sep="")), color=medium_blue) + 
  scale_y_continuous(limits=c(.2,.8),
                     breaks=seq(.25, .75, .1),
                     labels=percent_format(accuracy=1)) +
  labs(x="",
       y="",
       title="Proportion has increased since 2018",
       caption="Sharpest increase in\nproportion occured in 2021") +
  theme(plot.title.position="plot",
        plot.title=element_markdown())

a

ggsave("viz/Jan-Mar 2022/Family planning/FP ind3 Proportion of women visited by CHA and adopt modern family planning by quarter (Jan-Mar 2022).png",
       device="png",
       type="cairo",
       height=4,
       width=7)

frq(fam$cha_visit)

b <- ggplot(fam, aes(qdate)) + 
  geom_point(aes(y=cha_visit), color=usaid_red) + 
  geom_line(aes(y=cha_visit), color=usaid_red) + 
  geom_point(aes(y=cha_fp), color=medium_blue) + 
  geom_line(aes(y=cha_fp), color=medium_blue) +
  scale_y_continuous(limits=c(0, 80315),
                     breaks=seq(0, 8e4, 2e4),
                     labels=comma) +
  labs(x="",
       y="",
       title="Women visited by CHA and adopting modern\nfamily planning method <span style='color:#205493;'>**declined**</span><br>in Jan-Mar 2022, continuing a trend starting in Q4 2020") +
  theme(plot.title.position="plot",
        plot.title=element_markdown())

b

ggsave("viz/Jan-Mar 2022/Family planning/FP ind3 Women visited by CHA and adopt modern family planning (Jan-Mar 2022).png",
       device="png",
       type="cairo",
       height=4,
       width=7)

b/a


ggsave("viz/Jan-Mar 2022/Family planning/FP ind3 Women visited and adopt, with proportion (Jan-Mar 2022).png",
       device="png",
       type="cairo",
       height=5.5,
       width=7)



# Family Planning Ind 4: First-time users of modern contraception ---- 

names(fam)  

fam <- fam %>%
    rename(contra=9) %>%
    mutate(contra_prop=contra/100) %>%
    relocate(contra_prop, .after=contra)

describe(fam$contra)

ggplot(fam, aes(x=qdate, y=contra_prop)) + 
    geom_vline(aes(xintercept=qdate[5]), size=1.2, color="darkgoldenrod2", alpha=.6) +
    geom_line(color=web_blue, size=1, alpha=.6) + 
    geom_point(color=web_blue, size=2, alpha=.9) + 
    stat_smooth(color=medium_blue, method="lm", se=F, linetype="dashed") +
    scale_x_date(#limits=c(as.Date("2018-01-01"), as.Date("2021-12-31")),
        date_breaks="1 year",
        date_labels="%Y") +
    scale_y_continuous(limits=c(0,.2),
                       labels=percent_format(accuracy=1)) +
    labs(x="",
         y="",
         title="First-time users of modern contraception <span style='color:#205493;'>**increased**</span> in Jan-Mar 2022,<br> reversing a declining trend the previous three quarters") +
    annotate("text", x=as.Date("2018-12-01"), y=.13, label="SUNTA\nlaunch", color="grey60", size=4) +
    theme(plot.title.position="plot",
          plot.title=element_markdown())

#<br>Long-term trend is steady increase

ggsave("viz/Jan-Mar 2022/Family planning/FP ind4 First-time users of modern contraception (Jan-Mar 2022).png",
       device="png",
       type="cairo",
       height=4,
       width=7)


# by province

names(fam_prov_mnth)

frq(fam_prov_mnth$mnthyr)

fam_prov_mnth <- fam_prov_mnth %>%
    rename(contra=6) %>%
    mutate(contra_prop=contra/100) %>%
    relocate(contra_prop, .after=contra)

ggplot(fam_prov_mnth, aes(mnthyr, contra_prop, color=sunta)) + 
    geom_point(size=.5, alpha=.5) + 
    #geom_line() +
    stat_smooth(se=F, size=.8, alpha=.6) +
    facet_wrap(~province, ncol=4) +
    faceted +
    scale_color_manual(values=c(usaid_red, medium_blue)) +
    scale_x_date(#limits=c(as.Date("2018-01-01"), as.Date("2021-12-31")),
        date_breaks="2 years",
        date_labels="%Y") +
    scale_y_continuous(labels=percent) +
    labs(x="",
         y="",
         title="First-time use of modern contraception is <span style='color:#205493;'>**higher**</span> in SUNTA provinces<br>Trend in Eastern province <span style='color:#205493;'>**similar**</span> to SUNTA provinces",
         caption="Jan 2018-Mar 2022") +
    theme(plot.title.position="plot",
          plot.title=element_markdown(),
          legend.title=element_blank(),
          legend.position=c(.75,.15))

ggsave("viz/Jan-Mar 2022/Family planning/FP ind4 First-time users of modern contraception by province (Jan-Mar 2022).png",
       device="png",
       type="cairo",
       height=5.5,
       width=7)


# Hormonal IUDs inserted ---- 

names(fam)

iud <- fam %>%
  select(1:4, 31:34) %>%
  na.omit() 

names(iud)[5:8] <- c("age2", "age3","age4","age1")

iud

iudL <- iud %>%
  pivot_longer(cols=5:8,
               names_to="age",
               values_to="num") %>%
  arrange(age) %>%
  mutate(age_lab = factor(age, labels=c("under 15", "15-19", "20-24","25+"))) 

iudL

ggplot(iudL, aes(qdate, num, color=age_lab)) + 
  geom_point(size=3) + 
  geom_line(size=1) +
  scale_y_continuous(labels=comma,
                     sec.axis=sec_axis(~.,
                              breaks=sec$num,
                              labels=sec$age_lab)) +
  theme(legend.position="bottom",
        legend.title=element_blank(),
        axis.text.y=element_markdown(),
        plot.title.position="plot",
        plot.title=element_markdown()) +
  labs(x="",
       y="",
       title="Hormonal IUD insertions <span style='color:#205493;'>**increased**</span> among females age 20+,<br> reversing a <span style='color:#BA0C2F;'>**declining**</span> trend since Q2 2021")

ggsave("viz/Jan-Mar 2022/Family planning/FP ind7 Hormonal IUDs inserted (Jan-Mar 2022).png",
       device="png",
       type="cairo",
       height=5.5,
       width=7)



usaid_red

?last

sec.axis=sec_axis(~.,
                  breaks=reg_tar$n,
                  labels=reg_tar$reg)



library(glue)

sec <- iudL %>%
  select(age_lab, qdate, num) %>%
  group_by(age_lab) %>%
  filter(num==last(num)) %>%
  ungroup() %>%
  mutate(color=usaid_palette[1:4],
         name = glue("<i style='color:{color}'>{age_lab}</i>"))



?glue
sec

usaid_palette[1:4]





















