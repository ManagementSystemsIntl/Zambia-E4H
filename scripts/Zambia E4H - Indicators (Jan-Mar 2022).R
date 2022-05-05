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
         monyr = paste(month_code, year, sep="-"),
         mnthyr = my(monyr)) %>%
  relocate(mnthyr, .after=periodname)


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
  geom_vline(aes(xintercept=qdate[5]), size=1, color="darkgoldenrod2", alpha=.8) +
  geom_line(color=medium_blue, size=1, alpha=.6) + 
  geom_point(color=medium_blue, size=2.5) + 
#  stat_smooth(color=medium_blue) +
  scale_x_date(#limits=c(as.Date("2018-01-01"), as.Date("2021-12-31")),
               date_breaks="1 year",
               date_labels="%Y") +
  scale_y_continuous(labels=comma) +
  labs(x="",
       y="",
       title="Women of reproductive age visited\nby Community Health Association") +
  annotate("text", x=as.Date("2018-12-01"), y=45000, label="SUNTA\nlaunch", color="grey60", size=4)

ggsave("viz/Jan-Mar 2022/Women of reproductive age visited by CHA (Jan-Mar 2022).png",
       device="png",
       type="cairo",
       height=4,
       width=7)


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
       title="Women of reproductive age visited\nby Community Health Association") +
  theme(legend.title=element_blank(),
        legend.position=c(.75,.15))

ggsave("viz/Jan-Mar 2022/Women of reproductive age visited by CHA province (Jan-Mar 2022).png",
         device="png",
         type="cairo",
         height=5.5,
         width=7)


  # by program area

library(zoo)

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

ggsave("viz/Jan-Mar 2022/Women of reproductive age visited by CHA, by program area legend (Jan-Mar 2022).png",
       device="png",
       type="cairo",
       height=4,
       width=7)



library(geomtextpath)

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
       title="Women of reproductive age visited\nby Community Health Association",
       caption="Provincial data, aggregated by quarter\nand to program or non-program areas") +
  annotate("text", x=fam_prog_qtr$qtr[8], y=20000, label="SUNTA\nlaunch", color="grey60", size=4) +
  theme(legend.position="none")


ggsave("viz/Jan-Mar 2022/Women of reproductive age visited by CHA, by program area textpath (Jan-Mar 2022).png",
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
  
    ### Family planning ### 


# Women of reproductive age use modern family planning ---- 
  
modern_fp <- read_xls("data/Processed data.xls",
                      sheet="Family Planning",
                      range="B4:Q8") %>%
    slice(-3) %>%
    t() %>%
    as.data.frame() %>%
    rownames_to_column("quarter") %>%
    rename(date=2, q_num=3, value=4) %>%
    mutate(date=ymd(date),
           date2 = my(quarter),
           date3=date2+60,
           value=as.numeric(value),
           q_num=as.numeric(q_num)) %>%
    left_join(qkey)
  
modern_fp

str(cha_ind)  


ggplot(cha_ind, aes(x=date3, y=useFP)) + 
  geom_line(color=medium_blue, size=1, alpha=.6) + 
  geom_point(color=medium_blue, size=2.5) + 
  scale_x_date(limits=c(as.Date("2018-01-01"), as.Date("2021-12-31")),
               date_breaks="1 year",
               date_labels="%Y") +
  scale_y_continuous(labels=comma) +
  labs(x="",
       y="",
       title="Women of reproductive age use\na modern family planning method")

ggsave("viz/Women of reproductive age use modern family planning (Jan-Mar 2022).png",
       device="png",
       type="cairo",
       height=4,
       width=7)


# Coverage of modern FP initiation for reproductive age women visited by CHA ----  

b <- ggplot(cha_ind, aes(x=date3, y=useProp)) + 
#  geom_line(color=medium_blue, size=1, alpha=.6) + 
  geom_point(color="darkgoldenrod2", size=2.5) + 
  stat_smooth(color="darkgoldenrod2", se=T, size=1.3) +
  scale_x_date(limits=c(as.Date("2018-01-01"), as.Date("2021-12-31")),
               date_breaks="1 year",
               date_labels="%Y") +
  scale_y_continuous(limits=c(.3,.7),
                     labels=percent_format(accuracy=1)) +
  labs(x="",
       y="",
       title="Proportion of women visited by CHA\nwho use a modern family planning method")

b

ggsave("viz/Ratio of women visited by CHA and use modern family planning method (Jan-Mar 2022).png",
       device="png",
       type="cairo",
       height=4,
       width=7)


a <- ggplot(cha_ind, aes(x=date3)) + 
  geom_line(aes(y=visited), color=medium_blue, size=1, alpha=.6) + 
  geom_point(aes(y=visited), color=medium_blue, size=2.5) + 
  geom_line(aes(y=useFP), color=usaid_red, size=1, alpha=.6) + 
  geom_point(aes(y=useFP), color=usaid_red, size=2.5) + 
  scale_x_date(limits=c(as.Date("2018-01-01"), as.Date("2021-12-31")),
               date_breaks="1 year",
               date_labels="%Y") +
  scale_y_continuous(labels=comma) +
  labs(x="",
       y="",
       title="Visited by CHA (blue)\nUse a modern family planning method (red)")

a

ggsave("viz/Visited by CHA and use modern family planning method (Jan-Mar 2022).png",
       device="png",
       type="cairo",
       height=4,
       width=7)

library(patchwork)
d <- a / b

d

ggsave("viz/Visited by CHA, use modern FP, ratio (Jan-Mar 2022).png",
       device="png",
       type="cairo",
       height=6,
       width=7)


# Proportion of new FP acceptors ----

names(fam)


