# Zambia E4 Health
# Indicators
# Jan - Mar 2022

# prep ---- 

source("scripts/r prep.R")

ch <- read_xls("data/Downlaod Extract Childhealth Monthly At National.xls")

fam <- read_xls("data/Downlaod Extract Family Planning Monthly At National.xls")

fam_prov <- read_xls("data/Downlaod Extract Family Planning Yearly At Province.xls")

mat_prov <- read_xls("data/Downlaod Extract Maternal Yearly By Province.xls") 

mat <- read_xls("data/Downlaod Extract Maternal Monthly At National.xls")

  # Family planning

# Women of reproductive age visited by CHA ---- 

names(fam)

cha_ind <- read_xls("data/Processed data.xls",
                    sheet="Family Planning",
                    range="B4:Q8") %>%  
  t() %>%
  as.data.frame() %>%
  rownames_to_column("quarter") %>%
  rename(date=2, q_num=3, visited=4, useFP=5) %>%
  mutate(date=ymd(date),
         date2 = my(quarter),
         date3=date2+60,
         visited=as.numeric(visited),
         useFP=as.numeric(useFP),
         useProp=useFP/visited,
         q_num=as.numeric(q_num)) %>%
  left_join(qkey)

cha_ind
str(cha_ind)


ggplot(cha_ind, aes(x=date3, y=visited)) + 
  geom_line(color=medium_blue, size=1, alpha=.6) + 
  geom_point(color=medium_blue, size=2.5) + 
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


