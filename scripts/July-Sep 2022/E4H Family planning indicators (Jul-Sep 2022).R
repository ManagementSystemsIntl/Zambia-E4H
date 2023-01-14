# Zambia E4 Health
# Family planning indicators
# July - September 2022

# prep ---- 

source("scripts/r prep.r")

fp <- read_excel("data/July-Sep 2022/Family planning/Family Planning (July 2018 - Sep 2022 quarterly).xlsx",
                 sheet="export",
                 range="A2:AY21") %>%
  mutate(yrqtr = as.yearqtr(date),
         yrqtr = yearquarter(date, fiscal_start=10),
         qtr = round(quarter(date, with_year = T),2),
         year=year(date),
         fisc=fiscal_year(yrqtr))

?ymd

names(fp)
str(fp)

frq(fp$qtr)
frq(fp$fisc)

?fiscal_year

fp[,4:51] <- map(fp[,4:51], as.numeric)

varlabs <- read_xlsx("data/July-Sep 2022/Family Planning (July 2018 - Sep 2022 quarterly).xlsx",
                     sheet="export",
                     range='A1:AY1',
                     col_names = F) %>%
  t()

varlabs


fpDict <- data.frame(var=names(fp),
                      varlab = c(varlabs, "Date", "Quarter","Year", "Fiscal year")) %>%
  mutate(class=lapply(fp, class)) %>%
  remove_rownames(.)



# Women visited by CHA (visited) ---- 

describe(fp$visited)
frq(fp$fisc)

ggplot(fp, aes(yrqtr, visited)) + 
  geom_point(color=usaid_blue, size=2) + 
  geom_line(color=usaid_blue, size=.4) +
  stat_smooth(data=filter(fp, fisc==2022), 
              aes(y=visited), 
              method="lm", se=F, color=usaid_blue, alpha=.4) +
  scale_x_yearquarter(date_labels="%Y") +
  scale_y_continuous(limits=c(6273,8.0315e4),
                     breaks=seq(0,8e4, 1e4),
                     labels=comma,
                     sec.axis = dup_axis()) +
  labs(x="\nJan 2018 - Sep 2022",
       y="CHA\nvisits\n",
       title="FY2022 on an increasing trend") +
  theme(axis.title.y.left=element_text(angle=0, vjust=.5),
        axis.title.y.right=element_blank(),
        axis.text.y.left=element_blank())


ggsave("viz/Jul-Sep 2022/Family planning/CHA visits (Jan 2018 - Sep 2022).png",
       height=5.3,
       width=7.1)

# Women on modern family planning method (fp) ---- 

describe(fp$fp)
frq(fp$fisc)

ggplot(fp, aes(yrqtr, fp)) + 
  geom_point(color=usaid_blue, size=2) + 
  geom_line(color=usaid_blue, size=.4) +
  stat_smooth(data=filter(fp, fisc==2022), 
              aes(y=fp), 
              method="lm", se=F, color=usaid_blue, alpha=.4) +
  scale_x_yearquarter(date_labels="%Y") +
  scale_y_continuous(limits=c(2862,4.2e4),
                     breaks=seq(0,4.2e4, .5e4),
                     labels=comma,
                     sec.axis = dup_axis()) +
  labs(x="\nJan 2018 - Sep 2022",
       y="Modern\nfamily\nplanning\nmethod",
       title="FY2022 on a flat/decreasing trend") +
  theme(axis.title.y.left=element_text(angle=0, vjust=.5),
        axis.title.y.right=element_blank(),
        axis.text.y.left=element_blank())


ggsave("viz/Jul-Sep 2022/Family planning/Women on modern family planning method (Jan 2018 - Sep 2022).png",
       height=5.3,
       width=7.1)


# Clients accessing contraceptives (contra) ---- 

describe(fp$contra)

ggplot(fp, aes(yrqtr, contra/100)) + 
  geom_point(color=usaid_blue, size=2) + 
  geom_line(color=usaid_blue, size=.4) +
  stat_smooth(data=filter(fp, fisc==2022), 
              aes(y=contra/100), 
              method="lm", se=F, color=usaid_blue, alpha=.4) +
  scale_x_yearquarter(date_labels="%Y") +
  scale_y_continuous(limits=c(.025,.15),
                     breaks=seq(.025,.15, .025),
                     labels=percent,
                     sec.axis = dup_axis()) +
  labs(x="\nJan 2018 - Sep 2022",
       y="Accessing\ncontraceptives",
       title="FY2022 on a flat/decreasing trend") +
  theme(axis.title.y.left=element_text(angle=0, vjust=.5),
        axis.title.y.right=element_blank(),
        axis.text.y.left=element_blank())


ggsave("viz/Jul-Sep 2022/Family planning/Women accessing contraceptives (Jan 2018 - Sep 2022).png",
       height=5.3,
       width=7.1)


# New family planning acceptors (acceptors) ---- 

describe(fp$acceptors)

ggplot(fp, aes(yrqtr, acceptors/100)) + 
  geom_point(color=usaid_blue, size=2) + 
  geom_line(color=usaid_blue, size=.4) +
  stat_smooth(data=filter(fp, fisc==2022), 
              aes(y=acceptors/100), 
              method="lm", se=F, color=usaid_blue, alpha=.4) +
  scale_x_yearquarter(date_labels="%Y") +
  scale_y_continuous(limits=c(0,.3),
                     breaks=seq(0,.3, .05),
                     labels=percent,
                     sec.axis = dup_axis()) +
  labs(x="\nJan 2018 - Sep 2022",
       y="Accessing\ncontraceptives",
       title="FY2022 on a flat/decreasing trend") +
  theme(axis.title.y.left=element_text(angle=0, vjust=.5),
        axis.title.y.right=element_blank(),
        axis.text.y.left=element_blank())


ggsave("viz/Jul-Sep 2022/Family planning/Women accessing contraceptives (Jan 2018 - Sep 2022).png",
       height=5.3,
       width=7.1)




# fp removed ---- 

rem <- fp %>%
  select(yrqtr, fisc,
         iucd_rem:impl_rem) %>%
  pivot_longer(3:4,
               names_to="type",
               values_to="num")

rem

describe(rem$num)

labs <- c("Implants removed", "IUCDs removed")
names(labs) <- c("impl_rem","iucd_rem")

labs

ggplot(rem, aes(yrqtr, num, color=type)) + 
  geom_point(size=2) + 
  geom_line(size=.4) +
  stat_smooth(data=filter(rem, fisc==2022), 
              aes(y=num), 
              method="lm", se=F, alpha=.4) +
  scale_x_yearquarter(date_labels="%Y") +
  scale_y_continuous(sec.axis = dup_axis(),
                     labels=comma) +
  facet_wrap(~type, ncol=1, 
             scales="free_y",
             labeller=labeller(type=labs)) + 
  faceted  +
  theme(legend.position="none",
        axis.text.y.right=element_blank(),
        axis.ticks.y.right=element_blank(),
        axis.title.y.left=element_blank(),
        axis.title.y.right=element_text(angle=0, vjust=.5)) +
  labs(x="\nJan 2018 - Sep 2022",
       y="Number\nremoved",
       title="Implants removed on an increasing trend\nIUCDs removed on a decreasing trend")


ggsave("viz/Jul-Sep 2022/Family planning/Contraceptives removed (Jan 2018 - Sep 2022).png",
       height=5.3,
       width=7.1)













