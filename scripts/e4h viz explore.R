# Zambia 4 Health

# prep ---- 

source("r prep.R")

ch <- read_xls("data/Downlaod Extract Childhealth Monthly At National.xls")

fam <- read_xls("data/Downlaod Extract Family Planning Monthly At National.xls")

fam_prov <- read_xls("data/Downlaod Extract Family Planning Yearly At Province.xls")

mat_prov <- read_xls("data/Downlaod Extract Maternal Yearly By Province.xls") 

mat_prov

mat <- read_xls("data/Downlaod Extract Maternal Monthly At National.xls")

# family ---- 



# family provincial ---- 


# maternal ---- 

names(fam)


# maternal provincial ---- 

names(fam_prov)

fam_prov <- fam_prov %>%
  rename(inst_deliv = 8)

frq(fam_prov$inst_deliv)


# child health ---- 

names(ch)

?str_sub

ch <- ch %>%
  rename(postnatal_care = 5) %>%
  mutate(postnatal_care2 = postnatal_care / 100,
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
  

?as.Date
?month

sum(ch$month_chr!=ch$month)

mnth <- as.Date(ch$month_chr, format="%B")

mnth <- data.frame(factor(ch$month_chr))

frq(mnth)


frq(ch$postnatal_care2)
frq(ch$month_chr)
frq(ch$month)
frq(ch$year)
frq(ch$mnthyr)

ggplot(ch, aes(mnthyr, postnatal_care2)) + 
  geom_point(color="dodgerblue", alpha=.6, size=.8) + 
  geom_line(color="dodgerblue", alpha=.4) +
  stat_smooth(method="lm", color="dodgerblue", se=F, size=1.2, alpha=.8) +
  scale_y_continuous(limits=c(0,1),
                     labels=percent_format(accuracy=1)) +
  labs(x="",
       y="",
       title="Maternal postnatal care within 48 hours")

ggsave("viz/maternal postnatal care overally month.png",
       device="png",
       type="cairo",
       height=4,
       width=7)


postnatal_target <- read_xls("data/Processed data.xls",
                             sheet="Maternal",
                             range="A64:E66") %>%
  pivot_longer(2:5,
               names_to="year") %>%
  rename(type=1) %>%
  mutate(year=as.numeric(year),
         value=as.numeric(value),
         mnthyr=ymd(paste(year, "-12-01")))

postnatal_target
str(postnatal_target)

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





