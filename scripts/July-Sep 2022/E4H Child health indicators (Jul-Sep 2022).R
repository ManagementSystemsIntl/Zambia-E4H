# Zambia E4 Health
# Child health indicators
# July - September 2022

# prep ---- 

getwd()
source("scripts/r prep.r")


dat <- read_xlsx("data/July-Sep 2022/Child Health (Jan 2018 - Oct 2022).xlsx",
                 sheet="export",
                 range='A2:U60') %>%
  as_tibble()

varlabs <- read_xlsx("data/July-Sep 2022/Child Health (Jan 2018 - Oct 2022).xlsx",
                  sheet="export",
                  range='A1:U1',
                  col_names = F) %>%
  t()

varlabs

?read_xlsx
str(dat)

dat[,3:21] <- map_df(dat[,3:21], as.numeric)

dat  <- dat  %>%
  mutate(month_chr = str_sub(period,
                             start=1,
                             end=nchar(period)-5),
         month = factor(month_chr,
                        levels=c("January","February","March","April","May","June","July","August","September","October","November","December")),
         month_code = as.numeric(month), 
         year = as.numeric(str_sub(period, 
                        start=nchar(period)-4,
                        end=nchar(period))),
         monyr = paste(month_code, year, sep="-"),
         mnthyr = as.POSIXlt(my(monyr)),
         qtr=as.yearqtr(mnthyr),
         measles1p = measles1/100, # measles
         measles2p = measles2/100,
         ancc1 = ifelse(measles1p > 1, 1, measles1p),
         ancc2 = ifelse(measles2p > 1, 1, measles2p), # set measles1 and measles2 to 100 for all values >100
         vitAp = ifelse(vitAsupp>100, 1, vitAsupp/100),
         dewormp = ifelse(deworm>100, 1, deworm/100))
         
str(dat)

varlabs2 <- c("Month","Month","Month","Year","Month-Year","Year-Month-Day","Quarter",
              "Measles under 1","Measles under 2", "Measles under 1 recoded","Measles under 2 recoded", "Vitamin A truncated",
              "Deworming rate truncated")

datNames <- data.frame(names(dat))

cl <- data.frame(lapply(dat, class)) %>%
  slice(1) %>%
  t()

#cl

datDict <- data.frame(var=names(dat),
                      varlab = c(varlabs, varlabs2)) %>%
  mutate(class=cl) %>%
  remove_rownames(.)

?remove_rownames

sum(dat$month_chr!=dat$month) # expecting 0 if vars same

ann <- dat %>%
  filter(mnthyr>as.POSIXlt("2021-09-01")) %>%
  filter(mnthyr<as.POSIXlt("2022-10-01")) %>%
  slice(1:12)

frq(ann$mnthyr)



pnctrgts <- read_xls("data/Jan- Jun 2022/PNC Targets.xls")
Start <- as.Date(NULL)
End <- as.Date(NULL)

pnctrgts

#chldHq <- read_xls("data/Jan- Jun 2022/Child Health Data_National Level(Quarterly).xls")
#chldHqp <- read_xls("data/Jan-Mar 2022/Child Health Data_Provincial Level(Quarterly).xls")
#chldH_prov <- read_xls("data/Jan- Jun 2022/Child Health Data_Provincial Level(Monthly).xls")
#names(chldH_prov)
#chldH_prov

chldH_prov  <- chldH_prov  %>%
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

sum(chldH_prov$month_chr!=chldH_prov$month) # expecting 0 if vars same




# maternal deaths ---- 

ggplot(ann, aes(as.Date(mnthyr), maternal_dth)) +
  geom_line(color=usaid_blue) +
  stat_smooth(method="lm", se=F, color=usaid_blue) +
  geom_point() + 
  geom_label(aes(label=maternal_dth),
               show.legend=F,
             color=usaid_blue) +
  scale_x_date(date_breaks = "2 months",
               date_labels="%b-%y") +
  scale_y_continuous(limits=c(0, 70),
                     breaks=seq(0,70,10),
                     sec.axis = dup_axis()) +
  theme(legend.title=element_blank(),
        axis.title.y.right=element_text(angle=0, vjust=.5),
        axis.title.y.left=element_blank()) +
  labs(x="\nOct 2021 - Sep 2022",
       y="Maternal\ndeaths",
       title="Maternal deaths on a declining trend")

ggsave("viz/Jul-Sep 2022/Child health/Maternal deaths (Oct 2021 - Sep 2022).png",
       height=5.2,
       width=7)


# institutional deliveries ---- 

ggplot(ann, aes(as.Date(mnthyr), inst_deliv)) +
  geom_line(color=usaid_blue) +
  stat_smooth(method="lm", se=F, color=usaid_blue) +
  geom_point() + 
#  geom_label(aes(label=round(inst_deliv, -3)),
#             show.legend=F,
#             color=usaid_blue) +
  scale_x_date(date_breaks = "2 months",
               date_labels="%b-%y") +
  scale_y_continuous(limits=c(4e4, 6e4),
                     breaks=seq(4e4,6e4,5e3),
                     labels=comma,
                     sec.axis = dup_axis()) +
  theme(legend.title=element_blank(),
        axis.title.y.right=element_text(angle=0, vjust=.5),
        axis.title.y.left=element_blank()) +
  labs(x="\nOct 2021 - Sep 2022",
       y="Institutional\ndeliveries",
       title="Institutional deliveries on a increasing trend")

ggsave("viz/Jul-Sep 2022/Child health/Institutional deliveries (Oct 2021 - Sep 2022).png",
       height=5.2,
       width=7)

# Post natal care within 48 hours

ggplot(ann, aes(as.Date(mnthyr), pnc/100)) +
  geom_line(color=usaid_blue, size=.4) +
  stat_smooth(method="lm", se=F, color=usaid_blue) +
  geom_point() + 
  geom_label(aes(label=paste(round(pnc, 0), "%", sep="")),
             show.legend=F,
             color=usaid_blue) +
  scale_x_date(date_breaks = "2 months",
               date_labels="%b-%y") +
  scale_y_continuous(limits=c(.45,.65),
                     breaks=seq(.45,.65,.05),
                     labels=percent,
                     sec.axis = dup_axis()) +
  theme(legend.title=element_blank(),
        axis.title.y.right=element_text(angle=0, vjust=.5),
        axis.title.y.left=element_blank()) +
  labs(x="\nOct 2021 - Sep 2022",
       y="\n\n\n\nPostnatal\ncare within\n48 hours",
       title="Postnatal care within 48 hours on an increasing trend")

ggsave("viz/Jul-Sep 2022/Child health/Postnatal care (Oct 2021 - Sep 2022).png",
       height=5.2,
       width=7)



# Neonatal deaths ---- 

names(ann)
desc(ann$neon_dth)

ggplot(ann, aes(as.Date(mnthyr), neon_dth)) +
  geom_line(color=usaid_blue, size=.4) +
  stat_smooth(method="lm", se=F, color=usaid_blue) +
  geom_point() + 
  geom_label(aes(label=round(neon_dth, 0)),
             show.legend=F,
             color=usaid_blue) +
  scale_x_date(date_breaks = "2 months",
               date_labels="%b-%y") 
+
  scale_y_continuous(limits=c(0, 300),
                     breaks=seq(0,300,50),
                     labels=comma,
                     sec.axis = dup_axis()) +
  theme(legend.title=element_blank(),
        axis.title.y.right=element_text(angle=0, vjust=.5),
        axis.title.y.left=element_blank()) +
  labs(x="\nOct 2021 - Sep 2022",
       y="\n\n\n\nPostnatal\ncare within\n48 hours",
       title="Postnatal care within 48 hours on an increasing trend")

ggsave("viz/Jul-Sep 2022/Child health/Postnatal care (Oct 2021 - Sep 2022).png",
       height=5.2,
       width=7)

# Immunized under 1 ---- 

ggplot(ann, aes(as.Date(mnthyr), imm1/100)) +
  geom_line(color=usaid_blue, size=.4) +
  stat_smooth(method="lm", se=F, color=usaid_blue) +
  geom_point() + 
  geom_label(aes(label=paste(round(imm1, 0), "%", sep="")),
             show.legend=F,
             color=usaid_blue) +
  scale_x_date(date_breaks = "2 months",
               date_labels="%b-%y") +
  scale_y_continuous(limits=c(.6,1.1),
                     breaks=seq(.6,1,.05),
                     labels=percent,
                     sec.axis = dup_axis()) +
  theme(legend.title=element_blank(),
        axis.title.y.right=element_text(angle=0, vjust=.5),
        axis.title.y.left=element_blank()) +
  labs(x="\nOct 2021 - Sep 2022",
       y="\n\n\n\nImmunization\ncoverage\nunder 1",
       title="Immunization coverage under 1 on a decreasing trend")

ggsave("viz/Jul-Sep 2022/Child health/Immunization under 1 (Oct 2021 - Sep 2022).png",
       height=5.2,
       width=7)

# Immunization under 2 ---- 

ggplot(ann, aes(as.Date(mnthyr), imm2/100)) +
  geom_line(color=usaid_blue, size=.4) +
  stat_smooth(method="lm", se=F, color=usaid_blue) +
  geom_point() + 
  geom_label(aes(label=paste(round(imm2, 0), "%", sep="")),
             show.legend=F,
             color=usaid_blue) +
  scale_x_date(date_breaks = "2 months",
               date_labels="%b-%y") +
  scale_y_continuous(limits=c(.4,1),
                     breaks=seq(.4,1,.05),
                     labels=percent,
                     sec.axis = dup_axis()) +
  theme(legend.title=element_blank(),
        axis.title.y.right=element_text(angle=0, vjust=.5),
        axis.title.y.left=element_blank()) +
  labs(x="\nOct 2021 - Sep 2022",
       y="\n\n\n\nImmunization\ncoverage\nunder 2",
       title="Immunization coverage under 2 on a decreasing trend")

ggsave("viz/Jul-Sep 2022/Child health/Immunization under 2 (Oct 2021 - Sep 2022).png",
       height=5.2,
       width=7)

# Family planning attendance ---- 

ggplot(ann, aes(as.Date(mnthyr), fpvisits)) +
  geom_line(color=usaid_blue, size=.4) +
  stat_smooth(method="lm", se=F, color=usaid_blue) +
  geom_point() + 
#  geom_label(aes(label=round(fpvisits, 0)),
#             show.legend=F,
#             color=usaid_blue) +
  scale_x_date(date_breaks = "2 months",
               date_labels="%b-%y") +
  scale_y_continuous(limits=c(2e5, 4e5),
                     breaks=seq(2e5, 4e5, 5e4),
                     labels=comma,
                     sec.axis = dup_axis()) +
  theme(legend.title=element_blank(),
        axis.title.y.right=element_text(angle=0, vjust=.5),
        axis.title.y.left=element_blank()) +
  labs(x="\nOct 2021 - Sep 2022",
       y="\n\n\n\nFamily\nplanning\nvisits",
       title="Family planning visits on an increasing trend")

ggsave("viz/Jul-Sep 2022/Child health/Family planning visits (Oct 2021 - Sep 2022).png",
       height=5.2,
       width=7)

# BCG coverage ---- 
library(psych)
str(ann)
psych::describe(ann$bcg1)

ggplot(ann, aes(as.Date(mnthyr), bcg1/100)) +
  geom_line(color=usaid_blue, size=.4) +
  stat_smooth(method="lm", se=F, color=usaid_blue) +
  geom_point() + 
  geom_label(aes(label=paste(round(bcg1, 0), "%", sep="")),
             show.legend=F,
             color=usaid_blue) +
  scale_x_date(date_breaks = "2 months",
               date_labels="%b-%y") +
  scale_y_continuous(limits=c(.7,1.05),
                     breaks=seq(.7,1,.05),
                     labels=percent,
                     sec.axis = dup_axis()) +
  theme(legend.title=element_blank(),
        axis.title.y.right=element_text(angle=0, vjust=.5),
        axis.title.y.left=element_blank()) +
  labs(x="\nOct 2021 - Sep 2022",
       y="\n\n\n\nBCG\ncoverage",
       title="BCG coverage on a flat trend")

ggsave("viz/Jul-Sep 2022/Child health/BCG coverage (Oct 2021 - Sep 2022).png",
       height=5.2,
       width=7)





  # Shida


#'* BCG Immunization*
names(chldH)
# view(chldH)
bcgImz <- chldH %>%
  rename(bcg = 10) %>%
  
  mutate(bcgp = bcg/100)

#'*set msles1p & msles2p to 100 for all values >100*
bcgImz1 <- bcgImz %>% 
  dplyr::mutate(bcg = ifelse(bcg > 100, 100, bcg)) %>% 
  dplyr::mutate(bcgp = bcg/100)

#'*To create legend, gather method for including a legend --*

bcgImz1 <- gather(bcgImz1, key = subpop , value = rate, c(bcg))
bcgImz1$subpop <- factor(bcgImz1$subpop, levels = unique(bcgImz1$subpop)) # transform into factor
levels(bcgImz1$subpop)

# view(chldH)

bcg_plt <- ggplot(bcgImz1, aes(x = mnthyr, y = bcgp, colour=usaid_blue )) +
  geom_point(alpha=.4, size=.6) + 
  #geom_line(size=1) +
  geom_smooth(method = loess, size = .8, se=FALSE) +
  scale_y_continuous(limits = c(0,1),
                     labels = percent,
                     breaks = c(.1,.2,.3,.4,.5,.6,.7,.8,.9, 1)) +
  scale_x_date(date_labels="%b %y",date_breaks="4 months") +
  labs(x="", y="", caption="Data Source: HMIS", title="The proportion of infants receiving the BCG vaccine \nhas been constant in the last two years, \nafter experiencing a slight decline in late 2019") +
  scale_color_manual(name ="",
                     values = usaid_blue,
                     labels = "BCG Under 1") + baseX


bcg_plt
ggsave("viz/Apr-Jun 2022/Child Health/BCG Vaccines.png",
       device="png",
       type="cairo",
       height = 5.5,
       width = 10)


#'* BCG SEPERATED BY SUPPORTED AND NON USAID PROVINCES*
names(chldH_prov)

bcg_Prov <- chldH_prov %>%
  rename(prov =1,
         bcg = 10) %>%
  
  mutate(bcgP = bcg/100) %>%
  
  
  select(prov, mnthyr, bcgP) %>% 
  mutate(prov = factor(prov),
         ip = case_when(prov=="Northern" |
                          prov =="Central" |
                          prov =="Luapula" |
                          prov =="Muchinga" |
                          prov =="Southern" |
                          prov =="Eastern" ~ "ip",
                        TRUE ~ "non-ip"))

table(bcg_Prov$ip, bcg_Prov$prov)
frq(bcg_Prov$ip) #sjmisc

levels(bcg_Prov$prov)

#'*redraw for all USAID-funded provinces*

ipfunded <- bcg_Prov %>%
  filter(ip =="ip")

levels(bcg_Prov$prov)

ipfunded1 <- ipfunded %>% 
  gather(key = subpop , value = rate, c(bcgP)) %>% 
  mutate(ip = factor(ip),
         subpop = factor(subpop))

levels(ipfunded1$ip)
levels(ipfunded1$subpop)

ip_fundedprov <- ggplot(ipfunded1, aes(x = mnthyr, y = rate, group = subpop, colour = subpop)) +
  geom_point(alpha=.6, size=.6) + 
  #geom_area(alpha=.3, size=.8, color=usaid_blue, fill=light_blue) + 
  #geom_line(size=1) +
  geom_smooth(method = loess, size = .8, se=FALSE)  +
  facet_wrap(~prov) +
  faceted +
  scale_y_continuous(limits = c(0,1),
                     labels = percent,
                     breaks = c(.2,.4,.6,.8, 1)) +
  labs(x ="", y="", caption = "Activities being implemented by \nFHN, MOMENT and G2G mechanisms in all districts") +
  ggtitle("FH Activity-supported provinces") +
  scale_color_manual(name= "", values = usaid_blue) + baseX

ip_fundedprov


#'*redraw for all Non USAID-funded provinces*

ipNot_funded <- bcg_Prov %>%
  filter(ip =="non-ip")

levels(bcg_Prov$prov)

ipNot_funded1 <- ipNot_funded %>% 
  gather(key = subpop , value = rate, c(bcgP)) %>% 
  mutate(ip = factor(ip),
         subpop = factor(subpop))

levels(ipNot_funded1$ip)
levels(ipNot_funded1$subpop)

ipNot_fundedprov <- ggplot(ipNot_funded1, aes(x = mnthyr, y = rate, group = subpop, colour = subpop)) +
  #geom_area(alpha=.3, size=.8,color=usaid_red, fill=usaid_red) + 
  geom_point(alpha=.6, size=.6) + 
  #geom_line(size=1) +
  geom_smooth(method = loess, size = .8, se=FALSE)  +
  facet_wrap(~prov) +
  faceted +
  scale_y_continuous(limits = c(0,1),
                     labels = percent,
                     breaks = c(.2,.4,.6,.8, 1)) +
  labs(x ="", y="", caption = "Data Source: HMIS") +
  ggtitle("FH Activity-supported provinces") +
  scale_color_manual(name= "", values = usaid_red, labels = c("BCG Under 1")) + baseC

ipNot_fundedprov

ip_fundedprov + ipNot_fundedprov + plot_layout(guides = "collect")

ggsave("viz/Apr-Jun 2022/Child Health/Province BCG Vaccines faceted smooth ns.png",
       device="png",
       type="cairo",
       height = 7.5,
       width = 12)


# Measles ---- 

names(dat)
describe(chldH$msles1)
str(dat)

# chldH <- gather(chldH, key = subpop , value = rate, c(msles1p, msles2p))
# chldH$subpop <- factor(chldH$subpop, levels = unique(chldH$subpop)) # transform into factor
# levels(chldH$subpop)

# view(chldH)

frq(dat$mnthyr)

meas <- dat %>%
  filter(mnthyr>as.POSIXlt("2021-09-01")) %>%
  filter(mnthyr<as.POSIXlt("2022-10-01")) %>%
  select(mnthyr, measles1p, measles2p) %>%
  pivot_longer(2:3,
               names_to="var",
               values_to="perc") %>%
  slice(1:24)

frq(meas$mnthyr)
str(meas)


  # color 

ggplot(meas, aes(as.Date(mnthyr), perc, color=var)) +
  geom_line() +
  stat_smooth(method="lm", se=F) +
  geom_point() + 
#  geom_label(aes(label=paste(round(perc*100,0), "%", sep="")),
#             show.legend=F) +
  scale_x_date(date_breaks = "2 months",
               date_labels="%b-%y") +
  scale_y_continuous(labels=percent,
                     limits=c(.4,1.12),
                     breaks=seq(.4,1,.1),
                     sec.axis = dup_axis()) +
  theme(legend.title=element_blank(),
        axis.title.y.right=element_text(angle=0, vjust=.5),
        axis.title.y.left=element_blank()) +
#        axis.title.x=element_text(margin=margin())) +
#        legend.position="right") +
  scale_color_manual(values=usaid_palette[1:2],
                     labels=c("Measles coverage, 1st dose", 
                              "Measles coverage, 2nd dose")) +
  labs(x="\nOct 2021 - Sep 2022",
       y="Coverage\nrate",
       title="Measles coverage rates on a declining trend")

ggsave("viz/Jul-Sep 2022/Measles coverage (Oct 2021 - Sep 2022) no label.png",
       height=5.2,
       width=7)


# facet 

ggplot(meas, aes(as.Date(mnthyr), perc, color=var)) +
  geom_line() +
  stat_smooth(method="lm", se=F) +
  geom_point() + 
  geom_label(aes(label=paste(round(perc*100,0), "%", sep="")),
             show.legend=F) +
  scale_x_date(date_breaks = "2 months",
               date_labels="%b-%y") +
  scale_y_continuous(labels=percent,
                     limits=c(.4,1.12),
                     breaks=seq(.4,1,.1)) +
  theme(legend.title=element_blank(),
        axis.title.y=element_text(angle=0, vjust=.5)) +
  #        axis.title.x=element_text(margin=margin())) +
  #        legend.position="right") +
  scale_color_manual(values=usaid_palette[1:2],
                     labels=c("Measles coverage, 1st dose", 
                              "Measles coverage, 2nd dose")) +
  labs(x="\nOct 2021 - Sep 2022",
       y="Coverage\nrate",
       title="Measles coverage rate on a declining trend") +
  facet_wrap(~var, ncol=1, scales="free") +
  faceted

ggsave("viz/Jul-Sep 2022/Measles coverage (Oct 2021 - Sep 2022) facet.png",
       height=4,
       width=7)



str(dat2)

ggplot(dat2, aes(as.Date(mnthyr), measles1p)) +
  geom_point() + 
  geom_line() +
  scale_x_date(date_breaks = "1 month",
               date_labels="%b-%y") +
  scale_y_continuous(labels=percent,
                     limits=c(.5,1.12),
                     breaks=seq(.5,1,.1)) +
  stat_smooth(method="lm", se=F)

measQrt <- dat %>%
  group_by(year) %>%
  summarise(across(c(ancc1, ancc2)), mean)



last_error()
last_trace()

names(dat)
?summarize_at
?across

msles_plt <- ggplot(chldH, aes(x = mnthyr, y = rate, group = subpop, colour = subpop)) +
  geom_point(alpha=.6, size=.6) + 
  #geom_line(size=1) +
  geom_smooth(method = loess, size = .8, se=FALSE) +
  scale_y_continuous(limits = c(0,1),
                     labels = percent,
                     breaks = c(.1,.2,.3,.4,.5,.6,.7,.8,.9, 1)) +
  scale_x_date(date_labels="%b %y",date_breaks="4 months") +
  labs(x="", y="", caption="Data Source: HMIS", title="Coverage of the first measles vaccine dose has slightly decreased since June 2021, \nwhile the coverage the 2nd dose has slightly increased") +
  scale_color_manual(name ="",
                     values = usaid_palette,
                     labels = c("Measles under 1", "Measles under 2")
  ) + 
  basem
msles_plt
ggsave("viz/Apr-Jun 2022/Child Health/Proportion of infacts receiving Measles Vaccines.png",
       device="png",
       type="cairo",
       height = 5.5,
       width = 9)

  # MEASLES 1 & 2 PREDICTION
library(fpp)
library(forecast)
library(backtest)
library(quantmod)
library(tseries)
library(zoo)

names(chldH)
msles2p <- chldH %>%
  filter(subpop=="msles2p") %>%
  select(mnthyr, rate, subpop)

str(msles2p)

?ts

meas2 <- dat %>%
  mutate(measles2p = ifelse(measles2p>2, 1,measles2p)) %>%
  select(measles2p) %>%
  ts()

describe(dat$measles2p)
meas2
psych::describe(dat$measles2p)
str(meas2)
plot(meas2)
acf(meas2)
pacf(meas2)

meas2.arim <- auto.arima(meas2)

summary(meas2.arim)

meas2.arim.pred <- data.frame(forecast(meas2.arim, h=6)) %>%
  mutate(mnthyr=ymd(c("2022-10-01","2022-11-01","2022-12-01", "2023-01-01","2023-02-01","2023-03-01")),
         forecast=1) 
%>%
  select(mnthyr, 
         rate=1,
         lower=4,
         upper=5,
         forecast)
meas2.arim.pred

str(out.arim.pred)


out.arim.dat <- msles2p %>%
  select(-3) %>%
  mutate(lower=NA,
         upper=NA,
         forecast=0) %>%
  bind_rows(out.arim.pred)

tail(out.arim.dat)


ggplot(out.arim.dat, aes(mnthyr, rate)) + 
  geom_point(data=out.arim.dat[1:54,], 
             color="dodgerblue2",
             size=1,
             alpha=.8) + 
  stat_smooth(#data=filter(out.arim.dat, forecast==0),
    data=out.arim.dat[1:55,],
    color="dodgerblue2") +
  scale_x_date(labels=date_format("%b-%y"),
               breaks=date_breaks("4 months")) +
  geom_point(data=filter(out.arim.dat, forecast==1),
             color="darkgoldenrod2",
             size=1,
             alpha=.8) +
  stat_smooth(data=filter(out.arim.dat, forecast==1),
              color="darkgoldenrod2",
              method="lm",
              se=F,
              fill="darkgoldenrod2",
              alpha=.2,
              size=.8) +
  geom_ribbon(aes(ymin=lower, ymax=upper),
              fill="darkgoldenrod2",
              alpha=.2) +
  scale_y_continuous(limits=c(0,.5),
                     labels=percent_format(accuracy=1)) +
  labs(x="",
       y="",
       title="Measles under 2yrs with six-month forecast")


#'* MEASLES 1 & 2 SEPERATED BY SUPPORTED AND NON USAID PROVINCES* ----
names(chldH_prov)

msle_prov <- chldH_prov %>%
  rename(prov = 1,
         msles1 = 11,
         msles2 = 12
  ) %>%
  
  mutate(msles1p = msles1/100,
         msles2p = msles2/100) %>%


  select(prov, mnthyr, msles1p, msles2p) %>% 
  mutate(prov = factor(prov),
         ip = case_when(prov=="Northern" |
                          prov =="Central" |
                          prov =="Luapula" |
                          prov =="Muchinga" |
                          prov =="Southern" |
                          prov =="Eastern" ~ "ip",
                        TRUE ~ "non-ip"))
  
  table(msle_prov$ip, msle_prov$prov)
  frq(msle_prov$ip) #sjmisc
  
  levels(msle_prov$prov)
  
  #'*redraw for all USAID-funded provinces*
  
  ipfunded <- msle_prov %>%
    filter(ip =="ip")
  
  levels(msle_prov$prov)
  
  ipfunded1 <- ipfunded %>% 
    gather(key = subpop , value = rate, c(msles1p,msles2p)) %>% 
    mutate(ip = factor(ip),
           subpop = factor(subpop))
  
  levels(ipfunded1$ip)
  levels(ipfunded1$subpop)
  
  ip_fundedprov <- ggplot(ipfunded1, aes(x = mnthyr, y = rate, group = subpop, colour = subpop)) +
    geom_point(alpha=.6, size=.6) + 
    # geom_area(alpha=.3, size=.8, color=usaid_red, fill=usaid_red) + 
    #geom_line(size=1) +
    geom_smooth(method = loess, size = .8, se=T)  +
    facet_wrap(~prov) +
    faceted +
    scale_y_continuous(limits = c(0,1),
                       labels = percent,
                       breaks = c(.2,.4,.6,.8, 1)) +
    labs(x ="", y="", caption = "Activities being implemented by \nFHN, MOMENT and G2G mechanisms \nin all districts") +
    ggtitle("FH Activity-supported provinces") +
    scale_color_manual(name= "", values = (usaid_palette)) + baseC
  
  ip_fundedprov
  
  
  #'*redraw for all Non USAID-funded provinces*
  
  ipNot_funded <- msle_prov %>%
    filter(ip =="non-ip")
  
  levels(msle_prov$prov)
  
  ipNot_funded1 <- ipNot_funded %>% 
    gather(key = subpop , value = rate, c(msles1p,msles2p)) %>% 
    mutate(ip = factor(ip),
           subpop = factor(subpop))
  
  levels(ipNot_funded1$ip)
  levels(ipNot_funded1$subpop)
  
  ipNot_fundedprov <- ggplot(ipNot_funded1, aes(x = mnthyr, y = rate, group = subpop, colour = subpop)) +
    #geom_area(alpha=.3, size=.8,color=usaid_blue, fill=light_blue) + 
    geom_point(alpha=.6, size=.6) + 
    #geom_line(size=1) +
    geom_smooth(method = loess, size = .8, se=T)  +
    facet_wrap(~prov) +
    faceted +
    scale_y_continuous(limits = c(0,1),
                       labels = percent,
                       breaks = c(.2,.4,.6,.8, 1)) +
    labs(x ="", y="", caption = "Data Source: HMIS") +
    ggtitle("Non FH Activity-supported provinces") +
    scale_color_manual(name= "", values = (usaid_palette), labels = c("Measles under 1", "Measles under 2")) + basey
  
  ipNot_fundedprov
  
  ip_fundedprov + ipNot_fundedprov + plot_layout(guides = "collect")
  
  ggsave("viz/Apr-Jun 2022/Child Health/Province Proportion of infacts receiving Measles Vaccines faceted ns.png",
         device="png",
         type="cairo",
         height = 6.5,
         width = 12)
  
  
# DPT 1st dose under 1 ---- 
  
describe(ann$dptfirst1)

ggplot(ann, aes(as.Date(mnthyr), dptfirst1/100)) +
    geom_line(color=usaid_blue, size=.4) +
    stat_smooth(method="lm", se=F, color=usaid_blue) +
    geom_point() + 
    geom_label(aes(label=paste(round(dptfirst1, 0), "%", sep="")),
               show.legend=F,
               color=usaid_blue) +
    scale_x_date(date_breaks = "2 months",
                 date_labels="%b-%y") +
    scale_y_continuous(limits=c(.8,1.13),
                       breaks=seq(.8,1,.05),
                       labels=percent,
                       sec.axis = dup_axis()) +
    theme(legend.title=element_blank(),
          axis.title.y.right=element_text(angle=0, vjust=.5),
          axis.title.y.left=element_blank()) +
    labs(x="\nOct 2021 - Sep 2022",
         y="\n\n\n\nDPT\ncoverage",
         title="DPT first dose under 1 on a decreasing trend")
  
ggsave("viz/Jul-Sep 2022/Child health/DPT first dose under 1 (Oct 2021 - Sep 2022).png",
         height=5.2,
         width=7)
  
    
  
  
  #'* DPT HIB HEP 1st DOSE Coverage*
  names(chldH)
  # view(chldH)
  dpt_hep <- chldH %>%
    rename(dpt = 13) %>%
    
    mutate(dptp = dpt/100)
  
  #'*setting values of dpt 1st dose to 100 for all values >100*
  dpt_hep1 <- dpt_hep %>% 
    dplyr::mutate(dpt = ifelse(dpt > 100, 100, dpt)) %>% 
    dplyr::mutate(dptp = dpt/100)
  
  #'*To create legend, gather method for including a legend --*
  
  dpt_hep1 <- gather(dpt_hep1, key = subpop , value = rate, c(dpt))
  dpt_hep1$subpop <- factor(dpt_hep1$subpop, levels = unique(dpt_hep1$subpop)) # transform into factor
  levels(dpt_hep1$subpop)
  
  # view(chldH)
  
  dpt_plt <- ggplot(dpt_hep1, aes(x = mnthyr, y = dptp, colour=usaid_blue )) +
    geom_point(alpha=.4, size=.6) + 
    #geom_line(size=1) +
    geom_smooth(method = loess, size = .8, se=FALSE) +
    scale_y_continuous(limits = c(0,1),
                       labels = percent,
                       breaks = c(.1,.2,.3,.4,.5,.6,.7,.8,.9, 1)) +
    scale_x_date(date_labels="%b %y",date_breaks="4 months") +
    labs(x="", y="", caption="Data Source: HMIS", title="The proportion of infants receiving 1st Dose of DPT/Hep/Hib has remained constant \nover the last two years") +
    scale_color_manual(name ="",
                       values = usaid_blue,
                       labels = "DPT Hib Hep 1st dose") + baseX
  
  dpt_plt
  ggsave("viz/Apr-Jun 2022/Child Health/DPT Hib Hep 1st dose Vaccines.png",
         device="png",
         type="cairo",
         height = 5.5,
         width = 9)
  
  
  #'* DPT HIB HEP DOSE SEPERATED BY SUPPORTED AND NON USAID PROVINCES*
  names(chldH_prov)
  
  dpt_Prov <- chldH_prov %>%
    rename(prov =1,
           dpt = 13) %>%
    
    mutate(dptp = dpt/100) %>%
    
    
    select(prov, mnthyr, dptp) %>% 
    mutate(prov = factor(prov),
           ip = case_when(prov=="Northern" |
                            prov =="Central" |
                            prov =="Luapula" |
                            prov =="Muchinga" |
                            prov =="Southern" |
                            prov =="Eastern" ~ "ip",
                          TRUE ~ "non-ip"))
  
  table(dpt_Prov$ip, dpt_Prov$prov)
  frq(dpt_Prov$ip) #sjmisc
  
  levels(dpt_Prov$prov)
  
  #'*redraw for all USAID-funded provinces*
  
  ipfunded_dpt <- dpt_Prov %>%
    filter(ip =="ip")
  
  levels(dpt_Prov$prov)
  
  ipfunded_dpt1 <- ipfunded_dpt %>% 
    gather(key = subpop , value = rate, c(dptp)) %>% 
    mutate(ip = factor(ip),
           subpop = factor(subpop))
  
  levels(ipfunded_dpt1$ip)
  levels(ipfunded_dpt1$subpop)
  
  spprtd_dptprov <- ggplot(ipfunded_dpt1, aes(x = mnthyr, y = rate, group = subpop, colour = subpop)) +
    geom_point(alpha=.6, size=.6) + 
    #geom_area(alpha=.3, size=.8, color=usaid_blue, fill=light_blue) + 
    #geom_line(size=1) +
    geom_smooth(method = loess, size = .8, se=FALSE)  +
    facet_wrap(~prov) +
    faceted +
    scale_y_continuous(limits = c(0,1),
                       labels = percent,
                       breaks = c(.2,.4,.6,.8, 1)) +
    labs(x ="", y="", caption = "Activities being implemented by \nFHN, MOMENT and G2G mechanisms in all districts") +
    ggtitle("FH Activity-supported provinces") +
    scale_color_manual(name= "", values = usaid_blue) + baseC
  
  spprtd_dptprov
  
  
  #'*redraw for all Non USAID-funded provinces*
  
  nonaid_dpt <- dpt_Prov %>%
    filter(ip =="non-ip")
  
  levels(dpt_Prov$prov)
  
  nonaid_dpt1 <- nonaid_dpt %>% 
    gather(key = subpop , value = rate, c(dptp)) %>% 
    mutate(ip = factor(ip),
           subpop = factor(subpop))
  
  levels(nonaid_dpt1$ip)
  levels(nonaid_dpt1$subpop)
  
  nonaid_prov <- ggplot(nonaid_dpt1, aes(x = mnthyr, y = rate, group = subpop, colour = subpop)) +
    #geom_area(alpha=.3, size=.8,color=usaid_red, fill=usaid_red) + 
    geom_point(alpha=.6, size=.6) + 
    #geom_line(size=1) +
    geom_smooth(method = loess, size = .8, se=FALSE)  +
    facet_wrap(~prov) +
    faceted +
    scale_y_continuous(limits = c(0,1),
                       labels = percent,
                       breaks = c(.2,.4,.6,.8, 1)) +
    labs(x ="", y="", caption = "Data Source: HMIS") +
    ggtitle("Non FH Activity-supported provinces") +
    scale_color_manual(name= "", values = usaid_red, labels = c("DPT Hib Heb Under 1")) + baseX
  
  nonaid_prov
  
  spprtd_dptprov +nonaid_prov + plot_layout(guides = "collect")
  
  ggsave("viz/Apr-Jun 2022/Child Health/Province DPT Vaccines faceted smooth ns.png",
         device="png",
         type="cairo",
         height = 7.5,
         width = 12)
  
  
  #'* FULLY IMMUNIZED Coverage*
  names(chldH)
  # view(chldH)
  fullyimz <- chldH %>%
    rename(fic = 7) %>%
    
    mutate(ficp = fic/100)
  
  #'*setting values of dpt 1st dose to 100 for all values >100*
  fullyimz1 <- fullyimz %>% 
    dplyr::mutate(fic = ifelse(fic > 100, 100, fic)) %>% 
    dplyr::mutate(ficp = fic/100)
  
  #'*To create legend, gather method for including a legend --*
  
  fullyimz1 <- gather(fullyimz1, key = subpop , value = rate, c(fic))
  fullyimz1$subpop <- factor(fullyimz1$subpop, levels = unique(fullyimz1$subpop)) # transform into factor
  levels(fullyimz1$subpop)
  
  pnctrgts$Start <- as.Date(pnctrgts$Start) 
  pnctrgts$End <- as.Date(pnctrgts$End) 
  Start <- as.Date(NULL)
  End <- as.Date(NULL)
  
  # view(chldH)
  
  full_plt <- ggplot(fullyimz1, aes(x = mnthyr, y = ficp, colour=usaid_blue )) +
    geom_rect(data=pnctrgts, aes(NULL,NULL,xmin=Start,xmax=End,fill=PNCTargets),
              ymin=c(.79,.85,.90,.96) ,ymax=c(.80,.86,.91,.97), colour=light_grey, size=0.8, alpha=0.8, lty="solid", fill=usaid_red) +
    geom_point(alpha=.4, size=1.9) + 
    #geom_line(size=1) +
    geom_smooth(method = loess, size = .8, se=FALSE) +
    scale_y_continuous(limits = c(0,1),
                       labels = percent,
                       breaks = c(.1,.2,.3,.4,.5,.6,.7,.8,.9, 1)) +
    scale_x_date(date_labels="%b %y",date_breaks="4 months") +
    labs(x="", y="", caption="Data Source: HMIS", title="The proportion of infants that are fully immunized has been constant in the \nlast two years, after undergoing a slight decline in late 2020") +
    scale_color_manual(name ="",
                       values = usaid_blue,
                       labels = "Fully Immunized") + basey +
    annotate(geom = "text", x=trgt1, y = 0.79, family="Gill Sans Mt", colour = usaid_red, label=substitute("National Targets"), size= 4,  hjust =0, vjust=-4.5) +
    annotate(geom = "text", x=trgt1, y = 0.79, family="Gill Sans Mt", colour = rich_black, label=substitute(paste(bold("79%"))), size= 4,  hjust =-2.5, vjust=-.2) +
    annotate(geom = "text", x=trgt1, y = 0.85, family="Gill Sans Mt", colour = rich_black, label=substitute(paste(bold("85%"))), size= 4,  hjust =-9.5, vjust=-1) +
    annotate(geom = "text", x=trgt1, y = 0.90, family="Gill Sans Mt", colour = rich_black, label=substitute(paste(bold("90%"))), size= 4,  hjust =-15.5, vjust=-1) +
    annotate(geom = "text", x=trgt1, y = 0.96, family="Gill Sans Mt", colour = rich_black, label=substitute(paste(bold("96%"))), size= 4,  hjust =-22, vjust=-1)
  
  full_plt
  
  ggsave("viz/Apr-Jun 2022/Child Health/Fully immunised coverage.png",
         device="png",
         type="cairo",
         height = 5.5,
         width = 9)
  
  
  #'* FULLY IMMUNIZED SEPERATED BY SUPPORTED AND NON USAID PROVINCES*
  names(chldH_prov)
  
  fic_Prov <- chldH_prov %>%
    rename(prov =1,
           fic = 7) %>%
    
    mutate(ficp = fic/100) %>%
    
    
    select(prov, mnthyr, ficp) %>% 
    mutate(prov = factor(prov),
           ip = case_when(prov=="Northern" |
                            prov =="Central" |
                            prov =="Luapula" |
                            prov =="Muchinga" |
                            prov =="Southern" |
                            prov =="Eastern" ~ "ip",
                          TRUE ~ "non-ip"))
  
  table(fic_Prov$ip, fic_Prov$prov)
  frq(fic_Prov$ip) #sjmisc
  
  levels(fic_Prov$prov)
  
  #'*redraw for all USAID-funded provinces*
  
  ipfunded_fic <- fic_Prov %>%
    filter(ip =="ip")
  
  levels(fic_Prov$prov)
  
  ipfunded_fic1 <- ipfunded_fic %>% 
    gather(key = subpop , value = rate, c(ficp)) %>% 
    mutate(ip = factor(ip),
           subpop = factor(subpop))
  
  levels(ipfunded_fic1$ip)
  levels(ipfunded_fic1$subpop)
  
  spprtd_ficprov <- ggplot(ipfunded_fic1, aes(x = mnthyr, y = rate, group = subpop, colour = subpop)) +
    geom_point(alpha=.6, size=.6) + 
    #geom_area(alpha=.3, size=.8, color=usaid_blue, fill=light_blue) + 
    #geom_line(size=1) +
    geom_smooth(method = loess, size = .8, se=FALSE)  +
    facet_wrap(~prov) +
    faceted +
    scale_y_continuous(limits = c(0,1),
                       labels = percent,
                       breaks = c(.2,.4,.6,.8, 1)) +
    labs(x ="", y="", caption = "Activities being implemented by \nFHN, MOMENT and G2G mechanisms in all district") +
    ggtitle("FH Activity-supported provinces") +
    scale_color_manual(name= "", values = usaid_blue) + baseC
  
  spprtd_ficprov
  
  
  #'*redraw for all Non USAID-funded provinces*
  
  nonaid_fic <- fic_Prov %>%
    filter(ip =="non-ip")
  
  levels(fic_Prov$prov)
  
  nonaid_dpt1 <- nonaid_fic %>% 
    gather(key = subpop , value = rate, c(ficp)) %>% 
    mutate(ip = factor(ip),
           subpop = factor(subpop))
  
  levels(nonaid_dpt1$ip)
  levels(nonaid_dpt1$subpop)
  
  nonaid_ficprov <- ggplot(nonaid_dpt1, aes(x = mnthyr, y = rate, group = subpop, colour = subpop)) +
    #geom_area(alpha=.3, size=.8,color=usaid_red, fill=usaid_red) + 
    geom_point(alpha=.6, size=.6) + 
    #geom_line(size=1) +
    geom_smooth(method = loess, size = .8, se=FALSE)  +
    facet_wrap(~prov) +
    faceted +
    scale_y_continuous(limits = c(0,1),
                       labels = percent,
                       breaks = c(.2,.4,.6,.8, 1)) +
    labs(x ="", y="", caption = "Data Source: HMIS") +
    ggtitle("Non FH Activity-supported provinces") +
    scale_color_manual(name= "", values = usaid_red, labels = c("Fully Immunized Under 1")) + baseX
  
  nonaid_ficprov
  
  spprtd_ficprov +nonaid_ficprov + plot_layout(guides = "collect")
  
  ggsave("viz/Apr-Jun 2022/Child Health/Province Fully Immunized Vaccines faceted smooth ns.png",
         device="png",
         type="cairo",
         height = 7.5,
         width = 12)
  

# Vitamin A supplementation ---- 

describe(ann$vitAsupp)
describe(ann$vitAp)  

ggplot(ann, aes(as.Date(mnthyr), vitAp)) +
    geom_line(color=usaid_blue, size=.4) +
    stat_smooth(method="lm", se=F, color=usaid_blue) +
    geom_point() + 
    geom_label(aes(label=paste(round(vitAp*100, 0), "%", sep="")),
               show.legend=F,
               color=usaid_blue) +
    scale_x_date(date_breaks = "2 months",
                 date_labels="%b-%y") +
    scale_y_continuous(limits=c(0,1),
                       breaks=seq(0,1,.1),
                       labels=percent,
                       sec.axis = dup_axis()) +
    theme(legend.title=element_blank(),
          axis.title.y.right=element_text(angle=0, vjust=.5),
          axis.title.y.left=element_blank()) +
    labs(x="\nOct 2021 - Sep 2022",
         y="\n\n\n\nVitamin A\nsupplementation",
         title="Vitamin A supplementation on a decreasing trend")

ggsave("viz/Jul-Sep 2022/Child health/Vitamin A supplementation truncated (Oct 2021 - Sep 2022).png",
       height=5.2,
       width=7)
  
  
  
  
  #'* % OF CHILDREN RECEIVING Vitamin A*
  names(chldH)
  # view(chldH)
  crv <- chldH %>%
    rename(crv = 14) %>%
    
    mutate(crvp = crv/100)
  
  #'*setting values of dpt 1st dose to 100 for all values >100*
  crv1 <- crv %>% 
    dplyr::mutate(fic = ifelse(crv > 100, 100, crv)) %>% 
    dplyr::mutate(crvp = crv/100)
  
  #'*To create legend, gather method for including a legend --*
  
  crv1 <- gather(crv1, key = subpop , value = rate, c(crv))
  crv1$subpop <- factor(crv1$subpop, levels = unique(crv1$subpop)) # transform into factor
  levels(crv1$subpop)
  
  # view(chldH)
  
  vitamin_plt <- ggplot(crv1, aes(x = mnthyr, y = crvp, colour=usaid_blue )) +
    geom_point(alpha=.4, size=.6) + 
    #geom_line(size=1) +
    geom_smooth(method = loess, size = .8, se=FALSE) +
    scale_y_continuous(limits = c(0,1),
                       labels = percent,
                       breaks = c(.1,.2,.3,.4,.5,.6,.7,.8,.9, 1)) +
    scale_x_date(date_labels="%b %y",date_breaks="4 months") +
    labs(x="", y="", caption="Data Source: HMIS", title="Proportion of infants receiving 1st dose of Vitamin A \nhas generally been low, below 20% since 2018") +
    scale_color_manual(name ="",
                       values = usaid_blue,
                       labels = "Vitamin A") + baseX
  vitamin_plt
  
  ggsave("viz/Apr-Jun 2022/Child Health/Vitamin A coverage.png",
         device="png",
         type="cairo",
         height = 5.5,
         width = 9)
  
  
  #'* VITAMIN A COVERAGE SEPERATED BY SUPPORTED AND NON USAID PROVINCES*
  names(chldH_prov)
  
  crv_Prov <- chldH_prov %>%
    rename(prov =1,
           crv = 14) %>%
    
    mutate(crvp = crv/100) %>%
    
    
    select(prov, mnthyr, crvp) %>% 
    mutate(prov = factor(prov),
           ip = case_when(prov=="Northern" |
                            prov =="Central" |
                            prov =="Luapula" |
                            prov =="Copperbelt" ~ "ip",
                          TRUE ~ "non-ip"))
  
  table(crv_Prov$ip, crv_Prov$prov)
  frq(crv_Prov$ip) #sjmisc
  
  levels(crv_Prov$prov)
  
  #'*redraw for all USAID-funded provinces*
  
  ipfunded_crv <- crv_Prov %>%
    filter(ip =="ip")
  
  levels(crv_Prov$prov)
  
  ipfunded_crv1 <- ipfunded_crv %>% 
    gather(key = subpop , value = rate, c(crvp)) %>% 
    mutate(ip = factor(ip),
           subpop = factor(subpop))
  
  levels(ipfunded_crv1$ip)
  levels(ipfunded_crv1$subpop)
  
  spprtd_crvprov <- ggplot(ipfunded_crv1, aes(x = mnthyr, y = rate, group = subpop, colour = subpop)) +
    geom_point(alpha=.6, size=.6) + 
    #geom_area(alpha=.3, size=.8, color=usaid_blue, fill=light_blue) + 
    #geom_line(size=1) +
    geom_smooth(method = loess, size = .8, se=FALSE)  +
    facet_wrap(~prov) +
    faceted +
    scale_y_continuous(limits = c(0,1),
                       labels = percent,
                       breaks = c(.2,.4,.6,.8, 1)) +
    labs(x ="", y="", caption = "Activities being implemented \nby SUNTA in some districts") +
    ggtitle("Non FH Activity-supported provinces") +
    scale_color_manual(name= "", values = usaid_blue) + baseC
  
  spprtd_crvprov
  
  
  #'*redraw for all Non USAID-funded provinces*
  
  nonaid_crv <- crv_Prov %>%
    filter(ip =="non-ip")
  
  levels(crv_Prov$prov)
  
  nonaid_crv1 <- nonaid_crv %>% 
    gather(key = subpop , value = rate, c(crvp)) %>% 
    mutate(ip = factor(ip),
           subpop = factor(subpop))
  
  levels(nonaid_crv1$ip)
  levels(nonaid_crv1$subpop)
  
  nonaid_crvprov <- ggplot(nonaid_crv1, aes(x = mnthyr, y = rate, group = subpop, colour = subpop)) +
    #geom_area(alpha=.3, size=.8,color=usaid_red, fill=usaid_red) + 
    geom_point(alpha=.6, size=.6) + 
    #geom_line(size=1) +
    geom_smooth(method = loess, size = .8, se=FALSE)  +
    facet_wrap(~prov) +
    faceted +
    scale_y_continuous(limits = c(0,1),
                       labels = percent,
                       breaks = c(.2,.4,.6,.8, 1)) +
    labs(x ="", y="", caption = "Data Source: HMIS") +
    ggtitle("Non FH Activity-supported provinces") +
    scale_color_manual(name= "", values = usaid_red, labels = c("Fully Immunized Under 1")) + baseX
  
  nonaid_crvprov
  
  spprtd_crvprov + nonaid_crvprov + plot_layout(guides = "collect")
  
  ggsave("viz/Apr-Jun 2022/Child Health/Province Vitamin A Dose faceted smooth ns.png",
         device="png",
         type="cairo",
         height = 7.5,
         width = 12)
  
  
# Deworming rate ---- 
  
describe(ann$deworm)
  
ggplot(ann, aes(as.Date(mnthyr), dewormp)) +
  geom_line(color=usaid_blue, size=.4) +
  stat_smooth(method="lm", se=F, color=usaid_blue) +
  geom_point() + 
  geom_label(aes(label=paste(round(dewormp*100, 0), "%", sep="")),
             show.legend=F,
             color=usaid_blue) +
  scale_x_date(date_breaks = "2 months",
               date_labels="%b-%y") +
  scale_y_continuous(limits=c(0,1),
                     breaks=seq(0,1,.1),
                     labels=percent,
                     sec.axis = dup_axis()) +
  theme(legend.title=element_blank(),
        axis.title.y.right=element_text(angle=0, vjust=.5),
        axis.title.y.left=element_blank()) +
  labs(x="\nOct 2021 - Sep 2022",
       y="\n\n\n\nDeworming\nrate",
       title="Deworming rate on a decreasing trend")
  
ggsave("viz/Jul-Sep 2022/Child health/Deworming rate truncated (Oct 2021 - Sep 2022).png",
         height=5.2,
         width=7)


# Stunting ---- 

describe(ann$stunting)

ggplot(ann, aes(as.Date(mnthyr), stunting/100)) +
  geom_line(color=usaid_blue, size=.4) +
  stat_smooth(method="lm", se=F, color=usaid_blue) +
  geom_point() + 
  geom_label(aes(label=paste(round(stunting, 2), "%", sep="")),
             show.legend=F,
             color=usaid_blue) +
  scale_x_date(date_breaks = "2 months",
               date_labels="%b-%y") +
  scale_y_continuous(limits=c(0,.01),
                     breaks=seq(0,.1,.001),
                     labels=percent_format(accuracy=.1),
                     sec.axis = dup_axis()) +
  theme(legend.title=element_blank(),
        axis.title.y.right=element_text(angle=0, vjust=.5),
        axis.title.y.left=element_blank()) +
  labs(x="\nOct 2021 - Sep 2022",
       y="\n\n\n\nStunting\nrate",
       title="Stunting rate on a decreasing trend")

ggsave("viz/Jul-Sep 2022/Child health/Stunting rate (Oct 2021 - Sep 2022).png",
       height=5.2,
       width=7)


# Wasting ---- 

describe(ann$wasting)

ggplot(ann, aes(as.Date(mnthyr), wasting/100)) +
  geom_line(color=usaid_blue, size=.4) +
  stat_smooth(method="lm", se=F, color=usaid_blue) +
  geom_point() + 
  geom_label(aes(label=paste(round(wasting, 2), "%", sep="")),
             show.legend=F,
             color=usaid_blue, 
             size=3) +
  scale_x_date(date_breaks = "2 months",
               date_labels="%b-%y") +
  scale_y_continuous(limits=c(0,.0111),
                     breaks=seq(0,.011,.001),
                     labels=percent_format(accuracy=.1),
                     sec.axis = dup_axis()) +
  theme(legend.title=element_blank(),
        axis.title.y.right=element_text(angle=0, vjust=.5),
        axis.title.y.left=element_blank()) +
  labs(x="\nOct 2021 - Sep 2022",
       y="\n\n\n\nWasting\nrate",
       title="Wasting rate on a decreasing trend")

ggsave("viz/Jul-Sep 2022/Child health/Wasting rate (Oct 2021 - Sep 2022).png",
       height=5.2,
       width=7.1)


# Breastfeeding ---- 

  # breastfeeding within one hour

describe(ann$brstfd_1hr)

ggplot(ann, aes(as.Date(mnthyr), brstfd_1hr/100)) +
  geom_line(color=usaid_blue, size=.4) +
  stat_smooth(method="lm", se=F, color=usaid_blue) +
  geom_point() + 
  geom_label(aes(label=paste(round(brstfd_1hr, 0), "%", sep="")),
             show.legend=F,
             color=usaid_blue, 
             size=3) +
  scale_x_date(date_breaks = "2 months",
               date_labels="%b-%y") +
  scale_y_continuous(limits=c(.75,1),
                     breaks=seq(.75,1,.05),
                     labels=percent_format(accuracy=1),
                     sec.axis = dup_axis()) +
  theme(legend.title=element_blank(),
        axis.title.y.right=element_text(angle=0, vjust=.5),
        axis.title.y.left=element_blank()) +
  labs(x="\nOct 2021 - Sep 2022",
       y="\n\n\n\nBreastfeeding\n1 hr",
       title="Breastfeeding within 1 hour on a flat/increasing trend")

ggsave("viz/Jul-Sep 2022/Child health/Breastfeeding 1 hr (Oct 2021 - Sep 2022).png",
       height=5.2,
       width=7.1)

  # breastfeeding within at six months

describe(ann$brstfd_6mnth)

ggplot(ann, aes(as.Date(mnthyr), brstfd_6mnth/100)) +
  geom_line(color=usaid_blue, size=.4) +
  stat_smooth(method="lm", se=F, color=usaid_blue) +
  geom_point() + 
  geom_label(aes(label=paste(round(brstfd_6mnth, 0), "%", sep="")),
             show.legend=F,
             color=usaid_blue, 
             size=3) +
  scale_x_date(date_breaks = "2 months",
               date_labels="%b-%y") +
  scale_y_continuous(limits=c(.5,.85),
                     breaks=seq(.5,.85,.05),
                     labels=percent_format(accuracy=1),
                     sec.axis = dup_axis()) +
  theme(legend.title=element_blank(),
        axis.title.y.right=element_text(angle=0, vjust=.5),
        axis.title.y.left=element_blank()) +
  labs(x="\nOct 2021 - Sep 2022",
       y="\nBreastfeeding\n6 months",
       title="Breastfeeding at 6 months on an increasing trend")

ggsave("viz/Jul-Sep 2022/Child health/Breastfeeding 6 months (Oct 2021 - Sep 2022).png",
       height=5.2,
       width=7.1)

# OPD first attendance ---- 

describe(ann$opdAtt)

ggplot(ann, aes(as.Date(mnthyr), opdAtt)) +
  geom_line(color=usaid_blue, size=.4) +
  stat_smooth(method="lm", se=F, color=usaid_blue) +
  geom_point() + 
#  geom_label(aes(label=round(opdAtt, -3)),
#             show.legend=F,
#             color=usaid_blue, 
#             size=3) +
  scale_x_date(date_breaks = "2 months",
               date_labels="%b-%y") +
  scale_y_continuous(limits=c(1.25e6, 2.5e6),
                     breaks=seq(1.25e6, 2.5e6, 2.5e5),
                     labels=comma,
                     sec.axis = dup_axis()) +
  theme(legend.title=element_blank(),
        axis.title.y.right=element_text(angle=0, vjust=.5),
        axis.title.y.left=element_blank()) +
  labs(x="\nOct 2021 - Sep 2022",
       y="\nOPD\nattendance",
       title="OPD first attendance on a decreasing trend")

ggsave("viz/Jul-Sep 2022/Child health/OPD attendance (Oct 2021 - Sep 2022).png",
       height=5.2,
       width=7.1)

# EBF at 6 months ---- 

describe(ann$ebf)

ggplot(ann, aes(as.Date(mnthyr), ebf/100)) +
  geom_line(color=usaid_blue, size=.4) +
  stat_smooth(method="lm", se=F, color=usaid_blue) +
  geom_point() + 
  geom_label(aes(label=paste(round(ebf, 0), "%", sep="")),
             show.legend=F,
             color=usaid_blue, 
             size=3) +
  scale_x_date(date_breaks = "2 months",
               date_labels="%b-%y") +
  scale_y_continuous(limits=c(.5,.8),
                     breaks=seq(.5,.8, .05),
                     labels=percent,
                     sec.axis = dup_axis()) +
  theme(legend.title=element_blank(),
        axis.title.y.right=element_text(angle=0, vjust=.5),
        axis.title.y.left=element_blank()) +
  labs(x="\nOct 2021 - Sep 2022",
       y="\nEBF\nrate",
       title="EBF rate on an increasing trend")

ggsave("viz/Jul-Sep 2022/Child health/EBF rate (Oct 2021 - Sep 2022).png",
       height=5.2,
       width=7.1)



  
  #################'*Immunizations Trends* ---- 
  pger_summary <- (msles_plt | bcg_plt)/(dpt_plt | vitamin_plt)
  pger_summary
  ggsave("viz/Apr-Jun 2022/Child Health/Immunization SUmmary ns.png",
         plot=pger_summary,
         device="png",
         type="cairo",
         height = 8.5,
         width = 15)
  
  
  #'* EBF @6MONTHS & INITIATION ON BRESTMILK WITH ONE HOUR OF BIRTH A*
  
  names(chldH)
  # view(chldH)
  breastfeed <- chldH %>%
    rename(brst1hr = 18,
           ebf = 19
           ) %>%
    
    mutate(brst1hrp = brst1hr/100,
           ebfp = ebf/100)
  
  #'*set EBF & BREASTFEED 1HOUR to 100 for all values >100*
  breastfeed <- breastfeed %>% 
    dplyr::mutate(brst1hr = ifelse(brst1hr > 100, 100, brst1hr)) %>% 
    dplyr::mutate(brst1hrp = brst1hr/100)
  
  #'*To create legend, gather method for including a legend --*
  
  breastfeed <- gather(breastfeed, key = subpop , value = rate, c(brst1hrp, ebfp))
  breastfeed$subpop <- factor(breastfeed$subpop, levels = unique(breastfeed$subpop)) # transform into factor
  levels(breastfeed$subpop)
  
  pnctrgts$Start <- as.Date(pnctrgts$Start) 
  pnctrgts$End <- as.Date(pnctrgts$End) 
  Start <- as.Date(NULL)
  End <- as.Date(NULL)
  colnames(breastfeed)
  # view(chldH)
  
  brstfeeding_plt <- ggplot(breastfeed, aes(x = mnthyr, y = rate, group = subpop, colour = subpop)) +
    # geom_rect(data=pnctrgts, aes(NULL,NULL,xmin=Start,xmax=End,fill=PNCTargets),
    #           ymin=c(.79,.85,.90,.96) ,ymax=c(.80,.86,.91,.97), colour=light_grey, size=0.8, alpha=0.8, lty="solid", fill=usaid_red) +
    #geom_area(alpha=.3, size=.8,color=usaid_blue, fill=light_blue) +
    geom_point(alpha=.6, size=1.5) + 
    #geom_line(size=1) +
    geom_smooth(method = loess, size = .8, se=FALSE) +
    scale_y_continuous(limits = c(0,1),
                       labels = percent,
                       breaks = c(.1,.2,.3,.4,.5,.6,.7,.8,.9, 1)) +
    scale_x_date(date_labels="%b %y",date_breaks="4 months") +
    labs(x="", y="", caption="Data Source: HMIS", title="Proportion of infants breastfed within 1 hour of birth has slightly increased \nin the last years, reaching 90% in June 2022") +
    scale_color_manual(name ="",
                       values = usaid_palette,
                       labels = c("Initiation on breastmilk with one hour of birth", "Infants on EBF at 6 months")) + basey
#     basey +
#     annotate(geom = "text", x=trgt1, y = 0.79, family="Gill Sans Mt", colour = usaid_red, label=substitute("Targets Breastfeeding within 1 hour
# of birth"), size= 4,  hjust =0, vjust=-4.5) +
#     annotate(geom = "text", x=trgt1, y = 0.79, family="Gill Sans Mt", colour = rich_black, label=substitute(paste(bold("79%"))), size= 4,  hjust =-2.5, vjust=-.2) +
#     annotate(geom = "text", x=trgt1, y = 0.85, family="Gill Sans Mt", colour = rich_black, label=substitute(paste(bold("85%"))), size= 4,  hjust =-9.5, vjust=-1) +
#     annotate(geom = "text", x=trgt1, y = 0.90, family="Gill Sans Mt", colour = rich_black, label=substitute(paste(bold("90%"))), size= 4,  hjust =-15.5, vjust=-1) +
#     annotate(geom = "text", x=trgt1, y = 0.96, family="Gill Sans Mt", colour = rich_black, label=substitute(paste(bold("96%"))), size= 4,  hjust =-22, vjust=-1)
#   
  
  brstfeeding_plt
  
  ggsave("viz/Apr-Jun 2022/Child Health/National EBF ns.png",
         device="png",
         type="cairo",
         height = 5.5,
         width = 9.5)
  
  
  
  #'* EBF @6MONTHS & INITIATION ON BRESTMILK WITH ONE HOUR OF BIRTH A*
  #' *SEPERATED BY SUPPORTED AND NON USAID PROVINCES*

  
  names(chldH_prov)
  
  brstfeeding_Prov <- chldH_prov %>%
    rename(prov =1,
           brst1hr = 18,
           ebf = 19) %>%
    
    mutate(brst1hrp = brst1hr/100,
           ebfp = ebf/100) %>%
    
    select(prov, mnthyr, brst1hrp, ebfp) %>% 
    mutate(prov = factor(prov),
           ip = case_when(prov=="Northern" |
                            prov =="Central" |
                            prov =="Luapula" |
                            prov == "Muchinga" |
                            prov == "Southern" |
                            prov == "Eastern" |
                            prov =="Copperbelt" ~ "ip",
                          TRUE ~ "non-ip"))
  
  table(brstfeeding_Prov$ip, brstfeeding_Prov$prov)
  frq(brstfeeding_Prov$ip) #sjmisc
  
  levels(brstfeeding_Prov$prov)
  
  #'*redraw for all USAID-funded provinces*
  
  ipfunded_brstfd <- brstfeeding_Prov %>%
    filter(ip =="ip")
  
  levels(ipfunded_brstfd$prov)
  
  ipfunded_brstfd1 <- ipfunded_brstfd %>% 
    gather(key = subpop , value = rate, c(brst1hrp, ebfp)) %>% 
    mutate(ip = factor(ip),
           subpop = factor(subpop))
  
  levels(ipfunded_brstfd1$ip)
  levels(ipfunded_brstfd1$subpop)
  
  spprtd_brstfdprov <- ggplot(ipfunded_brstfd1, aes(x = mnthyr, y = rate, group = subpop, colour = subpop)) +
    geom_point(alpha=.6, size=.5) + 
    #geom_area(alpha=.3, size=.8, color=usaid_blue, fill=light_blue) + 
    #geom_line(size=1) +
    geom_smooth(method = loess, size = .8, se=FALSE)  +
    facet_wrap(~prov) +
    faceted +
    scale_y_continuous(limits = c(0,1),
                       labels = percent,
                       breaks = c(.2,.4,.6,.8, 1)) +
    labs(x ="", y="", caption = "") +
    ggtitle("FH Activity-supported provinces") +
    scale_color_manual(name= "", values = usaid_palette) + baseX
  
  spprtd_brstfdprov
  
  
  
  
  #'*redraw for all Non USAID-funded provinces*
  
  nonaid_brstfd <- brstfeeding_Prov %>%
    filter(ip =="non-ip")
  
  levels(brstfeeding_Prov$prov)
  
  nonaid_brstfd <- nonaid_brstfd %>% 
    gather(key = subpop , value = rate, c(brst1hrp, ebfp)) %>% 
    mutate(ip = factor(ip),
           subpop = factor(subpop))
  
  levels(nonaid_brstfd$ip)
  levels(nonaid_brstfd$subpop)

  
  nonaid_brstfdprov <- ggplot(nonaid_brstfd, aes(x = mnthyr, y = rate, group = subpop, colour = subpop)) +
    #geom_area(alpha=.3, size=.8,color=usaid_red, fill=usaid_red) + 
    geom_point(alpha=.6, size=.5) + 
    #geom_line(size=1) +
    geom_smooth(method = loess, size = .8, se=FALSE)  +
    facet_wrap(~prov) + 
    faceted +
    scale_y_continuous(limits = c(0,1),
                       labels = percent,
                       breaks = c(.2,.4,.6,.8, 1)) +
    labs(x ="", y="", caption = "Data Source: HMIS") +
    ggtitle("Non FH Activity-supported provinces") +
    scale_color_manual(name= "", values = usaid_palette, labels = c("Initiation on breastmilk with one hour of birth", "Infants on EBF at 6 months")) + basey
  
  spprtd_brstfdprov|nonaid_brstfdprov
  
  spprtd_brstfdprov + nonaid_brstfdprov + plot_layout(guides = "collect")
  
  ggsave("viz/Apr-Jun 2022/Child Health/FH Province Breastfeeding faceted smooth ns.png",
         device="png",
         type="cairo",
         height = 9,
         width = 13)
  
  