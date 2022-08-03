# Zambia E4 Health
# Family planning indicators
# April - June 2022






source("scripts/r prep2.r")


fam <- read_xls("data/Jan- Jun 2022/Family Planning Data_National Level(Monthly).xls")
fam  <- fam  %>%
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

sum(fam$month_chr!=fam$month) # expecting 0 if vars same

pnctrgts <- read_xls("data/Jan- Jun 2022/PNC Targets.xls")
Start <- as.Date(NULL)
End <- as.Date(NULL)


famq <- read_xls("data/Jan- Jun 2022/Family Planning Data_National Level(Quarterly).xls")
fam_prov <- read_xls("data/Jan- Jun 2022/Family Planning Data_Provincial Level(Monthly).xls")
names(fam_prov)
fam_prov
fam_prov  <- fam_prov  %>%
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

sum(fam_prov$month_chr!=fam_prov$month) # expecting 0 if vars same


#'*WOMEN OF REPRODUCTIVE AGE VISITED BY CHA's*
names(fam)
fam <- fam %>%
  rename(wmn.vstd=3)

ch_plt <- ggplot(fam, aes(x=mnthyr, y=wmn.vstd, colour=usaid_blue)) + 
  geom_point(alpha=.6, size=1.5) + 
  #geom_line(size=1) +
  geom_smooth(method = loess, size = .8, se=FALSE) +
  scale_y_continuous(labels=comma) +
  scale_x_date(date_labels="%b %y",date_breaks="4 months") +
  labs(x="", y="", caption="Data Source: HMIS", title="Number of Women in reproductive age visted by CHA/CBD has declined \nin the last two years, this trend began in October 2020") +
  scale_color_manual(name ="",
                     values = usaid_blue,
                     labels ="Women of reproductive age visted by CHA") + 
  baseX

ch_plt
ggsave("viz/Apr-Jun 2022/Family Planning/Women of reproductive age visted by CHA ns.png",
       device="png",
       type="cairo",
       height = 6.5,
       width = 12)

#'* WOMEN OF REPRODUCTIVE AGE VISITED BY CHA' SEPERATED BY SUPPORTED AND NON FH-USAID PROVINCES*
#'
names(fam_prov)

chav_Prov <- fam_prov %>%
  rename(prov =2,
         wmn.vstd=3) %>%
  
  
  select(prov, mnthyr, wmn.vstd) %>% 
  mutate(prov = factor(prov),
         ip = case_when(prov=="Northern" |
                          prov =="Central" |
                          prov =="Luapula" |
                          prov =="Copperbelt" ~ "ip",
                        TRUE ~ "non-ip"))

table(chav_Prov$ip, chav_Prov$prov)
frq(chav_Prov$ip) #sjmisc

levels(chav_Prov$prov)

#'*redraw for all USAID-funded provinces*

ipfunded_chav <- chav_Prov %>%
  filter(ip =="ip")

levels(ipfunded_chav$prov)

ipfunded_chav1 <- ipfunded_chav %>% 
  gather(key = subpop , value = rate, c(wmn.vstd)) %>% 
  mutate(ip = factor(ip),
         subpop = factor(subpop))

levels(ipfunded1$ip)
levels(ipfunded1$subpop)

chav_fundedprov <- ggplot(ipfunded_chav1, aes(x = mnthyr, y = rate, group = subpop, colour = subpop)) +
  geom_point(alpha=.6, size=.6) + 
  #geom_area(alpha=.3, size=.8, color=usaid_blue, fill=light_blue) + 
  #geom_line(size=1) +
  geom_smooth(method = loess, size = .8, se=FALSE)  +
  facet_wrap(~prov) +
  faceted +
  scale_y_continuous(labels=comma,
                     limits=c(0,6000),
                     breaks = c(0, 1000,2000,3000,4000,5000,6000)) +
  labs(x ="", y="", caption = "Activities being implemented by SUNTA") +
  ggtitle("FH Activity-supported provinces") +
  scale_color_manual(name= "", values = usaid_blue) + baseX

chav_fundedprov


#'*redraw for all Non USAID-funded provinces*

ipNot_funded <- chav_Prov %>%
  filter(ip =="non-ip")

levels(chav_Prov$prov)

ipNot_funded1 <- ipNot_funded %>% 
  gather(key = subpop , value = rate, c(wmn.vstd)) %>% 
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
  scale_y_continuous(labels=comma,
                     limits=c(0,6000),
                     breaks = c(0, 1000,2000,3000,4000,5000,6000)) +
  labs(x ="", y="", caption = "Data Source: HMIS") +
  ggtitle("Non FH Activity-supported provinces") +
  scale_color_manual(name= "", values = usaid_red, labels = "Women of reproductive age visted by CHA") + baseX

ipNot_fundedprov

chav_fundedprov + ipNot_fundedprov + plot_layout(guides = "collect")

ggsave("viz/Apr-Jun 2022/Child Health/Province Women of reproductive age visted by CHA faceted smooth ns.png",
       device="png",
       type="cairo",
       height = 7.5,
       width = 12)


