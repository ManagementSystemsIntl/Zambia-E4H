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

#* maternal mortality ratio ----

mat <- mat %>%
  rename(anc1 = 4,
         anc1u20 = 5,
         anc1hr = 12) %>%
  mutate(anc1p = anc1/100,
         anc1u20p = anc1u20/100,
         anc1hrp = anc1hr/100,
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

frq(mat$month) 
frq(mat$anc1) 

sum(mat$month_chr!=mat$month) # expecting 0 if vars same

ggplot(mat, aes(x = mnthyr)) + 
  geom_point(aes(y = anc1p), color="slateblue4", alpha=.6, size=.8) + 
  geom_line(aes(y = anc1p), color="slateblue4", alpha=.4) +
  #  stat_smooth(aes(y = anc1p), method="lm", color="slateblue4", se=F, size=.5, alpha=.8) +
  geom_point(aes(y = anc1u20p), color= "orchid3", alpha=.6, size=.8) + 
  geom_line(aes(y = anc1u20p), color= "orchid3", alpha=.4) +
  #  stat_smooth(aes(y = anc1u20p), method="lm", color="orchid3", se=F, size=.5, alpha=.8) +
  geom_point(aes(y = anc1hrp), color= "red3", alpha=.6, size=.8) + 
  geom_line(aes(y = anc1hrp), color= "red3", alpha=.4) +
  #  stat_smooth(aes(y = anc1hrp), method="lm", color="red3", se=F, size=.5, alpha=.8) +
  scale_y_continuous(limits=c(0,.5),
                     labels=percent) +
      labs(x="",
       y="",
       title="Antenatal care coverage at 1st trimestre (blue) and among women under 20 years of age (red)")



#* ANC coverage at first trimestre  ----

names(mat)

mat <- mat %>%
  rename(anc1 = 4,
         anc1u20 = 5,
         anc1hr = 12) %>%
  mutate(anc1p = anc1/100,
         anc1u20p = anc1u20/100,
         anc1hrp = anc1hr/100,
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

frq(mat$month) 
frq(mat$anc1) 

sum(mat$month_chr!=mat$month) # expecting 0 if vars same

ggplot(mat, aes(x = mnthyr)) + 
   geom_point(aes(y = anc1p), color="slateblue4", alpha=.6, size=.8) + 
   geom_line(aes(y = anc1p), color="slateblue4", alpha=.4) +
#  stat_smooth(aes(y = anc1p), method="lm", color="slateblue4", se=F, size=.5, alpha=.8) +
   geom_point(aes(y = anc1u20p), color= "orchid3", alpha=.6, size=.8) + 
   geom_line(aes(y = anc1u20p), color= "orchid3", alpha=.4) +
#  stat_smooth(aes(y = anc1u20p), method="lm", color="orchid3", se=F, size=.5, alpha=.8) +
  geom_point(aes(y = anc1hrp), color= "red3", alpha=.6, size=.8) + 
  geom_line(aes(y = anc1hrp), color= "red3", alpha=.4) +
#  stat_smooth(aes(y = anc1hrp), method="lm", color="red3", se=F, size=.5, alpha=.8) +
  scale_y_continuous(limits=c(0,1.5),
                     labels=percent) +
  labs(x="",
       y="",
       title="Antenatal care coverage at 1st trimestre (blue), among women under 20 (pink),\n and among high risk pregnancies (red)")

ggsave("viz/ANC at TM1.png",
       device="png",
       type="cairo",
       height=4,
       width=7)


# ANC care

mat <- mat %>%
  rename(folic = 6,
         fe = 7) %>%
  mutate(folicp = folic/100,
         fep = fe/100,
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

# look for examples of syntax for loop and conditionals

mat <- mat %>%
  for (folic in mat) {
    if (folic > 100) {
      mutate(folic = 100) 
    }
  }


frq(mat$month) 
frq(mat$anc1) 

sum(mat$month_chr!=mat$month) # expecting 0 if vars same

ggplot(mat, aes(x = mnthyr)) + 
  geom_point(aes(y = folicp), color= "slateblue4", alpha=.6, size=.8) + 
  geom_line(aes(y = folicp), color= "slateblue4", alpha=.4) +
  stat_smooth(aes(y = folicp), method="lm", color="slateblue4", se=F, size=.5, alpha=.8) +
  geom_point(aes(y = fep), color= "seagreen3", alpha=.6, size=.8) + 
  geom_line(aes(y = fep), color= "seagreen3", alpha=.4) +
  stat_smooth(aes(y = fep), method="lm", color= "seagreen3", se=F, size=.5, alpha=.8) +
  scale_y_continuous(limits=c(0,1.5),
                     labels=percent) +
  labs(x="",
       y="",
       title="ANC quality: Folic Acid ")

ggsave("viz/ANC at TM1.png",
       device="png",
       type="cairo",
       height=4,
       width=7)

# maternal provincial ---- 

names(fam_prov)

fam_prov <- fam_prov %>%
  rename(inst_deliv = 8)

frq(fam_prov$inst_deliv)


# child health ---- 

names(ch)

#* total perinatal deaths ----

ch <- ch %>%
  rename(neod = 6) %>%
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

frq(ch$month) 
frq(ch$neod) 

sum(ch$month_chr!=ch$month) # expecting 0 if vars same

ggplot(ch, aes(mnthyr, neod)) + 
  geom_point(color="dodgerblue", alpha=.6, size=.8) + 
  geom_line(color="dodgerblue", alpha=.4) +
  stat_smooth(method="lm", color="dodgerblue", se=F, size=1.2, alpha=.8) +
  scale_y_continuous(limits=c(0,500)) +
  labs(x="",
       y="",
       title="Neonatal Deaths")

ggsave("viz/neonatal deaths.png",
       device="png",
       type="cairo",
       height=4,
       width=7)


#* neonatal deaths ----

mat_prov <- mat_prov %>%
  rename(neod = 14) %>%
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

frq(ch$month) 
frq(ch$neod) 

sum(ch$month_chr!=ch$month) # expecting 0 if vars same

ggplot(ch, aes(mnthyr, neod)) + 
  geom_point(color="dodgerblue", alpha=.6, size=.8) + 
  geom_line(color="dodgerblue", alpha=.4) +
  stat_smooth(method="lm", color="dodgerblue", se=F, size=1.2, alpha=.8) +
  scale_y_continuous(limits=c(0,500)) +
  labs(x="",
       y="",
       title="Neonatal Deaths")

ggsave("viz/neonatal deaths from maternal dataset.png",
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





