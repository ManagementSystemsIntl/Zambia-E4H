# Zambia 4 Health

# prep ---- 

source("scripts/r prep.R")

ch <- read_xls("data/Downlaod Extract Childhealth Monthly At National.xls")
fam <- read_xls("data/Downlaod Extract Family Planning Monthly At National.xls")
fam_prov <- read_xls("data/Downlaod Extract Family Planning Yearly At Province.xls")
mat_prov <- read_xls("data/Downlaod Extract Maternal Yearly By Province.xls") 
mat_prov
mat <- read_xls("data/Downlaod Extract Maternal Monthly At National.xls")

# family ---- 



# family provincial ---- 


# maternal ---- 

#* M ANC coverage at first trimestre  ----

mat <- mat %>%
  rename(anc1 = 4,
         anc1u20 = 5,
         anc1hr = 12) %>%
  mutate(
    anc1p = anc1/100,
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
    mnthyr = my(monyr)
  )

# To create legend, gather method for including a legend --

mat <- gather(mat, key = subpop , value = rate, c(anc1p,anc1u20p,anc1hrp)) # 

sum(mat$month_chr!=mat$month) # expecting 0 if vars same

ggplot(mat, aes(x = mnthyr, y = rate, group = subpop, colour = subpop)) +
  geom_point(alpha=.6, size=1) + 
  geom_line(size = .5) +
  geom_smooth(method = lm, size = .7, se=FALSE) +
  scale_y_continuous(limits = c(0,1),
                     labels = percent,
                     breaks = c(.1,.2,.3,.4,.5,.6,.7,.8,.9, 1)) +
  xlab("") + 
  ylab("Proportion receiving ANC at TM1") +
  ggtitle("Proportion of expected pregnancies \n receiving antenatal care at first trimester (2018-2022)") +
  scale_colour_manual(name = "",
                      labels = c("High risk pregnancies", "All women", "Women under 20"),
                      values = c(usaid_red, medium_grey, usaid_blue)) +
  theme(plot.title = element_text(size = 14), 
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text = element_text(size = 9),
        legend.title = element_text(size = 12), 
        legend.text = element_text(size = 11)
  ) 

#? legend assignment seems out of order (why would high risk preg be first label) ----

ggsave("viz/Antenatal care at first trimester.png",
       device="png",
       type="cairo",
       height=4,
       width=7)

#* F ANC care (Folic acid + Fe) ----

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

mat %>% 
  ggplot(aes(x = mnthyr, y = rate, group = vit, colour = vit)) +
  geom_point(alpha=.6, size=1) + 
  geom_smooth(se= FALSE) +
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


ggsave("viz/Folic Acid and Iron Supplementation during ANC.png",
       device="png",
       type="cairo",
       height=4,
       width=7)

#* M institutional delivery coverage ----

mat <- mat  %>%
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

sum(mat$month_chr!=mat$month) # expecting 0 if vars same

ggplot(mat, aes(x=mnthyr, y=instdelp)) + 
  geom_point(color= usaid_blue, alpha=.6, size=.8) + 
  geom_line(color= usaid_blue, alpha=.4) +
  stat_smooth(color= usaid_blue, se=F, size=1.1, alpha=.8) +
  scale_y_continuous(limits=c(0,1),
                     labels = percent,
                     breaks = c(.1,.2,.3,.4,.5,.6,.7,.8,.9, 1)) +
  labs(x="",
       y="",
       title="Proportion of expected deliveries occurring in health facilities") +
  theme(plot.title = element_text(size = 14), 
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text = element_text(size = 9),
        legend.title = element_text(size = 12), 
        legend.text = element_text(size = 11))

ggsave("viz/Institutional births.png",
       device="png",
       type="cairo",
       height=4,
       width=7)

#* FM Cesarean rate ====

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

#* F Postnatal care ====

mat  <- mat  %>%
  rename(pnc = 11) %>%
  mutate(pncp = pnc/100,
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

ggplot(mat, aes(mnthyr, pncp)) + 
  geom_point(color= usaid_blue, alpha=.6, size=.8) + 
  geom_line(color= usaid_blue, alpha=.4) +
  scale_y_continuous(limits = c(0,1),
                     labels = percent,
                     breaks = c(.1, .2, .3, .4, .5, .6, .7, .8, .9, 1)) +
  stat_smooth(method="lm", color= usaid_blue, se=F, size=1.1, alpha=.8) +
  labs(x="",
       y="",
       title="Proportion of expected deliveries receiving postnatal care within 48 hours after delivery") +
  theme(plot.title = element_text(size = 14), 
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text = element_text(size = 9),
        legend.title = element_text(size = 12), 
        legend.text = element_text(size = 11))

ggsave("viz/Postnatal care.png",
       device="png",
       type="cairo",
       height=4,
       width=7)


#* maternal mortality ratio ----
 
#? Waiting to learn from Gift which variables to use ----

mat <- mat %>%
  rename(mmfr = 16,
         mmf = 13,
         mmc = 22
         ) %>%
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

str(mat)

mat <- gather(mat, key = mmtype , value = deaths, c(mmfr, mmf, mmc)) 

ggplot(mat, aes(x = mnthyr, y = deaths, group = mmtype, colour = mmtype)) + 
  geom_point(alpha=.6, size=1) + 
  geom_line(size=.7) +
  scale_y_continuous(limits = c(0,200),
                     breaks = c(20,40,60,80,100,120,140,160,180,200),
                     labels = c("20","40","60","80","100","120","140","160","180","200")) +
  xlab("") +
  ylab("Number of maternal deaths") +
  ggtitle("Maternal deaths occurring at health facilities \nand in the community  (2018-2022)") +
  scale_colour_manual(name = "",
                    labels= c("Maternal mortality facility ratio \n(per 10 000 live births)","Health facility deaths", "Community deaths"),
                    values = c(medium_grey, usaid_red, usaid_blue)) +
  theme(plot.title = element_text(size = 14), 
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text = element_text(size = 9.5),
        legend.text = element_text(size = 10))
        
ggsave("viz/Maternal deaths.png",
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
  geom_point(color= medium_blue, alpha=.6, size= 1.2) + 
  geom_line(color= medium_blue, alpha=.6, size = .6) +
  stat_smooth(method="lm", color= medium_blue, se=F, size=1, alpha=.6) +
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

ch <- ch  %>%
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

sum(ch$month_chr!=ch$month) # expecting 0 if vars same

ggplot(ch, aes(mnthyr, neod)) + 
  geom_point(color= usaid_blue, alpha=.6, size=.8) + 
  geom_line(color= usaid_blue, alpha=.4) +
  stat_smooth(method="lm", color= usaid_blue, se=F, size=1.1, alpha=.8) +
  scale_y_continuous(limits=c(0,500)) +
  labs(x="",
       y="",
       title="Neonatal Deaths") +
  theme(plot.title = element_text(size = 14), 
      axis.title.x = element_text(size = 12),
      axis.title.y = element_text(size = 12),
      axis.text = element_text(size = 9),
      legend.title = element_text(size = 12), 
      legend.text = element_text(size = 11))
      
ggsave("viz/Neonatal deaths.png",
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





