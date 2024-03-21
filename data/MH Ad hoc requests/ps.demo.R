
source("scripts/r prep2.r")
source("scripts/r prep3.r")



#'*Perinatal deaths, Fresh Still & Macerated Stillbirths*
pr.mr.st <- read_xlsx("data/Aug 2023 MHDR/provincial FSB MSB_monthly.xlsx")

pr.mr.st

pr.mr.st$Month <- as.Date(pr.mr.st$Month)

pr.mr.st


pr.mr.st2 <- pr.mr.st %>%
  rename(prov=1,
         mth=2,
         frsh.stlbrth.Rt=7,
         mcrtd.brth.Rt=6)

pr.mr.st2

pr.mr.st2
frsh.stillmacerbirth <- pr.mr.st2 %>%
  select(1,2,6,7)

frsh.stillmacerbirth


frsh.stillmacerbirth <- frsh.stillmacerbirth %>% 
  gather(key = subRt , value = rate, c(frsh.stlbrth.Rt, mcrtd.brth.Rt))

frsh.stillmacerbirth

ggplot(frsh.stillmacerbirth, aes(x = mth, y = rate, group = subRt, fill = subRt), alpha=0.6) +
  geom_area(alpha=.8, position = position_dodge()) +
  scale_y_continuous(limits = c(0,8),
                     breaks = c(0,2,4,6,8)) +
  xlab("") + 
  ylab("Rate") +
  ggtitle("Fresh stillbirth and Macerated stillbirth per 1000 live births (Oct 2022 - Jul 2023.)") +
  # facet_wrap(~prov, ncol=4) +
  # faceted +
  scale_x_date(date_labels="%b %y",date_breaks="1 month") +
  scale_fill_manual(name ="",
                    values = c(usaid_red,usaid_blue),labels = c("Macerated Stillbirth","Fresh Stillbirth")) + base

ggsave("viz/Aug 23 FHDR/stillbirths Sept National.png",
       device="png",
       type="cairo",
       height = 6.5,
       width = 12)








names(fam)
newAccpt <- fam %>%
  select(24:27,56) %>%
  na.omit() 

newAccpt
colnames(newAccpt)
newAccpt1 <- newAccpt[, c(4,1,2,3,5)]
colnames(newAccpt1)
newAccpt1

newAccpt1

newAccpt2 <- newAccpt1 %>%
  select(5,1,2,3,4) %>%
  na.omit()
names(newAccpt2)


newAccpt3 <- reshape2::melt(newAccpt2, id = "mnthyr")

Accpt_plt <- ggplot(newAccpt3,aes(x=mnthyr, y=value, color=variable))+
  geom_point(alpha=.6, size=1.4) +
  geom_smooth(method =loess,se=F, size=1.1, alpha=.8) +
  scale_x_date(date_labels="%b %y",date_breaks="2 months") +
  scale_y_continuous(labels=comma) +
  labs(x="",
       y="",
       caption="Data Source: HMIS",
       title="Family Planning New Acceptors Starting FP disaggregated by Age (Jan 2020 - Sept 2023).") +
  basey + scale_color_manual(name ="",
                             values =c(light_blue,light_grey,usaid_blue, usaid_red),
                             labels = c("under 15yrs","15-19yrs","20-24yrs","above 25yrs"))
Accpt_plt


ggsave("viz/Dec 23 FHDR/New Acceptors Starting FP disaggs.png",
       device="png",
       type="cairo",
       height = 6.5,
       width = 11)






#'*FAMILY PLANNING NEW ACCEPTORS............without age disaggs*


FPNewaccpt <- read_xls("data/Dec 2023 MHDR/FP New Acceptors_National monthly.xls")
FPNewaccpt  <- FPNewaccpt  %>%
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

names(FPNewaccpt)

naccfp <- FPNewaccpt %>%
  rename(family.plan = 3,
         new.accptors = 4
  ) %>%
  
  mutate(naccfp.ab = family.plan + 
           new.accptors)

naccfp

FPA_plt <- ggplot(naccfp, aes(x=mnthyr, y=naccfp.ab)) + 
  geom_point(color= usaid_blue, alpha=.6, size=1) + 
  geom_smooth(method = loess,color= usaid_blue, se=F, size=1.1, alpha=.8) +
  scale_x_date(date_labels="%b %y",date_breaks="3 months") +
  scale_y_continuous(labels=comma,
                     limits=c(0, 70000),
                     breaks = c(5000, 10000, 15000, 20000, 25000, 30000, 35000, 40000, 45000, 50000, 55000, 60000, 65000, 70000)) +
  labs(x="",
       y="",
       caption="Data Source: HMIS",
       title="Family Planning New Acceptors Jan 2021 - Sept 2023.") + 
  baseX

FPA_plt

ggsave("viz/Dec 23 FHDR/National FP new Acceptors.png",
       device="png",
       type="cairo",
       height = 6.5,
       width = 12)














#'*COVERAGE OF MODERN FAMILY PLANNING ADOPTION Private Clinics......*

famPrivate <- read_xls("data/Dec 2023 MHDR/FP in private facilities_national monthly.xls")
famPrivate  <- famPrivate  %>%
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

sum(famPrivate$month_chr!=famPrivate$month) # expecting 0 if vars same



famPrivate_prov <- read_xls("data/Dec 2023 MHDR/FP in private facilities_provincial monthly.xls")
names(famPrivate_prov)
famPrivate_prov
famPrivate_prov  <- famPrivate_prov  %>%
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

sum(famPrivate_prov$month_chr!=famPrivate_prov$month) # expecting 0 if vars same


#'*______COVERAGE OF MODERN FAMILY PLANNING ADOPTION private clinics*

famPrivate <- famPrivate %>%
  rename(wmn.mfp=4,
         wmn.vstd=3) %>%
  mutate(cvrg_fp = wmn.mfp/wmn.vstd) #%>%
# relocate(cvrg_fp, .after=wmn.mfp)
colnames(famPrivate)
crvg_plt <- ggplot(famPrivate, aes(x=mnthyr, y=cvrg_fp, colour=usaid_blue)) + 
  geom_point(alpha=.6, size=1.5) + 
  #geom_line(size=1) +
  geom_smooth(method = loess, linewidth = .8, se=FALSE) +
  scale_y_continuous(limits = c(0,.8),
                     labels = percent,
                     breaks = c(.1,.2,.3,.4,.5,.6,.7,.8)) +
  scale_x_date(date_labels="%b %y",date_breaks="3 months") +
  labs(x="", y="", caption="Data Source: HMIS", title="Coverage of Modern Family Planning use among women of reproductive age \nin PRIVATELY OWNED FACILITIES (Jan 2020 - Dec 2023).") +
  scale_color_manual(name ="",
                     values = usaid_blue,
                     labels ="Coverage of modern family planning adoption") + 
  baseX

crvg_plt
ggsave("viz/Dec 23 FHDR/Private Coverage of modern family planning adoption.png",
       device="png",
       type="cairo",
       height = 6.5,
       width = 12)




#'*______COVERAGE OF MODERN FAMILY PLANNING ADOPTION private clinics...FACETS*



names(famPrivate_prov)
famPrivate_prov <- famPrivate_prov %>%
  rename(prov=1,
         wmn.mfp=4,
         wmn.vstd=3) %>%
  mutate(cvrg_fp = wmn.mfp/wmn.vstd)

famPrivate_prov

fp_plt_prty <- ggplot(famPrivate_prov, aes(x = mnthyr, y = cvrg_fp, colour = usaid_blue)) +
  geom_point(alpha=.9, size=1.3) +
  stat_smooth(method = loess, size=.9, se=T) + 
  scale_y_continuous(limits = c(0,1),
                     labels = percent,
                     breaks = c(.2,.4,.6,.8,1)) +
  facet_wrap(~prov) +
  faceted +
  labs(x ="", y="", caption = "Data Source: HMIS") + labs(x ="", y="", caption = "Data Source: HMIS") +
  ggtitle("Provincial Coverage of Modern Family Planning utilization in PRIVATELY OWNED FACILITIES (Jan 2020 - Dec 2023).") +
  scale_color_manual(name ="",
                     values = usaid_palette,
                     labels = c("Family Planning Utilization")
  ) + basem

fp_plt_prty

ggsave("viz/Dec 23 FHDR/private Modern FP utilization facets.png",
       device="png",
       type="cairo",
       height = 6.5,
       width = 12)







#'*CLIENTS ACCESSING LARC.........private*

names(famPrivate)

larc <- famPrivate %>%
  rename(iucd.inserted = 11,
         implant.inserted = 12
  ) %>%
  
  mutate(larc.ab = iucd.inserted + 
           implant.inserted)

lrc_plt_pvty <- ggplot(larc, aes(x=mnthyr, y=larc.ab)) + 
  geom_point(color= usaid_blue, alpha=.6, size=1) + 
  geom_smooth(method = loess,color= usaid_blue, se=F, size=1.1, alpha=.8) +
  scale_x_date(date_labels="%b %y",date_breaks="3 months") +
  scale_y_continuous(labels=comma,
                     limits=c(0, 3000),
                     breaks = c(500, 1000, 1500, 2000, 2500, 3000)) +
  labs(x="",
       y="",
       caption="Data Source: HMIS",
       title="The Number of clients accessing LARCs (implants and IUDs) in Private-owned \nfacilities has been below 1000 until July 2023 when the picture begins to improve.") + 
  baseX

lrc_plt_pvty

ggsave("viz/Dec 23 FHDR/ private clinics Accessing LARCS.png",
       device="png",
       type="cairo",
       height = 6.5,
       width = 12)





#'*____________CLIENTS ACCESSING LARC........private*

names(famPrivate_prov)

larc <- famPrivate_prov %>%
  rename(iucd.inserted = 11,
         implant.inserted = 12
  ) %>%
  
  mutate(larc.ab = iucd.inserted + 
           implant.inserted)

lrc_prov_pvty <- ggplot(larc, aes(x=mnthyr, y=larc.ab)) + 
  geom_point(color= usaid_blue, alpha=.6, size=1) + 
  geom_smooth(method = loess,color= usaid_blue, se=F, size=1.1, alpha=.8) +
  #scale_x_date(date_labels="%b %y",date_breaks="3 months") +
  facet_wrap(~prov, ncol=4) +
  faceted +
  scale_y_continuous(labels=comma,
                     limits=c(0, 2000),
                     breaks = c(500, 1000, 1500, 2000)) +
  labs(x="",
       y="",
       caption="Data Source: HMIS",
       title="The Number of clients accessing LARCs (implants and IUDs) seems to be improving in Lusaka, \nbut static in the other provinces with PRIVATELY-OWNED facility reports.") + 
  baseX

lrc_prov_pvty

ggsave("viz/Dec 23 FHDR/Accessing LARCS Facets_private.png",
       device="png",
       type="cairo",
       height = 6.5,
       width = 12)




#'*PERCENTAGE OF DISCONTINUNING LARC......private*
names(famPrivate)

larc <- famPrivate %>%
  rename(iucd.inserted = 11,
         implant.inserted = 12,
         iucd.removed = 8,
         implant.removed = 9
  ) %>%
  
  mutate(larc.dis.p = (iucd.removed + implant.removed) / (iucd.inserted + implant.inserted))


larc.dis.p_plt <- ggplot(larc, aes(x=mnthyr, y=larc.dis.p)) + 
  geom_point(color= usaid_blue, alpha=.6, size=1) + 
  geom_smooth(method =loess, color= usaid_blue, se=F, size=1.1, alpha=.8) +
  scale_x_date(date_labels="%b %y",date_breaks="3 months") +
  scale_y_continuous(limits=c(0,.5),
                     labels=percent,
                     breaks = c(.1,.2,.3,.4,.5)) +
  labs(x="",
       y="",
       caption="Data Source: HMIS",
       title="Percentage of clients discontinuing LARCs from privately-owned facilities \nhas been on a downward trend and now stands at below 20%!.") + 
  baseX

larc.dis.p_plt

ggsave("viz/Dec 23 FHDR/Discontinuing LARCS_privately..png",
       device="png",
       type="cairo",
       height = 6.5,
       width = 12)























