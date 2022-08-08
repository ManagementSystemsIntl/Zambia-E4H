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
  labs(x="", y="", caption="Data Source: HMIS", title="Number of Women in reproductive age visted by CHA/CBD has declined \nin the last two years, a trend that started in October 2020") +
  scale_color_manual(name ="",
                     values = usaid_blue,
                     labels ="Women of reproductive age visted by CHA") + 
  baseX

ch_plt
ggsave("viz/Apr-Jun 2022/Family Planning/Women of reproductive age visted by CHA ns.png",
       device="png",
       type="cairo",
       height = 5.5,
       width = 9)

#'* WOMEN OF REPRODUCTIVE AGE VISITED BY CHA' SEPERATED BY SUPPORTED AND NON FH-USAID PROVINCES*
#'
names(fam_prov)

chav_Prov <- fam_prov %>%
  rename(prov =2,
         wmn.vstd=3,
         wmn.mfp=4) %>%
  
  
  select(prov, mnthyr,wmn.mfp, wmn.vstd) %>% 
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
  gather(key = subpop , value = rate, c(wmn.mfp,wmn.vstd)) %>% 
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
  # scale_x_date(date_labels="%y", date_breaks="1 years") +
  labs(x ="", y="", caption = "Activities being implemented \nby SUNTA in some districts") +
  ggtitle("FH Activity-supported provinces") +
  scale_color_manual(name= "", values = usaid_palette) + baseX

chav_fundedprov


#'*redraw for all Non USAID-funded provinces*

ipNot_funded <- chav_Prov %>%
  filter(ip =="non-ip")

levels(chav_Prov$prov)

ipNot_funded1 <- ipNot_funded %>% 
  gather(key = subpop , value = rate, c(wmn.mfp,wmn.vstd)) %>% 
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
  # scale_x_date(date_labels="%Y", date_breaks="2 years") +
  labs(x ="", y="", caption = "Data Source: HMIS", plot.caption = element_text(hjust = 0)) +
  ggtitle("Non FH Activity-supported provinces") +
  scale_color_manual(name= "", values = usaid_palette, labels = c("Women in reproductive age group on a modern FP method",
                                                                  "Women of reproductive age visted by CHA")) + basey

ipNot_fundedprov

chav_fundedprov + ipNot_fundedprov + plot_layout(guides = "collect")

ggsave("viz/Apr-Jun 2022/Family Planning/Province visted by CHA & on Modern FP faceted smooth ns.png",
       device="png",
       type="cairo",
       height = 7.5,
       width = 15)

#'*WOMEN IN REPRODUCTIVE AGE GROUP ON A MDOERN FP*
names(fam)
fam <- fam %>%
  rename(wmn.mfp=4)

mfp_plt <- ggplot(fam, aes(x=mnthyr, y=wmn.mfp, colour=usaid_blue)) + 
  geom_point(alpha=.6, size=1.5) + 
  #geom_line(size=1) +
  geom_smooth(method = loess, size = .8, se=FALSE) +
  scale_y_continuous(labels=comma) +
  scale_x_date(date_labels="%b %y",date_breaks="4 months") +
  labs(x="", y="", caption="Data Source: HMIS", title="The number of WRA on a modern FP method increased in the last quarter, \nbut has been on the decline since October 2020") +
  scale_color_manual(name ="",
                     values = usaid_blue,
                     labels ="Women in reproductive age group on a modern FP method") + 
  baseX

mfp_plt
ggsave("viz/Apr-Jun 2022/Family Planning/Women in reproductive age group on a modern FP method smooth ns.png",
       device="png",
       type="cairo",
       height = 5.5,
       width = 9)


#'* WOMEN IN REPRODUCTIVE AGE GROUP ON A MDOERN FP VISITED BY CHA*
#'*SEPERATED BY SUPPORTED AND NON FH-USAID PROVINCES*
#'
names(fam_prov)

mfp_Prov <- fam_prov %>%
  rename(prov =2,
         wmn.mfp=4) %>%
  
  
  select(prov, mnthyr, wmn.mfp) %>% 
  mutate(prov = factor(prov),
         ip = case_when(prov=="Northern" |
                          prov =="Central" |
                          prov =="Luapula" |
                          prov =="Copperbelt" ~ "ip",
                        TRUE ~ "non-ip"))

table(mfp_Prov$ip, mfp_Prov$prov)
frq(mfp_Prov$ip) #sjmisc

levels(mfp_Prov$prov)

#'*redraw for all USAID-funded provinces*

ipfunded_mfp <- mfp_Prov %>%
  filter(ip =="ip")

levels(ipfunded_mfp$prov)

ipfunded_mfp1 <- ipfunded_mfp %>% 
  gather(key = subpop , value = rate, c(wmn.mfp)) %>% 
  mutate(ip = factor(ip),
         subpop = factor(subpop))

levels(ipfunded_mfp1$ip)
levels(ipfunded_mfp1$subpop)

mfp_fundedprov <- ggplot(ipfunded_mfp1, aes(x = mnthyr, y = rate, group = subpop, colour = subpop)) +
  geom_point(alpha=.6, size=.6) + 
  #geom_area(alpha=.3, size=.8, color=usaid_blue, fill=light_blue) + 
  #geom_line(size=1) +
  geom_smooth(method = loess, size = .8, se=FALSE)  +
  facet_wrap(~prov) +
  faceted +
  scale_y_continuous(labels=comma,
                     limits=c(0,5000),
                     breaks = c(0, 1000, 2000, 3000, 4000, 5000)) +
  labs(x ="", y="", caption = "Activities being implemented \nby SUNTA in some districts") +
  ggtitle("FH Activity-supported provinces") +
  scale_color_manual(name= "", values = usaid_blue) + baseX

mfp_fundedprov


#'*redraw for all Non USAID-funded provinces*

mfpNot_funded <- mfp_Prov %>%
  filter(ip =="non-ip")

levels(mfp_Prov$prov)

mfpNot_funded1 <- mfpNot_funded %>% 
  gather(key = subpop , value = rate, c(wmn.mfp)) %>% 
  mutate(ip = factor(ip),
         subpop = factor(subpop))

levels(mfpNot_funded1$ip)
levels(mfpNot_funded1$subpop)

mfpNot_fundedprov <- ggplot(mfpNot_funded1, aes(x = mnthyr, y = rate, group = subpop, colour = subpop)) +
  #geom_area(alpha=.3, size=.8,color=usaid_red, fill=usaid_red) + 
  geom_point(alpha=.6, size=.6) + 
  #geom_line(size=1) +
  geom_smooth(method = loess, size = .8, se=FALSE)  +
  facet_wrap(~prov) +
  faceted +
  scale_y_continuous(labels=comma,
                     limits=c(0,5000),
                     breaks = c(0,1000,2000,3000,4000,5000)) +
  labs(x ="", y="", caption = "Data Source: HMIS") +
  ggtitle("Non FH Activity-supported provinces") +
  scale_color_manual(name= "", values = usaid_red, labels = "Women in reproductive age group on a modern FP method smooth ns") + baseX

mfpNot_fundedprov

mfp_fundedprov + mfpNot_fundedprov + plot_layout(guides = "collect")

ggsave("viz/Apr-Jun 2022/Family Planning/Province Women in reproductive age group on a modern FP method smooth faceted ns.png",
       device="png",
       type="cairo",
       height = 7.5,
       width = 15)

#'*COVERAGE OF MODERN FAMILY PLANNING ADOPTION*

names(fam)
fam <- fam %>%
  rename(wmn.mfp=4,
         wmn.vstd=3) %>%
  mutate(cvrg_fp = wmn.mfp/wmn.vstd) #%>%
  # relocate(cvrg_fp, .after=wmn.mfp)
colnames(fam)
  crvg_plt <- ggplot(fam, aes(x=mnthyr, y=cvrg_fp, colour=usaid_blue)) + 
  geom_point(alpha=.6, size=1.5) + 
  #geom_line(size=1) +
  geom_smooth(method = loess, size = .8, se=FALSE) +
    scale_y_continuous(limits = c(0,.8),
                       labels = percent,
                       breaks = c(.1,.2,.3,.4,.5,.6,.7,.8)) +
  scale_x_date(date_labels="%b %y",date_breaks="4 months") +
  labs(x="", y="", caption="Data Source: HMIS", title="Coverage of modern family planning use among women of reproductive \nage has been increasing since 2018") +
  scale_color_manual(name ="",
                     values = usaid_blue,
                     labels ="Coverage of modern family planning adoption") + 
  baseX
  
  crvg_plt
  ggsave("viz/Apr-Jun 2022/Family Planning/Coverage of modern family planning adoption smooth ns.png",
       device="png",
       type="cairo",
       height = 6.5,
       width = 11)
  

  #'*NUMBER OF FIRST TIME USERS OF MODERN CONTRACEPTION*
  
  names(fam)
  fam <- fam %>%
    rename(nw.users=19) %>%
  mutate(nw.usersp = nw.users/100)
  
  #'*set nw.usersp to 100 for all values >100*
  fam <- fam %>% 
    dplyr::mutate(nw.users = ifelse(nw.users > 100, 100, nw.users)) %>% 
    dplyr::mutate(nw.usersp = nw.users/100)
  
  
  nwFP_plt <- ggplot(fam, aes(x=mnthyr, y=nw.usersp, colour=usaid_blue)) + 
    geom_point(alpha=.6, size=1.5) + 
    #geom_line(size=1) +
    geom_smooth(method = loess, size = .8, se=FALSE) +
    scale_y_continuous(limits = c(0,.5),
                       labels = percent,
                       breaks = c(.1,.2,.3,.4,.5)) +
    scale_x_date(date_labels="%b %y",date_breaks="4 months") +
    labs(x="", y="", caption="Data Source: HMIS", title="Number of first-time users of modern FP users was on the rise from \n2018 to October 2019, but are declining continuining a trend started in October 2019") +
    scale_color_manual(name ="",
                       values = usaid_blue,
                       labels ="Number of first time modern FP") + 
    baseX
  
  nwFP_plt
  ggsave("viz/Apr-Jun 2022/Family Planning/Number of first-time users of modern FP method smooth ns.png",
         device="png",
         type="cairo",
         height=5.5,
         width=9)
  

  #'*SEPERATED BY FH ACTIVITY SUPPORTED PROVINCES*
    names(fam_prov)
  
  nw.users_Prov <- fam_prov %>%
    rename(prov =2,
           nw.users=19) %>%
    
    mutate(nw.usersp = nw.users/100) %>%
  
  #'*set nw.usersp to 100 for all values >100*
  # fam <- fam %>% 
  #   dplyr::mutate(nw.users = ifelse(nw.users > 100, 100, nw.users)) %>% 
  #   dplyr::mutate(nw.usersp = nw.users/100)
  #   
    select(prov, mnthyr, nw.usersp) %>% 
    mutate(prov = factor(prov),
           ip = case_when(prov=="Southern" |
                            prov =="Central" |
                            prov =="Lusaka" |
                            prov =="Copperbelt" ~ "ip",
                          TRUE ~ "non-ip"))
  
  table(nw.users_Prov$ip, nw.users_Prov$prov)
  frq(nw.users_Prov$ip) #sjmisc
  
  levels(nw.users_Prov$prov)
  
  #'*redraw for all USAID-funded provinces*
  
  ipfunded_nw.users <- nw.users_Prov %>%
    filter(ip =="ip")
  
  levels(ipfunded_nw.users$prov)
  
  ipfunded_nw.users1 <- ipfunded_nw.users %>% 
    gather(key = subpop , value = rate, c(nw.usersp)) %>% 
    mutate(ip = factor(ip),
           subpop = factor(subpop))
  
  levels(ipfunded_nw.users1$ip)
  levels(ipfunded_nw.users1$subpop)
  
  nw.users_fundedprov <- ggplot(ipfunded_nw.users1, aes(x = mnthyr, y = rate, group = subpop, colour = subpop)) +
    geom_point(alpha=.6, size=1.5) + 
    #geom_area(alpha=.3, size=.8, color=usaid_blue, fill=light_blue) + 
    #geom_line(size=1) +
    geom_smooth(method = loess, size = .8, se=FALSE)  +
    facet_wrap(~prov) +
    faceted +
    scale_y_continuous(limits = c(0,.5),
                       labels = percent,
                       breaks = c(.1,.2,.3,.4,.5)) +
    labs(x ="", y="", caption = "Activities being implemented \nby ZAM-Health (only operating in the private sector)") +
    ggtitle("FH Activity-supported provinces") +
    scale_color_manual(name= "", values = usaid_blue) + baseX
  
  nw.users_fundedprov
  
  
  #'*redraw for all Non USAID-funded provinces*
  
  mfpNot_funded_nw.users <- nw.users_Prov %>%
    filter(ip =="non-ip")
  
  levels(nw.users_Prov$prov)
  
  mfpNot_funded_nw.users1 <- mfpNot_funded_nw.users %>% 
    gather(key = subpop , value = rate, c(nw.usersp)) %>% 
    mutate(ip = factor(ip),
           subpop = factor(subpop))
  
  levels(mfpNot_funded_nw.users1$ip)
  levels(mfpNot_funded_nw.users1$subpop)
  
  nw.usersNot_fundedprov <- ggplot(mfpNot_funded_nw.users1, aes(x = mnthyr, y = rate, group = subpop, colour = subpop)) +
    #geom_area(alpha=.3, size=.8,color=usaid_red, fill=usaid_red) + 
    geom_point(alpha=.6, size=.6) + 
    #geom_line(size=1) +
    geom_smooth(method = loess, size = .8, se=FALSE)  +
    facet_wrap(~prov) +
    faceted +
    scale_y_continuous(limits = c(0,.5),
                       labels = percent,
                       breaks = c(.1,.2,.3,.4,.5)) +
    labs(x ="", y="", caption = "Data Source: HMIS") +
    ggtitle("Non FH Activity-supported provinces") +
    scale_color_manual(name= "", values = usaid_red, labels = "Number of first-time users of modern FP") + baseX
  
  nw.usersNot_fundedprov
  
  nw.users_fundedprov + nw.usersNot_fundedprov + plot_layout(guides = "collect")
  
  ggsave("viz/Apr-Jun 2022/Family Planning/Province Number of first-time users of modern FP smooth faceted ns.png",
         device="png",
         type="cairo",
         height = 7.5,
         width = 15)
  
  
  
  #'*CLIENTS ACCESSING LARC*
  
  names(fam)
  
  larc <- fam %>%
    rename(iucd.inserted = 11,
           implant.inserted = 12
           ) %>%
    
    mutate(larc.ab = iucd.inserted + 
             implant.inserted)
  
  lrc_plt <- ggplot(larc, aes(x=mnthyr, y=larc.ab)) + 
    geom_point(color= usaid_blue, alpha=.6, size=1) + 
    geom_smooth(method = loess,color= usaid_blue, se=F, size=1.1, alpha=.8) +
    scale_x_date(date_labels="%b %y",date_breaks="4 months") +
    scale_y_continuous(labels=comma,
                       limits=c(0, 40000),
                       breaks = c(5000, 10000, 15000, 20000, 25000, 30000, 35000, 40000)) +
    labs(x="",
         y="", 
         title="Number of clients accessing LARCs (implants and IUDs) has increased \nsince 2018, though variance has also increased in the last two years") + 
    baseX
  
  lrc_plt
    
  ggsave("viz/Apr-Jun 2022/Family Planning/LARCS ns.png",
         device="png",
         type="cairo",
         height = 5.5,
         width = 9)
  
  
  #'*PERCENTAGE OF DISCONTINUNING LARC*
  names(fam)
  
  larc <- fam %>%
    rename(iucd.inserted = 11,
           implant.inserted = 12,
           iucd.removed = 8,
           implant.removed = 9
    ) %>%
    
    mutate(larc.dis.p = (iucd.removed + implant.removed) / (iucd.inserted + implant.inserted))

  
  larc.dis.p_plt <- ggplot(larc, aes(x=mnthyr, y=larc.dis.p)) + 
    geom_point(color= usaid_blue, alpha=.6, size=1) + 
    geom_smooth(method =loess, color= usaid_blue, se=F, size=1.1, alpha=.8) +
    scale_x_date(date_labels="%b %y",date_breaks="4 months") +
    scale_y_continuous(limits=c(0,.5),
                       labels=percent,
                       breaks = c(.1,.2,.3,.4,.5)) +
    labs(x="",
         y="", 
         title="Percentage of clients discontinuing LARC declined from \n2019 to mid-2021 and has since increased") + 
    baseX
  
  larc.dis.p_plt
  
  ggsave("viz/Apr-Jun 2022/Family Planning/discontinued Long-Acting Reversible ns.png",
         device="png",
         type="cairo",
         height = 5.5,
         width = 9)
  
  #'*BY PROVINCE*
  names(fam_prov)
  
  larc_prov <- fam_prov %>%
    rename(prov=2,
           iucd.inserted = 11,
           implant.inserted = 12,
           iucd.removed = 8,
           implant.removed = 9
    ) %>%
        
    mutate(larc.p = (iucd.removed + implant.removed) / (iucd.inserted + implant.inserted)) %>%
    
    select(prov, mnthyr, larc.p) %>% 

  
    mutate(prov = factor(prov),
           ip = case_when(prov=="Copperbelt" |
                            prov =="Central" |
                            prov =="Luapula" |
                            prov =="Muchinga" |
                            prov =="Northern" |
                            prov =="Northwestern" |
                            prov =="Copperbelt" ~ "ip",
                          TRUE ~ "non-ip"))
  
  table(larc_prov$ip, larc_prov$prov)
  frq(larc_prov$ip) #sjmisc
  
  levels(larc_prov$prov)
  
  #'*redraw for all USAID-funded provinces*
  
  larc_prov_spprtd <- larc_prov %>%
    filter(ip =="ip")
  
  levels(larc_prov_spprtd$prov)
  
  larc_prov_spprtd1 <- larc_prov_spprtd %>% 
    gather(key = subpop , value = rate, c(larc.p)) %>% 
    mutate(ip = factor(ip),
           subpop = factor(subpop))
  
  levels(larc_prov_spprtd1$ip)
  levels(larc_prov_spprtd1$subpop)
  
  larc.p_fundedprov <- ggplot(larc_prov_spprtd1, aes(x = mnthyr, y = rate, group = subpop, colour = subpop)) +
    geom_point(alpha=.6, size=1.5) + 
    #geom_area(alpha=.3, size=.8, color=usaid_blue, fill=light_blue) + 
    #geom_line(size=1) +
    geom_smooth(method = loess, size = .8, se=FALSE)  +
    facet_wrap(~prov) +
    faceted +
    scale_y_continuous(limits = c(0,.9),
                       labels = percent,
                       breaks = c(.1,.2,.3,.4,.5,.6,.7,.8,.9)) +
    labs(x ="", y="", caption = "") +
    ggtitle("USAID-supported provinces") +
    scale_color_manual(name= "", values = usaid_blue) + baseX
  
  larc.p_fundedprov
  
  
  #'*redraw for all Non USAID-funded provinces*
  
  mfpNot_funded_larc <- larc_prov %>%
    filter(ip =="non-ip")
  
  levels(larc_prov$prov)
  
  mfpNot_funded_larc1 <- mfpNot_funded_larc %>% 
    gather(key = subpop , value = rate, c(larc.p)) %>% 
    mutate(ip = factor(ip),
           subpop = factor(subpop))
  
  levels(mfpNot_funded_larc1$ip)
  levels(mfpNot_funded_larc1$subpop)
  
  larc.p_Notfundedprov <- ggplot(mfpNot_funded_larc1, aes(x = mnthyr, y = rate, group = subpop, colour = subpop)) +
    geom_point(alpha=.6, size=1.5) + 
    #geom_area(alpha=.3, size=.8, color=usaid_blue, fill=light_blue) + 
    #geom_line(size=1) +
    geom_smooth(method = loess, size = .8, se=FALSE)  +
    facet_wrap(~prov) +
    faceted +
    scale_y_continuous(limits = c(0,.9),
                       labels = percent,
                       breaks = c(.1,.2,.3,.4,.5,.6,.7,.8,.9)) +
    labs(x ="", y="", caption = "") +
    ggtitle("NON USAID-supported provinces") +
    scale_color_manual(name= "", values = usaid_blue) + baseX
  
  larc.p_Notfundedprov
  
  all_Larcp.prov <- larc.p_fundedprov + larc.p_Notfundedprov + plot_layout(guides = "collect")
  
  all_Larcp.prov
  
  ggsave("viz/Apr-Jun 2022/Family Planning/Province faceted smooth discontinued Long-Acting Reversible ns.png",
         plot = all_Larcp.prov,
         device="png",
         type="cairo",
         height=7,
         width=12)
  
  #'*MODERN FP METHOD: ALL METHODS SHORT & LONG-ACTING METHODS*

  ###Medroxyprogesterone injection DMPA-IM
  
    names(fam)
    # view(fam)
    iud <- fam %>%
      select(40:43,56) %>%
      na.omit() 
    
    iud
    colnames(iud)
    iud1 <- iud[, c(4,1,2,3,5)]
    colnames(iud1)
    iud1
    
    iud1
    
    iud2 <- iud1 %>%
      select(4,1,2,3,5) %>%
      na.omit()
    names(iud2)
    
      
    iud3 <- melt(iud2, id = "mnthyr")
    
    medim <- ggplot(iud3,aes(x=mnthyr, y=value, color=variable))+
      geom_point(alpha=.6, size=1.4) +
      geom_smooth(method =loess,se=F, size=1.1, alpha=.8) +
      scale_x_date(date_labels="%b %y",date_breaks="2 months") +
      scale_y_continuous(labels=comma) +
      labs(x="",
           y="",
           title="Medroxyprogesterone injection DMPA-IM ultilisation has been constant \nsince May 2021 except for the last quarter") +
      basey + scale_color_manual(name ="",
                         values = usaid_palette,
                         labels = c("under 15yrs", "15-19yrs", "20-24yrs", "abover 25yrs"))
    
      medim
      
      
      ggsave("viz/Apr-Jun 2022/Family Planning/Medroxyprogesterone injection DMPA-IM ns.png",
             device="png",
             type="cairo",
             height = 5.5,
             width = 9)
    
      
      #'*Medroxyprogesterone injection DMPA-SC*
      
      names(fam)
      # view(fam)
      iucd <- fam %>%
        select(44:47,56) %>%
        na.omit() 
      
      iucd
      colnames(iucd)
      iucd1 <- iucd[, c(4,1,2,3,5)]
      colnames(iucd1)
      iucd1
      
      iucd1
      
      iucd2 <- iucd1 %>%
        select(4,1,2,3,5) %>%
        na.omit()
      names(iud2)
      
      
      iucd3 <- melt(iucd2, id = "mnthyr")
      
      medim <- ggplot(iucd3,aes(x=mnthyr, y=value, color=variable))+
        geom_point(alpha=.6, size=1.4) +
        geom_smooth(method =loess,se=F, size=1.1, alpha=.8) +
        scale_x_date(date_labels="%b %y",date_breaks="2 months") +
        scale_y_continuous(labels=comma) +
        labs(x="",
             y="",
             title="Medroxyprogesterone injection DMPA-SC ultilisation has been steadly increasing,  \nexcept for the 15-19yrs that has been constant since July 2021") +
        basey + scale_color_manual(name ="",
                                   values = usaid_palette,
                                   labels = c("under 15yrs", "15-19yrs", "20-24yrs", "abover 25yrs"))
      
      medim
      
      
      ggsave("viz/Apr-Jun 2022/Family Planning/Medroxyprogesterone injection DMPA-SC ns.png",
             device="png",
             type="cairo",
             height = 5.5,
             width = 9)
      