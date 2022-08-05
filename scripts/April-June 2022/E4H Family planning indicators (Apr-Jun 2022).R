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
       width = 11)

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
  # scale_x_date(date_labels="%y", date_breaks="1 years") +
  labs(x ="", y="", caption = "Activities being implemented \nby SUNTA in some districts") +
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
  # scale_x_date(date_labels="%Y", date_breaks="2 years") +
  labs(x ="", y="", caption = "Data Source: HMIS") +
  ggtitle("Non FH Activity-supported provinces") +
  scale_color_manual(name= "", values = usaid_red, labels = "Women of reproductive age visted by CHA") + baseX

ipNot_fundedprov

chav_fundedprov + ipNot_fundedprov + plot_layout(guides = "collect")

ggsave("viz/Apr-Jun 2022/Family Planning/Province Women of reproductive age visted by CHA faceted smooth ns.png",
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
  # scale_x_date(date_labels="%b %y",date_breaks="4 months") +
  labs(x="", y="", caption="Data Source: HMIS", title="Women in reproductive age group on a modern FP method \ndeclined in January-March 2022, continuining a trend starting in October 2020") +
  scale_color_manual(name ="",
                     values = usaid_blue,
                     labels ="Women in reproductive age group on a modern FP method") + 
  baseX

mfp_plt
ggsave("viz/Apr-Jun 2022/Family Planning/Women in reproductive age group on a modern FP method smooth ns.png",
       device="png",
       type="cairo",
       height = 6.5,
       width = 12)


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
  mutate(cvrg_fp = wmn.mfp/wmn.vstd) #%>%
  # relocate(cvrg_fp, .after=wmn.mfp)

  crvg_plt <- ggplot(fam, aes(x=mnthyr, y=cvrg_fp, colour=usaid_blue)) + 
  geom_point(alpha=.6, size=1.5) + 
  #geom_line(size=1) +
  geom_smooth(method = loess, size = .8, se=FALSE) +
    scale_y_continuous(limits = c(0,.8),
                       labels = percent,
                       breaks = c(.1,.2,.3,.4,.5,.6,.7,.8)) +
  scale_x_date(date_labels="%b %y",date_breaks="4 months") +
  labs(x="", y="", caption="Data Source: HMIS", title="Coverage of modern family planning adoption after visits by \nCHAs' has been increasing since 2018") +
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
    labs(x="", y="", caption="Data Source: HMIS", title="Number of first-time users of modern FP users was on the rise from 2018 to October 2019, \nbut are declining continuining a trend started in October 2019") +
    scale_color_manual(name ="",
                       values = usaid_blue,
                       labels ="Number of first time modern FP") + 
    baseX
  
  nwFP_plt
  ggsave("viz/Apr-Jun 2022/Family Planning/Number of first-time users of modern FP method smooth ns.png",
         device="png",
         type="cairo",
         height=7.5,
         width=12)
  

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
         height = 7.5,
         width = 12)
  
  
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
         title="Percentage of clients discontinued LARC has increased and declined \nfrom 2018 to October 2021, Since then they have been increasing") + 
    baseX
  
  larc.dis.p_plt
  
  ggsave("viz/Apr-Jun 2022/Family Planning/discontinued Long-Acting Reversible ns.png",
         device="png",
         type="cairo",
         height = 7.5,
         width = 11)
  
  
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
           title="Medroxyprogesterone injection DMPA-IM has declined \nin the last 3 months of 2022, excpet for under 15yrs") +
      basey + scale_color_manual(name ="",
                         values = usaid_palette,
                         labels = c("under 15yrs", "15-19yrs", "20-24yrs", "abover 25yrs"))
    
      medim
      
      
      ggsave("viz/Apr-Jun 2022/Family Planning/Medroxyprogesterone injection DMPA-IM ns.png",
             device="png",
             type="cairo",
             height = 5.5,
             width = 9)
    
      
      #M##edroxyprogesterone injection DMPA-SC