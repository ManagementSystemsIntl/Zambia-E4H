# Zambia E4 Health
# Child health indicators
# April - June 2022

source("scripts/r prep2.r")

chldH <- read_xls("data/Jan- Jun 2022/Child Health Data_National Level(Monthly).xls")
chldH  <- chldH  %>%
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

sum(chldH$month_chr!=chldH$month) # expecting 0 if vars same

pnctrgts <- read_xls("data/Jan- Jun 2022/PNC Targets.xls")
Start <- as.Date(NULL)
End <- as.Date(NULL)


chldHq <- read_xls("data/Jan- Jun 2022/Child Health Data_National Level(Quarterly).xls")
chldHqp <- read_xls("data/Jan-Mar 2022/Child Health Data_Provincial Level(Quarterly).xls")
chldH_prov <- read_xls("data/Jan- Jun 2022/Child Health Data_Provincial Level(Monthly).xls")
names(chldH_prov)
chldH_prov
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




#'*IMMUNIZATION*

#'* MEASLES 1 & 2*
names(chldH)
# view(chldH)
chldH <- chldH %>%
  rename(msles1 = 11,
         msles2 = 12
         ) %>%
  
  mutate(msles1p = msles1/100,
         msles2p = msles2/100)

#'*set msles1p & msles2p to 100 for all values >100*
chldH <- chldH %>% 
  dplyr::mutate(ancc = ifelse(msles1 > 100, 100, msles1)) %>% 
  dplyr::mutate(msles1p = msles1/100)

#'*To create legend, gather method for including a legend --*

chldH <- gather(chldH, key = subpop , value = rate, c(msles1p, msles2p))
chldH$subpop <- factor(chldH$subpop, levels = unique(chldH$subpop)) # transform into factor
levels(chldH$subpop)

# view(chldH)

ggplot(chldH, aes(x = mnthyr, y = rate, group = subpop, colour = subpop)) +
  geom_point(alpha=.6, size=1.9) + 
  #geom_line(size=1) +
  geom_smooth(method = loess, size = .8, se=FALSE) +
  scale_y_continuous(limits = c(0,1),
                     labels = percent,
                     breaks = c(.1,.2,.3,.4,.5,.6,.7,.8,.9, 1)) +
  scale_x_date(date_labels="%b %Y",date_breaks="4 months") +
  labs(x="", y="", caption="Data Source: HMIS", title="Proportion of under 1year infants who received the vaccine has been declining since June 2021,  \nwhilest overall under 2 years trend has been steadly increasing") +
  scale_color_manual(name ="",
                     values = usaid_palette,
                     labels = c("Measle under 1", "Measeles under 2")
  ) + 
  base

ggsave("viz/Apr-Jun 2022/Child Health/Proportion of infacts receiving Measles Vaccines.png",
       device="png",
       type="cairo",
       height = 6.5,
       width = 12)


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
    geom_smooth(method = loess, size = .8, se=FALSE)  +
    facet_wrap(~prov) +
    faceted +
    scale_y_continuous(limits = c(0,1),
                       labels = percent,
                       breaks = c(.2,.4,.6,.8, 1)) +
    labs(x ="", y="", caption = "") +
    ggtitle("USAID-supported provinces") +
    scale_color_manual(name= "", values = (usaid_palette)) + baseX
  
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
    geom_smooth(method = loess, size = .8, se=FALSE)  +
    facet_wrap(~prov) +
    faceted +
    scale_y_continuous(limits = c(0,1),
                       labels = percent,
                       breaks = c(.2,.4,.6,.8, 1)) +
    labs(x ="", y="", caption = "Data Source: HMIS") +
    ggtitle("USAID-supported provinces") +
    scale_color_manual(name= "", values = (usaid_palette), labels = c("Measle under 1", "Measeles under 2")) + basey
  
  ipNot_fundedprov
  
  ip_fundedprov + ipNot_fundedprov + plot_layout(guides = "collect")
  
  ggsave("viz/Apr-Jun 2022/Child Health/Province Proportion of infacts receiving Measles Vaccines faceted ns.png",
         device="png",
         type="cairo",
         height = 6.5,
         width = 12)
  
  
  
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
  
  ggplot(bcgImz1, aes(x = mnthyr, y = bcgp, colour=usaid_blue )) +
    geom_point(alpha=.4, size=1.9) + 
    #geom_line(size=1) +
    geom_smooth(method = loess, size = .8, se=FALSE) +
    scale_y_continuous(limits = c(0,1),
                       labels = percent,
                       breaks = c(.1,.2,.3,.4,.5,.6,.7,.8,.9, 1)) +
    scale_x_date(date_labels="%b %Y",date_breaks="4 months") +
    labs(x="", y="", caption="Data Source: HMIS", title="Proportion of infants who received BCG under 1 has been on a decline trend, with slightly steady periods around June 2021 , \nthis decline trend started June 2019") +
    scale_color_manual(name ="",
                       values = usaid_blue,
                       labels = "BCG Under 1") + baseX
  
  ggsave("viz/Apr-Jun 2022/Child Health/BCG Vaccines.png",
         device="png",
         type="cairo",
         height = 6.5,
         width = 12)
  
  
  #'* BCG SEPERATED BY SUPPORTED AND NON USAID PROVINCES* ----
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
    labs(x ="", y="", caption = "") +
    ggtitle("USAID-supported provinces") +
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
    ggtitle("USAID-supported provinces") +
    scale_color_manual(name= "", values = usaid_red, labels = c("BCG Under 1")) + baseX
  
  ipNot_fundedprov
  
  ip_fundedprov + ipNot_fundedprov + plot_layout(guides = "collect")
  
  ggsave("viz/Apr-Jun 2022/Child Health/Province BCG Vaccines faceted smooth ns.png",
         device="png",
         type="cairo",
         height = 7.5,
         width = 12)
  
  
