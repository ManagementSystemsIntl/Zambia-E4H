
source("scripts/r prep2.r")


malnat <- read_xls("data/Malaria/Malaria National(monthly)_2014-2022.xls")
malnat  <- malnat  %>%
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

sum(malnat$month_chr!=malnat$month) # expecting 0 if vars same



malnch <- read_xls("data/Malaria/Nchelenge(monthly)_2014-2022.xls")
malnch  <- malnch  %>%
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

sum(malnch$month_chr!=malnch$month) # expecting 0 if vars same


pnctrgts <- read_xls("data/Jan- Jun 2022/PNC Targets.xls")
Start <- as.Date(NULL)
End <- as.Date(NULL)


mal_prov <- read_xls("data/Malaria/Provincial(monthly)_2014-2022.xls")
names(mal_prov)
mal_prov
mal_prov  <- mal_prov  %>%
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

sum(mal_prov$month_chr!=mal_prov$month)


#'*Provincial Rainfall*
rnf_malprov <- read_xlsx("data/Malaria/Provincial rainfall.xlsx")
names(rnf_malprov)

# rnf1<- rnf %>%
#   group_by(Month, Year, Province) %>%
#   summarise(sm.rainfall=sum(Rainfall),
#             avg.rainfall=(mean(Rainfall))) %>%
#   mutate(date = my(paste(Month, Year, sep="-")))

rnf_malprov <- rnf_malprov %>%
  rename(prov = 1,
         mal.cases = 2,
         mnthyr = 3,
         rainfall = 4) %>%
  
  mutate(rainfallr = rainfall*250) %>% 
  
  select(prov, mal.cases, mnthyr, rainfallr) %>% 
  mutate(prov = factor(prov),
         ip = case_when(prov=="Northern" |
                          prov =="Central" |
                          prov =="Muchinga" |
                          prov =="Southern" |
                          prov =="Copperbelt" |
                          prov =="Lusaka" |
                          prov =="Western" |
                          prov =="Muchinga" |
                          prov == "Northwestern"|
                          prov =="Eastern" ~ "ip",
                        TRUE ~ "non-ip"))

table(rnf_malprov$ip, rnf_malprov$prov)
frq(rnf_malprov$ip) #sjmisc

levels(rnf_malprov$prov)

rnf_malprov1 <- rnf_malprov %>% 
 
  gather(key = subpop , value = rate, c(mal.cases, rainfallr)) %>% 
  
  mutate(ip = factor(ip),
         subpop = factor(subpop))

levels(rnf_malprov1$ip)
levels(rnf_malprov1$subpop)

ggplot(rnf_malprov1, aes(x = mnthyr, y = rate, group = subpop, colour = subpop)) +
  #geom_point(alpha=.6, size=.4) + 
  geom_smooth(method = loess, size = .6, se=F)  +
  scale_y_continuous(labels=comma,
                     limits=c(0,150000),
                     breaks = c(0,50000,100000,150000),
                     sec.axis=sec_axis(trans = ~ .*0.003, name = "Rainfall (mm)")) +
  labs(x ="", y="Confirmed Malaria Cases",
       caption = "Data Source: \nWFP - Rainfall \nHMIS - Malaria Cases") +
  facet_wrap(~prov) +faceted + basey +
  scale_color_manual(name= "", values = usaid_palette, labels = c("Confirmed Malaria Cases",
                                                                  "Rainfall(mm)")) +
  plot_layout(guides = "collect")

  ggsave("viz/Malaria/PMI April June 2022/Provincial Malaria and Rainfall No smooth.png",
       device="png",
       type="cairo",
       height = 8.5,
       width = 15)
  
  rnf_malprov
  
  library("ggpubr")
  ggscatter(rnf_malprov,x = mnthyr, y = rate, group = subpop, colour = subpop, 
            add = "reg.line", conf.int = TRUE, 
   
                   cor.coef = TRUE, cor.method = "pearson")
  
  
  
  
  
  

#'*IRS COVERAGE*
  irs_ins <- read_xls("data/Malaria/Insecticidetype_malariacases.xls")
  colnames(irs_ins) 
  
  irs_ins1 <- irs_ins %>%
  rename(mnthyr = 1,
         pop = 2,
         insct = 3,
         mal.cases = 4)
  
  irs_ins1
  irs_ins1$mnthyr <- as.Date(irs_ins1$mnthyr)
  irs_ins1
  
  # 
  # irs_ins2 <- irs_ins1 %>% 
  #   
  #   gather(key = subpop , value = rate, c(pop,mal.cases))
  # 
  # irs_ins2
  # ggplot(irs_ins2, aes(x = mnthyr, y = rate, group = subpop, colour = subpop)) +
  #   geom_point(alpha=.6, size=.4) + 
  #   geom_smooth(method = loess, size = .6, se=F)
    # scale_y_continuous(labels=comma,
    #                    limits=c(0,150000),
    #                    breaks = c(0,50000,100000,150000),
    #                    sec.axis=sec_axis(trans = ~ .*0.003, name = "Rainfall (mm)"))
    # 
    # 
  
  id_plt <- ggplot(irs_ins1) + 
    geom_bar(aes(x=mnthyr, y=Pop, fill=insct), stat="identity", position = "dodge") +
    scale_fill_manual(values=c("Actelic"="#002F6C","Fludora"="#F5B041", "Sumishield"="#BA0C2F", "Malaria Cases"="#6C6463")) +
    geom_smooth(aes(x=mnthyr, y=mal.cases/120, fill="Malaria Cases") ,color="#6C6463", size=1, se=F) + 
    geom_point(aes(x=mnthyr, y=mal.cases/1200), stat="identity",color="#6C6463",size=3) +
    scale_x_date(date_labels="%Y",date_breaks="1 year", limits = NULL)+
    scale_y_continuous(labels=comma, sec.axis = sec_axis(trans = ~ .*1200, labels=comma, name = "Malaria cases"))+
    labs(fill="Legend:", title="Malaria Cases and Insecticide Types Used Over Time - Nchelenge",
         x=" Period",
         y="Coverage(%)") + base
 
  id_plt
  
  