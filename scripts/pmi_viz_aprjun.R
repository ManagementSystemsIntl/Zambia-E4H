
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
rnf <- read_xlsx("data/Malaria/Provincial rainfall.xlsx")
colnames(rnf)

rnf1<- rnf %>%
  group_by(Month, Year, Province) %>%
  summarise(sm.rainfall=sum(Rainfall),
            avg.rainfall=(mean(Rainfall))) %>%
  mutate(date = my(paste(Month, Year, sep="-")))

mal_prov
names(mal_prov)
mal_provfil <- mal_prov %>%
  select(2:3, 28) %>%
  na.omit()
mal_provfil

colnames(write_xlsx(rnf1,"data/avr.rainfall.xlsx"))
view(mal_provfil)
colnames(rnf1)
view(rnf1)

write_xlsx(rnf1,"data/avr.rainfall.xlsx")
write_xlsx(mal_provfil,"data/cases.xlsx")


?bind_rows
comb_rainMalProv <- bind_cols(rnf1,mal_provfil)
colnames(comb_rainMalProv)
view(comb_rainMalProv)

names(comb_rainMalProv)

combined_dat <- comb_rainMalProv %>%
select(3,5,8,9) %>%
  na.omit()
combined_dat
names(combined_dat)

write_xlsx(combined_dat,"data/combined_dat.xlsx")

combined_dat <- combined_dat %>%
  rename(mnth=6,
         prov =3,
         avg.rainfall=4,
         mal.cases=5) %>%
  
  select(mnth, prov,avg.rainfall, mal.cases) %>%

ipfunded_chav1 <- ipfunded_chav %>% 
  gather(key = subpop , value = rate, c(wmn.mfp,wmn.vstd)) %>% 
  mutate(ip = factor(ip),
         subpop = factor(subpop))



ggplot(rnf1, aes(x=date, y=avg.rainfall)) + 
  geom_point(size=.5, alpha=.5, colour=usaid_blue) + 
  stat_smooth(se=F, size=.8, alpha=.6, colour=usaid_blue) +
  scale_y_continuous(limits = c(0,80),
                     breaks = c(20,40,60,80)) +
  scale_x_date(date_labels="%Y",date_breaks="1 year")+
  labs(x ="", y="mm", caption = "Data Source: WFP") +labs(x ="", y="", caption = "Data Source: WFP") +
  ggtitle("Provincial Rainfall, 2018-2022") +
  facet_wrap(~Province) +
  faceted +
  scale_color_manual(values=usaid_blue) + basey


ggsave("viz/Malaria/PMI April June 2022/Provincial Rainfall.png",
       device="png",
       type="cairo",
       height = 6.5,
       width = 15)

