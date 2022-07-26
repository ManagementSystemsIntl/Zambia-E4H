# Zambia E4 Health
# Maternal neonatal health indicators
# April - June 2022


source("scripts/r prep.R")

#ggplot_shiny()

mat <- read_xls("data/Jan- Jun 2022/Reproductive Maternal Data_National Level(Monthly).xls")
mat  <- mat  %>%
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

sum(mat$month_chr!=mat$month) # expecting 0 if vars same

matq <- read_xls("data/Jan- Jun 2022/Reproductive Maternal Data_National Level(Quarterly).xls")
mat_prov <- read_xls("data/Jan- Jun 2022/Reproductive Maternal Data_National Level(Monthly).xls")
mat_prov  <- mat_prov  %>%
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

sum(mat_prov$month_chr!=mat_prov$month) # expecting 0 if vars same

# Maternal ---- 

#* (1) Client: ANC coverage ----

mat <- mat %>%
  rename(ancc = 3,
         anc1 = 4,
         anc1u20 = 5,
         anc1hr = 12) %>%
  mutate(anccp = ancc/100,
         anc1p = anc1/100,
         anc1u20p = anc1u20/100,
         anc1hrp = anc1hr/100)

# set anccp to 100 for all values >100
mat <- mat %>% 
  dplyr::mutate(ancc = ifelse(ancc > 100, 100, ancc)) %>% 
  dplyr::mutate(anccp = ancc/100)

# To create legend, gather method for including a legend --

mat <- gather(mat, key = subpop , value = rate, c(anccp, anc1p,anc1u20p,anc1hrp))
mat$subpop <- factor(mat$subpop, levels = unique(mat$subpop)) # transform into factor
levels(mat$subpop)

ggplot(mat, aes(x = mnthyr, y = rate, group = subpop, colour = subpop)) +
  geom_point(alpha=.6, size=1.5) + 
  geom_smooth(method = loess, size = .8, se=FALSE) +
  scale_y_continuous(limits = c(0,1),
                     labels = percent,
                     breaks = c(.1,.2,.3,.4,.5,.6,.7,.8,.9, 1)) +
  xlab("") + 
  ylab("") +
  ggtitle("Proportion of expected pregnancies receiving antenatal care (ANC), 2018-2022") +
  scale_color_manual(name ="",
                     values = usaid_palette,
                     labels = c("1st ANC, all TMs", "1st ANC at TM1", "1st ANC at TM1: Women <20 yrs", "ANC at TM1: High risk pregs")
  ) + base
  # theme(plot.title = element_text(size = 14), 
  #       axis.title.x = element_text(size = 12),
  #       axis.title.y = element_text(size = 12),
  #       axis.text = element_text(size = 9),
  #       legend.position = "bottom", 
  #       legend.text = element_text(size = 9)
  # ) 

ggsave("viz/Apr-Jun 2022/Reproductive Maternal & Neontal/Proportion of expected pregnancies receiving antenatal care ns.png",
       device="png",
       type="cairo",
       height = 5.5,
       width = 10)


#* (1.1) Client: ANC coverage PROVINCIAL ----

names(mat_prov)

anc_prov <- mat_prov %>%
  rename(prov = 1,
         anc1 = 4,
         #         anc1u20 = 5,
         anc1hr = 12) %>%
  mutate(anc1p = anc1/100,
         #         anc1u20p = anc1u20/100,
         anc1hrp = anc1hr/100,
  ) %>% 
  select(prov, mnthyr, anc1p, anc1hrp) %>% 
  #  select(prov, mnthyr, anc1p, anc1u20p, anc1hrp) %>% 
  mutate(prov = factor(prov),
         ip = case_when(prov=="Northern" |
                          prov =="Central" |
                          prov =="Muchinga" |
                          prov =="Southern" |
                          prov =="Eastern" ~ "ip",
                        TRUE ~ "non-ip"))

levels(anc_prov$prov)

anc_prov_l <- anc_prov %>% 
  gather(key = subpop , value = rate, c(anc1p, anc1hrp)) %>% 
  # gather(key = subpop , value = rate, c(anc1p,anc1u20p,anc1hrp)) %>% 
  mutate(ip = factor(ip),
         subpop = factor(subpop))

levels(anc_prov_l$ip)
levels(anc_prov_l$subpop)

ggplot(anc_prov_l, aes(x = mnthyr, y = rate, group = subpop, colour = subpop)) +
  #  geom_point(alpha=.6, size=.6) + 
  geom_smooth(method = loess, size = .7, se=FALSE)  +
  facet_wrap(~prov) +
  faceted +
  scale_y_continuous(limits = c(0,1),
                     labels = percent,
                     breaks = c(.2,.4,.6,.8, 1)) +
  labs(x ="", y="", caption = "Provinces supported by FHN, MOMENT and G2G:\nNorthern, Central, Luapula, Muchinga Southern and Eastern") +
  ggtitle("ANC coverage for high risk pregnancies has increased in the last two years independently 
          \nof USAID support., 2018-2022") +
  scale_color_manual(name= "", values = (usaid_palette), labels = c("High risk pregnancies", "All", "Pregnancies of women <20 years")) +
  theme(# plot.title = element_text(size = 12), 
    plot.title=element_markdown(),
    axis.title.x = element_text(size = 10),
    axis.title.y = element_text(size = 10),
    axis.text.x = element_text(size = 7),
    axis.text.y = element_text(size = 7),
    legend.text = element_text(size = 10),
    legend.title=element_blank(),
    legend.position=c(.75,.15),
    strip.text=element_text(size=7, family="Gill Sans Mt"),
  )

ggsave("viz/Apr-Jun 2022/Reproductive Maternal & Neontal/ANC by province faceted ns.png",
       device="png",
       type="cairo",
       height=5.5,
       width=10)

