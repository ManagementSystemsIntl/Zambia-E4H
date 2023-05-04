
source("scripts/r prep2.r")
source("scripts/r prep3.r")




#'*1st ANC COVERAGE ALL TRIMESTERS*

anc1_dist <- read_xls("data/MC Health April 2023/FHN Ngabwe Kasempa and Mushindamo_2018 to 2022.xls")
names(anc1_dist)
anc1_dist
anc1_dist  <- anc1_dist  %>%
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


names(anc1_dist)
names(anc1_dist)
anc1_dist <- anc1_dist %>%
  rename(dist=1,
         fANC.ATm=3) %>%
  mutate(fANC.ATmP = fANC.ATm/100)
ggplot(anc1_dist, aes(x=mnthyr, y=fANC.ATmP)) + 
  geom_point(size=.8, alpha=.8, colour=usaid_blue) + 
  stat_smooth(se=F, size=.9, alpha=.6, colour=usaid_blue) +
  scale_y_continuous(limits = c(0,1),
                     labels = percent,
                     breaks = c(.2,.4,.6,.8,1)) +
  labs(x ="", y="", caption = "Data Source: HMIS") +labs(x ="", y="", caption = "Data Source: HMIS") +
  ggtitle("1st ANC Coverage (All Trimesters), 2018 - 2022") +
  facet_wrap(~dist, ncol=4) +
  faceted +
  scale_color_manual(values=usaid_blue) + basey

ggsave("viz/M&C Health Apr 2023/1st ANC ATMs Coverage faceted PS.png",
       device="png",
       type="cairo",
       height = 6.5,
       width = 11)


#'*________MATERNAL POSTNATAL CARE WITHIN 48HRS*

MatPNC_dist <- read_xls("data/MC Health April 2023/FHN Ngabwe Kasempa and Mushindamo_2018 to 2022.xls")
names(MatPNC_dist)
MatPNC_dist
MatPNC_dist  <- MatPNC_dist  %>%
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


names(MatPNC_dist)
names(MatPNC_dist)
MatPNC_dist <- MatPNC_dist %>%
  rename(dist=1,
         MatPNC=6) %>%
  mutate(MatPNCP = MatPNC/100)
ggplot(MatPNC_dist, aes(x=mnthyr, y=MatPNCP)) + 
  geom_point(size=.5, alpha=.5, colour=usaid_blue) + 
  stat_smooth(se=F, size=.8, alpha=.6, colour=usaid_blue) +
  scale_y_continuous(limits = c(0,1),
                     labels = percent,
                     breaks = c(.2,.4,.6,.8,1)) +
  labs(x ="", y="", caption = "Data Source: HMIS") +labs(x ="", y="", caption = "Data Source: HMIS") +
  ggtitle("Maternal Postnatal Care within 48 hours After Delivery, 2018 - 2022") +
  facet_wrap(~dist, ncol=4) +
  faceted +
  scale_color_manual(values=usaid_blue) + basey

ggsave("viz/M&C Health Apr 2023/District Maternal Postnatal 48 faceted PS.png",
       device="png",
       type="cairo",
       height = 6.5,
       width = 11)


#'*________INSTITUTIONAL DELIVERY COVERAGE*

InstDel_dist <- read_xls("data/MC Health April 2023/FHN Ngabwe Kasempa and Mushindamo_2018 to 2022.xls")
names(InstDel_dist)
InstDel_dist
InstDel_dist  <- InstDel_dist  %>%
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


names(InstDel_dist)
names(InstDel_dist)
InstDel_dist <- InstDel_dist %>%
  rename(dist=1,
         InstDel=5) %>%
  mutate(InstDelP = InstDel/100)
ggplot(InstDel_dist, aes(x=mnthyr, y=InstDelP)) + 
  geom_point(size=.5, alpha=.5, colour=usaid_blue) + 
  stat_smooth(se=F, size=.8, alpha=.6, colour=usaid_blue) +
  scale_y_continuous(limits = c(0,1),
                     labels = percent,
                     breaks = c(.2,.4,.6,.8,1)) +
  labs(x ="", y="", caption = "Data Source: HMIS") +labs(x ="", y="", caption = "Data Source: HMIS") +
  ggtitle("Institutional Delivery Coverage, 2018 - 2022") +
  facet_wrap(~dist, ncol=4) +
  faceted +
  scale_color_manual(values=usaid_blue) + basey

ggsave("viz/M&C Health Apr 2023/Institutional delivery coverage.png",
       device="png",
       type="cairo",
       height = 6.5,
       width = 11)


#'*Skilled Personnel as % of Institutional DEL*

sklld.persnll_dist <- read_xls("data/MC Health April 2023/FHN Ngabwe Kasempa and Mushindamo_2018 to 2022.xls")
names(sklld.persnll_dist)
sklld.persnll_dist
sklld.persnll_dist  <- sklld.persnll_dist  %>%
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


names(sklld.persnll_dist)
names(sklld.persnll_dist)
sklld.persnll_dist <- sklld.persnll_dist %>%
  rename(dist=1,
         skilled=4) %>%
  mutate(skilledP = skilled/100)
ggplot(sklld.persnll_dist, aes(x=mnthyr, y=skilledP)) + 
  geom_point(size=.5, alpha=.5, colour=usaid_blue) + 
  stat_smooth(se=F, size=.8, alpha=.6, colour=usaid_blue) +
  scale_y_continuous(limits = c(0,1),
                     labels = percent,
                     breaks = c(.2,.4,.6,.8,1)) +
  labs(x ="", y="", caption = "Data Source: HMIS") +labs(x ="", y="", caption = "Data Source: HMIS") +
  ggtitle("Skilled Personnel as a % of Institutional Delivery, 2018 -2022") +
  facet_wrap(~dist, ncol=4) +
  faceted +
  scale_color_manual(values=usaid_blue) + basey

ggsave("viz/M&C Health Apr 2023/Skilled personnell as a percentage.png",
       device="png",
       type="cairo",
       height = 6.5,
       width = 11)

