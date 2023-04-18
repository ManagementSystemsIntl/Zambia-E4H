
source("scripts/r prep2.r")
source("scripts/r prep3.r")


# install.packages("libwgeom")

# install.packages("libwgeom")
# 
# install.packages("libwgeom", lib = "C:/R/R-4.2.2/library")
# 
# # 
# # install.packages("here")
devtools::install_github("talgalili/d3heatmap")
# remotes::install_gitlab("dickoa/rgeoboundaries")
# # install.packages("remotes")
# remotes::install_gitlab("dickoa/rgeoboundaries")
# remotes::install_github("wmgeolab/rgeoboundaries")
# # install.packages("remotes")
# remotes::install_github("Nowosad/rcartocolor")


# install.packages("ps")
# 
# 
# library(rgeoboundaries)
# library(sf)
# library(rcartocolor)

perinatal.mort <- read_xlsx("data/prematurity/perinatal mortality rate.xlsx")
perinatal.mort  <- perinatal.mort  %>%
  mutate(year = str_sub(period,
                        start=nchar(period)-4,
                        end=nchar(period)))
perinatal.mort

perinatal.mort1 <- perinatal.mort %>%
  select(1,2,4) %>%
  na.omit()

perinatal.mort2 <- perinatal.mort1 %>%
  rename(prov =1,
         peri.mr=2,
         yr=3)

perinatal.mort2

perinatal.mort3 <- perinatal.mort2 %>% 
  gather(key = subRt , value = rate, c(peri.mr))

perinatal.mort3

zam.boundary <- geoboundaries(country = "Zambia"
                              , adm_lvl = 1) %>% 
  select(shapeName)

zam.boundary

#write_xlsx(zam.boundary,"data/prematurity/province.xlsx")

zam.boundary1 <- zam.boundary %>%
  select(1, 2) %>%
  na.omit()

zam.boundary1


map_colors <- carto_pal(name = "Burg")


perinatal.mort4 <- perinatal.mort3 %>%
  group_by(yr,prov, subRt)


perinatal.mort4

perinatal.mort5 <- left_join(perinatal.mort4
                      , zam.boundary1
                      , by = c("prov" = "shapeName")) %>%
  sf::st_as_sf()

perinatal.mort5


ggplot(perinatal.mort5, aes(geometry = geometry, fill = rate)) +
  geom_sf()+
  geom_sf_text(aes(label = prov), size = 3) +
  facet_wrap(~yr) +
  scale_fill_carto_c(name="Proportion of\n Mortality Rate"
                     , palette = "Burg") +
  labs(x="", y="", caption = "Data Source: PDSR",
       title = "Perinatal Mortality Rate, 2017-2022"
       , subtitle = "Darker colors represent a higher proportion of mortality rate") + #for faceted and xy labels include x="Longitude", y="Latitude", +faceted
  theme_void() +
  theme(plot.title.position = "plot",
        plot.title = element_text(size = 16, hjust=0.5, family="Gill Sans Mt", face="bold"),
        plot.subtitle = element_text(size = 12, hjust=0.5),
        plot.caption = element_text(size=11),
        # axis.title.x = element_text(size = 12, family="Gill Sans Mt", face="bold"),
        # axis.title.y = element_text(size = 12, family="Gill Sans Mt", face="bold"),
        # axis.text.x = element_text(size = 8),
        # axis.text.y = element_text(size = 10),
        legend.text = element_text(size = 12),
        legend.title=element_blank(),
        legend.position="right",
        strip.text=element_text(size=14, family="Gill Sans Mt"))

##interactive map  
# zam.boundary2 %>%
#   leaflet() %>%
#   addTiles() %>%
#   addPolygons(label = zam.boundary2$shapeName)

ggsave("viz/prematurity/perinatal_mortality_rate.png",
       device="png",
       type="cairo",
       height = 6.5,
       width = 15)

#'*Prematurity rate*
prema.rate <- read_xlsx("data/prematurity/prematurity rate.xlsx")

prema.rate$period <- as.Date(prema.rate$period)

prema.rate

prema.rate2 <- prema.rate %>%
  rename( prema.rate = 2) %>%
  mutate(prema.rate.prt = prema.rate/100)



prema.rate2 <- prema.rate2 %>%
  select(1,3)

prema.rate2

prema.rate3 <- prema.rate2 %>% 
  gather(key = subRt , value = rate, c(prema.rate.prt))

prema.rate3

ggplot(prema.rate3, aes(x = period, y = rate, group = subRt, colour = subRt)) +
  geom_point(alpha=.6, size=1.9) + 
  #geom_line(size=1) +
  geom_smooth(method = loess, size = .8, se=FALSE) +
  scale_y_continuous(limits = c(0,1),
                     labels = percent,
                     breaks = c(.1,.2,.3,.4,.5,.6,.7,.8,.9, 1)) +
  scale_x_date(date_labels="%b %y",date_breaks="4 months") +
  xlab("") + 
  ylab("") +
  ggtitle("Prematurity Rate , Sept 2017-Oct 2022") +
  scale_color_manual(name ="",
                     values = usaid_red) + 
  baseX

ggsave("viz/prematurity/prematurity_rate.png",
       device="png",
       type="cairo",
       height = 5.5,
       width = 12)


#'*Perinatal deaths, Fresh Still & Macerated Stillbirths*
pr.mr.st <- read_xlsx("data/prematurity/perinatal & stillbirths.xlsx")

pr.mr.st

pr.mr.st$Month <- as.Date(pr.mr.st$Month)

pr.mr.st


pr.mr.st2 <- pr.mr.st %>%
  rename(mth =1,
         peri.deaths=2,
         peri.Rt=3,
         frsh.stlbrth.Rt=4,
         mcrtd.brth.Rt=5)

pr.mr.st2

#'*Perinatal deaths*
perinatal.deaths <- pr.mr.st2 %>%
  select(1,2)

perinatal.deaths

perinatal.deaths <- perinatal.deaths %>% 
  gather(key = subRt , value = rate, c(peri.deaths))

perinatal.deaths

ggplot(perinatal.deaths, aes(x = mth, y = rate, group = subRt, colour = subRt)) +
  geom_point(alpha=.6, size=1.9) + 
  #geom_line(size=1) +
  geom_smooth(method = loess, size = .8, se=FALSE) +
  # scale_y_continuous(limits = c(0,1),
  #                    labels = percent,
  #                    breaks = c(.1,.2,.3,.4,.5,.6,.7,.8,.9, 1)) +
  scale_x_date(date_labels="%b %y",date_breaks="3 months") +
  xlab("") + 
  ylab("Deaths") +
  ggtitle("Perinatal deaths ,Jan 2019 - Oct 2022") +
  scale_color_manual(name ="",
                     values = usaid_red) + 
  basey

ggsave("viz/prematurity/perinatal deaths.png",
       device="png",
       type="cairo",
       height = 5.5,
       width = 11)

#'*Stillbirths*
pr.mr.st2
frsh.stillmacerbirth <- pr.mr.st2 %>%
  select(1,4,5)

frsh.stillmacerbirth


frsh.stillmacerbirth <- frsh.stillmacerbirth %>% 
  gather(key = subRt , value = rate, c(frsh.stlbrth.Rt, mcrtd.brth.Rt))

frsh.stillmacerbirth

# pr.mr.st23 <- pr.mr.st %>%
#   select(1,4,5)
# 
# pr.mr.st23
# 
# pr.mr.st24 <- melt(pr.mr.st23[c(1, 2, 3)], id = 'Month')
# 
# pr.mr.st24
# 
# 
# fmsb <- ggplot(pr.mr.st24, aes(x=Month, y=value , fill=variable), alpha=0.6)+ 
#   geom_area(position=position_dodge(), color="#CFCDC9") +
#   geom_area(position = position_dodge()) +
#   scale_y_continuous(limits = c(0,8),
#                      breaks = c(0,2,4,6,8)) +
#   xlab("") + 
#   ylab("Rate") +
#   ggtitle("Fresh stillbirth and Macerated stillbirth per 1000 live births ,Jan 2019 - Oct 2022") +
#   scale_x_date(date_labels="%b %y",date_breaks="3 months") +
#   scale_fill_manual(name ="",
#                     values = c(usaid_red,usaid_blue),labels = c("Fresh Stillbirth", "Macerated Stillbirth")) + base
# 
# fmsb
# 
# fmsb <- ggplot(pr.mr.st, aes(x=period, y=value , fill=variable), alpha=0.6)+
#   geom_area(position=position_dodge(), color="#CFCDC9")

ggplot(frsh.stillmacerbirth, aes(x = mth, y = rate, group = subRt, fill = subRt), alpha=0.6) +
  geom_area(alpha=.8, position = position_dodge()) +
  scale_y_continuous(limits = c(0,8),
                     breaks = c(0,2,4,6,8)) +
  xlab("") + 
  ylab("Rate") +
  ggtitle("Fresh stillbirth and Macerated stillbirth per 1000 live births ,Jan 2019 - Oct 2022") +
  scale_x_date(date_labels="%b %y",date_breaks="3 months") +
  scale_fill_manual(name ="",
                     values = c(usaid_red,usaid_blue),labels = c("Macerated Stillbirth","Fresh Stillbirth")) + base

ggsave("viz/prematurity/stillbirths.png",
       device="png",
       type="cairo",
       height = 5.0,
       width = 10)


#'*Causes Perinatal Deaths*
cod <- read_xlsx("data/prematurity/Perinatal Deaths.xlsx")

# cod$causes <- as.Date(cod$causes)

cod
cod <- melt(cod[c(1, 2, 3, 4, 5, 6, 7)], id = 'causes')

cod

cod1 <- ggplot(cod, aes(x=causes, y=value, fill=variable), alpha=0.6)+ 
  geom_bar(alpha=.7,stat="identity", position="dodge") +
  scale_fill_manual(values=c( usaid_palette6)) +
  scale_y_continuous(labels=comma) +
  labs(fill="Legend:", title="Causes of Perinatal Deaths, 2017-2022",
       x="",
       y="Number of cases") + base

cod1
ggsave("viz/prematurity/causes.png",
       device="png",
       type="cairo",
       height = 6.0,
       width = 13)

#'*Perinatal Deaths by death*
peri.dth.prv <- read_xlsx("data/prematurity/perinatal deaths by province.xlsx")
peri.dth.prv  <- peri.dth.prv  %>%
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

peri.dth.prv
colnames(peri.dth.prv)

peri.dth.prv1 <- peri.dth.prv %>%
  select(2,3,9)

peri.dth.prv1

peri.dth.prv2 <- peri.dth.prv1 %>%
  rename(prov=2,
         mnth=3)

peri.dth.prv2

pd <- ggplot(peri.dth.prv2, aes(x=mnth, y=Deaths), alpha=0.5)+ 
  geom_smooth(method=loess, color=usaid_red, size=0.7,se=F) + 
  geom_point(color=usaid_red) + faceted +
  facet_wrap(~prov) + ##scales="free_y" tom allow for independ y axis variables
  scale_x_date(date_labels="%b",date_breaks="1 month") + 
  labs(fill="Legend:", title="Perinatal Deaths by Province, 2022",
       x="",
       y="Number of Deaths")
pd

ggsave("viz/prematurity/perinatal deaths.png",
       device="png",
       type="cairo",
       height = 6.0,
       width = 13)




#'*THE NEW REQUEST SCRIPT ADJUSTMENTS*
source("scripts/r prep2.r")
source("scripts/r prep3.r")

perinatal.mort <- read_xlsx("data/prematurity April 2023/perinatal mortality rate q1s.xlsx")
perinatal.mort  <- perinatal.mort  %>%
  mutate(year = str_sub(period,
                        start=nchar(period)-4,
                        end=nchar(period)))
perinatal.mort

perinatal.mort1 <- perinatal.mort %>%
  select(1,2,4) %>%
  na.omit()

perinatal.mort2 <- perinatal.mort1 %>%
  rename(prov =1,
         peri.mr=2,
         yr=3)

perinatal.mort2

perinatal.mort3 <- perinatal.mort2 %>% 
  gather(key = subRt , value = rate, c(peri.mr))

perinatal.mort3

zam.boundary <- geoboundaries(country = "Zambia"
                              , adm_lvl = 1) %>% 
  select(shapeName)

zam.boundary

#write_xlsx(zam.boundary,"data/prematurity/province.xlsx")

zam.boundary1 <- zam.boundary %>%
  select(1, 2) %>%
  na.omit()

zam.boundary1


map_colors <- carto_pal(name = "Burg")


perinatal.mort4 <- perinatal.mort3 %>%
  group_by(yr,prov, subRt)


perinatal.mort4

perinatal.mort5 <- left_join(perinatal.mort4
                             , zam.boundary1
                             , by = c("prov" = "shapeName")) %>%
  sf::st_as_sf()

perinatal.mort5


ggplot(perinatal.mort5, aes(geometry = geometry, fill = rate)) +
  geom_sf()+
  geom_sf_text(aes(label = prov), size = 3) +
  facet_wrap(~yr) +
  scale_fill_carto_c(name="Proportion of\n Mortality Rate"
                     , palette = "Burg") +
  labs(x="", y="", caption = "Data Source: PDSR & HMIS",
       title = "Perinatal Mortality Rate - Quarters 1, 2019-2023"
       , subtitle = "Darker colors represent a higher proportion of mortality rate") + #for faceted and xy labels include x="Longitude", y="Latitude", +faceted
  theme_void() +
  theme(plot.title.position = "plot",
        plot.title = element_text(size = 16, hjust=0.5, family="Gill Sans Mt", face="bold"),
        plot.subtitle = element_text(size = 12, hjust=0.5),
        plot.caption = element_text(size=11),
        # axis.title.x = element_text(size = 12, family="Gill Sans Mt", face="bold"),
        # axis.title.y = element_text(size = 12, family="Gill Sans Mt", face="bold"),
        # axis.text.x = element_text(size = 8),
        # axis.text.y = element_text(size = 10),
        legend.text = element_text(size = 12),
        legend.title=element_blank(),
        legend.position="right",
        strip.text=element_text(size=14, family="Gill Sans Mt"))

##interactive map  
# zam.boundary2 %>%
#   leaflet() %>%
#   addTiles() %>%
#   addPolygons(label = zam.boundary2$shapeName)

ggsave("viz/prematurity Apr 2023/q1 perinatal_mortality_rate.png",
       device="png",
       type="cairo",
       height = 6.5,
       width = 15)

#'*Prematurity rate*
prema.rate <- read_xlsx("data/prematurity April 2023/prematurity rate.xlsx")

prema.rate$period <- as.Date(prema.rate$period)

prema.rate

prema.rate2 <- prema.rate %>%
  rename( prema.rate = 2) %>%
  mutate(prema.rate.prt = prema.rate/100)



prema.rate2 <- prema.rate2 %>%
  select(1,3)

prema.rate2

prema.rate3 <- prema.rate2 %>% 
  gather(key = subRt , value = rate, c(prema.rate.prt))

prema.rate3

ggplot(prema.rate3, aes(x = period, y = rate, group = subRt, colour = subRt)) +
  geom_point(alpha=.6, size=1.9) + 
  #geom_line(size=1) +
  geom_smooth(method = loess, size = .8, se=FALSE) +
  scale_y_continuous(limits = c(0,1),
                     labels = percent,
                     breaks = c(.1,.2,.3,.4,.5,.6,.7,.8,.9, 1)) +
  scale_x_date(date_labels="%b %y",date_breaks="4 months") +
  xlab("") + 
  ylab("") +
  ggtitle("Prematurity Rate , Sept 2017 - Mar 2023") +
  scale_color_manual(name ="",
                     values = usaid_red) + 
  baseX

ggsave("viz/prematurity Apr 2023/prematurity_rate.png",
       device="png",
       type="cairo",
       height = 5.5,
       width = 12)


#'*Perinatal deaths, Fresh Still & Macerated Stillbirths*
pr.mr.st <- read_xlsx("data/prematurity April 2023/perinatal & stillbirths q1 2019_2023.xlsx")

pr.mr.st

pr.mr.st$Month <- as.Date(pr.mr.st$Month)

pr.mr.st


pr.mr.st2 <- pr.mr.st %>%
  rename(mth =1,
         peri.deaths=2,
         peri.Rt=3,
         frsh.stlbrth.Rt=4,
         mcrtd.brth.Rt=5)

pr.mr.st2

#'*Perinatal deaths*
perinatal.deaths <- pr.mr.st2 %>%
  select(1,2)

perinatal.deaths

perinatal.deaths <- perinatal.deaths %>% 
  gather(key = subRt , value = rate, c(peri.deaths))

perinatal.deaths

ggplot(perinatal.deaths, aes(x = mth, y = rate, group = subRt, colour = subRt)) +
  geom_point(alpha=.6, size=1.9) + 
  #geom_line(size=1) +
  geom_smooth(method = loess, size = .8, se=FALSE) +
  # scale_y_continuous(limits = c(0,1),
  #                    labels = percent,
  #                    breaks = c(.1,.2,.3,.4,.5,.6,.7,.8,.9, 1)) +
  scale_x_date(date_labels="%b %y",date_breaks="3 months") +
  xlab("") + 
  ylab("Deaths") +
  ggtitle("Perinatal deaths ,Jan 2019 - Oct 2022") +
  scale_color_manual(name ="",
                     values = usaid_red) + 
  basey

ggsave("viz/prematurity/perinatal deaths.png",
       device="png",
       type="cairo",
       height = 5.5,
       width = 11)

#'*Stillbirths*
pr.mr.st2
frsh.stillmacerbirth <- pr.mr.st2 %>%
  select(1,4,5)

frsh.stillmacerbirth


frsh.stillmacerbirth <- frsh.stillmacerbirth %>% 
  gather(key = subRt , value = rate, c(frsh.stlbrth.Rt, mcrtd.brth.Rt))

frsh.stillmacerbirth

# pr.mr.st23 <- pr.mr.st %>%
#   select(1,4,5)
# 
# pr.mr.st23
# 
# pr.mr.st24 <- melt(pr.mr.st23[c(1, 2, 3)], id = 'Month')
# 
# pr.mr.st24
# 
# 
# fmsb <- ggplot(pr.mr.st24, aes(x=Month, y=value , fill=variable), alpha=0.6)+ 
#   geom_area(position=position_dodge(), color="#CFCDC9") +
#   geom_area(position = position_dodge()) +
#   scale_y_continuous(limits = c(0,8),
#                      breaks = c(0,2,4,6,8)) +
#   xlab("") + 
#   ylab("Rate") +
#   ggtitle("Fresh stillbirth and Macerated stillbirth per 1000 live births ,Jan 2019 - Oct 2022") +
#   scale_x_date(date_labels="%b %y",date_breaks="3 months") +
#   scale_fill_manual(name ="",
#                     values = c(usaid_red,usaid_blue),labels = c("Fresh Stillbirth", "Macerated Stillbirth")) + base
# 
# fmsb
# 
# fmsb <- ggplot(pr.mr.st, aes(x=period, y=value , fill=variable), alpha=0.6)+
#   geom_area(position=position_dodge(), color="#CFCDC9")

ggplot(frsh.stillmacerbirth, aes(x = mth, y = rate, group = subRt, fill = subRt), alpha=0.6) +
  geom_area(alpha=.8, position = position_dodge()) +
  scale_y_continuous(limits = c(0,8),
                     breaks = c(0,2,4,6,8)) +
  xlab("") + 
  ylab("Rate") +
  ggtitle("Fresh stillbirth and Macerated stillbirth per 1000 live births ,Jan 2019 - Mar 2023") +
  scale_x_date(date_labels="%b %y",date_breaks="3 months") +
  scale_fill_manual(name ="",
                    values = c(usaid_red,usaid_blue),labels = c("Macerated Stillbirth","Fresh Stillbirth")) + base

ggsave("viz/prematurity Apr 2023/stillbirths.png",
       device="png",
       type="cairo",
       height = 5.0,
       width = 10)


#'*Causes Perinatal Deaths*
cod <- read_xlsx("data/prematurity April 2023/Perinatal Deaths and cause by quarter.xlsx")

# cod$causes <- as.Date(cod$causes)

cod
cod <- melt(cod[c(1, 2, 3, 4, 5, 6, 7)], id = 'causes')

cod

cod1 <- ggplot(cod, aes(x=causes, y=value, fill=variable), alpha=0.6)+ 
  geom_bar(alpha=.7,stat="identity", position="dodge") +
  scale_fill_manual(values=c( usaid_palette6)) +
  scale_y_continuous(labels=comma) +
  labs(fill="Legend:", title="Causes of Perinatal Deaths - Quarters 1, 2019-2023",
       x="",
       y="Number of cases") + base

cod1
ggsave("viz/prematurity Apr 2023/causes.png",
       device="png",
       type="cairo",
       height = 6.0,
       width = 13)

#'*Perinatal Deaths by Province*
peri.dth.prv <- read_xlsx("data/prematurity April 2023/perinatal deaths by province.xlsx")
peri.dth.prv  <- peri.dth.prv  %>%
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

peri.dth.prv
colnames(peri.dth.prv)

peri.dth.prv1 <- peri.dth.prv %>%
  select(2,3,9)

peri.dth.prv1

peri.dth.prv2 <- peri.dth.prv1 %>%
  rename(prov=2,
         mnth=3)

peri.dth.prv2

pd <- ggplot(peri.dth.prv2, aes(x=mnth, y=Deaths), alpha=0.5)+ 
  geom_smooth(method=loess, color=usaid_red, size=0.7,se=F) + 
  geom_point(color=usaid_red) + faceted +
  facet_wrap(~prov) + ##scales="free_y" tom allow for independ y axis variables
  scale_x_date(date_labels="%b",date_breaks="1 month") + 
  labs(fill="Legend:", title="Perinatal Deaths by Province - Quarters 1, 2019 - 2023",
       x="",
       y="Number of Deaths")
pd

ggsave("viz/prematurity Apr 2023/perinatal deaths.png",
       device="png",
       type="cairo",
       height = 6.0,
       width = 13)


