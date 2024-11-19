
#source("scripts/r prep2.r")

source("scripts/r prep2.r")
install.packages("rgdal")
install.packages("network")
install.packages("quanteda")
install.packages("sna")
install.packages("maps")
install.packages("RODBC")
install.packages("elevatr")
install.packages("sfdep")
install.packages("likert")
install_pakages <- c("transformr", "tidygraph","tm","tibble","quanteda.textplots","spData")
install.packages(install_pakages)
install.packages("rgdal", repos="http://R-Forge.R-project.org")
devtools::install_github('thomasp85/ggraph')
provinces_zam <- st_read("Data/Updated Shapefiles/Updated_Province.shp")
perinatal.mort <- read_xlsx("Data/prematurity/perinatal mortality rate.xlsx")
perinatal.mort
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
#zam.boundary <- geoboundaries(country = "Zambia"
#, adm_lvl = 1) %>%
#select(shapeName)
#zam.boundary
#write_xlsx(zam.boundary,"data/prematurity/province.xlsx")
provinces_zam1 <- provinces_zam %>%
  select(1, 2) %>%
  na.omit()
provinces_zam1

map_colors <- carto_pal(name = "Burg")

perinatal.mort4 <- perinatal.mort3 %>%
  group_by(yr,prov, subRt)

perinatal.mort4
perinatal.mort5 <- left_join(perinatal.mort4
                             , provinces_zam1
                             , by = c("prov" = "PROVINCE")) %>%
  sf::st_as_sf()
perinatal.mort5

ggplot(perinatal.mort5, aes(geometry = geometry, fill = rate)) +
  geom_sf()+
  geom_sf_text(aes(label = prov), size = 3) +
  facet_wrap(~yr) +
  scale_fill_carto_c(name="Proportion of\n Mortality Rate"
                     , palette = "Blue") +
  labs(x="", y="", caption = "Data Source: Action HIV",
       title = "HIV Positivity Rate, 2017-2022"
       , subtitle = "Darker colors represent a higher proportional rate") + #for faceted and xy labels include x="Longitude", y="http://127.0.0.1:28939/graphics/plot_zoom_png?width=923&height=900Latitude", +faceted
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


#'*START from here!.......................*

#'*THE NEW REQUEST SCRIPT ADJUSTMENTS*
source("scripts/r prep2.r")
source("scripts/r prep3.r")
# install.packages("rgdal")
# install.packages("network")
# install.packages("quanteda")
# install.packages("sna")
# install.packages("maps")
# install.packages("RODBC")
# install.packages("elevatr")
# install.packages("sfdep")
# install.packages("likert")
# install_pakages <- c("transformr", "tidygraph","tm","tibble","quanteda.textplots","spData")
# install.packages(install_pakages)
# install.packages("rgdal", repos="http://R-Forge.R-project.org")
# devtools::install_github('thomasp85/ggraph')
provinces_zam <- st_read("Data/Updated Shapefiles/Updated_Province.shp")
perinatal.mort <- read_xlsx("Data/Prematurity Jan 2024/perinatal mortality rate.xlsx")
perinatal.mort
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
#zam.boundary <- geoboundaries(country = "Zambia"
#, adm_lvl = 1) %>%
#select(shapeName)
#zam.boundary
#write_xlsx(zam.boundary,"data/prematurity/province.xlsx")
provinces_zam1 <- provinces_zam %>%
  select(1, 2) %>%
  na.omit()
provinces_zam1

map_colors <- carto_pal(name = "Burg")

perinatal.mort4 <- perinatal.mort3 %>%
  group_by(yr,prov, subRt)

perinatal.mort4
perinatal.mort5 <- left_join(perinatal.mort4
                             , provinces_zam1
                             , by = c("prov" = "PROVINCE")) %>%
  sf::st_as_sf()
perinatal.mort5

ggplot(perinatal.mort5, aes(geometry = geometry, fill = rate)) +
  geom_sf()+
  geom_sf_text(aes(label = prov), size = 3) +
  facet_wrap(~yr) +
  scale_fill_carto_c(name="Proportion of\n Mortality Rate"
                     , palette = "Blue") +
  labs(x="", y="", caption = "Data Source: MPDSR & HMIS",
       title = "Perinatal Mortality Rate, 2019 - 2023"
       , subtitle = "Darker colors represent a higher proportional rate") + #for faceted and xy labels include x="Longitude", y="http://127.0.0.1:28939/graphics/plot_zoom_png?width=923&height=900Latitude", +faceted
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


ggsave("viz/Prematurity viz jan 24/perinatal_mortality_rate.png",
       device="png",
       type="cairo",
       height = 6.5,
       width = 12.5)


#'*Redraw for Quarter 4s.................*

provinces_zam <- st_read("Data/Updated Shapefiles/Updated_Province.shp")
perinatal.mort <- read_xlsx("Data/Prematurity Jan 2024/perinatal mortality rate q4s_2019_2023.xlsx")
perinatal.mort
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
#zam.boundary <- geoboundaries(country = "Zambia"
#, adm_lvl = 1) %>%
#select(shapeName)
#zam.boundary
#write_xlsx(zam.boundary,"data/prematurity/province.xlsx")
provinces_zam1 <- provinces_zam %>%
  select(1, 2) %>%
  na.omit()
provinces_zam1

map_colors <- carto_pal(name = "Teal")

perinatal.mort4 <- perinatal.mort3 %>%
  group_by(yr,prov, subRt)

perinatal.mort4
perinatal.mort5 <- left_join(perinatal.mort4
                             , provinces_zam1
                             , by = c("prov" = "PROVINCE")) %>%
  sf::st_as_sf()
perinatal.mort5

ggplot(perinatal.mort5, aes(geometry = geometry, fill = rate)) +
  geom_sf()+
  geom_sf_text(aes(label = prov), size = 3) +
  facet_wrap(~yr) +
  scale_fill_carto_c(name="Proportion of\n Mortality Rate"
                     , palette = "Blue") +
  labs(x="", y="", caption = "Data Source: MPDSR & HMIS",
       title = "Perinatal Mortality Rate, Quarters 4 (2019 - 2023)"
       , subtitle = "Darker colors represent a higher proportional rate") + #for faceted and xy labels include x="Longitude", y="http://127.0.0.1:28939/graphics/plot_zoom_png?width=923&height=900Latitude", +faceted
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


ggsave("viz/Prematurity viz jan 24/perinatal_mortality_rate qrts 4.png",
       device="png",
       type="cairo",
       height = 6.5,
       width = 12.5)










#'*REDRAW FOR MATERNAL MORTALITY RATE*

maternal.mort <- read_xls("data/prematurity April 2023/Maternal Mortality Ratio 2019_2023 yearly.xls")
maternal.mort  <- maternal.mort  %>%
  mutate(year = str_sub(period,
                        start=nchar(period)-4,
                        end=nchar(period)))
maternal.mort

maternal.mort1 <- maternal.mort %>%
  select(1,2,4) %>%
  na.omit()

maternal.mort2 <- maternal.mort1 %>%
  rename(prov =1,
         mat.mr=2,
         yr=3)

maternal.mort2

maternal.mort3 <- maternal.mort2 %>% 
  gather(key = subRt , value = rate, c(mat.mr))

maternal.mort3

zam.boundary <- geoboundaries(country = "Zambia"
                              , adm_lvl = 1) %>% 
  select(shapeName)

zam.boundary

#write_xlsx(zam.boundary,"data/prematurity/province.xlsx")

zam.boundary1 <- zam.boundary %>%
  select(1, 2) %>%
  na.omit()

 ddfzam.boundary1


map_colors <- carto_pal(name = "Teal")


maternal.mort4 <- maternal.mort3 %>%
  group_by(yr,prov, subRt)


maternal.mort4

maternal.mort5 <- left_join(maternal.mort4
                             , zam.boundary1
                             , by = c("prov" = "shapeName")) %>%
  sf::st_as_sf()

maternal.mort5


ggplot(maternal.mort5, aes(geometry = geometry, fill = rate)) +
  geom_sf()+
  geom_sf_text(aes(label = prov), size = 3) +
  facet_wrap(~yr) +
  scale_fill_carto_c(name="Proportion of\n Mortality Rate"
                     , palette = "#") +
  labs(x="", y="", caption = "Data Source: HMIS",
       title = "Maternal Mortality Ratio per 100, 000 Deliveries, 2019 - 2024"
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

ggsave("viz/Nov 2024 FHDR/Maternal_mortality_rate_map Aug 23.png",
       device="png",
       type="cairo",
       height = 6.5,
       width = 12.5)







#'*Prematurity rate*
prema.rate <- read_xlsx("data/Prematurity Jan 2024/prematurity rate.xlsx")

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
  labs(x="", y="", caption="Data Source: PDSR", title="Prematurity Rate , Sept 2017 - Dec 2023.") +
  scale_color_manual(name ="",
                     values = usaid_red) + 
  baseX

ggsave("viz/Prematurity viz jan 24/prematurity_rate Jan.png",
       device="png",
       type="cairo",
       height = 6.0,
       width = 11)


#'*Perinatal deaths, Fresh Still & Macerated Stillbirths*
pr.mr.st <- read_xlsx("data/Prematurity Jan 2024/perinatal & stillbirths 2019_2023_monthly.xlsx")

pr.mr.st

pr.mr.st$Month <- as.Date(pr.mr.st$Month)

pr.mr.st


pr.mr.st2 <- pr.mr.st %>%
  rename(mth =1,
         peri.deaths=2,
         peri.Rt=3,
         mcrtd.brth.Rt=5,
         frsh.stlbrth.Rt=4)

pr.mr.st2

#'*Perinatal deaths*
# perinatal.deaths <- pr.mr.st2 %>%
#   select(1,2)
# 
# perinatal.deaths
# 
# perinatal.deaths <- perinatal.deaths %>% 
#   gather(key = subRt , value = rate, c(peri.deaths))
# 
# perinatal.deaths
# 
# ggplot(perinatal.deaths, aes(x = mth, y = rate, group = subRt, colour = subRt)) +
#   geom_point(alpha=.6, size=1.9) + 
#   #geom_line(size=1) +
#   geom_smooth(method = loess, size = .8, se=FALSE) +
#   # scale_y_continuous(limits = c(0,1),
#   #                    labels = percent,
#   #                    breaks = c(.1,.2,.3,.4,.5,.6,.7,.8,.9, 1)) +
#   scale_x_date(date_labels="%b %y",date_breaks="3 months") +
#   xlab("") + 
#   ylab("Deaths") +
#   ggtitle("Perinatal deaths ,Jan 2019 - Oct 2022") +
#   scale_color_manual(name ="",
#                      values = usaid_red) + 
#   basey
# 
# ggsave("viz/Aug 23 FHDR/perinatal deaths.png",
#        device="png",
#        type="cairo",
#        height = 5.5,
#        width = 11)

#'*Stillbirths..................Adjusted for August 2023*
pr.mr.st2
frsh.stillmacerbirth <- pr.mr.st2 %>%
  select(1,4,5)

frsh.stillmacerbirth


frsh.stillmacerbirth <- frsh.stillmacerbirth %>% 
  gather(key = subRt , value = rate, c(mcrtd.brth.Rt, frsh.stlbrth.Rt))

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
  ggtitle("Fresh stillbirth and Macerated stillbirth per 1000 live births (Jan 2019 - Dec 2023.)") +
  scale_x_date(date_labels="%b %y",date_breaks="3 months") +
  scale_fill_manual(name ="",
                    values = c(usaid_red, usaid_blue),labels = c("Macerated Stillbirth","Fresh Stillbirth")) + base

ggsave("viz/Prematurity viz jan 24/stillbirths & Macerated Jan 24.png",
       device="png",
       type="cairo",
       height = 6.5,
       width = 12)


#'*Causes Perinatal Deaths*
cod <- read_xlsx("data/Prematurity Jan 2024/Perinatal Deaths and cause by quarter.xlsx")

# cod$causes <- as.Date(cod$causes)

cod
cod <- reshape2::melt(cod[c(1, 2, 3, 4, 5, 6, 7)], id = 'causes')

cod

cod1 <- ggplot(cod, aes(x=causes, y=value, fill=variable), alpha=0.6)+ 
  geom_bar(alpha=.7,stat="identity", position="dodge") +
  scale_fill_manual(values=c( usaid_palette6)) +
  scale_y_continuous(labels=comma) +
  labs(fill="Legend:",  caption="Data Source: PDSR", title="Causes of Perinatal Deaths, quarters 4 (2019 - 2023).",
       x="",
       y="Number of cases") + base

cod1
ggsave("viz/Prematurity viz jan 24/ qtr 4 causes 2023.png",
       device="png",
       type="cairo",
       height = 6.0,
       width = 13)




#'*Causes of Maternal Deaths...........March 2024*
cod <- read_xlsx("data/Dec 2023 MHDR/Maternal Deaths cause_2023.xlsx")

cod
cod <- reshape2::melt(cod[c(1, 2, 3, 4, 5, 6, 7,8,9,10)], id = 'causes')

cod

cod1 <- ggplot(cod, aes(x=causes, y=value, fill=variable), alpha=0.6)+ 
  geom_bar(alpha=.7,stat="identity", position="dodge") +
  scale_fill_manual(values=c( usaid_palette9)) +
  scale_y_continuous(labels=comma) +
  labs(fill="Legend:",  caption="Data Source: MPDSR", title="Causes of Maternal Deaths, Jan - Dec 2023 (though these numbers exclude Lusaka Province data).",
       x="",
       y="Number of deaths") + base

cod1
ggsave("viz/Dec 23 FHDR/causes of MD 2023.png",
       device="png",
       type="cairo",
       height = 7.0,
       width = 14)







#'*Causes of Maternal Deaths by year...........March 2024*
cody <- read_xlsx("data/Dec 2023 MHDR/Maternal Deaths causes by year_2018-2023.xlsx")

cody
cody <- reshape2::melt(cody[c(1, 2, 3, 4, 5, 6, 7,8,9,10)], id = 'causes')

cody

cody2 <- ggplot(cody, aes(x=causes, y=value, fill=variable), alpha=0.6)+ 
  geom_bar(alpha=.7,stat="identity", position="dodge") +
  scale_fill_manual(values=c( usaid_palette9)) +
  scale_y_continuous(labels=comma) +
  labs(fill="Legend:",  caption="Data Source: MPDSR", title="Causes of Maternal Deaths by Year, Jan 2018 - Dec 2023 (excluding Lusaka Province data for 2023).",
       x="",
       y="Number of deaths") + base

cody2
ggsave("viz/Dec 23 FHDR/causes of MD by year.png",
       device="png",
       type="cairo",
       height = 7.0,
       width = 14)




#' #'*Maternal Deaths by Province..........March 2024*
#' peri.dth.prv <- read_xlsx("data/Dec 2023 MHDR/Maternal death by province_yearly.xlsx")
#' peri.dth.prv  <- peri.dth.prv  %>%
#'   mutate(month_chr = str_sub(periodname,
#'                              start=1,
#'                              end=nchar(periodname)-5),
#'          month = factor(month_chr,
#'                         levels=c("January","February","March","April","May","June","July","August","September","October","November","December")),
#'          month_code = as.numeric(month), 
#'          year = str_sub(periodname, 
#'                         start=nchar(periodname)-4,
#'                         end=nchar(periodname)),
#'          monyr = paste(month_code, year, sep="-"),
#'          mnthyr = my(monyr))
#' 
#' peri.dth.prv
#' colnames(peri.dth.prv)
#' 
#' peri.dth.prv1 <- peri.dth.prv %>%
#'   select(2,3,4)
#' 
#' peri.dth.prv1
#' 
#' peri.dth.prv2 <- peri.dth.prv1 %>%
#'   rename(prov=2,
#'          mnyr=3)
#' 
#' peri.dth.prv2
#' 
#' pd <- ggplot(peri.dth.prv2, aes(x=mnyr, y=Deaths), alpha=0.5)+ 
#'   geom_col(method="loess", color=usaid_palette9, size=0.8,se=F) + 
#'   #geom_bar(color=usaid_red, size=0.8) + 
#'   faceted +
#'   facet_wrap(~prov) + ##scales="free_y" tom allow for independ y axis variables
#'   #scale_x_date(date_labels="%b %y",date_breaks="3 months") + 
#'   labs(fill="Legend:", title="Perinatal Deaths by Province, 2019 - 2023 Q4.",
#'        x="",
#'        y="Number of Deaths",
#'        caption = "Data Source: MPDSR")
#' pd
#' 
#' ggsave("viz/Prematurity viz jan 24/perinatal deaths by prov by month.png",
#'        device="png",
#'        type="cairo",
#'        height = 6.5,
#'        width = 12)

















#' #'*Perinatal Deaths by Province*
#' peri.dth.prv <- read_xlsx("data/Aug 2023 MHDR/perinatal deaths by province.xlsx")
#' peri.dth.prv  <- peri.dth.prv  %>%
#'   mutate(month_chr = str_sub(periodname,
#'                              start=1,
#'                              end=nchar(periodname)-5),
#'          month = factor(month_chr,
#'                         levels=c("January","February","March")),
#'          month_code = as.numeric(month), 
#'          year = str_sub(periodname, 
#'                         start=nchar(periodname)-4,
#'                         end=nchar(periodname)),
#'          monyr = paste(month_code, year, sep="-"),
#'          mnthyr = my(monyr))
#' 
#' peri.dth.prv
#' colnames(peri.dth.prv)
#' 
#' peri.dth.prv1 <- peri.dth.prv %>%
#'   select(2,3,9)
#' 
#' peri.dth.prv1
#' 
#' peri.dth.prv2 <- peri.dth.prv1 %>%
#'   rename(prov=2,
#'          mnth=3)
#' 
#' peri.dth.prv2
#' 
#' pd <- ggplot(peri.dth.prv2, aes(x=mnth, y=Deaths), alpha=0.5)+ 
#'   geom_smooth(method=loess, color=usaid_red, size=0.7,se=F) + 
#'   geom_point(color=usaid_red) + faceted +
#'   facet_wrap(~prov) + ##scales="free_y" tom allow for independ y axis variables
#'   # scale_x_date(date_labels="%b %y",date_breaks="") + 
#'   labs(fill="Legend:", title="Perinatal Deaths by Province - Quarters 1, 2019 - 2023",
#'        x="",
#'        y="Number of Deaths")
#' pd
#' 
#' ggsave("viz/Aug 23 FHDR/perinatal deaths by prov.png",
#'        device="png",
#'        type="cairo",
#'        height = 6.0,
#'        width = 13)





#'*Perinatal Deaths by Province by MONTH_August 2023*
peri.dth.prv <- read_xlsx("data/Prematurity Jan 2024/perinatal deaths by province_monthly.xlsx")
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
  geom_smooth(method="loess", color=usaid_red, size=0.8,se=F) + 
  geom_point(color=usaid_red, size=0.5) + 
  faceted +
  facet_wrap(~prov) + ##scales="free_y" tom allow for independ y axis variables
  #scale_x_date(date_labels="%b %y",date_breaks="3 months") + 
  labs(fill="Legend:", title="Trends of Perinatal Deaths by Province, Jan 2019 - Dec 2023. \nNote that Lusaka is split in two: Lusaka and Women & Newborn.",
       x="",
       y="Number of Deaths",
       caption = "Data Source: PDSR")
pd

ggsave("viz/Prematurity viz jan 24/perinatal deaths by prov by month.png",
       device="png",
       type="cairo",
       height = 6.5,
       width = 12)


#'*_____________Redraw for National Level....perinatal rates*

peri.mr <- read_xlsx("data/Prematurity Jan 2024/perinatal mortality rates national_monthly.xlsx")
peri.mr  <- peri.mr  %>%
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

peri.mr

peri.mr1 <- peri.mr %>%
  select(2,9)

peri.mr1

peri.mr2 <- peri.mr1 %>%
  rename(perinatal.mortRate=1)

peri.mr2

nat_pmr <- ggplot(peri.mr2, aes(x=mnthyr, y=perinatal.mortRate, colour=usaid_red)) + 
  geom_point(alpha=.6, size=.9) + 
  geom_smooth(method = loess, linewidth = .8, se=FALSE) +
  scale_y_continuous(labels=comma,
                     limits=c(9,21)) +
  scale_x_date(date_labels="%b %y",date_breaks="3 months") +
  labs(x="", y="", caption="Data Source: PDSR & HMIS", title="Perinatal Mortality Rate per 1,000 live births, had been on a downward trend from January 2020, \nbut has begun to rise begining Jan 2023.") +
  scale_color_manual(name ="",
                     values = usaid_red,
                     labels ="Perinatal Mortality Rates") + 
  basey

nat_pmr
ggsave("viz/Prematurity viz jan 24/National perinatal mortality rates Jan 2020-2023.png",
       device="png",
       type="cairo",
       height = 6.5,
       width = 12)





#'*_____________Redraw for National Level....perinatal rates_USAID Supported*

peri.mr <- read_xlsx("data/Prematurity Jan 2024/perinatal mortality rates national_monthly_IP supported.xlsx")
peri.mr  <- peri.mr  %>%
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

peri.mr

peri.mr1 <- peri.mr %>%
  select(2,9)

peri.mr1

peri.mr2 <- peri.mr1 %>%
  rename(perinatal.mortRate=1)

peri.mr2

nat_pmr <- ggplot(peri.mr2, aes(x=mnthyr, y=perinatal.mortRate, colour=usaid_blue)) + 
  geom_point(alpha=.6, size=.9) + 
  geom_smooth(method = loess, size = .8, se=FALSE) +
  scale_y_continuous(labels=comma) +
  scale_x_date(date_labels="%b %y",date_breaks="3 months") +
  labs(x="", y="", caption="Data Source: PDSR", title="Perinatal Mortality Rate per 1,000 live births, had been on a downward trend from Jan 2020 to Jan 2022, \nbut has since remained almost constant upto 2024 in all 8 USAID supported provinces.") +
  scale_color_manual(name ="",
                     values = usaid_blue,
                     labels ="Perinatal Mortality Rates") + 
  basey

nat_pmr
ggsave("viz/Prematurity viz jan 24/National perinatal mortality rates_IP Supported.png",
       device="png",
       type="cairo",
       height = 6.5,
       width = 12)






