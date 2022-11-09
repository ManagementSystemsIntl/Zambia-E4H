
source("scripts/r prep2.r")

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
       height = 6.5,
       width = 12)
