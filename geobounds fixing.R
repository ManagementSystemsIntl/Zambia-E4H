source("scripts/r prep2.r")
# source("scripts/r prep3.r")

perinatal.mort <- read_xlsx("data/Aug 2023 MHDR/perinatal mortality rate q2s_2019_2023.xlsx")
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
  #na.omit()
  
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
       title = "Perinatal Mortality Rate - Quarters 2, 2019-2023."
       , subtitle = "Darker colors represent a higher proportion of mortality rate.") + #for faceted and xy labels include x="Longitude", y="Latitude", +faceted
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

ggsave("viz/Aug 23 FHDR/q2s perinatal_mortality_rate.png",
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

zam.boundary1


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
       title = "Maternal Mortality Ratio per 100, 000 Deliveries, 2019 - 2023"
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