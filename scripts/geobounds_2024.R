
source("scripts/r prep2.r")
install.packages("rgdal")
install.packages("rgdal", repos="http://R-Forge.R-project.org")
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
