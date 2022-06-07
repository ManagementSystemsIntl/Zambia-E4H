#Dont mind directory, i was working off Report
source("C:/Users/NyimbiliShida/Documents/MSI/GIS & Visuals/R Data/Scripts/r prep.R")
library(ggplot2)
library(hrbrthemes)

#Dont mind directory, i was working off Report
data <- read_xls("C:/Users/NyimbiliShida/Documents/MSI/GIS & Visuals/R Data/Data PMI/Nchelenge Confirmed Cases.xls")
pmi_data <- data %>%
  mutate(name2=name)

pmi_data %>%
  ggplot(aes(x=year, y=Confirmed_Cases,group=name))+
  geom_line( data=pmi_data %>% dplyr::select(-name), aes(group=name2), color="grey", size=0.5, alpha=0.5) +
  geom_line(aes(color = name, group = name), color="#002a6c", size=1.2 ) +
  #geom_point(size=2, colour="002a6c")
  #scale_color_viridis(discrete = TRUE) +
  #theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11),
    panel.grid = element_blank()
  ) +
  ggtitle("Confirmed Malria Cases") +
  facet_wrap(~name)+faceted