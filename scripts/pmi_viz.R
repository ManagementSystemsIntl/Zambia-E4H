#Dont mind directory, i was working off Report
source("C:/Users/NyimbiliShida/Documents/MSI/GIS & Visuals/R Data/Scripts/r prep.R") # use the package 'here'

library(ggplot2)
library(hrbrthemes)

#Dont mind directory, i was working off Report
data <- read_xls("C:/Users/NyimbiliShida/Documents/MSI/GIS & Visuals/R Data/Data PMI/Nchelenge Confirmed Cases.xls")
dat <- read_xls(here("data/Nchelenge Confirmed Cases.xls")) # don't use 'data' as the name of your object, because that is a base function in R. I usually use 'dat'. 

pmi_data <- dat %>%
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

pmi_data %>%
  ggplot(aes(x=year, y=Confirmed_Cases)) + 
  geom_col(width=.4, fill=medium_blue, alpha=.4) + 
  scale_x_continuous(breaks=2014:2021) +
  scale_y_continuous(labels=comma) +
  labs(title="centered title",
       x="",
       y="Confirmed\ncases") +
  theme(axis.title.y=element_text(angle=0, vjust=.5),
        plot.title=element_text(hjust=.5)) # note that this in line 53 of the r prep file. YOu could just uncomment line 53 to get centered titles for all your plots

ggsave("viz/Nchelenge confirmed cases bar.png",
       device="png",
       type="cairo",
       height=4,
       width=7)
