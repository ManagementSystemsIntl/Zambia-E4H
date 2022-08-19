source("scripts/r prep.r")


rnf <- read_xlsx("data/Malaria/Provincial rainfall.xlsx")
colnames(rnf)

rnf1<- rnf %>%
  group_by(Month, Year, Province) %>%
  summarise(sm.rainfall=sum(Rainfall),
            avg.rainfall=(mean(Rainfall))) %>%
  mutate(date = my(paste(Month, Year, sep="-")))



rnf1

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

