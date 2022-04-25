# Zambia 4 Health
# practice

d <- read_xlsx("data/Zambia provincial data.xlsx")

d

d_l <- d %>%
  pivot_longer(-1,
               names_to="month") %>%
  mutate(Month=factor(month, 
                      labels=c("Jan","Feb","Mar","Apr","May", "Jun","Jul","Aug","Sep","Oct","Nov","Dec")))

d_l

ggplot(d_l, aes(Month, value, color=Province, group=Province)) + 
  geom_line() + 
  geom_label(aes(label=value), size=3) + 
  scale_color_viridis_d()


ggplot(d_l, aes(Month, value, color=Province, group=Province)) + 
  stat_smooth(se=F, size=1) + 
  geom_point(size=1, alpha=.8) +
#  geom_label(aes(label=value), size=3) + 
  scale_color_viridis_d() + 
  facet_wrap(~Province) +
  faceted +
  theme(legend.position="none") +
  labs(x="",
       y="",
       title="Maternal deaths, by province",
       caption="Calendar year 2021") +
  scale_x_discrete(breaks=c("Feb","Apr","Jun","Aug","Oct","Dec")) +
  scale_y_continuous(limits=c(0,20))

ggsave("viz/Zambia maternal deaths, by province (22 Apr 2022).png",
       device="png",
       type="cairo",
       height=5,
       width=9)
