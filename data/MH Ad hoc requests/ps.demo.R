
source("scripts/r prep2.r")
source("scripts/r prep3.r")



#'*Perinatal deaths, Fresh Still & Macerated Stillbirths*
pr.mr.st <- read_xlsx("data/Aug 2023 MHDR/provincial FSB MSB_monthly.xlsx")

pr.mr.st

pr.mr.st$Month <- as.Date(pr.mr.st$Month)

pr.mr.st


pr.mr.st2 <- pr.mr.st %>%
  rename(prov=1,
         mth=2,
         frsh.stlbrth.Rt=7,
         mcrtd.brth.Rt=6)

pr.mr.st2

pr.mr.st2
frsh.stillmacerbirth <- pr.mr.st2 %>%
  select(1,2,6,7)

frsh.stillmacerbirth


frsh.stillmacerbirth <- frsh.stillmacerbirth %>% 
  gather(key = subRt , value = rate, c(frsh.stlbrth.Rt, mcrtd.brth.Rt))

frsh.stillmacerbirth

ggplot(frsh.stillmacerbirth, aes(x = mth, y = rate, group = subRt, fill = subRt), alpha=0.6) +
  geom_area(alpha=.8, position = position_dodge()) +
  scale_y_continuous(limits = c(0,8),
                     breaks = c(0,2,4,6,8)) +
  xlab("") + 
  ylab("Rate") +
  ggtitle("Fresh stillbirth and Macerated stillbirth per 1000 live births (Oct 2022 - Jul 2023.)") +
  # facet_wrap(~prov, ncol=4) +
  # faceted +
  scale_x_date(date_labels="%b %y",date_breaks="1 month") +
  scale_fill_manual(name ="",
                    values = c(usaid_red,usaid_blue),labels = c("Macerated Stillbirth","Fresh Stillbirth")) + base

ggsave("viz/Aug 23 FHDR/stillbirths Sept National.png",
       device="png",
       type="cairo",
       height = 6.5,
       width = 12)
