# forecasting demonstration

library(fpp)
library(forecast)
library(backtest)
library(quantmod)
library(tseries)
library(zoo)


anc1p <- mat %>%
  filter(subpop=="anc1p") %>%
  select(mnthyr, rate, subpop)

str(anc1p)

?ts

out <- ts(anc1p$rate)

str(out)

plot(out)

acf(out)
pacf(out)

out.arim <- auto.arima(out)
summary(out.arim)

out.arim.pred <- data.frame(forecast(out.arim, h=6)) %>%
  mutate(mnthyr=ymd(c("2022-07-01","2022-08-01","2022-09-01", "2022-10-01","2022-11-01","2022-12-01")),
         forecast=1) %>%
  select(mnthyr, 
         rate=1,
         lower=4,
         upper=5,
         forecast)

out.arim.pred

str(out.arim.pred)


out.arim.dat <- anc1p %>%
  select(-3) %>%
  mutate(lower=NA,
         upper=NA,
         forecast=0) %>%
  bind_rows(out.arim.pred)

tail(out.arim.dat)


ggplot(out.arim.dat, aes(mnthyr, rate)) + 
  geom_point(data=out.arim.dat[1:54,], 
             color="dodgerblue2",
             size=1,
             alpha=.8) + 
  stat_smooth(#data=filter(out.arim.dat, forecast==0),
              data=out.arim.dat[1:55,],
              color="dodgerblue2") +
  scale_x_date(labels=date_format("%b-%y"),
               breaks=date_breaks("4 months")) +
  geom_point(data=filter(out.arim.dat, forecast==1),
             color="darkgoldenrod2",
             size=1,
             alpha=.8) +
  stat_smooth(data=filter(out.arim.dat, forecast==1),
              color="darkgoldenrod2",
              method="lm",
              se=F,
              fill="darkgoldenrod2",
              alpha=.2,
              size=.8) +
  # stat_smooth(data=filter(out.arim.dat, forecast==1),
  #             aes(y=lower),
  #             color="darkgoldenrod2",
  #             linetype="dotdash",
  #             size=1.2) + 
  # stat_smooth(data=filter(out.arim.dat, forecast==1),
  #             aes(y=upper),
  #             color="darkgoldenrod2",
  #             linetype="dotdash",
  #             size=1.2) + 
  geom_ribbon(aes(ymin=lower, ymax=upper),
              fill="darkgoldenrod2",
              alpha=.2) +
  scale_y_continuous(limits=c(0,.5),
                     labels=percent_format(accuracy=1)) +
  labs(x="",
       y="",
       title="ANC Coverage 1st Trimester with six-month forecast")


ggsave("viz/Apr-Jun 2022/Reproductive Maternal & Neontal/ANC Coverage 1st Trimester, with six-month forecast.png",
       device="png",
       type="cairo",
       height=5.5,
       width=9)


