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

anc1p_pred <- anc1p %>%
  slice(-(52:54))

anc1p_ts <- ts(anc1p_pred$rate)
anc1p_arim <- auto.arima(anc1p_ts)

anc1p_forecast <- data.frame(forecast(anc1p_arim, h=3)) %>%
  mutate(mnthyr=ymd(c("2022-04-01","2022-05-01","2022-06-01")),
         forecast=1) %>%
  select(mnthyr, 
         rate=1,
         lower=4,
         upper=5,
         forecast)

str(anc1p)

anc1p_forecast

?ts

out <- ts(anc1p$rate)
out
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

out.arim.pred2 <- data.frame(forecast(out.arim, h=3)) %>%
  mutate(mnthyr=ymd(c("2022-04-01","2022-05-01","2022-06-01")),
         forecast=1) %>%
  select(mnthyr, 
         rate=1,
         lower=4,
         upper=5,
         forecast)

out.arim.pred
out.arim.pred2



out.arim.dat <- anc1p %>%
  select(-3) %>%
  mutate(lower=NA,
         upper=NA,
         forecast=0) %>%
  bind_rows(out.arim.pred)

tail(out.arim.dat)


ggplot(anc1p, aes(mnthyr, rate)) + 
  geom_point(color="dodgerblue") + 
  stat_smooth() + 
  geom_point(data=anc1p_forecast, aes(y=rate), color="darkgoldenrod2") +
  stat_smooth(data=anc1p_forecast, aes(y=rate), color="darkgoldenrod2",fill="darkgoldenrod2") +
  geom_ribbon(data=anc1p_forecast, aes(ymin=lower, ymax=upper),
              fill="darkgoldenrod2",
              alpha=.2) +
  scale_y_continuous(limits=c(0,.5),
                     labels=percent_format(accuracy=1))


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




rain <- read_excel("data/Malaria/Updated Nchelenge(monthly)_2014-2022.xls")

rain

rn <- rain %>%
  select(1,3,23) %>%
  rlang::set_names(nm=c("period","cases","rain")) %>%
  as.data.frame() %>%
  mutate(period = my(period),
         log_cases=log(cases),
         log_rain=log(rain),
         rain_chng=rain - lag(rain),
         cases_chng = cases-lag(cases),
         log_cases_chng = log_cases-lag(log_cases),
         log_rain_chng=log_rain - lag(log_rain))

head(rn)
str(rn)
?set_names

ggplot(rn, aes(period)) + 
  geom_point(aes(y=log_cases)) + 
  geom_point(aes(y=rain)) +
  geom_line(aes(y=rain))

l1 <- lm(log_cases ~ rain,
         data=rn)
summary(l1)

l2 <- lm(cases_chng ~ rain_chng,
         data=rn)

summary(l2)


ggplot(rn, aes(rain_chng, cases_chng)) + 
  geom_point() +
  stat_smooth(method="lm")

ggplot(rn, aes(period)) + 
  geom_point(aes(y=rain_chng),
             color="dodgerblue2") +
#  geom_point(aes(y=cases_chng),
#             color="firebrick2") +
  geom_line(aes(y=rain_chng),
            color="dodgerblue2")  
#  geom_line(aes(y=cases_chng),
#            color="firebrick2")


ggplot(rn, aes(period)) + 
#  geom_point(aes(y=log_rain_chng),
#             color="dodgerblue2") +
#  geom_point(aes(y=log_cases_chng),
#               color="firebrick2") +
  geom_line(aes(y=log_rain_chng),
            color="dodgerblue2") +  
  geom_line(aes(y=log_cases_chng),
            color="firebrick2")

l3 <- lm(log_cases_chng ~ log_rain_chng,
         data=rn)

library(psych)
describe(rn)










