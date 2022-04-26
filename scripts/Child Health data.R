
library(esquisse)

ch <- read_xls("data/Downlaod Extract Childhealth Monthly At National.xls")
fam <- read_xls("data/Downlaod Extract Family Planning Monthly At National.xls")
fam_prov <- read_xls("data/Downlaod Extract Family Planning Yearly At Province.xls")
mat_prov <- read_xls("data/Downlaod Extract Maternal Yearly By Province.xls") 
mat <- read_xls("data/Downlaod Extract Maternal Monthly At National.xls")

# esquisse::esquisser(ch, viewer = "browser")
esquisser(ch)

# Neonatal deaths - wrangling from Dan's PAC

ch <- ch %>%
  rename(postnatal_care = 5) %>%
  mutate(month_chr = str_sub(periodname,
                           start=1,
                           end=nchar(periodname)-5),
       month = factor(month_chr,
                      levels=c("January","February","March","April","May","June","July","August","September","October","November","December")),
       month_code = as.numeric(month),
       year = str_sub(periodname, 
                      start=nchar(periodname)-4,
                      end=nchar(periodname)),
       monyr = paste(month_code, year, sep="-"),
       mnthyr = my(monyr))

str(ch)
glimpse(ch)

ggplot(ch, aes(mnthyr, "Neonatal Deaths")) + 
  geom_point(color="dodgerblue", alpha=.6, size=.8) + 
  geom_line(color="dodgerblue", alpha=.4) +
  stat_smooth(method="lm", color="dodgerblue", se=F, size=1.2, alpha=.8) +
  scale_y_continuous(limits=c(0,1),
                     labels=percent_format(accuracy=1)) +
  labs(x="",
       y="",
       title="Neonatal Deaths")





ggplot(ch, aes(x = periodname, y  = `Neonatal Deaths`))
 geom_line(color="dodgerblue", alpha=.4) +
 labs(x = "Month", y = "Number of neonatal deaths", title = "Neonatal deaths, all", caption = "Caption") +
 stat_smooth(method="lm", color="dodgerblue", se=F, size=1.2, alpha=.8) +
 scale_y_continuous(limits=c(0,1),
                      labels=percent_format(accuracy=1)) +
   theme_minimal()
getwd()


# Dan's model script 

ch <- ch %>%
  rename(postnatal_care = 5) %>%
  mutate(postnatal_care2 = postnatal_care / 100,
         month_chr = str_sub(periodname,
                             start=1,
                             end=nchar(periodname)-5),
         month = factor(month_chr,
                        levels=c("January","February","March","April","May","June","July","August","September","October","November","December")),
         month_code = as.numeric(month),
         year = str_sub(periodname, 
                        start=nchar(periodname)-4,
                        end=nchar(periodname)),
         monyr = paste(month_code, year, sep="-"),
         mnthyr = my(monyr))


sum(ch$month_chr!=ch$month)

mnth <- as.Date(ch$month_chr, format="%B")

mnth <- data.frame(factor(ch$month_chr))

frq(mnth)
frq(ch$postnatal_care2)
frq(ch$month_chr)
frq(ch$month)
frq(ch$year)
frq(ch$mnthyr)

ggplot(ch, aes(mnthyr, postnatal_care2)) + 
  geom_point(color="dodgerblue", alpha=.6, size=.8) + 
  geom_line(color="dodgerblue", alpha=.4) +
  stat_smooth(method="lm", color="dodgerblue", se=F, size=1.2, alpha=.8) +
  scale_y_continuous(limits=c(0,1),
                     labels=percent_format(accuracy=1)) +
  labs(x="",
       y="",
       title="Maternal postnatal care within 48 hours")

