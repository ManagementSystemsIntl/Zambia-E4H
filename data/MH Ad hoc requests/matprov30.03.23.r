source("scripts/r prep2.r")


#'Provincial Maternal Mortality Ratio and Reporting Rates
matprv1 <- read_xls("Data/Malaria/Maternal Mortality Ratio and Reporting rates_ (2019 to 2022).xls")


matprv  <- matprv1  %>%
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

matprv

names(matprv)


matprv
matprv1 <- matprv %>%
  select(1,3,4,10)

matprv1
matprv3 <- matprv1 %>%
  rename(prov = 1,
         yr = 4,
         mr = 2,
         hrr = 3)

matprv3

matprv4 <- gather(matprv3, key = variable , value = rate, c(mr, hrr))
matprv4$variable <- factor(matprv4$variable, levels = unique(matprv4$variable)) # transform into factor
levels(matprv4$variable)

matprv4

#write_xlsx(matprv3,"C:/Users/SNyimbili/OneDrive - Right to Care/Documents/RTCZ/matprv3.xlsx")


mt_plt <- ggplot(matprv4, aes(x = yr, y = rate, group = variable, colour = variable)) +
  geom_point(alpha=.9, size=1.3) +
  stat_smooth(method = "loess", size=.9, se=T) + facet_wrap(~prov) +
  faceted +
  scale_y_continuous(sec.axis = sec_axis(~ .*0.005, labels = scales::label_percent())) +
  xlab("") + 
  ylab("") +
  ggtitle("Maternal mortality Ratio and reporting rates, 2019-2022") +
  scale_color_manual(name ="",
                     values = usaid_palette,
                     labels = c("Facility Maternal mortality ratio per 100 000 deliveries", "HIA2 Reporting rate (%)")
  ) + basem

mt_plt
ggsave("Viz/Maternal mortality Ratio and reporting rates.png",
       device="png",
       type="cairo",
       height = 7.5,
       width=14)


