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

matprv3
ggplot(matprv3, aes(x=yr, y=mr)) +
  geom_col(stat="identity", position=position_dodge(), fill=usaid_blue) +
  geom_line(aes(x = yr, y = hrr*2.2, color=usaid_red)) +
  # geom_point(aes(aes(x= yr, y= hrr*2.2),color=usaid_red, size=3)) +
  facet_wrap(~prov) +
  faceted +
  scale_y_continuous(sec.axis = sec_axis(trans = ~ .*0.0045,name = "Reporting rate", labels = scales::label_percent())) +
  labs(x="", y="Mortality Ratio", caption="Data Source: HMIS",title="Maternal mortality Ratio and reporting rates, 2019-2022") +
  scale_color_manual(name ="",
                     values = usaid_red,
                     labels = c("HIA2 Reporting rate (%)")) +
  basem + geom_label(aes( x= yr, y = hrr*2.2,label=hrr), fontface = "bold", hjust=0.9, vjust = 0.8)

ggsave("viz/May 2023 data review/Maternal MR and HIA2 RR facets.png",
       device="png",
       type="cairo",
       height = 7.5,
       width = 14)

