source("C:/Users/PIMPA.SAWULU/Desktop/R project doc_E4H/E4H-Zambia/scripts/r prep3.r")
# source("C:/Users/PIMPA.SAWULU/Desktop/R project doc_E4H/E4H-Zambia/scripts/r prep2.r")


#'*Provincial Maternal Mortality Ratio and Reporting Rates*
matmorr_prov <- read_xls("C:/Users/PIMPA.SAWULU/Desktop/R project doc_E4H/E4H-Zambia/data/MH Ad hoc requests/Maternal Mortality Ratio and Reporting rates_ (2019 to 2022).xls")
names(matmorr_prov)
matmorr_prov1 <- matmorr_prov %>%
  select(1,2,3,4)
matmorr_prov1
matmorr_prov2 <- matmorr_prov1 %>%
  rename(prov = 1,
         yr = 2,
         mr = 4,
         hrr = 3) %>%
  mutate(hrrP = hrr/100)
matmorr_prov2

matmorr_prov3 <- matmorr_prov2 %>%
  select(1,2,4,5)
matmorr_prov3

matmorr_prov3 <- gather(matmorr_prov2, key = mmtype , value = deaths, c(mr, hrrP))
matmorr_prov3
colnames(matmorr_prov3)

# comStat_prov3$mmtypef <- factor(comStat_prov3$mmtype, levels = unique(comStat_prov3$mmtype))
# levels(comStat_prov3$mmtypef)
# 
# names(comStat_prov3)
# colnames(comStat_prov3)
scalefactor <- 2
matmorr_prov3_plt <- ggplot(matmorr_prov3, aes(x = yr, y = deaths , colour =   mmtype, linetype=mmtype)) + 
  geom_point(alpha=.5, size=.7) + 
  geom_smooth(method = loess, size=.9, se=F) + facet_wrap(~prov) + faceted +
  scale_y_continuous(limits = c(0,200),
                     breaks = c(20,60,100,140,180,200),
                     sec.axis = sec_axis( trans=~.*scalefactor, name="Reporting Rates (%)") +
                     labels = c("20","60","100","140","180","200")) +
  scale_x_date(date_breaks = "", date_labels = "%Y")+
  scale_linetype_manual(name="",
                        labels= c("Maternal mortality facility ratio (per 100,000 live births)", "HIA2 Reporting Rates"), 
                        values=c("solid","solid", "dashed"))+
  labs(x ="", y="", caption = "Data Source:HMIS") +
  # xlab("") +
  # ylab("", caption = "Data Source:HMIS") + 
  ggtitle("Provincia Maternal Mortality Ratio vs Reporting Rates") +
  scale_colour_manual(name = "",
                      labels= c("Maternal mortality facility ratio (per 100,000 live births)", "HIA2 Reporting Rates"),
                      values = c(usaid_blue, medium_grey, usaid_red)) +
  base  
matmorr_prov3_plt
ggsave("C:/Users/PIMPA.SAWULU/Desktop/R project doc_E4H/E4H-Zambia/graphs/Maternal mortality Ratio and reporting rates.png",
       device="png",
       type="cairo",
       height = 7.5,
       width = 12)

