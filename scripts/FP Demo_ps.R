

#'*Provincial Level.....*



  options(scipen = 999)
ggplot(FP_Data_Provincial_3, aes(Year, Value, fill = Method)) +
  geom_bar(stat= "identity") +
  facet_wrap(~Province) +
  scale_y_continuous(labels = scales::label_number_si()) +
  labs(x = "Year", y = "Number of distributed contraceptives", fill = "Method/Type") +
  theme(legend.position = "top")





#'*Distribution quantities/methodes...........provincial facets*
#This is to try the bars
source("scripts/r prep2.r")
source("scripts/r prep3.r")

fpd <- read_xlsx("data/Aug 2023 MHDR/FP Type disaggregations_provincial.xlsx")

fpd
fpd1 <- fpd %>% 
  rename(prov=1,
         yr=2)

fpd1

fpd1_plt <- ggplot(fpd1, aes(x=yr, y=Value, fill="Method"), alpha=0.6)+ 
  geom_bar(alpha=.7, stat="identity", position="dodge") +
  facet_wrap(~prov, ncol=4) +
  faceted +
  scale_fill_manual(values=c( usaid_palette6)) +
  scale_y_continuous(labels=comma) +
  labs(caption = "Data Source: HMIS",
       x="",
       y="Number of distributed Contraceptives", fill = "") +
  #theme(legend.position = "bottom") +
  ggtitle("Family Planning Methods and their consumption levels, 2021-2023 Q2.") +
  scale_color_manual(name ="",
                     values = usaid_palette,
                     labels = c("CopperT", "Hormonal US", "Implanon", "Jadelle",
                                "Medroxyprogesterone injection DMPA-IM", "Medroxyprogesterone injection DMPA-SC")
  ) + 
  base

fpd1_plt
ggsave("viz/Aug 23 FHDR/number of distributed FP by province.png",
       device="png",
       type="cairo",
       height = 6.0,
       width = 11)




library(scales)

fpd <- read_xlsx("data/Aug 2023 MHDR/FP Type disaggregations_provincial.xlsx")

fpd
fpd1 <- fpd %>% 
  rename(prov=1,
         yr=2)

fpd1

options(scipen = 999)
ggplot(fpd1, aes(yr, Value, fill = Method)) +
  geom_bar(stat= "identity") +
  facet_wrap(~prov) +
  ggtitle("Family Planning Methods and their consumption levels, 2021-2023 Q2.") +
  scale_y_continuous(labels = scales::label_number_si()) +
  scale_fill_manual(values=c( usaid_palette6)) +
  labs(x = "", y = "Number of distributed contraceptives", fill = "Method/Type", caption = "Data Source: HMIS") +
  theme(legend.position = "bottom")

ggsave("viz/Aug 23 FHDR/number of distributed FP by province.png",
       device="png",
       type="cairo",
       height = 6.0,
       width = 11)



library(scales)

inst.pnc <- read_xlsx("data/Aug 2023 MHDR/Institutional delivery national.xlsx")

inst.pnc

inst.pnc1 <- inst.pnc %>% 
  select(1,2,3,5)

inst.pnc1
inst.pnc2 <- inst.pnc1 %>% 
  rename(yr = 1,
         mpc = 4)

inst.pnc2


options(scipen = 999)
ggplot(inst.pnc2, aes(x = yr, y = mpc)) +
  geom_bar(stat="identity", position=position_dodge(), fill=usaid_blue) +
  geom_line(aes(x = yr, y = mpc*3.34, color=usaid_red)) +
  #geom_bar(stat= "identity") +
  # facet_wrap(~yr) +
  # faceted +
  scale_y_continuous(sec.axis = sec_axis(trans = ~ .*0.0035,name = "Maternal PNC Coverage", labels = scales::label_percent())) +
  labs(x="", y="Deliveries", caption="Data Source: HMIS",title="Institutional deliveries and PNC coverage within 48hrs, 2019 - 2023 Q2.") +
  scale_color_manual(name ="",
                     values = usaid_red,
                     labels = c("Maternal PNC Coverage (%)")) + 
  basem + geom_label(aes( x= yr, y = mpc*3.44,label=mpc), fontface = "bold", hjust=0.9, vjust = 2.7)

ggsave("viz/Aug 23 FHDR/number of distributed FP by province.png",
       device="png",
       type="cairo",
       height = 6.0,
       width = 11)











#'*Institutional delivery and PNC Coverage..............Aug 23*





idc <- read_xlsx("data/Aug 2023 MHDR/Institutional delivery national level.xlsx")

idc
idc <- reshape2::melt(idc[c(1, 2, 3, 4, 5)], id = 'Deliveries')

idc

idc1 <- ggplot(idc, aes(x=Deliveries, y=value, fill=variable), alpha=0.6)+ 
  geom_bar(alpha=.7,stat="identity", position="dodge") +
  scale_fill_manual(values=c( usaid_palette6)) +
  scale_y_continuous(sec.axis = sec_axis(trans = ~ .*0.0035,name = "Maternal PNC Coverage", labels = scales::label_percent())) +
  labs(x="", y="Deliveries", caption="Data Source: HMIS",title="Institutional deliveries and PNC coverage within 48hrs, 2019 - 2023 Q2.") +
  #scale_y_continuous(labels=comma) +
  #labs(fill="Legend:", title="Institutional deliveries and PNC Coverage - Quarters 1, 2019-2023 Q2.",
       # x="",
       # y="Number of Deliveries") +
       # 
    base

idc1
ggsave("viz/prematurity Apr 2023/causes.png",
       device="png",
       type="cairo",
       height = 6.0,
       width = 13)















