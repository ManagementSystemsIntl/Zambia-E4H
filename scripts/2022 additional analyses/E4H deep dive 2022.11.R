#'*Zambia E4 Health*
#'*Maternal neonatal health indicators*
#'*April - June 2022*

source("scripts/r prep3.r")
library(dplyr)
library(janitor)
library(foreign)
library(MASS)
library(epiDisplay)
library(writexl)
library(readxl)
library(compareGroups)
library(stringr)
library(tidyr)
library(tigerstats)

# Rename vars for consumption dataset ----

consume <- read_excel("data/FP stock and consumption data/FP_Consumption.xlsx")

consumer  <- consume  %>%
  rename(prov = 1,
         dist = 2,
         hu0 = 3,
         hu = 4,
         pgma = 5,
         fpcode = 6,
         fptype = 7, 
         cqty = 8,
         cmth = 9,
         cmthid = 10,
         cyr = 11,
  )

consumer <- consumer %>% 
  mutate(fptyper = case_when(fpcode == "RH0001" ~ "Condom",
                             fpcode == "RH0002" ~ "Condom",
                             fpcode == "RH0003"~ "Condom",
                             fpcode == "RH0004"~ "Pill",
                             fpcode == "RH0005"~ "Implant",
                             fpcode == "RH0006"~ "IUD",
                             fpcode == "RH0007"~ "Pill",
                             fpcode == "RH0008"~ "Emergency pill",
                             fpcode == "RH0009"~ "Injection",
                             fpcode == "RH0010"~ "Injection",
                             fpcode == "RH0011"~ "Condom",
                             fpcode == "RH0012"~ "Injection",
                             fpcode == "RH0013"~ "IUD",
                             fpcode == "RH0015"~ "Pill",
                             fpcode == "RH0016"~ "IUD",
                             fpcode == "RH0017"~ "Condom",
                             fpcode == "RH0018"~ "Female Organ Model",
                             fpcode == "RH0019"~ "Condom",
                             fpcode == "RH0020"~ "Condom",
                             fpcode == "RH0021"~ "Medroxyprogesterone",
                             fpcode == "RH0022"~ "Condom",
                             fpcode == "RH0023"~ "Injection",
                             fpcode == "RH0024"~ "Pill",
                             fpcode == "RH0025"~ "Implant",
                             fpcode == "RH0026"~ "Pill",
                             fpcode == "RH0027"~ "Pill",
                             fpcode == "RH0028"~ "Implant",
                             fpcode == "RH0030"~ "IUD",
                             fpcode == "RH0031"~ "Implant",
                             fpcode == "RH0032"~ "Condom",
                             fpcode == "RH0033"~ "Injection",
                             fpcode == "RH0041"~ "Mifepristone",
                             fpcode == "RH0042"~ "Mifepristone",
                             fpcode == "RH0043"~ "Mifepristone",
                             fpcode == "RH0046"~ "Implant",
                             fpcode == "RH0049"~ "Implant"
  ))

# Select consumption data for injections in one province/district for simplicity ----
# In the future, could look at shifts in consumption of an FP method in one level of care vs another over time 
## and how that relates to changes in stock

tab1(consumer$dist, bar.values="percent") # district with highest # of facilities reporting FP consumption data = Mansa (Luapula)
consume.mansa.inj <- consumer %>% 
  filter(dist == "Mansa",
         fptyper == "Injection") 

write_csv(consume.mansa.inj, path = "data/FP stock and consumption data/consume.mansa.inj.csv") 

consume.mansa.inj.sum <- consume.mansa.inj %>% 
  group_by(dist, cmth) %>% 
  summarise(sum = sum(cqty), 
            n=n(),
            .groups = 'drop') # sum verified

str(consume.inject)
consume.mansa.inj.sum <- as.data.frame(consume.mansa.inj.sum)

is.data.frame(consume.mansa.inj.sum)

consume.mansa.inj.sum %>% 
  ggplot(aes(x = cmth, y = sum)) +
  geom_col(fill = usaid_blue) 
 
# Get stock file to overlay ----



# Explore stock file ----

stock <- read_excel("data/FP stock and consumption data/FP_Stock_Status.xlsx")

summary(stock)
names(stock)
str(stock)
stock 

tab1(stock$Province)
tab1(stock$Province)
tab1(stock$MonthID)
tab1(stock$ReportingYear)
tab1(stock$ReportingPeriod)
tab1(stock$YearID)
tab1(stock$StockStatus)
head(stock)


# rename vars for easier manipulation stockr ----

stockr  <- stock  %>%
  rename(prov = 1,
         dist = 2,
         hu = 3,
         hu0 = 4,
         level = 5,
         fpcode = 6,
         fptype = 7, 
         rpdate = 8,
         mth = 9,
         mthn = 10,
         yr4 = 11,
         yr1 = 12,
         count = 13, #Physical Count
         amc = 14, #average monthly consumption
         mos = 15, #months of stock
         orderq = 16, #order qty
         status = 17, #Adequately, Over Stocked, Under Stocked, Stocked Out
  )

# drop last 2 columns, which do not change

is.data.frame(stockr)
stockr <- subset(stockr, select = -c(19, 18))

# Create variable that groups fptype into larger groups ----

stockr <- stockr %>% 
  mutate(fptyper = case_when(fpcode == "RH0001" ~ "Condom",
                             fpcode == "RH0002" ~ "Condom",
                             fpcode == "RH0003"~ "Condom",
                             fpcode == "RH0004"~ "Pill",
                             fpcode == "RH0005"~ "Implant",
                             fpcode == "RH0006"~ "IUD",
                             fpcode == "RH0007"~ "Pill",
                             fpcode == "RH0008"~ "Emergency pill",
                             fpcode == "RH0009"~ "Injection",
                             fpcode == "RH0010"~ "Injection",
                             fpcode == "RH0011"~ "Condom",
                             fpcode == "RH0012"~ "Injection",
                             fpcode == "RH0013"~ "IUD",
                             fpcode == "RH0015"~ "Pill",
                             fpcode == "RH0016"~ "IUD",
                             fpcode == "RH0017"~ "Condom",
                             fpcode == "RH0018"~ "Female Organ Model",
                             fpcode == "RH0019"~ "Condom",
                             fpcode == "RH0020"~ "Condom",
                             fpcode == "RH0021"~ "Medroxyprogesterone",
                             fpcode == "RH0022"~ "Condom",
                             fpcode == "RH0023"~ "Injection",
                             fpcode == "RH0024"~ "Pill",
                             fpcode == "RH0025"~ "Implant",
                             fpcode == "RH0026"~ "Pill",
                             fpcode == "RH0027"~ "Pill",
                             fpcode == "RH0028"~ "Implant",
                             fpcode == "RH0030"~ "IUD",
                             fpcode == "RH0031"~ "Implant",
                             fpcode == "RH0032"~ "Condom",
                             fpcode == "RH0033"~ "Injection",
                             fpcode == "RH0041"~ "Mifepristone",
                             fpcode == "RH0042"~ "Mifepristone",
                             fpcode == "RH0043"~ "Mifepristone",
                             fpcode == "RH0046"~ "Implant",
                             fpcode == "RH0049"~ "Implant"
  ))

str(stockr)

# Define dataset for implants in Mansa ----

stock.mansa.inj <- stockr %>% 
  filter(dist == "Mansa",
         fptyper == "Injection") 

# Make sure hu0 + fpcode is unique

unique(df)

# Stock snapshot: Define object with data for only Sep 2022 ----

stock.0922 <- stockr %>% 
  filter(rpdate == as.Date('2022-09-01'))

hu.num <- stock.0922 %>% 
  group_by(hu0) %>% 
  filter(n() > 1)

# Preparation for linking level of care indicator from 

unique(hu.num$hu0) # how many unique health facilities? >1000 too many to print on screen

#* Do all values of fpcode correspond to all values in fptype? ----

test <- stockr %>% 
  group_by(fpcode) %>% 
  filter(n() > 1)

unique(test$fpcode) # yes

#* Create table with correspondence between fpcode & fptype ----
test <- stockr %>% 
  dplyr::select(fptype, fpcode) %>% 
  unique()

# table with all correspondence for checking
fpgrouping <- stockr %>% 
  dplyr::select(fptype, fpcode, fptyper) %>% 
  unique()

write.csv(fpgrouping, "data/FP stock and consumption data/fpgrouping.csv") 

# Provinces with highest and lowest stockouts ----

provhilo <- stock.0922 %>% 
  group_by(prov) %>% 
  summarise(pct = count(status == "Stocked Out")/count(prov)
    )

provhilo %>% 
  ggplot(aes(x = prov, y = pct, fill = prov)) +
  geom_col(position = 'dodge') + 
  scale_fill_manual(values = rep(c("#A7C6ED","#002F6C","#BA0C2F", "#8C8985",  "#212721", "#651D32", "#0067B9"), 2)) + 
  scale_y_continuous(labels = scales::percent_format())+
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none") +
  labs (title = "Proportion of facility-reported FP methods that were stocked out in September 2022", x="Stock status", y = "Proportion", fill ="FP method")


# Reconstruct charts with USAID colours ----

str(stock.0922)

# stock.0922$fptyper <- as.factor(stock.0922$fptyper)

stockr %>% # COUNT
  ggplot(aes(x=rpdate, fill = rpdate)) +
  geom_bar(position = "dodge") +
  scale_y_continuous() +
  scale_fill_manual(values = rep(c("#BA0C2F", "#A7C6ED","#8C8985", "#002F6C","#212721", "#651D32", "#0067B9"), 2)) + 
  ggtitle("Number of facilities reporting stock data each month") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs (fill ="FP method", x = "Reporting month")

# Sum of MOS by FP method, Sep 2022 ----

stock.0922.fptype <- stock.0922 %>% 
  group_by(fptyper) %>% 
  summarise(summos.fp = sum(mos)) 

stock.0922.fptype %>% 
  ggplot(aes(x=fptyper, y= summos.fp, fill = fptyper)) +
  geom_col(position = "dodge") +
  scale_y_continuous()+
  scale_fill_manual(values = rep(c("#BA0C2F", "#A7C6ED","#8C8985", "#002F6C","#212721", "#651D32", "#0067B9"), 2)) + 
          ggtitle("Stock of family planning methods\nSeptember 2022 \n(N=8759)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs (fill ="FP method", x = "FP method", y = "Months of supply") +
  coord_flip()



# Stock status of all methods across levels of care (Sept 2022) ----

stock.0922.status <- stock.0922 %>% # PERCENT
  group_by(status) %>% 
  count() %>% 
  mutate(pct = n / 8759) #

stock.0922.status %>% 
  ggplot(aes(x = status, y = pct, fill = status)) +
  geom_col(position = 'dodge') + 
  scale_fill_manual(values = rep(c("#A7C6ED","#002F6C","#BA0C2F", "#8C8985",  "#212721", "#651D32", "#0067B9"), 2)) + 
  scale_y_continuous(labels = scales::percent_format())+
  theme(plot.title = element_text(hjust = 0.5)) +
  labs (title = "Stock status of facility-reported FP methods in September 2022", x="Stock status", y = "Proportion", fill ="FP method")

# Try diff crosstabs options ----
# Crosstab 1: table() -----

attach(stock.0922)
fptpxlvl <- table(level, fptyper)
prop.table(fptpxlvl, 1) # nice, quick and produces neat easy to read tables in proportion

# set key parameters as factors
is.vector(stock.0922$level)
is.data.frame(stock.0922)
level <- as.factor(stock.0922$level)
status <- is.factor(stock.0922$status)
fptyper <- is.factor(stock.0922$fptyper)

str(stock.0922)

# xtabs() for 1-way freq table

fpm.lvl <- xtabs(~level, stock.0922)
ftable(fpm.lvl)
summary(fpm.lvl) # chi2
fpm.lvl

# Crosstab 2: xtabs() for 2-way freq table ----

fpm.lvl <- xtabs(~level+fptyper, stock.0922)
ftable(fpm.lvl) # crosstab in frequencies
rowPerc(fpm.lvl) # crosstab in row %
summary(fpm.lvl) # chi2: sig
fpm.lvl

# barchartGC() - xtabs() can be followed by barchart GC() to produce grouped bar charts
# can be easily adjusted to % or count

barchartGC(fpm.lvl, type = "percent", 
           main= "Distribution of FP method by level of care",
           xlab= "Level of care")
#          sub= "Implants were most frequently available in primary health")

barchartGC(fpm.lvl, type = "count",
           main= "Distribution of FP method by level of care",
           xlab= "Level of care")

# Do same for stock status by level of care
# Define separate objects for data from each level of care

stock.0922 %>% 
  dplyr::select(level) %>% 
  unique()

names(stock.0922)
str(stock.0922)
stock.0922.l1dho <- stock.0922 %>% 
  filter(level == "District Health Office" | level == "Level 1 Hospital")

stock.0922.l2l3 <- stock.0922 %>% 
 filter(level == "Level 2 Hospital" | level == "Level 3 Hospital")

stock.0922.hp <- stock.0922 %>% 
  filter(level == "Health Post")

stock.0922.hchp <- stock.0922 %>% 
 filter(level == "Health Center" | level == "Health Post")

# FP stock by type and level of care (count) -- use geom_bar ----

str(stock.0922)

stock.0922.lvl.fpt <- stock.0922 %>% 
  group_by(level, fptyper) %>% 
  summarise(summos.fp = sum(mos)) %>% 
  ungroup() # confirmed correct on Excel

fpm.lvl <- stock.0922.lvl.fpt %>% # accurate
  ggplot(aes(x=level, y=summos.fp, fill=fptyper)) +
  geom_col(position = "dodge") +
  scale_y_continuous() +
  scale_fill_manual(values = rep(c("#A7C6ED","#8C8985", "#002F6C","#BA0C2F", "#212721", "#651D32", "#0067B9"), 2)) + 
  ggtitle("Distribution of family planning stock by method type\nSeptember 2022 \n(N=8759)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs (fill ="FP method", x = "Level of care", y = "Months of stock (MOS)")

# Accuracy check/ Conclusion: summos.fp was correctly calculated
library("writexl")
write_xlsx(stock.0922,"stock.0922.xlsx")
                                                                                                                                                                                                                                                                                                                                                                                                                                  
str(stock.0922)
stock.0922$status <- as.factor(stock.0922$status)

# FP stock by type and level of care: PERCENT --use geom_col

stock.0922.lvl.fpt.p <- stock.0922.lvl.fpt %>% # PERCENT
  group_by(level) %>% 
  mutate(pct = summos.fp/sum(summos.fp))

stock.0922.lvl.fpt.p %>% 
  ggplot(aes(level, pct, fill = factor(fptyper))) +
  geom_col(position = 'dodge') + 
  ggplot2::scale_fill_manual(values = rep(c("#A7C6ED","#8C8985", "#002F6C","#BA0C2F", "#212721", "#651D32", "#0067B9"), 2)) + 
  scale_y_continuous(labels = scales::percent_format())+
  theme(plot.title = element_text(hjust = 0.5), legend.position = "right") +
  labs (title = "Proportion of FP methods available at each level of care \nSeptember 2022", x="Level of care", y = "Percent", fill ="FP method")

# Stock status of FP methods at all DHIS2 facilities ----
stk.st.fp <- stock.0922 %>% 
  group_by(fptyper) %>% 
  count(status) %>% 
  mutate(pct = n / sum(n)) 

stk.st.fp %>% 
  ggplot(aes(fptyper, pct, fill = factor(status))) +
  geom_col(position = position_dodge2(preserve = "single")) + 
  ggplot2::scale_fill_manual(values = rep(c("#A7C6ED","#CFCDC9", "#002F6C","#BA0C2F", "#212721", "#651D32", "#0067B9"), 2)) + 
  scale_y_continuous(labels = scales::percent_format())+
  theme(plot.title = element_text(hjust = 0.5)) +
  labs (title = "Stock status of FP methods at all DHIS2 facilities \n September 2022 \n (N= 8769)", x="Family planning method", y = "Proportion", fill ="Stock status")

# Stock status of FP methods at HPs
 # stock.0922.hchp %>% 
 #  group_by(fptyper) %>% 
 #  ggplot(aes(x = fptyper, fill = factor(status))) +
 #  geom_bar(position = 'dodge') + 
 #  scale_fill_manual(values = rep(c("#0B2632", "#205493","#AC84B4", "#009CA6","#5482AB","#CFCDC9", "#9E0000"))) +
 #  # scale_y_continuous(labels = scales::percent_format())+
 #  theme(plot.title = element_text(hjust = 0.5)) +
 #  labs (title = "Stock status of FP methods at health centers and health posts \n September 2022 \n N=7730", x="Family planning method", y = "Count", fill ="Stock status")

# Stock status of FP methods at HCs
# stock.0922.hc %>% 
#   group_by(fptyper) %>% 
#   ggplot(aes(x = fptyper, fill = factor(status))) +
#   geom_bar(position = 'dodge') + 
#   scale_fill_manual(values = rep(c("#0B2632", "#205493","#AC84B4", "#009CA6","#5482AB","#CFCDC9", "#9E0000"))) +
#   # scale_y_continuous(labels = scales::percent_format())+
#   theme(plot.title = element_text(hjust = 0.5)) +
#   labs (title = "Stock status of FP methods at health centers", x="Family planning method", y = "Count", fill ="Stock status")
# 
# stock.0922.hc %>%
#   group_by(fptyper, status) %>%
#   tally() %>%
#   spread(fptyper, n)

# Stock status of FP methods at HCs & HPs ----

stock.0922.hchp %>% 
  group_by(fptyper) %>% 
  count(status) %>% 
  mutate(pct = n / sum(n)) %>% 
  ggplot(aes(fptyper, pct, fill = factor(status))) +
  geom_col(position = 'dodge') + 
  ggplot2::scale_fill_manual(values = rep(c("#A7C6ED","#CFCDC9", "#002F6C","#BA0C2F", "#212721", "#651D32", "#0067B9"), 2)) + 
  scale_y_continuous(labels = scales::percent_format())+
  theme(plot.title = element_text(hjust = 0.5)) +
  labs (title = "Proportion of FP methods available at HCs & HPs \nSeptember 2022 \nN=7730", x="Level of care", y = "Percent", fill ="FP method")

stock.0922.hc %>%
  group_by(fptyper, status) %>%
  tally() %>%
  spread(fptyper, n)

# Stock status of FP methods at Level 1 Hospitals & DHOs ----
stock.0922.l1dho %>% 
  group_by(fptyper) %>% 
  count(status) %>% 
  mutate(pct = n/sum(n)) %>% 
  ggplot(aes(x = fptyper, y = pct, fill = factor(status))) +
  geom_col(position = position_dodge2(preserve = "single")) + 
  ggplot2::scale_fill_manual(values = rep(c("#A7C6ED","#CFCDC9", "#002F6C","#BA0C2F", "#212721", "#651D32", "#0067B9"), 2)) + 
  scale_y_continuous(labels = scales::percent_format())+
  theme(plot.title = element_text(hjust = 0.5)) +
  labs (title = "Stock status of FP methods at Level 1 Hospitals and DHOs \nSeptember 2022 \nN=776", x="Family planning method", y = "Percent", fill ="Stock status")

# Stock status of FP methods at Level 2 & 3  Hospitals ----
stock.0922.l2l3 %>% 
  group_by(fptyper) %>% 
  count(status) %>% 
  mutate(pct = n/sum(n)) %>% 
  ggplot(aes(x = fptyper, y = pct, fill = factor(status))) +
  geom_col(position = position_dodge2(preserve = "single")) + 
  ggplot2::scale_fill_manual(values = rep(c("#A7C6ED","#CFCDC9", "#002F6C","#BA0C2F", "#212721", "#651D32", "#0067B9"), 2)) + 
  scale_y_continuous(labels = scales::percent_format())+
    theme(plot.title = element_text(hjust = 0.5)) +
  labs (title = "Stock status of FP methods at Level 2 and 3 Hospitals  \nSeptember 2022 \nN=253", x = "Family planning method", y = "Percent", fill ="Stock status")

## QUICK METHODS
# FP method by stock status at HPs
lvl.ss.hp <- xtabs(~fptyper+status, stock.0922.hp)
ftable(lvl.ss.hp)
rowPerc(lvl.ss.hp)
summary(lvl.ss.hp) # chi2: sig

# FP method by stock status at HCs
lvl.ss.hc <- xtabs(~fptyper+status, stock.0922.hc)
ftable(lvl.ss.hc)
rowPerc(lvl.ss.hc)
summary(lvl.ss.hc) # chi2: sig

# FP method by stock status at Level 1 hospitals & DHOs
lvl.ss.l1dho <- xtabs(~fptyper+status, stock.0922.l1dho)
ftable(lvl.ss.hc)
rowPerc(lvl.ss.hc)
summary(lvl.ss.hc) # chi2: sig

# FP method by stock status at HCs
lvl.ss.l2l3 <- xtabs(~fptyper+status, stock.0922.l2l3)
ftable(lvl.ss.hc)
rowPerc(lvl.ss.hc)
summary(lvl.ss.hc) # chi2: sig

#   QUICK METHOD: barchartGC()

barchartGC(lvl.ss.hp, type = "count",
           main= "Stock status of FP methods at health posts",
           xlab= "Family Planning method")

barchartGC(lvl.ss.hc, type = "count",
           main= "Stock status of FP methods at health centers",
           xlab= "Family Planning method")

barchartGC(lvl.ss.l1dho, type = "count",
           main= "Stock status of FP methods at Level 1 Hospitals & DHOs",
           xlab= "Family Planning method")

barchartGC(lvl.ss.l2l3, type = "count",
           main= "Stock status of FP methods at Level 2 and Level 3 Hospitals",
           xlab= "Family Planning method")


# crosstabs

crosstab(stock.0922)

unique(stock.0922$level) # 6 levels
unique(stock.0922$fptyper) # 7 fp methods
       
stock.0922 %>% 
  spread(key = level, value = count(fptyper))

