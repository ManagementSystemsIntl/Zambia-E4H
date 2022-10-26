#'*Zambia E4 Health*
#'*Maternal neonatal health indicators*
#'*April - June 2022*

source("scripts/r prep2.r")
library(dplyr)
library(janitor)
library(epiDisplay)

# FY2022, Q3: by district

#setwd("C:/Users/yashin.lin/Dropbox/0 Current Work/R R for work/Zambia-E4H")
pnc48.Q3d <- read_xls("data/48 hrs postnatal/PNC 48 hrs by facility.district Apr-Jun 22.xls")

str(pnc48.Q3d) # 2147 rows * 5
n_distinct(pnc48.Q3d$Province)

# Define dataset with 48hr indicator, facility, district, provincial info  ----
# + indicators that meet PNC w/in 48hr std (90% of expected deliveries)  ----

pnc48.Q3d <- pnc48.Q3d %>% 
  rename(pnc = 5,
         dist = District,
         prov = Province,
         facility = Facility) %>% 
  mutate(pnc = as.numeric(pnc),
         pncp = pnc/100,
         pnc90 = ifelse(pncp >.9, 1, 0),
#        pnc90 = factor(pnc90),
         pnc100 = ifelse(pncp >1, "Over 100%", "100 and under"))
       
pnc48.Q3d$prov = as.factor(pnc48.Q3d$prov)
str(pnc48.Q3d) # 2147 rows * 5

tab1(pnc48.Q3d$prov, main = "Number of facilities in each province with indicator data \non PNC within 48 hrs (N=10)")
tab1(pnc48.Q3d$prov, bar.values="percent", main = "Proportion of facilities in each province with indicator data \non PNC within 48 hrs (N=10)")

# Subset data with facilities meeting PNC48 standard  ----

pnc48.Q3.90 <- subset(pnc48.Q3d, pncp>.9) #236 observations
str(pnc48.Q3.90)

# Show distribution of facilities meeting standard by province ----

ggplot(pnc48.Q3.90, aes(x=prov, y=pnc90)) +
  geom_col()

str(pnc48.Q3.90$pnc90)

prov90 <-pnc48.Q3d %>% 
  group_by(prov) %>% 
  summarise(meet90 = mean(pnc90, na.rm =TRUE)) 

prov90 %>% 
  ggplot(aes(x=meet90, y = prov)) +
  geom_col()

, stat = "count"

  #  group_by(dist) %>% 
  ggplot(pnc48.Q3.90, aes(x=facility, y = pnc90)) +
    geom_bar()
  
  tab1(pnc90, main = "Types of facilities meeting the standard of 90% of expected deliveries \nreceive PNC within 48 hours \n (N = 236)")
  
  #pnc48.Q3d <- 
  
  pnc48.Q3d %>% 
    filter(prov =="Central") %>% 
    group_by(dist) %>% 
    count(ifelse(pnc90)
          
          #       , bar.values="percent", main = "Proportion of facilities in x with indicator data \non PNC within 48 hrs (N=)")
          

pnc48.Q3.90.c <- subset(pnc48.Q3.90, prov == "Central") 
pnc48.Q3.90.w <- subset(pnc48.Q3.90, prov == "Western") 
pnc48.Q3.90.nw <- subset(pnc48.Q3.90, prov == "Northwestern") 
pnc48.Q3.90.n <- subset(pnc48.Q3.90, prov == "Northern") 
pnc48.Q3.90.m <- subset(pnc48.Q3.90, prov == "Muchinga") 
pnc48.Q3.90.ls <- subset(pnc48.Q3.90, prov == "Lusaka") 
pnc48.Q3.90.lp <- subset(pnc48.Q3.90, prov == "Luapula") 
pnc48.Q3.90.e <- subset(pnc48.Q3.90, prov == "Eastern") 
pnc48.Q3.90.cp <- subset(pnc48.Q3.90, prov == "Copperbelt") 
pnc48.Q3.90.s <- subset(pnc48.Q3.90, prov == "Southern") 



# FY2022, Q3: by level of care

pnc48.Q3 <- read_xls("data/48 hrs postnatal/PNC 48hrs by facility Apr-Jun 2022.xls")

# N = 3233

pnc48.Q3  <- pnc48.Q3  %>%
  rename(hu = organisationunitname,
         pnc = 3) 

is.data.frame(pnc48)

pnc48.Q3 <- pnc48.Q3 %>% 
  mutate(level = ifelse(grepl("District Hospital", pnc48.Q3$hu, ignore.case = T), "DistHosp", 
                 ifelse(grepl("Central Hospital", pnc48.Q3$hu, ignore.case = T), "CentralHosp",
                 ifelse(grepl("Mini Hospital", pnc48.Q3$hu, ignore.case = T), "MiniHosp", 
                 ifelse(grepl("Mini Hosp", pnc48.Q3$hu, ignore.case = T), "MiniHosp", 
                 ifelse(grepl("Mission Hospital", pnc48.Q3$hu, ignore.case = T), "MissionHosp", 
                 ifelse(grepl("General Hospital", pnc48.Q3$hu, ignore.case = T), "GeneralHosp", 
                 ifelse(grepl("Mission First level hospital", pnc48.Q3$hu, ignore.case = T), "FLHosp",
                 ifelse(grepl("min hospital", pnc48.Q3$hu, ignore.case = T), "MiniHosp", 
                 ifelse(grepl("Central Hospital", pnc48.Q3$hu, ignore.case = T), "CentralHosp",
                 ifelse(grepl("University Hospital", pnc48.Q3$hu, ignore.case = T), "UniHosp", 
                 ifelse(grepl("Mini-Hospital", pnc48.Q3$hu, ignore.case = T), "MiniHosp",
                 ifelse(grepl("First Level Hospital", pnc48.Q3$hu, ignore.case = T), "FirstLevelHosp",
                 ifelse(grepl("University Hospital", pnc48.Q3$hu, ignore.case = T), "UniHosp", 
                 ifelse(grepl("Hospital", pnc48.Q3$hu, ignore.case = T), "Hosp",

# Health posts                        
               ifelse(grepl("Heath Post", pnc48.Q3$hu, ignore.case = T), "HP", 
               ifelse(grepl("HP", pnc48.Q3$hu, ignore.case = T), "HP", 
               ifelse(grepl("Healht Post", pnc48.Q3$hu, ignore.case = T), "HP", 
               ifelse(grepl("Health Post", pnc48.Q3$hu, ignore.case = T), "HP", 
               ifelse(grepl("Health  Post", pnc48.Q3$hu, ignore.case = T), "HP",  

# Health centres
               ifelse(grepl("Rural Health Centre", pnc48.Q3$hu, ignore.case = T), "RHC", 
               ifelse(grepl("Medical centre", pnc48.Q3$hu, ignore.case = T), "MC",
               ifelse(grepl("Urban Health Centre", pnc48.Q3$hu, ignore.case = T), "UHC", 
               ifelse(grepl("HAHC", pnc48.Q3$hu, ignore.case = T), "HAHC",
               ifelse(grepl("Hospital Affiliated Health Centre", pnc48.Q3$hu, ignore.case = T), "HAHC",
               ifelse(grepl("Health Centre", pnc48.Q3$hu, ignore.case = T), "HC",
               ifelse(grepl("Health Center", pnc48.Q3$hu, ignore.case = T), "HC",
               ifelse(grepl("Urban Clinic", pnc48.Q3$hu, ignore.case = T), "UC",       
               ifelse(grepl("Clinic", pnc48.Q3$hu, ignore.case = T), "Clinic",
               ifelse(grepl("Mbala Clinic", pnc48.Q3$hu, ignore.case = T), "Clinic", 
               ifelse(grepl("Clinic -Hospital Affiliated", pnc48.Q3$hu, ignore.case = T), "HAClinic",
               ifelse(grepl("Medical Center", pnc48.Q3$hu, ignore.case = T), "Med Center",
               ifelse(grepl("Dental Clinic", pnc48.Q3$hu, ignore.case = T), "Clinic", hu)))))))))))))))))))))))))))))))))

#tabyl(pnc48.Q3$level)
tab1(pnc48.Q3$level) # automatically creates bar charts
summarytools::freq(pnc48.Q3$level)

# Subset facilities with any PNC data
pnc48.Q3d <- subset(pnc48.Q3, pnc != "NA") #2147 observations with data
tab1(pnc48.Q3d$level, bar.values="percent", main = "Proportion of facilities with any PNC data \n (N=2147)")

# Subset facilities with any PNC > 90
str(pnc48.Q3d)
pnc48.Q3d$pnc <- as.numeric(pnc48.Q3d$pnc)

## create pncp proportion
pnc48.Q3d <- pnc48.Q3d %>% 
  mutate(pncp = pnc/100,
         pnc90 = ifelse(pncp >.9, "Meet target", "Do not meet target"),
         pnc100 = ifelse(pncp >1, "Over 100%", "100 and under")
         )

tab1(pnc48.Q3d$pnc100)
  
pnc48.Q3.90 <- subset(pnc48.Q3d, pncp>.9) #236 observations
tab1(pnc48.Q3.90$level, bar.values="percent", main = "Types of facilities meeting the standard of 90% of expected deliveries \nreceive PNC within 48 hours \n (N = 236)")

############################################################
# Binding data, which were downloaded into two chunks 

pnc48.18 <- read_xls("data/48 hrs postnatal/PNC 48hrs 2018-2020.xls")
pnc48.21 <- read_xls("data/48 hrs postnatal/PNC 48hrs 2021-22.xls")
pnc48 <- rbind(pnc48.18,pnc48.21)

str(pnc48)
nrow(pnc48) # 16,128

# Extract time period to manipulate data

pnc48  <- pnc48  %>%
  rename(hu = organisationunitname,
         pnc = 3) %>% 
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
         mnthyr = my(monyr)) # 576 failed to parse

tab1(pnc48$mnthyr)


is.data.frame(pnc48)

# Extract level of care

pnc48 <- pnc48 %>% 
  mutate(level = ifelse(grepl("District Hospital", pnc48$hu, ignore.case = T), "DH", 
    ifelse(grepl("Mini Hospital", pnc48$hu, ignore.case = T), "MiniH", 
    ifelse(grepl("Mini Hosp", pnc48$hu, ignore.case = T), "MiniH", 
    ifelse(grepl("Mission Hospital", pnc48$hu, ignore.case = T), "MissH", 
    ifelse(grepl("General Hospital", pnc48$hu, ignore.case = T), "GH", 
    ifelse(grepl("Urban Clinic", pnc48$hu, ignore.case = T), "UC", 
    ifelse(grepl("Rural Health Centre", pnc48$hu, ignore.case = T), "RHC", 
    ifelse(grepl("Urban Health Centre", pnc48$hu, ignore.case = T), "UHC", 
    ifelse(grepl("Mission First level hospital", pnc48$hu, ignore.case = T), "MFLH",
    ifelse(grepl("HAHC", pnc48$hu, ignore.case = T), "HAHC",
    ifelse(grepl("Clinic -Hospital Affiliated", pnc48$hu, ignore.case = T), "CHA",
    ifelse(grepl("Special Forces Clinic", pnc48$hu, ignore.case = T), "SFC",
    ifelse(grepl("Police Clinic", pnc48$hu, ignore.case = T), "PC", 
    ifelse(grepl("Mbala Clinic", pnc48$hu, ignore.case = T), "Mbala", 
        ifelse(grepl("Health Post", pnc48$hu, ignore.case = T), "HP", 
          ifelse(grepl("Health Post", pnc48$hu, ignore.case = T), "HP", hu)))))))))))))))))

# tabyl(pnc48$level)
tab1(pnc48$level) # automatically creates bar charts
summarytools::freq(pnc48$level)

#pnc48 <- pnc48 %>% 
#  mutate(level2 =recode(level, "GH" = "Hosp",
                        "HAHC" = "HC",
                        "MiniH" = "Hosp",
                        "MissH" = "Hosp",
                        "" = "",
                        "" = "",
                        "" = "",
                        "" = "",
                        ))

# pnc48 <- pnc48 %>%
#  filter(hu != "DH" & hu != "MiniH" & hu != "MissH" & hu != "GH" & hu != "UC" & hu != "RHC" & hu != "UHC" & hu != "HP" & hu != "CHA" & hu != "UHC" & hu != "HP")
# commonhu <- data.frame(level = c("DH","MiniH","MissH","GH","UC","RHC", "UHC", "HP"))
# remainhu <- anti_join(pnc48, commonhu, by="level")
# write.table(remainhu, file = 'remainhu', col.names = TRUE, sep = ",")

# Define object for PNC over 90%

str(pnc48)
pnc48$pnc <- as.numeric(pnc48$pnc) 

pnc48 <- pnc48 %>% 
mutate(pncp = pnc/100,
       pnc90 = ifelse(pncp >.9, "Meet target", "Do not meet target" ))

tab1(pnc48$pnc90, bar.values="percent", main = "Proportion of facilities that meet the 90% target rate for PNC within 48 hours")

pnc48.data <- pnc48 %>% 
  filter(pnc90 != "NA") # ) # 16,128 * 37% = 5966   , so object correctly defined

tab1(pnc48.data$pnc90, bar.values="percent", main = "Proportion of facilities that meet the 90% target rate for PNC within 48 hours")

pnc48.90 <- pnc48 %>% 
  filter(pnc90 =="Meet target") # 16,128 * .06052 = 976 so object correctly defined

tab1(pnc48.90$level, main = "Proportion of facilities that meet the 90% target rate for PNC within 48 hours")
tab1(pnc48.90$level, bar.values="percent", main = "Facilities that meet the 90% target rate for PNC within 48 hours, by facility type")


summarytools::freq(pnc48$pnc90)

pnc48 <- pnc48 %>% 
  filter(pnc90 ==1) %>% 
  summarytools::freq(pnc48$pnc90)

# long to wide

pnc48w = pnc48l %>% 
  spread()
wide


#'*set pnccp to 100 for all values >100*
mat <- mat %>% 
  dplyr::mutate(pncc = ifelse(ancc > 100, 100, ancc)) %>% 
  dplyr::mutate(pnccp = ancc/100)

#'*To create legend, gather method for including a legend --*

mat <- gather(mat, key = subpop , value = rate, c(anccp, anc1p,anc1u20p))
mat$subpop <- factor(mat$subpop, levels = unique(mat$subpop)) # transform into factor
levels(mat$subpop)

view(mat)

ggplot(mat, aes(x = mnthyr, y = rate, group = subpop, colour = subpop)) +
  geom_point(alpha=.6, size=1.9) + 
  geom_line(size=1) +
  #geom_smooth(method = loess, size = .8, se=FALSE) +
  scale_y_continuous(limits = c(0,1),
                     labels = percent,
                     breaks = c(.1,.2,.3,.4,.5,.6,.7,.8,.9, 1)) +
  scale_x_date(date_labels="%b %Y",date_breaks="4 months") +
  xlab("") + 
  ylab("") +
  ggtitle("Proportion of expected pregnancies receiving antenatal care (ANC), 2018-2022") +
  scale_color_manual(name ="",
                     values = usaid_palette,
                     labels = c("1st ANC coverage (all trimesters)", "1st ANC Coverage (1st Trimester)", 
                                "1st ANC visits in the 1st trimester: Women <20 yrs")
  ) + 
  base

ggsave("viz/Apr-Jun 2022/Reproductive Maternal & Neontal/Proportion of expected pregnancies receiving antenatal care geom-line ns1.png",
       
