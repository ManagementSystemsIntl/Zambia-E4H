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

# Proportion of all facilities (N=3233) that have data on 48 hr indicator ----

pnc48.Q3.alll <- read_xls("data/48 hrs postnatal/PNC 48hrs 3233 level.xls") # have level of care

pnc48.Q3.alll  <- pnc48.Q3.alll  %>%
  rename(prov = 1,
         dist = 2,
         hu = 4,
         hosp2 = 5,
         hosp1 = 6, 
         rhc = 7,
         ahc = 8,
         hp = 9,
         uhc = 10,
         dh = 11
  ) 

str(pnc48.Q3.alll)

# List variables with hu containing string "mini h"

mini <- pnc48.Q3.alll %>% 
  filter(grepl("mini h", hu, ignore.case = T)) # L: most are DH, 1 is hp (Chikonshi), and 3 are uhc (Mundengwa, Tubalange, Nakachenje)

# Recode HPs misclassified as mini and set mini to NA for such observations

pnc48.Q3.alll <- pnc48.Q3.alll %>% 
    mutate(mini = if_else(!is.na(hp) & grepl("mini h", hu, ignore.case = T), hp, 
                          if_else(!is.na(uhc) & grepl("mini h", hu, ignore.case = T), uhc, dh)),
           hp = ifelse(!is.na(hp) & grepl("mini h", hu, ignore.case = T), NA, hp),
           uhc = ifelse(!is.na(uhc) & grepl("mini h", hu, ignore.case = T), NA, uhc))

is.data.frame(pnc48.Q3.alll)

# Create one level of care variable with all data

pnc48.Q3.all <- pnc48.Q3.alll %>% 
  mutate(pnc48 = case_when(!is.na(hosp2) ~ hosp2,
                           !is.na(hp) ~ hp,
                           !is.na(hosp1) ~ hosp1,
                           !is.na(rhc) ~ rhc, 
                           !is.na(ahc) ~ ahc,
                           !is.na(uhc) ~ uhc,
                           !is.na(mini) ~ mini),
         level = case_when(!is.na(hosp2) ~ "hosp2",
                           !is.na(hp) ~ "hp",
                           !is.na(hosp1) ~ "hosp1",
                           !is.na(rhc) ~ "rhc", 
                           !is.na(ahc) ~ "ahc",
                           !is.na(uhc) ~ "uhc",
                           !is.na(mini) ~ "mini"),
         level2 = case_when(!is.na(hosp2) ~ "Hospital 2nd Level",
                            !is.na(hp) ~ "Health Post",
                            !is.na(hosp1) ~ "Hospital 1st Level",
                            !is.na(rhc) ~ "Rural Health Centre", 
                            !is.na(ahc) ~ "Hosp Affiliated Health Centre",
                            !is.na(uhc) ~ "Urban Health Centre",
                            !is.na(mini) ~ "Mini Hospital"))

##########################################################################################################################################
#*PNC within 48 hours--what facilities meet the 90% standard by level of care? Data: DHIS2, FY22, Q3 (Apr-Jun 2022) ----
##########################################################################################################################################

#pnc48.Q3 <- read_xls("data/48 hrs postnatal/PNC 48hrs fdp Apr-Jun 2022.xls")
pnc48.Q3 <- pnc48.Q3.all

str(pnc48.Q3) # 

# identify and remove empty columns
# empty_columns <- colSums(is.na(pnc48.Q3) | pnc48.Q3 == "") == nrow(pnc48.Q3) # for each column, counts total NA or "" 
# empty_columns # Boolean that indicates if colummn empty or not
# pnc48.Q3 <- pnc48.Q3[,!empty_columns]

# identify and remove empty rows (N=2146)
pnc48.Q3 <- pnc48.Q3 %>% 
  filter(!is.na(pnc48)) 
str(pnc48.Q3d) #N = 2146 (ie. includes only facilities with indicator data out of 3233 facilities)

tab1(pnc48.Q3$level2) # automatically creates bar charts
summarytools::freq(pnc48.Q3$level)

# Subset facilities with any PNC > 90
str(pnc48.Q3)
pnc48.Q3$pnc48 <- as.numeric(pnc48.Q3$pnc48)

## create pnc48p variable (ie. proportion meeting target)
pnc48.Q3d <- pnc48.Q3 %>% 
  mutate(pncp = pnc48/100,
         pnc90 = ifelse(pncp >.9, "Meet target", "Do not meet target"),
         pnc100 = ifelse(pncp >1, "Over 100%", "100 and under")
         )

tab1(pnc48.Q3d$pnc100) # 8.2%  have values > 100%

# Distribution by facility level/type
  
pnc48.Q3.90 <- subset(pnc48.Q3d, pncp>.9) #236 observations
tab1(pnc48.Q3.90$level2, bar.values="percent", main = "Proportion of facilities meeting the standard of 90% of expected deliveries \nreceive PNC within 48 hours \n (N = 236)")

# Recreate with usaid colours

str(pnc48.Q3.90)

pct.m.level <- pnc48.Q3d %>%
  group_by(level2) %>%
  mutate(n = count(level)
         , meet = count(pnc90 == "Meet target")
         , pct = meet/n
         ) %>%
  select(level2, n, meet, pct) %>%
  unique()

pct.m.levelc <- #
  
pct.m.level %>%
  ggplot(aes(x=level2, y = pct, fill = factor(level2))) +
  geom_col() +
  scale_x_discrete(labels = function(x)
    stringr::str_wrap(x, width =15)) +
  scale_y_continuous() #+
  usaid() +
  ggtitle("Number of facilities meeting the standard,* by level of care\nN=236") +
  labs (x = "Level of care", y = "Frequency", subtitle = "*Standard: 90% of expected deliveries receive PNC within 48 hours") +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none", plot.caption = element_text(hjust = 1)
  ) +
  coord_flip()

    ggplot(aes(x = fptyper, y = pct, fill = factor(status))) +
    geom_col(position = position_dodge2(preserve = "single")) + 
    ggplot2::scale_fill_manual(values = rep(c("#A7C6ED","#CFCDC9", "#002F6C","#BA0C2F", "#212721", "#651D32", "#0067B9"), 2)) + 
    scale_y_continuous(labels = scales::percent_format())+
    theme(plot.title = element_text(hjust = 0.5)) +
    labs (title = "Stock status of FP methods at Level 2 and 3 Hospitals  \nSeptember 2022 \nN=253", x = "Family planning method", y = "Percent", fill ="Stock status")
  
  
# Distribution by province and district

tab1(pnc48.Q3.90$prov, bar.values="percent", main = "Proportion of facilities meeting the standard of 90% of expected deliveries \nreceive PNC within 48 hours \n (N = 236)")
tab1(pnc48.Q3.90$prov, main = "Number of facilities meeting the standard of 90% of expected deliveries \nreceive PNC within 48 hours \n (N = 236)")

pnc48.w <- pnc48.Q3.90 %>% 
  filter(prov == "Western") 
tab1(pnc48.w$dist, bar.values="percent", main = "Proportion of facilities meeting the standard of 90% of expected deliveries \nreceive PNC within 48 hours in Western\n (N=22)")

pnc48.s <- pnc48.Q3.90 %>% 
  filter(prov == "Southern") 
tab1(pnc48.s$dist, bar.values="percent", main = "Proportion of facilities meeting the standard of 90% of expected deliveries \nreceive PNC within 48 hours in Southern\n (N=41)")

pnc48.nw <- pnc48.Q3.90 %>% 
  filter(prov == "Northwestern") 
tab1(pnc48.nw$dist, bar.values="percent", main = "Proportion of facilities meeting the standard of 90% of expected deliveries \nreceive PNC within 48 hours in North Western\n (N=13)")

pnc48.n <- pnc48.Q3.90 %>% 
  filter(prov == "Northern") 
tab1(pnc48.n$dist, bar.values="percent", main = "Proportion of facilities meeting the standard of 90% of expected deliveries \nreceive PNC within 48 hours in Northern\n (N=28)")

pnc48.m <- pnc48.Q3.90 %>% 
  filter(prov == "Muchinga") 
tab1(pnc48.m$dist, bar.values="percent", main = "Proportion of facilities meeting the standard of 90% of expected deliveries \nreceive PNC within 48 hours in Muchinga\n (N=11)")

pnc48.lk <- pnc48.Q3.90 %>% 
  filter(prov == "Lusaka") 
tab1(pnc48.lk$dist, bar.values="percent", main = "Proportion of facilities meeting the standard of 90% of expected deliveries \nreceive PNC within 48 hours in Lusaka\n (N=12)")

pnc48.lp <- pnc48.Q3.90 %>% 
  filter(prov == "Luapula") 
tab1(pnc48.lp$dist, bar.values="percent", main = "Proportion of facilities meeting the standard of 90% of expected deliveries \nreceive PNC within 48 hours in Luapula\n (N=33)")

pnc48.e <- pnc48.Q3.90 %>% 
  filter(prov == "Eastern") 
tab1(pnc48.e$dist, bar.values="percent", main = "Proportion of facilities meeting the standard of 90% of expected deliveries \nreceive PNC within 48 hours in Eastern\n (N=32)")

pnc48.cp <- pnc48.Q3.90 %>% 
  filter(prov == "Copperbelt") 
tab1(pnc48.cp$dist, bar.values="percent", main = "Proportion of facilities meeting the standard of 90% of expected deliveries \nreceive PNC within 48 hours in Copperbelt\n (N=23)")

pnc48.c <- pnc48.Q3.90 %>% 
  filter(prov == "Central") 
tab1(pnc48.c$dist, bar.values="percent", main = "Proportion of facilities meeting the standard of 90% of expected deliveries \nreceive PNC within 48 hours in Central\n (N=21)")

# =========================================================
# Analysis of monthly 
# =========================================================

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

# Save just in case ----
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
       
       
# FY2022, Q3: by district

#setwd("C:/Users/yashin.lin/Dropbox/0 Current Work/R R for work/Zambia-E4H")
# pnc48.Q3d <- read_xls("data/48 hrs postnatal/PNC 48 hrs by facility.district Apr-Jun 22.xls") # dataset has district, no level of care 
pnc48.Q3 <- read_xls("data/48 hrs postnatal/PNC 48hrs fdp Apr-Jun 2022.xls") # dataset has district and level of care

str(pnc48.Q3) # 2147 rows * 5

# Define dataset with 48hr indicator, facility, district, provincial info  ----
# + indicators that meet PNC w/in 48hr std (90% of expected deliveries)  ----

pnc48.Q3 <- pnc48.Q3 %>% 
  rename(pnc = 5,
         dist = District,
         prov = Province,
         facility = Facility) %>% 
  mutate(pnc = as.numeric(pnc),
         pncp = pnc/100,
         pnc90 = ifelse(pncp >.9, 1, 0),
         #        pnc90 = factor(pnc90),
         pnc100 = ifelse(pncp >1, "Over 100%", "100 and under"))

n_distinct(pnc48.Q3$prov)
pnc48.Q3d$prov = as.factor(pnc48.Q3d$prov)
str(pnc48.Q3d) # 2147 rows * 5

tab1(pnc48.Q3d$prov, main = "Number of facilities in each province with indicator data \non PNC within 48 hrs (N=10)")
tab1(pnc48.Q3d$prov, bar.values="percent", main = "Proportion of facilities in each province with indicator data \non PNC within 48 hrs (N=10)")

# Join dataset with PNC48 indicator in one column with data set with indicators by level of care   ----

str(pnc48.Q3m)
pnc48.Q3m <- cbind(pnc48.Q3d, pnc48.Q3) # facilities in same order so delete repeated variables
pnc48.Q3 <- subset(pnc48.Q3m, select = c(prov, dist, hu, pnc, pncp, pnc90, pnc100, hosp2, hosp3, zone, hp, hosp1, rhc, ahc, hospw, uhc, 21, 22))

# Identify list of facilities with PNC within 48 hours > 100

pnc48.100 <- pnc48.Q3 %>% 
  filter(pnc48.Q3$pnc>100)

write.xlsx(pnc48.100, file = "PNC48over100.xlsx")

# Subset data with facilities meeting PNC48 standard  ----

pnc48.Q3 <- subset(pnc48.Q3, pncp>.9) #236 observations
str(pnc48.Q3.90)

# Show distribution of facilities meeting standard by province ----

nbrhu.pnc48 <- ggplot(pnc48.Q3.90, aes(x=prov, y=pnc90)) + 
  geom_col() # counts number of cases since pnc90 is set to 1

ggplot(pnc48.Q3.90, aes(x=prov)) + 
  geom_bar() # counts number of cases since pnc90 is set to 1

# Show percentage of facilities in each province that meet standard ----

pnc48.Q3 %>% 
  ggplot(aes(x=prov, y=pncp)) + 
  geom_col()

prov90 %>% 
  ggplot(aes(x=meet90, y = prov)) +
  geom_col()

group_by(dist) %>%
  ggplot(pnc48.Q3.90, aes(x=facility, y = pnc90)) +
  geom_bar()

tab1(pnc90, main = "Types of facilities meeting the standard of 90% of expected deliveries \nreceive PNC within 48 hours \n (N = 236)")

pnc48.Q3d <-  pnc48.Q3d %>% 
  filter(prov =="Central") %>% 
  group_by(dist) %>% 
  #   count(ifelse(pnc90)
  
  #bar.values="percent", main = "Proportion of facilities in x with indicator data \non PNC within 48 hrs (N=)")
  
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
