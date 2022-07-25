# Zambia E4 Health
# Maternal neonatal health indicators
# April - June 2022

setwd("C:/Users/NyimbiliShida/Documents/MSI/GIS & Visuals/R/Zambia-E4H git")

source("scripts/r prep.R")

#ggplot_shiny()

mat <- read_xls("data/Jan- Jun 2022/Reproductive Maternal Data_National Level(Monthly).xls")
mat  <- mat  %>%
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

sum(mat$month_chr!=mat$month) # expecting 0 if vars same

matq <- read_xls("data/Jan- Jun 2022/Reproductive Maternal Data_National Level(Quarterly).xls")
mat_prov <- read_xls("data/Jan- Jun 2022/Reproductive Maternal Data_National Level(Monthly).xls")
mat_prov  <- mat_prov  %>%
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

sum(mat_prov$month_chr!=mat_prov$month) # expecting 0 if vars same

# Maternal ---- 

#* (1) Client: ANC coverage ----

mat <- mat %>%
  rename(ancc = 3,
         anc1 = 4,
         anc1u20 = 5,
         anc1hr = 12) %>%
  mutate(anccp = ancc/100,
         anc1p = anc1/100,
         anc1u20p = anc1u20/100,
         anc1hrp = anc1hr/100)

# set anccp to 100 for all values >100
mat <- mat %>% 
  dplyr::mutate(ancc = ifelse(ancc > 100, 100, ancc)) %>% 
  dplyr::mutate(anccp = ancc/100)

# To create legend, gather method for including a legend --

mat <- gather(mat, key = subpop , value = rate, c(anccp, anc1p,anc1u20p,anc1hrp))
mat$subpop <- factor(mat$subpop, levels = unique(mat$subpop)) # transform into factor
levels(mat$subpop)

ggplot(mat, aes(x = mnthyr, y = rate, group = subpop, colour = subpop)) +
  geom_point(alpha=.6, size=1) + 
  geom_smooth(method = loess, size = .7, se=FALSE) +
  scale_y_continuous(limits = c(0,1),
                     labels = percent,
                     breaks = c(.1,.2,.3,.4,.5,.6,.7,.8,.9, 1)) +
  xlab("") + 
  ylab("") +
  ggtitle("Proportion of expected pregnancies receiving antenatal care (ANC), 2018-2022") +
  scale_color_manual(name ="",
                     values = usaid_palette,
                     labels = c("1st ANC, all TMs", "ANC at TM1", "ANC at TM1: Women <20 yrs", "ANC at TM1: High risk pregs")
  ) + base
  # theme(plot.title = element_text(size = 14), 
  #       axis.title.x = element_text(size = 12),
  #       axis.title.y = element_text(size = 12),
  #       axis.text = element_text(size = 9),
  #       legend.position = "bottom", 
  #       legend.text = element_text(size = 9)
  # ) 

ggsave("viz/deleteme.png",
       device="png",
       type="cairo",
       height = 5.5,
       width = 7)

# ggsave("viz/Apr-Jun 2022/Proportion of expected pregnancies receiving ANC.png",
#        device="png",
#        type="cairo",
#        height=6,
#        width=12)
