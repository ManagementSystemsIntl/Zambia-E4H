source("C:/Users/NyimbiliShida/Documents/MSI/GIS & Visuals/R Data/Scripts/r prep.R")

source("scripts/r prep.r")

dat <- read_xls("C:/Users/NyimbiliShida/Documents/MSI/GIS & Visuals/R Data/Data PMI/Book3.xls")

dat2 <- read_xls(here("data/hlb.xls"))
dat2

dat <- dat2 %>%
  pivot_longer(2:3,
               names_to="variable",
               values_to="value")

#  melt(.[, c(1, 2, 3)], id.vars = 'Year')
  
dat

sapply(dat, mode)

ggplot(dat, aes(x=Year, y=value, fill = variable)) + 
  geom_area(alpha=.6)+
#  scale_fill_manual(values=c("blue","red")) +
  scale_fill_manual(values=c("#212721","#EF7D00")) +
  labs(title="Human biting rate at Shikapande (Sprayed) Indoors No. of bites per person per Night",
       x="",
       y="Human Bite Rate") +
  theme(
    axis.title.y=element_text(angle=0, hjust=2, vjust=6),
    plot.title=element_text(size=10, hjust=1.5)) + 
#  faceted # don't use this if the plot doesn't have facet_wrap
  geom_vline(xintercept = c(as.POSIXct("2019-10-01"), as.POSIXct("2020-10-01")),
             color=c("#212721","#EF7D00"),
             lty=c("1343", "1343") ,
             size=c(1,1), 
             alpha=0.5) 

#with this line i want to insert a vertical line on Oct 2020 & oct 2019+

  ggsave("C:/Users/NyimbiliShida/Documents/MSI/GIS & Visuals/R Data/Visuals Exports/Nchelenge HLC.png",
         device="png",
         type="cairo",
         height=6,
         width=10)

  