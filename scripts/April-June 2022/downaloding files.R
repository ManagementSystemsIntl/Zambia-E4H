#Load packages

library(tidyverse)
library(RSelenium)
library(netstat)

#start the server
rs_driver_object <- rsDriver(browser = 'chrome',
                             chromever = '105.0.5195.102',
                             verbose = FALSE,
                             port = free_port())
library(RSelenium)
remDr <- remoteDriver(remoteServerAddr = "192.168.99.100",
                      port = 4445L)
remDr$open(silent = TRUE)
remDr$navigate("http://www.google.com/ncr")
remDr$getTitle()
remDr$screenshot(display = TRUE)
