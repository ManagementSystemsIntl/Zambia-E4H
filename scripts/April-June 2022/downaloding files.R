#'*Load packages*

library(tidyverse)
library(RSelenium)
library(netstat)

#'*start the server*
rs_driver_object <- rsDriver(browser = 'chrome',
                             chromever = '105.0.5195.19',
                             verbose = FALSE,
                             port = free_port())

#checking server access type
class(rs_driver_object)

#client accesss object
remDr <- rs_driver_object$client

#opening a browser
remDr$open()
remDr$maxwindowSize()

#navigate to website
remDr$navigate('https://www.ebay.com')

# finding elements
electronics_object <- remDr$findElement(using = 'link text', 'Electronics')
electronics_object$getElementAttribute('href')
electronics_object$clickElement()

# go back
remDr$goBack()
# remDr$goForward)

# search for an item
search_box <- remDr$findElement(using = 'id', 'gh-ac')
search_box$sendKeysToElement(list('Camera', key = 'enter'))

# scroll to the end of the webpage
remDr$executeScript("window.scrollTo(0, document.body.scrollHeight);")

# click on the United States filter box
us_checkbox <- remDr$findElement(using = 'xpath', '//input[@aria-label="China"]')
us_checkbox$clickElement()

us_checkbox$refresh()

# click on the color dropdown
remDr$findElement(using = 'xpath', '//*[text()="Color"]')$clickElement()

# click on the white color
remDr$findElement(using = 'xpath', '//input[@aria-label="White"]')$clickElement()

# identify the price 
prices <- remDr$findElements(using = 'class name', 's-item__price')

price_values <- lapply(prices, function (x) x$getElementText()) %>% 
  unlist() %>% 
  str_remove_all('[$]')

price_values = price_values[-33]

# convert from number to string
price_values = price_values %>% 
  as.numeric()

mean(price_values)
median(price_values)


# terminate the selenium server
system("taskkill /im java.exe /f")

install.packages('shadowr')


#'*load packages*

library(tidyverse)
library(RSelenium)
library(netstat)
library(shadowr)


#'*start the server*
rs_driver_object <- rsDriver(browser = 'chrome',
                             chromever = '105.0.5195.19',
                             verbose = F,
                             port = free_port())


# rs_driver_object <- remDr$navigate("https://myterminal.ect.nl/app/object-schedule")
# remDr$navigate(rs_driver_object)
# shadow_rd <- shadow(remDr) 
# element <- find_elements(shadow_rd, 'button[class="_ect-button"]')[[5]]
# element$getElementText()[[1]] #to preview the element we found
# element$clickElement()[[1]]



class(rs_driver_object)
remDr <- rs_driver_object$client
remDr$open()

#navigate to website
remDr$navigate('https://learning.ulster.ac.uk/?new_loc=%2Fultra%2Fcourses%2F_297097_1%2Fcl%2Foutline')



cookies <- remDr$getAllCookies()
saveRDS(cookies, "cookies.rds")
for (i in 1:length(cookies)) {
  remDr$addCookie(name = cookies[[i]][["name"]], value = cookies[[i]][["value"]])
}




remDr$find_element('?new_loc=%2Fultra%2Fcourses%2F_297097_1%2Fcl%2Foutline')$clickElement()
shadow_rd <- shadow(remDr)
element <- find_elements(shadow_rd, 'button[class="button-1"]')
element
element$getElementText()
element$clickElement()



myurl <- remDr$navigate("https://myterminal.ect.nl/app/object-schedule")
remDr$navigate(myurl)
shadow_rd <- shadow(remDr) 
element <- find_elements(shadow_rd, 'button[class="_ect-button"]')[[5]]
element$getElementText()[[1]] #to preview the element we found
element$clickElement()[[1]]
