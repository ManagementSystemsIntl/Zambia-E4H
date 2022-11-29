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
search_box <- remDr$findElement(using = 'id', 'i0116')
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



#'*Download Files*

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


class(rs_driver_object)
remDr <- rs_driver_object$client
remDr$open()

#navigate to website
remDr$navigate('https://learning.ulster.ac.uk/webapps/blackboard/content/listContent.jsp?course_id=_297097_1&content_id=_4954803_1')

login_object <- remDr$findElement(using = 'link text', 'Login using SSO')$clickElement()

remDr$findElement(using = 'id', 'i0116')$sendKeysToElement(list("nyimbili-s@ulster.ac.uk", key="enter"))
remDr$findElement(using = 'id', 'i0118')$sendKeysToElement(list("KueEH2K$fuZeCySh", key="enter"))

#Approve on the Phone

Then

#click yes

remDr$findElement(using = 'id', 'idSIButton9')$clickElement()

remDr$findElement(using = 'link text', 'School of Geog & Env Sci - GIS Masters Project (EGM701) Support Area')$clickElement()
<a href="/webapps/blackboard/content/listContent.jsp?course_id=_297097_1&amp;content_id=_4954803_1"><span style="color:#00CC99;">LIBRARY OF EXAMPLES</span></a>




remDr$goBack()
remDr$goForward()


search_box
# access the client object
remDr <- rs_driver_object$client

# open a web browser
remDr$open()

# navigate to the Stats NZ website
remDr$navigate("https://www.stats.govt.nz/large-datasets/csv-files-for-download/")

# find the 'a' tags within the specified class name using the xpath method
data_files <- remDr$findElements(using = 'xpath', "//h3[@class='block-document__title']/a")

# return the names of the files
data_file_names <- lapply(data_files, function(x) {
  x$getElementText() %>% unlist()
}) %>% flatten_chr() %>% 
  str_remove_all("[:]")

# return the links to the files
data_file_links <- lapply(data_files, function(x) {
  x$getElementAttribute('href') %>% unlist()
}) %>% flatten_chr()

# the loop to download all the files
for (i in 1:length(data_file_names)) {
  download.file(
    url = data_file_links[i],
    destfile = paste0(data_file_names[i], gsub(x = data_file_links[i], pattern = ".*[.]", replacement = "."))
  )
}
