library(rvest)
library(dplyr)

library(RSelenium)

url = "https://www.bankofgreece.gr/en/main-tasks/supervision/private-insurance/insurance-undertakings/register"
driver <- RSelenium::rsDriver(browser = "firefox",
                              geckover = "0.34.0",
                              verbose = FALSE)

# creating a client object and opening the browser 
remDr <- driver$client 

# navigate to the url 
remDr$navigate(url) 

## When I try the loop from page 1 it fails. If it starts from 2 it's fine.
## I think the problem is that on base link it doesn't have the parameters (page etc.)
## That's why I will click the next page and I will turn back to one


nextPage = function(){
  nextButton = remDr$findElement(using = "css selector", ".listPaginationWrap .next a")
  nextButton$sendKeysToElement(list(key = "enter"))
}

prevPage = function(){
  prevButton = remDr$findElement(using = "css selector", ".listPaginationWrap .prev a")
  prevButton$clickElement() 
}

acceptCookies = function(){
  acceptCookiesButton = remDr$findElement(using = "css selector", ".cookiebar__cont .cookiebar__opts__item")
  acceptCookiesButton$clickElement()
}
acceptCookies()
nextPage()
prevPage()


# get_class = function(){
#   
#   element <- remDr$waitElementVisible("branches-tab")
#   
#   t = remDr$findElement(using = "xpath", "//*[@id='branches-tab']")
#   t$clickElement()
#   
#   classbusiness <- remDr$findElements(using = "xpath", value = "//*[@id = 'branches']/*[@class = 'insuranceCompany__tableWrap']/table/tbody/tr/td[2]")
#   text_list <- list()
#   
#   # Loop through each element and get the text
#   for (element in classbusiness) {
#     text <- element$getElementText()
#     text_list <- c(text_list, text)
#   }
#   
#   text_list = text_list |> unlist()
#   
#   # Print or use the text
#   return(text_list)
# }

get_ad = function(url){
  read_html(url)
  

  table = remDr$findElement(using = "css selector", ".insuranceCompany__table")
  address_element <- table$findChildElement(using = "xpath", "//tr[td[contains(text(), 'Address')]]//td[2]")
  
  if (is.null(table)){
    adress = NULL
    return(address)
  }
  # Check if the address element is found
  if (!is.null(address_element)) {
    address <- address_element$getElementText()
    return(address)
  } else {
    return("Address element not found")
  }
  Sys.sleep(10)
  return(address)
}

scrapePerPage = function(urlpage){
  
  page = read_html(urlpage)
  data = page %>%
    html_table() %>%
    .[[1]] %>%
    .[-nrow(.),]
  
  data$Link = page |>
    html_node("table") |>
    html_node("tbody") |>
    html_nodes(xpath = "//tr/td[1]") |>
    html_node("a") |>
    html_attr("href") %>%
    .[-length(.)] %>%
    paste("https://www.bankofgreece.gr", . , sep = "")
  
   num_rows <- length(remDr$findElements(using = "xpath", "//table/tbody/tr"))
  
  
  for (i in 1:num_rows){
    
    xpath <- paste("//table/tbody/tr[", i, "]/td[1]/a", sep = "")
    firstElementClick = remDr$findElement(using = "xpath", xpath)
    firstElementClick$clickElement()
    remDr$executeScript("window.scrollBy(0, window.innerHeight)")
    
    data$address[i] = get_ad(remDr$getCurrentUrl()[[1]])
    
    Sys.sleep(5)
    remDr$goBack()
    
    print(paste0("End of row ", i))
  }
  
  return(data)
}



data = NULL
new_data = NULL
for (url in urls){
  remDr$navigate(url)
  data = scrapePerPage(url)
  print(paste0("End of page"))
  Sys.sleep(5)

  new_data = bind_rows(new_data, data)
}

urls = NULL
for (i in 52:65){
  urls[i - 51] = paste0("https://www.bankofgreece.gr/en/main-tasks/supervision/private-insurance/insurance-undertakings/register?order=asc&page=", i)
}

for (url in urls){
  print(url)
}

new_data$address = as.character(new_data$address)
                                

write.csv(new_data, "GRins.csv")
