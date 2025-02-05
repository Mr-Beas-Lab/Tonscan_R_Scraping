library(RSelenium)
library(wdman)
library(netstat)

selenium()

selenium_object <- selenium(retcommand = T, check = F)

#google chrome
binman::list_versions("chromedriver")

remote_driver <- rsDriver(browser = "chrome",
                          chromever = "106.0.5249.21",
                          verbose = F,
                          port = free_port())

# Conecta al navegador
browser <- remote_driver$client
browser$open()

# Navega al sitio web
url <- "https://tonscan.org/jetton/EQC18yLE5Ad71VntcIwaMq_PwAW1o2-0CCoH_sTfcdRc7rWZ#holders"
browser$navigate(url)

# Opcional: Esperar a que la página cargue completamente
Sys.sleep(5)

# Realiza scroll para cargar todos los elementos
scroll_page <- function(driver, max_scrolls = 30, delay = 2) {
  prev_count <- 0
  for (i in 1:max_scrolls) {
    driver$executeScript("window.scrollTo(0, document.body.scrollHeight);")
    Sys.sleep(delay)
    current_count <- length(driver$findElements(using = "css selector", value = "span[timeago-id]"))
    if (current_count == prev_count) break
    prev_count <- current_count
  }
}
scroll_page(browser, max_scrolls = 30, delay = 3)

# Extraer address usando XPath
address_elements <- browser$findElements(using = "xpath", value = '//*[@id="tab_pane_holders"]/section/div/table/tbody/tr/td[1]/a')
address_data <- sapply(address_elements, function(element) {
  element$getElementAttribute("href")[[1]]
})

# Extraer balance usando XPath
balance_elements <- browser$findElements(using = "xpath", value = '//*[@id="tab_pane_holders"]/section/div/table/tbody/tr/td[2]')
balance_data <- sapply(balance_elements, function(element) {
  element$getElementText()[[1]]
})

# Imprime los datos extraídos
print(balance_data)

# Extraer pie usando XPath
pie_elements <- browser$findElements(using = "xpath", value = '//*[@id="tab_pane_holders"]/section/div/table/tbody/tr/td[3]')
pie_data <- sapply(pie_elements, function(element) {
  element$getElementText()[[1]]
})

# Imprime los datos extraídos
print(pie_data)

# Combinar los datos en un data.frame
holders_data <- data.frame(
  Address = address_data,
  Balance_MRB = balance_data,
  Pie = pie_data,
  stringsAsFactors = FALSE
)

# Imprimir los datos extraídos
print(holders_data)


# Cierra el navegador
browser$close()

remote_driver$server$stop()
