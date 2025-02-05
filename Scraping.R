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
url <- "https://tonscan.org/jetton/EQC18yLE5Ad71VntcIwaMq_PwAW1o2-0CCoH_sTfcdRc7rWZ#swaps"
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

#Extraer MarketCap
MarketCap_element <- browser$findElement(using = "xpath", value = '//div[contains(@class, "jetton-stats-row")]')
MarketCap_data <- MarketCap_element$getElementText()[[1]]

# Extraer solo los números y Convertir el resultado en numérico (opcional)
MarketCap_value <- as.numeric(gsub("\\s", "", str_extract(MarketCap_data, "\\d[\\d\\s]*\\.\\d+")))


# Extraer Age usando XPath
age_elements <- browser$findElements(using = "xpath", value = '//*[@id="tab_pane_swaps"]/section/div[1]/table/tbody/tr/td[2]')
age_data <- sapply(age_elements, function(element) {
  element$getElementText()[[1]]
})

# Extraer Value_1 usando XPath
value_elements <- browser$findElements(using = "xpath", value = '//*[@id="tab_pane_swaps"]/section/div[1]/table/tbody/tr/td[3]')
value_data <- sapply(value_elements, function(element) {
  element$getElementText()[[1]]
})

# Extraer Maker
# Usar XPath para apuntar al enlace dentro de la celda
maker_elements <- browser$findElements(
  using = "xpath", 
  value = '//*[@id="tab_pane_swaps"]/section/div[1]/table/tbody/tr/td[4]/span/a'
)

# Extraer el atributo 'href' de cada elemento
maker_data <- sapply(maker_elements, function(element) {
  element$getElementAttribute("href")[[1]]
})

# Combinar los datos en un data.frame
table_data <- data.frame(
  Age = age_data,
  Value = value_data,
  Maker = maker_data,
  stringsAsFactors = FALSE
)

# Imprimir los datos extraídos
print(table_data)

# Cierra el navegador
browser$close()

remote_driver$server$stop()
