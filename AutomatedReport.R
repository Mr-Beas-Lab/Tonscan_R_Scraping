# Install needed packages
required_packages <- c("RSelenium", "wdman", "netstat", "tidyverse", "lubridate", "officer")
installed_packages <- rownames(installed.packages())
for (pkg in required_packages) {
  if (!pkg %in% installed_packages) {
    install.packages(pkg, dependencies = TRUE)
  }
}
 # Load needed packages
library(RSelenium)
library(wdman)
library(netstat)
library(tidyverse)
library(lubridate)
library(officer)

selenium()

selenium_object <- selenium(retcommand = T, check = F)

remote_driver <- rsDriver(browser = "chrome",
                          chromever = "106.0.5249.21",
                          verbose = F,
                          port = free_port())

# Connect to browser
browser <- remote_driver$client

# Go to website
url <- "https://tonscan.org/jetton/EQC18yLE5Ad71VntcIwaMq_PwAW1o2-0CCoH_sTfcdRc7rWZ#swaps"
browser$navigate(url)

# Wait for website to load
Sys.sleep(5)

# Scroll to the end of the website
scroll_page <- function(driver, max_scrolls = 100, delay = 5) {
  prev_count <- 0
  same_count_retries <- 0
  max_retries <- 50  # Retry limit if no new data is detected
  loading_selector <- ".loading"  # Replace with the selector for any loading spinner/message
  
  for (i in 1:max_scrolls) {
    # Scroll down by a small step
    driver$executeScript("window.scrollBy(0, 10000);")
    Sys.sleep(runif(1, delay - 1, delay + 1))  # Randomize the delay slightly
    
    # Wait for loading spinner (if exists)
    loading_elements <- driver$findElements(using = "css selector", value = loading_selector)
    if (length(loading_elements) > 0) {
      Sys.sleep(2)  # Allow extra time if a loading spinner is detected
    }
    
    # Count the number of loaded elements
    current_count <- length(driver$findElements(using = "css selector", value = "span[timeago-id]"))
    
    # Break if no new elements are detected for multiple iterations
    if (current_count == prev_count) {
      same_count_retries <- same_count_retries + 1
      if (same_count_retries >= max_retries) {
        message("No new elements detected after retries. Stopping scroll.")
        break
      }
    } else {
      same_count_retries <- 0  # Reset retries if new elements are found
    }
    
    prev_count <- current_count
    
    # Force scroll to bottom every few iterations to ensure triggers
    if (i %% 2 == 0) {
      driver$executeScript("window.scrollTo(0, document.body.scrollHeight);")
      Sys.sleep(delay)
    }
  }
  
  message("Scrolling completed. Total elements found: ", prev_count)
}



scroll_page(browser, max_scrolls = 5000, delay = 2)

#Extract MarketCap
MarketCap_element <- browser$findElement(using = "xpath", value = '//div[contains(@class, "jetton-stats-row")]')
MarketCap_data <- MarketCap_element$getElementText()[[1]]
MarketCap_value <- as.numeric(gsub("\\s", "", str_extract(MarketCap_data, "\\d[\\d\\s]*\\.\\d+")))

#Extract Holders
Holders_element <- browser$findElement(using = "css", value = ".jetton-meta .jetton-stats-row:nth-child(2)")
Holders_data <- Holders_element$getElementText()[[1]]
Holders_value <- as.numeric(str_extract(Holders_data, "\\d+"))

# Go to transactions URL
url <- "https://tonscan.org/jetton/EQC18yLE5Ad71VntcIwaMq_PwAW1o2-0CCoH_sTfcdRc7rWZ#transactions"
browser$navigate(url)

# Wait for website to load
Sys.sleep(5)

# Scroll to the page
scroll_page(browser, max_scrolls = 5000, delay = 2)

# Extraer transactions urls
transactions_elements <- browser$findElements(using = "xpath", value = '//td/a[@class="tx-table-cell-icon"]')
transactions_data <- sapply(transactions_elements, function(element) {
  element$getElementAttribute("href")[[1]]
})
unique_transactions <- unique(transactions_data)
num_unique_transactions <- length(unique_transactions)

# Close the browser
browser$close()
remote_driver$server$stop()


# Almacenar datos en un archivo CSV
file_path <- "tons_can_data.csv"
if (file.exists(file_path)) {
  existing_data <- read_csv(file_path)
  updated_data <- bind_rows(existing_data, tibble(date = Sys.Date(), MarketCap_value, Holders_value, num_unique_transactions))
} else {
  updated_data <- tibble(date = Sys.Date(), MarketCap_value, Holders_value, num_unique_transactions)
}

write_csv(updated_data, file_path)

# Crear reportes
# Gráfico para Market Cap
ggplot(updated_data, aes(x = date, y = MarketCap_value)) +
  geom_line(color = "blue") +
  geom_point(color = "blue") +
  labs(title = "Market Cap Over Time", x = "Date", y = "Market Cap Value") +
  theme_minimal()
ggsave("market_cap_report.png", width = 8, height = 6)

# Gráfico para Holders
ggplot(updated_data, aes(x = date, y = Holders_value)) +
  geom_line(color = "green") +
  geom_point(color = "green") +
  labs(title = "Holders Over Time", x = "Date", y = "Number of Holders") +
  theme_minimal()
ggsave("holders_report.png", width = 8, height = 6)

# Gráfico para Transactions
ggplot(updated_data, aes(x = date, y = num_unique_transactions)) +
  geom_line(color = "red") +
  geom_point(color = "red") +
  labs(title = "Transactions Over Time", x = "Date", y = "Number of Transactions") +
  theme_minimal()
ggsave("transactions_report.png", width = 8, height = 6)

# Crear un archivo Word y agregar los gráficos
doc <- read_docx()
doc <- body_add_par(doc, "Daily Report", style = "heading 1")
doc <- body_add_par(doc, paste("Report date:", Sys.Date()), style = "Normal")

# Agregar cada gráfico al documento
doc <- body_add_par(doc, "Market Cap", style = "heading 2")
doc <- body_add_img(doc, src = "market_cap_report.png", width = 6, height = 4, style = "centered")

doc <- body_add_par(doc, "Holders", style = "heading 2")
doc <- body_add_img(doc, src = "holders_report.png", width = 6, height = 4, style = "centered")

doc <- body_add_par(doc, "Transactions", style = "heading 2")
doc <- body_add_img(doc, src = "transactions_report.png", width = 6, height = 4, style = "centered")

# Guardar el archivo Word
print(doc, target = "tonscan_report.docx")
