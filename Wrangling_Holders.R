library(tidyverse)

holders_data <- holders_data %>%
  mutate(Address = str_sub(Address, 29),
         Balance_MRB = str_replace_all(Balance_MRB, " ", ""),
         Balance_MRB = str_replace_all(Balance_MRB, "MRB", ""),
         Balance_MRB = as.numeric(Balance_MRB))

print(holders_data)

str(holders_data)

write_csv(holders_data, "holders_data.csv")
