
table_data %>%
  summarize(
    Total_TON = sum(Value_TON, na.rm = TRUE),
    Total_MRB = sum(Value_MRB, na.rm = TRUE),
    Average_TON = mean(Value_TON, na.rm = TRUE),
    Average_MRB = mean(Value_MRB, na.rm = TRUE),
    Maximum_TON = max(Value_TON, na.rm = TRUE),
    Maximum_MRB = max(Value_MRB, na.rm = TRUE)
  )

ggplot(table_data, aes(x = Value_TON)) +
  geom_histogram(bins = 30, fill = "blue", alpha = 0.7) +
  scale_x_log10() +
  labs(title = "Distribution of TON swap values", x = "TON (log10)", y = "Frequency") +
  theme_minimal()


ggplot(table_data, aes(x = Value_MRB)) +
  geom_histogram(bins = 30, fill = "blue", alpha = 0.7) +
  scale_x_log10() +
  labs(title = "Distribution of MRB swap values", x = "MRB (log10)", y = "Frequency") +
  theme_minimal()


ggplot(table_data, aes(x = Value_TON, y = Value_MRB)) +
  geom_point(alpha = 0.7) +
  scale_x_log10() +
  scale_y_log10() +
  labs(title = "Relationship between TON and MRB", x = "TON (log10)", y = "MRB (log10)") +
  theme_minimal()

  
# Agregar por fecha (día)
daily_data <- table_data %>%
  mutate(Transaction_Date = as.Date(Transaction_Time)) %>%
  group_by(Transaction_Date) %>%
  summarize(
    Daily_TON = sum(Value_TON, na.rm = TRUE),
    Daily_MRB = sum(Value_MRB, na.rm = TRUE),
    Transactions = n()
  )

# Verificar los datos diarios
print(daily_data)

# Gráfico de transacciones diarias
ggplot(daily_data, aes(x = Transaction_Date)) +
  geom_line(aes(y = Daily_TON, color = "TON")) +
  geom_line(aes(y = Daily_MRB, color = "MRB")) +
  labs(title = "Daily Transactions: TON and MRB",
       x = "Date",
       y = "Total Value") +
  scale_color_manual(values = c("TON" = "blue", "MRB" = "red")) +
  theme_minimal()

# Agregar por semana
weekly_data <- table_data %>%
  mutate(Transaction_Week = floor_date(Transaction_Time, unit = "week")) %>%
  group_by(Transaction_Week) %>%
  summarize(
    Weekly_TON = sum(Value_TON, na.rm = TRUE),
    Weekly_MRB = sum(Value_MRB, na.rm = TRUE),
    Transactions = n()
  )

# Verificar los datos semanales
print(weekly_data)

# Gráfico de transacciones semanales
ggplot(weekly_data, aes(x = Transaction_Week)) +
  geom_bar(aes(y = Weekly_TON, fill = "TON"), stat = "identity", alpha = 0.7) +
  geom_bar(aes(y = Weekly_MRB, fill = "MRB"), stat = "identity", alpha = 0.7) +
  labs(title = "Weekly Transactions: TON and MRB",
       x = "Week",
       y = "Total Value") +
  scale_fill_manual(values = c("TON" = "blue", "MRB" = "red")) +
  theme_minimal()

# Calcular el ratio MRB / TON por día
table_data %>%
  mutate(Transaction_Date = as.Date(Transaction_Time)) %>%
  group_by(Transaction_Date) %>%
  summarize(
    Total_TON = sum(Value_TON, na.rm = TRUE),
    Total_MRB = sum(Value_MRB, na.rm = TRUE),
    MRB_per_TON = ifelse(Total_TON > 0, Total_MRB / Total_TON, NA)  # Evitar división entre 0
  ) %>%
  ggplot(aes(x = Transaction_Date, y = MRB_per_TON)) +
  geom_line(color = "purple", size = 1) +
  labs(
    title = "Daily MRB / TON Ratio",
    x = "Date",
    y = "MRB per TON"
  ) +
  theme_minimal()
