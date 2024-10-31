library(tidyverse)
library(janitor)
library(ggplot2)
library(zoo)


# Read and prepare data
df <- read_csv("data/ecmt3150_reef_unstacked.csv")
funding_rates <- read_csv("data/merged_funding_rates.csv")

#df = df[0:200000,]

# Ensure timestamps are in correct format
df <- df %>%
  mutate(timestamp = as.POSIXct(timestamp, tz="UTC"))

funding_rates <- funding_rates %>%
  mutate(`Time(UTC)` = as.POSIXct(`Time(UTC)`, tz="UTC"))

# Clean join with funding rates - now looking backwards
df <- df %>%
  arrange(timestamp) %>%
  left_join(
    funding_rates %>% 
      select(`Time(UTC)`, Bybit_Rate, XT_Rate, GateIO_Rate) %>%
      arrange(`Time(UTC)`),
    by = join_by(closest(timestamp <= `Time(UTC)`))  # Changed >= to <= to look backwards
  ) %>%
  fill(Bybit_Rate, XT_Rate, GateIO_Rate, .direction = "up")  # Changed from "down" to "up"



# Apply funding rates to bid/ask prices for each exchange
df <- df %>%
  mutate(
    # Bybit adjustments 
    Bybit_bid_price = Bybit_bid_price * (1 + Bybit_Rate),
    Bybit_ask_price = Bybit_ask_price * (1 + Bybit_Rate),
    
    # XT adjustments
    XT_bid_price = XT_bid_price * (1 + XT_Rate), 
    XT_ask_price = XT_ask_price * (1 + XT_Rate),
    
    # GateIO adjustments
    GateIO_bid_price = GateIO_bid_price * (1 + GateIO_Rate),
    GateIO_ask_price = GateIO_ask_price * (1 + GateIO_Rate)
  )


# Check the results
print(df)

# Save the processed dataframe to CSV
write.csv(df, "data/processed_data_with_funding.csv", row.names = FALSE)



