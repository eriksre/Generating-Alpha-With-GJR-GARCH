library(tidyverse)
library(janitor)
library(ggplot2)
library(zoo)
library(rugarch)
library(forecast)


df <- read_csv('data/filtered_ecmt3150_reef_unstacked.csv')
#df <- df[10000:12000,]




aggregate_timestamps <- function(df, interval_seconds = 10, agg_function = mean) {
  df <- df[5000:nrow(df),]
  # Convert timestamp to POSIXct if not already
  df$timestamp <- as.POSIXct(df$timestamp, tz="UTC")
  
  # Round timestamps to nearest interval
  df$timestamp_rounded <- round(as.numeric(df$timestamp)/interval_seconds)*interval_seconds
  df$timestamp_rounded <- as.POSIXct(df$timestamp_rounded, tz="UTC")
  
  # Group by rounded timestamp and calculate aggregation for all numeric columns
  df <- df %>%
    group_by(timestamp_rounded) %>%
    summarise(
      across(where(is.numeric), agg_function),
      across(where(is.character), first)
    ) %>%
    ungroup()
  
  # Reset timestamp column
  df$timestamp <- df$timestamp_rounded
  df$timestamp_rounded <- NULL
  
  return(df)
}

# Aggregate data to 10-second intervals using mean
df <- aggregate_timestamps(df, interval_seconds = 10, agg_function = mean)



df$bybit_mid = (df$Bybit_ask_price + df$Bybit_bid_price) / 2
df$xt_mid = (df$XT_ask_price + df$XT_bid_price) / 2
df$gateio_mid = (df$GateIO_ask_price_adjusted + df$GateIO_bid_price_adjusted) / 2

df$bybit_gate_arbitrage_coefficient = df$bybit_mid / df$gateio_mid
df$bybit_xt_arbitrage_coefficient = df$bybit_mid / df$xt_mid


df$bybit_mid_returns = c(NA, diff(log(df$bybit_mid)))
df$xt_mid_returns = c(NA, diff(log(df$xt_mid))) 
df$gateio_mid_returns = c(NA, diff(log(df$gateio_mid)))

# Calculate returns of arbitrage coefficients
df$bybit_gate_arbitrage_coefficient_returns = c(NA, diff(log(df$bybit_gate_arbitrage_coefficient)))
df$bybit_xt_arbitrage_coefficient_returns = c(NA, diff(log(df$bybit_xt_arbitrage_coefficient)))


# important
df <- na.omit(df)



#acf's show nothing
# acf(df$bybit_mid, main="ACF of Bybit Mid Price")
# acf(df$xt_mid, main="ACF of XT Mid Price") 
# acf(df$gateio_mid, main="ACF of GateIO Mid Price")



# # Create PACF plots for each mid price
pacf(df$bybit_mid, main="PACF of Bybit Mid Price")
pacf(df$xt_mid, main="PACF of XT Mid Price")
pacf(df$gateio_mid, main="PACF of GateIO Mid Price")
pacf(df$bybit_gate_arbitrage_coefficient, main="PACF of Bybit Gate Arbitrage Coefficient")
pacf(df$bybit_xt_arbitrage_coefficient, main="PACF of Bybit XT Arbitrage Coefficient")


# Create plot comparing bid/ask prices across exchanges
ggplot(df) +
  # Bybit prices
  geom_line(aes(x = seq_along(Bybit_bid_price), y = Bybit_bid_price, color = "Bybit Bid"), alpha = 0.8) +
  geom_line(aes(x = seq_along(Bybit_ask_price), y = Bybit_ask_price, color = "Bybit Ask"), alpha = 0.8) +
  
  # XT prices  
  geom_line(aes(x = seq_along(XT_bid_price), y = XT_bid_price, color = "XT Bid"), alpha = 0.8) +
  geom_line(aes(x = seq_along(XT_ask_price), y = XT_ask_price, color = "XT Ask"), alpha = 0.8) +
  
  # GateIO adjusted prices
  geom_line(aes(x = seq_along(GateIO_bid_price_adjusted), y = GateIO_bid_price_adjusted, color = "GateIO Bid"), alpha = 0.8) +
  geom_line(aes(x = seq_along(GateIO_ask_price_adjusted), y = GateIO_ask_price_adjusted, color = "GateIO Ask"), alpha = 0.8) +
  
  scale_color_manual(values = c(
    "Bybit Bid" = "blue",
    "Bybit Ask" = "purple", 
    "XT Bid" = "red",
    "XT Ask" = "orange",
    "GateIO Bid" = "green",
    "GateIO Ask" = "lightgreen"
  )) +
  labs(
    title = "Bid/Ask Prices Across Exchanges",
    x = "Time Sequence",
    y = "Price",
    color = "Price Type"
  ) +
  theme_minimal()




# Create plot comparing adjusted vs unadjusted GateIO prices visualisation
ggplot(df) +
  # Unadjusted GateIO prices
  geom_line(aes(x = seq_along(GateIO_bid_price), y = GateIO_bid_price, color = "GateIO Bid Unadjusted"), alpha = 0.8) +
  geom_line(aes(x = seq_along(GateIO_ask_price), y = GateIO_ask_price, color = "GateIO Ask Unadjusted"), alpha = 0.8) +
  
  # Adjusted GateIO prices
  geom_line(aes(x = seq_along(GateIO_bid_price_adjusted), y = GateIO_bid_price_adjusted, color = "GateIO Bid Adjusted"), alpha = 0.8) +
  geom_line(aes(x = seq_along(GateIO_ask_price_adjusted), y = GateIO_ask_price_adjusted, color = "GateIO Ask Adjusted"), alpha = 0.8) +
  
  scale_color_manual(values = c(
    "GateIO Bid Unadjusted" = "red",
    "GateIO Ask Unadjusted" = "red",
    "GateIO Bid Adjusted" = "blue", 
    "GateIO Ask Adjusted" = "blue"
  )) +
  labs(
    title = "GateIO Prices: Adjusted vs Unadjusted",
    x = "Time Sequence",
    y = "Price",
    color = "Price Type"
  ) +
  theme_minimal()




# Calculate new spread with adjusted prices - histogram
df$gateio_spread_adjusted = df$GateIO_ask_price_adjusted / df$GateIO_bid_price_adjusted
df$gateio_spread = df$GateIO_ask_price / df$GateIO_bid_price


# Create histogram comparing original vs adjusted spreads
histogram = ggplot(df) +
  geom_histogram(aes(x = gateio_spread, fill = "Original Spread"), alpha = 0.5, binwidth = 0.00002) +
  geom_histogram(aes(x = gateio_spread_adjusted, fill = "Adjusted Spread"), alpha = 0.5, binwidth = 0.00002) +
  scale_fill_manual(values = c("Original Spread" = "steelblue", "Adjusted Spread" = "darkred")) +
  labs(
    title = "Distribution of GateIO Bid-Ask Spreads: Original vs Adjusted",
    x = "Bid/Ask Ratio",
    y = "Frequency",
    fill = "Spread Type"
  ) +
  theme_minimal() +
  xlim(0.995, 1.005)

print(histogram)
