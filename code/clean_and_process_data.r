library(tidyverse)
library(janitor)
library(ggplot2)
library(zoo)


df <- read_csv("data/ecmt3150_reef_unstacked.csv")
funding_rates <- read_csv("data/merged_funding_rates.csv")


#df = df[0:10000,]
# APPLY FUNDING RATES
#########################################################################

# 1. APPLY FUNDING RATES FIRST
df <- df %>%
  mutate(timestamp = as.POSIXct(timestamp, tz="UTC"))

funding_rates <- funding_rates %>%
  mutate(`Time(UTC)` = as.POSIXct(`Time(UTC)`, tz="UTC"))

# Apply funding rates (looking backwards)
df <- df %>%
  # Apply funding rates first
  arrange(timestamp) %>%
  left_join(
    funding_rates %>% 
      select(`Time(UTC)`, Bybit_Rate, XT_Rate, GateIO_Rate),
    by = join_by(closest(timestamp <= `Time(UTC)`))
  ) %>%
  fill(Bybit_Rate, XT_Rate, GateIO_Rate, .direction = "up") %>%
  # Calculate mark prices
  mutate(
    Bybit_bid_price = Bybit_bid_price * (1 + Bybit_Rate),
    Bybit_ask_price = Bybit_ask_price * (1 + Bybit_Rate),
    XT_bid_price = XT_bid_price * (1 + XT_Rate),
    XT_ask_price = XT_ask_price * (1 + XT_Rate),
    GateIO_bid_price = GateIO_bid_price * (1 + GateIO_Rate),
    GateIO_ask_price = GateIO_ask_price * (1 + GateIO_Rate)
  )

# 2. NOW CLEAN GATEIO DATA
#########################################################################




# (main algorithm bit)
# - Calculate 1000 period rolling average of Bybit/GateIO bid and Bybit/GateIO ask prices
# - Find the percentage difference of the current point from this rollong average
# - For bids, if the ratio is greater than the 35th percentile, replace with the last non-outlier value
# - For asks, if the ratio is less than the 65th percentile, replace with the last non-outlier value

# (negative spread bit)
# - Iterative algorithm to correct for negative spreads, and further outliers
# - If any negative spreads remain after initial cleaning (ask/bid < 1):
#   * Revert both ask and bid prices back to original values for those rows
#   * Increment bid threshold by 5 and decrement ask threshold by 5 (-5)
# - Calculate rolling average of bid/ask ratio using 50-point window
# - Calculate rolling percentiles of this average spread using 50-point window 
# - For points in bottom 10th percentile of spread:
#   * Calculate mean spread from rolling average
#   * symetrically increment bid and ask prices by the amount needed to match the mean spread of this rolling average

# - If any negative spreads remain after this process, repeat processbut using new thresholds
# - Repeat this loop 10 times
# - If any negative spreads remain after this process, revert to original prices for those rows (potentially could fix, but whatever)





df1 <- df %>%
  mutate(row_number = row_number()) %>%
  group_by(1) %>%  # Create a dummy group to use lag() function
  filter(
    GateIO_bid_price != lag(GateIO_bid_price) |
    GateIO_ask_price != lag(GateIO_ask_price) |
    row_number() == 1  # Keep the first row since it has no previous row to compare
  ) %>%
  ungroup() %>%
  mutate(row_number = row_number()) %>%
  column_to_rownames(var = "row_number")


# Fix the ratio calculations to use df1 values instead of df values
df1$bids_ratio = df1$Bybit_bid_price / df1$GateIO_bid_price
df1$gateio_spread = df1$GateIO_ask_price / df1$GateIO_bid_price
df1$bybit_spread = df1$Bybit_ask_price / df1$Bybit_bid_price
df1$asks_ratio = df1$Bybit_ask_price / df1$GateIO_ask_price


clean_price_data <- function(df, ratio_col, original_price_col, window_size=1000, percentile_threshold=50, is_ask=FALSE) {
  # Calculate smoothed ratio
  smoothed_ratio <- rollapply(ratio_col, width=window_size, FUN=mean, align="right", fill=NA)
  
  # Calculate difference from smoothed ratio
  diff_from_smoothed <- ratio_col / smoothed_ratio
  
  # Calculate rolling percentile ranks
  diff_percentile <- rollapply(diff_from_smoothed,
                              width=window_size,
                              FUN=function(x) {
                                if(length(x) == 0) return(NA)
                                rank(x)[length(x)]/length(x) * 100
                              },
                              align="right",
                              fill=NA)
  
  # Create adjusted price series
  price_adjusted <- original_price_col
  threshold <- percentile_threshold[1]  # Take first value if vector
  
  # Replace prices based on threshold, with different logic for asks vs bids
  if(length(price_adjusted) > 1) {
    for(i in 2:length(price_adjusted)) {
      if(!is.na(diff_percentile[i])) {
        if(is_ask && diff_percentile[i] < threshold) {
          # For ask prices, only use adjusted (previous) price if it's lower than original
          potential_new_price <- price_adjusted[i-1]
          price_adjusted[i] <- min(potential_new_price, original_price_col[i])
        } else if(!is_ask && diff_percentile[i] > threshold) {
          # For bid prices, only use adjusted (previous) price if it's higher than original
          potential_new_price <- price_adjusted[i-1]
          price_adjusted[i] <- max(potential_new_price, original_price_col[i])
        }
      }
    }
  }
  
  return(list(
    smoothed_ratio=smoothed_ratio,
    diff_from_smoothed=diff_from_smoothed,
    diff_percentile=diff_percentile,
    price_adjusted=price_adjusted
  ))
}

clean_prices_with_spread_check <- function(df, window_size=500, initial_bid_threshold=65, initial_ask_threshold=35, max_iterations=10) {
  # Initial cleaning as before
  bid_threshold <- rep(initial_bid_threshold, nrow(df))
  ask_threshold <- rep(initial_ask_threshold, nrow(df))
  
  for(iteration in 1:max_iterations) {
    # Clean bid prices
    bid_results <- clean_price_data(
      df,
      df$bids_ratio,
      df$GateIO_bid_price,
      window_size=window_size,
      percentile_threshold=bid_threshold[1]  # Use single value
    )
    
    # Clean ask prices
    ask_results <- clean_price_data(
      df,
      df$asks_ratio,
      df$GateIO_ask_price,
      window_size=window_size,
      percentile_threshold=ask_threshold[1],  # Use single value
      is_ask=TRUE
    )
    
    # Check for negative spreads
    temp_ask_prices <- ask_results$price_adjusted
    temp_bid_prices <- bid_results$price_adjusted
    negative_spreads <- temp_ask_prices / temp_bid_prices < 1
    
    # If no negative spreads, we're done
    if(!any(negative_spreads)) {
      break
    }
    
    # Adjust thresholds for rows with negative spreads
    bid_threshold[negative_spreads] <- bid_threshold[negative_spreads] + 5
    ask_threshold[negative_spreads] <- ask_threshold[negative_spreads] - 5
    
    # Ensure thresholds stay within reasonable bounds
    bid_threshold <- pmin(pmax(bid_threshold, 0), 100)
    ask_threshold <- pmin(pmax(ask_threshold, 0), 100)
  }
  
  # Calculate rolling average of bid/ask ratio
  spread_ratio <- ask_results$price_adjusted / bid_results$price_adjusted
  rolling_avg_spread <- rollapply(spread_ratio, width=50, FUN=mean, align="right", fill=NA)
  
  # Calculate percentiles of rolling average spread
  spread_percentiles <- rollapply(rolling_avg_spread, 
                                width=50, 
                                FUN=function(x) {
                                  if(length(x) == 0) return(NA)
                                  rank(x)[length(x)]/length(x) * 100
                                },
                                align="right",
                                fill=NA)
  
  # Identify points needing adjustment (bottom 10 percentile)
  needs_adjustment <- !is.na(spread_percentiles) & spread_percentiles < 10
  
  # Get the final prices from the initial cleaning
  ask_prices <- ask_results$price_adjusted
  bid_prices <- bid_results$price_adjusted
  
  # Adjust spreads where needed
  for(i in 2:length(ask_prices)) {
    if(needs_adjustment[i]) {
      # Get the average spread from the rolling window
      target_spread <- rolling_avg_spread[i]
      
      # Calculate the midpoint of current prices
      current_midpoint <- (ask_prices[i] + bid_prices[i]) / 2
      
      # Adjust prices to match rolling average spread while maintaining midpoint
      half_spread <- current_midpoint * (target_spread - 1) / 2
      ask_prices[i] <- current_midpoint + half_spread
      bid_prices[i] <- current_midpoint - half_spread
    }
  }
  
  return(list(
    ask_prices=ask_prices,
    bid_prices=bid_prices,
    final_bid_threshold=bid_threshold,
    final_ask_threshold=ask_threshold,
    iterations=iteration,
    spread_percentiles=spread_percentiles
  ))
}

# Apply the new cleaning function
results <- clean_prices_with_spread_check(df1)

# Assign results to dataframe
df1$GateIO_ask_price_adjusted <- results$ask_prices
df1$GateIO_bid_price_adjusted <- results$bid_prices

# Check for negative spreads and revert to original prices where they occur
negative_spreads <- df1$GateIO_ask_price_adjusted / df1$GateIO_bid_price_adjusted < 1
df1$GateIO_ask_price_adjusted[negative_spreads] <- df1$GateIO_ask_price[negative_spreads]
df1$GateIO_bid_price_adjusted[negative_spreads] <- df1$GateIO_bid_price[negative_spreads]

# Merge df1 back into df
df <- df %>%
  left_join(df1 %>% 
              select(timestamp, GateIO_ask_price_adjusted, GateIO_bid_price_adjusted),
            by = "timestamp") %>%
  arrange(timestamp) %>%
  mutate(
    GateIO_ask_price_adjusted = zoo::na.locf(GateIO_ask_price_adjusted, fromLast = FALSE),
    GateIO_bid_price_adjusted = zoo::na.locf(GateIO_bid_price_adjusted, fromLast = FALSE)
  )





# # # write.csv(df, "./data/filtered_ecmt3150_reef_unstacked.csv", row.names = FALSE)





# 
# # Create comparison plot of original vs adjusted GateIO bid and ask prices
# plotww = ggplot(df) +
#   geom_line(aes(x = seq_along(GateIO_bid_price), y = GateIO_bid_price, color = "Original Bid"), alpha = 0.8) +
#   geom_line(aes(x = seq_along(GateIO_bid_price_adjusted), y = GateIO_bid_price_adjusted, color = "Adjusted Bid"), alpha = 0.7) +
#   geom_line(aes(x = seq_along(GateIO_ask_price), y = GateIO_ask_price, color = "Original Ask"), alpha = 0.8) +
#   geom_line(aes(x = seq_along(GateIO_ask_price_adjusted), y = GateIO_ask_price_adjusted, color = "Adjusted Ask"), alpha = 0.7) +
#   scale_color_manual(values = c(
#     "Original Bid" = "red",
#     "Adjusted Bid" = "blue",
#     "Original Ask" = "red",
#     "Adjusted Ask" = "blue"
#   )) +
#   labs(
#     title = "GateIO Prices: Original vs Adjusted (Rows 1000-5000)",
#     x = "Time Sequence",
#     y = "Price",
#     color = "Price Type"
#   ) +
#   theme_minimal()
# 
# 
# print(plotww)
#
# Calculate new spread with adjusted prices
# df$gateio_spread_adjusted = df$GateIO_ask_price_adjusted / df$GateIO_bid_price_adjusted
# df$gateio_spread = df$GateIO_ask_price / df$GateIO_bid_price
# 
# 
# # Create histogram comparing original vs adjusted spreads
# histogram = ggplot(df) +
#   geom_histogram(aes(x = gateio_spread, fill = "Original Spread"), alpha = 0.5, binwidth = 0.00002) +
#   geom_histogram(aes(x = gateio_spread_adjusted, fill = "Adjusted Spread"), alpha = 0.5, binwidth = 0.00002) +
#   scale_fill_manual(values = c("Original Spread" = "steelblue", "Adjusted Spread" = "darkred")) +
#   labs(
#     title = "Distribution of GateIO Bid-Ask Spreads: Original vs Adjusted",
#     x = "Bid/Ask Ratio",
#     y = "Frequency",
#     fill = "Spread Type"
#   ) +
#   theme_minimal() +
#   xlim(0.995, 1.005)
# 
# print(histogram)
# 
# 
# 
# 


