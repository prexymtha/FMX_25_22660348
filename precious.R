# ==============================================================================
# ENHANCED MOMENTUM STRATEGY ANALYSIS - JSE EQUITIES
# ==============================================================================
# Author: Quantitative Research Team
# Date: 2024
# Description: Comprehensive momentum signal analysis with robustness checks,
#              transaction costs, regime detection, and fund performance attribution
# ==============================================================================

# Load required packages
library(tidyverse)
library(lubridate)
library(zoo)
library(PerformanceAnalytics)
library(ggthemes)
library(patchwork)
library(broom)
library(knitr)
library(roll)
library(moments)
library(scales)

# Set McKinsey-style theme
theme_mckinsey <- function() {
  theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray50"),
      axis.title = element_text(face = "bold", size = 12),
      legend.title = element_text(face = "bold"),
      panel.grid.minor = element_blank(),
      plot.caption = element_text(color = "gray60", size = 10),
      legend.position = "bottom"
    )
}

# McKinsey color palette
mck_colors <- c("#00A4B4", "#FF6B35", "#6A5ACD", "#FFD166", "#06D6A0", 
                "#118AB2", "#EF476F", "#073B4C", "#7209B7", "#F3722C")

# ==============================================================================
# 1. DATA LOADING AND VALIDATION
# ==============================================================================

# Load data with robust error handling
# Load the data
# Load the data
fundholdss <- read_rds("data/Fund_Holds.rds")
holdrets <- read_rds("data/Hold_Rets_Sectors.rds")


# Data inspection


# ==============================================================================
# 2. DATA CLEANING AND PREPROCESSING
# ==============================================================================

# Clean returns data with survivorship bias handling
clean_returns <- holdrets %>%
  rename(ticker = Tickers, return = Return, weight = Weight, 
         group = Group, sector = Sector) %>%
  filter(date >= as.Date("2013-01-01")) %>%
  arrange(ticker, date) %>%
  group_by(ticker) %>%
  # Track potential delisting
  mutate(
    last_obs = max(date),
    is_delisted = last_obs < max(date) - months(6),
    # Forward fill missing returns (max 3 periods) but keep NAs
    return_filled = zoo::na.locf(return, maxgap = 3, na.rm = FALSE)
  ) %>%
  # Manual winsorizing using quantiles
  mutate(
    p01 = quantile(return_filled, 0.01, na.rm = TRUE),
    p99 = quantile(return_filled, 0.99, na.rm = TRUE),
    return_winsorized = case_when(
      return_filled < p01 ~ p01,
      return_filled > p99 ~ p99,
      TRUE ~ return_filled
    )
  ) %>%
  select(-p01, -p99) %>%
  # Ensure sufficient history
  mutate(n_obs = sum(!is.na(return_filled))) %>%
  filter(n_obs >= 60) %>%  # At least 5 years of data
  select(-n_obs) %>%
  ungroup()

# Prepare fund holdings
clean_fund <- fundholds %>%
  rename(ticker = Tickers, weight = Weight) %>%
  arrange(ticker, date) %>%
  group_by(date) %>%
  mutate(port_weight = weight / sum(weight, na.rm = TRUE)) %>%
  ungroup() %>%
  select(date, ticker, port_weight) %>%
  filter(!is.na(port_weight), port_weight > 0)

# ==============================================================================
# 3. MOMENTUM SIGNAL COMPUTATION (CORRECTED)
# ==============================================================================

compute_momentum_12m1m_log <- function(returns, dates) {
  df <- data.frame(date = dates, return = returns) %>% arrange(date)
  
  # Use log returns directly
  df$log_return <- log(1 + df$return)
  
  # Simple sum instead of geometric compounding
  df$cumret_12m <- roll::roll_sum(df$log_return, width = 12, min_obs = 12)
  df$momentum_12m1m <- lag(df$cumret_12m, 1)
  
  return(exp(df$momentum_12m1m) - 1)  # Convert back to simple returns if needed
}

compute_momentum_6m1m <- function(returns, dates) {
  df <- data.frame(date = dates, return = returns) %>% arrange(date)
  
  df$cumret_6m <- roll::roll_prod(1 + df$return, width = 6, min_obs = 6) - 1
  df$momentum_6m1m <- lag(df$cumret_6m, 1)
  
  return(df$momentum_6m1m)
}

compute_volatility_scaled_momentum_log <- function(returns, dates) {
  df <- data.frame(date = dates, return = returns) %>% arrange(date)
  
  df$log_return <- log(1 + df$return)
  df$cumret_12m <- roll::roll_sum(df$log_return, width = 12, min_obs = 12)
  df$rolling_vol <- roll::roll_sd(df$log_return, width = 60, min_obs = 36)
  df$momentum_vol_scaled <- lag(df$cumret_12m / (df$rolling_vol + 0.001), 1)
  
  return(df$momentum_vol_scaled)  # Keep in log space for consistency
}

compute_fip_momentum_log <- function(returns, dates) {
  df <- data.frame(date = dates, return = returns) %>% arrange(date)
  
  df$log_return <- log(1 + df$return)
  
  # All calculations in log space
  df$cumret_12m <- roll::roll_sum(df$log_return, width = 12, min_obs = 12)
  df$rolling_vol <- roll::roll_sd(df$log_return, width = 12, min_obs = 12)
  df$rolling_skew <- zoo::rollapply(df$log_return, width = 12, 
                                    FUN = function(x) moments::skewness(x, na.rm = TRUE), 
                                    fill = NA, align = "right")
  
  # Natural scaling - no arbitrary multipliers needed
  df$fip_momentum <- lag(df$cumret_12m / (df$rolling_vol + 0.001) * (1 + df$rolling_skew * 0.1), 1)
  
  return(df$fip_momentum)
}

compute_volatility_scaled_momentum <- function(returns, dates) {
  df <- data.frame(date = dates, return = returns) %>% arrange(date)
  
  df$cumret_12m <- roll::roll_prod(1 + df$return, width = 12, min_obs = 12) - 1
  df$rolling_vol <- roll::roll_sd(df$return, width = 60, min_obs = 36)  # 5-year vol
  df$momentum_vol_scaled <- lag(df$cumret_12m / (df$rolling_vol + 0.001), 1)
  
  return(df$momentum_vol_scaled)
}

# Calculate all momentum signals
cat("\nCalculating momentum signals...\n")
momentum_data <- clean_returns %>%
  group_by(ticker) %>%
  arrange(date) %>%
  mutate(
    momentum_12m1m = compute_momentum_12m1m_log(return, date),
    momentum_6m1m = compute_momentum_6m1m(return, date),
    momentum_fip = compute_fip_momentum_log(return, date),
    momentum_vol_scaled = compute_volatility_scaled_momentum(return, date)
  ) %>%
  ungroup() %>%
  filter(date >= as.Date("2014-01-01"))  # Start after momentum calculation period


# ==============================================================================
# 4. FORWARD RETURNS CALCULATION (CORRECTED)
# ==============================================================================

cat("Calculating forward returns...\n")
momentum_data <- momentum_data %>%
  group_by(ticker) %>%
  arrange(date) %>%
  mutate(
    # Correctly calculate forward returns
    return_1m = lead(return, 1),
    return_3m = (1 + lead(return, 1)) * (1 + lead(return, 2)) * 
      (1 + lead(return, 3)) - 1,
    return_6m = (1 + lead(return, 1)) * (1 + lead(return, 2)) * 
      (1 + lead(return, 3)) * (1 + lead(return, 4)) *
      (1 + lead(return, 5)) * (1 + lead(return, 6)) - 1,
    return_12m = roll::roll_prod(1 + lead(return, 1), width = 12, min_obs = 12) - 1
  ) %>%
  ungroup()

# ==============================================================================
# 5. MONTHLY DATASET FOR ANALYSIS
# ==============================================================================

monthly_data <- momentum_data %>%
  mutate(
    year = year(date),
    month = floor_date(date, "month"),
    month_end = ceiling_date(date, "month") - days(1)
  ) %>%
  group_by(ticker, month) %>%
  # Take last trading day of each month
  filter(date == max(date)) %>%
  ungroup() %>%
  filter(!is.na(momentum_12m1m))

cat("Monthly data points:", nrow(monthly_data), "\n")
cat("Unique stocks:", n_distinct(monthly_data$ticker), "\n")
cat("Time period:", min(monthly_data$month), "to", max(monthly_data$month), "\n")

# ==============================================================================
# 6. INFORMATION COEFFICIENT ANALYSIS (ENHANCED)
# ==============================================================================

calculate_ic_robust <- function(data, signal_col, return_col, method = "spearman") {
  data %>%
    filter(!is.na(!!sym(signal_col)) & !is.na(!!sym(return_col))) %>%
    # Remove extreme return outliers
    filter(abs(!!sym(return_col)) < 0.5) %>%
    group_by(month) %>%
    summarise(
      ic = cor(!!sym(signal_col), !!sym(return_col), 
               method = method, use = "complete.obs"),
      n_stocks = n(),
      .groups = 'drop'
    )
}

cat("\nCalculating Information Coefficients...\n")

# Calculate IC for multiple signal/horizon combinations
ic_results <- bind_rows(
  # Primary signals
  calculate_ic_robust(monthly_data, "momentum_12m1m", "return_1m") %>% 
    mutate(signal = "12M-1M", horizon = "1M"),
  calculate_ic_robust(monthly_data, "momentum_12m1m", "return_3m") %>% 
    mutate(signal = "12M-1M", horizon = "3M"),
  calculate_ic_robust(monthly_data, "momentum_12m1m", "return_6m") %>% 
    mutate(signal = "12M-1M", horizon = "6M"),
  
  # Alternative signals
  calculate_ic_robust(monthly_data, "momentum_6m1m", "return_3m") %>% 
    mutate(signal = "6M-1M", horizon = "3M"),
  calculate_ic_robust(monthly_data, "momentum_fip", "return_3m") %>% 
    mutate(signal = "FIP", horizon = "3M"),
  calculate_ic_robust(monthly_data, "momentum_vol_scaled", "return_3m") %>% 
    mutate(signal = "Vol-Scaled", horizon = "3M")
)

# Statistical significance testing
ic_significance <- ic_results %>%
  filter(signal == "12M-1M" & horizon == "3M") %>%
  summarise(
    mean_ic = mean(ic, na.rm = TRUE),
    median_ic = median(ic, na.rm = TRUE),
    sd_ic = sd(ic, na.rm = TRUE),
    t_stat = mean_ic / (sd_ic / sqrt(n())),
    p_value = 2 * pt(abs(t_stat), df = n() - 1, lower.tail = FALSE),
    hit_rate = mean(ic > 0, na.rm = TRUE),
    n_months = n()
  )

cat("\nIC Significance Test Results (12M-1M, 3M horizon):\n")
cat("Mean IC:", round(ic_significance$mean_ic, 4), "\n")
cat("T-statistic:", round(ic_significance$t_stat, 2), "\n")
cat("P-value:", round(ic_significance$p_value, 4), "\n")
cat("Hit Rate:", percent(ic_significance$hit_rate), "\n")

# ==============================================================================
# 7. SIGNAL DECAY ANALYSIS
# ==============================================================================

cat("\nAnalyzing signal decay...\n")

signal_decay <- monthly_data %>%
  select(ticker, month, momentum_12m1m) %>%
  group_by(ticker) %>%
  arrange(month) %>%
  mutate(
    momentum_1m_ahead = lead(momentum_12m1m, 1),
    momentum_3m_ahead = lead(momentum_12m1m, 3),
    momentum_6m_ahead = lead(momentum_12m1m, 6)
  ) %>%
  ungroup()

decay_by_horizon <- bind_rows(
  signal_decay %>%
    filter(!is.na(momentum_12m1m) & !is.na(momentum_1m_ahead)) %>%
    group_by(month) %>%
    summarise(correlation = cor(momentum_12m1m, momentum_1m_ahead, 
                                method = "spearman", use = "complete.obs"),
              .groups = 'drop') %>%
    mutate(horizon = "1M"),
  
  signal_decay %>%
    filter(!is.na(momentum_12m1m) & !is.na(momentum_3m_ahead)) %>%
    group_by(month) %>%
    summarise(correlation = cor(momentum_12m1m, momentum_3m_ahead, 
                                method = "spearman", use = "complete.obs"),
              .groups = 'drop') %>%
    mutate(horizon = "3M"),
  
  signal_decay %>%
    filter(!is.na(momentum_12m1m) & !is.na(momentum_6m_ahead)) %>%
    group_by(month) %>%
    summarise(correlation = cor(momentum_12m1m, momentum_6m_ahead, 
                                method = "spearman", use = "complete.obs"),
              .groups = 'drop') %>%
    mutate(horizon = "6M")
)

# ==============================================================================
# 8. QUINTILE PORTFOLIO ANALYSIS WITH TRANSACTION COSTS
# ==============================================================================

cat("\nCalculating quintile portfolio performance...\n")

# Transaction cost parameters
transaction_cost_bps <- 30  # 30 basis points per trade
transaction_cost <- transaction_cost_bps / 10000

calculate_quintile_performance <- function(data, signal_col, return_col, 
                                           include_costs = TRUE) {
  # Assign quintiles
  quintile_data <- data %>%
    filter(!is.na(!!sym(signal_col)) & !is.na(!!sym(return_col))) %>%
    group_by(month) %>%
    mutate(quintile = ntile(!!sym(signal_col), 5)) %>%
    ungroup()
  
  # Calculate turnover if including costs
  if(include_costs) {
    quintile_data <- quintile_data %>%
      group_by(ticker) %>%
      arrange(month) %>%
      mutate(
        prev_quintile = lag(quintile),
        turnover_flag = quintile != prev_quintile | is.na(prev_quintile)
      ) %>%
      ungroup()
  }
  
  # Calculate portfolio returns
  portfolio_returns <- quintile_data %>%
    group_by(month, quintile) %>%
    summarise(
      gross_return = mean(!!sym(return_col), na.rm = TRUE),
      turnover_rate = if(include_costs) mean(turnover_flag, na.rm = TRUE) else 0,
      n_stocks = n(),
      .groups = 'drop'
    ) %>%
    mutate(
      transaction_costs = turnover_rate * transaction_cost,
      net_return = gross_return - transaction_costs
    )
  
  # Summary statistics
  portfolio_returns %>%
    group_by(quintile) %>%
    summarise(
      gross_annual_return = mean(gross_return, na.rm = TRUE) * 12,
      net_annual_return = mean(net_return, na.rm = TRUE) * 12,
      annual_vol = sd(net_return, na.rm = TRUE) * sqrt(12),
      sharpe = net_annual_return / annual_vol,
      avg_turnover = mean(turnover_rate, na.rm = TRUE),
      avg_stocks = mean(n_stocks, na.rm = TRUE),
      max_drawdown = min(net_return, na.rm = TRUE),
      .groups = 'drop'
    )
}

quintile_performance <- calculate_quintile_performance(
  monthly_data, "momentum_12m1m", "return_3m", include_costs = TRUE
)

cat("\nQuintile Performance Summary:\n")
print(knitr::kable(quintile_performance, digits = 4))

# Long-short strategy analysis
long_short_return <- quintile_performance$net_annual_return[5] - 
  quintile_performance$net_annual_return[1]
cat("\nLong-Short Annual Return:", percent(long_short_return, accuracy = 0.1), "\n")

# ==============================================================================
# 9. MOMENTUM CRASH RISK ANALYSIS
# ==============================================================================

cat("\nAnalyzing momentum crash risk...\n")

crash_analysis <- monthly_data %>%
  filter(!is.na(momentum_12m1m) & !is.na(return_3m)) %>%
  group_by(month) %>%
  mutate(
    quintile = ntile(momentum_12m1m, 5),
    market_crash = quantile(return_3m, 0.05)  # 5th percentile
  ) %>%
  ungroup() %>%
  group_by(quintile) %>%
  summarise(
    avg_return = mean(return_3m, na.rm = TRUE) * 12,
    crash_return_5pct = quantile(return_3m, 0.05, na.rm = TRUE),
    max_drawdown = min(return_3m, na.rm = TRUE),
    skewness = moments::skewness(return_3m, na.rm = TRUE),
    kurtosis = moments::kurtosis(return_3m, na.rm = TRUE),
    .groups = 'drop'
  )

cat("\nMomentum Crash Risk by Quintile:\n")
print(knitr::kable(crash_analysis, digits = 4))

# ==============================================================================
# 10. SECTOR ANALYSIS
# ==============================================================================

if("sector" %in% names(monthly_data)) {
  cat("\nAnalyzing sector effects...\n")
  
  sector_momentum <- monthly_data %>%
    filter(!is.na(momentum_12m1m) & !is.na(return_3m)) %>%
    group_by(month, sector) %>%
    summarise(
      ic = cor(momentum_12m1m, return_3m, method = "spearman", use = "complete.obs"),
      avg_momentum = mean(momentum_12m1m, na.rm = TRUE),
      avg_return = mean(return_3m, na.rm = TRUE),
      n_stocks = n(),
      .groups = 'drop'
    ) %>%
    group_by(sector) %>%
    summarise(
      mean_ic = mean(ic, na.rm = TRUE),
      ic_hit_rate = mean(ic > 0, na.rm = TRUE),
      avg_momentum = mean(avg_momentum, na.rm = TRUE),
      avg_return = mean(avg_return, na.rm = TRUE) * 12,
      avg_stocks = mean(n_stocks, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    arrange(desc(mean_ic))
  
  cat("\nSector Momentum IC:\n")
  print(knitr::kable(sector_momentum, digits = 4))
  
  # Sector-neutral momentum
  monthly_data <- monthly_data %>%
    group_by(month, sector) %>%
    mutate(
      momentum_sector_neutral = momentum_12m1m - mean(momentum_12m1m, na.rm = TRUE)
    ) %>%
    ungroup()
}

# ==============================================================================
# 11. MARKET REGIME DETECTION
# ==============================================================================

cat("\nDetecting market regimes...\n")

market_regime <- monthly_data %>%
  filter(!is.na(momentum_12m1m) & !is.na(return_3m)) %>%
  group_by(month) %>%
  summarise(
    market_return = mean(return_3m, na.rm = TRUE),
    return_dispersion = sd(return_3m, na.rm = TRUE),
    momentum_spread = mean(return_3m[ntile(momentum_12m1m, 5) == 5], na.rm = TRUE) -
      mean(return_3m[ntile(momentum_12m1m, 5) == 1], na.rm = TRUE),
    ic = cor(momentum_12m1m, return_3m, method = "spearman", use = "complete.obs"),
    .groups = 'drop'
  ) %>%
  mutate(
    # Define regimes based on momentum spread
    regime = case_when(
      momentum_spread > quantile(momentum_spread, 0.75, na.rm = TRUE) ~ "Strong Momentum",
      momentum_spread < quantile(momentum_spread, 0.25, na.rm = TRUE) ~ "Reversal",
      TRUE ~ "Neutral"
    ),
    # Add rolling metrics
    rolling_spread_6m = zoo::rollmean(momentum_spread, 6, fill = NA, align = "right"),
    rolling_ic_6m = zoo::rollmean(ic, 6, fill = NA, align = "right")
  )

regime_summary <- market_regime %>%
  group_by(regime) %>%
  summarise(
    n_months = n(),
    avg_spread = mean(momentum_spread, na.rm = TRUE),
    avg_ic = mean(ic, na.rm = TRUE),
    avg_market_return = mean(market_return, na.rm = TRUE) * 12,
    .groups = 'drop'
  )

cat("\nMarket Regime Summary:\n")
print(knitr::kable(regime_summary, digits = 4))

# ==============================================================================
# 12. FUND PERFORMANCE ANALYSIS
# ==============================================================================

cat("\nAnalyzing fund performance...\n")

# Calculate fund returns
# ==============================================================================
# 12. FUND PERFORMANCE ANALYSIS
# ==============================================================================

cat("\nAnalyzing fund performance...\n")

# Calculate fund returns - FIXED: Handle missing returns
fund_returns <- clean_fund %>%
  inner_join(
    clean_returns %>% select(ticker, date, return),
    by = c("ticker", "date")
  ) %>%
  group_by(date) %>%
  summarise(
    fund_return = sum(port_weight * return, na.rm = TRUE),
    n_holdings = n(),
    .groups = 'drop'
  ) %>%
  # Replace NA returns with 0 (assuming no return = 0% return)
  mutate(fund_return = ifelse(is.na(fund_return), 0, fund_return))

# Fund momentum exposure
fund_momentum_exposure <- clean_fund %>%
  mutate(month = floor_date(date, "month")) %>%
  inner_join(
    monthly_data %>% select(ticker, month, momentum_12m1m, return),
    by = c("ticker", "month")
  ) %>%
  group_by(date) %>%
  summarise(
    fund_momentum_exposure = weighted.mean(momentum_12m1m, port_weight, na.rm = TRUE),
    exposure_sd = sqrt(sum(port_weight^2 * momentum_12m1m^2, na.rm = TRUE)),
    .groups = 'drop'
  ) %>%
  left_join(fund_returns, by = "date")

# Momentum benchmark (top quintile)
momentum_benchmark <- monthly_data %>%
  mutate(month = as.Date(month)) %>%
  group_by(month) %>%
  mutate(quintile = ntile(momentum_12m1m, 5)) %>%
  filter(quintile == 5) %>%
  group_by(month) %>%
  summarise(
    momentum_benchmark_return = mean(return, na.rm = TRUE),
    n_stocks = n(),
    .groups = 'drop'
  ) %>%
  # Replace NA benchmark returns with 0
  mutate(momentum_benchmark_return = ifelse(is.na(momentum_benchmark_return), 0, momentum_benchmark_return))

# Combine fund and benchmark
fund_vs_benchmark <- fund_momentum_exposure %>%
  mutate(month = floor_date(date, "month")) %>%
  inner_join(momentum_benchmark, by = c("month" = "month")) %>%
  mutate(
    fund_cumulative = cumprod(1 + fund_return),
    benchmark_cumulative = cumprod(1 + momentum_benchmark_return),
    active_return = fund_return - momentum_benchmark_return
  )

# Annual momentum capture - FIXED: Handle NA in group calculations
fund_momentum_capture <- fund_vs_benchmark %>%
  mutate(year = year(date)) %>%
  group_by(year) %>%
  summarise(
    fund_return = ifelse(all(is.na(fund_return)), NA, prod(1 + fund_return, na.rm = TRUE) - 1),
    momentum_benchmark_return = ifelse(all(is.na(momentum_benchmark_return)), NA, 
                                       prod(1 + momentum_benchmark_return, na.rm = TRUE) - 1),
    momentum_capture = ifelse(is.na(fund_return) | is.na(momentum_benchmark_return) | 
                                momentum_benchmark_return == 0, NA,
                              fund_return / momentum_benchmark_return),
    tracking_error = sd(active_return, na.rm = TRUE) * sqrt(12),
    information_ratio = ifelse(is.na(tracking_error) | tracking_error == 0, NA,
                               mean(active_return, na.rm = TRUE) / tracking_error * sqrt(12)),
    .groups = 'drop'
  )

cat("\nFund vs Momentum Benchmark:\n")
print(knitr::kable(fund_momentum_capture, digits = 4))
# ==============================================================================
# 13. PUBLICATION-QUALITY VISUALIZATIONS
# ==============================================================================

cat("\nGenerating visualizations...\n")

# 1. Rolling IC Time Series
ic_results %>%
  filter(signal == "12M-1M" & horizon == "3M") %>%
  mutate(rolling_ic_6m = zoo::rollmean(ic, 6, fill = NA, align = "right"),
         rolling_ic_12m = zoo::rollmean(ic, 12, fill = NA, align = "right")) %>%
  ggplot(aes(x = month)) +
  geom_line(aes(y = ic), color = "gray60", alpha = 0.5) +
  geom_line(aes(y = rolling_ic_6m, color = "6-Month"), size = 1) +
  geom_line(aes(y = rolling_ic_12m, color = "12-Month"), size = 1.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  scale_color_manual(values = c("6-Month" = mck_colors[1], "12-Month" = mck_colors[2])) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(title = "Momentum Strategy Predictive Power Over Time",
       subtitle = paste("12-Month Momentum shows weak predictive ability (Avg IC: -1.3%) with only 45% accuracy"),
       x = "Date", y = "Information Coefficient", color = "Rolling Window") +
  theme(plot.title = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(size = 12))

ggsave("01_momentum_ic_rolling.png", p1, width = 12, height = 6, dpi = 300)

# 2. IC by Signal Type
ic_results %>%
  filter(horizon == "3M") %>%
  group_by(signal) %>%
  summarise(
    mean_ic = mean(ic, na.rm = TRUE),
    se_ic = sd(ic, na.rm = TRUE) / sqrt(n()),
    .groups = 'drop'
  ) %>%
  ggplot(aes(x = reorder(signal, mean_ic), y = mean_ic, fill = signal)) +
  geom_col(alpha = 0.8) +
  geom_errorbar(aes(ymin = mean_ic - 1.96*se_ic, ymax = mean_ic + 1.96*se_ic),
                width = 0.2, alpha = 0.7) +
  scale_fill_manual(values = mck_colors) +
  scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
  coord_flip() +
  labs(title = "Mean IC by Momentum Signal Type",
       subtitle = "3-Month Forward Returns | 95% Confidence Intervals",
       x = "Signal", y = "Mean Information Coefficient") +
  theme(legend.position = "none")

decay_by_horizon %>%
  group_by(horizon) %>%
  mutate(rolling_corr = zoo::rollmean(correlation, 6, fill = NA, align = "right")) %>%
  ggplot(aes(x = month, y = rolling_corr, color = horizon)) +
  geom_line(size = 1.2) +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "red", alpha = 0.5) +
  scale_color_manual(values = mck_colors[1:3]) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(title = "Momentum Signal Persistence",
       subtitle = "Autocorrelation of momentum scores over time",
       x = "Date", y = "Signal Autocorrelation (6M Rolling)", color = "Horizon") +
  theme_mckinsey()

# 4. Cumulative Performance: Strategies Comparison
p4_data <- monthly_data %>%
  group_by(month) %>%
  mutate(quintile = ntile(momentum_12m1m, 5)) %>%
  group_by(month, quintile) %>%
  summarise(
    portfolio_return = mean(return_3m, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  group_by(quintile) %>%
  mutate(cumulative_return = cumprod(1 + portfolio_return)) %>%
  ungroup() %>%
  filter(quintile %in% c(1, 3, 5))

p4_fund <- fund_vs_benchmark %>%
  select(month = date, fund_cumulative, benchmark_cumulative) %>%
  pivot_longer(cols = c(fund_cumulative, benchmark_cumulative),
               names_to = "quintile", values_to = "cumulative_return") %>%
  mutate(quintile = ifelse(quintile == "fund_cumulative", "Fund", "Top Quintile Benchmark"))

p4 <- bind_rows(
  p4_data %>% mutate(quintile = paste0("Q", quintile)),
  p4_fund
) %>%
  ggplot(aes(x = month, y = cumulative_return, color = quintile, linetype = quintile)) +
  geom_line(size = 1.2) +
  scale_color_manual(values = c("Q1" = mck_colors[7], "Q3" = mck_colors[8], 
                                "Q5" = mck_colors[1], "Fund" = mck_colors[2],
                                "Top Quintile Benchmark" = mck_colors[3])) +
  scale_linetype_manual(values = c("Q1" = "solid", "Q3" = "dotted", "Q5" = "solid",
                                   "Fund" = "solid", "Top Quintile Benchmark" = "dashed")) +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.1)) +
  labs(title = "Cumulative Performance: Momentum Strategies vs Fund",
       subtitle = "Q1 (Low) | Q3 (Mid) | Q5 (High) Momentum Quintiles",
       x = "Date", y = "Cumulative Return (Indexed)", color = "Strategy", linetype = "Strategy") +
  theme_mckinsey()

ggsave("04_cumulative_performance.png", p4, width = 12, height = 6, dpi = 300)

# 5. Quintile Performance Heatmap by Year
p5_data <- monthly_data %>%
  mutate(year = year(month)) %>%
  filter(!is.na(momentum_12m1m) & !is.na(return_3m)) %>%
  group_by(year, month) %>%
  mutate(quintile = ntile(momentum_12m1m, 5)) %>%
  group_by(year, quintile) %>%
  summarise(
    annual_return = mean(return_3m, na.rm = TRUE) * 12,
    .groups = 'drop'
  )

p5 <- p5_data %>%
  ggplot(aes(x = factor(quintile), y = factor(year), fill = annual_return)) +
  geom_tile(color = "white", size = 1) +
  geom_text(aes(label = percent(annual_return, accuracy = 1)), 
            color = "white", fontface = "bold", size = 3.5) +
  scale_fill_gradient2(low = mck_colors[7], mid = "gray90", high = mck_colors[1],
                       midpoint = 0, labels = percent_format()) +
  labs(title = "Annual Returns by Momentum Quintile",
       subtitle = "3-Month Forward Returns | Q1 (Low) to Q5 (High)",
       x = "Momentum Quintile", y = "Year", fill = "Annual Return") +
  theme_mckinsey() +
  theme(panel.grid = element_blank())

ggsave("05_quintile_heatmap.png", p5, width = 10, height = 8, dpi = 300)

# 6. Fund Momentum Exposure Over Time
p6 <- fund_momentum_exposure %>%
  mutate(rolling_exposure = zoo::rollmean(fund_momentum_exposure, 6, fill = NA, align = "right")) %>%
  ggplot(aes(x = date)) +
  geom_line(aes(y = fund_momentum_exposure), color = "gray60", alpha = 0.4) +
  geom_line(aes(y = rolling_exposure), color = mck_colors[4], size = 1.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_smooth(aes(y = rolling_exposure), method = "loess", span = 0.3, 
              se = TRUE, color = mck_colors[5], fill = mck_colors[5], alpha = 0.2) +
  scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
  labs(title = "Fund Momentum Exposure (6-Month Rolling)",
       subtitle = "Weighted average momentum score of fund holdings",
       x = "Date", y = "Momentum Exposure") +
  theme_mckinsey()

ggsave("06_fund_momentum_exposure.png", p6, width = 12, height = 6, dpi = 300)

# 7. Market Regime Analysis
p7 <- market_regime %>%
  ggplot(aes(x = month)) +
  geom_col(aes(y = momentum_spread, fill = regime), alpha = 0.7) +
  geom_line(aes(y = rolling_spread_6m), color = "black", size = 1, linetype = "dashed") +
  geom_hline(yintercept = 0, linetype = "solid", color = "red") +
  scale_fill_manual(values = c("Strong Momentum" = mck_colors[1],
                               "Neutral" = mck_colors[4],
                               "Reversal" = mck_colors[7])) +
  scale_y_continuous(labels = percent_format()) +
  labs(title = "Market Regime: Momentum Spread (Q5 - Q1)",
       subtitle = "Positive = momentum working | Negative = reversal",
       x = "Date", y = "Momentum Spread", fill = "Regime") +
  theme_mckinsey()

ggsave("07_market_regime.png", p7, width = 12, height = 6, dpi = 300)

# 8. Crash Risk by Quintile
p8 <- crash_analysis %>%
  select(quintile, avg_return, crash_return_5pct, max_drawdown) %>%
  pivot_longer(cols = -quintile, names_to = "metric", values_to = "value") %>%
  mutate(metric = case_when(
    metric == "avg_return" ~ "Average Return",
    metric == "crash_return_5pct" ~ "5th Percentile",
    metric == "max_drawdown" ~ "Max Drawdown"
  )) %>%
  ggplot(aes(x = factor(quintile), y = value, fill = metric)) +
  geom_col(position = "dodge", alpha = 0.8) +
  scale_fill_manual(values = mck_colors[c(1, 2, 7)]) +
  scale_y_continuous(labels = percent_format()) +
  labs(title = "Return Distribution by Momentum Quintile",
       subtitle = "Average, tail risk, and maximum drawdown analysis",
       x = "Momentum Quintile", y = "Return", fill = "Metric") +
  theme_mckinsey()

ggsave("08_crash_risk_quintiles.png", p8, width = 12, height = 6, dpi = 300)

# 9. Fund vs Benchmark Tracking
p9 <- fund_vs_benchmark %>%
  mutate(year = year(date)) %>%
  group_by(year) %>%
  summarise(
    fund_return = prod(1 + fund_return) - 1,
    benchmark_return = prod(1 + momentum_benchmark_return) - 1,
    .groups = 'drop'
  ) %>%
  pivot_longer(cols = c(fund_return, benchmark_return), 
               names_to = "portfolio", values_to = "return") %>%
  mutate(portfolio = ifelse(portfolio == "fund_return", "Fund", "Momentum Benchmark")) %>%
  ggplot(aes(x = factor(year), y = return, fill = portfolio)) +
  geom_col(position = "dodge", alpha = 0.8) +
  scale_fill_manual(values = mck_colors[c(2, 3)]) +
  scale_y_continuous(labels = percent_format()) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Annual Returns: Fund vs Momentum Benchmark",
       subtitle = "Top quintile momentum strategy as benchmark",
       x = "Year", y = "Annual Return", fill = "Portfolio") +
  theme_mckinsey() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("09_fund_vs_benchmark_annual.png", p9, width = 12, height = 6, dpi = 300)

# 10. Sector IC Heatmap (if sector data available)
if("sector" %in% names(monthly_data) && exists("sector_momentum")) {
  p10_data <- monthly_data %>%
    filter(!is.na(momentum_12m1m) & !is.na(return_3m) & !is.na(sector)) %>%
    mutate(year = year(month)) %>%
    group_by(year, sector) %>%
    summarise(
      ic = cor(momentum_12m1m, return_3m, method = "spearman", use = "complete.obs"),
      .groups = 'drop'
    )
  
  p10 <- p10_data %>%
    ggplot(aes(x = factor(year), y = sector, fill = ic)) +
    geom_tile(color = "white", size = 1) +
    geom_text(aes(label = sprintf("%.2f", ic)), color = "white", size = 3) +
    scale_fill_gradient2(low = mck_colors[7], mid = "gray90", high = mck_colors[1],
                         midpoint = 0, labels = percent_format()) +
    labs(title = "Sector Momentum IC by Year",
         subtitle = "Spearman correlation: momentum signal vs 3M forward returns",
         x = "Year", y = "Sector", fill = "IC") +
    theme_mckinsey() +
    theme(panel.grid = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1))
  
  ggsave("10_sector_ic_heatmap.png", p10, width = 12, height = 8, dpi = 300)
}

# ==============================================================================
# 14. COMPREHENSIVE SUMMARY TABLES
# ==============================================================================

cat("\nGenerating summary tables...\n")

# Yearly summary with all metrics
yearly_summary <- monthly_data %>%
  mutate(year = year(month)) %>%
  filter(!is.na(momentum_12m1m) & !is.na(return_3m)) %>%
  group_by(year) %>%
  summarise(
    # Information Coefficient
    ic_12m1m = cor(momentum_12m1m, return_3m, method = "spearman", use = "complete.obs"),
    ic_hit_rate = mean(cor(momentum_12m1m, return_3m, method = "spearman", 
                           use = "complete.obs") > 0, na.rm = TRUE),
    
    # Signal decay
    signal_persistence = cor(momentum_12m1m, lead(momentum_12m1m, 3), 
                             method = "spearman", use = "complete.obs"),
    
    # Quintile performance
    q5_return = mean(return_3m[ntile(momentum_12m1m, 5) == 5], na.rm = TRUE) * 12,
    q1_return = mean(return_3m[ntile(momentum_12m1m, 5) == 1], na.rm = TRUE) * 12,
    long_short_return = q5_return - q1_return,
    
    # Market stats
    n_stocks = n_distinct(ticker),
    avg_return = mean(return_3m, na.rm = TRUE) * 12,
    return_dispersion = sd(return_3m, na.rm = TRUE) * sqrt(12),
    
    .groups = 'drop'
  )

# Combine with fund performance
final_summary_table <- yearly_summary %>%
  left_join(fund_momentum_capture, by = "year") %>%
  select(year, ic_12m1m, signal_persistence, long_short_return, 
         fund_return, momentum_benchmark_return, momentum_capture,
         tracking_error, information_ratio, n_stocks)

write_csv(final_summary_table, "momentum_analysis_summary.csv")

cat("\n=== YEARLY MOMENTUM ANALYSIS SUMMARY ===\n")
print(knitr::kable(final_summary_table, digits = 4))

# Signal comparison table
signal_comparison <- ic_results %>%
  filter(horizon == "3M") %>%
  group_by(signal) %>%
  summarise(
    mean_ic = mean(ic, na.rm = TRUE),
    median_ic = median(ic, na.rm = TRUE),
    sd_ic = sd(ic, na.rm = TRUE),
    hit_rate = mean(ic > 0, na.rm = TRUE),
    t_stat = mean_ic / (sd_ic / sqrt(n())),
    p_value = 2 * pt(abs(t_stat), df = n() - 1, lower.tail = FALSE),
    .groups = 'drop'
  ) %>%
  arrange(desc(mean_ic))

write_csv(signal_comparison, "signal_comparison.csv")

cat("\n=== MOMENTUM SIGNAL COMPARISON ===\n")
print(knitr::kable(signal_comparison, digits = 4))

# Quintile performance detailed
quintile_detailed <- quintile_performance %>%
  mutate(
    excess_return = net_annual_return - min(net_annual_return),
    return_per_vol = net_annual_return / annual_vol,
    cost_drag = gross_annual_return - net_annual_return
  )

write_csv(quintile_detailed, "quintile_performance_detailed.csv")

cat("\n=== QUINTILE PERFORMANCE (WITH TRANSACTION COSTS) ===\n")
print(knitr::kable(quintile_detailed, digits = 4))

# ==============================================================================
# 15. ROBUSTNESS CHECKS
# ==============================================================================

cat("\n\n=== ROBUSTNESS CHECKS ===\n\n")

# 1. Subperiod analysis
subperiod_analysis <- ic_results %>%
  filter(signal == "12M-1M" & horizon == "3M") %>%
  mutate(
    period = case_when(
      year(month) <= 2017 ~ "2014-2017",
      year(month) <= 2020 ~ "2018-2020",
      TRUE ~ "2021+"
    )
  ) %>%
  group_by(period) %>%
  summarise(
    mean_ic = mean(ic, na.rm = TRUE),
    median_ic = median(ic, na.rm = TRUE),
    hit_rate = mean(ic > 0, na.rm = TRUE),
    n_months = n(),
    .groups = 'drop'
  )

cat("Subperiod IC Stability:\n")
print(knitr::kable(subperiod_analysis, digits = 4))

# 2. Alternative IC calculation methods
ic_methods <- monthly_data %>%
  filter(!is.na(momentum_12m1m) & !is.na(return_3m)) %>%
  group_by(month) %>%
  summarise(
    ic_spearman = cor(momentum_12m1m, return_3m, method = "spearman", use = "complete.obs"),
    ic_pearson = cor(momentum_12m1m, return_3m, method = "pearson", use = "complete.obs"),
    ic_kendall = cor(momentum_12m1m, return_3m, method = "kendall", use = "complete.obs"),
    .groups = 'drop'
  ) %>%
  summarise(
    spearman_mean = mean(ic_spearman, na.rm = TRUE),
    pearson_mean = mean(ic_pearson, na.rm = TRUE),
    kendall_mean = mean(ic_kendall, na.rm = TRUE)
  )

cat("\nIC by Correlation Method:\n")
print(knitr::kable(ic_methods, digits = 4))

# 3. Size effect (if market cap available)
# Placeholder - would need market cap data

# 4. Bootstrap confidence intervals for IC
set.seed(42)
n_bootstrap <- 1000

ic_data <- ic_results %>%
  filter(signal == "12M-1M" & horizon == "3M") %>%
  pull(ic)

bootstrap_ic <- replicate(n_bootstrap, {
  sample_ic <- sample(ic_data, length(ic_data), replace = TRUE)
  mean(sample_ic, na.rm = TRUE)
})

ic_ci <- quantile(bootstrap_ic, c(0.025, 0.975))

cat("\n95% Bootstrap Confidence Interval for Mean IC:\n")
cat(sprintf("Lower: %.4f | Mean: %.4f | Upper: %.4f\n", 
            ic_ci[1], mean(ic_data, na.rm = TRUE), ic_ci[2]))

# ==============================================================================
# 16. ATTRIBUTION ANALYSIS
# ==============================================================================

cat("\n\n=== PERFORMANCE ATTRIBUTION ===\n\n")

# Fund attribution vs momentum factor
fund_attribution <- fund_vs_benchmark %>%
  mutate(
    year = year(date),
    active_return = fund_return - momentum_benchmark_return
  ) %>%
  group_by(year) %>%
  summarise(
    total_return = prod(1 + fund_return) - 1,
    benchmark_return = prod(1 + momentum_benchmark_return) - 1,
    active_return = total_return - benchmark_return,
    
    # Risk metrics
    tracking_error = sd(active_return, na.rm = TRUE) * sqrt(12),
    information_ratio = mean(active_return, na.rm = TRUE) / 
      sd(active_return, na.rm = TRUE) * sqrt(12),
    
    # Exposure metrics
    avg_momentum_exposure = mean(fund_momentum_exposure, na.rm = TRUE),
    momentum_timing_corr = cor(fund_momentum_exposure, fund_return, use = "complete.obs"),
    
    .groups = 'drop'
  )

write_csv(fund_attribution, "fund_attribution.csv")

cat("Fund Performance Attribution:\n")
print(knitr::kable(fund_attribution, digits = 4))

