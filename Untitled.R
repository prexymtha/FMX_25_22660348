
# Load required libraries
library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)
library(zoo)
library(PerformanceAnalytics)

# Data Preparation - Align all datasets to ABC Fund's timeframe
benchmark_common <- Benchmark %>%
  filter(date >= as.Date("2012-07-31") & date <= as.Date("2025-10-31"))

active_managers_common <- Active_Managers %>%
  filter(date >= as.Date("2012-07-31") & date <= as.Date("2025-10-31"))

# Create analysis dataset
analysis_data <- ABC_Fund %>%
  select(date, ABC_Returns = Returns) %>%
  left_join(benchmark_common %>% select(date, Benchmark_Returns = Returns), by = "date") %>%
  left_join(active_managers_common %>% 
              group_by(date) %>%
              summarise(Peers_Avg_Returns = mean(Returns, na.rm = TRUE)),
            by = "date")

# 1. Calculate Rolling Beta (36-month window)
calculate_rolling_beta <- function(fund_returns, benchmark_returns, window = 36) {
  betas <- rep(NA, length(fund_returns))
  
  for(i in window:length(fund_returns)) {
    fund_window <- fund_returns[(i-window+1):i]
    bench_window <- benchmark_returns[(i-window+1):i]
    
    if(sum(!is.na(fund_window)) >= 24 && sum(!is.na(bench_window)) >= 24) {
      cov_matrix <- cov(fund_window, bench_window, use = "complete.obs")
      var_bench <- var(bench_window, na.rm = TRUE)
      if(var_bench > 0) {
        betas[i] <- cov_matrix / var_bench
      }
    }
  }
  return(betas)
}

# Calculate rolling beta for ABC Fund
abc_beta <- calculate_rolling_beta(analysis_data$ABC_Returns, 
                                   analysis_data$Benchmark_Returns)

# Calculate rolling beta for each active manager
active_betas <- active_managers_common %>%
  group_by(Name) %>%
  mutate(Beta = calculate_rolling_beta(Returns, 
                                       analysis_data$Benchmark_Returns[match(date, analysis_data$date)])) %>%
  ungroup() %>%
  filter(!is.na(Beta))

# Prepare data for beta visualization
beta_comparison_data <- analysis_data %>%
  mutate(ABC_Beta = abc_beta) %>%
  filter(!is.na(ABC_Beta)) %>%
  select(date, ABC_Beta) %>%
  left_join(active_betas %>% 
              select(date, Name, Peer_Beta = Beta), by = "date")

# 2. Create Boxplot of Rolling Betas
beta_boxplot <- function() {
  # Prepare data for boxplot
  boxplot_data <- bind_rows(
    data.frame(
      Fund = "ABC Fund",
      Beta = abc_beta[!is.na(abc_beta)]
    ),
    data.frame(
      Fund = "Industry Peers",
      Beta = active_betas$Beta
    )
  ) %>%
    filter(!is.na(Beta))
  
  ggplot(boxplot_data, aes(x = Fund, y = Beta, fill = Fund)) +
    geom_boxplot(alpha = 0.7, outlier.shape = NA) +
    geom_jitter(width = 0.2, alpha = 0.3, size = 0.8) +
    stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "white") +
    labs(title = "Distribution of Rolling 3-Year Betas: ABC Fund vs Industry Peers",
         subtitle = "Boxplot shows median, quartiles, and distribution of beta values over time",
         x = "", y = "Beta", 
         caption = "White diamond shows mean value") +
    scale_fill_manual(values = c("ABC Fund" = "red", "Industry Peers" = "lightblue")) +
    theme_minimal() +
    theme(legend.position = "none",
          plot.title = element_text(face = "bold"))
}

# 3. Create Spread Plot of Rolling Betas Over Time
beta_spread_plot <- function() {
  # Calculate peer beta statistics by date
  peer_beta_summary <- active_betas %>%
    group_by(date) %>%
    summarise(
      Peer_Median_Beta = median(Beta, na.rm = TRUE),
      Peer_Q1 = quantile(Beta, 0.25, na.rm = TRUE),
      Peer_Q3 = quantile(Beta, 0.75, na.rm = TRUE),
      Peer_Min = min(Beta, na.rm = TRUE),
      Peer_Max = max(Beta, na.rm = TRUE),
      Peer_Count = n()
    )
  
  # Combine with ABC Fund beta
  spread_data <- analysis_data %>%
    mutate(ABC_Beta = abc_beta) %>%
    filter(!is.na(ABC_Beta)) %>%
    left_join(peer_beta_summary, by = "date")
  
  ggplot(spread_data, aes(x = date)) +
    # Peer beta range
    geom_ribbon(aes(ymin = Peer_Q1, ymax = Peer_Q3), 
                fill = "lightblue", alpha = 0.5, 
                label = "Peer Interquartile Range") +
    geom_ribbon(aes(ymin = Peer_Min, ymax = Peer_Max), 
                fill = "lightblue", alpha = 0.2,
                label = "Peer Full Range") +
    # Peer median
    geom_line(aes(y = Peer_Median_Beta, color = "Median Peer Beta"), 
              size = 1, linetype = "dashed") +
    # ABC Fund beta
    geom_line(aes(y = ABC_Beta, color = "ABC Fund Beta"), 
              size = 1.2) +
    # Reference line at beta = 1
    geom_hline(yintercept = 1, linetype = "dotted", color = "black", alpha = 0.5) +
    
    labs(title = "Rolling 3-Year Beta Spread: ABC Fund vs Industry Peers Over Time",
         subtitle = "Shaded areas show peer beta distribution, dashed line shows median peer beta",
         x = "Date", y = "Beta", color = "") +
    scale_color_manual(values = c("ABC Fund Beta" = "red", 
                                  "Median Peer Beta" = "blue")) +
    theme_minimal() +
    theme(legend.position = "top",
          plot.title = element_text(face = "bold"))
}

# 4. Additional Performance Metrics and Visualizations

# Calculate cumulative returns
cumulative_returns <- analysis_data %>%
  mutate(
    ABC_Cumulative = cumprod(1 + replace_na(ABC_Returns, 0)),
    Benchmark_Cumulative = cumprod(1 + replace_na(Benchmark_Returns, 0)),
    Peers_Cumulative = cumprod(1 + replace_na(Peers_Avg_Returns, 0))
  )

# Create cumulative performance plot
cumulative_plot <- ggplot(cumulative_returns, aes(x = date)) +
  geom_line(aes(y = ABC_Cumulative, color = "ABC Fund"), size = 1.2) +
  geom_line(aes(y = Benchmark_Cumulative, color = "Benchmark"), size = 1) +
  geom_line(aes(y = Peers_Cumulative, color = "Average Peer"), size = 1) +
  labs(title = "Cumulative Performance Comparison",
       subtitle = "Growth of R1 investment over time",
       x = "Date", y = "Cumulative Return", color = "") +
  scale_color_manual(values = c("ABC Fund" = "red", 
                                "Benchmark" = "blue", 
                                "Average Peer" = "darkgreen")) +
  theme_minimal() +
  theme(legend.position = "top")

# Calculate key performance metrics
performance_metrics <- analysis_data %>%
  filter(complete.cases(.)) %>%
  summarise(
    # Annualized Returns
    ABC_Return = (prod(1 + ABC_Returns)^(12/n()) - 1) * 100,
    Benchmark_Return = (prod(1 + Benchmark_Returns)^(12/n()) - 1) * 100,
    Peers_Return = (prod(1 + Peers_Avg_Returns)^(12/n()) - 1) * 100,
    
    # Annualized Volatility
    ABC_Vol = sd(ABC_Returns) * sqrt(12) * 100,
    Benchmark_Vol = sd(Benchmark_Returns) * sqrt(12) * 100,
    Peers_Vol = sd(Peers_Avg_Returns) * sqrt(12) * 100,
    
    # Sharpe Ratio (assuming 0% risk-free rate for simplicity)
    ABC_Sharpe = ABC_Return / ABC_Vol,
    Benchmark_Sharpe = Benchmark_Return / Benchmark_Vol,
    Peers_Sharpe = Peers_Return / Peers_Vol,
    
    # Beta (full period)
    ABC_Beta = cov(ABC_Returns, Benchmark_Returns) / var(Benchmark_Returns),
    
    # Win Rates
    ABC_vs_Benchmark_WinRate = mean(ABC_Returns > Benchmark_Returns) * 100,
    ABC_vs_Peers_WinRate = mean(ABC_Returns > Peers_Avg_Returns) * 100
  )

# Create risk-return scatter plot
risk_return_data <- data.frame(
  Fund = c("ABC Fund", "Benchmark", "Average Peer"),
  Return = c(performance_metrics$ABC_Return, 
             performance_metrics$Benchmark_Return, 
             performance_metrics$Peers_Return),
  Risk = c(performance_metrics$ABC_Vol, 
           performance_metrics$Benchmark_Vol, 
           performance_metrics$Peers_Vol),
  Sharpe = c(performance_metrics$ABC_Sharpe, 
             performance_metrics$Benchmark_Sharpe, 
             performance_metrics$Peers_Sharpe)
)

risk_return_plot <- ggplot(risk_return_data, aes(x = Risk, y = Return, color = Fund)) +
  geom_point(aes(size = Sharpe), alpha = 0.7) +
  geom_text(aes(label = Fund), vjust = -0.8, hjust = 0.5, size = 3) +
  labs(title = "Risk-Return Profile: ABC Fund vs Benchmark and Peers",
       subtitle = "Bubble size represents Sharpe Ratio (risk-adjusted returns)",
       x = "Annualized Volatility (%)", y = "Annualized Return (%)") +
  scale_color_manual(values = c("ABC Fund" = "red", 
                                "Benchmark" = "blue", 
                                "Average Peer" = "darkgreen")) +
  theme_minimal()

# Generate all plots
cat("=== ABC FUND COMPREHENSIVE ANALYSIS ===\n\n")

cat("1. Beta Distribution Boxplot...\n")
print(beta_boxplot())

cat("2. Beta Spread Over Time...\n")
print(beta_spread_plot())

cat("3. Cumulative Performance...\n")
print(cumulative_plot)

cat("4. Risk-Return Scatter Plot...\n")
print(risk_return_plot)

# Print performance metrics
cat("\n=== KEY PERFORMANCE METRICS ===\n")
print(performance_metrics)

# Beta-specific insights
abc_final_beta <- performance_metrics$ABC_Beta
peer_median_beta <- median(active_betas$Beta, na.rm = TRUE)

cat("\n=== BETA ANALYSIS INSIGHTS ===\n")
cat(sprintf("ABC Fund Beta: %.3f\n", abc_final_beta))
cat(sprintf("Median Peer Beta: %.3f\n", peer_median_beta))
cat(sprintf("Beta Difference: %.3f\n", abc_final_beta - peer_median_beta))

if(abc_final_beta < peer_median_beta) {
  cat("• ABC Fund has LOWER market sensitivity than typical peers\n")
  cat("• This suggests the fund provides more than just market beta\n")
  cat("• Potentially offers better diversification benefits\n")
} else if(abc_final_beta > peer_median_beta) {
  cat("• ABC Fund has HIGHER market sensitivity than typical peers\n")
  cat("• This may indicate more aggressive market positioning\n")
} else {
  cat("• ABC Fund has similar market sensitivity to peers\n")
}

# Additional beta statistics
beta_stats <- active_betas %>%
  summarise(
    Peer_Beta_Mean = mean(Beta, na.rm = TRUE),
    Peer_Beta_SD = sd(Beta, na.rm = TRUE),
    Peer_Beta_Min = min(Beta, na.rm = TRUE),
    Peer_Beta_Max = max(Beta, na.rm = TRUE)
  )

cat(sprintf("\nPeer Beta Statistics:\n"))
cat(sprintf("Mean: %.3f, Std Dev: %.3f, Range: [%.3f, %.3f]\n",
            beta_stats$Peer_Beta_Mean, beta_stats$Peer_Beta_SD,
            beta_stats$Peer_Beta_Min, beta_stats$Peer_Beta_Max))

