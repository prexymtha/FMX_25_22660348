# Load packages using pacman
pacman::p_load(
  tidyverse,
  lubridate,
  readr,
  tbl2xts,
  PerformanceAnalytics,
  zoo,
  roll,
  fmxdat
)

# 1. READ THE DATA ---------------------------------------------------------

benchmark <- read_rds("C:/Users/pmnha/OneDrive/Desktop/22660348/data/Capped_SWIX.rds")
active_managers <- read_rds("C:/Users/pmnha/OneDrive/Desktop/22660348/data/Active_Managers.rds")
abc_fund <- read_rds("C:/Users/pmnha/OneDrive/Desktop/22660348/data/ABC_Fund.rds")

# Put them into a list to pass into the function
q1_data <- list(
  benchmark       = benchmark,
  active_managers = active_managers,
  abc_fund        = abc_fund
)

# 2. HELPER: ROLLING CAPM ALPHA/BETA --------------------------------------

# Local replacement for the missing fmxdat::roll_capm_alpha_beta
roll_capm_alpha_beta <- function(Ra, Rb, width = 36) {
  # Ra, Rb: xts objects with same dates, single column each
  
  # Align by common dates
  common_index <- zoo::index(Ra)[zoo::index(Ra) %in% zoo::index(Rb)]
  Ra <- Ra[common_index]
  Rb <- Rb[common_index]
  
  # Rolling CAPM using roll::roll_lm
  fit <- roll::roll_lm(
    x     = as.matrix(Rb),
    y     = as.matrix(Ra),
    width = width
  )
  
  # Extract coefficients: column 1 = alpha, column 2 = beta
  alpha_xts <- xts::xts(fit$coef[, 1], order.by = common_index)
  beta_xts  <- xts::xts(fit$coef[, 2], order.by = common_index)
  
  colnames(alpha_xts) <- "alpha"
  colnames(beta_xts)  <- "beta"
  
  list(
    alpha = alpha_xts,
    beta  = beta_xts
  )
}

# 3. QUESTION 1: ROLLING BETA ANALYSIS ------------------------------------

analyze_rolling_beta <- function(data) {
  pacman::p_load(tidyverse, tbl2xts, lubridate, roll, fmxdat)
  
  # Ensure common time period between ABC Fund and Active Managers
  start_date <- max(
    min(data$abc_fund$date, na.rm = TRUE),
    min(data$active_managers$date, na.rm = TRUE)
  )
  
  # Benchmark xts (single index series)
  benchmark_xts <- data$benchmark %>% 
    filter(date >= start_date) %>%
    tbl_xts(cols_to_xts = Returns, spread_by = Name)
  
  # ABC Fund xts
  abc_fund_xts <- data$abc_fund %>% 
    filter(date >= start_date) %>%
    tbl_xts(cols_to_xts = Returns, spread_by = Name)
  
  # Active managers xts (we will build per manager later)
  active_managers_tbl <- data$active_managers %>% 
    filter(date >= start_date)
  
  # Rolling beta for ABC Fund
  abc_rolling_beta <- roll_capm_alpha_beta(
    Ra    = abc_fund_xts,
    Rb    = benchmark_xts,
    width = 36
  )$beta %>% 
    xts_tbl()
  
  # Rolling beta for each active manager
  manager_names <- unique(active_managers_tbl$Name)
  
  all_manager_betas <- purrr::map_dfr(manager_names, function(manager) {
    manager_returns_xts <- active_managers_tbl %>% 
      filter(Name == manager) %>%
      tbl_xts(cols_to_xts = Returns)
    
    # Only compute if we have at least 36 observations
    if (nrow(manager_returns_xts) >= 36) {
      beta_xts <- roll_capm_alpha_beta(
        Ra    = manager_returns_xts,
        Rb    = benchmark_xts,
        width = 36
      )$beta
      
      beta_xts %>% 
        xts_tbl() %>% 
        mutate(Name = manager)
    } else {
      NULL
    }
  })
  
  # Peer beta cross section (IQR and median) at each date
  peer_beta_stats <- all_manager_betas %>%
    group_by(date) %>%
    summarise(
      peer_median_beta = median(beta, na.rm = TRUE),
      peer_q25         = quantile(beta, 0.25, na.rm = TRUE),
      peer_q75         = quantile(beta, 0.75, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Combine ABC Fund and peer stats
  beta_comparison <- abc_rolling_beta %>%
    rename(abc_beta = beta) %>%
    left_join(peer_beta_stats, by = "date")
  
  beta_comparison
}

# 4. VISUALISATION FOR QUESTION 1 -----------------------------------------

# Debug and fix the visualization function
visualize_beta_comparison_fixed <- function(beta_data) {
  
  # Ensure we have the required packages
  pacman::p_load(ggplot2, fmxdat, lubridate)
  
  # Filter out NA values that cause issues
  beta_data_clean <- beta_data %>%
    filter(!is.na(abc_beta), !is.na(peer_median_beta))
  
  cat("Cleaned data has", nrow(beta_data_clean), "rows after removing NAs\n")
  
  # Create a basic plot first to test
  p <- beta_data_clean %>%
    ggplot(aes(x = date)) +
    # Shaded area for interquartile range
    geom_ribbon(aes(ymin = peer_q25, ymax = peer_q75), 
                alpha = 0.3, fill = "steelblue", na.rm = TRUE) +
    # Line for median peer beta
    geom_line(aes(y = peer_median_beta, color = "Peer Median Beta"), 
              size = 1, alpha = 0.8, na.rm = TRUE) +
    # Line for ABC Fund beta
    geom_line(aes(y = abc_beta, color = "ABC Fund Beta"), 
              size = 1.5, alpha = 0.9, na.rm = TRUE) +
    # Use manual color scale to ensure proper labeling
    scale_color_manual(
      values = c("Peer Median Beta" = "steelblue", "ABC Fund Beta" = "darkorange"),
      breaks = c("ABC Fund Beta", "Peer Median Beta")
    ) +
    # Simple labels without complex formatting
    labs(
      title = "Rolling 3-Year Beta: ABC Fund vs Peer Group",
      subtitle = "Shaded area shows interquartile range (25th-75th percentile) of peer betas",
      x = "Date",
      y = "Rolling 36-Month Beta",
      color = "Series"
    ) +
    # Basic theme first
    theme_minimal() +
    theme(
      plot.title = element_text(size = 16, face = "bold"),
      plot.subtitle = element_text(size = 12),
      legend.position = "bottom"
    )
  
  # Try to apply fmxdat theme if available, otherwise keep basic theme
  if(requireNamespace("fmxdat", quietly = TRUE)) {
    p <- p + fmxdat::theme_fmx(
      title.size = fmxdat::ggpts(16), 
      subtitle.size = fmxdat::ggpts(12)
    )
  }
  
  # Return the plot without using finplot to avoid the error
  return(p)
}

# Try the fixed version
cat("Attempting fixed visualization...\n")
beta_plot_fixed <- visualize_beta_comparison_fixed(beta_data)
print(beta_plot_fixed)

