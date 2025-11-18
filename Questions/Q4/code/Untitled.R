# Load required libraries
pacman::p_load(
  tidyverse, lubridate, rugarch, tbl2xts, PerformanceAnalytics,
  ggthemes, RcppRoll, forecast, rmgarch, parallel, fmxdat, kableExtra
)

# Set theme
theme_set(fmxdat::theme_fmx())



# Load the data
cncy   <- cncy  # spot or index, all vs USD
Carry  <- cncy_Carry    # G10 carry environment
value  <- cncy_Value    # valuation metric
cncyIV <- cncyIV     # implied vol data
bbdxy  <- bbdxy        # broad dollar index






# Check data structure
print("Currency data structure:")
glimpse(cncy)
print("Carry data structure:")
glimpse(cncy_Carry)

# =============================================================================
# FIXED DATA PREPARATION
# =============================================================================

# Calculate log returns with proper date handling
cncy_rts <- cncy %>%  
  group_by(Name) %>%  
  arrange(date) %>% 
  mutate(
    dlogret = log(Price) - log(lag(Price)),
    scaledret = (dlogret - mean(dlogret, na.rm = TRUE))
  ) %>% 
  filter(!is.na(dlogret)) %>% 
  ungroup() %>% 
  mutate(Name = gsub("_Cncy", "", Name))

# Filter for post-2015 period
analysis_data <- cncy_rts %>% filter(date > as.Date("2015-01-01"))

# =============================================================================
# VOLATILITY RANKING ANALYSIS
# =============================================================================

# Calculate volatility rankings
volatility_comparison <- analysis_data %>%
  group_by(Name) %>%
  summarize(
    Simple_Volatility = sd(dlogret, na.rm = TRUE),
    MAD_Volatility = mad(dlogret, na.rm = TRUE),
    IQR_Volatility = IQR(dlogret, na.rm = TRUE)
  ) %>%
  mutate(
    Simple_Rank = rank(-Simple_Volatility),
    MAD_Rank = rank(-MAD_Volatility)
  ) %>%
  arrange(Simple_Rank)

# ZAR's position
zar_volatility <- volatility_comparison %>% 
  filter(Name == "SouthAfrica") %>%
  select(Name, Simple_Volatility, Simple_Rank)

# Display results
print("=== VOLATILITY RANKING ANALYSIS ===")
print(paste("ZAR Rank:", zar_volatility$Simple_Rank, "out of", nrow(volatility_comparison)))

top_volatile <- volatility_comparison %>%
  slice_head(n = 15) %>%
  select(Name, Simple_Volatility, Simple_Rank) %>%
  mutate(Simple_Volatility = round(Simple_Volatility, 6))

kable(top_volatile, caption = "Top 15 Most Volatile Currencies (2015-2023)")

# Visualize volatility ranking
volatility_comparison %>%
  slice_head(n = 15) %>%
  ggplot(aes(x = reorder(Name, Simple_Volatility), y = Simple_Volatility)) +
  geom_col(fill = "steelblue", alpha = 0.8) +
  coord_flip() +
  labs(
    title = "Volatility Ranking: ZAR vs Other Currencies",
    subtitle = "Standard deviation of log returns (2015-2023)",
    x = "", y = "Volatility"
  )

# =============================================================================
# GARCH ANALYSIS FOR ZAR
# =============================================================================

# Prepare ZAR data
zar_data <- analysis_data %>% 
  filter(Name == "SouthAfrica") %>%
  select(date, dlogret) %>%
  rename(Date = date, Returns = dlogret)

# Test for ARCH effects
arch_test <- Box.test(zar_data$Returns^2, type = "Ljung-Box", lag = 12)
print("=== ARCH EFFECTS TEST ===")
print(paste("Ljung-Box Test p-value:", round(arch_test$p.value, 6)))
print(paste("ARCH effects present:", arch_test$p.value < 0.05))

# Prepare data for GARCH
zar_xts <- zar_data %>% 
  select(Date, Returns) %>% 
  tbl_xts()

# Test multiple GARCH specifications
models <- c("sGARCH", "gjrGARCH", "eGARCH")
model.list <- list()

for (p in 1:length(models)) { 
  garch_spec <- ugarchspec(
    variance.model = list(model = models[p], garchOrder = c(1, 1)), 
    mean.model = list(armaOrder = c(1, 0), include.mean = TRUE), 
    distribution.model = "std"
  )
  
  garch_fit <- ugarchfit(spec = garch_spec, data = zar_xts) 
  model.list[[p]] <- garch_fit
}

names(model.list) <- models

# Compare models using information criteria
fit.mat <- sapply(model.list, infocriteria)
rownames(fit.mat) <- rownames(infocriteria(model.list[[1]]))

# Select best model
best_model_idx <- which.min(fit.mat["Akaike",])
best_model_name <- names(model.list)[best_model_idx]
best_model <- model.list[[best_model_idx]]

print("=== GARCH MODEL COMPARISON ===")
kable(fit.mat, caption = "Information Criteria for GARCH Models")

# Best model coefficients
best_coefs <- best_model@fit$matcoef
print(paste("Best Model:", best_model_name))
kable(best_coefs, caption = paste("Coefficients for", best_model_name, "Model"))

# Extract conditional volatility
# Robust method to extract GARCH volatility
sigma_best <- as.data.frame(sigma(best_model))
sigma_best$date <- as.Date(rownames(sigma_best))
rownames(sigma_best) <- NULL
colnames(sigma_best) <- c("sigma", "date")
sigma_best <- sigma_best %>% select(date, sigma)

# Now continue with the plotting
vol_comparison_data <- zar_data %>%
  mutate(Simple_Vol = Returns^2) %>%
  left_join(sigma_best, by = c("Date" = "date")) %>%
  mutate(GARCH_Vol = sigma^2)

# Create the comparison plot
ggplot(vol_comparison_data) + 
  geom_line(aes(x = Date, y = Simple_Vol), alpha = 0.6, color = "gray", size = 0.7) + 
  geom_line(aes(x = Date, y = GARCH_Vol), color = "red", size = 1.1, alpha = 0.8) + 
  labs(
    title = "ZAR Volatility: Raw vs GARCH-Smoothed",
    subtitle = paste("GARCH (", best_model_name, ") provides noise-reduced volatility estimate", sep = ""),
    x = "", 
    y = "Volatility (Squared Returns)"
  ) +
  theme_fmx() +
  theme(plot.title = element_text(face = "bold"))
# =============================================================================
# CARRY TRADE AND DOLLAR STRENGTH ANALYSIS
# =============================================================================

# Prepare carry trade data with proper date handling
carry_data <- cncy_Carry %>% 
  mutate(date = as.Date(date)) %>%
  filter(date > as.Date("2015-01-01")) %>% 
  arrange(date) %>%
  mutate(carry_ret = log(Price) - log(lag(Price))) %>% 
  filter(!is.na(carry_ret))

# Identify high carry trade periods
carry_threshold <- quantile(carry_data$carry_ret, 0.75, na.rm = TRUE)
high_carry_periods <- carry_data %>% 
  filter(carry_ret > carry_threshold) %>% 
  pull(date)

# Prepare dollar strength data
dollar_data <- bbdxy %>% 
  mutate(date = as.Date(date)) %>%
  filter(date > as.Date("2015-01-01")) %>% 
  arrange(date) %>%
  mutate(dollar_ret = log(Price) - log(lag(Price))) %>% 
  filter(!is.na(dollar_ret))

# Identify strong dollar periods
dollar_threshold <- quantile(dollar_data$dollar_ret, 0.75, na.rm = TRUE)
strong_dollar_periods <- dollar_data %>% 
  filter(dollar_ret > dollar_threshold) %>% 
  pull(date)

# Calculate ZAR performance during different regimes
zar_performance <- zar_data %>%
  mutate(
    Carry_Regime = ifelse(Date %in% high_carry_periods, "High_Carry", "Normal"),
    Dollar_Regime = ifelse(Date %in% strong_dollar_periods, "Strong_Dollar", "Normal")
  )

# Performance summary
performance_summary <- zar_performance %>%
  group_by(Carry_Regime, Dollar_Regime) %>%
  summarize(
    Mean_Return = mean(Returns, na.rm = TRUE),
    Volatility = sd(Returns, na.rm = TRUE),
    Observations = n(),
    .groups = "drop"
  )

print("=== REGIME PERFORMANCE ANALYSIS ===")
kable(performance_summary, caption = "ZAR Performance Across Different Regimes")

# =============================================================================
# ROLLING VOLATILITY COMPARISON
# =============================================================================

# Calculate rolling volatility for major currencies
major_currencies <- c("SouthAfrica", "EU", "UK", "Japan", "China")
rolling_vol <- analysis_data %>%
  filter(Name %in% major_currencies) %>%
  group_by(Name) %>%
  arrange(date) %>%
  mutate(
    Rolling_Vol = rollapply(dlogret, width = 30, FUN = sd, fill = NA, align = "right")
  ) %>%
  ungroup()

# Plot rolling volatility
rolling_plot <- ggplot(rolling_vol) +
  geom_line(aes(x = date, y = Rolling_Vol, color = Name), alpha = 0.7) +
  labs(
    title = "30-Day Rolling Volatility: ZAR vs Major Currencies",
    x = "", y = "Rolling Volatility"
  ) +
  theme(legend.position = "bottom")

print(rolling_plot)

# =============================================================================
# COMPREHENSIVE CONCLUSIONS
# =============================================================================

# Create summary table
summary_results <- tibble(
  Analysis = c("Volatility Rank", "Best GARCH Model", "ARCH Effects (p-value)",
               "High Carry Performance", "Strong Dollar Performance",
               "Volatility Persistence"),
  Result = c(
    paste0(zar_volatility$Simple_Rank, "/", nrow(volatility_comparison)),
    best_model_name,
    format.pval(arch_test$p.value, digits = 3),
    paste0(round(performance_summary$Mean_Return[performance_summary$Carry_Regime == "High_Carry"][1] * 100, 3), "%"),
    paste0(round(performance_summary$Mean_Return[performance_summary$Dollar_Regime == "Strong_Dollar"][1] * 100, 3), "%"),
    round(persistence(best_model), 3)
  ),
  Interpretation = c(
    "Lower rank = more volatile",
    "Lower AIC = better fit", 
    "p < 0.05 indicates ARCH effects",
    "Positive = performs well in carry trade periods",
    "Positive = performs well when dollar is strong",
    "Close to 1 = high volatility persistence"
  )
)

print("=== COMPREHENSIVE ZAR ANALYSIS RESULTS ===")
kable(summary_results, caption = "Summary of ZAR Volatility and Performance Analysis")

# Final conclusions
print("=== KEY CONCLUSIONS ===")

# Statement 1: Volatility Analysis
if(zar_volatility$Simple_Rank <= 5) {
  print("✓ STATEMENT 1 SUPPORTED: ZAR is indeed one of the most volatile currencies")
} else if(zar_volatility$Simple_Rank <= 10) {
  print("~ STATEMENT 1 PARTIALLY SUPPORTED: ZAR is relatively volatile but not among the most extreme")
} else {
  print("✗ STATEMENT 1 NOT SUPPORTED: ZAR is not among the most volatile currencies")
}

print(paste("ZAR volatility rank:", zar_volatility$Simple_Rank, "out of", nrow(volatility_comparison)))

# Statement 2: Performance during regimes
carry_perf <- performance_summary %>% 
  filter(Carry_Regime == "High_Carry") %>% 
  pull(Mean_Return) %>% mean(na.rm = TRUE) > 0

dollar_perf <- performance_summary %>% 
  filter(Dollar_Regime == "Strong_Dollar") %>% 
  pull(Mean_Return) %>% mean(na.rm = TRUE) > 0

if(carry_perf & dollar_perf) {
  print("✓ STATEMENT 2 FULLY SUPPORTED: ZAR performs well during both carry trade and strong dollar periods")
} else if(carry_perf | dollar_perf) {
  print("~ STATEMENT 2 PARTIALLY SUPPORTED: ZAR performs well in one of the regimes")
} else {
  print("✗ STATEMENT 2 NOT SUPPORTED: ZAR does not show superior performance in these regimes")
}

print("=== ANALYSIS COMPLETE ===")

