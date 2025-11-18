# Load required packages
library(tidyverse)
library(lubridate)
library(PerformanceAnalytics)
library(tbl2xts)
library(fmxdat)

# Load the data
holdrets <- Hold_Rets

# First, let's examine the data structure
glimpse(holdrets)

# Check the date range and groups/sectors available
holdrets %>%
  summarise(
    min_date = min(date),
    max_date = max(date),
    n_groups = n_distinct(Group),
    n_sectors = n_distinct(Sector)
  )

# Check the groups and sectors
holdrets %>% count(Group)
holdrets %>% count(Sector)

# Step 1: Calculate peak-to-trough (maximum drawdown) by year for each size group
group_drawdowns <- holdrets %>%
  # Calculate daily weighted returns for each group
  group_by(date, Group) %>%
  summarise(
    Group_Return = sum(Return * Weight, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # Calculate cumulative returns for each group
  group_by(Group) %>%
  arrange(date) %>%
  mutate(
    Cumulative_Return = cumprod(1 + Group_Return) - 1
  ) %>%
  # Calculate annual maximum drawdown
  mutate(Year = year(date)) %>%
  group_by(Group, Year) %>%
  summarise(
    Max_Drawdown = PerformanceAnalytics::maxDrawdown(Cumulative_Return),
    Annual_Volatility = sd(Group_Return) * sqrt(252),
    .groups = "drop"
  ) %>%
  filter(Year >= 2015 & Year <= 2024)  # Focus on past decade

# Step 2: Calculate peak-to-trough by year for each sector
sector_drawdowns <- holdrets %>%
  # Calculate daily weighted returns for each sector
  group_by(date, Sector) %>%
  summarise(
    Sector_Return = sum(Return * Weight, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # Calculate cumulative returns for each sector
  group_by(Sector) %>%
  arrange(date) %>%
  mutate(
    Cumulative_Return = cumprod(1 + Sector_Return) - 1
  ) %>%
  # Calculate annual maximum drawdown
  mutate(Year = year(date)) %>%
  group_by(Sector, Year) %>%
  summarise(
    Max_Drawdown = PerformanceAnalytics::maxDrawdown(Cumulative_Return),
    Annual_Volatility = sd(Sector_Return) * sqrt(252),
    .groups = "drop"
  ) %>%
  filter(Year >= 2015 & Year <= 2024)

# Step 3: Visualization - Group drawdowns over time
g1 <- group_drawdowns %>%
  ggplot(aes(x = Year, y = Max_Drawdown, color = Group, group = Group)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  labs(
    title = "Annual Maximum Drawdown by Size Group",
    subtitle = "Peak-to-Trough Analysis (2015-2024)",
    x = "Year",
    y = "Maximum Drawdown",
    color = "Size Group"
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_x_continuous(breaks = 2015:2024) +
  fmxdat::theme_fmx() +
  fmxdat::fmx_cols()

# Step 4: Visualization - Sector drawdowns (top sectors)
top_sectors <- sector_drawdowns %>%
  group_by(Sector) %>%
  summarise(Mean_Drawdown = mean(Max_Drawdown)) %>%
  arrange(desc(Mean_Drawdown)) %>%
  head(6) %>%
  pull(Sector)

g2 <- sector_drawdowns %>%
  filter(Sector %in% top_sectors) %>%
  ggplot(aes(x = Year, y = Max_Drawdown, color = Sector, group = Sector)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  labs(
    title = "Annual Maximum Drawdown by Sector",
    subtitle = "Top 6 Most Volatile Sectors (2015-2024)",
    x = "Year",
    y = "Maximum Drawdown",
    color = "Sector"
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_x_continuous(breaks = 2015:2024) +
  fmxdat::theme_fmx() +
  fmxdat::fmx_cols()

# Step 5: Compare volatility across groups
g3 <- group_drawdowns %>%
  ggplot(aes(x = Group, y = Annual_Volatility, fill = Group)) +
  geom_boxplot(alpha = 0.7) +
  labs(
    title = "Annual Volatility Comparison by Size Group",
    subtitle = "Distribution across 2015-2024 period",
    x = "Size Group",
    y = "Annual Volatility"
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  fmxdat::theme_fmx() +
  fmxdat::fmx_fills() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Step 6: Statistical analysis - Are smaller stocks more volatile?
volatility_test <- group_drawdowns %>%
  group_by(Group) %>%
  summarise(
    Mean_Drawdown = mean(Max_Drawdown),
    Mean_Volatility = mean(Annual_Volatility),
    Median_Drawdown = median(Max_Drawdown),
    n_years = n()
  ) %>%
  arrange(Mean_Volatility)

# Step 7: Additional insights - Worst drawdown years
worst_years <- group_drawdowns %>%
  group_by(Year) %>%
  summarise(
    Avg_Drawdown = mean(Max_Drawdown),
    Worst_Group = Group[which.max(Max_Drawdown)],
    Worst_Drawdown = max(Max_Drawdown)
  ) %>%
  arrange(desc(Avg_Drawdown))

# Step 8: Rolling 1-year maximum drawdown analysis for groups
rolling_drawdowns <- holdrets %>%
  group_by(date, Group) %>%
  summarise(
    Group_Return = sum(Return * Weight, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(Group) %>%
  arrange(date) %>%
  mutate(
    Cumulative_Index = cumprod(1 + Group_Return)
  ) %>%
  group_by(Group) %>%
  mutate(
    Rolling_Max = RcppRoll::roll_max(Cumulative_Index, n = 252, fill = NA, align = "right"),
    Rolling_Drawdown = (Rolling_Max - Cumulative_Index) / Rolling_Max
  ) %>%
  filter(!is.na(Rolling_Drawdown))

# Plot rolling drawdowns
g4 <- rolling_drawdowns %>%
  ggplot(aes(x = date, y = Rolling_Drawdown, color = Group)) +
  geom_line(size = 0.8, alpha = 0.8) +
  labs(
    title = "Rolling 1-Year Maximum Drawdown by Size Group",
    subtitle = "Daily rolling window analysis",
    x = "Date",
    y = "Rolling Maximum Drawdown",
    color = "Size Group"
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  fmxdat::theme_fmx() +
  fmxdat::fmx_cols()

# Print key results
print("Volatility and Drawdown Summary by Size Group:")
print(volatility_test)

print("\nWorst Drawdown Years:")
print(worst_years)

# Display plots
g1
g2
g3
g4




