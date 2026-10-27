# =============================================================================
# Retail Sales Forecasting & Plan-vs-Actual Variance Analysis (Clothing Retail)
# Data: Australian Bureau of Statistics (ABS) monthly retail turnover,
#       1982-2018, via the open-source R package tsibbledata (aus_retail).
# Author: Mayuresh Khopade
# =============================================================================

suppressPackageStartupMessages({
  library(forecast)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
})

set.seed(42)
dir.create("output", showWarnings = FALSE)

# ---- 1. Load and prepare data ----------------------------------------------
url <- "https://raw.githubusercontent.com/tidyverts/tsibbledata/master/data/aus_retail.rda"
if (!file.exists("data/aus_retail.rda")) {
  dir.create("data", showWarnings = FALSE)
  download.file(url, "data/aus_retail.rda", mode = "wb")
}
load("data/aus_retail.rda")
retail <- aus_retail
class(retail) <- "data.frame"                      # drop tsibble class
retail$Month <- as.Date(unclass(retail$Month), origin = "1970-01-01")  # yearmonth -> Date
retail <- retail[, c("State", "Industry", "Month", "Turnover")]  # Turnover in AUD million

# Focus series: clothing retailing, all of Australia (sum of the 8 states)
clothing <- retail %>%
  filter(Industry == "Clothing retailing") %>%
  group_by(Month) %>%
  summarise(Turnover = sum(Turnover), n_states = n(), .groups = "drop") %>%
  filter(n_states == 8) %>%                          # only months where every state reports
  arrange(Month)

cat("Clothing series:", format(min(clothing$Month)), "to", format(max(clothing$Month)),
    "-", nrow(clothing), "months\n")

start_year  <- as.numeric(format(min(clothing$Month), "%Y"))
start_month <- as.numeric(format(min(clothing$Month), "%m"))
y <- ts(clothing$Turnover, start = c(start_year, start_month), frequency = 12)

# ---- 2. Exploratory analysis ------------------------------------------------
annual <- clothing %>%
  mutate(Year = as.numeric(format(Month, "%Y"))) %>%
  group_by(Year) %>%
  summarise(Turnover = sum(Turnover), months = n(), .groups = "drop") %>%
  filter(months == 12) %>%
  mutate(YoY_pct = 100 * (Turnover / lag(Turnover) - 1))
write.csv(annual, "output/annual_turnover_yoy.csv", row.names = FALSE)

seasonal <- clothing %>%
  filter(Month >= as.Date("2014-01-01"), Month <= as.Date("2018-12-01")) %>%
  mutate(Year = format(Month, "%Y"), Mon = as.numeric(format(Month, "%m"))) %>%
  group_by(Year) %>% mutate(share = Turnover / sum(Turnover)) %>%
  group_by(Mon) %>% summarise(avg_share_pct = 100 * mean(share), .groups = "drop")
write.csv(seasonal, "output/seasonal_profile.csv", row.names = FALSE)

p1 <- ggplot(clothing, aes(Month, Turnover)) +
  geom_line(colour = "#2E6F40") +
  labs(title = "Australian clothing retail turnover (monthly)",
       x = NULL, y = "Turnover (AUD million)") +
  theme_minimal()
ggsave("output/01_turnover_trend.png", p1, width = 8, height = 4, dpi = 150)

p2 <- ggplot(seasonal, aes(factor(Mon, labels = month.abb), avg_share_pct)) +
  geom_col(fill = "#2E6F40") +
  labs(title = "Seasonality: average share of annual turnover by month (2014-2018)",
       x = NULL, y = "% of annual turnover") +
  theme_minimal()
ggsave("output/02_seasonality.png", p2, width = 8, height = 4, dpi = 150)

# ---- 3. Forecasting: rolling 12-month-ahead evaluation ---------------------
# Like an annual budget: at the end of each year, forecast the next 12 months.
# Origins: end of 2016 (-> plan 2017) and end of 2017 (-> plan 2018).
models <- list(
  "Seasonal naive (benchmark)" = function(tr) snaive(tr, h = 12),
  "ETS"                        = function(tr) forecast(ets(tr, lambda = 0), h = 12),
  "ARIMA"                      = function(tr) forecast(auto.arima(tr, lambda = 0), h = 12)
)

acc_rows <- list(); fc_store <- list()
for (yr in c(2017, 2018)) {
  tr <- window(y, end = c(yr - 1, 12))
  te <- window(y, start = c(yr, 1), end = c(yr, 12))
  for (m in names(models)) {
    fc <- models[[m]](tr)
    fc_store[[paste(m, yr)]] <- fc
    a <- accuracy(fc, te)["Test set", c("RMSE", "MAE", "MAPE")]
    acc_rows[[length(acc_rows) + 1]] <- data.frame(Model = m, Year = yr, t(a))
  }
}
acc_long <- do.call(rbind, acc_rows)
acc <- acc_long %>% group_by(Model) %>%
  summarise(across(c(RMSE, MAE, MAPE), ~ round(mean(.x), 2)), .groups = "drop") %>%
  arrange(MAPE)
print(acc_long); print(acc)
write.csv(acc_long, "output/model_accuracy_by_year.csv", row.names = FALSE)
write.csv(acc, "output/model_accuracy.csv", row.names = FALSE)
cat("ETS model (trained to 2017):", ets(window(y, end = c(2017, 12)), lambda = 0)$method, "\n")
cat("ARIMA model (trained to 2017):", as.character(auto.arima(window(y, end = c(2017, 12)), lambda = 0)), "\n")

best_name <- acc$Model[1]
cat("Best model by average MAPE:", best_name, "\n")

months_2018 <- seq(as.Date("2018-01-01"), by = "month", length.out = 12)
fc_df <- data.frame(
  Month  = months_2018,
  Actual = as.numeric(window(y, start = c(2018, 1), end = c(2018, 12))),
  `Seasonal naive` = as.numeric(fc_store[["Seasonal naive (benchmark) 2018"]]$mean),
  ETS    = as.numeric(fc_store[["ETS 2018"]]$mean),
  ARIMA  = as.numeric(fc_store[["ARIMA 2018"]]$mean),
  check.names = FALSE
)
hist_df <- data.frame(Month = clothing$Month, Turnover = clothing$Turnover) %>%
  filter(Month >= as.Date("2015-01-01"), Month < as.Date("2018-01-01"))
p3 <- fc_df %>%
  pivot_longer(-Month, names_to = "Series", values_to = "Turnover") %>%
  ggplot(aes(Month, Turnover, colour = Series)) +
  geom_line(data = hist_df, aes(Month, Turnover), colour = "black", inherit.aes = FALSE) +
  geom_line(aes(linetype = Series == "Actual"), linewidth = 0.8) +
  scale_linetype_manual(values = c(`TRUE` = "solid", `FALSE` = "dashed"), guide = "none") +
  scale_colour_manual(values = c(Actual = "black", ETS = "#2E6F40",
                                 ARIMA = "#D17A22", `Seasonal naive` = "grey60")) +
  labs(title = "2018 forecasts (made with data up to Dec 2017) vs. actual turnover",
       x = NULL, y = "Turnover (AUD million)", colour = NULL) +
  theme_minimal() + theme(legend.position = "bottom")
ggsave("output/03_forecast_vs_actual.png", p3, width = 8, height = 4.5, dpi = 150)

# ---- 4. Plan-vs-actual variance analysis 2018 (controlling view) -----------
# The best model's forecast made at the end of 2017 is the 2018 "plan";
# monthly variances beyond +/- 5 % are flagged for follow-up.
best_fc <- fc_store[[paste(best_name, 2018)]]
variance <- data.frame(Month = months_2018,
                       Plan = round(as.numeric(best_fc$mean), 1),
                       Actual = fc_df$Actual) %>%
  mutate(Variance     = round(Actual - Plan, 1),
         Variance_pct = round(100 * (Actual - Plan) / Plan, 1),
         Flag         = ifelse(abs(Variance_pct) > 5, "check", ""))
print(variance)
write.csv(variance, "output/plan_vs_actual_monthly_2018.csv", row.names = FALSE)

total <- variance %>% summarise(Plan = sum(Plan), Actual = sum(Actual)) %>%
  mutate(Variance = Actual - Plan, Variance_pct = round(100 * Variance / Plan, 1))
print(total)
write.csv(total, "output/plan_vs_actual_total_2018.csv", row.names = FALSE)
cat("Months flagged (|variance| > 5 %):", sum(variance$Flag == "check"), "of 12\n")

p4 <- ggplot(variance, aes(Month, Variance_pct, fill = Variance_pct > 0)) +
  geom_col() +
  geom_hline(yintercept = c(-5, 5), linetype = "dotted") +
  scale_fill_manual(values = c(`TRUE` = "#2E6F40", `FALSE` = "#B23A3A"), guide = "none") +
  labs(title = paste0("2018 plan-vs-actual variance by month (plan = ", best_name, ")"),
       subtitle = "Dotted lines: +/- 5 % tolerance",
       x = NULL, y = "Variance (%)") +
  theme_minimal()
ggsave("output/04_variance_by_month.png", p4, width = 8, height = 4, dpi = 150)

# Same logic per state for 2018: which regions beat or missed their plan?
state_var <- retail %>%
  filter(Industry == "Clothing retailing") %>%
  group_by(State) %>% arrange(Month, .by_group = TRUE) %>%
  group_modify(function(d, key) {
    s  <- ts(d$Turnover, start = c(as.numeric(format(min(d$Month), "%Y")),
                                   as.numeric(format(min(d$Month), "%m"))), frequency = 12)
    tr <- window(s, end = c(2017, 12))
    ac <- window(s, start = c(2018, 1), end = c(2018, 12))
    pl <- models[[best_name]](tr)$mean
    data.frame(Plan_2018 = round(sum(pl), 1), Actual_2018 = round(sum(ac), 1))
  }) %>%
  ungroup() %>%
  mutate(Variance_pct = round(100 * (Actual_2018 - Plan_2018) / Plan_2018, 1)) %>%
  arrange(Variance_pct)
print(state_var)
write.csv(state_var, "output/plan_vs_actual_by_state_2018.csv", row.names = FALSE)

# ---- 5. Segmentation of retail industries (k-means clustering) --------------
# Leaf industries only (aggregate categories would double count).
leaf <- c("Cafes, restaurants and catering services", "Takeaway food services",
          "Clothing retailing", "Footwear and other personal accessory retailing",
          "Department stores", "Electrical and electronic goods retailing",
          "Furniture, floor coverings, houseware and textile goods retailing",
          "Hardware, building and garden supplies retailing", "Liquor retailing",
          "Newspaper and book retailing", "Other recreational goods retailing",
          "Other retailing n.e.c.", "Other specialised food retailing",
          "Pharmaceutical, cosmetic and toiletry goods retailing",
          "Supermarket and grocery stores")

nat <- retail %>%
  filter(Industry %in% leaf, Month >= as.Date("2009-01-01"), Month <= as.Date("2018-12-01")) %>%
  group_by(Industry, Month) %>% summarise(Turnover = sum(Turnover), .groups = "drop")

features <- nat %>%
  mutate(Year = as.numeric(format(Month, "%Y")), Mon = as.numeric(format(Month, "%m"))) %>%
  group_by(Industry) %>%
  summarise(
    growth_pct_pa = 100 * ((sum(Turnover[Year == 2018]) / sum(Turnover[Year == 2009]))^(1/9) - 1),
    december_share_pct = 100 * sum(Turnover[Mon == 12]) / sum(Turnover),
    volatility_pct = 100 * sd(diff(log(Turnover), lag = 12)),   # sd of YoY log growth
    .groups = "drop")

X <- scale(features[, c("growth_pct_pa", "december_share_pct", "volatility_pct")])
km <- kmeans(X, centers = 3, nstart = 50)
features$cluster <- km$cluster
features <- features %>% mutate(across(where(is.numeric) & !cluster, ~ round(.x, 2))) %>%
  arrange(cluster, desc(growth_pct_pa))
print(as.data.frame(features))

short <- c("Cafes, restaurants and catering services" = "Cafes & restaurants",
           "Takeaway food services" = "Takeaway food",
           "Clothing retailing" = "Clothing",
           "Footwear and other personal accessory retailing" = "Footwear & accessories",
           "Department stores" = "Department stores",
           "Electrical and electronic goods retailing" = "Electronics",
           "Furniture, floor coverings, houseware and textile goods retailing" = "Furniture & houseware",
           "Hardware, building and garden supplies retailing" = "Hardware & garden",
           "Liquor retailing" = "Liquor",
           "Newspaper and book retailing" = "Newspapers & books",
           "Other recreational goods retailing" = "Recreational goods",
           "Other retailing n.e.c." = "Other retailing",
           "Other specialised food retailing" = "Specialised food",
           "Pharmaceutical, cosmetic and toiletry goods retailing" = "Pharmacy & cosmetics",
           "Supermarket and grocery stores" = "Supermarkets")
features$label <- short[features$Industry]
# Name clusters by their centre: highest December share = seasonal, lowest growth = declining
centres <- features %>% group_by(cluster) %>%
  summarise(g = mean(growth_pct_pa), d = mean(december_share_pct), .groups = "drop")
seasonal_c <- centres$cluster[which.max(centres$d)]
weak_c     <- setdiff(centres$cluster[order(centres$g)], seasonal_c)[1]
features$segment <- ifelse(features$cluster == seasonal_c, "Christmas-driven (high seasonality)",
                    ifelse(features$cluster == weak_c, "Stagnating / volatile",
                           "Steady growth, low seasonality"))
write.csv(features, "output/industry_clusters.csv", row.names = FALSE)

p5 <- ggplot(features, aes(growth_pct_pa, december_share_pct, colour = segment)) +
  geom_point(size = 3) +
  geom_text(aes(label = label), size = 2.8, vjust = -1, show.legend = FALSE,
            check_overlap = FALSE) +
  scale_x_continuous(expand = expansion(mult = 0.12)) +
  scale_y_continuous(expand = expansion(mult = 0.08)) +
  labs(title = "Retail industries clustered by growth, seasonality and volatility (k-means, 2009-2018)",
       x = "Average growth p.a. (%)", y = "December share of annual turnover (%)", colour = NULL) +
  theme_minimal() + theme(legend.position = "bottom")
ggsave("output/05_industry_clusters.png", p5, width = 10, height = 6, dpi = 150)

cat("\nDone. Results written to ./output\n")
