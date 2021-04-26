# LIBRARIES ----

library(rvest)
library(tidyverse)
library(lubridate)
library(wesanderson)
library(ggtext)

# Scrape short sales data from the ASIC website
# daily_shorts <- read_csv("https://asic.gov.au/Reports/Daily/2021/04/RR20210412-001-SSDailyAggShortPos.csv")

url <- "http://asic.gov.au/regulatory-resources/markets/short-selling/short-position-reports-table/"
pg <- read_html(url)

# OBTAIN CSVs ----

csv_list <- pg %>% 
  html_nodes("a") %>%
  html_attr("href") %>% 
  str_subset(".csv")

# Link to latest is 2nd, based on current website configuration
latest <- csv_list[2]

url_begin <- "http://asic.gov.au"
latest_csv <- str_c(url_begin, latest, sep = "")

# Review
latest_csv

# READ IN DATA ----

# Cannot read file in with readr::read_csv. Use read.csv instead.

daily_shorts <- read.csv(latest_csv, 
                         header = TRUE, 
                         sep = '\t', 
                         fileEncoding = "utf-16")

# DATA WRANGLING 1 ----

daily_shorts_cleaned <- daily_shorts %>%
  as_tibble() %>%
  
  # remove white space after product.code (and any character cols)
  mutate(across(where(is.character), str_trim)) %>% 
  
  # rename required columns
  select(company = Product,
         ticker = Product.Code,
         short_ratio = X..of.Total.Product.in.Issue.Reported.as.Short.Positions) %>%
  
  # Create a label to place half way down geom_bar
  group_by(ticker) %>%
  mutate(label_y = cumsum(short_ratio) - 0.5 * short_ratio) %>% 
  ungroup()


# Add Trade Date - may use later to look at changes over time.
daily_shorts_cleaned <- daily_shorts_cleaned %>% 
  mutate(trade_date = str_split(latest, 'RR', simplify = TRUE)[,2] %>%
           str_sub(1,8) %>%
           as_date())
# Review
daily_shorts_cleaned

# VISUALISATION 1 ----

daily_shorts_cleaned %>% 
  arrange(desc(short_ratio)) %>% 
  slice(1:50) %>% 
  ggplot(aes(reorder(ticker, short_ratio), short_ratio, fill = short_ratio)) +
  geom_bar(stat = "identity") +
  geom_text(aes(y = label_y, label = str_glue("{round(short_ratio, 2)} %")),
            size = 3.25, colour = "white", vjust = 0.3) +
  
  scale_y_continuous(expand = c(0,0), breaks = seq(0, max(daily_shorts_cleaned$short_ratio), 3)) +
  
  coord_flip() +
  theme_light() +
  
  scale_fill_gradient(trans = "reverse", low = "#79402E", high = "#CCBA72") +
  labs(title = "Top 50 most Shorted ASX stocks", 
       x = "",
       y = "Short Positions / Shares Outstanding (%)",
       caption = "Source: @GrantChalmers | https://asic.gov.au/") +
  theme(plot.caption = element_text(size = 8, color = "gray50", face = "italic"),
        plot.background = element_rect(fill = 'antiquewhite', colour = 'antiquewhite'),
        panel.background = element_rect(fill = 'snow'),
        legend.position = "none")

# Save ggplot
ggsave("top_50_shorted_asx_stocks.png", plot = last_plot(), path = "images",
       width = 5, height = 8)


# OPTIONAL - archive data in RDS file ------------------------------------------

# Write initial tibble to rds file - only need to run once
# daily_shorts_cleaned %>% write_rds("data/daily_shorts.rds")
# This script could be setup to run as a cron job (or Windows Task Scheduler) every weekday afternoon.

# Read historical rds file
daily_shorts_history <- read_rds("data/daily_shorts.rds")

# Combine with latest
combined_tbl <- daily_shorts_history %>% bind_rows(daily_shorts_cleaned)

# Remove any duplicates
combined_tbl <- combined_tbl %>% distinct()

# Update rds with latest information
combined_tbl %>% write_rds("data/daily_shorts.rds")


# Gather mean week-over-week change
wk_over_wk_chg <- combined_tbl %>% 
  mutate(week = week(trade_date),
         year = isoyear(trade_date)) %>%
  group_by(ticker, year, week) %>%
  summarise(mean_short = mean(short_ratio)) %>% 
  arrange(ticker, year, week) %>%
  ungroup %>%
  mutate(change = mean_short - lag(mean_short))
  

# DATA WRANGLING 2 ----
# Generate a top and bottom tibble, then bind rows
# Too much data to view all

top_30 <- wk_over_wk_chg %>% 
  filter(week == max(week)) %>% 
  arrange(desc(change)) %>% 
  slice(1:30)

bottom_30 <- wk_over_wk_chg %>% 
  filter(week == max(week)) %>% 
  arrange(change) %>% 
  slice(1:30)

top_30_bottom_30 <- bind_rows(top_30, bottom_30)

# VISUALISATION 2 ----
# Plot

top_30_bottom_30 %>% 
  filter(week == max(week)) %>% 
  arrange(desc(change)) %>% 
  
  ggplot(aes(x = fct_reorder(ticker, change), y = change)) +
  geom_point(aes(colour = (change <= 0)), size = 2) + 
  geom_segment(aes(x = ticker, 
                   xend = ticker, 
                   y = 0, 
                   yend = change,
                   colour = (change <= 0)), size = 0.75) +
  scale_colour_manual(values = wes_palette("Moonrise2")) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 12)) +
  
  coord_flip() +
  theme_light() +
  
  labs(title = "Movement in Shorted ASX Stocks (<span style='color:#798E87'>top</span> 30 & <span style='color:#C27D38'>bottom</span> 30)",
       y = "Short Positions / Shares Outstanding Week over Week (mean) Change (%)", x = NULL,
       caption = "Source: @GrantChalmers | https://asic.gov.au/") +
  theme(plot.title = element_markdown(face = "bold", size = 11),
        # plot.title = element_text(size = 11, face = "bold"),
        axis.text.x = element_text(size = 10, angle = 00),
        axis.title.x = element_text(size = 9),
        axis.title = element_text(size = 11), legend.position = "none",
        plot.caption = element_text(size = 8, color = "gray50", face = "italic"),
        plot.background = element_rect(fill = 'antiquewhite', colour = 'antiquewhite'),
        panel.background = element_rect(fill = 'snow'),
        legend.title = element_blank())

# Save ggplot
ggsave("top_bottom_30_shorted_asx_stocks.png", plot = last_plot(), path = "images",
       width = 6, height = 10)


# REFERENCES ----
# https://rpubs.com/Cormac/313070