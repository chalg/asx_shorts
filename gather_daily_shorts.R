# LIBRARIES ----

library(rvest)
library(tidyverse)
library(lubridate)


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

# DATA WRANGLING ----

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

# PLOT ----

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
  labs(title = "Top 50 most shorted ASX stocks", 
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

# Read historical rds file
daily_shorts_history <- read_rds("data/daily_shorts.rds")

# Combine with latest
combined_tbl <- daily_shorts_history %>% bind_rows(daily_shorts_cleaned)

# Update rds with latest information
combined_tbl %>% write_rds("data/daily_shorts.rds")

# REFERENCES ----
# https://rpubs.com/Cormac/313070

