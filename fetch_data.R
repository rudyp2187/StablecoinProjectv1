library(httr)
library(jsonlite)
library(dplyr)
library(readr)
library(lubridate)
library(tibble)

get_json_ok <- function(url) {
  resp <- RETRY(
    "GET",
    url,
    user_agent("stablecoin-watchtower/0.3"),
    times = 4,
    pause_min = 1,
    pause_cap = 8
  )
  if (status_code(resp) != 200) stop(paste("HTTP", status_code(resp), "for", url))
  fromJSON(content(resp, "text", encoding = "UTF-8"), flatten = TRUE)
}

get_usd_stablecoin_category_id <- function() {
  url <- "https://api.coingecko.com/api/v3/coins/categories/list"
  cats <- get_json_ok(url)
  df <- as_tibble(cats)
  
  id_col <- if ("category_id" %in% names(df)) "category_id" else if ("id" %in% names(df)) "id" else NA_character_
  name_col <- if ("name" %in% names(df)) "name" else NA_character_
  
  if (is.na(id_col) || is.na(name_col)) stop("Unexpected categories list format")
  
  df2 <- df %>%
    transmute(
      cid = as.character(.data[[id_col]]),
      cname = tolower(as.character(.data[[name_col]]))
    )
  
  hit <- df2 %>% filter(cname == "usd stablecoin") %>% slice(1)
  if (nrow(hit) == 1) return(hit$cid)
  
  hit2 <- df2 %>% filter(grepl("usd stablecoin", cname, fixed = TRUE)) %>% slice(1)
  if (nrow(hit2) == 1) return(hit2$cid)
  
  "usd-stablecoin"
}

fetch_usd_stablecoins_markets <- function() {
  base <- "https://api.coingecko.com/api/v3/coins/markets"
  cat_id <- get_usd_stablecoin_category_id()
  pages <- 1:2
  
  out <- list()
  for (p in pages) {
    url <- modify_url(base, query = list(
      vs_currency = "usd",
      category = cat_id,
      order = "market_cap_desc",
      per_page = 250,
      page = p,
      sparkline = "false"
    ))
    
    x <- get_json_ok(url)
    df <- as_tibble(x) %>%
      transmute(
        id = as.character(id),
        name = as.character(name),
        symbol = toupper(as.character(symbol)),
        price = as.numeric(current_price),
        mcap_usd = as.numeric(market_cap),
        volume_usd = as.numeric(total_volume),
        circulating = suppressWarnings(as.numeric(circulating_supply))
      ) %>%
      mutate(
        circulating = ifelse(!is.na(circulating), circulating,
                             ifelse(!is.na(mcap_usd) & !is.na(price) & price > 0, mcap_usd / price, NA_real_))
      )
    
    out[[length(out) + 1]] <- df
    Sys.sleep(1)
  }
  
  bind_rows(out) %>%
    filter(!is.na(price), !is.na(circulating)) %>%
    distinct(id, .keep_all = TRUE) %>%
    arrange(desc(mcap_usd))
}

dir.create("data", showWarnings = FALSE)

today <- Sys.Date()
sc <- fetch_usd_stablecoins_markets() %>%
  mutate(
    date = today,
    peg_dev = abs(price - 1),
    peg_bp = peg_dev * 10000
  )

hist_path <- "data/stablecoins_history.csv"

if (!file.exists(hist_path)) {
  write_csv(sc, hist_path)
} else {
  old <- read_csv(hist_path, show_col_types = FALSE) %>% mutate(date = as.Date(date))
  old <- old %>% filter(date != today)
  out <- bind_rows(old, sc)
  write_csv(out, hist_path)
}

write_csv(sc, "data/stablecoins_latest.csv")
cat("Saved:", nrow(sc), "usd-stablecoins for", as.character(today), "\n")