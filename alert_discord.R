# alert_discord.R
# Sends a single Discord alert message based on today's stablecoin snapshot.
# IMPORTANT:
# - Do NOT put Sys.setenv(...) or source("alert_discord.R") inside this file.
# - Store DISCORD_WEBHOOK in ~/.Renviron or set it in the Console before running.

# install.packages(c("readr","dplyr","httr","lubridate"))

library(readr)
library(dplyr)
library(httr)
library(lubridate)

# ------------------ CONFIG ------------------
HIST_PATH <- "data/stablecoins_history.csv"

PEG_THR_BP <- 50        # 50 bps = 0.50% off $1
SUPPLY_THR_PCT <- 2     # 2% day-over-day circulating change
TOP_N <- 10

# Safety filter so gold/commodity-pegged coins don't trigger off-peg vs $1
# If you are sure your fetch_data.R only pulls USD stablecoins, you can widen/disable this.
USD_PRICE_MIN <- 0.80
USD_PRICE_MAX <- 1.20

# Prevent spam if you accidentally run it repeatedly
COOLDOWN_SEC <- 90
LOCK_PATH <- "data/discord_last_sent.txt"

# Avoid re-sending the exact same message (even after cooldown)
LAST_MSG_PATH <- "data/discord_last_message.txt"
# --------------------------------------------

dir.create("data", showWarnings = FALSE)

# Cooldown lock
if (file.exists(LOCK_PATH)) {
  last_time <- suppressWarnings(as.POSIXct(readLines(LOCK_PATH, warn = FALSE), tz = "UTC"))
  if (!is.na(last_time)) {
    age_sec <- as.numeric(difftime(Sys.time(), last_time, units = "secs"))
    if (!is.na(age_sec) && age_sec < COOLDOWN_SEC) {
      cat("Cooldown active — not sending to Discord.\n")
      quit(save = "no")
    }
  }
}

webhook <- Sys.getenv("DISCORD_WEBHOOK")
if (webhook == "") stop("DISCORD_WEBHOOK is not set. Put it in ~/.Renviron and restart RStudio, or Sys.setenv() in Console.")

if (!file.exists(HIST_PATH)) stop("No history yet. Run fetch_data.R first to create data/stablecoins_history.csv")

h <- read_csv(HIST_PATH, show_col_types = FALSE) %>%
  mutate(date = as.Date(date))

max_date <- max(h$date, na.rm = TRUE)
prev_date <- max_date - 1

today <- h %>% filter(date == max_date)
yday  <- h %>% filter(date == prev_date) %>% select(id, circulating) %>% rename(circ_yday = circulating)

# Off-peg (only USD-like prices to avoid PAXG/XAUT/etc.)
offpeg <- today %>%
  filter(!is.na(price), price >= USD_PRICE_MIN, price <= USD_PRICE_MAX) %>%
  mutate(peg_bp = abs(price - 1) * 10000) %>%
  filter(!is.na(peg_bp), peg_bp >= PEG_THR_BP) %>%
  arrange(desc(peg_bp)) %>%
  select(symbol, name, price, peg_bp) %>%
  head(TOP_N)

# Supply shocks (today vs yesterday)
supply <- today %>%
  left_join(yday, by = "id") %>%
  mutate(
    supply_chg_pct = ifelse(!is.na(circ_yday) & circ_yday > 0,
                            100 * (circulating - circ_yday) / circ_yday,
                            NA_real_)
  ) %>%
  filter(!is.na(supply_chg_pct), abs(supply_chg_pct) >= SUPPLY_THR_PCT) %>%
  arrange(desc(abs(supply_chg_pct))) %>%
  select(symbol, name, supply_chg_pct) %>%
  head(TOP_N)

msg_lines <- c(
  paste0("**Stablecoin Watchtower — ", max_date, "**"),
  paste0("Thresholds: depeg ≥ ", PEG_THR_BP, " bps, supply shock ≥ ", SUPPLY_THR_PCT, "%"),
  ""
)

if (nrow(offpeg) == 0 && nrow(supply) == 0) {
  msg_lines <- c(msg_lines, "No alerts triggered today ✅")
} else {
  if (nrow(offpeg) > 0) {
    msg_lines <- c(msg_lines, "**Off-peg:**")
    for (i in seq_len(nrow(offpeg))) {
      msg_lines <- c(msg_lines,
                     paste0("- ", offpeg$symbol[i], " @ $", signif(offpeg$price[i], 6),
                            " (", round(offpeg$peg_bp[i], 1), " bps)"))
    }
    msg_lines <- c(msg_lines, "")
  }
  
  if (nrow(supply) > 0) {
    msg_lines <- c(msg_lines, "**Supply shocks (vs yesterday):**")
    for (i in seq_len(nrow(supply))) {
      msg_lines <- c(msg_lines,
                     paste0("- ", supply$symbol[i], ": ", round(supply$supply_chg_pct[i], 2), "%"))
    }
  }
}

message_text <- paste(msg_lines, collapse = "\n")

# Don’t resend identical message
if (file.exists(LAST_MSG_PATH)) {
  last_msg <- paste(readLines(LAST_MSG_PATH, warn = FALSE), collapse = "\n")
  if (identical(last_msg, message_text)) {
    cat("Message unchanged — not sending to Discord.\n")
    quit(save = "no")
  }
}

payload <- list(content = message_text)

resp <- POST(webhook, body = payload, encode = "json")
sc <- status_code(resp)

if (sc %in% c(200, 204)) {
  writeLines(as.character(Sys.time()), LOCK_PATH)
  writeLines(message_text, LAST_MSG_PATH)
  cat("Discord alert sent.\n")
} else if (sc == 429) {
  # Rate limited. Do not keep retrying.
  cat("Discord rate limit hit (HTTP 429). Wait a bit and run again.\n")
  quit(save = "no")
} else {
  # Show response body for debugging
  body_txt <- content(resp, "text", encoding = "UTF-8")
  stop(paste("Discord webhook failed. HTTP", sc, "Body:", body_txt))
}