source(here::here("R/readers.R"))

delay <- read_olkiluoto_delay()

naive_plaw <- function(x, alpha = 2) {
  (alpha*x)/(alpha-1)
}

delay.rep <- delay %>%
  mutate(notified.diff = replace_na(as.numeric(notified-lag(notified)), 0),
         est_days = as.numeric(est_completion-notified),
         est_years = est_days/365.25,
         days_total = cumsum(notified.diff),
         years_total = days_total/365.25,
         plaw_years = naive_plaw(est_years, 2),
         penalty = log1p(years_total/est_years),
         plaw_year.robust = plaw_years*penalty,
         est_completion.robust = notified + floor(plaw_year.robust*365.25))

