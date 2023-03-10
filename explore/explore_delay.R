source(here::here("R/readers.R"))

delay <- read_olkiluoto_delay()

delay.rep <- delay %>%
  mutate(notified.diff = replace_na(as.numeric(notified-lag(notified)), 0),
         time_total = cumsum(notified.diff),
         est_completion.diff = replace_na(as.numeric(est_completion-lag(est_completion)), 0),
         planned_delay = as.numeric(est_completion - notified),
         planned_delay.robust = 3*planned_delay,
         penalty = 1+log1p(time_total/planned_delay.robust),
         est_completion.robust = notified + planned_delay.robust*penalty)


ggplot(delay.rep) +
  geom_segment(aes(x = notified, xend = est_completion, y = n, yend = n), linewidth = 2, color = "grey") +
  #geom_segment(aes(x = notified, xend = est_completion.robust, y = n, yend = n)) +
  theme_minimal()
