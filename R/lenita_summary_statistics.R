

lenita_summary_statistics <- function(DF){
  DF %>%
  filter(!is.na(value)) %>%
  group_by_at(vars(-value)) %>%
  summarise(
    n = n(),
    mean = mean(value),
    sd = sd(value),
    sem = sd/sqrt(n),
    median = median(value),
    "25%" = quantile(value, .25),
    "75%" = quantile(value, .75)
  )}