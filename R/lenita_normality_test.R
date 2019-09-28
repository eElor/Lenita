
lenita_normality_test <- function(DF){
  DF %>%
    group_by_at(vars(-value)) %>%
    nest() %>%
    mutate(norm_test = map(data, ~ broom::tidy(shapiro.test(.x$value)))) %>%
    unnest(norm_test) %>%
    select(-data) %>%
    mutate(sig = lenita_signif(p.value),
           normal_distributed = ifelse(is.na(sig), "yes", "no"))}