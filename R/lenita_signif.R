
lenita_signif <- function(x){
  st <- x
  st[x >= 0.05] <- NA
  st[x < 0.05] <- "*"
  st[x < 0.01] <- "**"
  st[x < 0.001] <- "***"
  st
  }