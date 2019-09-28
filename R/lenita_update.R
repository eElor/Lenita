
lenita_update <- function() {
  devtools::install_github("eElor/Lenita")
  detach("package:Lenita", unload = TRUE)
  library(Lenita)
}