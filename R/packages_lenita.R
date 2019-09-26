
packages_lenita <- function(){
  Packs <- c("tidyverse", "writexl", "readxl", "ggbeeswarm", "devtools", "cowplot", "crayon", "broom", "ggrepel")
  for(package in Packs){
    if(package %in% installed.packages()){
      cat(package, "is already installed\n")
      next()
    }
    if(!package %in% installed.packages()){
      cat(package, "is being installed...")
      install.packages("bladdf", dependencies = TRUE, verbose = FALSE, quiet = TRUE)
      if(package %in% installed.packages()){
        cat(package, "successfully installed")
      } else {
        warning("\n", package, " could not be installed!!!!\n")
        Sys.sleep(1)
      }
    }
  }
}