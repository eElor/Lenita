
lenita_packages <- function(){

  Packs <- c("tidyverse", "writexl", "readxl", "ggbeeswarm", "devtools", "cowplot", "crayon", "broom", "ggrepel", "glue")

  for(package in Packs){

    if(package %in% installed.packages()){
      cat(package, "is already installed\n")
      next()
    }

    if(!package %in% installed.packages()){
      cat(package, "is being installed...")
      install.packages(package, dependencies = TRUE, verbose = FALSE, quiet = TRUE)
    }

    if(package %in% installed.packages()){
      cat(package, "successfully installed")
    } else {
      warning("\n", package, " could not be installed!!!!\n")
    }
  }
}