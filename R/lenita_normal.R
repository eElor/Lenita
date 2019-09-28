
lenita_normal <- function() {
  norm_dist <- is.na(normality_test$sig)
  ratio <- paste0(as.character(sum(norm_dist)), "/", length(normality_test$sig), "")
  if (all(norm_dist)) {
    cat(crayon::white$bgGreen$bold("\n\n\n\n All groups are normally distributed!   \n   >>> use t.test for inference! JUHU!!!\n\n\n\n\n"))
  } else if (any(!norm_dist)) {
    cat(crayon::white$bgMagenta$bold("\n\n\n\n Not all groups are normally distributed! :'(  [", ratio, "] \n   >>> Maybe use a non-parametric test (e. g. Wilcox) \n\n\n\n\n"))
  } else {
    cat(crayon::white$bgRed$bold("\n\n\n\n ERROR:                  \n>>> Something went wrong \nwith the normality test! \nCheck that again!!!      \n\n\n\n\n"))
  }
}