
get_data <- function(Dir=".", FileType="csv", ColsToExtract, MetaNames, MetaSep="[\\/\\s]", Meta=NULL, SaveMeta=FALSE){

  # get file names
  FileNames <- list.files(Dir, recursive = TRUE)

  # get metadata as list
  if (is.null(Meta)){
    MetaList <- str_remove(FileNames, str_c("\\.", FileType)) %>% str_split(MetaSep)
  }

  # determine length of metadata coded in file name
  MetaLengths <- sapply(MetaList, length)
  MaxMetaLength <- max(MetaLengths)

  # test if meta in file names complete
  if(!all(MetaLengths==MaxMetaLength)){
    stop("\n\n!!!\nLength of the metadata (info coded in filename) is not the same for every file.\n",
         "Expected legth: ", MaxMetaLength, "\n",
         "Verify files:\n",
         str_c("\t\t-> '", FileNames[MetaLengths!=MaxMetaLength], "'\n"), "!!!\n\n")
  }

  # get colnames for metadata
  MetaColumns <- map_chr(seq_len(MaxMetaLength), ~ {
      ex <- head(unique(map_chr(MetaList, .x)), 3) # examples of unique values to name
      ex <- str_c(ex, collapse = " / ")
      cat(str_c("Provide a name for: ", ex, "\n"))
      readline(" >>> ")
    })

  # assign names to MetaList and transform to data.frame
  Meta <- map(MetaList, ~ set_names(.x, MetaColumns) %>% t %>% as_tibble) %>%
    bind_rows() %>%
    mutate(path=FileNames)

  # save Meta as excel if needed
  if (SaveMeta) writexl::write_xlsx(Meta, "../Meta.xlsx")

  # load data as nested tibbles
  Data <- tibble(path=FileNames,
                 meta=str_remove(path, str_c("\\.", FileType)) %>% str_split(MetaSep),
                 data=map(path, read.csv, stringsAsFactors=FALSE)) %>%
    separate(path, into=c("dpg", "genotype", "treatment", "x"), sep=regex("[\\/\\s]"), remove=FALSE)
  # info=str_remove(path, "\\.csv") %>% str_split(pattern = "\\/")) %>%
  unnest(info)

}


# create_meta <- function(Dir=".", TryAuto= SaveInDir = FALSE){
#
#   # get metadata
#   if (is.null(Meta)){
#     Meta <- str_remove(FileNames, "\\.fileType") %>% str_split(MetaSep)
#   }
#
#   # determine length of metadata coded in file name
#   MetaLengths <- sapply(Meta, length)
#   MaxMetaLength <- max(MetaLengths)
#
#   # test if meta in file names complete
#   if(!all(MetaLengths==MaxMetaLength))
#     stop("\n\n!!!\nLength of the metadata (info coded in filename) is not the same for every file.\n",
#          "-> Expected legth: ", MaxMetaLength, "\n",
#          "Verify files:\n",
#          str_c("    '", FileNames[MetaLengths!=MaxMetaLength], "'\n"), "!!!\n\n")
#
#
#
#   # get file names
#   FileNames <- list.files(Dir, recursive = TRUE)
#
#   # separate
#   str_remove(FileNames, "\\.fileType") %>% str_split(MetaSep)
#
# }


norm_distrib <- function() {
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
