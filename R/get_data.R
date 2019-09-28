
get_data <- function(Dir="RAW_DATA", FileType="csv", MetaSep="[\\/\\s]", Meta=NULL, SaveMeta=FALSE){

  # get file names
  FileNames <- list.files(Dir, recursive = TRUE)

  # get metadata as list
  if (is.null(Meta)){
    MetaList <- str_remove(FileNames, str_c("\\.", FileType)) %>%
      str_split(MetaSep) %>%
      map(str_trim, side='both') %>%
      map(na_if, "NA")
  }

  # determine length of metadata coded in file name
  MetaLengths <- sapply(MetaList, length)
  MaxMetaLength <- round(median(MetaLengths))

  # test if meta in file names complete
  if(!all(MetaLengths==MaxMetaLength)){
    stop("\n\n!!!\nLength of the metadata (info coded in filename) is not the same for every file.\n",
         "Expected legth: ", MaxMetaLength, "\n",
         "Verify files:\n",
         str_c("\t\t-> '", FileNames[MetaLengths!=MaxMetaLength], "'\n"), "!!!\n\n")
  }

  # get colnames for metadata
  MetaColumns <- map_chr(seq_len(MaxMetaLength), ~ {
      ex <- map_chr(MetaList, .x) %>%
        na.omit() %>%
        unique() %>%
        head(3) # examples of unique values to name
      ex <- str_c(ex, collapse = " / ")
      cat(str_c("Provide a name for: ", ex, "\n"))
      readline(" >>> ")
    })

  # assign names to MetaList and transform to data.frame
  Meta <- map(MetaList, ~ set_names(.x, MetaColumns) %>% t %>% as_tibble) %>%
    bind_rows() %>%
    mutate(path=file.path(Dir, FileNames))

  # save Meta as excel if needed
  if (SaveMeta) writexl::write_xlsx(Meta, file.path(Dir,"../Meta.xlsx"))

  # load data as nested tibbles
  Data <- mutate(Meta, data=map(path, read_csv, col_types=cols()))

  # which columns to extract
  Cols <- names(Data$data[[1]])
  cat("Which column would you like to keep? (write one number)\nOptions: \n",
      map_chr(seq_along(Cols), ~ str_c("\t [", .x, "] ", Cols[.x],"\n")))
  # ColsToKeep <- readline(" >>> ") %>% {eval(parse(text = .))}
  ColToKeep <- readline(" >>> ")

  # accept only one columns
  while(!length(ColToKeep)==1){
    cat("Please select only one columns to keep!")
    ColToKeep <- readline(" >>> ")
  }

  # extract columns
  Data <- mutate(Data, data=map(data, ~select(.x, value=ColToKeep))) %>%
    select(-path)
  cat(str_c("Data from column '", Cols[ColToKeep], "' was saved in column 'value'"))
  return(Data)

}


