## setup
rm(list=ls(all=TRUE));gc();gc();
library(tidyverse)

## functions
insert_sep <- function(x, sep = " "){
  chr <- ""
  len <- stringr::str_length(x)
  for(i in 1:len){
    chr <- stringr::str_c(chr, stringr::str_sub(x, i, i), sep)
  }
  stringr::str_sub(chr, 1, stringr::str_length(chr)-1)
}
ngram <- function(tbl, col, n = 1){
  new_col <- stringr::str_c(col, "_", n + 1)
  tbl %>%
    dplyr::mutate(`:=`({{new_col}}, dplyr::lead(.data[[col]], n)))
}

## main
wamei <- 
  readr::read_tsv("wamei.txt", show_col_types = FALSE) %>%
  dplyr::mutate(id = 1:nrow(.))
wamei <- 
  wamei %>%
  head() %T>%
  {
    max_len <<- 
      dplyr::mutate(., len = stringr::str_length(name)) %>%
      dplyr::summarise(max(len)) %>%
      `[[`(1)
  } %>%
  dplyr::group_by(id) %>%
  dplyr::mutate(name_tmp = insert_sep(name)) %>%
  tidyr::separate(name_tmp, into = as.character(1:max_len), sep = " ", fill = "right") %>%
  tidyr::pivot_longer(cols = !c(id, name), names_to = "no" , values_to = "chr", values_drop_na = TRUE)
for(i in 1:max_len){
  wamei <- ngram(wamei, "chr", i)
}

## old
  # devide_char <-function(x){
  #   len <- stringr::str_length(x)
  #   char <- vector()
  #   for(i in 1:len){
  #     char[i] <- stringr::str_sub(x, i, i)
  #   }
  #   list(char)
  # }
