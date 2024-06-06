## 不要なコード
  # add_text_id <- function(tbl){
  #   tbl %>% dplyr::mutate(text_id=1:nrow(.))
  # }

  # select_text <- function(tbl, text="text"){
  #   dplyr::select(tbl, all_of(text))
  # }

  # select_others <- function(tbl, text="text"){
  #   dplyr::select(tbl, !all_of(text))
  # }

  # add_others <- function(tbl, others){
  #   dplyr::left_join(tbl, others)
  # }

  # BPを使っていたけど，不要
  #   tbl %>%
  #   select_text() %>%
  #   add_break_point(bp="BPOID") %>%
  #   mecab() %>%
  #   add_text_id_by_bp(bp="BPOID", bp_col="表層形", text_id="text_id") %>%
  #   add_text_id_by_bp(bp="句点", bp_col="品詞細分類1", text_id="sent_id") %>%
  #   print(n=100)


## 辞書ファイルの作成：不要
  # windwosでは動作確認済み
set_mecab_dic_bp <- function(
  #   dic_tool = "/opt/local/mecab/libexec/bin/mecab-dict-index", # tools for making user dictionary
  #   dic_dir  = "/opt/local/lib/mecab/dic/ipadic",               # directory of dictionary
  #   usr_dic  = "user.dic",                                      # file of user dictionary
  #   dic_char = "utf8",                                          # charset of user dictionary
  #   csv_char = "utf8",                                          # charset of csv file
  #   usr_csv  = "user.csv"                                       # entry of user dictionary
  ){

library(tidyverse)

  # setting of HP (win)
  dic_tool <- "D:/pf/MeCab/bin/mecab-dict-index"
  dic_dir  <- "D:/pf/MeCab/dic/ipadic"
  dic_char <- "shift-jis"
  csv_char <- "shift-jis"
  usr_dic  <- "D:/matu/workOLD/lecture/enq/all/dic/usr_dic_bp.dic"
  usr_csv  <- "D:/matu/workOLD/lecture/enq/all/dic/usr_dic_bp.csv"
  mecab_dir <- "D:/pf/mecab/dic/usr_dic_bp.dic"

  # make csv file
  bp <- c("BPOID", "BP_ID", "BREAK_POINT_OF_ID")
  stringr::str_c(bp, ",,,1,記号,一般,*,*,*,*,", bp, ",,") %>%
    tibble::tibble() %>%
    write.table(usr_csv, quote=FALSE, col.names=FALSE, row.names=FALSE, fileEncoding = "CP932")


  #     readr::write_delim(usr_csv, col_names=FALSE)

  # make dictionary
  cmd <- 
    stringr::str_c(
      dic_tool, " ", 
      "-d ", dic_dir,   " ", 
      "-u ", usr_dic,   " ", 
      "-f ", csv_char,  " ", 
      "-t ", dic_char,  " ", 
      usr_csv
    )
  shell(cmd)

  # copy dictionary
  cmd <- stringr::str_c("cp ", usr_dic, " ", mecab_dir)
  shell(cmd)

}

## 辞書ファイル(linux用?)：不要
set_mecab_dic <- function(
  dic_tool = "/opt/local/mecab/libexec/bin/mecab-dict-index", # tools for making user dictionary
  dic_dir  = "/opt/local/lib/mecab/dic/ipadic",               # directory of dictionary
  usr_dic  = "user.dic",                                      # file of user dictionary
  dic_char = "shift-jis ",                                    # charset of user dictionary
  csv_char = "shift-jis ",                                    # charset of csv file
  usr_csv  = "user.csv"                                       # entry of user dictionary
  ){
  
  
}

set_sudachi_dic <- function(){
  
  
}

  # システム情報
Sys.getenv("OS")
