#' Morphological analysis for a specific column in dataframe
#'
#' Using 'MeCab' for morphological analysis.
#' Keep other colnames in dataframe.
#'
#' @param tbl          A tibble or data.frame.
#' @param text_col     A text. Colnames for morphological analysis.
#' @param bin_dir      A text. Directory of mecab.
#' @param method       A text. Method to use: "mecab", "ginza",
#'                     "sudachi_a", "sudachi_b", "sudachi_c", or "chamame".
#'                     "a", "b" and "c" specify the mode of splitting.
#'                     "a" split shortest, "b" middle and "c" longest.
#'                     See https://github.com/WorksApplications/Sudachi for detail.
#'                     "chamame" use https://chamame.ninjal.ac.jp/ and rvest.
#' @param option       A text. Options for mecab.
#'                     "-b" option is already set by moranajp.
#'                     To see option, use "mecab -h" in command (win) or terminal (Mac).
#' @param iconv        A text. Convert encoding of MeCab output.
#'                     Default (""): don't convert.
#'                     "CP932_UTF-8": iconv(output, from = "Shift-JIS" to = "UTF-8")
#'                     "EUC_UTF-8"  : iconv(output, from = "eucjp", to = "UTF-8")
#'                     iconv is also used to convert input text before running MeCab.
#'                     "CP932_UTF-8": iconv(input, from =  "UTF-8", to = "Shift-JIS")
#' @param col_lang     A text. "jp" or "en"
#' @param dic          A text. Dictionary of web chamame.
#'                     Only used when `method = "chamame"`.
#'                     See [web_chamame()].
#' @return A tibble.   Output of morphological analysis and added column "text_id".
#'                     `NULL` when `method = "chamame"` and
#'                     web chamame is not available.
#' @examples
#' # sample data of Japanese sentences
#' data(neko)
#' neko <-
#'     neko |>
#'     unescape_utf()
#'
#' \dontrun{
#'   # chamame: need to connect https://chamame.ninjal.ac.jp/ .
#'   neko |>
#'     moranajp_all(method = "chamame") |>
#'         print(n=100)
#' }
#' \dontrun{
#'   # Need to install 'mecab', 'ginza', or 'sudachi' in local PC
#'
#'   # mecab
#'   bin_dir <- "d:/pf/mecab/bin"
#'   iconv <- "CP932_UTF-8"
#'   neko |>
#'     moranajp_all(text_col = "text", bin_dir = bin_dir, iconv = iconv) |>
#'         print(n=100)
#'
#'   # ginza
#'   neko |>
#'     moranajp_all(text_col = "text", method = "ginza") |>
#'       print(n=100)
#'
#'   # sudachi
#'   bin_dir <- "d:/pf/sudachi"
#'   iconv <- "CP932_UTF-8"
#'   neko |>
#'     moranajp_all(text_col = "text", bin_dir = bin_dir,
#'                  method = "sudachi_a", iconv = iconv) |>
#'         print(n=100)
#' }
#' 
#' @export
moranajp_all <- function(tbl, bin_dir = "", method = "mecab",
             text_col = "text", option = "", iconv = "",
             col_lang = "jp", dic = "unidic-spoken"){
  # text_col = "text"; option = ""; bin_dir = "d:/pf/mecab/bin/"; iconv = "CP932_UTF-8"; method = "mecab"; tbl = review |> unescape_utf(); col_lang = "jp"
  message(paste0("Analaysing by ", method, ". Please wait."))
  text_id    <- "text_id"
  tmp_group  <- "tmp_group"  # Use temporary
  str_length <- "str_length" # Use temporary
  tbl    <- dplyr::mutate(tbl, `:=`({{ text_id }}, dplyr::row_number()))
  others <- dplyr::select(tbl, !dplyr::all_of(text_col))
  tbl    <- remove_linebreaks(tbl, text_col)
  if(method == "chamame"){
    tbl <-
      tbl |>
        make_input(text_col = text_col, iconv = iconv) |>
        web_chamame(col_lang = col_lang, dic = dic)
    # web_chamame() returns NULL when it is not available
    if(is.null(tbl)){ return(invisible(NULL)) }
  }else{
    tbl <-
      tbl |>
      make_groups(text_col = text_col, length = 8000,   # if error decrease length
        tmp_group = tmp_group, str_length = str_length) |>
      dplyr::group_split(.data[[tmp_group]]) |>
      purrr::map(dplyr::select, dplyr::all_of(text_col)) |>
      purrr::map(moranajp,
        bin_dir = bin_dir, method = method,
        text_col = text_col, option = option, iconv = iconv, col_lang = col_lang) |>
      dplyr::bind_rows()
  }
  tbl <-
    tbl |>
    add_text_id(method = method) |>
    remove_brk(method = method) |>
    dplyr::left_join(others, by = text_id) |>
    dplyr::relocate(dplyr::all_of(text_id), colnames(others))
  return(dplyr::slice(tbl, -nrow(tbl)))
}

#' @rdname moranajp_all
#' @export
moranajp <- function(tbl, bin_dir, method, text_col, option = "", iconv = "", col_lang){
  input <- make_input(tbl, text_col, iconv)
  command <- make_cmd(method, bin_dir, option = "")
  output <- system(command, intern = TRUE, input = input)
  output <- iconv_x(output, iconv) # Convert Encoding
  out_cols <- switch(method,
    "mecab"     = out_cols_mecab(col_lang),
    "ginza"     = out_cols_ginza(col_lang),
    "sudachi_a" = out_cols_sudachi(col_lang),
    "sudachi_b" = out_cols_sudachi(col_lang),
    "sudachi_c" = out_cols_sudachi(col_lang)
  )
  tbl <-
    output |>
    tibble::tibble() |>
    tidyr::separate(1, into = out_cols,
      sep = "\t|,", fill = "right", extra = "drop")
  if(method == "ginza"){
    tbl <- separate_cols_ginza(tbl, col_lang)
  }
  return(tbl)
}

#' @rdname moranajp_all
remove_linebreaks <- function(tbl, text_col){
  if (sum(stringr::str_detect(
          stringr::str_c(tbl[[text_col]], collapse = NULL), "\\r\\n"))){
        message("Removed line breaks !")
  }
  if (sum(stringr::str_detect(
          stringr::str_c(tbl[[text_col]], collapse = NULL), "\\n"))){
    message("Removed line breaks !")
  }
  if (sum(stringr::str_detect(
      stringr::str_c(tbl[[text_col]], collapse = NULL), '&|\\||<|>|"'))){
    message('Removed &, |, <. > or " !')
  }
  tbl |>
    dplyr::mutate(`:=`({{text_col}},
      stringr::str_replace_all(.data[[text_col]], "\\r\\n", ""))) |>
    dplyr::mutate(`:=`({{text_col}},
      stringr::str_replace_all(.data[[text_col]], "\\n", ""))) |>
    dplyr::mutate(`:=`({{text_col}},
      stringr::str_remove_all(.data[[text_col]], '&|\\||<|>|"')))
}

#' @rdname moranajp_all
separate_cols_ginza <- function(tbl, col_lang){
  into <-
    c("\\u54c1\\u8a5e",
      paste0(rep("\\u54c1\\u8a5e\\u7d30\\u5206\\u985e", 2), 1:2)) |>
    unescape_utf()
  if(col_lang == "en"){
    into <-
      tibble::tibble(jp = into) |>
      dplyr::left_join(out_cols()) |>
      `[[`(_, "en")
  }
  xpos <- out_cols_ginza(col_lang)[5]
  tbl <-
    tbl |>
    tidyr::separate(dplyr::all_of(xpos), into = into,
      sep = "-", fill = "right", extra = "drop", remove = TRUE)
  return(tbl)
}

  # review_mecab |>
  #   unescape_utf() |>
  #   dplyr::filter(stringr::str_detect(.$表層形, "-"))
  #
  # review_sudachi_a |>
  #   unescape_utf() |>
  #   dplyr::filter(stringr::str_detect(.$表層形, "-"))
  #
  # review_ginza |>
  #   unescape_utf() |>
  #   dplyr::filter(stringr::str_detect(lemma, "-"))

#' @rdname moranajp_all
#' @param  brk A string of break point
#' @return A string
#' @export
make_input <- function(tbl, text_col, iconv,
  brk = "BP"){ # Break Point of moranajp: need space to split with English words
  input <-
    tbl |>
    dplyr::select(dplyr::all_of(text_col)) |>
    unlist() |>
    stringr::str_c(collapse = brk) |>
    stringr::str_c(brk) |>  # NEED brk at the end of input
    iconv_x(iconv, reverse = TRUE)
  return(input)
}

#' @rdname moranajp_all
#' @return A string
make_cmd <- function(method, bin_dir, option = ""){
  cmd <- switch(method,
    "mecab"   = make_cmd_mecab(option = ""),
    "ginza"   = "ginza",
    "sudachi_a" = "java -jar sudachi.jar -m A",
    "sudachi_b" = "java -jar sudachi.jar -m B",
    "sudachi_c" = "java -jar sudachi.jar -m C",
  )
  cmd <- 
    paste0(bin_dir, "/", cmd) |>
    stringr::str_replace_all("//", "/")
  return(cmd)
}

#' @rdname moranajp_all
#' @return A string
make_cmd_mecab <- function(option = ""){
  cmd <- stringr::str_c("mecab -b 17000", option)
  return(cmd)
}

#' @rdname moranajp_all
#' @return A character vector
out_cols_mecab <- function(col_lang = "jp"){
  jp <-
    c("\\u8868\\u5c64\\u5f62", "\\u54c1\\u8a5e",
      paste0(rep("\\u54c1\\u8a5e\\u7d30\\u5206\\u985e", 3), 1:3),
      "\\u6d3b\\u7528\\u578b", "\\u6d3b\\u7528\\u5f62",
      "\\u539f\\u5f62", "\\u8aad\\u307f", "\\u767a\\u97f3") |>
    unescape_utf()
  if(col_lang == "jp"){
    return(jp)
  }else{
    tibble::tibble(jp = jp) |>
      dplyr::left_join(out_cols()) |>
      `[[`(_, "en")
  }
}

#' @rdname moranajp_all
#' @return A character vector
out_cols_ginza <- function(col_lang = "jp"){
  # ID:
  # FORM:   Word form or punctuation symbol.
  # LEMMA:  Lemma or stem of word form.
  # UPOS:   Universal part-of-speech tag.
  # XPOS:   Language-specific part-of-speech tag; underscore if not available.
  # FEATS:  List of morphological features from the universal feature inventory or from a defined language-specific extension; underscore if not available.
  # HEAD:   Head of the current word, which is either a value of ID or zero (0).
  # DEPREL: Universal dependency relation to the HEAD (root if HEAD = 0) or a defined language-specific subtype of one.
  # DEPS:   Enhanced dependency graph in the form of a list of head-deprel pairs.
  # MISC:   Any other annotation.
  #   c("id", "form", "lemma", "upos", "xpos", "feats", "head", "deprel", "deps", "misc")
  jp <-
    c("id"                                  , "\\u8868\\u5c64\\u5f62"               , "\\u539f\\u5f62"                      , # ginza
       "UD\\u54c1\\u8a5e\\u30bf\\u30b0"     , "\\u54c1\\u8a5e\\u30bf\\u30b0"        , "\\u5c5e\\u6027"                      ,
       "\\u4fc2\\u53d7\\u5143"              , "\\u4fc2\\u53d7\\u30bf\\u30b0"        , "\\u4fc2\\u53d7\\u30da\\u30a2"        ,
       "\\u305d\\u306e\\u4ed6")  |>
    unescape_utf()
  if(col_lang == "jp"){
    return(jp)
  }else{
    tibble::tibble(jp = jp) |>
      dplyr::left_join(out_cols()) |>
      `[[`(_, "en")
  }
}

#' @rdname moranajp_all
#' @return A character vector
out_cols_sudachi <- function(col_lang = "jp"){
  jp <-
    c("\\u8868\\u5c64\\u5f62", "\\u54c1\\u8a5e",
      paste0("\\u54c1\\u8a5e\\u7d30\\u5206\\u985e", 1:5),
      "\\u539f\\u5f62") |>
    unescape_utf()
  if(col_lang == "jp"){
    return(jp)
  }else{
    tibble::tibble(jp = jp) |>
      dplyr::left_join(out_cols()) |>
      `[[`(_, "en")
  }
}

#' @rdname web_chamame
#' @return A character vector
out_cols_chamame <- function(col_lang = "jp"){
  jp <-
    c("\\u8868\\u5c64\\u5f62", "\\u54c1\\u8a5e",
      paste0("\\u54c1\\u8a5e\\u7d30\\u5206\\u985e", 1:3),
      "\\u539f\\u5f62") |>
    unescape_utf()
  if(col_lang == "jp"){
    return(jp)
  }else{
    tibble::tibble(jp = jp) |>
      dplyr::left_join(out_cols()) |>
      `[[`(_, "en")
  }
}

#' @rdname moranajp_all
#' @return A character vector
out_cols_jp <- function(){
  c(out_cols_mecab(),
    out_cols_sudachi(),
    out_cols_ginza(),
    out_cols_chamame())
}

#' @rdname moranajp_all
#' @return A character vector
out_cols_en <- function(){
  c("form", "pos", "pos_1", "pos_2", "pos_3", "conjugation_type", "conjugation_form", "lemma", "reading", "soud", # mecab
    "form", "pos", "pos_1", "pos_2", "pos_3", "pos_4", "pos_5", "lemma",                                          # sudachi
    "id", "form", "lemma", "upos", "xpos", "feats", "head", "deprel", "deps", "misc",                             # ginza
    "form", "pos", "pos_1", "pos_2", "pos_3", "lemma")                                                            # chamame
}

#' @rdname moranajp_all
#' @return A data.frame
out_cols <- function(){
  order <- c(10, 99, 99, 99, 99, 12, 13, 11, 2, 99, 99, 99, 1, 3, 9, 4, 5, 6, 7, 8)
  tibble::tibble("jp" := out_cols_jp(), "en" := out_cols_en()) |>
  dplyr::distinct() |>
  dplyr::arrange(.data[["jp"]]) |>
  dplyr::bind_cols(order = order) |>
  dplyr::arrange(order, .data[["jp"]])
}

#' Add id column into result of morphological analysis
#'
#' Internal function for moranajp_all().
#' Add `text_id` column when there is brk ("BP").
#'    "BP": Break Point of moranajp
#'
#' @inheritParams moranajp_all
#' @inheritParams make_input
#' @return A data.frame with column "text_id".
#' @export
add_text_id <- function(tbl, method, brk = "BP"){
  text_id <- "text_id"
  cnames  <- colnames(tbl)
  if (any(text_id %in% cnames)){
    stop("colnames must NOT have a colname 'text_id'")
  }
  if(method == "ginza"){
    tbl <- dplyr::filter(tbl, !is.na(.data[[cnames[3]]]))
  }
  col_no <- ifelse(method == "ginza", 2, 1)
  col <- cnames[col_no]
  # add_group() do not work inside this function
  #   add_group() work on its own.
  # Do NOT lag: brk belongs to the next text, as in add_group(end_with_brk = FALSE).
  tbl <-
    tbl |>
    dplyr::mutate(`:=`({{ text_id }},
                       (.data[[col]] == brk) |>
                       cumsum() |>
                       `+`(e1 = _, e2 = 1) ))
  return(tbl)
}

#' Remove break point and other unused rows from the result of
#' morphological analysis
#'
#' Internal function for moranajp_all().
#'
#' @inheritParams moranajp_all
#' @inheritParams make_input
#' @return A data.frame.
#' @export
remove_brk <- function(tbl, method, brk = "BP"){
  cnames  <- colnames(tbl)
  col_no <- ifelse(method == "ginza", 2, 1)
  col <- cnames[col_no]
  tbl <-
    tbl |>
    dplyr::filter(.data[[col]] != brk) |>
    dplyr::filter(.data[[col]] != " ")
  if(method == "ginza"){
    input_col <- "^# text = "
    tbl <- dplyr::filter(tbl, !stringr::str_detect(.data[["id"]], input_col))
  }
  return(tbl)
}

#' Morphological analysis for Japanese text by web chamame
#'
#' Using https://chamame.ninjal.ac.jp/ and rvest.
#' Because it uses an internet resource, it fails gracefully:
#' when the web service is not available or its response has changed,
#' shows a message and returns `NULL` instead of an error.
#'
#' @param text        A text.
#' @param col_lang    A text. "jp" or "en"
#' @param dic         A text. Dictionary of web chamame,
#'                    e.g. "unidic-spoken", "gendai" or "ipadic".
#'                    Use the value of the dictionary checkbox in the form.
#' @param url         A text. URL of web chamame.
#' @return A dataframe, or `NULL` when web chamame is not available.
#' @examples
#' \dontrun{
#'   # Need to connect https://chamame.ninjal.ac.jp/ .
#' text <-
#'   paste0("\\u3059",
#'          paste0(rep("\\u3082",8),collapse=""),
#'          "\\u306e\\u3046\\u3061") |>
#'   unescape_utf()
#' web_chamame(text)
#' }
#'
#' @export
web_chamame <- function(text, col_lang = "jp", dic = "unidic-spoken",
                        url = "https://chamame.ninjal.ac.jp/index.html"){
  html <- read_html_safely(url)
  if(is.null(html)){ return(invisible(NULL)) }
  # Submitting and parsing are also guarded:
  #   the response of web chamame may have changed.
  chamame <- try(submit_chamame(html, text, col_lang, dic), silent = TRUE)
  if(inherits(chamame, "try-error")){
    message(msg_not_available(url, chamame))
    return(invisible(NULL))
  }
  return(chamame)
}

#' Helper function for web_chamame
#'
#' Submits a text to web chamame and parses the response.
#' Errors are handled by web_chamame().
#'
#' @param html      A xml_document of web chamame.
#' @return A dataframe
#' @rdname web_chamame
submit_chamame <- function(html, text, col_lang = "jp", dic = "unidic-spoken"){
  form <-
    rvest::html_form(html)[[1]] |>
    rvest::html_form_set(st = text) |>
    html_radio_set("out-e" = "html")
  form <- select_chamame_fields(form, dic)
  resp <- rvest::html_form_submit(form)
  # An error page can be parsed as a table, so check the status first
  if(resp$status_code >= 400){
    stop("HTTP status ", resp$status_code, " for the submitted form.")
  }
  chamame <-
    rvest::read_html(resp) |>
    rvest::html_table() |>
    `[[`(_, 1) |>
    check_chamame_table() |>
    extract_chamame_cols(col_lang = col_lang)

  zen_bp <- stringi::stri_trans_general("BP", "halfwidth-fullwidth")
  chamame[["\u8868\u5c64\u5f62"]] <-
    stringr::str_replace(chamame[["\u8868\u5c64\u5f62"]], zen_bp, "BP")

  return(chamame)
}

#' Select the fields to submit to web chamame
#'
#' Selects the fields by name, NOT by index:
#' web chamame sometimes adds fields, which shifts all the indices.
#' Errors are handled by web_chamame().
#'
#' @param form  A rvest_form object of web chamame.
#' @return A rvest_form object
#' @rdname web_chamame
select_chamame_fields <- function(form, dic = "unidic-spoken"){
  # dic_version: added by web chamame in 2025.
  #   Without it, web chamame returns a server error for UniDic dictionaries.
  # Request ONLY the output items to use.
  #   When many items are requested, the header row of the result
  #   does not correspond to the cells, and the columns can not be selected.
  #   f3: part of speech, f3_1:f3_4: its sub categories,
  #   f12: orthographic base form
  need_name <-
    c("hankaku-zenkaku", "dic_version",
      "f3", "f3_1", "f3_2", "f3_3", "f3_4", "f12")
  f_name  <- names(form$fields)
  f_type  <- purrr::map_chr(form$fields, chamame_field, "type")
  f_value <- purrr::map_chr(form$fields, chamame_field, "value")
  if(!any(f_value == dic, na.rm = TRUE)){
    stop("Dictionary '", dic, "' is not in the form of web chamame.")
  }
  is_value <- function(x){ !is.na(f_value) & f_value == x }
  need_index <-
    which(f_type %in% c("button", "submit", "textarea") |  # submit and input
          f_name %in% need_name |
          is_value(dic) |                                  # only one dictionary
          (f_name == "out-e" & is_value("html")))          # output format
  del_index <- sort(
    setdiff(seq_along(form$fields), need_index),
    decreasing = TRUE)
  for (i in del_index) {
      form$fields[[i]] <- NULL
  }
  return(form)
}

#' @param field  A field in a rvest_form object.
#' @param item   A string to specify the item of `field`.
#' @return A string
#' @rdname web_chamame
chamame_field <- function(field, item){
  value <- field[[item]]
  if(length(value) == 0){ return(NA_character_) }
  paste0(as.character(value), collapse = ",")
}

#' Columns to use in the result of web chamame
#'
#' The names of the output items of web chamame,
#' in the same order as `out_cols_chamame()`.
#' Only a few items are requested, so the header row of the result
#' corresponds to the cells and the columns can be selected by their names.
#'
#' @return A character vector
#' @rdname web_chamame
cols_chamame <- function(){
  unescape_utf(
    c("\\u30ad\\u30fc\\uff08\\uff1d\\u8868\\u5c64\\u5f62\\uff09", # key (= form)
      "\\u5927\\u5206\\u985e",                                    # major class
      "\\u4e2d\\u5206\\u985e",                                    # middle class
      "\\u5c0f\\u5206\\u985e",                                    # minor class
      "\\u7d30\\u5206\\u985e",                                    # fine class
      "\\u66f8\\u5b57\\u5f62(\\u57fa\\u672c\\u5f62)"))            # base form
}

#' Extract the columns to use from the result of web chamame
#'
#' @return A dataframe
#' @rdname web_chamame
extract_chamame_cols <- function(tbl, col_lang = "jp"){
  chamame <- dplyr::select(tbl, dplyr::all_of(cols_chamame()))
  colnames(chamame) <- out_cols_chamame(col_lang = col_lang)
  return(chamame)
}

#' Check the result table of web chamame
#'
#' Web chamame returns a table even when the analysis failed,
#' e.g. a table with an error page of the server.
#' The positions of the columns are also checked,
#' because they change when web chamame changes its output items.
#' Errors are handled by web_chamame().
#'
#' @param tbl  A dataframe parsed from the response of web chamame.
#' @return A dataframe
#' @rdname web_chamame
check_chamame_table <- function(tbl){
  cols <- cols_chamame()
  key  <- cols[1]  # key (= surface form)
  # Do NOT return a wrong result silently when the output items changed
  lack <- setdiff(cols, colnames(tbl))
  if(length(lack) > 0){
    stop("The result of web chamame has no column ",
         paste0("'", lack, "'", collapse = ", "), ".")
  }
  err <- stringr::str_detect(tbl[[key]], "Internal Server Error")
  if(any(err, na.rm = TRUE)){
    stop("Web chamame returned a server error for the dictionary.")
  }
  return(tbl)
}

#' Read a web page without an error
#'
#' Helper function to fail gracefully when an internet resource is
#' not available, as required by the CRAN repository policy.
#'
#' @param url  A text. URL to read.
#' @return A xml_document, or `NULL` when the resource is not available.
#' @rdname web_chamame
read_html_safely <- function(url){
  html <- try(rvest::read_html(url), silent = TRUE)
  if(inherits(html, "try-error")){
    message(msg_not_available(url, html))
    return(NULL)
  }
  return(html)
}

#' @param cnd  A try-error object.
#' @return A string
#' @rdname web_chamame
msg_not_available <- function(url, cnd){
  paste0("Could not use the internet resource: ", url, "\n",
         "  It may not be available or may have changed.\n",
         "  Returns NULL. Original message: ",
         stringr::str_trim(as.character(cnd)))
}

#' Helper function for web_chamame
#'
#' @param form vest_form object
#' @param ... dynamic-dots Name-value pairs giving radio button to modify.
#' @return vest_form object
#'
#' @rdname web_chamame
#' @export
html_radio_set <- function(form, ...){
  new_values <- rlang::list2(...)
  v_name <- names(new_values)
  field   <- form$fields
  f_name  <- names(field)
  dup_rad <- unique(f_name[duplicated(f_name) & is_radio(field)])

  i_radio <-
    dup_rad |>
    purrr::map(`==`, f_name) |>
    purrr::map(which) |>
    `[`(_, dup_rad %in% v_name)

  for(i in seq_along(i_radio)){
    for(j in i_radio[[i]]){
      form$fields[[j]]$value <- new_values[i]
    }
  }

  return(form)
}

#' Helper function for web_chamame
#' @param fields  $fields in vest_form object
#' @return A boolean or vector
#' @rdname web_chamame
#' @export
is_radio <- function(fields){
  fields |>
    purrr::map_chr(`$`, "type") |>
    purrr::map_lgl(`==`, "radio")
}

#' @rdname moranajp_all
#' @export
mecab_all <- function(tbl, text_col = "text", bin_dir = ""){
  message("'mecab_all()' will be removed in version 1.0.0.")
  .Deprecated("moranajp_all")
  moranajp_all(tbl=tbl, text_col = text_col)
}
#' @rdname moranajp_all
#' @export
mecab <- function(tbl, bin_dir){
  message("'mecab()' will be removed in version 1.0.0.")
  .Deprecated("moranajp")
  moranajp(tbl = tbl, bin_dir = bin_dir)
}
