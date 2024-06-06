#' Find relative position of a common word in a sentence
#'
#' Helper function for mark()
#' @param   x,y  A string vector
#' @return  numeric from 1 to 0.
#'          1   : common word in y with x locate in a head of y.
#'          > 0 : (len - i + 1) / len;
#'                  where len is the length of y,
#'                  i is the position of common word.
#'          0   : no common word.
#' @examples
#' x <- sample(letters, 3, FALSE)
#' y <- sample(letters, 3, FALSE)
#' position_sentence(x, y)
#' 
#' @export
position_sentence <- function(x, y){
  len <- length(y)
  for(i in seq(len)){
    detected <- sum(stringr::str_detect(x, y[i]))
    if( detected ){ return( (len - i + 1) / len ) }
  }
  return(0)
}

#' Find relative position of a common word in a paragraph
#' 
#' @inheritParams  align_sentence
#' @param   word   A string
#' 
#' @export
position_paragraph <- function(df, s_id, word){
  len_y <- max(df[[s_id]])
  for(i in 2:len_y){
    y <- dplyr::filter(df, s_id == i)[[word]]
    for(j in 1:(i-1)){
      x <- dplyr::filter(df, s_id == j)[[word]]
      pos <- position_sentence(x, y)
    }
  }
}

#' Delete parenthesis and its internals
#'
#' @param   df  A dataframe analysed by MeCab
#' @return  A dataframe
#' @examples
#' library(magrittr)
#' library(dplyr)
#' data(review_mecab)
#' cols <- c("text_id", "\\u8868\\u5c64\\u5f62", "\\u54c1\\u8a5e", 
#'           "\\u54c1\\u8a5e\\u7d30\\u5206\\u985e1", "\\u539f\\u5f62") |>
#'          unescape_utf()
#' review_sudachi_a |>
#'   unescape_utf() |>
#'   dplyr::mutate(`:=`(text_id, as.numeric(text_id))) |>
#'   dplyr::filter(text_id < 5) |>
#'   dplyr::select(dplyr::all_of(cols)) |>
#'   print(n=80) |>
#'   delete_parenthesis() |>
#'   print(n=80)
#' 
#' @export
delete_parenthesis <- function(df){
  pare_begin <- unescape_utf("\\u62ec\\u5f27\\u958b")
  pare_end   <- unescape_utf("\\u62ec\\u5f27\\u9589")
  paren <- "parenthesis"
  del <- "delete"
  pos_1 <- term_pos_1(df)
  df |>
    dplyr::mutate(`:=`({{paren}}, 
      dplyr::case_when(
        .data[[pos_1]] == pare_begin ~ -1,
        .data[[pos_1]] == pare_end   ~  1,
        TRUE ~ 0
      )
    )) |>
    dplyr::mutate(`:=`({{del}}, 
      purrr::accumulate(.data[[paren]], `+`)
    )) |>
    dplyr::filter(.data[[del]] == 0 & .data[[paren]] == 0) |>
    dplyr::select(-.data[[paren]], -.data[[del]])
}


#' Align x_position of words according to common words between two sentences
#' 
#' @name align_sentence
#' @param  df    A dataframe analysed by MeCab
#' @param  s_id  A String to specify sentence
#' @param  term,x_pos A String to specify term and x_position
#' @return  A dataframe
#' @examples
#' \donttest{
#' library(magrittr)
#' library(dplyr)
#' library(purrr)
#' library(ggplot2)
#'   # settings
#' s1 <- 1:4
#' s2 <- 3:6
#' s3 <- 3:6
#' s4 <- 7:10
#' s_order <- list(s1, s2, s3, s4)
#' s_id <- "sentence"
#' term <- map2(list(letters), s_order, `[`)
#' df <- tibble::tibble(
#'         {{s_id}} := rep(seq_along(term), 
#'         purrr::map_int(term, length)),
#'         term = unlist(term),
#'         x = seq_along(term))
#'   # show dataframe
#' df
#' align_sentence(df)
#'   # plot
#' df |>
#'   align_sentence() |>
#'   dplyr::mutate(`:=`({{s_id}}, .data[[s_id]] + max(.data[[s_id]]))) |>
#'   dplyr::bind_rows(df) |>
#'   ggplot2::ggplot(aes(x, .data[[s_id]], label = term)) + 
#'     ggplot2::geom_text() + 
#'     ggplot2::theme_bw()
#' }
#' 
#' @export
align_sentence <- function(df, 
                           s_id = "sentence",
                           term = "term",
                           x_pos = "x"){
  # s_id = "sentence"; term = "term"; x_pos = "x"  # for debug
  ids <- unique(df[[s_id]])
  df_original <- df
  for(j in utils::tail(seq_along(ids), -1)){ # 2:n
    for(i in seq(from = j - 1, to = 1)){
  # print(paste0("j: ", ids[j], ", i: ", ids[i]))  # for debug
      diff <- calc_diff_x_pos(df, s_id, term, x_pos, ids[i], ids[j])
      if( sum(diff) != 0 ){ break }
      if(ids[i] == ids[1]){  # no common word: need_adjust
  # print("no match")  # for debug
        diff_correct <- 
          max(dplyr::filter(df_original, .data[[s_id]] == ids[j-1])[[x_pos]]) -
          min(dplyr::filter(df_original, .data[[s_id]] == ids[j  ])[[x_pos]])
        diff_present <- 
          max(dplyr::filter(df, .data[[s_id]] == ids[j-1])[[x_pos]]) -
          min(dplyr::filter(df, .data[[s_id]] == ids[j  ])[[x_pos]])
        diff <- diff_present - diff_correct
      }
    }
    df_aligned <- 
      df |>
      dplyr::filter(.data[[s_id]] == ids[j]) |>
      dplyr::mutate(`:=`({{x_pos}}, .data[[x_pos]] + diff))
    df <- 
      df |>
      dplyr::filter(.data[[s_id]] != ids[j]) |>
      dplyr::bind_rows(df_aligned)
  }
  #   if( length(need_adjust) ){
  #     df <- adjust_sentence(df, s_id, term, x_pos, need_adjust, str_width)
  #   }
  return(df)
}

#' Adjust x position of sentences without common term
#' 
#' @inheritParams  align_sentence
#' @param    need_adjust   A integer or vector to specify that need to adjust
#' @param    str_width     A integer or vector to adjust x position
#' @return  A dataframe
#' 
#' @export
adjust_sentence <- function(df, 
                            s_id = "sentence",
                            term = "term",
                            x_pos = "x", 
                            need_adjust, 
                            str_width
                            ){
  # df <- review_sudachi_c; s_id = "sentence"; term = "term"; x_pos = "x"  # for debug
  ids <- unique(df[[s_id]])
  for(j in need_adjust){
    i <- ids[match(j, ids) - 1]
    former_x <- max(dplyr::filter(df, .data[[s_id]] == i)[[x_pos]])
    latter_x <- min(dplyr::filter(df, .data[[s_id]] == j)[[x_pos]])
    diff <- former_x - latter_x + str_width[[j]]
    df_adjusted <- 
      df |>
      dplyr::filter(.data[[s_id]] >= j) |>
      dplyr::mutate(`:=`({{x_pos}}, .data[[x_pos]] + diff))
    df <- 
      df |>
      dplyr::filter(.data[[s_id]] < j) |>
      dplyr::bind_rows(df_adjusted)
  }
  return(df)
}


#' Calculate difference of x_position of common word between two sentences
#' 
#' @inheritParams  align_sentence
#' @param    i,j   A integer to specify sentence number
#' @return  A numeric
#' @examples
#' s1 <- letters[1:4]
#' s2 <- letters[3:6]
#' term <- c(s1, s2)
#' df <- tibble::tibble(
#'         sentence = rep(1:2, c(length(s1), length(s2))), 
#'         term = term,
#'         x = seq_along(term))
#' s_id <- "sentence"
#' term <- "term"
#' x_pos <- "x"
#' calc_diff_x_pos(df, s_id, term, x_pos, 1, 2)
#' 
#' intersect(1:3 ,4:6)
#' 
#' @export
calc_diff_x_pos <- function(df, s_id, term, x_pos, i, j){
  former <- dplyr::filter(df, .data[[s_id]] == i)
  latter <- dplyr::filter(df, .data[[s_id]] == j)
  former_w <- former[[term]]
  latter_w <- latter[[term]]
  matched_w  <- intersect(latter_w, former_w)[1]
  if(is.na(matched_w)){
    diff <- 0
    return(diff)
  }
  former_pos <- match(matched_w, former_w)
  latter_pos <- match(matched_w, latter_w)
  x_former <- former[[x_pos]][former_pos]
  x_latter <- latter[[x_pos]][latter_pos]
  diff <- x_former - x_latter
print(paste0(j, "-", i, ": ", matched_w, diff))
  return(diff)
}


#' Add word ids in a sentence
#' 
#' @inheritParams  align_sentence
#' @param   w_id   A string to specify word id
#' @return  A dataframe
#' @examples
#' df <- tibble::tibble(s_id = rep(1:4, 4:1))
#' add_word_id(df, s_id = "s_id", w_id = "w_id")
#' 
#' @export
add_word_id <- function(df, s_id, w_id){
  df |>
    dplyr::group_by(.data[[s_id]]) |>
    dplyr::mutate(`:=`({{w_id}}, dplyr::row_number())) |>
    dplyr::ungroup()
}
