# moranajp release news

# moranajp 0.9.8

* 2026-08-04
* `web_chamame()` fails gracefully when web chamame is not available
    * Shows a message and returns `NULL` instead of an error
    * `moranajp_all(method = "chamame")` also returns `NULL`
* Examples using web chamame are wrapped in `\dontrun{}`
* Follow the 2025 update of web chamame
    * Select the form fields by name, not by index
      (web chamame added 8 fields, which shifted all the indices)
    * Keep the new `dic_version` field:
      without it, web chamame returns a server error for UniDic dictionaries
    * Add `dic` argument to `web_chamame()` and `moranajp_all()`
      to select the dictionary
    * Request only the output items to use, and select the columns by name
      (the columns of web chamame changed, and the previous positions
      returned the conjugation type as the part of speech,
      and the lexeme as the base form)
* Fix `add_sentence_no()`: `cond` is evaluated in the caller's environment
* Fix `add_depend_ginza()`: remove an unused argument of `add_sentence_no()`
* Restore `brk` argument of `add_text_id()`

# moranajp 0.9.7

* 2024-07-12
* Update `moranajp()` according to web-chamame update
* Use pkgdown
* Change pipe `%>%` into `|>`

# moranajp 0.9.6

* 2023-02-28
* Add `bigram()` and related functions
* Can use "sudachi", "ginza" and "chamame"
    * Add `method` argument in `mecab()` and `mecab_all()` 
      to be able to use "sudachi", "ginza" and "chamame"

# moranajp 0.9.5

* 2022-07-12
* Fix Bugs: to apply illegal character
    * `moranajp()` add argument "iconv" to convert encoding of MeCab output
    * Remove illegal character ( &, |, <. > or ") for command in `moranajp_all()`

# moranajp 0.9.4

* 2022-05-06
* Can apply over 8000 length strings.
    * `make_groups()`
    * `make_groups_sub()`
    * `max_sum_str_length()`
* Use `purrr::map()` in `moranajp()`

# moranajp 0.9.3

* 2022-03-30
* Improve functions.
    * `moranajp_all()` <- `mecab_all()`
    * `moranajp()` <- `mecab()`
* Add tests by testthat
* Add data-raw

# moranajp 0.9.2

* bug fix

# moranajp 0.9.1

* code of line breaks will be removed to avoid declination. 

#  moranajp 0.9.0

* First release
* `mecab()`, `mecab_all()` : main functions for morphological analysis using 'MeCab'. Can use data.frame. 
* `add_text_id()`: internal function. 
* `neko`: The first part of 'I Am a Cat' by Soseki Natsume
