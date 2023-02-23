source(here::here("R/global.R"))

#' Small adjustments to harmonize names
harmonize_delay <- function(df) {
  df <- df %>%
    mutate(across(everything(), ~ as.character(.x))) %>%
    select(-Viite)
  
  names(df) <- names(df) %>%
    tolower() %>%
    str_replace_all(pattern = "ä|å", "a") %>%
    str_replace_all(pattern = "ö", "o")
    
  
  df
}

#' Read olkiluoto delay
#' 
#' @examples 
#' 
#' delay <- read_olkiluoto_delay()
#' 
#' cite_info <- attr(delay, "cite_info")
#'

read_olkiluoto_delay <- function() {
  url <- "https://fi.wikipedia.org/wiki/Olkiluoto_3_-ydinvoimalan_rakennushanke"
  delay_list <- read_html(url) %>%
    html_table() %>%
    extract(3:5)
  
  delay <- map(delay_list, harmonize_delay) %>%
    bind_rows() %>%
    transmute(n = as_integer.delay(viivastys),
              notified = as_date.delay(ilmoitusajankohta),
              est_completion = as_date.delay(kaupallinentuotantoalkaa))
  
  structure(delay,
            cite_info = list("date" = Sys.Date(),
                             "url" = url))
}

