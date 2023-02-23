library(dplyr)
library(tidyr)
library(lubridate)
library(purrr)
library(ggplot2)
library(rvest)
library(magrittr)
library(stringr)

as_integer.delay <- function(string) {
  as.integer(str_remove_all(string, "\\(|\\)"))
}

as_date.delay <- function(string) {
  string <- str_remove_all(string, "\\–(.*)") # rm everything after "-"
  string <- ifelse(nchar(string) == 4, paste(string, "tammikuu"), string) # left pad month
  string <- str_replace(string, "kesä$", "kesäkuu")
  string <- str_replace(string, "syksy$", "syyskuu")
  string <- paste(string, "01")
  
  date <- as.Date(string, format = "%Y %B %d")
}