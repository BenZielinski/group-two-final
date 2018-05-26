bz_get_first_word <- function(x) {
  strsplit(x, " ")[[1]][1]
}

bz_get_month <- function(x) {
  strsplit(x, "/")[[1]][1]
}