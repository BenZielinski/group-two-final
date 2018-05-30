# Takes a string and returns the characters up to the first space
bz_get_first_word <- function(x) {
  strsplit(x, " ")[[1]][1]
}

# Takes a string and gets the characters up to the first slash "/", then
#   converts those characters into a numeric. Returns the resulting number
bz_get_month <- function(x) {
  p <- strsplit(x, "/")[[1]][1]
  as.numeric(p)
}