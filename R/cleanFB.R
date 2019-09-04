cleanFB <- function(x) {
  x = gsub('http\\S+\\s*', '', x)
  x = gsub('[[:cntrl:]]', '', x)
  x = gsub("\\d", '', x)
  x = gsub('[[:punct:]]', ' ', x)
  x = gsub("^[[:space:]]*","",x)
  x = gsub("[[:space:]]*$","",x)
  x = gsub(' +',' ',x)
  x
}
