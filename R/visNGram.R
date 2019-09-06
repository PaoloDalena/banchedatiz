#'Funzione per visualizzare n-grammi
#'
#'Questa funzione facilita le operazioni di visualizzazione degli n-grammi,
#'permettendo di personalizzare le operazioni che si vogliono eseguire.
#'Una volta visualizzati gli n-grammi delle lunghezze specificate è necessario
#'specificare manualmente le sostituzioni da effettuare,
#'utilizzando la funzione \code{gsub()}
#'
#'@param x Vettore dei testi "puliti".
#'@param ngrI Valore minimo lunghezza n-grammi (default = 2).
#'@param ngrF Valore massimo lunghezza n-grammi (default = 3).
#'@param nn Numero di n-grammi da visualizzare per ciascuna lunghezza.
#'
#'@return Gli n-grammi di lunghezza definita dai parametri \code{ngrI} e \code{ngrF}
#'
#' @examples
#' \dontrun{
#'
#' tw_amala <- search_tweets(
#' q = "inter", n = 5000, type = "recent", include_rts = T, lang="it"
#' )
#'
#'tw_amala$clean <- cleanTxt(tw_amala$text)
#'
#'visNGram(tw_amala$clean, ngrI = 2, ngrF = 4, nn = 10)
#'
#' }
#'@export

visNGram <- function(x = NULL, ngrI = 2, ngrF = 3 ,nn = 20){

  if(is.null(x) | class(x) != "character"){
    message("vettore testi non valido")
    return()
  }
  if(ngrI < 2){
    message("valore iniziale n-grammi non valido")
    return()
  }
  if(ngrF < ngrI){
    message("valori n-grammi non validi")
    return()
  }
  ngr <- seq(from = ngrI, to = ngrF, by = 1)
  vnWrd <- numeric()
  for(i in 1:length(x)){
    vnWrd[i] <- wordcount(x[i])
  }
  ngr <- rev(ngr)
  for(i in 1:length(ngr)){
    minc <- ngr[i]-1
    ng <- ngram(x[vnWrd>minc], n=ngr[i])
    ngt <- get.phrasetable(ng)
    ngt$ngrams <- iconv(ngt$ngrams,from="UTF-8",to = "latin1",sub = "byte")
    cat("--------------------","\n")
    cat("n-gram n = ",ngr[i],"\n")
    print(ngt[1:nn,])
  }
}
