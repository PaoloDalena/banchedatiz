#'Funzione per la pulizia dei testi
#'
#'Questa funzione facilita le operazioni di pre-trattamento e pulizia dei
#'testi, permettendo di personalizzare le operazioni che si vogliono eseguire.
#'Di default:
#'\itemize{
#'\item \bold{non} viene effettuata la conversione da \emph{utf-8} a \emph{latin1}
#'\item vengono rimossi gli \emph{hashtag} e le \emph{mention}
#'\item \bold{non} vengono rimossi i numeri
#'\item viene effettuata la conversione a minuscolo.
#'}
#'
#'@param x Vettore dei testi (tweets).
#'@param conv Logico, se \code{TRUE} converte da UTF-8 a latin1.
#'@param hash Logico, se \code{TRUE} rimuove per intero gli hashtags.
#'@param hashlist Lista con gli hashtags, necessaria se \code{hash = FALSE}
#'(è la lista \emph{hashtag} nell’oggetto tweet, restituito da
#'\code{get_timeline} o da \code{search_tweet}).
#'@param mention Logico, se \code{TRUE} rimuove per intero le mentions.
#'@param mentlist Lista con le mentions, necessaria se \code{mention = FALSE}
#'(\code{oggetto.tweet$mentions_screen_name}).
#'@param numeri Logico, se \code{TRUE} rimuove tutti i numeri.
#'@param minusc Logico, se \code{TRUE} trasforma i testi in minuscolo.
#'
#'@return Il vettore dei testi pulito, più semplice da utilizzare per le analisi.
#'
#' @examples
#' \dontrun{
#'
#' tw_amala <- search_tweets(
#' q = "inter", n = 5000, type = "recent", include_rts = T, lang="it"
#' )
#'
#' cleanTxt(
#' tw_amala$text,
#' conv = TRUE,
#' hash = TRUE,
#' mention = TRUE,
#' numeri = TRUE
#' minusc = TRUE)
#' }
#'@export

cleanTxt <- function(
  x = NULL,
  conv = FALSE,
  hash = TRUE,
  hashlist = NULL,
  mention = TRUE,
  mentlist = NULL,
  numeri = FALSE,
  minusc = TRUE
  ){

  if(is.null(x) | class(x) != "character"){
    message("vettore testi non valido")
    return()
  }
  if(hash==F & is.null(hashlist)){
    message("non definita lista hshtag")
    return()
  }
  if(mention==F & is.null(mentlist)){
    message("non definita lista mention")
    return()
  }
  xtxt <- x
  # ricodifica da UTF-8 a latin1
  if(conv == TRUE){
    xtxt = iconv(xtxt, from = "UTF-8", to = "latin1", sub = "")
  }
  # rimuove i link
  xtxt = gsub("(f|ht)(tp)(s?)(://)(.\\S+)[.|/](.\\S+)", " ", xtxt)
  # rimuove i riferimenti nei retweet
  xtxt = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", " ", xtxt)
  xtxt = gsub("(rt|via)((?:\\b\\W*@\\w+)+)", " ", xtxt)
  # hashtag
  if(hash == FALSE | hash == TRUE){
    xtxt = gsub("#\\S+", " ", xtxt)
  }
  # mention
  if(mention == FALSE | mention == TRUE){
    xtxt = gsub("@\\S+", " ", xtxt)
  }
  # punteggiatura
  xtxt = gsub("([#@])|[[:punct:]]", " \\1", xtxt)
  # caratteri di controllo
  xtxt = gsub('[[:cntrl:]]', ' ', xtxt)
  # quelli che non sono caratteri grafici (quello che non è [[:alnum:][:punct:]])
  xtxt = gsub('[^[:graph:]]', ' ', xtxt)
  # se hashtag = FALSE accoda al testo gli hashtag
  if(hash == FALSE){
    for(i in 1:length(hashlist)){
      if(!is.na(hashlist[[i]][1])){
        vhash <- iconv(hashlist[[i]],from = "UTF-8", to = "latin1",sub = "")
        newtxt <- paste(xtxt[i],paste("#",vhash,sep="",collapse = " "))
        # newtxt <- iconv(newtxt, from = "UTF-8", to = "latin1", sub = "")
        xtxt[i] <- newtxt
      }
    }
  }
  # se mention = FALSE accoda al testo le mention
  if(mention == FALSE){
    for(i in 1:length(mentlist)){
      if(!is.na(mentlist[[i]][1])){
        vment <- iconv(mentlist[[i]],from = "UTF-8", to = "latin1",sub = "")
        newtxt <- paste(xtxt[i],paste("@",vment,sep="",collapse = " "))
        # newtxt <- iconv(newtxt, from = "UTF-8", to = "latin1", sub = "")
        xtxt[i] <- newtxt
      }
    }
  }
  # numeri
  if(numeri == TRUE){
    xtxt = gsub("[[:digit:]]", "", xtxt)
  }
  # tabulazioni e più spazi in mezzo al testo
  xtxt = gsub("[ \t]{2,}", " ", xtxt)
  xtxt = gsub(' +',' ',xtxt)
  # spazi all'inizio e alla fine
  xtxt = gsub("^\\s+|\\s+$", "", xtxt)
  # trasforma tutto in minuscolo
  if(minusc == TRUE){
    xtxt = tolower(xtxt)
  }
  return(xtxt)
}
