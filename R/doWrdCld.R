#'Funzione per la costruzione delle Wordcloud
#'
#'Questa funzione crea la wordcloud partendo dal vettore dei testi e ritorna il
#'data.frame con le frequenze delle parole o il data.frame della
#'TermDocumentMatrix o entrambi in una lista; è inoltre possibile definire il
#'criterio di ponderazione.
#'
#'@param x Vettore di testi (i tweet ripuliti).
#'@param stopw  Vettore con le stopwords definite dall'utente.
#'@param col    Vettore con i colori.
#'@param scale  Proporzioni delle parole nella wordcloud.
#'@param maxw   Numero massimo di parole da riportare nella wordcloud.
#'@param lang   Lingua rispetto a cui rimuovere le stopwords (default "italian")
#'@param stem   Valore logico che indica se fare o meno lo stemming, se
#'\code{TRUE} procede allo stemming rispetto alla lingua specificata in \code{lang}
#'@param weight Tipo di ponderazione dei dati, valori:
#'\code{tf}     = frequenza dei termini;
#'\code{tfidf}  = Term Frequency, Inverse Document Frequency;
#'\code{binary} = binaria.
#'@param plot   Logico, se TRUE viene visualizzata la nuvola di parole.
#'@param out    Tipologia di output della funzione, valori:
#'\code{tdm}  = data frame con TermDocumentMatrix;
#'\code{freq} = data frame con frequenza parole;
#'\code{both} = list con tdm e freq.
#'
#'@return La wordcloud e il data.frame con le frequenze delle parole o il
#'data.frame della TermDocumentMatrix o entrambi in una lista.
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
#'doWrdCld(
#'  tw_amala$clean,
#'  stopw = c("inter", "milan", "milano"),
#'  col = "cyan",
#'  maxw = 200,
#'  min.freq = 15,
#'  plot = TRUE
#')
#'
#' }
#'@export


doWrdCld <- function(x = NULL, stopw = NULL, col = "black", scale = c(4, 0.5),
                     maxw = 100, min.freq = 10, lang = "italian", stem = F,
                     weight = c("tf","tfidf","binary"),
                     plot = TRUE, out = c("tdm", "freq", "both")){
  if(is.null(x) | class(x) != "character"){
    message("vettore testi non valido")
    return()
  }
  if(missing(out)){out="tdm"}
  if(missing(weight)){weight="tf"}
  crps <- Corpus(VectorSource(x))
  crps <- tm_map(crps, removeWords, iconv(stopwords(lang),from = "UTF-8",to = "latin1",sub = ""))
  if(!is.null(stopw)){
    crps <- tm_map(crps, removeWords, stopw)
  }
  if(stem==T){
    crps <- tm_map(crps, stemDocument, language = lang)
  }
  tdm <- TermDocumentMatrix(crps)
  if(weight == "tfidf"){
    tdm <- weightTfIdf(tdm)
  }
  if(weight == "binary"){
    tdm <- weightBin(tdm)
  }
  m <- as.matrix(tdm)
  ddm <- as.data.frame(m)
  word_freqs= sort(rowSums(m), decreasing=TRUE)
  dm <- data.frame(word=names(word_freqs), freq=word_freqs, stringsAsFactors = F)
  if(plot == TRUE){
    par(mar=c(0,0,0,0))
    wordcloud(dm$word, dm$freq,scale= scale,
              max.words= maxw,min.freq=min.freq,random.order=FALSE,
              colors=col)
  }
  switch(out,
         "tdm"=return(ddm),
         "freq"=return(dm),
         "both"=return(list(tdm=ddm,freq=dm)))
}
