#'Funzione per la lemmatizzazione di una Term-Document Matrix
#'
#'Questa funzione richiede in input un data.frame con una matrice
#'TerminiDocumenti, come quella prodotta dalla funzione \code{doWrdCld}.
#'Questa funzione necessita dell'utilizzo del vocabolario \code{formelemmi},
#'già presente di default nella libreria.
#'
#'@param x Data.frame con Term-Document Matrix.
#'
#'@return Un data frame LemmiDocumenti, con la categoria grammaticale e la
#'frequenza totale dei lemmi.
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
#'tw_amala_tdm <- doWrdCld(tw_amala$clean, plot = FALSE)
#'
#'tw_amala_lem <- lemmaTDM(tw_amala_tdm)
#'
#' }
#'@export

lemmaTDM <- function(x = NULL){
  # FUNZIONE PER LA LEMMATIZZAZIONE DELLA TERM-DOCUMENT-MATRIX
  # x = data frame con term-document-matrix come ritornata da doWrdCld
  if(is.null(x) | class(x) != "data.frame"){
    message("data frame TDM non valido")
    return()
  }
  if(exists("formelemmi") == FALSE){
    message("data frame formelemmi non presente")
    return()
  }
  require(dplyr)
  FrmLmm <- formelemmi
  FrmLmm$lemma <- iconv(FrmLmm$lemma, to = "UTF-8", sub = "byte")
  vLem <- character()
  vCGr <- character()
  {pb <- txtProgressBar(min = 0,max = nrow(x),style=3)
    for(i in 1:nrow(x)){
      setTxtProgressBar(pb,i)
      prl <- rownames(x)[i]
      ll <- FrmLmm[FrmLmm$forma==prl,c("lemma","CatGrL")]
      if(nrow(ll)>0){
        vLem[i] <- ll[1, "lemma"]
        vCGr[i] <- ll[1, "CatGrL"]
      } else {
        vLem[i] <- prl
        vCGr[i] <- "unknown"
      }
    }
    close(pb)}
  dftmp <- data.frame(forma = rownames(x), lemma = vLem,
                      CatGr = vCGr, x, Tot = rowSums(x), stringsAsFactors = F)
  rownames(dftmp) <- NULL
  dfout <- dftmp[,-1] %>%
    group_by(lemma,CatGr) %>%
    summarise_all(sum)
  dfout <- as.data.frame(dfout)
  dfout <- dfout[order(dfout$Tot,decreasing = T),]
  return(dfout)
}
