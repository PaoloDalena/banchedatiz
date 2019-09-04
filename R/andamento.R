#' Andamento nel tempo del numero di tweet, favoriti e retweet
#'
#' Osserva l'andamento nel tempo del numero di tweet, favoriti e retweet in un
#' determinato periodo di tempo, da specificare in freq.
#' Se grafico = TRUE, restituisce il grafico dei tre andamenti, se grafico
#' = FALSE, restituisce solo la tibble riassuntiva.
#'
#' @param twtbl Insieme di tweet.
#' @param freq Ogni quanto vengono raggruppati i tweet.
#' @param grafico Operatore logico.
#'
#' @return Un grafico o una tibble riassuntiva degli andamenti del numero di
#'  tweet, favoriti e retweet nel tempo.
#' @examples
#' \dontrun{
#'
#' tw_amala <- search_tweets(
#' q = "inter",n = 5000,type = "recent",include_rts = T,lang="it"
#' )
#'
#' andamento(tw_amala, "38 mins", grafico = F)
#' andamento(tw_amala, "2 hours", grafico = T)
#' }

andamento <- function(twtbl, freq, grafico = TRUE){
  tab <- twtbl %>%
    dplyr::filter(is_retweet==FALSE) %>%
    group_by(orario=lubridate::floor_date(created_at, freq)) %>%
    summarise(n=n(),fav=sum(favorite_count),ret=sum(retweet_count))

  if (as.logical(grafico)==TRUE) {
    ggplot(tab, aes(orario)) +
      geom_line(aes(y = fav, colour = "favorite")) +
      geom_line(aes(y = ret, colour = "retweet"))+
      geom_line(aes(y = n, colour = "numero"),size= 1)+
      ggtitle(stringr::str_c("Numero di Tweet, Favoriti e ReTweet per ",freq)) +
      xlab("") + ylab("") +
      theme_light()
  }
  else {return(tab)}
}
