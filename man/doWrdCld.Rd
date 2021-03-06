% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/doWrdCld.R
\name{doWrdCld}
\alias{doWrdCld}
\title{Funzione per la costruzione delle Wordcloud}
\usage{
doWrdCld(x = NULL, stopw = NULL, col = "black", scale = c(4, 0.5),
  maxw = 100, min.freq = 10, lang = "italian", stem = F,
  weight = c("tf", "tfidf", "binary"), plot = TRUE, out = c("tdm",
  "freq", "both"))
}
\arguments{
\item{x}{Vettore di testi (i tweet ripuliti).}

\item{stopw}{Vettore con le stopwords definite dall'utente.}

\item{col}{Vettore con i colori.}

\item{scale}{Proporzioni delle parole nella wordcloud.}

\item{maxw}{Numero massimo di parole da riportare nella wordcloud.}

\item{lang}{Lingua rispetto a cui rimuovere le stopwords (default "italian")}

\item{stem}{Valore logico che indica se fare o meno lo stemming, se
\code{TRUE} procede allo stemming rispetto alla lingua specificata in \code{lang}}

\item{weight}{Tipo di ponderazione dei dati, valori:
\code{tf}     = frequenza dei termini;
\code{tfidf}  = Term Frequency, Inverse Document Frequency;
\code{binary} = binaria.}

\item{plot}{Logico, se TRUE viene visualizzata la nuvola di parole.}

\item{out}{Tipologia di output della funzione, valori:
\code{tdm}  = data frame con TermDocumentMatrix;
\code{freq} = data frame con frequenza parole;
\code{both} = list con tdm e freq.}
}
\value{
La wordcloud e il data.frame con le frequenze delle parole o il
data.frame della TermDocumentMatrix o entrambi in una lista.
}
\description{
Questa funzione crea la wordcloud partendo dal vettore dei testi e ritorna il
data.frame con le frequenze delle parole o il data.frame della
TermDocumentMatrix o entrambi in una lista; è inoltre possibile definire il
criterio di ponderazione.
}
\examples{
\dontrun{

tw_amala <- search_tweets(
q = "inter", n = 5000, type = "recent", include_rts = T, lang="it"
)

tw_amala$clean <- cleanTxt(tw_amala$text)

doWrdCld(
 tw_amala$clean,
 stopw = c("inter", "milan", "milano"),
 col = "cyan",
 maxw = 200,
 min.freq = 15,
 plot = TRUE
)

}
}
