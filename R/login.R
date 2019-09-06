#'Funzione interattiva per l'autenticazione via access token
#'
#'Segui le istruzioni e compila con le informazioni che trovi sul sito
#'developer.twitter.com dopo aver creato la tua app twitter secondo le
#'indicazioni che si trovano sulle dispense del Corso.
#'Inserisci insicuro = TRUE per un check dei dati inseriti.
#'
#'@param insicuro Operatore logico.
#'
#'@export

login <- function(insicuro = FALSE){
  message(
    "Ciao! Stai per permettere ad R di accedere alle informazioni di Twitter.
    Segui le istruzioni e compila con le informazioni che trovi sul sito
    developer.twitter.com dopo aver creato la tua app."
    )

  yr_appname <- readline("Inserisci il nome della tua app twitter: ")
  yr_API_Key <-  readline("Inserisci la tua API key: ")
  yr_API_Secret <-  readline("Inserisci la tua API secret key: ")
  yr_Access_Token <-  readline("Inserisci il tuo Access token: ")
  yr_Access_Token_Secret <-  readline("Inserisci il tuo Access token secret: ")

  if(insicuro == TRUE){

    message("Confermi?")
    print(yr_appname)
    print(yr_API_Key)
    print(yr_API_Secret)
    print(yr_Access_Token)
    print(yr_Access_Token_Secret)

    continua <- readline("Y/N? ")
    if (continua == "Y"){
      twitter_token <<- create_token(
        app = yr_appname,
        consumer_key = yr_API_Key,
        consumer_secret = yr_API_Secret,
        access_token = yr_Access_Token,
        access_secret = yr_Access_Token_Secret)
    }
    else{
      message("Okay, ricominciamo.")
      login()
    }
  }

  else {
    twitter_token <<- create_token(
      app = yr_appname,
      consumer_key = yr_API_Key,
      consumer_secret = yr_API_Secret,
      access_token = yr_Access_Token,
      access_secret = yr_Access_Token_Secret)
  }
}
