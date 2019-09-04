cleanTweets3 <-function(tweet){
  tweet= gsub("(f|ht)(tp)(s?)(://)(.*)[.|/](.*)", " ", tweet)
  tweet= gsub("(RT|via)((?:\\b\\W*@\\w+)+)", " ", tweet)
  tweet= gsub("(rt|via)((?:\\b\\W*@\\w+)+)", " ", tweet)
  tweet= gsub("([#@])|[[:punct:]]", " \\1", tweet)
  # Thenremovenumbers, weneedonlytext for analytics
  # tweet= gsub("[[:digit:]]", " ", tweet)
  # finally, weremoveunnecessaryspaces(whitespaces, tabsetc)
  tweet= gsub("[ \t]{2,}", " ", tweet)
  tweet= gsub("^\\s+|\\s+$", "", tweet)
  tweet
}
