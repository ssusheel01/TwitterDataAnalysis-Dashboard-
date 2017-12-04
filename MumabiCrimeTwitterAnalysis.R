library(shiny)
#library(shinydashboard)

library(twitteR)
library(wordcloud)
library(RColorBrewer)
library(plyr)
library(ggplot2)
library(sentiment)

library(tm)
library(Rstem)

# Find OAuth settings for twitter:
library(httr)
library("R6")
library("ROAuth")

ui <- fluidPage(
  titlePanel("Sentiment Analysis"),
  textOutput("currentTime"),
  h4("Tweets:"),
  sidebarLayout(position = "left",
                sidebarPanel("sidebar panel",
                             checkboxInput("do2", "Make 2 plots", value = T),
                             dataTableOutput("tweets_table")),
                mainPanel("main panel",
                          fluidRow(
                            splitLayout(cellWidths = c("50%", "50%"), plotOutput("distPlot1"), plotOutput("distPlot2"))
                            ),
                          fluidRow(plotOutput("distPlot3"))
                          )
                )
  )


server <- function(input, output, session) {
  
  setup_twitter_oauth(consumer_key = "do5pxJOxRmc18iSltwhX0kQ9T", consumer_secret = "dkOgsGotAR1wOV6faJj6bA5jKR6qxTNYe7cO9N6YwU4oP4jMKc")
  
  token <- get("oauth_token", twitteR:::oauth_cache)
  token$cache()
  
  output$currentTime <- renderText({invalidateLater(1000, session) 
    paste("Current time is: ",Sys.time())})
  
  observe({
    
    invalidateLater(60000,session)
    
    tweets_result = ""
    
    tweets_results = searchTwitter("mumbai+crime", n=2000, lang = "en")
    tweets_result = sapply(tweets_results, function(x) x$getText())
    tweets_result = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", tweets_result) #retweets
    tweets_result = gsub("@\\w+", "", tweets_result)  # all "@people"
    tweets_result = gsub("[[:punct:]]", "", tweets_result) 
    tweets_result = gsub("[[:digit:]]", "", tweets_result)
    tweets_result = gsub("http\\w+", "", tweets_result) #html links
    tweets_result = gsub("[ \t]{2,}", "", tweets_result) #white spaces
    tweets_result = gsub("^\\s+|\\s+$", "", tweets_result) #"slang words"
    
    catch.error = function(x){
      y = NA
      catch_error = tryCatch(tolower(x), error=function(e) e)
      if (!inherits(catch_error, "error"))
        y = tolower(x)
      return(y)
    }
    
    tweets_result = sapply(tweets_result, catch.error)
    tweets_result = tweets_result[!is.na(tweets_result)]
    names(tweets_result) = NULL
    bjp_class_emo = classify_emotion(tweets_result, algorithm="bayes", prior=1.0)
    emotion = bjp_class_emo[,7]
    emotion[is.na(emotion)] = "unknown"
    
    bjp_class_pol = classify_polarity(tweets_result, algorithm="bayes")
    polarity = bjp_class_pol[,4]
    
    sentiment_dataframe = data.frame(text=tweets_result, emotion=emotion, polarity=polarity, stringsAsFactors=FALSE)
    sentiment_dataframe = within(sentiment_dataframe, emotion <- factor(emotion, levels=names(sort(table(emotion), decreasing=TRUE))))
    
    
    df_users_sentiment <- data.frame(sentiment_dataframe)
    
    output$tweets_table = renderDataTable({
      df_users_sentiment
    })
    
    output$distPlot1 <- renderPlot ({
      
      results = data.frame(sentiment_dataframe)
      
        ggplot(sentiment_dataframe, aes(x=emotion)) + geom_bar(aes(y=..count.., fill=emotion)) +
        scale_fill_brewer(palette="Dark2") +
        ggtitle('Sentiment Analysis of Tweets on Twitter about Mumbai Crime') +
        theme(legend.position='right') + ylab('Number of Tweets') + xlab('Emotion Categories')
        
    })
    output$distPlot2 <- renderPlot ({
      
      results = data.frame(sentiment_dataframe)
      
      ggplot(sentiment_dataframe, aes(x=polarity)) +
        geom_bar(aes(y=..count.., fill=polarity)) +
        scale_fill_brewer(palette="RdGy") +
        ggtitle('Sentiment Analysis of Tweets on Twitter about Mumbai Crime') +
        theme(legend.position='right') + ylab('Number of Tweets') + xlab('Polarity Categories')
      
    })
    output$distPlot3 <- renderPlot ({
      
      tweet_emos = levels(factor(sentiment_dataframe$emotion))
      n_tweet_emos = length(tweet_emos)
      tweet.emo.docs = rep("", n_tweet_emos)
      
      for (i in 1:n_tweet_emos){
        tmp = tweets_result[emotion == tweet_emos[i]]
        tweet.emo.docs[i] = paste(tmp, collapse="")
      }
      
      tweet.emo.docs = removeWords(tweet.emo.docs, stopwords("english"))
      tweet.corpus = Corpus(VectorSource(tweet.emo.docs))
      tweet.tdm = TermDocumentMatrix(tweet.corpus)
      tweet.tdm = as.matrix(tweet.tdm)
      colnames(tweet.tdm) = tweet_emos
      
      comparison.cloud(tweet.tdm, colors = brewer.pal(n_tweet_emos, "Dark2"),
                       scale = c(3,.5), random.order = FALSE, title.size = 1.5)
      
    })
  })
}

shinyApp(ui = ui, server = server)

