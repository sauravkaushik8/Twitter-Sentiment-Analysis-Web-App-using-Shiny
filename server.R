
  
  library(shiny)
  

  shinyServer(function(input, output) {
    

    
    
    library('twitteR')
    library('ROAuth')
    
    
    consumerKey="XXX"
    consumerSecret="XXX"
    accessURL="XXX"
    authURL="XXX"
    reqURL="XXX"
    

    
    
    
    Cred <- OAuthFactory$new(consumerKey=consumerKey,
                             consumerSecret=consumerSecret,
                             requestURL=reqURL,
                             accessURL=accessURL,
                             authURL=authURL)
    
    setup_twitter_oauth(consumerKey,consumerSecret,access_token=NULL,access_secret=NULL)
    
    
    
    
    output$plot<-renderPlot({ 
      if(input$pType=='a')
      {
        
        #access tweets and create cumulative file
        
        
        searchterm<-input$term
        num<-input$i    
    
    
        
        list <- searchTwitter(searchterm,n= num, lang="en", since=NULL, until=NULL, retryOnRateLimit=10) 
        df <- twListToDF(list)
        df <- df[, order(names(df))]
        df$created <- strftime(df$created, '%Y-%m-%d')
        if (file.exists(paste(searchterm, '_stack.csv'))==FALSE) write.csv(df, file=paste(searchterm, '_stack.csv'), row.names=F)
        #merge last access with cumulative file and remove duplicates
        stack <- read.csv(file=paste(searchterm, '_stack.csv'))
        stack <- rbind(stack, df)
        stack <- subset(stack, !duplicated(stack$text))
        write.csv(stack, file=paste(searchterm, '_stack.csv'), row.names=F)
        
        
        #evaluation tweets function
        score.sentiment <- function(sentences, pos.words, neg.words, .progress='none')
        {
          library("plyr")
          library("stringr")
          scores <- laply(sentences, function(sentence, pos.words, neg.words){
            
            
            
            
            sentence <- iconv(sentence, "latin1", "ASCII//TRANSLIT")
            
            
            sentence <- iconv(sentence, to='ASCII//TRANSLIT')
            
            
            
            
            sentence <- gsub('[[:punct:]]', "", sentence)
            sentence <- gsub('[[:cntrl:]]', "", sentence)
            sentence <- gsub('\\d+', "", sentence)
            sentence <- tolower(sentence)
            word.list <- str_split(sentence, '\\s+')
            words <- unlist(word.list)
            pos.matches <- match(words, pos.words)
            neg.matches <- match(words, neg.words)
            pos.matches <- !is.na(pos.matches)
            neg.matches <- !is.na(neg.matches)
            score <- sum(pos.matches) - sum(neg.matches)
            return(score)
          }, pos.words, neg.words, .progress=.progress)
          scores.df <- data.frame(score=scores, text=sentences)
          return(scores.df)
          detach("package:plyr", unload = TRUE)
        }
        library("ggplot2")
        pos <- scan('http://ptrckprry.com/course/ssd/data/positive-words.txt', what='character', comment.char=';') #folder with positive dictionary
        neg <- scan('http://ptrckprry.com/course/ssd/data/negative-words.txt', what='character', comment.char=';') #folder with negative dictionary
        
        pos.words <- c(pos, 'upgrade')
        neg.words <- c(neg, 'wtf', 'wait', 'waiting', 'epicfail')
        Dataset <- stack
        Dataset$text <- as.factor(Dataset$text)
        scores <- score.sentiment(Dataset$text, pos.words, neg.words, .progress='text')
        write.csv(scores, file=paste(searchterm, '_scores.csv'), row.names=TRUE) #save evaluation results into the file
        #total evaluation: positive / negative / neutral
        stat <- scores
        stat$created <- stack$created
        stat$created <- as.Date(stat$created)
        stat <- mutate(stat, tweet=ifelse(stat$score > 0, 'positive', ifelse(stat$score < 0, 'negative', 'neutral')))
        require("dplyr")
        by.tweet <- group_by(stat, tweet, created)
        by.tweet <- summarise(by.tweet, number=n())
        detach("package:dplyr", unload = TRUE)
        write.csv(by.tweet, file=paste(searchterm, '_opin.csv'), row.names=TRUE)
        #create chart
        
        
        (ggplot(by.tweet, aes(created,number)) + geom_line(aes(group=tweet, color=tweet), size=2) +
          geom_point(aes(group=tweet, color=tweet), size=4) +
          theme(text = element_text(size=18), axis.text.x = element_text(angle=90, vjust=1)) +
          stat_summary(fun.y = 'sum', fun.ymin='sum', fun.ymax='sum', colour = 'yellow', size=2, geom = 'line') + 
          ggtitle(searchterm) + 
          xlab("Date of Tweet") + 
          ylab("Sentiments")   
        )
        
        
        
        
        
      }
      else if(input$pType=='b')
      {
        
        
        
        searchterm<-input$term
        num<-input$i
        
        #access tweets and create cumulative file
        list <- searchTwitter(searchterm,n= num, lang="en", since=NULL, until=NULL, retryOnRateLimit=10) 
        
        df <- twListToDF(list)
        df <- df[, order(names(df))]
        df$created <- strftime(df$created, '%Y-%m-%d')
        if (file.exists(paste(searchterm, '_stack.csv'))==FALSE) write.csv(df, file=paste(searchterm, '_stack.csv'), row.names=F)
        #merge last access with cumulative file and remove duplicates
        stack <- read.csv(file=paste(searchterm, '_stack.csv'))
        stack <- rbind(stack, df)
        stack <- subset(stack, !duplicated(stack$text))
        write.csv(stack, file=paste(searchterm, '_stack.csv'), row.names=F)
        
        
        #evaluation tweets function
        score.sentiment <- function(sentences, pos.words, neg.words, .progress='none')
        {
          library("plyr")
          library("stringr")
          scores <- laply(sentences, function(sentence, pos.words, neg.words){
            
            
            
            
            sentence <- iconv(sentence, "latin1", "ASCII//TRANSLIT")
            
            
            sentence <- iconv(sentence, to='ASCII//TRANSLIT')
            
            
            
            
            sentence <- gsub('[[:punct:]]', "", sentence)
            sentence <- gsub('[[:cntrl:]]', "", sentence)
            sentence <- gsub('\\d+', "", sentence)
            sentence <- tolower(sentence)
            
            
            
            word.list <- str_split(sentence, '\\s+')
            words <- unlist(word.list)
            pos.matches <- match(words, pos.words)
            neg.matches <- match(words, neg.words)
            pos.matches <- !is.na(pos.matches)
            neg.matches <- !is.na(neg.matches)
            score <- sum(pos.matches) - sum(neg.matches)
            return(score)
          }, pos.words, neg.words, .progress=.progress)
          scores.df <- data.frame(score=scores, text=sentences)
          return(scores.df)
          detach("package:plyr", unload = TRUE)
          
        }
        library("ggplot2")
        pos <- scan('http://ptrckprry.com/course/ssd/data/positive-words.txt', what='character', comment.char=';') #folder with positive dictionary
        neg <- scan('http://ptrckprry.com/course/ssd/data/negative-words.txt', what='character', comment.char=';') #folder with negative dictionary
        
        pos.words <- c(pos, 'upgrade')
        neg.words <- c(neg, 'wtf', 'wait', 'waiting', 'epicfail')
        Dataset <- stack
        Dataset$text <- as.factor(Dataset$text)
        scores <- score.sentiment(Dataset$text, pos.words, neg.words, .progress='text')
        write.csv(scores, file=paste(searchterm, '_scores.csv'), row.names=TRUE) #save evaluation results into the file
        #total evaluation: positive / negative / neutral
        stat <- scores
        stat$created <- stack$created
        stat$created <- as.Date(stat$created)
        stat <- mutate(stat, tweet=ifelse(stat$score > 0, 'positive', ifelse(stat$score < 0, 'negative', 'neutral')))
        require("dplyr")
        by.tweet <- group_by(stat, tweet, created)
        by.tweet <- summarise(by.tweet, number=n())
        detach("package:dplyr", unload = TRUE)
        write.csv(by.tweet, file=paste(searchterm, '_opin.csv'), row.names=TRUE)
        #create chart
        
        qplot(scores$score, geom="histogram",xlim = c(-5,5),xlab = "Tweet Sentiment Score (-5 to +5)",ylab="Count",main = searchterm) 
        
        
      }
      
      else if(input$pType=='c')
      {
        
        
        searchterm<-input$term
        num<-input$i
        
        list <- searchTwitter(searchterm, n= num, lang="en", since=NULL, until=NULL, retryOnRateLimit=10) 
        
        library("twitteR")
        library("wordcloud")
        library("tm")
        
        l <- sapply(list, function(x) x$getText())
        
        l <- iconv(l, "latin1", "ASCII//TRANSLIT")
        
        
        l <- iconv(l, to='ASCII//TRANSLIT')
        
        
        
        
        #create corpus
        lc <- Corpus(VectorSource(l))
        
        #clean up
        
        
        
        
        
        lc <- tm_map(lc, content_transformer(tolower)) 
        lc <- tm_map(lc, removePunctuation)
        lc <- tm_map(lc, function(x)removeWords(x,stopwords()))
        
        library(RColorBrewer)
        pal2 <- brewer.pal(8,"Dark2")
        wordcloud(lc,min.freq=num/200,max.words=500, random.order=T, colors=pal2)      
        
      }
    }
    )
    
  }) 
  #})
  
  