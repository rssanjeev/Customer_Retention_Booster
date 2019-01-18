library(shiny)
#install.packages("shinythemes")
library(shinythemes)
#install.packages("sqldf")
library(sqldf)
#install.packages("RColorBrewer")
library(RColorBrewer)
#install.packages("wordcloud")
library(wordcloud)
#install.packages("tm")
library(tm)
install.packages("stringi")
library(stringi)
install.packages("memoise")
library(memoise)
install.packages("highcharter")
library(highcharter)
install.packages("sentimeter")
library(sentimentr)
install.packages("pacman")
library(pacman)
install.packages("XML")
library(XML)
library(dplyr)
install.packages("stringr")
library(stringr)
install.packages("rvest")
library(rvest)
install.packages("audio")
library(audio)
install.packages("DT")
library(DT)
install.packages("crosstalk")
library(crosstalk)
install.packages("dtt")
library(dtt)
install.packages("english")
library(english)
install.packages("lexicon")
library(lexicon)
install.packages("mgsub")
library(mgsub)
install.packages("qdapRegex")
library(qdapRegex)
install.packages("sentimentr")
library(sentimentr)
install.packages("shinydashboard")
library(shinydashboard)
library(tidyverse)
install.packages("splitstackshape")
library(splitstackshape)
install.packages("RSelenium")
library(RSelenium)
install.packages("wdman")
library(wdman)
install.packages("config")
library(config)
install.packages("methods")
library(methods)
require(RSelenium)

# pJS <- wdman::phantomjs()
# remDr <- remoteDriver(browserName = "phantomjs", port = 4567L)
# remDr$open()


shinyServer(function(input, output,session){
r <- reactive({
  withProgress({
    setProgress(message = "Loading Please Wait !! ")    
  i<-1
      review_all <- data.frame()
      
      while(i <= input$pages ){
        
        pJS <- wdman::phantomjs()
        Sys.sleep(5) # give the binary a moment
        
        remDr <- remoteDriver(browserName = "phantomjs", port = 4567L)
        remDr$open() 
        url <- paste0("https://www.amazon.com/product-reviews/",input$url,"/ref=cm_cr_arp_d_paging_btm_next_2?pageNumber=",i)
        remDr$navigate(url)
        Sys.sleep(5)
        
        page_source <- remDr$getPageSource()
        
        reviews <- read_html(page_source[[1]]) %>% 
          html_nodes("div.a-row.a-spacing-small.review-data") %>% 
          html_text()
        
        star <- read_html(page_source[[1]]) %>% 
          html_nodes(".a-icon-alt") %>% 
          html_text()
        
        star <- as.list(star)
        star <- star[4:13]
        star <- as.character(star)
        
        review_temp <- data.frame(stars=star,comments=reviews)
        review_all <- rbind(review_all,review_temp)
        remDr$close()
        rm(pJS)
        gc()
        i <- i+1
        review_all
      }
      
      reviews_all <- cSplit(review_all,"stars"," ")
      reviews_all <- reviews_all[,c("comments","stars_1")]
      names(reviews_all) <- c("comments","stars")
      reviews_all$stars <- as.numeric(reviews_all$stars)
      reviews_all
      })
})

observeEvent(input$Submit,{
        output$table1 <- renderDataTable({
                                          reviews_all<- r()
                                          reviews_all
                                        })


    output$hcontainer <- renderHighchart({

      input$Submit
      tt <- r()
      tt1 = sqldf("select stars,count(stars) as count from tt group by stars")

      canvasClickFunction <- JS("function(event) {Shiny.onInputChange('canvasClicked', [this.name, event.point.category]);}")
      legendClickFunction <- JS("function(event) {Shiny.onInputChange('legendClicked', this.name);}")

      highchart() %>%
        hc_chart(type = "column") %>%

        hc_title(text = "<b>Distribution of Ratings</b>",
                 margin = 20, align = "center",
                 style = list(color = "blue", useHTML = TRUE)) %>%
        hc_xAxis(title=list(text = "Ratings"),categories = unique(tt1$stars)) %>%
        hc_add_series(data = tt1$count,
                      name = "Ratings")

    })

output$tablecust1 <- renderValueBox({
      input$Submit
      a =  isolate(round(mean(r()$stars),0))
      valueBox(
        value = format(a),
        subtitle = "Average Rating",
        icon = if (a >=3.5) icon("thumbs-up") else icon("thumbs-down"),
        color = if (a >= 3.5) "aqua" else "red"
      )

    })

output$tablecust2 <- renderValueBox({
      input$Submit
      sent_agg <- with(r(), sentiment_by(as.character(r()$comments)))

      a = ifelse(mean(sent_agg$ave_sentiment)<0,"Negative",ifelse(mean(sent_agg$ave_sentiment)>0 & mean(sent_agg$ave_sentiment)<0.3,"Neutral","Positive"))
      valueBox(
        value = format(a),
        subtitle = "Average Sentiment",
        icon = if (a >= 0.1) icon("thumbs-up") else icon("thumbs-down"),
        color = if (a >= 0.1) "aqua" else "red"
      )

    })

    getPage<-function() {
      sent_agg <- with(r(), sentiment_by(as.character(r()$comments)))
      best_reviews <- isolate(slice(r(), top_n(sent_agg, 3, ave_sentiment)$element_id))
      with(best_reviews, sentiment_by(as.character(comments))) %>% highlight()
      return(includeHTML("polarity.html"))
    }

    getPage1<-function() {
      sent_agg <- with(r(), sentiment_by(as.character(r()$comments)))
      worst_reviews <- isolate(slice(r(), top_n(sent_agg, 3, -ave_sentiment)$element_id))
      with(worst_reviews, sentiment_by(as.character(comments))) %>% highlight()
      return(includeHTML("polarity.html"))
    }

output$inc<-renderUI({getPage()})
output$inc1<-renderUI({getPage1()})

    reviews_all <- r()

    wordcloud_rep  <- repeatable(wordcloud)
    output$cloud1  = renderPlot({
      reviews_all$comments=str_replace_all(reviews_all$comments,"[^[:graph:]]", " ")

      docs <- Corpus(VectorSource(reviews_all$comments))
      toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
      docs <- tm_map(docs, toSpace, "/")
      docs <- tm_map(docs, toSpace, "@")
      docs <- tm_map(docs, toSpace, "\\|")

      # Remove numbers
      docs <- tm_map(docs, removeNumbers)
      # Remove english common stopwords
      docs <- tm_map(docs, removeWords, stopwords("english"))
      # Remove your own stop word
      # specify your stopwords as a character vector
      docs <- tm_map(docs, removeWords, c("blabla1", "blabla2"))
      # Remove punctuations
      docs <- tm_map(docs, removePunctuation)
      # Eliminate extra white spaces
      docs <- tm_map(docs, stripWhitespace)
      # Text stemming
      #docs <- tm_map(docs, stemDocument)

      #docs = tm_map(docs, function(x) iconv(enc2utf8(x), sub = "byte"))

      dtm <- TermDocumentMatrix(docs)
      m <- as.matrix(dtm)
      v <- sort(rowSums(m),decreasing=TRUE)
      d <- data.frame(word = names(v),freq=v)

      wordcloud_rep(words = d$word, freq = d$freq, scale=c(4,0.5),
                    min.freq = input$freq, max.words=input$max,
                    colors=brewer.pal(8, "Dark2"))
    })

  })
 ## clearing memory
  })


