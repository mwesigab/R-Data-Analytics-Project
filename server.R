library(shiny)
library(shinydashboard)
library(data.table)
library(dplyr)
library(ggplot2)
library(reshape2)
library(grid)
library(colorspace)
library(gridExtra)
library(memisc)
library(sqldf)
library(lubridate)
library(tm)
library(wordcloud)
library(RColorBrewer)
library(RWeka)
library(quanteda)
library(syuzhet)
library(maps)
library(mapdata)
library(forecast)
library(plotrix)

shinyServer(
  
  function(input, output, session){

    myData <- reactive({
      file1<-input$file
      if(is.null(file))return(NULL)
      
      data<-read.csv(file1$datapath, header = TRUE)
      data
    })
    
    output$contents <- renderTable(
      myData()
    )
    
    output$date_and_Waitime <- renderPlot({
      
      plot(myData()$Date, myData()$Wait.time, xlab="Date",ylab="Waiting time",col="blue")
      
    })
    

    
    read<-function(){
      if(is.null(input$file))
        return(NULL)
      library(ggplot2)
      
      infile<-input$file
      data<-read.csv(infile$datapath)
      
      df<-data.frame(
        visitor=data$Visitor.Name,
        email=data$E.mail,
        country=data$Country,
        phone=data$Phone,
        Wait=data$Wait.time,
        Minute=data$Minutes,
        vistior=data$Visitor.Name,
        operator=data$Operator,
        IP=data$IP,
        Page=data$Page,
        Date=data$Date,
        chat=data$Chat.content
      )
      df 
    }
    # reading the two tables in the data frame
    output$mytable1<-renderDataTable({read()[,1:6]})
    output$mytable2<-renderDataTable({read()[,7:12]})
    
###########EDITING###########################
    
    output$statement <- renderText(
      paste("They are ", sum(is.na(myData()$City)))
    )
    
    output$total <- renderText(
      paste("Number of records is ", nrow(myData()))
    )
    
    output$grouping <- renderText(
      paste("Uganda appears ", nrow(filter(myData(), Country == "Uganda")))

    )
    
    output$columns <- renderText(
      arrange(myData()$ID, as.numeric(myData()$Date))
      
    )
    
    output$countries <- renderText(
      table(myData()$Country)
      
    )
    

    
#######  PIE-CHART ##################
    
    output$pie2 <- renderPlot(
      pie(table(myData()$Date),main = "Appearance of dates", col=rainbow(length(table(myData()$Date))), width=10, height=6)
      
    )
    
    output$pietable2<-renderTable({
      tx4 <- table(myData()$Date)
      tx4[order(-tx4)]
    })
    
    output$pie2number <- renderText({
      nrow(table(myData()$Date))
    })
    
    output$pie3 <- renderPlot({
      
      piepercent<- paste0(round(100*table(myData()$Mail.send)/sum(table(myData()$Mail.send)), 1),"%")
      
      pie3D(table(myData()$Mail.send),labels=piepercent ,main = "Pie chart showing Mail send status", col=rainbow(length(table(myData()$Mail.send))), explode = 0.1)
      legend("topright", c("Yes","No"), cex=0.8,
             fill=rainbow(length(table(myData()$Mail.send))))
      
    })
    
    output$pietable3 <- renderTable({
     tx3 <- table(myData()$Mail.send)
     tx3[order(-tx3)]
      
    })
    
    output$pie4 <- renderPlot({
      
      piepercent<- paste0(round(100*table(myData()$Vote.status)/sum(table(myData()$Vote.status)), 1),"%")
      
      pie3D(table(myData()$Vote.status),labels=piepercent,main = "Pie chart showing vote status", col=rainbow(length(table(myData()$Vote.status))), explode=0.1)
      legend("topright", c("DOWN","NONE","UP"), cex=0.8,
             fill=rainbow(length(table(myData()$Vote.status))))
      
    })
    
    output$pietable4 <- renderTable({
      
      tx2 <- table(myData()$Vote.status)
      tx2[order(-tx2)] 
      
    })
    
    output$pie5 <- renderPlot({
      
      piepercent<- paste0(round(100*table(myData()$Operator)/sum(table(myData()$Operator)), 1),"%")
      
      pie3D(table(myData()$Operator),labels=piepercent,main = "Pie chart showing Operator operations",explode=0.1)
      legend("topright", c("Diana(customerservice@nssfug.org)","fachieng(customerservice@nssfug.org)","pkyomugisha(pkyomugisha@nssfug.org)","snalwoga(snalwoga@nssfug.org)"), cex=0.8,
             fill=rainbow(length(table(myData()$Operator))))
      
      
    })
    
    output$pietable5 <- renderTable({
      tx1 <- table(myData()$Operator)
      tx1[order(-tx1)] 
      
    })
    
    ############# BAR_CHART ############################
    
    output$bar1 <- renderPlot({
      
      d <- myData()
      
      Date.freq = table(as.Date(d$Date,"%m/%d/%Y"))
      barplot(Date.freq, col="pink", xlab="Days", ylab="Number of chats",main="Barchart showing number of chats against days")
      
    })
    
    output$bar1table<-renderTable({
      tx11 <- table(myData()$Date)
      tx11[order(-tx11)]
    })
    
    output$bar2 <- renderPlot({
      
      d <- myData()
      d$Minutes <- as.POSIXct(d$Minutes, format="%H:%M:%S")
      
      qplot(d$Minutes, data = d, geom = "histogram", binwidth = 60, xlab = "Time", main="Histogram showing frequency(minute count) against minutes")
     
    })
    
    output$bar3 <- renderPlot({
      
      d <- myData()
      
      country.freq = table(d$Country)
      
      par(mar=c(8,15,5,8))
      barplot(country.freq, col="blue", xlab="Frequency", ylab="Country",horiz = TRUE, las = 1, main="Barchart showing countries against frequency")
    
    })
    
    output$bar3b <- renderPlot({
      
      d <- myData()
      
      country.freq = table(d$Country)
      
      par(mar=c(8,15,5,8))
      barplot(country.freq, col="blue", xlab="Waiting time", ylab="Country",horiz = TRUE, las = 1)
      
    })
    
    output$bar3number <- renderText({
      d <- myData()
      nrow(table(d$Country))
    })
    
    output$bar3table <- renderTable({
      
      tx5 <- table(myData()$Country)
      tx5[order(-tx5)]
      
    })
    
    output$bar4 <- renderPlot({
      
      d <- myData()
      colors = c("red", "yellow", "green")
      vote.freq = table(d$Vote.status)
      barplot(vote.freq, col=colors,  xlab="Vote status", ylab="Number of customers", main="Barchart showing number of customers against vote status")
      
    })
    
    output$bar4number <- renderText({
      d <- myData()
      nrow(table(d$Vote.status))
    })
    
    output$bar4table <- renderTable({
      
      tx9 <- table(myData()$Vote.status)
      tx9[order(-tx9)]
      
    })
    
    output$bar5 <- renderPlot({
      
      d <- myData()
      colors = c("red", "yellow", "green","blue")
      operator.freq = table(d$Operator)
      
      par(mar=c(8,20,5,5))
      barplot(operator.freq, col=colors, xlab="Frequency", ylab="Operators", horiz= TRUE,las=1, main="Barchart showing number of customers(chats) against operators")
      
    })
    
    output$bar5table <- renderTable({
      
      tx5 <- table(myData()$Operator)
      tx5[order(-tx5)]
      
    })
    
    output$bar5number <- renderText({
      d <- myData()
      nrow(table(d$Operator))
    })
    
    output$bar6 <- renderPlot({
      
      d <- myData()
      
      page.freq = table(d$Page)
      
      par(mar=c(8,50,5,3))
      barplot(page.freq, col="red", xlab="Frequency", horiz = TRUE, las = 1, main="Barchart showing pages against frequency(count)")
      
    })
    
    output$table6 <- renderTable({
      d <- myData()
      tx <- table(d$Page)
      tx[order(-tx)] 
      
    })
    
    output$bar6number <- renderText({
      d <- myData()
      nrow(table(d$Page))
    })
    
    

    
    ############# LINE_CHART ############################
    
    output$line1 <- renderPlot(
      qplot(myData()$Wait.time, myData()$Country, data = myData(), geom = "line", main = "Country vs Wait time", xlab = "Waiting time", ylab = "Country")
    )
    
    output$line2 <- renderPlot({
      
      d <- myData()
      
      plot(table(as.Date(d$Date,"%m/%d/%Y")),type="l",
           xlab="Date",ylab="Count", col="red")
      
    })
    
    output$line2table <- renderTable({
      txy <- table(myData()$Date)
      txy[order(-txy)]
    })
    
    output$average <- renderText({
      
      mean(myData()$Wait.time)
      
    })
    
    output$line3 <- renderPlot({ 
      
      d <- myData()
    d$Minutes <- as.POSIXct(d$Minutes, format="%H:%M:%S")
    
    plot(d$Minutes ~ d$ID,type="l",
         xlab="ID",ylab="TIME", col="orange")
    
    
    })

   ############# SCATTER PLOT ############################

output$ID_and_time <- renderPlot({
  
  d <- myData()
  d$Minutes <- as.POSIXct(d$Minutes, format="%H:%M:%S")
  Minutes <- d$Minutes
  ID <- d$ID
  ggplot(data=d, aes(x=Minutes,y=ID)) + geom_point() 
  
  
})

    
    ########### BREAKING DOWN THE CSV DATA ################
    
    myData <- reactive({
      file1<-input$file
      if(is.null(file))return(NULL)
      
      data<-read.csv(file1$datapath, header = TRUE)
      data
    })
    
    output$contents <- renderTable(
      myData()
    )
    
    
    output$save <- downloadHandler(
      filename = function() {
        paste(input$file1, '.csv', sep='') },
      content = function(file) {
        write.csv(input$file1, file)
      }
    ) 
    output$table<-renderTable({
      mydataDate <- gsub(" .*","",read()$Date)
      mydataDateFreq <- as.data.frame(table(mydataDate))
      colnames(mydataDateFreq) <- c("Date", "Number Of Customers")
      labs <- colnames(mydataDateFreq)
      mydataDateFreq
    })
    
    
    
    plotter <- function(distTypeProt, dtsets, labs){
      if(distTypeProt == "Histogram"){
        
        hist(as.numeric(unlist(dtsets[2])),xlab="Days",col="green",border="red",xlim = c(0,40),main=" histogram showing number of chats", ylim = c(0,5),
             breaks = 5 )
      }
      else if(distTypeProt == "Barplot"){
        dtsets <- as.data.frame(dtsets)
        colnames(dtsets) <- c("Number", "Chats")
        barplot(as.numeric(unlist(dtsets[2])),names.arg=as.vector(unlist(dtsets[1])),xlab="days",ylab="chats",col="blue",
                main="Number of chats ",border="red")
      }
      else{
        pie(as.numeric(unlist(dtsets[2])),labels=as.vector(unlist(dtsets[1])),explode=0.1, main="PIE-CHART FOR DATES",
            col=rainbow(length(as.numeric(unlist(dtsets[2]))))
        )
      }
    }
    #Plotting the graph for the date
    output$histogram <- renderPlot({
      distType <- input$visual
      mydataDate <- gsub(" .*","",read()$Date)
      mydataDateFreq <- as.data.frame(table(mydataDate))
      colnames(mydataDateFreq) <- c("Date", "Chats")
      labs <- colnames(mydataDateFreq)
      
      plotter(distType, mydataDateFreq, labs)
      
    })
    
    output$mytext <- renderTable(
      select(myData()$Date, myData()$Minutes)
    )
    
    ########### DEALING WITH WORDS AND QUESTIONS ################
    
    output$words1 <- renderTable({
      
      nssfdata<-myData()$Chat.content
      nssfdata<-tolower(nssfdata)
      nssfdata<-paste(nssfdata, collapse = "")
      nssfdata<-VCorpus(VectorSource(nssfdata))
      inspect(nssfdata)
      
      
      nssfdata.ng = tm_map(nssfdata,removeWords,c(stopwords("english"),"welcome to nssf live chat, hold on while we attend to your query",
                                                  "olivia kyomugisha tamale","date of birth", "mother", "name", "father's name","one","prefer","like to know","can","get","send",
                                                  "diana","catherine","profulla kumar sarker","akurut","chat","nssf","system assistant","visitor has closed the chat explicitly",
                                                  "has accepted the chat","poover kyomugisha","customerservice@nssfug.org","samali nalwoga","nearest", "office","good morning","thank",
                                                  "Kindly Call","they will be of help","kindly change","please follow that link","otto denis rugumayo","thanks",
                                                  "fathers name","patel tusharkumar","please call","copy and paste","just type","hello","try","want to know"
                                                  ,"pkyomugisha@nssfug.org","has closed the chat!","https://www.nssfug.org/Home/est","help",
                                                  "snalwoga (snalwoga@nssfug.org) has closed the chat!","(pkyomugisha@nssfug.org)","let",
                                                  "will","assist","view","yes","please","snalwoga@nssfug.org","kisakye damali","email"))
      inspect(nssfdata.ng)
      nssfdata.ng = tm_map(nssfdata.ng,removePunctuation)
      nssfdata.ng = tm_map(nssfdata.ng,removeNumbers)
      class(nssfdata.ng)
      
      
      #use Weka's n-gram tokenizer to create a TDM 
      TrigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 4))
      nssfdata_trigram = TermDocumentMatrix(nssfdata.ng,
                                            control = list(tokenize = TrigramTokenizer))
      
      freq = sort(rowSums(as.matrix(nssfdata_trigram)),decreasing = TRUE)
      freq.df = data.frame(word=names(freq), freq=freq)
      head(freq.df, 20)
      
    })
    
    output$words2 <- renderPlot({
      
      nssfdata<-myData()$Chat.content
      nssfdata<-tolower(nssfdata)
      nssfdata<-paste(nssfdata, collapse = "")
      nssfdata<-VCorpus(VectorSource(nssfdata))
      inspect(nssfdata)
      
      
      nssfdata.ng = tm_map(nssfdata,removeWords,c(stopwords("english"),"welcome to nssf live chat, hold on while we attend to your query",
                                                  "olivia kyomugisha tamale","date of birth", "mother", "name", "father's name","one","prefer","like to know","can","get","send",
                                                  "diana","catherine","profulla kumar sarker","akurut","chat","nssf","system assistant","visitor has closed the chat explicitly",
                                                  "has accepted the chat","poover kyomugisha","customerservice@nssfug.org","samali nalwoga","nearest", "office","good morning","thank",
                                                  "Kindly Call","they will be of help","kindly change","please follow that link","otto denis rugumayo","thanks",
                                                  "fathers name","patel tusharkumar","please call","copy and paste","just type","hello","try","want to know"
                                                  ,"pkyomugisha@nssfug.org","has closed the chat!","https://www.nssfug.org/Home/est","help",
                                                  "snalwoga (snalwoga@nssfug.org) has closed the chat!","(pkyomugisha@nssfug.org)","let",
                                                  "will","assist","view","yes","please","snalwoga@nssfug.org","kisakye damali","email"))
      inspect(nssfdata.ng)
      nssfdata.ng = tm_map(nssfdata.ng,removePunctuation)
      nssfdata.ng = tm_map(nssfdata.ng,removeNumbers)
      class(nssfdata.ng)
      
      
      #use Weka's n-gram tokenizer to create a TDM 
      TrigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 4))
      nssfdata_trigram = TermDocumentMatrix(nssfdata.ng,
                                            control = list(tokenize = TrigramTokenizer))
      
      freq = sort(rowSums(as.matrix(nssfdata_trigram)),decreasing = TRUE)
      freq.df = data.frame(word=names(freq), freq=freq)
      head(freq.df, 20)
      
      pal=brewer.pal(8,"Blues")
      pal=pal[-(1:3)]
      
      wordcloud(freq.df$word,freq.df$freq,max.words=100,random.order = F, colors=pal)
    })
    
    output$words3 <- renderPlot({
      
      nssfdata<-myData()$Chat.content
      nssfdata<-tolower(nssfdata)
      nssfdata<-paste(nssfdata, collapse = "")
      nssfdata<-VCorpus(VectorSource(nssfdata))
      inspect(nssfdata)
      
      nssfdata.ng = tm_map(nssfdata,removeWords,c(stopwords("english"),"welcome to nssf live chat, hold on while we attend to your query",
                                                  "olivia kyomugisha tamale","date of birth", "mother", "name", "father's name","one","prefer","like to know","can","get","send",
                                                  "diana","catherine","profulla kumar sarker","akurut","chat","nssf","system assistant","visitor has closed the chat explicitly",
                                                  "has accepted the chat","poover kyomugisha","customerservice@nssfug.org","samali nalwoga","nearest", "office","good morning","thank",
                                                  "Kindly Call","they will be of help","kindly change","please follow that link","otto denis rugumayo","thanks",
                                                  "fathers name","patel tusharkumar","please call","copy and paste","just type","hello","try","want to know"
                                                  ,"pkyomugisha@nssfug.org","has closed the chat!","https://www.nssfug.org/Home/est","help",
                                                  "snalwoga (snalwoga@nssfug.org) has closed the chat!","(pkyomugisha@nssfug.org)","let",
                                                  "will","assist","view","yes","please","snalwoga@nssfug.org","kisakye damali","email"))
      inspect(nssfdata.ng)
      nssfdata.ng = tm_map(nssfdata.ng,removePunctuation)
      nssfdata.ng = tm_map(nssfdata.ng,removeNumbers)
      class(nssfdata.ng)
      
      
      #use Weka's n-gram tokenizer to create a TDM 
      TrigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 4))
      nssfdata_trigram = TermDocumentMatrix(nssfdata.ng,
                                            control = list(tokenize = TrigramTokenizer))
      
      freq = sort(rowSums(as.matrix(nssfdata_trigram)),decreasing = TRUE)
      freq.df = data.frame(word=names(freq), freq=freq)
      head(freq.df, 20)
      
      ggplot(head(freq.df,15), aes(reorder(word,freq), freq)) +   
        geom_bar(stat="identity") + coord_flip() + 
        xlab("Questions") + ylab("Frequency") +
        ggtitle("Most common questions asked")
      
    })
    
    output$words4 <- renderPlot({
      
      nssfchat<-as.character(myData()$Chat.content)
      
      #the get_nrc_sentiment returns a data frame in which each 
      #row represents a sentence from the original file 
      nssfchat<-get_nrc_sentiment(nssfchat)
      nssfchat<-data.frame(t(nssfchat))
      
      #Rowsums function performs sums of columns across each character grouping
      nssfchatNew<-data.frame(rowSums(nssfchat[1:1294]))
      #Transforomation and cleaning
      names(nssfchatNew)[1]<-"count"
      nssfchatNew<-cbind("Sentiment" = rownames(nssfchatNew),nssfchatNew)
      rownames(nssfchatNew)<-NULL
      nssfchatclean<-nssfchatNew[1:10,]
      
      qplot(Sentiment, data = nssfchatclean, weight=count, geom = "bar", fill=Sentiment) +ggtitle("NSSF Chat Sentiments")
      
    })
    
    
    output$words5 <- renderPlot({
      
      nssfchat<-as.character(myData()$Chat.content)
      
      #the get_nrc_sentiment returns a data frame in which each 
      #row represents a sentence from the original file 
      nssfchat<-get_nrc_sentiment(nssfchat)
      nssfchat<-data.frame(t(nssfchat))
      
      #Rowsums function performs sums of columns across each character grouping
      nssfchatNew<-data.frame(rowSums(nssfchat[1:1294]))
      #Transforomation and cleaning
      names(nssfchatNew)[1]<-"count"
      nssfchatNew<-cbind("Sentiment" = rownames(nssfchatNew),nssfchatNew)
      rownames(nssfchatNew)<-NULL
      nssfchatclean<-nssfchatNew[1:10,]
      
      
      #visualization
      
      plot(
        nssfchatclean, 
        type="l", 
        main="Plot Trajectory of Emotions", 
        xlab = "Emotions", 
        ylab= "Emotional Valence"
      )
      
    })
    
    ############ MOST ACTIVE #######################################
    
    output$mostactive <- renderTable({
    
      tx5 <-table(myData()$Visitor.Name)
      tx5[order(-tx5)]
      
    })
    
    
    ############## DOWNLOAD #########################
    
    
    output$down1 <- downloadHandler(
    
      #Specify the file name
      
      filename = function(){
        #countries.png
        #countries.pdf
        paste("plot", '.png', sep="")
      },
      
      content = function(file){
          png(file)
        d <- myData()
        country.freq = table(d$Country)
        par(mar=c(8,15,5,8))
        barplot(country.freq, col="blue", xlab="Waiting time", ylab="Country",horiz = TRUE, las = 1)
          dev.off()
      },
      
      contentType = 'image/png'
    )
    
    output$down2 <- downloadHandler(
      
      #Specify the file name
      
      filename = function(){
        #countries.png
        #countries.pdf
        paste("plot", '.png', sep="")
      },
      
      content = function(file){
        png(file)
        d <- myData()
        nssfdata<-myData()$Chat.content
        nssfdata<-tolower(nssfdata)
        nssfdata<-paste(nssfdata, collapse = "")
        nssfdata<-VCorpus(VectorSource(nssfdata))
        inspect(nssfdata)
        
        nssfdata.ng = tm_map(nssfdata,removeWords,c(stopwords("english"),"welcome to nssf live chat, hold on while we attend to your query",
                                                    "olivia kyomugisha tamale","date of birth", "mother", "name", "father's name","one","prefer","like to know","can","get","send",
                                                    "diana","catherine","profulla kumar sarker","akurut","chat","nssf","system assistant","visitor has closed the chat explicitly",
                                                    "has accepted the chat","poover kyomugisha","customerservice@nssfug.org","samali nalwoga","nearest", "office","good morning","thank",
                                                    "Kindly Call","they will be of help","kindly change","please follow that link","otto denis rugumayo","thanks",
                                                    "fathers name","patel tusharkumar","please call","copy and paste","just type","hello","try","want to know"
                                                    ,"pkyomugisha@nssfug.org","has closed the chat!","https://www.nssfug.org/Home/est","help",
                                                    "snalwoga (snalwoga@nssfug.org) has closed the chat!","(pkyomugisha@nssfug.org)","let",
                                                    "will","assist","view","yes","please","snalwoga@nssfug.org","kisakye damali","email"))
        inspect(nssfdata.ng)
        nssfdata.ng = tm_map(nssfdata.ng,removePunctuation)
        nssfdata.ng = tm_map(nssfdata.ng,removeNumbers)
        class(nssfdata.ng)
        
        
        #use Weka's n-gram tokenizer to create a TDM 
        TrigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 4))
        nssfdata_trigram = TermDocumentMatrix(nssfdata.ng,
                                              control = list(tokenize = TrigramTokenizer))
        
        freq = sort(rowSums(as.matrix(nssfdata_trigram)),decreasing = TRUE)
        freq.df = data.frame(word=names(freq), freq=freq)
        head(freq.df, 20)
        
        pal=brewer.pal(8,"Blues")
        pal=pal[-(1:3)]
        
        wordcloud(freq.df$word,freq.df$freq,max.words=100,random.order = F, colors=pal)
        dev.off()
      },
      
      contentType = 'image/png'
    )
    
    output$down3 <- downloadHandler(
      
      #Specify the file name
      
      filename = function(){
        #countries.png
        #countries.pdf
        paste("plot", '.png', sep="")
      },
      
      content = function(file){
        png(file)
        dev.off()
      },
      
      contentType = 'image/png'
    )
    
    output$down4 <- downloadHandler(
      
      #Specify the file name
      
      filename = function(){
        #countries.png
        #countries.pdf
        paste("plot", '.png', sep="")
      },
      
      content = function(file){
        png(file)
        dev.off()
      },
      
      contentType = 'image/png'
    )
    
    output$down5 <- downloadHandler(
      
      #Specify the file name
      
      filename = function(){
        #countries.png
        #countries.pdf
        paste("plot", '.png', sep="")
      },
      
      content = function(file){
        png(file)
        d <- myData()
        nssfchat<-as.character(myData()$Chat.content)
        
        #the get_nrc_sentiment returns a data frame in which each 
        #row represents a sentence from the original file 
        nssfchat<-get_nrc_sentiment(nssfchat)
        nssfchat<-data.frame(t(nssfchat))
        
        #Rowsums function performs sums of columns across each character grouping
        nssfchatNew<-data.frame(rowSums(nssfchat[1:1294]))
        #Transforomation and cleaning
        names(nssfchatNew)[1]<-"count"
        nssfchatNew<-cbind("Sentiment" = rownames(nssfchatNew),nssfchatNew)
        rownames(nssfchatNew)<-NULL
        nssfchatclean<-nssfchatNew[1:8,]
        
        
        #visualization
        
        plot(
          nssfchatclean, 
          type="l", 
          main="Plot Trajectory of Emotions", 
          xlab = "Emotions", 
          ylab= "Emotional Valence"
        )
        dev.off()
      },
      
      contentType = 'image/png'
    )
    
    output$down6 <- downloadHandler(
      
      #Specify the file name
      
      filename = function(){
        #countries.png
        #countries.pdf
        paste("plot", '.png', sep="")
      },
      
      content = function(file){
        png(file)
        d <- myData()
        colors = c("red", "yellow", "green","blue")
        operator.freq = table(d$Operator)
        
        par(mar=c(8,20,5,5))
        barplot(operator.freq, col=colors, xlab="Frequency", ylab="Operators", horiz= TRUE,las=1)
        dev.off()
      },
      
      contentType = 'image/png'
    )
    
    output$down7 <- downloadHandler(
      
      #Specify the file name
      
      filename = function(){
        #countries.png
        #countries.pdf
        paste("plot", '.png', sep="")
      },
      
      content = function(file){
        png(file)
        d <- myData()
        d <- myData()
        colors = c("red", "yellow", "green")
        vote.freq = table(d$Vote.status)
        barplot(vote.freq, col=colors,  xlab="Vote status", ylab="Number of customers")
        dev.off()
      },
      
      contentType = 'image/png'
    )
    
    output$down8 <- downloadHandler(
      
      #Specify the file name
      
      filename = function(){
        #countries.png
        #countries.pdf
        paste("plot", '.pdf', sep="")
      },
      
      content = function(file){
        pdf(file)
        d <- myData()
        d$Minutes <- as.POSIXct(d$Minutes, format="%H:%M:%S")
        qplot(d$Minutes, data = d, geom = "histogram", binwidth = 60, xlab = "Time")
        dev.off()
      }

    )
    
    output$down10 <- downloadHandler(
      
      #Specify the file name
      
      filename = function(){
        #countries.png
        #countries.pdf
        paste("plot", '.png', sep="")
      },
      
      content = function(file){
        png(file)
        d <- myData()
        pie(table(myData()$Date),main = "Appearance of dates", col=rainbow(length(table(myData()$Date))), width=10, height=6)
        dev.off()
      },
      
      contentType = 'image/png'
      
    )
    
    output$down11 <- downloadHandler(
      
      #Specify the file name
      
      filename = function(){
        #countries.png
        #countries.pdf
        paste("plot", '.png', sep="")
      },
      
      content = function(file){
        png(file)
        piepercent<- paste0(round(100*table(myData()$Mail.send)/sum(table(myData()$Mail.send)), 1),"%")
        d <- myData()
        
        pie3D(table(myData()$Mail.send),labels=piepercent ,main = "Pie chart showing Mail send status", col=rainbow(length(table(myData()$Mail.send))), explode = 0.1)
        legend("topright", c("Yes","No"), cex=0.8,
               fill=rainbow(length(table(myData()$Mail.send))))
        dev.off()
      },
      
      contentType = 'image/png'
      
    )
    
    output$down12 <- downloadHandler(
      
      #Specify the file name
      
      filename = function(){
        #countries.png
        #countries.pdf
        paste("plot", '.png', sep="")
      },
      
      content = function(file){
        png(file)
        piepercent<- paste0(round(100*table(myData()$Vote.status)/sum(table(myData()$Vote.status)), 1),"%")
        
        pie3D(table(myData()$Vote.status),labels=piepercent,main = "Pie chart showing vote status", col=rainbow(length(table(myData()$Vote.status))), explode=0.1)
        legend("topright", c("DOWN","NONE","UP"), cex=0.8,
               fill=rainbow(length(table(myData()$Vote.status))))
        dev.off()
      },
      
      contentType = 'image/png'
      
    )
    
    output$down13 <- downloadHandler(
      
      #Specify the file name
      
      filename = function(){
        #countries.png
        #countries.pdf
        paste("plot", '.png', sep="")
      },
      
      content = function(file){
        png(file)
        piepercent<- paste0(round(100*table(myData()$Operator)/sum(table(myData()$Operator)), 1),"%")
        
        pie3D(table(myData()$Operator),labels=piepercent,main = "Pie chart showing Operator operations",explode=0.1)
        legend("topright", c("Diana(customerservice@nssfug.org)","fachieng(customerservice@nssfug.org)","pkyomugisha(pkyomugisha@nssfug.org)","snalwoga(snalwoga@nssfug.org)"), cex=0.8,
               fill=rainbow(length(table(myData()$Operator))))
        dev.off()
      },
      
      
      contentType = 'image/png'
      
    )
    
    output$down14 <- downloadHandler(
      
      #Specify the file name
      
      filename = function(){
        #countries.png
        #countries.pdf
        paste("plot", '.png', sep="")
      },
      
      content = function(file){
        png(file)
        d <- myData()
        
        Date.freq = table(as.Date(d$Date,"%m/%d/%Y"))
        barplot(Date.freq, col="pink", xlab="Days", ylab="Number of chats", main="Barchart showing number of chats against days")
        dev.off()
      },
      
      
      contentType = 'image/png'
      
    )
    
    output$down15 <- downloadHandler(
      
      #Specify the file name
      
      filename = function(){
        #countries.png
        #countries.pdf
        paste("plot", '.png', sep="")
      },
      
      content = function(file){
        png(file)
        
        d <- myData()
        
        plot(table(as.Date(d$Date,"%m/%d/%Y")),type="l",
             xlab="Date",ylab="Count", col="red")
        dev.off()
      },
      
      
      contentType = 'image/png'
      
    )
    
    output$down16 <- downloadHandler(
      
      #Specify the file name
      
      filename = function(){
        #countries.png
        #countries.pdf
        paste("plot", '.png', sep="")
      },
      
      content = function(file){
        png(file)
        d <- myData()
        d$Minutes <- as.POSIXct(d$Minutes, format="%H:%M:%S")
        
        plot(d$Minutes ~ d$ID,type="l",
             xlab="ID",ylab="TIME")
        dev.off()
      },
      
      
      contentType = 'image/png'
      
    )

    
    
    ############# TIME SERIES AND FORECASTING  #############
    
    output$forecast1<-renderPlot({
      
      table(myData()$Date)
      v<-data.frame(table(myData()$Date))
      colnames(v)<-c("dates","charts")
      y = ts(v$charts, start=c(2016, yday("2016-05-02")),end =c(2016, yday("2016-06-01")),  frequency=365)
      plot(forecast(ets(y), 7), xaxt="n")
      a = seq(as.Date("2016-05-02"), by="days", length=53)
      axis(1, at = decimal_date(a), labels = format(a), cex.axis=0.8)
      
      
    })
    output$forecast2<-renderTable({
      table(myData()$Date)
      v<-data.frame(table(myData()$Date))
      colnames(v)<-c("dates","charts")
      y <- ts(v$charts, start=c(2016, yday("2016-05-02")),end =c(2016, yday("2016-06-01")),  frequency=365)
      l<-forecast(ets(y), h=8)
      l
      
    })

})
  