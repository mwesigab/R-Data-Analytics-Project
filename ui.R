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

#This section begins the user interface
ui<-dashboardPage( skin = "red",
                   dashboardHeader(title ="NSSF CUSTOMER SUPPORT DATA ANALYSIS SYSTEM",titleWidth="600"),
                   dashboardSidebar(
                     
                     tags$img(src='mygif.gif', width=230),
                     
                     sidebarMenu(
                      
                       menuItem("RawData Upload",tabName ="upload",icon =icon("upload"),
                                menuSubItem(icon = icon("none"),
                                            
                      fileInput(inputId ="file","choose File",
                                accept =c('text/csv','text/comma-separated-values,text/plain','.csv')))),
                       menuItem("Data",tabName ="data",icon =icon("table")),
                      menuItem("Customer activity",tabName ="customer",icon =icon("table")),
                       menuItem("charts",tabName="", icon=icon("bar-chart-o"),
                                menuSubItem( "Pie-Charts" ,
                                             tabName = "pie" , icon = icon("pie-chart")) ,
                                menuSubItem( "Line-Charts" ,
                                             tabName = "line" , icon = icon("line-chart")),
                                menuSubItem( "Bar-Charts" ,
                                             tabName = "bar" , icon = icon("bar-chart")),
                                menuSubItem( "Scatter-Charts" ,
                                             tabName = "scatter" , icon = icon("line-chart")),
                                menuSubItem("Customer chat mining",
                                            tabName = "words", icon = icon("bar-chart")),
                                menuSubItem("Predictions",
                                            tabName = "forecast", icon = icon("line-chart"))
                                
                       )
                     )
                   ),
                   
                   
                   dashboardBody(
                     
                     tabItems(
                       tabItem(tabName = "data",
                               tabsetPanel(
                                 type="tab",
                                 tabsetPanel(id="tabs",
                                    tabPanel(value="panel1", "Raw-Data",icon = icon("table"), dataTableOutput("mytable1"),dataTableOutput("mytable2"))
                                 
                               ))),
                       
                       tabItem(tabName = "customer",
                               tabsetPanel(
                                 type="tab",
                                 tabsetPanel(id="tabs",
                                             tabPanel(value="panel1", "Most Active Customer",icon = icon("table"),
                                                      box(width=12,tags$b("The most active customer ranking"),
                                                        p("The table shows the ranks of active customer from most active to least based on number of times their names and email appear")),
                                                      box(tableOutput("mostactive"))
                                                      )
                                             
                                 ))),
                       
                       tabItem(tabName = "pie", h2("Pie Chart"),
                               tabsetPanel(
                                 type="tab",
                      
                                 tabPanel("date Visualization",
                                    box(width=12,tags$b("Piechart representing number of chats of different days"),
                                    p("As a result we can easily determine the number of customers on specific days by looking at the frequency table"),
                                    p("The different sectors of the pie chart show the number of charts as a result of the frequency table."),
                                    p("Var1 in the table is the column with dates and the freq colum respresents number of chats on a specific day"),
                                    infoBox(textOutput("pie2number"), "Number Of Days", icon = icon("credit-card"))),
                                    box(tableOutput("pietable2")),
                                    box(plotOutput("pie2", width="500px", height = "700px"),
                                        downloadButton(outputId='down10', label='Download the Plot'))
                                    ),
                                 
                                 tabPanel("Mail send Visualization",
                                          box(width=12,tags$b("Pie chart illustrating mail send status"),
                                          p("The pie-chart demonstrates those that sent by mail and those that did not")),
                                          box(plotOutput("pie3", width="500px", height = "450px"), downloadButton(outputId='down11', label='Download the Plot')),
                                          box(tableOutput("pietable3")
                                             )
                                         
                               ),
                                 
                                 tabPanel("Vote status Visualization",
                                          box(width=12, tags$b("Pie chart illustrating vote status"),
                                          p("The pie-chart demonstrates vote status")),
                                          box(plotOutput("pie4", width="500px", height = "450px"), downloadButton(outputId='down12', label='Download the Plot')),
                                          box(tableOutput("pietable4")
                                             )
                                          
                               ),
                               
                               tabPanel("Operator",
                                        box(width=12, tags$b("Pie chart illustrating Operator operations"),
                                            p("The pie-chart demonstrates different operators and how many operations they perform i.e their services to the customers")),
                                        box(plotOutput("pie5", width="500px", height = "600px"),downloadButton(outputId='down13', label='Download the Plot')),
                                        box(tableOutput("pietable5"))
                               )
                               )),
                       
                       
                       tabItem(tabName = "line", h2("Line Chart"),
                               tabsetPanel(
                                 type="tab",
                                 
                                 tabPanel("Countries", 
                                          box(width=12,tags$b("Line graph showing Countries and their average waiting times"),
                                              p("Different countries represented are plotted against waiting times as shown below.")),
                                          box(width=12,plotOutput("line1"))
                                         
                                 ),
                                 
                                 tabPanel("Days",
                                   box(width=12,tags$b("Number of chats against days"),
                                       p("The line graph illustrates the count of chats and activity on different days")),
                                   box(width=2,tableOutput("line2table")),
                                   box(width=10,plotOutput("line2"), downloadButton(outputId='down15', label='Download the Plot'))
                                   
                                   
                                 ),
                                 
                                 tabPanel("Time",
                                        box(width=12, tags$b("Line graph showing Time against ID"),
                                            p("The line graph illustrates the times most common where the customers access the system")),
                                        box(width=12,plotOutput("line3"), downloadButton(outputId='down16', label='Download the Plot'))
                                  
                                          )
                                 )),

                       
                       tabItem(tabName = "bar", 
                               tabsetPanel(
                                 type="tab",
                                 
                                 tabPanel("Days",
                                          box(width=12,tags$b("The bar chart of number of activities or chats against days"),
                                              p("The bar chart below can be used to know the day when there was most activity.")),
                                          box(width=2,tableOutput("bar1table")),
                                          box(width=10,plotOutput("bar1", width="1050px", height = "500px"),
                                              downloadButton(outputId='down14', label='Download the Plot'))
                                 ),
                               
                               tabPanel("Time",
                                        box(width=12, tags$b("The bar chart for time count"),
                                        p("The bar graph shows the time of the day when there were many clients accessing the site")),
                                        box(width=12,plotOutput("bar2", width="1050px", height = "500px"),
                                            downloadButton(outputId='down8', label='Download the Plot'))
                               ),
                               
                               tabPanel("Countries",
                                        box(width=12, tags$b("Bar Chart Represnting Countries present"),
                                        p("The table shows the different countries and the number of customers that are from there as shown by the frequency column. Var1 Represents the column showing the Country"),
                                        infoBox(textOutput("bar3number"), "Number Of Countries", icon = icon("credit-card"))),
                                        box(width=3,tableOutput("bar3table")),
                                        box(width=8,plotOutput("bar3", width="600px", height = "500px"),
                                            downloadButton(outputId='down1', label='Download the Plot'))
                                        
                                        
                               ),
                               
                               tabPanel("Vote status",
                                        box(width=12, tags$b("Bar Chart showing vote status count"),
                                            p("The table shows the vote status of the different customers. It then illustrates the most common and least coomon vote status"),
                                            infoBox(textOutput("bar4number"), "Number ofvote status categories", icon = icon("credit-card"))),
                                        box(tableOutput("bar4table")),
                                        box(width=12,plotOutput("bar4", width="1050px", height = "500px"),
                                        downloadButton(outputId='down7', label='Download the Plot'))
                               ),
                               
                               tabPanel("Operators",
                                        box(width=12, tags$b("Bar Chart showing operators"),
                                            p("The table shows the number of customers that are handled by each of the operators. This is further illustrated in the bar graph"),
                                            infoBox(textOutput("bar5number"), "Number of operators", icon = icon("credit-card"))),
                                        box(width=4,tableOutput("bar5table")),
                                        box(width=10,plotOutput("bar5", width="800px", height = "500px"),
                                        downloadButton(outputId='down6', label='Download the Plot'))
                               ),
                               
                               tabPanel("Pages",
                                        box(width=12, tags$b("Bar Chart Represnting count of various pages"),
                                        p("The table shows the different pages visited by the customers and the number of customers that visited a page as shown by the frequency column. Var1 Represents the column showing the web page"),
                                        infoBox(textOutput("bar6number"), "Number Of Pages visited", icon = icon("credit-card"))),
                                        box(width = 12, tableOutput("table6")),
                                        box(width=12,plotOutput("bar6", width="1100px", height = "1000px"),
                                        downloadButton(outputId='down9', label='Download the Plot'))
                                        
                               )
                                 
                               )),
                       
                       tabItem(tabName = "scatter", 
                               h2("Scatter Plots"),
                               box(width=12,tags$b("ID AGAINST TIME"),
                               p("The scatter plot shows the times customers are active.")),
                               plotOutput("ID_and_time")
                               
                       ),
                       
                       tabItem(tabName = "words", 
                               h2("Chat Analysis"),
                               tabsetPanel(
                                 type="tab",
                                 
                                 tabPanel("Bi-gram cloud", 
                                          box(width=12,tags$b("The n-gram cloud"),
                                          p("We show bi-grams on an n-gram cloud which illustrate the most commonly asked questions"),
                                          p("We have a tabular and graphical illustration.")),
                                          box(tableOutput("words1")),
                                          box(plotOutput("words2", width="500px", height = "500px"),
                                              downloadButton(outputId='down2', label='Download the Plot'))
                                 ),
                                 
                                 tabPanel("Common Questions",
                                          box(width=12,tags$b("histogram that shows the most commonly asked questions"),
                                           p("It displays the most common words which we use to know the most commonly asked questions")),
                                          box(width=12,plotOutput("words3", width="1000px", height = "450px"),
                                              downloadButton(outputId='down3', label='Download the Plot'))
                                          ),
                                 
                                 tabPanel("Sentiment Analysis",
                                          box(width=12,tags$b("The sentiment analysis"),
                                          p("Sentiment analysis (sometimes known as opinion mining or emotion AI) refers to the use of natural language processing, text analysis, computational linguistics, and biometrics to systematically identify, extract, quantify, and study affective states and subjective information."),
                                          p("Generally speaking, sentiment analysis aims to determine the attitude of a speaker, writer, or other subject with respect to some topic or the overall contextual polarity or emotional reaction to a document, interaction, or event. The attitude may be a judgment or evaluation (see appraisal theory), affective state (that is to say, the emotional state of the author or speaker), or the intended emotional communication (that is to say, the emotional effect intended by the author or interlocutor).")),
                                          box(width=12,plotOutput("words4", width="1000px", height = "450px"),
                                              downloadButton(outputId='down4', label='Download the Plot')),
                                          box(width=12,p("The accuracy of a sentiment analysis system is, in principle, how well it agrees with human judgments. This is usually measured by variant measures based on precision and recall over the two target categories of negative and positive texts"))
                                 ),
                                 
                                 tabPanel("Emotions",
                                          box(width=12, tags$b("Plot trajectory of different emotions"),
                                              p("The Emotional analysis")),
                                          box(width=12,plotOutput("words5", width="1000px", height = "450px"),
                                              downloadButton(outputId='down5', label='Download the Plot'))
                                 )
                               )
                               ),
                       
                       tabItem(tabName = "forecast", 
                               h2("Customer predictions"),
                               tabsetPanel(
                                 type="tab",
                                 
                                 tabPanel("forecast graph",
                                          p("A line graph showing the forecasts."),
                                          p("forecasts for 2016-06-01 tot 2016-06-09 are plotted as a blue line, 
                                            the 80 % prediction interval as an grew shaded area, and the 95% 
                                            prediction interval as a light-grew shaded area"),
                                          plotOutput("forecast1", width="1000px", height = "450px")),
                                 
                                 
                                 tabPanel("forecast values",
                                          p("forecasted values for each date."),  
                                          box(tableOutput("forecast2")))
                                 
                                 
                                 )
                                 )
                       
                       
                       )
                     
                   )
      )
    


