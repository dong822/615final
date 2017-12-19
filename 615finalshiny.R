#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

pacman::p_load("leaflet")
library(shiny)
library(tidyverse)
library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)
library(tm)
library(memoise)
alldata <- read.csv("locations.csv", stringsAsFactors = FALSE)
alldata1 <- read.csv("locations1.csv", stringsAsFactors = FALSE)
df<-read.csv("df1.csv", stringsAsFactors = FALSE)
df1<-read.csv("df2.csv", stringsAsFactors = FALSE)
# Define UI for application that draws a histogram
cities<-list("la","ny")
getTermMatrix<-memoise(function(city){
  if (!(city %in% cities))
    stop("Unknown city")
df$text<-str_replace_all(df$text, "�", "")
myCorpus <- Corpus(VectorSource(str_replace_all(tweetsdf1$text, "@", "")))
myCorpus <- tm_map(myCorpus, removePunctuation)
myCorpus <- tm_map(myCorpus, content_transformer(tolower))
myCorpus <- tm_map(myCorpus, removeWords, stopwords("english"))
myCorpus <- tm_map(myCorpus, removeWords, c("starbucks","make"))
myCorpus <- tm_map(myCorpus , removeNumbers)
myCorpus <- tm_map(myCorpus , removeWords,c("just","get"))
myCorpus <- tm_map(myCorpus, stripWhitespace)
myCorpus<-tm_map(myCorpus, stemDocument)
word.counts<-as.matrix(TermDocumentMatrix(myCorpus))
word.freq<-sort(rowSums(word.counts), decreasing=TRUE)
df1$text<-str_replace_all(df$text, "�", "")
myCorpus1 <- Corpus(VectorSource(str_replace_all(tweetsdf1$text, "@", "")))
myCorpus1 <- tm_map(myCorpus1, removePunctuation)
myCorpus1 <- tm_map(myCorpus1, content_transformer(tolower))
myCorpus1 <- tm_map(myCorpus1, removeWords, stopwords("english"))
myCorpus1 <- tm_map(myCorpus1, removeWords, c("starbucks","make"))
myCorpus1 <- tm_map(myCorpus1 , removeNumbers)
myCorpus1 <- tm_map(myCorpus1 , removeWords,c("just","get"))
myCorpus1 <- tm_map(myCorpus1, stripWhitespace)
myCorpus1<-tm_map(myCorpus1, stemDocument)
word.counts<-as.matrix(TermDocumentMatrix(myCorpus))
word.freq<-sort(rowSums(word.counts), decreasing=TRUE)

})
ui <- shinyUI(fluidPage(
  titlePanel("Twitter Project on Starbucks"),
  navbarPage(title = "Content",
             
             tabPanel("Brand",
                      h1("starbucks"),
                      hr(),
                                  img(src = "2.png"),
                      h5("just follow your heart")
             ),# end of tab
             
            
             tabPanel("Starbucks in Los Angeles",
                      mainPanel(plotOutput("plotiris"))
             ),
             tabPanel("Starbucks in New York",
                      mainPanel(plotOutput("plotana"))
             ),
             tabPanel("Word Cloud for New York",
                      mainPanel(plotOutput("plo"))
             ),
             tabPanel("Word Cloud for LA",

                       plotOutput("plot")
                      )
             
  )# end of navbar
)# end fluid page
)# end shiny UI
# Define server logic required to draw a histogram
server <- function(input, output,session) {
  output$plotiris <- renderPlot(
    ggmap(get_map(location = "los angeles",  zoom = 8,
                  source = "google", maptype = "roadmap")) + 
      geom_point(aes(x=lon,
                     y = lat), data = alldata, alpha = 0.5, size = 2,
                 color = "red")
  )    
    output$plotana <- renderPlot(
      ggmap(get_map(location = "new york",  zoom = 8,
                    source = "google", maptype = "roadmap")) + 
        geom_point(aes(x=lon,
                       y = lat), data = alldata1, alpha = 0.5, size = 2,
                   color = "red")
  )

 
    output$plot<-renderPlot({  
      set.seed(123)
      wordcloud(words = myCorpus, scale=c(2,0.2),max.words=500, random.order=FALSE, 
                rot.per=0.35, use.r.layout=FALSE, colors=brewer.pal(8,"red"))
    })
    output$plo<-renderPlot({  
      set.seed(123)
      wordcloud(words = myCorpus1, scale=c(2,0.2),max.words=500, random.order=FALSE, 
                rot.per=0.35, use.r.layout=FALSE, colors=brewer.pal(8,"Dark2"))
    })

    
    # Make the wordcloud drawing predictable during a session

    

  }
  

  
shinyApp(ui = ui, server = server)

