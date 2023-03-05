library(shiny)
library(shinycssloaders)
library(tidyverse)
library(tidytext)
library(syuzhet)
library(stats)

ui <- fluidPage(
  fluidRow(
    column(
        width = 4,
          textAreaInput(
            inputId = "user_poem",
            label = "Enter text here",
            width = "100%", 
            height="600px"
            ),
           submitButton("Calculate"),
          ),
    column(
      width = 8,
      withSpinner(tableOutput(outputId = "table"))
    ),
    column(
      width = 8,
      withSpinner(plotOutput(outputId = "plot"))
    ),
  )
)

server <- function(input, output) {

### OUTPUTS ###

  output$table <- renderTable({
    
    req(input$user_poem)
    
    # IMPORT AND CLEAN
    poem_text <- input$user_poem
    poem_lines <- strsplit(poem_text, "\n")[[1]]
    poem_lines <- poem_lines[nchar(trimws(poem_lines)) > 0]
    poem_tbl <- tibble(lineno = 1:length(poem_lines), text = poem_lines)
    
    # SENTIMENT ANALYSIS
    sentiment_words <- poem_tbl %>%
      unnest_tokens(word, text) %>%
      mutate(sentiment = get_sentiment(word))
    
    pos_miensum <- sum(sentiment_words$sentiment[sentiment_words$sentiment > 0])
    neg_miensum <- sum(sentiment_words$sentiment[sentiment_words$sentiment < 0])
    if (pos_miensum > abs(neg_miensum)) {
      Mien <- mean(sentiment_words$sentiment[sentiment_words$sentiment > 0])
    } else {
      Mien <- mean(sentiment_words$sentiment[sentiment_words$sentiment < 0])
    }
    
    Fickleness = max(sentiment_words$sentiment) - min(sentiment_words$sentiment)
    
    Moxie = max(abs(sentiment_words$sentiment))
    
    num_letters = function(word) {nchar(word)}
    Sesquipedalianism = mean(sapply(sentiment_words$word, num_letters))
    
    data.frame(Humour = c("Mien", "Fickleness", "Moxie", "Sesquipedalianism"), 
               Score = c(Mien, Fickleness, Moxie, Sesquipedalianism))
  })
  
  output$plot <- renderPlot({
    
    req(input$user_poem)
    
    # IMPORT AND CLEAN
    poem_text <- input$user_poem
    poem_lines <- strsplit(poem_text, "\n")[[1]]
    poem_lines <- poem_lines[nchar(trimws(poem_lines)) > 0]
    poem_tbl <- tibble(lineno = 1:length(poem_lines), text = poem_lines)
    
    # SENTIMENT ANALYSIS
    sentiment_scores <- poem_tbl %>%
      unnest_tokens(word, text) %>%
      group_by(lineno) %>%
      summarize(total_sentiment = sum(get_sentiment(word)))
    
    # VISUALIZE
    time = as.character(1:nrow(sentiment_scores))
    
    graph = ggplot(sentiment_scores, aes(x=lineno, y=total_sentiment)) +
      geom_bar(stat="identity") + 
      geom_smooth(method="auto") +
      labs(x = "Passage of Time", y = "Syuzhetic Polarity") +
      scale_x_discrete(limits = unique(sentiment_scores$lineno), labels = time)
    
    # DISPLAY
    plot(graph)
  })
}

shinyApp(ui, server)