library(shiny)
library(httr)
library(stringr)

# Function to fetch README.md from GitHub
fetchReadme <- function() {
  url <- "https://raw.githubusercontent.com/gigikenneth/ai-ethics-resources/main/README.md"
  res <- GET(url)
  content <- content(res, "text")
  return(content)
}

# Function to parse markdown content
parseMarkdown <- function(mdContent) {
  sections <- unlist(str_split(mdContent, "\n## "))
  
  parsedSections <- lapply(sections[-1], function(section) {
    header <- str_extract(section, "^[^\n]+")
    
    # Extract bullet points and convert markdown links to HTML
    bullets <- str_match_all(section, "- (.*)")[[1]][,2]
    bullets <- sapply(bullets, function(bullet) {
      markdownToHTML(bullet)
    }, USE.NAMES = FALSE)
    
    list(header = header, bullets = bullets)
  })
  
  return(parsedSections)
}

# Function to convert markdown links to HTML
markdownToHTML <- function(text) {
  return(gsub("\\[([^]]+)\\]\\(([^)]+)\\)", "<a href='\\2' target='_blank'>\\1</a>", text))
}

ui <- fluidPage(
  titlePanel("AI Ethics Resources"),
  uiOutput("tabsUI")
)

server <- function(input, output, session) {
  markdownContent <- reactiveVal(list())
  
  observe({
    content <- fetchReadme()
    parsedContent <- parseMarkdown(content)
    markdownContent(parsedContent)
    invalidateLater(300000, session)  # Refresh every 5 minutes
  })
  
  output$tabsUI <- renderUI({
    content <- markdownContent()
    if (is.null(content)) return(NULL)
    
    tabs <- lapply(content, function(section) {
      tabPanel(title = section$header,
               HTML(paste("<ul><li>", paste(section$bullets, collapse = "</li><li>"), "</li></ul>")))
    })
    
    do.call(navbarPage, c("AI Ethics Resources", id = "nav", tabs))
  })
}

shinyApp(ui, server)
