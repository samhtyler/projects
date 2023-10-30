library(shiny)
library(ggplot2)

shinyApp(
  ui = fluidPage(
    titlePanel(h1("Balloon Game", align="center")),
    plotOutput("plot"),
    textOutput("tt"),
    tags$head(tags$style("#tt{font-size:20px;
                         font-style: bold;
                         text-align:center;
                         height:100px;
                         }")),
    textOutput("tb"),
    tags$head(tags$style("#tb{font-size:20px;
                         font-style: bold;
                         text-align:center;
                         height:50px;
                         }")),
    fluidRow(
      column(
        width = 6,
        textOutput("text")
      ),
      column(
        width = 6,
        textOutput("t")
      )
    ),
    fluidRow(
      column(
        width = 6,
        verbatimTextOutput("verb")
      ),
      column(
        width = 6,
        verbatimTextOutput("v")
      )
    ),
    fluidRow(
      column(
        width = 6,
        align="right",
        actionButton("go", "Pump"),
        tags$style("#go {text-align:right;
                   font-style:bold;}")
      ),
      column(
        width = 6,
        actionButton("stop","Cash Out")
      )
    ),
  ),
  server = function(input, output) {
    rv <- reactiveValues(loop = 0, sum=0, total=0, bl=0)
    bl_total = 8
    inc = 0.1
    observeEvent(input$go, {
      rv$loop <- 1
    })
    
    observeEvent(input$stop, {
      rv$loop <- -1
    })
    observeEvent(rv$loop,{
      if(rv$loop > 0){
        rv$r = rbinom(1,1,0.04)
        if (rv$r==1){
          rv$text <- "Pop goes the Balloon"
          rv$sum = 0
          rv$bl <-rv$bl + 1
        } else {
          rv$text <- "Keep it going!"
          rv$sum = rv$sum + inc
        }
      } else if (rv$loop < 0){
        rv$text <- "You've cashed out"
        rv$total <-  rv$total + rv$sum
        rv$sum <- 0
        rv$bl <- rv$bl + 1
      } else if (rv$bl >= bl_total){
        rv$text <- paste("That was your last balloon. Your total earnings are: $",rv$total, ". Press Pump to play again")
        rv$total <- 0
        rv$bl <- 0
      }
      rv$loop = 0
    })
    
    output$plot <- renderPlot({
      ggplot() + geom_circle(aes(x0 = 0, y0 = 0, r = rv$sum + 0.05), fill='cyan') + coord_fixed() + 
        scale_x_continuous(limits = c(-5, 5)) + scale_y_continuous(limits = c(-5, 5)) + theme_bw() + 
        theme(panel.border =  element_blank(), panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(),
              axis.title = element_blank())
    })
    output$tt <- renderText({rv$text})
    output$tb <- renderText({paste(rv$bl,"/",bl_total)})
    output$text <- renderText({"Current Balloon $"})
    output$t <- renderText({"Total Winnings $$"})
    output$verb <- renderText({rv$sum})
    output$v <- renderText({rv$total})
    
  }
)



