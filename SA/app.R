library(shiny)
library(DT)

# Generate 10x10 height grid with random values between 0 and 20
grid <- matrix(sample(0:20, 100, replace=TRUE), nrow=10, byrow=TRUE)

# UI
ui <- fluidPage(
     titlePanel("Simulated Annealing Dice Roll"),
     sidebarLayout(
          sidebarPanel(
               actionButton("roll_start", "Roll for Start Position"),
               actionButton("roll_move", "Roll to Move"),
               textOutput("position"),
               textOutput("height"),
               textOutput("highest"),
               textOutput("rolls_left")
          ),
          mainPanel(
               DTOutput("grid")
          )
     )
)

# Server
server <- function(input, output, session) {
     pos <- reactiveValues(row=NA, col=NA, highest=NA, rolls=10, new_row=NA, new_col=NA)
     
     observeEvent(input$roll_start, {
          pos$row <- sample(1:10, 1)
          pos$col <- sample(1:10, 1)
          pos$highest <- grid[pos$row, pos$col]
          pos$rolls <- 10
     })
     
     observeEvent(input$roll_move, {
          if (is.na(pos$row) || is.na(pos$col) || pos$rolls <= 0) return()
          
          roll <- sample(1:6, 1)
          pos$new_row <- pos$row
          pos$new_col <- pos$col
          
          if (roll == 1 && pos$row > 1) pos$new_row <- pos$row - 1  # Up
          if (roll == 2 && pos$row < 10) pos$new_row <- pos$row + 1  # Down
          if (roll == 3 && pos$col > 1) pos$new_col <- pos$col - 1  # Left
          if (roll == 4 && pos$col < 10) pos$new_col <- pos$col + 1  # Right
          
          new_height <- grid[pos$new_row, pos$new_col]
          
          if (new_height >= grid[pos$row, pos$col]) {
               pos$row <- pos$new_row
               pos$col <- pos$new_col
          } else {
               roll_accept <- sample(1:6, 1)
               if (roll_accept <= 3) {
                    pos$row <- pos$new_row
                    pos$col <- pos$new_col
               }
          }
          
          pos$highest <- max(pos$highest, grid[pos$row, pos$col])
          pos$rolls <- pos$rolls - 1
     })
     
     output$position <- renderText({
          paste("Current Position: Row", pos$row, "Column", pos$col)
     })
     
     output$height <- renderText({
          if (is.na(pos$row)) "Roll to Start" else paste("Current Height:", grid[pos$row, pos$col])
     })
     
     output$highest <- renderText({
          paste("Highest Height Reached:", pos$highest)
     })
     
     output$rolls_left <- renderText({
          paste("Rolls Left:", pos$rolls)
     })
     
     output$grid <- renderDT({
          df <- as.data.frame(grid)
          colnames(df) <- paste("C", 1:10, sep="")
          rownames(df) <- paste("R", 1:10, sep="")
          
          datatable(df, options = list(dom = 't', paging = FALSE), rownames = TRUE) %>%
               formatStyle(
                    paste("C", pos$col, sep=""),
                    target = "row",
                    backgroundColor = styleEqual(pos$row, "yellow")
               ) %>%
               formatStyle(
                    paste("C", pos$new_col, sep=""),
                    target = "row",
                    backgroundColor = styleEqual(pos$new_row, "lightblue")
               )
     })
}

# Run App
shinyApp(ui, server)