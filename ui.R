
# Define UI for application that draws a histogram
ui <- navbarPage("Tree Thinking",
  
  # Application title
  #titlePanel(),
  
  # Sidebar with a slider input for number of bins 
  tabPanel("Quiz 1", 
    sidebarLayout(
      sidebarPanel(
        actionButton("update_q1", "New Quiz"),
        radioButtons("radio_q1", "Which of the four trees depicts a different relationship?",
                     c("A" = 1,
                       "B" = 2,
                       "C" = 3,
                       "D" = 4), 
                     selected=character(0)), 
        actionButton("check_q1", "Check my choice")
      ),
    
      # Show a plot of the generated distribution
      mainPanel(
        plotOutput("trees_q1"), 
        textOutput("txt_q1")
      )
    )
  ), 
  tabPanel("Quiz 2",
    sidebarLayout(
      sidebarPanel(
        actionButton("update_q2", "New Quiz"),
        radioButtons("radio_q2", "Which of the four trees depicts a different relationship?",
                     c("A" = 1,
                       "B" = 2,
                       "C" = 3,
                       "D" = 4), 
                     selected=character(0)), 
        actionButton("check_q2", "Check my choice")
      ),
             
      mainPanel(
        textOutput("txt_q2"),
        plotOutput("trees_q2") 
      )
    )
  ),       
  tabPanel("Quiz 3"),
  tabPanel("Quiz 4"),
  tabPanel("About")
)

