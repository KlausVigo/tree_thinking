library(markdown)
tabPanelAbout <- source("about.R")$value

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
        textOutput("txt_q1"),        
        plotOutput("trees_q1")
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
  tabPanel("Quiz 3",
           sidebarLayout(
             sidebarPanel(
               actionButton("update_q3", "New Quiz"),
               radioButtons("radio_q3", "Which of the four smaller trees is compatible with the larger tree once the extra taxa are pruned?",
                            c("A" = 1,
                              "B" = 2,
                              "C" = 3,
                              "D" = 4), 
                            selected=character(0)), 
               actionButton("check_q3", "Check my choice")
             ),
             
             mainPanel(
               textOutput("txt_q3"),
               plotOutput("trees_q3") 
             )
           )
  ),           
  tabPanel("Quiz 4",
    sidebarLayout(
     sidebarPanel(
       actionButton("update_q4", "New tree"),
#       p("Write the tree in paranthetical notation (Newick), e.g. ((A,B),C);"),
       textInput("newick", h4("Write the tree in paranthetical notation (Newick), e.g. ((A,B),C);"), 
                 value = ""), 
       actionButton("check_q4", "Check my choice")
      ),
      mainPanel(
        textOutput("txt_q4"),
        plotOutput("trees_q4"), 
        plotOutput("your_trees_q4")
      )
    )
  ),       
  tabPanelAbout()
)

