#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

library(ape)
library(phangorn)
library(praise)

rotate_clades <- function(tree, times = tree$Nnode %/% 2, swaps = NULL ){
    if(is.null(swaps)) swaps <- sample( unique(tree$edge[,1]), times) 
    for(i in swaps) {
        tree <- rotate(tree, i)
        tree <- read.tree(text=write.tree(tree)) 
    }  
    tree
}

topology <- function(tree=NULL, ntips=8, nni=1, k=4, 
                     type=c("phylogram", "cladogram", "fan", "unrooted"), 
                     direction=NULL){
    
    if(is.null(tree)) tree <- rtree(ntips, tip.label = LETTERS[1:ntips])  
    tree2 <- rNNI(tree, nni)
    while(RF.dist(tree, tree2)==0) tree2 <- rNNI(tree, nni)
    
    pos <- sample(k, 1L) 
    
    trees <- vector("list", k)
    for(i in seq_len(k)){
        if(pos==i) trees[[i]] <- rotate_clades(tree2)
        else trees[[i]] <- rotate_clades(tree)
    }
    class(trees) <- "multiPhylo"
    
    if(is.null(type)) type <- sample(c("phylogram", "cladogram", "fan", "unrooted"), 
                                     k, replace = TRUE)
    if(length(type) < k) type <- rep(type, length.out=k)
    if(is.null(direction)) direction <- sample(c("rightwards", "leftwards", "upwards", "downwards"), 
                                               k, replace = TRUE)
    if(length(direction) < k) direction <- rep(direction, length.out=k)
    # add srt adj arguments for upwards and downwards
    list(trees=trees, pos=pos, type=type, direction=direction)
}


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Tree Thinking"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            actionButton("update", "New Quiz"),
            radioButtons("radio_ABCD", "Which of the four trees depicts a different relationship?",
                         c("A" = 1,
                           "B" = 2,
                           "C" = 3,
                           "D" = 4), 
                         selected=character(0)), 
            actionButton("check", "Check my choice")
        ),

        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("trees"), 
            textOutput("txt")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    datasetInput <- eventReactive(input$update, {
        topology(type=c("phylogram"), direction="right")
    }, ignoreNULL = FALSE)
    
    textResult <- eventReactive(input$check, {
        td  <- datasetInput() 
        if(is.null(input$radio_ABCD) )"Choose tree!"
        else if(td$pos == input$radio_ABCD) praise()
        else "Try again!"
    }, ignoreNULL = FALSE)
    
    comment <- eventReactive(input$check, {
        switch(input$dataset,
               "rock" = rock,
               "pressure" = pressure,
               "cars" = cars)
    }, ignoreNULL = FALSE)

    output$trees <- renderPlot({
        # generate bins based on input$bins from ui.R
        td  <- datasetInput()
        par(mfrow=c(2,2))
        for(i in 1:4){
            plot(td$trees[[i]], type=td$type[i], direction=td$direction[i], 
                         use.edge.length = FALSE, main=LETTERS[i])
        }
    })
    output$txt <- renderText({
        textResult()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)



