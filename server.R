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
  srt <- rep(0, k)
  srt[direction=="upwards"] = -90
  srt[direction=="downwards"] = 90
  srt[type=="fan"] = 0
#  adj <- rep(NULL, k)
#  adj[direction=="downwards" | direction=="upwards"] = 0.5
  # add srt adj arguments for upwards and downwards
  list(trees=trees, pos=pos, type=type, direction=direction, srt=srt)
}


# Define server logic required to draw a histogram
server <- function(input, output, session) {
  # Quiz 1
  observeEvent(input$update_q1, {
    updateRadioButtons(session, "radio_q1", selected = character(0))
  })
  
  datasetInput_q1 <- eventReactive(input$update_q1, {
    topology(type=c("phylogram"), direction="right") 
  }, ignoreNULL = FALSE)
  
  textResult_q1 <- eventReactive(input$check_q1, {
    td  <- datasetInput_q1() 
    if(is.null(input$radio_q1) )"Choose a tree!"
    else if(td$pos == input$radio_q1) praise()
    else "Try again!"
  }, ignoreNULL = FALSE)
  
  output$trees_q1 <- renderPlot({
    # generate bins based on input$bins from ui.R
    td  <- datasetInput_q1()
    par(mfrow=c(2,2))
    for(i in 1:4){
      plot(td$trees[[i]], type=td$type[i], direction=td$direction[i], 
           use.edge.length = FALSE, main=LETTERS[i])
    }
  })
  
  output$txt_q1 <- renderText({
    textResult_q1()
  })
  
  
  # Quiz 2
  observeEvent(input$update_q2, {
    updateRadioButtons(session, "radio_q2", selected = character(0))
  })
  
  datasetInput_q2 <- eventReactive(input$update_q2, {
    topology(type=c("phylogram", "phylogram", "cladogram", "fan")) 
  }, ignoreNULL = FALSE)
  
  textResult_q2 <- eventReactive(input$check_q2, {
    td  <- datasetInput_q2() 
    if(is.null(input$radio_q2) )"Choose a tree!"
    else if(td$pos == input$radio_q2) praise()
    else "Try again!"
  }, ignoreNULL = FALSE)
  
  output$trees_q2 <- renderPlot({
    # generate bins based on input$bins from ui.R
    td  <- datasetInput_q2()
    par(mfrow=c(2,2))
    for(i in 1:4){
      plot(td$trees[[i]], type=td$type[i], direction=td$direction[i], 
           use.edge.length = FALSE, main=LETTERS[i], srt=td$srt[i], lab4ut="h")
    }
  })
  
  output$txt_q2 <- renderText({
    textResult_q2()
  })
}