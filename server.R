library(shiny)
library(ape)
library(phangorn)
library(praise)

source("fitch.R")

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
  adj <- rep(0.5, k)
#  adj[direction=="downwards" | direction=="upwards"] = 0.5
  # add srt adj arguments for upwards and downwards
  list(trees=trees, pos=pos, type=type, direction=direction, srt=srt, adj=adj)
}


pruning <- function(tree=NULL, ntips=c(5,7,6,6), nni=1, max_tips=12){
  trees <- vector("list", length(ntips))
  tree <- rtree(max_tips, tip.label = LETTERS[1:max_tips])
  for(i in seq_along(ntips)){
    trees[[i]] <- keep.tip(tree, sample(max_tips, ntips[i]))
  }
  class(trees) <- "multiPhylo"
  k <- length(ntips)
  pos <- sample(k, 1L) 
  
  for(i in seq_len(k)){
    trees[[i]] <- rotate_clades(trees[[i]])
    if(i!=pos){
      tmp <- rNNI(trees[[i]])
      while(RF.dist(tmp, trees[[i]])==0) tmp <- rNNI(trees[[i]])
      trees[[i]] <- tmp
    }
  }
  class(trees) <- "multiPhylo"
  list(trees = c(tree, trees), pos=pos)
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
           use.edge.length = FALSE, main=LETTERS[i], edge.width=2)
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
    par(mfrow=c(2,2), mar=c(2,2,2,2))
    for(i in 1:4){
      if(td$type[i]=="fan")
      plot(td$trees[[i]], type=td$type[i], direction=td$direction[i], adj=0.5,
           use.edge.length = FALSE, main=LETTERS[i], srt=td$srt[i], lab4ut="h",
           x.lim=c(-1.1,1.1), y.lim=c(-1.1,1.1), edge.width=2)
      else
      plot(td$trees[[i]], type=td$type[i], direction=td$direction[i], adj=0.5,
           use.edge.length = FALSE, main=LETTERS[i], srt=td$srt[i], lab4ut="h", 
           edge.width=2)
    }
  })
  
  output$txt_q2 <- renderText({
    textResult_q2()
  })
  
  
  # Quiz 3
  observeEvent(input$update_q3, {
    updateRadioButtons(session, "radio_q3", selected = character(0))
  })
  
  datasetInput_q3 <- eventReactive(input$update_q3, {
    pruning() 
  }, ignoreNULL = FALSE)
  
  textResult_q3 <- eventReactive(input$check_q3, {
    td  <- datasetInput_q3() 
    if(is.null(input$radio_q3) )"Choose a tree!"
    else if(td$pos == input$radio_q3) praise()
    else "Try again!"
  }, ignoreNULL = FALSE)
  
  output$trees_q3 <- renderPlot({
    # generate bins based on input$bins from ui.R
    td  <- datasetInput_q3()
    nf <- layout(matrix(c(1,1,2,3,4,5), 3, 2, byrow = TRUE))
    par(mar=c(2,2,2,2))
    plot(td$trees[[1]], type="cladogram", direction="upwards", adj=0.5,
         label.offset = 1.1, use.edge.length = FALSE, srt=-90, cex=1.5, 
         edge.width=2)
    for(i in 1:4){
      plot(td$trees[[i+1]], type="cladogram", direction="upwards", adj=0.5,
           use.edge.length = FALSE, main=LETTERS[i], srt=-90,
           label.offset = 0.25, cex=1.5, edge.width=2)
    }
  })
  
  output$txt_q3 <- renderText({
    textResult_q3()
  })
  
  
  # Quiz 4
  
#  datasetInput <- eventReactive(input$update, {
#    switch(input$dataset,
#           "rock" = rock,
#           "pressure" = pressure,
 #          "cars" = cars)
 # }, ignoreNULL = FALSE)
  observeEvent(input$update_q4, {
    updateTextInput(session, "newick", value="")
  })
  
  
  datasetInput_q4 <- eventReactive(input$update_q4, {
    ntips <- 6
    tree <- rtree(ntips, tip.label=LETTERS[1:ntips]) 
    tree$tip.label <- LETTERS[1:ntips]
    tree
  }, ignoreNULL = FALSE)
  
  textResult_q4 <- eventReactive(input$check_q4, {
    td  <- datasetInput_q4() 
    nwk <- input$newick

    l_p <- lengths(regmatches(nwk, gregexpr("\\(", nwk)))
    r_p <- lengths(regmatches(nwk, gregexpr("\\)", nwk)))
    ## Be nice and check for semicolons and upper cases
    has_sc <- lengths(regmatches(nwk, gregexpr(";", nwk)))
    if(has_sc == 0) nwk <- paste0(nwk, ";")
    nwk <- toupper(nwk)
    txt=""
    if( l_p != r_p ){
      txt <- paste0(l_p, " left, ", r_p, "right paranthesis!")
    }
    else {
      tree <- read.tree(text=nwk)
      if(!is.null(tree)){
        if(RF.dist(unroot(td), unroot(tree))==0) txt <- praise()
        else txt <-"Not quite yet!"
      }
      else txt <- "Not quite yet!"
    }
    txt
  }, ignoreNULL = FALSE)
  
  output$txt_q4 <- renderText({
    textResult_q4()
  })
  
  
  output$trees_q4 <- renderPlot({
    # generate bins based on input$bins from ui.R
    td  <- datasetInput_q4()
    par(mar=c(2,2,2,2))
    plot(td, type="cladogram", direction="upwards", adj=0.5,
         label.offset = 0.5, use.edge.length = FALSE, srt=-90, cex=1.5, 
         edge.width=2)
  })
  
  
  ## Quiz 5
  datasetInput_q5 <- eventReactive(input$update_q5, {
    fitch_quiz() 
  }, ignoreNULL = FALSE)
  
  observeEvent(input$update_q5, {
    updateTextInput(session, "node_1", value = character(0))
    updateTextInput(session, "node_2", value = character(0))
    updateTextInput(session, "node_3", value = character(0))
    updateTextInput(session, "pscore", value = character(0))
#    output$txt_q5 <- renderText({""})
  })
  
  output$trees_q5 <- renderPlot({
    # generate bins based on input$bins from ui.R
    td  <- datasetInput_q5()
    plot(td$tree, use.edge.length = FALSE, show.tip.label = FALSE, 
         direction = "down", edge.width=2)
    tiplabels(td$tips, cex=2)
    n1 <- input$node_1 
    n2 <- input$node_2
    n3 <- input$node_3
    nodelabels(paste(td$node_labels, c(n1, n2, n3)), td$pos, cex=2)
  })
  
  textResult_q5 <- reactive({
    td  <- datasetInput_q5() 
    anc <- td$anc_states
    ord <- td$pos - 4L
    anc <- anc[ord]
    
    n1 <- identical(help_fun(input$node_1), help_fun(anc[1]))  
    n2 <- identical(help_fun(input$node_2), help_fun(anc[2]))
    n3 <- identical(help_fun(input$node_3), help_fun(anc[3]))
    psc <- td$pscore == input$pscore
    
    hint <- "Check "
    
    if(all(n1, n2, n3, psc)) praise()
    else{ 
      hint <- "Not quite yet!\nCheck "
      if(!n1) hint <- paste(hint, "node 1,")
      if(!n2) hint <- paste(hint, "node 2,")
      if(!n3) hint <- paste(hint, "node 3,")
      if(!psc) hint <- paste(hint, "parsimony score.")
      hint
    }
  }) |> bindEvent(input$check_q5)
  
  output$txt_q5 <- renderText({
    textResult_q5()
  })
  
  
  
}