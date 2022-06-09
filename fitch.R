

fitch_quiz <- function(ntips = 4, levels = c("A", "B"), i=4) {
  tree <- rtree(ntips)
  tree$tip.label <- paste0("t", seq_len(Ntip(tree)))
  node <- reorder(tree, "postorder")$edge[,2]
  pos <- node[node>Ntip(tree)]
  pos <- c(pos, Ntip(tree) + 1L)
  nnode <- Nnode(tree)

  dat <- allSitePattern(ntips, levels = levels) 
  nc <- attr(dat, "nc")
  x <- dat[,i]
  contrast <- attr(x, "contrast")
  colnames(contrast) <- levels(x)

  node_labels <- paste0("Node ", seq_along(pos), ":")
  pscore <- fitch(tree, x)
  anc_states <- ancestral.pars(tree, x, "POSTORDER")
  y <- matrix(unlist(anc_states), ncol = nc, byrow = TRUE)
  y[y>0] <- 1
  anc_char <- apply(y, 1, \(x, levels){paste0(levels[x==1], collapse = "")}, levels)
  anc_char <- anc_char[-(1:Ntip(tree))]

  list(anc_states=anc_char, tree=tree, tips=as.character(x), pos=pos, 
       my_anc=character(length(anc_char)), pscore=pscore, 
       node_labels=node_labels)
}

 