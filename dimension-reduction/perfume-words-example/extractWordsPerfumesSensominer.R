countWords <- function(base, sep.word=";"){
  base <- as.data.frame(base)
  if (!inherits(base, "data.frame")) 
    stop("base should be a data.frame")
  nobj <- nrow(base)
  groups <- numeric()
  ind.words <- numeric()
  allwords <- textual(data.frame(c(sapply(base, as.character))), 
                      num.text=1, sep.word = ";", contingence.by = 1)$cont.table
  woerter <- colnames(allwords)
  data <- matrix(0, nrow=ncol(allwords), ncol=nobj*ncol(base), 
                 dimnames=list(word=woerter, 
                 objectByPerson=
                   paste(rep(rownames(base), each=ncol(base)),
                         rep(1:ncol(base), nobj),sep="_")))
  sumdata <- matrix(0, ncol(allwords), nobj, 
                 dimnames=list(word=woerter, 
                               object=rownames(base)))
  for (i in 1:ncol(base)) {
  ## table of words used by ith panelist
    ## objects are column names (all i have same set)
    ## words are row names (different i have different sets)
  ind.table <- textual(tab = cbind.data.frame(rownames(base), 
                       base[, i]), num.text = 2, contingence.by = 1, 
                       maj.in.min = TRUE, 
                       sep.word = sep.word)$cont.table
  data[colnames(ind.table), (i-1)*nobj + (1:nobj)] <- t(ind.table)
  sumdata[colnames(ind.table), ] <- sumdata[colnames(ind.table), ] + t(ind.table)
  }
  list(data=data, sumdata=sumdata)
}
require(SensoMineR)
data("perfume") # contained in the SensoMineR package
cW <- countWords(perfume)
cW$sumdata
