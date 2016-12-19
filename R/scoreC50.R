getXVars.C5.0 <- function(x){
  x$predictors
}

predProb.C5.0 <- function(x, new.data, ...){
  C50::predict.C5.0(object = x, newdata = new.data, type = "prob")
}

samplePct.C5.0 <- function(x, os.value, new.data){
  y.levels <- getYlevels(x)
  new.y <- if(y.levels[1] == os.value) 2 - as.numeric(x$y) else as.numeric(x$y) - 1

  100*sum(new.y) / length(new.y)
}
