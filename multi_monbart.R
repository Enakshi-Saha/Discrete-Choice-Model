multi_monbart <- function (x.train, y.train, x.test = matrix(0, 0, 0), type = "pbart", 
          ntype = as.integer(factor(type, levels = c("wbart", "pbart", 
                                                     "lbart"))), sparse = FALSE, theta = 0, omega = 1, a = 0.5, 
          b = 1, augment = FALSE, rho = NULL, xinfo = matrix(0, 0, 
                                                             0), usequants = FALSE, rm.const = TRUE, k = 2, power = 2, 
          base = 0.95, tau.num = c(NA, 3, 6)[ntype], offset = NULL, 
          ntree = c(200L, 50L, 50L)[ntype], numcut = 100L, ndpost = 1000L, 
          nskip = 100L, keepevery = c(1L, 10L, 10L)[ntype], printevery = 100L, 
          transposed = FALSE, hostname = FALSE, mc.cores = 2L, nice = 19L, 
          seed = 99L) 
{
  library(mBART)
  if (type == "wbart" || is.na(ntype)) 
    stop("type argument must be set to either 'pbart' or 'lbart'")
  cats <- unique(sort(y.train))
  K <- length(cats)
  if (K < 2) 
    stop("there must be at least 2 categories")
  L = length(offset)
  if (!(L %in% c(0, K))) 
    stop(paste0("length of offset argument must be 0 or ", 
                K))
  if (!transposed) {
    temp = bartModelMatrix(x.train, numcut, usequants = usequants, 
                           xinfo = xinfo, rm.const = rm.const)
    x.train = t(temp$X)
    numcut = temp$numcut
    xinfo = temp$xinfo
    if (length(x.test) > 0) {
      x.test = bartModelMatrix(x.test)
      x.test = t(x.test[, temp$rm.const])
    }
    rm.const <- temp$rm.const
    rm(temp)
  }
  post <- list()
  post$K <- K
  post$cats <- cats
  N <- length(y.train)
  P <- nrow(x.train)
  if (length(x.test)) {
    Q <- ncol(x.test)
    post$yhat.test <- matrix(nrow = ndpost, ncol = Q * K)
    post$prob.test <- matrix(nrow = ndpost, ncol = Q * K)
    post$comp.test <- matrix(nrow = ndpost, ncol = Q * K)
  }else Q <- 0
  L <- K - 1
  post$varcount <- as.list(1:L)
  post$varprob <- as.list(1:L)
  post$varcount.mean <- matrix(nrow = L, ncol = P)
  post$varprob.mean <- matrix(nrow = L, ncol = P)
  post$offset <- 0
  post$treedraws <- list()
  post$treedraws$trees <- as.list(1:L)
  post.list <- as.list(1:L)
  for (h in 1:K) {
    cond <- which(y.train >= cats[h])
    if (h < K) {
      post.list[[h]] <- mon.bart(x.train = t(x.train[,cond]), y.train = (y.train[cond] == h) * 1, x.test=t(x.test), ndpost=ndpost)
    }
    if (Q > 0) 
      for (i in 1:Q) {
        j <- (i - 1) * K + h
        if (h == K) 
          post$prob.test[, j] <- post$comp.test[, j - 
                                                  1]
        else {
          post$yhat.test[, j] <- post.list[[h]]$yhat.test[, 
                                                          i]
          if (h == 1) {
            post$comp.test[, j] <- 1 - post.list[[h]]$prob.test[, 
                                                                i]
            post$prob.test[, j] <- post.list[[h]]$prob.test[, 
                                                            i]
          }
          else {
            post$comp.test[, j] <- post$comp.test[, j - 
                                                    1] * (1 - post.list[[h]]$prob.test[, i])
            post$prob.test[, j] <- post$comp.test[, j - 
                                                    1] * post.list[[h]]$prob.test[, i]
          }
        }
      }
    #if (h < K) {
      #post$varcount[[h]] <- post.list[[h]]$varcount
      #post$varprob[[h]] <- post.list[[h]]$varprob
      #for (j in 1:P) {
        #post$varcount.mean[h, j] <- post.list[[h]]$varcount.mean[j]
        #post$varprob.mean[h, j] <- post.list[[h]]$varprob.mean[j]
      #}
      #post$offset[h] <- post.list[[h]]$offset
      #post$treedraws$trees[[h]] <- post.list[[h]]$nkeeptreedraws$trees
    #}
  }
  #post$treedraws$cutpoints <- post.list[[1]]$nkeeptreedraws$cutpoints
  #dimnames(post$varcount.mean)[[2]] <- dimnames(post$varcount[[1]])[[2]]
  #dimnames(post$varprob.mean)[[2]] <- dimnames(post$varprob[[1]])[[2]]
  #post$rm.const <- post.list[[1]]$rm.const
  #post$type <- type
  #post$comp.test <- NULL
  if (Q > 0) {
    post$prob.test.mean <- apply(post$prob.test, 2, mean)
  }
  attr(post, "class") <- "mbart"
  return(post)
}