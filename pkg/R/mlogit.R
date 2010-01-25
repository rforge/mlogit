mlogit <- function(formula, data, subset, weights, na.action,
                   alt.subset = NULL, reflevel= NULL, estimate = TRUE, ...){

  start.time <- proc.time()
  callT <- match.call(expand.dots = TRUE)
  callF <- match.call(expand.dots = FALSE)
  formula <- callF$formula <- logitform(formula)
  
  # 1 ############################################################
  #  check whether arguments for mlogit.data are present: if so run
  #  mlogit.data
  ################################################################
  
  mldata <- callT
  m <- match(c("data","choice","shape","varying","sep",
               "alt.var","id.var","alt.levels",
               "opposite", "drop.index"),
             names(mldata), 0L)
  use.mlogit.data <- sum(m[-1]) > 0
  if (use.mlogit.data){
    mldata <- mldata[c(1L, m)]
    mldata[[1L]] <- as.name("mlogit.data")
    data <- eval(mldata, parent.frame())
  }
  
  # 2 ###########################################################
  # model.frame (with the data provided or the one computed by
  # mlogit.data
  ###############################################################

  mf <- callT
  m <- match(c("formula", "data", "subset", "na.action","weights","id"),
             names(mf), 0L)
  mf <- mf[c(1L, m)]
  mf$drop.unused.levels <- TRUE
  mf$formula <- formula
  mf[[1L]] <- as.name("model.frame")
  if (use.mlogit.data) mf$data <- data
  mf <- eval(mf, parent.frame())
  index <- attr(mf, "index")
  alt <- index[["alt"]]
  chid <- index[["chid"]]
  # change the reference level of the response if required
  if (!is.null(reflevel)){
    alt <- relevel(alt, reflevel)
  }
  # compute the relevent subset if required
  if (!is.null(alt.subset)){
    # we keep only choices that belong to the subset
    choice <- alt[model.response(mf)]
    choice <- choice %in% alt.subset
    unid <- unique(chid)
    names(choice) <- as.character(unid)
    id.kept <- choice[as.character(chid)]
    # we keep only the relevant alternatives
    alt.kept <- alt %in% alt.subset
    # the relevant subset for the data.frame and the indexes
    mf <- mf[id.kept & alt.kept, , drop = FALSE]
    alt <- alt[id.kept & alt.kept , drop = TRUE]
    chid <- chid[id.kept & alt.kept , drop = TRUE]
  }
  # balanced the data.frame i.e. insert rows with NA when an
  # alternative is not relevant
  alt.un <- unique(alt)
  chid.un <- unique(chid)
  n <- length(chid.un)
  T <- length(alt.un)
  if (nrow(mf) != (n * T)){
    rownames(mf) <- paste(chid, alt, sep = ".")
    all.rn <- as.character(t(outer(chid.un, alt.un, paste, sep = ".")))
    mf <- mf[all.rn, ]
    rownames(mf) <- all.rn
    chid <- rep(chid.un, each = T)
    alt <- rep(alt.un, n)
    index <- data.frame(chid = chid, alt = alt, row.names = rownames(mf))
  }
  #suppress individuals for which no choice is made
  delete.id <- tapply(model.response(mf), chid, sum, na.rm = TRUE)
  delete.id <- names(delete.id[delete.id == 0])
  mf <- mf[!(chid %in% delete.id), ]
  index <- index[rownames(mf),]
  index[[1]] <- index[[1]][, drop=TRUE]
  index[[2]] <- alt <- index[[2]][, drop=TRUE]
  attr(mf, "index") <- index
  
  # if estimate is FALSE, return the data.frame
  if (!estimate) return(mf)

  # 3 ###########################################################
  # extract the elements of the model
  ###############################################################

  y <- model.response(mf)
  X <- model.matrix(formula, mf)
  K <- ncol(X)
  colnamesX <- colnames(X)
  if (any(names(mf)=="(weights)"))
    mf[["(weights)"]] <- mf[["(weights)"]]/mean(mf[["(weights)"]])
  freq <- table(alt[y])

  X <- split(as.data.frame(X), alt)
  X <- lapply(X, as.matrix)
  y <- split(y, alt)
  rownamesX <- split(chid, alt)
  y <- lapply(y, function(x){x[is.na(x)] <- FALSE ; x})

  # 4 ###########################################################
  # estimate the model using maxLik and passing the correct arguments
  ###############################################################
  
  # simplify the likelihood, the gradient and the hessian
  f <- function(param) mlogit.lnl(param, X, y, weights = NULL)
  g <- function(param) mlogit.grad(param, X, y, weights = NULL)
  h <- function(param) mlogit.hess(param, X, y, weights = NULL)

  myopposite <- FALSE
  mysum <- TRUE
  
##   f <- function(param){
##     lnl.mlogit(param, X, y, weights = NULL, gradient = FALSE,
##                hessian = FALSE, opposite = myopposite,
##                sumlnl = mysum, sumgrad = mysum)
##   }

##   g <- function(param){
##     attr(lnl.mlogit(param, X, y, weights = NULL, gradient = TRUE,
##                     hessian = FALSE, opposite = myopposite,
##                     sumlnl = mysum, sumgrad = mysum),
##          "gradient"
##          )
##   }

##   h <- function(param){
##     attr(lnl.mlogit(param, X, y, weights = NULL, gradient = FALSE,
##                     hessian = TRUE, opposite = myopposite,
##                     sumlnl = mysum, sumgrad = mysum),
##          "hessian"
##          )
##   }

##   l <- function(param){
##     lnl.mlogit(param, X, y, weights = NULL, gradient = TRUE,
##                hessian = TRUE, opposite = TRUE,
##                sumlnl = TRUE, sumgrad = TRUE)
##   }

  
  result <- callT
  m <- match(c("method","print.level","iterlim","start","constPar","activePar"),
             names(result), 0L)
  result <- result[c(1L, m)]
  result[[1L]] <- as.name("maxLik")
  result[c('logLik', 'grad', 'hess')] <- c(f,  g, h)
  if (is.null(result$start)) result$start <- rep(0, K)
  result <- eval(result, parent.frame())
  
  # 5 ###########################################################
  # put the result in form
  ###############################################################

  n <- sum(freq)
  logLik0 <- sum(freq*log(freq/n))
  coef <- result$estimate
  P <- mlogit.P(coef,X)
  fitted.values <- as.matrix(as.data.frame(P))
  residuals <- as.matrix(as.data.frame(y)) - fitted.values
  logLik <- result$maximum
  attr(logLik,"df") <- length(coef)
  hessian <- result$hessian
  gradient <- result$gradient
  names(coef) <- rownames(hessian) <- colnames(hessian) <- colnamesX

  # compute the optimisation summary
  est.stat <- structure(list(elaps.time = proc.time() - start.time,
                             nb.iter = result$iterations,
                             eps = gradient%*%solve(-hessian)%*%gradient,
                             method = result$type,
                             message = result$message),
                        class = "est.stat")

  result <- list(coefficients = coef, logLik = logLik, logLik0 = logLik0,
                 hessian = hessian, gradient = gradient,
                 call = callT, est.stat = est.stat, freq = freq,
                 residuals = residuals, fitted.values = fitted.values,
                 formula = formula, model= mf)
  class(result) <- "mlogit"
  result
}

  # 6 ###########################################################
  # Computation of the Likelihood
  ###############################################################
  
mlogit.P <- function(param, X){
  eXb <- lapply(X,function(x) exp(crossprod(t(x), param)))
  seXb <- suml(eXb)
  P <- lapply(eXb,
              function(x){
                v <- x/seXb
                v[is.na(v)] <- 0
                as.vector(v)
              }
              )
  P
}

mlogit.lnl <- function(param, X, y, weights = NULL){
  if (is.null(weights)) weights <- 1
  P <- mlogit.P(param,X)
  Pch <- suml(mapply("*", P, y, SIMPLIFY = FALSE))
  weights*log(Pch)
}

mlogit.grad <- function(param, X, y, weights = NULL){
  if (is.null(weights)) weights <- 1
  P <- mlogit.P(param,X)
  PX <- suml(mapply("*", X, P, SIMPLIFY = FALSE))
  Xch <- suml(mapply("*", X, y, SIMPLIFY = FALSE))
  weights*(Xch-PX)
}

mlogit.hess <- function(param, X, y, weights = NULL){
  if (is.null(weights)) weights <- 1
  P <- mlogit.P(param,X)
  PX <-  suml(mapply("*", X, P, SIMPLIFY = FALSE))
  Pch <- suml(mapply("*", P, y, SIMPLIFY = FALSE))
  Xch <- suml(mapply("*", X, y, SIMPLIFY = FALSE))
  XmPX <- lapply(X,
                 function(x){
                   g <- x - PX
                   g[is.na(g)] <- 0
                   g
                 }
                 )
  - suml( mapply(function(x, y) crossprod(x*y, y), P, XmPX, SIMPLIFY = FALSE))
}


print.est.stat <- function(x, ...){
  et <- x$elaps.time[3]
  i <- x$nb.iter[1]
  halton <- x$halton
  eps <- as.numeric(x$eps)
  if (!is.null(x$type) && x$type != "simple"){
    R <- x$nb.draws
    cat(paste("Simulated maximum likelihood with",R,"draws\n"))
  }
  s <- round(et,0)
  h <- s%/%3600
  s <- s-3600*h
  m <- s%/%60
  s <- s-60*m
  tstr <- paste(h,"h:",m,"m:",s,"s",sep="")
  cat(paste(i,"iterations,",tstr,"\n"))
  if (!is.null(halton)) cat("Halton's sequences used\n")
  cat(paste("g'(-H)^-1g =",sprintf("%5.3G",eps),"\n"))
}

suml <- function(x){
  n <- length(x)
  if (!is.null(dim(x[[1]]))){
    d <- dim(x[[1]])
    s <- matrix(0,d[1],d[2])
    for (i in 1:n){
      x[[i]][is.na(x[[i]])] <- 0
      s <- s+x[[i]]
    }
  }
  else{
    s <- rep(0,length(x[[n]]))
    for (i in 1:n){
      s <- s+x[[i]]
    }
  }
  s
}


  numderiv <- function(f, param){
  eps <- 1E-4
  nc <- c()
  for (i in 1:length(param)){
    params <- param
    parami <- param
    params[i] <- params[i]+eps
    parami[i] <- parami[i]-eps
    lnls <- eval(call(f, param = params))
    lnli <- eval(call(f, parami))
    nc <- c(nc, (lnls-lnli)/(2*eps))
  }
  nc
}
