#' General FGLS Estimators
#' 
#' General FGLS estimators for panel data (balanced or unbalanced)
#' 
#' 
#' `pggls` is a function for the estimation of linear panel models by
#' general feasible generalized least squares, either with or without
#' fixed effects. General FGLS is based on a two-step estimation
#' process: first a model is estimated by OLS (`model = "pooling"`),
#' fixed effects (`model = "within"`) or first differences (`model =
#' "fd"`), then its residuals are used to estimate an error covariance
#' matrix for use in a feasible-GLS analysis. This framework allows
#' the error covariance structure inside every group (if `effect =
#' "individual"`, else symmetric) of observations to be fully
#' unrestricted and is therefore robust against any type of intragroup
#' heteroskedasticity and serial correlation. Conversely, this
#' structure is assumed identical across groups and thus general FGLS
#' estimation is inefficient under groupwise heteroskedasticity. Note
#' also that this method requires estimation of \eqn{T(T+1)/2}
#' variance parameters, thus efficiency requires N >> T (if `effect =
#' "individual"`, else the opposite). Setting `model = "random"` or
#' `model = "pooling"`, both produce an unrestricted FGLS model as in
#' Wooldridge, Ch. 10.5, although the former is deprecated and
#' included only for retro--compatibility reasons. If `model =
#' "within"` (the default) then a FEGLS (fixed effects GLS, see ibid.)
#' is estimated; if `model = "fd"` a FDGLS (first-difference GLS).
#' 
#' @aliases pggls
#' @importFrom bdsmatrix bdsmatrix
#' @param formula a symbolic description of the model to be estimated,
#' @param object,x an object of class `pggls`,
#' @param data a `data.frame`,
#' @param subset see [lm()],
#' @param na.action see [lm()],
#' @param effect the effects introduced in the model, one of
#'     `"individual"` or `"time"`,
#' @param model one of `"within"`, `"pooling"`, `"random"` or `"fd"`,
#' @param index the indexes, see [pdata.frame()],
#' @param digits digits,
#' @param width the maximum length of the lines in the print output,
#' @param \dots further arguments.
#' @return An object of class `c("pggls","panelmodel")` containing:
#'     \item{coefficients}{the vector of coefficients,}
#'     \item{residuals}{the vector of residuals,}
#'     \item{fitted.values}{the vector of fitted values,}
#'     \item{vcov}{the covariance matrix of the coefficients,}
#'     \item{df.residual}{degrees of freedom of the residuals,}
#'     \item{model}{a data.frame containing the variables used for the
#'     estimation,}
#'     \item{call}{the call,}
#'     \item{sigma}{the estimated intragroup (or cross-sectional, if
#'     `effect = "time"`) covariance of errors,}
#' @export
#' @author Giovanni Millo
#' @references
#'
#' \insertRef{IM:SEUN:SCHM:WOOL:99}{plm}
#'
#' \insertRef{KIEF:80}{plm}
#' 
#' \insertRef{WOOL:02}{plm}
#' 
#' \insertRef{WOOL:10}{plm}
#' 
#' @keywords regression
#' @examples
#' 
#' data("Produc", package = "plm")
#' zz_wi <- pggls(log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp,
#'                data = Produc, model = "within")
#' summary(zz_wi)
#' 
#' zz_pool <- pggls(log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp,
#'                  data = Produc, model = "pooling")
#' summary(zz_pool)
#' 
#' zz_fd <- pggls(log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp,
#'                data = Produc, model = "fd")
#' summary(zz_fd)
#' 
#' 
pggls <- function(formula, data, subset, na.action,
                  effect = c("individual", "time"),
                  model = c("within", "random", "pooling", "fd"),
                  index = NULL, ...)
{
  # check and match the arguments
    effect <- match.arg(effect)
    model.name <- match.arg(model)
    if (model.name == "random") {
        warning("for argument 'model' to pggls(), the value 'random' has been renamed as 'pooling'",
                call. = FALSE)
        model.name <- "pooling"
    }
    data.name <- paste(deparse(substitute(data)))
    cl <- match.call()
    plm.model <- match.call(expand.dots = FALSE)
    m <- match(c("formula", "data", "subset", "na.action", "effect", "model", "index"), names(plm.model), 0)
    plm.model <- plm.model[c(1L, m)]
    plm.model[[1L]] <- as.name("plm")
    plm.model$model <- model.name
    plm.model <- eval(plm.model, parent.frame())
    
    index <- attr(model.frame(plm.model), "index")
    pdim <- pdim(plm.model)
    balanced <- pdim$balanced
    
    time.names <- pdim$panel.names$time.names
    id.names <- pdim$panel.names$id.names
    coef.names <- names(coef(plm.model))
    K <- length(coef.names)
    
    if (model.name == "fd") {
    ## eliminate first year in indices
        nt <- pdim$Tint$nt[-1L]
        Ti <- pdim$Tint$Ti - 1
        T <- pdim$nT$T - 1
        n <- pdim$nT$n
        N <- pdim$nT$N - pdim$Tint$nt[1L]
        time.names <- pdim$panel.names$time.names[-1L]
        tind <- as.numeric(index[ , 2L])
        sel <- (tind-c(-1,tind[-length(tind)]))==1
        index <- index[sel, ]
        id <- index[[1L]]
        time <- factor(index[[2L]], levels = attr(index[ , 2L], "levels")[-1L])
    } else {
        nt <- pdim$Tint$nt
        Ti <- pdim$Tint$Ti
        T <- pdim$nT$T
        n <- pdim$nT$n
        N <- pdim$nT$N
        
        id <- index[[1L]]
        time <- index[[2L]]
    }
    
    if (effect == "time") {
        cond <- time
        other <- id
        ncond <- T
        nother <- n
        cond.names <- time.names
        other.names <- id.names
        groupsdim <- nt
    }
    else {
        cond <- id
        other <- time
        ncond <- n
        nother <- T
        cond.names <- id.names
        other.names <- time.names
        groupsdim <- Ti
    }
    myord <- order(cond, other)
    X <- model.matrix(plm.model)[myord, , drop = FALSE]
    commonpars <- intersect(names(coef(plm.model)), colnames(X))
    X <- X[ , commonpars, drop = FALSE]
    y <- pmodel.response(plm.model)[myord]
    resid <- lm.fit(X, y)$residuals
    
    cond <- cond[myord]
    other <- other[myord]
    drop1 <- FALSE
    if (drop1 && model.name %in% c("within", "fd")) {
    ## drop one time period (e.g., first as we do here)
    ## (see Wooldridge (2002) 10.5, eq. 10.61)/Woolridge (2010),10.5.5, eq.10.61)
    ## this is needed according to Wooldridge (2002), p.277 / Wooldridge (2010), p. 312
    ## but is not totally robust to unbalancedness, dummies etc.
    ## 
    ## The function turns out to work irrespective of dropping
    ## one time period or not! Absolutely the same results!
    ## This is thx to solve.bdsmatrix() using a generalized
    ## inverse, which in this case where rank=T-1 is equivalent
    ## to discarding one year (N columns)
    ## -> as noted by Wooldridge
    ##
    ## The 'if' parameterization is just for debugging.
    
        numeric.t <- as.numeric(other)
        t1 <- which(numeric.t != min(numeric.t))
        X0 <- X
        y0 <- y
        X <- X[t1, ]
        y <- y[t1]
        resid <- lm.fit(X, y)$residuals
                                        #resid[t1]
        cond <- cond[t1]
        other <- other[t1]
        nother <- nother - 1
        other.names <- other.names[-1L]
    }
    tres <- array(NA_real_, dim = c(nother, nother, ncond),
                  dimnames = list(other.names, other.names, cond.names))
    lcnd <- levels(cond)
    if (balanced) {
        for (i in 1:ncond) {
            ut <- resid[cond == lcnd[i]]
            tres[ , , i] <- ut %o% ut
        }
        subOmega <- rowMeans(tres, dims = 2L) # == apply(tres, 1:2, mean) but faster
        omega <- bdsmatrix(rep(nother, ncond), rep(subOmega, ncond))
    } else {
        lti <- list()
        for (i in 1:ncond) {
            cond.i <- cond == lcnd[i]
            ut <- resid[cond.i]
            names(ut) <- lti[[i]] <- other[cond.i]
            out <- ut %o% ut
            tres[names(ut), names(ut), i] <- out
        }
        subOmega <- rowMeans(tres, dims = 2L, na.rm = TRUE) # == apply(tres, 1:2, mean, na.rm = TRUE) but faster
        list.cov.blocks <- list()
        for (i in 1:ncond) {
            list.cov.blocks[[i]] <- subOmega[lti[[i]], lti[[i]]]
        }
        omega <- bdsmatrix(groupsdim, unlist(list.cov.blocks, use.names = FALSE))
    }
    A <- crossprod(X, solve(omega, X))
    B <- crossprod(X, solve(omega, y))
    vcov <- solve(A)
    coef <- as.numeric(solve(A, B))
    if (drop1 && model == "within") {
        X <- X0
        y <- y0
    }
    residuals <- y - as.numeric(tcrossprod(coef, X))
    df.residual <- nrow(X) - ncol(X)
    fitted.values <- y - residuals
    names(coef) <- rownames(vcov) <- colnames(vcov) <- coef.names
    pmodel <- attr(plm.model, "pmodel")
    pmodel$model.name <- model
    pmodel$effect.name <- effect
    fullGLS <- list(coefficients  = coef,
                    residuals     = residuals,
                    fitted.values = fitted.values,
                    vcov          = vcov,
                    df.residual   = df.residual,
                    model         = model.frame(plm.model),
                    sigma         = subOmega,
                    call          = cl,
                    formula       = plm.model$formula)
    
    fullGLS <- structure(fullGLS, pdim = pdim, pmodel = pmodel)
    class(fullGLS) <- c("pggls", "panelmodel")
    fullGLS
}

#' @rdname pggls
#' @export
summary.pggls <- function(object,...){
  pmodel <- attr(object, "pmodel")
  std.err <- sqrt(diag(object$vcov))
  b <- object$coefficients
  z <- b/std.err
  p <- 2*pnorm(abs(z), lower.tail = FALSE)
  CoefTable <- cbind(b, std.err, z, p)
  colnames(CoefTable) <- c("Estimate", "Std. Error", "z-value", "Pr(>|z|)")
  object$CoefTable <- CoefTable
  y <- object$model[[1L]]
  object$tss <- tss(y)
  object$ssr <- as.numeric(crossprod(residuals(object)))
  object$rsqr <- 1-object$ssr/object$tss
  class(object) <- c("summary.pggls")
  return(object)
}

#' @rdname pggls
#' @export
print.summary.pggls <- function(x, digits = max(3, getOption("digits") - 2), width = getOption("width"), ...){
  pmodel <- attr(x, "pmodel")
  pdim <- attr(x, "pdim")
  formula <- pmodel$formula
  model.name <- pmodel$model.name
  effect.name <- pmodel$effect.name
  cat(paste(effect.pggls.list[effect.name], " ", sep = ""))
  cat(paste(model.pggls.list[model.name], "\n", sep = ""))
  cat("\nCall:\n")
  print(x$call)
  cat("\n")
  print(pdim)
  cat("\nResiduals:\n")
  print(sumres(x)) # was until rev. 1176:  print(summary(unlist(residuals(x))))
  cat("\nCoefficients:\n")
  printCoefmat(x$CoefTable, digits = digits)
  cat(paste("Total Sum of Squares: ",    signif(x$tss,digits),  "\n", sep=""))
  cat(paste("Residual Sum of Squares: ", signif(x$ssr,digits),  "\n", sep=""))
  cat(paste("Multiple R-squared: ",      signif(x$rsqr,digits), "\n", sep=""))
  invisible(x)
}

#' @rdname pggls
#' @export
residuals.pggls <- function(object, ...) {
  return(pres(object))
}
