## Common Correlated Effects Pooled/MG estimators
  ## ref. Holly, Pesaran and Yamagata JoE 158 (2010)
  ## (also Kapetanios, Pesaran and Yamagata JoE 2010)
  ## CCEP and CCEMG together in the same SW framework
  ## based on generalized FEs

  ## this version 6: includes both defactored (cce) and raw (standard) residuals,
  ## leaving to a special residuals.pcce method the choice of which to retrieve

  ## NB the effect of including a trend is exactly the same as for
  ## including as.numeric(<timeindex>) in the model specification
  ## If the panel is unbalanced, though, then for some i the trend becomes
  ## (3,4,5, ...) instead of (1,2,3, ...); the difference is absorbed by
  ## the individual intercept, and *the group intercept* changes.

## needed for standalone operation:
#plm <- plm:::plm
#pdim <- plm:::pdim

#model.matrix.plm <- plm:::model.matrix.plm
#pmodel.response.plm <- plm:::pmodel.response.plm

#tss <- plm:::tss


#' Common Correlated Effects estimators
#' 
#' Common Correlated Effects Mean Groups (CCEMG) and Pooled (CCEP)
#' estimators for panel data with common factors (balanced or
#' unbalanced)
#' 
#' `pcce` is a function for the estimation of linear panel models by
#' the Common Correlated Effects Mean Groups or Pooled estimator,
#' consistent under the hypothesis of unobserved common factors and
#' idiosyncratic factor loadings. The CCE estimator works by
#' augmenting the model by cross-sectional averages of the dependent
#' variable and regressors in order to account for the common factors,
#' and adding individual intercepts and possibly trends.
#' 
#' @aliases pcce
#' @param formula a symbolic description of the model to be estimated,
#' @param object,x an object of class `"pcce"`,
#' @param data a `data.frame`,
#' @param subset see `lm`,
#' @param na.action see `lm`,
#' @param model one of `"mg"`, `"p"`, selects Mean Groups vs. Pooled
#'     CCE model,
#' @param index the indexes, see [pdata.frame()],
#' @param trend logical specifying whether an individual-specific
#'     trend has to be included,
#' @param digits digits,
#' @param width the maximum length of the lines in the print output,
#' @param type one of `"defactored"` or `"standard"`,
#' @param vcov a variance-covariance matrix furnished by the user or a function to calculate one,
#' @param \dots further arguments.
#' @return An object of class `c("pcce", "panelmodel")` containing:
#'     \item{coefficients}{the vector of coefficients,}
#'     \item{residuals}{the vector of (defactored) residuals,}
#'     \item{stdres}{the vector of (raw) residuals,}
#'     \item{tr.model}{the transformed data after projection on H,}
#'     \item{fitted.values}{the vector of fitted values,}
#'     \item{vcov}{the covariance matrix of the coefficients,}
#'     \item{df.residual}{degrees of freedom of the residuals,}
#'     \item{model}{a data.frame containing the variables used for the
#'     estimation,}
#'     \item{call}{the call,}
#'     \item{indcoef}{the matrix of individual coefficients from
#'     separate time series regressions,}
#'     \item{r.squared}{numeric, the R squared.}
#' @export
#' @importFrom MASS ginv
#' @author Giovanni Millo
#' @references
#'
#' \insertRef{kappesyam11}{plm}
#' 
#' @keywords regression
#' @examples
#' 
#' data("Produc", package = "plm")
#' ccepmod <- pcce(log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp, data = Produc, model="p")
#' ## IGNORE_RDIFF_BEGIN
#' summary(ccepmod)
#' summary(ccepmod, vcov = vcovHC) # use argument vcov for robust std. errors
#' ## IGNORE_RDIFF_END
#' 
#' ccemgmod <- pcce(log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp, data = Produc, model="mg")
#' ## IGNORE_RDIFF_BEGIN
#' summary(ccemgmod)
#' ## IGNORE_RDIFF_END
#' 
pcce <- function (formula, data, subset, na.action,
                  model = c("mg", "p"),
                   #residuals = c("defactored", "standard"),
                  index = NULL, trend = FALSE, ...) {
  
  ## Create a Formula object if necessary (from plm.R)
#  if (!inherits(formula, "pFormula")) formula <- pFormula(formula)
  if (!inherits(formula, "Formula")) formula <- as.Formula(formula)

  ## same as pggls but for effect, fixed at "individual" for compatibility
  ## ind for id, tind for time, k for K, coefnam for coef.names
  effect <- "individual"

  ## record call etc.
  model <- match.arg(model)
  model.name <- paste("cce", model, sep="")
  data.name <- paste(deparse(substitute(data)))
  cl <- match.call()
  plm.model <- match.call(expand.dots = FALSE)
  m <- match(c("formula", "data", "subset", "na.action", "effect",
      "model", "index"), names(plm.model), 0)
  plm.model <- plm.model[c(1L, m)]
  plm.model[[1L]] <- as.name("plm")
  ## change the 'model' in call
  plm.model$model <- "pooling"
  ## evaluates the call, modified with model = "pooling", inside the
  ## parent frame resulting in the pooling model on formula, data
  plm.model <- eval(plm.model, parent.frame())
  index <- unclass(attr(model.frame(plm.model), "index")) # unclass for speed
  ind  <- index[[1L]] ## individual index
  tind <- index[[2L]] ## time index
  ## set dimension variables
  pdim <- pdim(plm.model)
  balanced <- pdim$balanced
  nt <- pdim$Tint$nt
  Ti <- pdim$Tint$Ti
  T. <- pdim$nT$T
  n <- pdim$nT$n
  N <- pdim$nT$N
  ## set index names
  time.names <- pdim$panel.names$time.names
  id.names <- pdim$panel.names$id.names
  coef.names <- names(coef(plm.model))
  ## number of coefficients
  k <- length(coef.names)

  ## model data
  X <- model.matrix(plm.model)
  y <- model.response(model.frame(plm.model))

  ## det. *minimum* group numerosity
  t <- min(Ti) # ==  min(tapply(X[ , 1], ind, length))

  ## check min. t numerosity
    ## NB it is also possible to allow estimation if there *is* one group
    ## with t large enough and average on coefficients removing NAs
    ## Here we choose the explicit way: let estimation fail if we lose df
    ## but a warning would do...
  if(t < (k+1)) stop("Insufficient number of time periods")

  ## one regression for each group i in 1..n
  ## and retrieve coefficients putting them into a matrix
  ## (might be unbalanced => t1 != t2 but we don't care as long
  ## as min(t) > k+1)

  ## subtract intercept from parms number and names
  if(attr(terms(plm.model), "intercept")) {
      k <- k-1
      coef.names <- coef.names[-1L]
  }

  ## "pre-allocate" coefficients matrix for the n models
  tcoef <- matrix(NA_real_, nrow = k, ncol = n)

  ## pre-allocate residuals lists for individual regressions
  ## (lists allow for unbalanced panels)
  cceres <- vector("list", n)
  stdres <- vector("list", n)

  ## CCE by-group estimation

  ## must put the intercept into the group-invariant part!!
  ## so first drop it from X
  if(attr(terms(plm.model), "intercept")) {
      X <- X[ , -1L, drop = FALSE]
  }

  ## group-invariant part, goes in Hhat
    ## between-periods transformation (take means over groups for each t)
      Xm <- Between(X, effect = tind, na.rm = TRUE)
      ym <- as.numeric(Between(y, effect = "time", na.rm = TRUE))

      if(attr(terms(plm.model), "intercept")) {
        Hhat <- cbind(ym, Xm, 1L)
        } else {
          Hhat <- cbind(ym, Xm)
      }

      ## prepare XMX, XMy arrays
      XMX <- array(data = NA_real_, dim = c(k, k, n))
      XMy <- array(data = NA_real_, dim = c(k, 1L, n))

      ## hence calc. beta_i anyway because of vcov

      ## for each x-sect. i=1..n estimate (over t) the CCE for every TS
      ## as in KPY, eq. 15
      unind <- unique(ind)
      for(i in 1:n) {
          tX <- X[ind == unind[i], , drop = FALSE]
          ty <- y[ind == unind[i]]
          tHhat <- Hhat[ind == unind[i], , drop = FALSE]

          ## if 'trend' then augment the xs-invariant component
          if(trend) tHhat <- cbind(tHhat, 1:(dim(tHhat)[[1L]]))

          ## NB tHat, tMhat should be i-invariant
          tMhat <- diag(1, length(ty)) -
              tHhat %*% solve(crossprod(tHhat), t(tHhat))
          
          CP.tXtMhat <- crossprod(tX, tMhat)
          tXMX <- tcrossprod(CP.tXtMhat, t(tX))
          tXMy <- tcrossprod(CP.tXtMhat, t(ty))

          ## XMX_i, XMy_i
          XMX[ , , i] <- tXMX
          XMy[ , , i] <- tXMy

          ## single CCE coefficients
          tb <- ginv(tXMX) %*% tXMy  #solve(tXMX, tXMy)
          ## USED A GENERALIZED INVERSE HERE BECAUSE OF PBs WITH ECM SPECS
          ## Notice remark in Pesaran (2006, p.977, between (27) and (28))
          ## that XMX.i is invariant to the choice of a g-inverse for H'H
          tcoef[ , i] <- tb

          ## cce (defactored) residuals as M_i(y_i - X_i * bCCEMG_i)
          tytXtb <- ty - tcrossprod(tX, t(tb))
          cceres[[i]] <- tcrossprod(tMhat, t(tytXtb))
          ## std. (raw) residuals as y_i - X_i * bCCEMG_i - a_i
          ta <- mean(ty - tX)
          stdres[[i]] <- tytXtb - ta
        }

  ## module for making transformed data My, MX for vcovHC use
    ## (NB M is symmetric)
    ## Some redundancy because this might be moved to model.matrix.pcce

    ## initialize
    tX1 <- X[ind == unind[1L], , drop = FALSE]
    ty1 <- y[ind == unind[1L]]
    tHhat1 <- Hhat[ind == unind[1L], , drop = FALSE]

    ## if 'trend' then augment the xs-invariant component
    if(trend) tHhat1 <- cbind(tHhat1, 1:(dim(tHhat)[[1L]]))

    ## NB tHat, tMhat should be i-invariant (but beware of unbalanced)
    tMhat1 <- diag(1, length(ty1)) -
        tHhat1 %*% solve(crossprod(tHhat1), t(tHhat1))
    MX <- crossprod(tMhat1, tX1)
    My <- crossprod(tMhat1, ty1)
    for(i in 2:n) {
        tX <- X[ind == unind[i], , drop = FALSE]
        ty <- y[ind == unind[i]]
        tHhat <- Hhat[ind == unind[i], , drop = FALSE]

        ## if 'trend' then augment the xs-invariant component
        if(trend) tHhat <- cbind(tHhat, 1:(dim(tHhat)[[1L]]))

        ## NB tHat, tMhat should be i-invariant
        tMhat <- diag(1, length(ty)) -
            tHhat %*% solve(crossprod(tHhat), t(tHhat))
        tMX <- crossprod(tMhat, tX)
        tMy <- crossprod(tMhat, ty)

        MX <- rbind(MX, tMX)
        My <- c(My, tMy)
    }

    ## checks
    ## MX <<- MX
    ## My <<- My

    ## ALT:
    ## MXa <<- kronecker(diag(n), tMhat1) %*% X
    ## Mya <<- kronecker(diag(n), tMhat1) %*% y
    ## very same result, less efficient

  ## end data module

    ## CCEMG coefs are averages across individual regressions
    ## (here: coefs of xs-variants only!)
    coefmg <- rowMeans(tcoef) # was: apply(tcoef, 1, mean)

    ## make matrix of cross-products of demeaned individual coefficients
    Rmat <- array(data = NA_real_, dim = c(k, k, n))

    ## make b_i - b_CCEMG
    demcoef <- tcoef - coefmg # coefmg gets recycled n times by column

    ## calc. coef and vcov according to model
    switch(model,
        "mg" = {
            ## assign beta CCEMG
            coef <- coefmg
            for(i in 1:n) Rmat[ , , i] <- outer(demcoef[ , i], demcoef[ , i])
            vcov <- 1/(n*(n-1)) * rowSums(Rmat, dims = 2L) # == 1/(n*(n-1)) * apply(Rmat, 1:2, sum), but rowSums(., dims = 2L)-construct is way faster
        },
           
        "p" = {
            ## calc beta_CCEP
            sXMX <- rowSums(XMX, dims = 2L) # == apply(XMX, 1:2, sum), but rowSums(., dims = 2L)-construct is way faster
            sXMy <- rowSums(XMy, dims = 2L) # == apply(XMy, 1:2, sum), but rowSums(., dims = 2L)-construct is way faster
            coef <- solve(sXMX, sXMy)
    
            ## calc CCEP covariance:
            psi.star <- 1/N * sXMX
    
            for(i in 1:n) Rmat[ , , i] <- XMX[ , , i] %*%
                outer(demcoef[ , i], demcoef[ , i]) %*% XMX[ , , i]
            ## summing over the n-dimension of the array we get the
            ## covariance matrix of coefs
            R.star <- 1/(n-1) * rowSums(Rmat, dims = 2L) * 1/(t^2) # rowSums(Rmat, dims = 2L) faster than == apply(Rmat, 1:2, sum)
    
            Sigmap.star <- solve(psi.star, R.star) %*% solve(psi.star)
            vcov <- Sigmap.star/n
    
            ## calc CCEP residuals both defactored and raw
            for(i in 1:n) {
                ## must redo all this because needs b_CCEP, which is
                ## not known at by-groups step
                tX <- X[ind == unind[i], , drop = FALSE]
                ty <- y[ind == unind[i]]
                tHhat <- Hhat[ind == unind[i], , drop = FALSE]
    
                ## if 'trend' then augment the xs-invariant component
                if(trend) tHhat <- cbind(tHhat, 1:(dim(tHhat)[[1L]]))
    
                ## NB tHat, tMhat should be i-invariant (but for the
                ## group size if unbalanced)
                tMhat <- diag(1, length(ty)) -
                    tHhat %*% solve(crossprod(tHhat), t(tHhat))
    
                ## cce residuals as M_i(y_i - X_i * bCCEP)
                tytXcoef <- ty - tcrossprod(tX, t(coef))
                cceres[[i]] <- tcrossprod(tMhat, t(tytXcoef))
                ## std. (raw) residuals as y_i - X_i * bCCEMG_i - a_i
                ta <- mean(ty - tX)
                stdres[[i]] <- tytXcoef - ta
            }
    })

    ## calc. measures of fit according to model type
    switch(model,
        "mg" = {

            ## R2 as in HPY 2010: sigma2ccemg = average (over n) of variances
            ## of defactored residuals
            ## (for unbalanced panels, each variance is correctly normalized
            ## by group dimension T.i)
            ##
            ## If balanced, would simply be
            ## sum(unlist(cceres)^2)/(n*(T.-2*k-2))
    
            ## pre-allocate list for individual CCEMG residual variances
            sigma2cce.i <- vector("list", n)
            ## average variance of defactored residuals sigma2ccemg as in
            ## Holly, Pesaran and Yamagata, (3.14)
            for(i in 1:n) {
                sigma2cce.i[[i]] <- crossprod(cceres[[i]])*
                    1/(length(cceres[[i]])-2*k-2)
            }
            sigma2cce <- 1/n*sum(unlist(sigma2cce.i, use.names = FALSE))
        },
           
        "p" = {
            ## variance of defactored residuals sigma2ccep as in Holly,
            ## Pesaran and Yamagata, (3.15)
            sigma2cce <- 1/(n*(T.-k-2)-k)*
                sum(vapply(cceres, crossprod, FUN.VALUE = 0.0, USE.NAMES = FALSE))
            ## is the same as sum(unlist(cceres)^2)
    })

    ## calc. overall R2, CCEMG or CCEP depending on 'model'
    sigma2.i <- vector("list", n)
    for(i in 1:n) {
          ty <- y[ind == unind[i]]
          sigma2.i[[i]] <- as.numeric(crossprod((ty-mean(ty))))/(length(ty)-1)
      }
    sigma2y <- mean(unlist(sigma2.i, use.names = FALSE))
    r2cce <- 1 - sigma2cce/sigma2y

    ## allow outputting different types of residuals
    stdres <- unlist(stdres)
    residuals <- unlist(cceres)

    ## add transformed data (for now a simple list)
    tr.model <- list(y = My, X = MX)
    ## so that if the model is ccepmod,
    ## > lm(ccepmod$tr.model[["y"]] ~ ccepmod$tr.model[["X"]]-1)
    ## reproduces the model results

    ## Final model object:
    ## code as in pggls, differences:
    ## - here there is no 'sigma'
    ## - there are two types of residuals
    ## - transformed data My, MX are included for vcovHC usage
    df.residual <- nrow(X) - ncol(X)
    fitted.values <- y - residuals
    coef <- as.numeric(coef)
    names(coef) <- rownames(vcov) <- colnames(vcov) <- coef.names
    dimnames(tcoef) <- list(coef.names, id.names)
    pmodel <- attr(plm.model, "pmodel")
    pmodel$model.name <- model.name
    pccemod <- list(coefficients  = coef,
                    residuals     = residuals,
                    stdres        = stdres,
                    tr.model      = tr.model,
                    fitted.values = fitted.values,
                    vcov          = vcov,
                    df.residual   = df.residual,
                    model         = model.frame(plm.model),
                    indcoef       = tcoef,
                    r.squared     = r2cce,
                    #cceres   = as.vector(cceres),
                    #ccemgres = as.vector(ccemgres),
                    formula       = formula,
                    call          = cl)
    pccemod <- structure(pccemod, pdim = pdim, pmodel = pmodel)
    class(pccemod) <- c("pcce", "panelmodel")
    pccemod
}

#' @rdname pcce
#' @export
summary.pcce <- function(object, vcov = NULL, ...){
  pmodel <- attr(object, "pmodel")
  vcov_arg <- vcov
  std.err <- if (!is.null(vcov_arg)) {
    if (is.matrix(vcov_arg))   rvcov <- vcov_arg
    if (is.function(vcov_arg)) rvcov <- vcov_arg(object)
    sqrt(diag(rvcov))
  } else {
    sqrt(diag(stats::vcov(object)))
  }
  b <- object$coefficients
  z <- b/std.err
  p <- 2*pnorm(abs(z), lower.tail = FALSE)
  CoefTable <- cbind(b, std.err, z, p)
  colnames(CoefTable) <- c("Estimate", "Std. Error", "z-value", "Pr(>|z|)")
  object$CoefTable <- CoefTable
  y <- object$model[[1L]]
  object$tss <- tss(y)
  object$ssr <- as.numeric(crossprod(residuals(object)))
  object$rsqr <- object$r.squared #1-object$ssr/object$tss
  ## add some info to summary.pcce object 
  # robust vcov (next to "normal" vcov)
  if (!is.null(vcov_arg)) {
    object$rvcov <- rvcov
    rvcov.name <- paste0(deparse(substitute(vcov)))
    attr(object$rvcov, which = "rvcov.name") <- rvcov.name 
  }
  class(object) <- c("summary.pcce")
  return(object)
}

#' @rdname pcce
#' @export
print.summary.pcce <- function(x, digits = max(3, getOption("digits") - 2), width = getOption("width"), ...){
  pmodel <- attr(x, "pmodel")
  pdim <- attr(x, "pdim")
#  formula <- pmodel$formula
  model.name <- pmodel$model.name
  cat("Common Correlated Effects ")
  cat(paste(model.pcce.list[model.name], "\n", sep = ""))
  if (!is.null(x$rvcov)) {
    cat("\nNote: Coefficient variance-covariance matrix supplied: ", attr(x$rvcov, which = "rvcov.name"), "\n", sep = "")
  }
  cat("\nCall:\n")
  print(x$call)
  cat("\n")
  print(pdim)
  cat("\nResiduals:\n")
  print(sumres(x)) # was until rev. 1178: print(summary(unlist(residuals(x))))
  cat("\nCoefficients:\n")
  printCoefmat(x$CoefTable, digits = digits)
  cat(paste("Total Sum of Squares: ",    signif(x$tss,digits), "\n", sep=""))
  cat(paste("Residual Sum of Squares: ", signif(x$ssr,digits), "\n", sep=""))
  cat(paste("HPY R-squared: ",           signif(x$rsqr,digits),"\n", sep=""))
  invisible(x)
}

#' @rdname pcce
#' @export
residuals.pcce <- function(object,
                           type = c("defactored", "standard"),
                           ...) {
    ## special resid() method for pcce: allows to extract either
    ## defactored residuals (default) or raw residuals
    defres <- pres(object)
    switch(match.arg(type),
           "standard" = {
               ## add panel features and names from 'defres'
               residuals <- add_pseries_features(object$stdres, index(defres))
               names(residuals) <- names(defres)
              },
           "defactored" = { residuals <- defres }
           )
    return(residuals)
}

#' @rdname pcce
#' @export
model.matrix.pcce <- function(object, ...) {
    object$tr.model$X
}

#' @rdname pcce
#' @export
pmodel.response.pcce <- function(object, ...) {
    object$tr.model$y
}
