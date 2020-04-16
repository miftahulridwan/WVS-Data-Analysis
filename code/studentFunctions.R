### Title:    Functions for Student Use
### Author:   Kyle M. Lang
### Created:  2017-08-24
### Modified: 2019-10-02

### USAGE:
### 1) Save this script and the "studentSubroutines.R" script in your current
###    working directory
### 2) Source this script.
### 3) The functions defined below are now available for use in your current R
###    session.

source("studentSubroutines.R", chdir = TRUE)
library(MASS)

###--------------------------------------------------------------------------###

## Function to find outliers via the boxplot method

## Input:
##   x     = Vector of numeric data to check for outliers
##   iCoef = The coefficient that is multiplied by the inner quartile range 
##           (IQR) to define the inner fences
##   oCoef = The coefficient that is multiplied by the IQR to define the outer
##           fences

## Output: A two-element list containing the indices of any possible and
##         probable outliers that were detected.

bpOutliers <- function(x, iCoef = 1.5, oCoef = 3.0) {
    ## Compute inner and outer fences:
    iFen <- boxplot.stats(x, coef = iCoef)$stats[c(1, 5)]
    oFen <- boxplot.stats(x, coef = oCoef)$stats[c(1, 5)]
    
    ## Return the row indices of flagged 'possible' and 'probable' outliers:
    list(possible = which(x < iFen[1] | x > iFen[2]),
         probable = which(x < oFen[1] | x > oFen[2])
         )
}

###--------------------------------------------------------------------------###

## Function to find outliers via the MAD method

## Input:
##   x     = Vector of numeric data to check for outliers
##   cut   = The value of the (absolute) test statistic above which an
##           observation is flagged as an outlier
##   na.rm = A logical switch. Should missing values be excluded when estimating
##           the median and MAD?

## Output: A vector containing the indices of any possible outliers detected.

madOutliers <- function(x, cut = 2.5, na.rm = TRUE) {
    ## Compute the median and MAD of x:
    mX   <- median(x, na.rm = na.rm)
    madX <- mad(x, na.rm = na.rm)
    
    ## Return row indices of observations for which |T_MAD| > cut:
    which(abs(x - mX) / madX > cut)
}

###--------------------------------------------------------------------------###

## Function to find multivariate outliers via (robust) Mahalanobis distance

## Input:
##   data     = A data.frame to check for multivariate outliers
##   critProb = The quantile of the chi-squared distribution used to define the
##              critical value against which the Mahalanobis distances will be
##              compared to flag outliers
##   statType = The method of estimation used by "MASS::cov.rob" to estimate the
##              mean vector and covariance matrix.
##              Possible values are:
##                - "mve"       = minimum volume ellipsoid method
##                - "mcd"       = minimum covariance determinant method
##                - "classical" = non-robust product-moment estimation
##              See the documentation of "cov.rob" from the MASS package for
##              more details.
##   ratio    = The minimum proportion of observations that will be used to
##              estimate the robust mean vector and covariance matrix.
##   seed     = A random number seed.

## Output: A vector of row indices for any observations flagged as outliers.

mdOutliers <-
    function(data, critProb, statType = "mcd", ratio = 0.75, seed = NULL)
{
    ## Set a seed, if one is provided:
    if(!is.null(seed)) set.seed(seed)
    
    ## Compute (robust) estimates of the mean and covariance matrix:
    stats <- cov.rob(x             = data,
                     quantile.used = floor(ratio * nrow(data)),
                     method        = statType)
    
    ## Compute robust squared Mahalanobis distances
    md <- mahalanobis(x = data, center = stats$center, cov = stats$cov)
    
    ## Find the cutoff value:
    crit <- qchisq(critProb, df = ncol(data))
    
    ## Return row indices of flagged observations:
    which(md > crit)
}

###--------------------------------------------------------------------------###

## Function to winsorize univariate outliers

## Input:
##   x  = A numeric vector of data to be winsorized
##   ol = A numeric vector giving the indices of 'x' containing outliers

## Output: A modified version of "x" where the data in every entry represented
##         in the "ol" vector is replaced by the closest legal datum.

winsorizeOutliers <- function(x, ol) {
    ## Get the outlying values:
    y <- x[ol]
    
    ## Get a sorted list of unique data values:
    ux <- sort(unique(x))
    
    ## Winsorize outliers in the positive tail:
    flag <- y > median(x)
    if(any(flag)) {
        lub        <- ux[which(ux == min(y[flag])) - 1]
        x[x > lub] <- lub
    }
    
    ## Winsorize outliers in the negative tail:
    flag <- y < median(x)
    if(any(flag)) {
        glb        <- ux[which(ux == max(y[flag])) + 1]
        x[x < glb] <- glb
    }
    
    x # Return the winsorized data
}

###--------------------------------------------------------------------------###

## Function to do K-fold cross-validation with lm()

## Input:
##   data   = A data.frame to use for model estimation
##   models = A list of model formulas (given as character strings) representing
##            the models to be compared via cross-validation
##   K      = The number of folds to use
##   names  = A character vector used to name the output. These names should
##            match the ordering of the models given in the "models" argument.
##   seed   = A random number seed

## Output: A (named) vector containing the cross-validation errors for each
##         model listed in the "models" argument

cv.lm <- function(data, models, K = 10, names = NULL, seed = NULL) {
    ## Set seed, if necessary:
    if(!is.null(seed)) set.seed(seed)
    
    ## Create a partition vector:
    part <- sample(rep(1 : K, ceiling(nrow(data) / K)))[1 : nrow(data)]
    
    ## Apply over candidate models:
    cve <- sapply(models, getCve, data = data, K = K, part = part)
    
    ## Name output:
    if(!is.null(names))          names(cve) <- names
    else if(is.null(names(cve))) names(cve) <- paste("Model", 1 : length(cve))
    
    cve
}

###--------------------------------------------------------------------------###

## A summary method for cell-means coded lm models.
## This function will correct the R^2 and F-stats from the usual summary.lm()
## when applied to an object with cell-means coded predictors.

## Input:  A fitted lm object
## Output: The correctly-computed summary object

summary.cellMeans <- function(obj) {
    ## Get broken summaries:
    s0  <- summary.lm(obj)
    av0 <- anova(obj)

    ## Extract model info:
    y  <- obj$model[ , 1]
    df <- obj$rank - 1

    ## Compute correct measures of variability:
    ss <- crossprod(obj$fitted.values - mean(y))
    ms <- ss / df 

    ## Compute correct stats:
    r2  <- as.numeric(ss / crossprod(y - mean(y)))
    r2a <- as.numeric(1 - (1 - r2) * ((length(y) - 1) / obj$df.residual))
    f   <- as.numeric(ms / av0["Residuals", "Mean Sq"])

    ## Replace broken stats:
    s0$r.squared           <- r2
    s0$adj.r.squared       <- r2a
    s0$fstatistic[c(1, 2)] <- c(f, df)

    s0 # Return corrected summary
}

###--------------------------------------------------------------------------###

## Function to automatically fix EC names

## Input: A factor with an effects-coded contrasts attribute
## Ouput: A modified version of the input with appropriate column names for the
##        contrasts matrix

fixEcNames <- function(x) {
    tmp                    <- contrasts(x)
    colnames(contrasts(x)) <- rownames(tmp)[rowSums(tmp) > 0]
    x
}

###--------------------------------------------------------------------------###

## Function to automatically change the omitted group for unweighted effects
## codes

## Input: A factor with an effects-coded contrasts attribute
## Ouput: A modified version of the input factor with the levels re-ordered to
##        ensure a different omitted group

changeOmitted <- function(x) relevel(x, ref = levels(x)[nlevels(x)])

###--------------------------------------------------------------------------###
