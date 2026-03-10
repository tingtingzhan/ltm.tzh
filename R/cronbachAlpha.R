

# methods(class = 'cronbachAlpha') 
# only ?ltm:::print.cronbachAlpha


#' @title `cronbachAlpha` Object
#' 
#' @examples
#' list(
#'  LSAT = cronbach.alpha(LSAT, CI = TRUE, B = 500)
#' ) |> fastmd::render2html()
#' 
#' @name cronbachAlpha
NULL




#' @importFrom stats coef setNames
# @export coef.cronbachAlpha
#' @export
coef.cronbachAlpha <- function(object, ...) setNames(object$alpha, nm = object$name)


#' @importFrom stats confint
# @export confint.cronbachAlpha
#' @export
confint.cronbachAlpha <- function(object, ...) {
  ci <- object$ci
  if (!length(ci)) stop('re-run ltm::cronbach.alpha with CI = TRUE')
  array(ci, dim = 1:2, dimnames = list(object$name, names(ci)))
}

#' @importFrom stats nobs
#' @export
nobs.cronbachAlpha <- function(object, ...) object[['n']]
  

#' @title Categorize `cronbachAlpha`
#' 
#' @description
#' An S3 method dispatch of generic function \link[base]{cut}.
#' 
#' @param x an object of class `'cronbachAlpha'`, 
#' returned from function \link[ltm]{cronbach.alpha}
#' 
#' @param ordered_result see function \link[base]{cut.default}
#' 
#' @param ... S3 method dispatch place holder, not used
#' 
#' @examples
#' library(ltm)
#' cronbach.alpha(LSAT, CI = TRUE, B = 500) |> cut()
#' @references
#' \url{https://www.researchgate.net/figure/Range-of-reliability-and-its-coefficient-of-Cronbachs-alpha_tbl1_326698967}
#' @export cut.cronbachAlpha
#' @export
cut.cronbachAlpha <- function(x, ordered_result = TRUE, ...) {
  cut.default(
    x$alpha, 
    breaks = c(-Inf, 5:9/10, Inf), 
    labels = c('unacceptable', 'poor', 'questionable', 'acceptable', 'good', 'excellent'), 
    right = FALSE, include.lowest = TRUE, 
    ordered_result = ordered_result
  )
}



#' @importFrom ecip endpoint
#' @export
endpoint.cronbachAlpha <- function(x) quote(Questionaire)

#' @importFrom ecip estnm
#' @export
estnm.cronbachAlpha <- function(x) 'Cronbach\'s \u03b1'





#' @importFrom fastmd md_ md_int
#' @importClassesFrom fastmd md_lines
#' @export
md_.cronbachAlpha <- function(x, xnm, ...) {
  
  lev <- c(
    'unacceptable $(\\alpha<.5)$', 
    'poor $(.5\\leq\\alpha<.6)$', 
    'questionable $(.6\\leq\\alpha<.7)$', 
    'acceptable $(.7\\leq\\alpha<.8)$', 
    'good $(.8\\leq\\alpha<.9)$', 
    'excellent $(\\alpha\\geq.9)$'
  )
  
  id <- x |>
    cut.cronbachAlpha() |>
    unclass()
  
  lev[id] <- lev[id] |>
    sprintf(fmt = '[%s]{style=\"background-color: #FFFF00\"}')
  
  z1 <- lev |>
    paste(collapse = ', ') |>
    sprintf(fmt = '[@Cronbach51\'s $\\alpha$](https://en.wikipedia.org/wiki/Cronbach%%27s_alpha), categorized into %s, is calculated using <u>**`R`**</u> package <u>**`ltm`**</u>.') |>
    new(Class = 'md_lines', package = 'ltm', bibentry = .cronbach51())
  
  z2 <- md_int(x = x, xnm = xnm, engine = 'print', ...)
  
  c(z1, z2)
  
}






