

# methods(class = 'cronbachAlpha') 
# only ?ltm:::print.cronbachAlpha

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
# @export nobs.cronbachAlpha
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
#' @param breaks,labels,right,include.lowest,ordered_result,... see function \link[base]{cut.default}
#' 
#' @examples
#' library(ltm)
#' m = cronbach.alpha(LSAT, CI = TRUE, B = 500) 
#' m |> cut()
#' @references
#' \url{https://www.researchgate.net/figure/Range-of-reliability-and-its-coefficient-of-Cronbachs-alpha_tbl1_326698967}
#' @export cut.cronbachAlpha
#' @export
cut.cronbachAlpha <- function(
    x,
    breaks = c(-Inf, 5:9/10, Inf), 
    labels = c('unacceptable', 'poor', 'questionable', 'acceptable', 'good', 'excellent'),
    right = FALSE, include.lowest = TRUE, ordered_result = TRUE,
    ...
) {
  cut.default(
    x$alpha, breaks = breaks, labels = labels, 
    right = right, include.lowest = include.lowest, 
    ordered_result = ordered_result, ...
  )
}



#' @title S3 methods for `cronbachAlpha` 
#' 
#' @description
#' Additional S3 methods for `'cronbachAlpha'` (`cibeta`).
#' 
#' @param x an object of class `'cronbachAlpha'`, 
#' returned from function \link[ltm]{cronbach.alpha}
#' 
#' @name S3_cronbachAlpha
#' @export
endpoint.cronbachAlpha <- function(x) quote(Questionaire)

#' @rdname S3_cronbachAlpha
#' @importFrom stats setNames
#' @export
.pval.cronbachAlpha <- function(x) setNames(NA_real_, nm = x$name)

#' @rdname S3_cronbachAlpha
#' @export
estnm.cronbachAlpha <- function(x) 'Cronbach\'s \u03b1'

#' @rdname S3_cronbachAlpha
#' @export
note_.cronbachAlpha <- function(x) x |> cut.cronbachAlpha() |> as.character.factor()


#' @rdname S3_cronbachAlpha
#' @export
Sprintf.cronbachAlpha <- function(x) {
  'Cronbach\'s $\\alpha$ is calculated using <u>**`R`**</u> package <u>**`ltm`**</u>.
  The levels are unacceptable $(\\alpha<.5)$, poor $(.5\\leq\\alpha<.6)$, questionable $(.6\\leq\\alpha<.7)$, acceptable $(.7\\leq\\alpha<.8)$, good $(.8\\leq\\alpha<.9)$, excellent $(\\alpha\\geq.9)$.'
}


#' @title rmd_.cronbachAlpha
#' 
#' @param x ..
#' 
#' @param xnm ..
#' 
#' @param ... ..
#' 
#' @export
rmd_.cronbachAlpha <- function(x, xnm, ...) {
  return(c(
    Sprintf.cronbachAlpha(),
    '```{r results = \'asis\'}', 
    sprintf(fmt = 'as_flextable.cibeta(cibeta(%s))', xnm),
    '```', 
    '<any-text>'
  ))
}



