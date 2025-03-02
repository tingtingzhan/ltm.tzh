

# methods(class = 'cronbachAlpha') 
# only ?ltm:::print.cronbachAlpha

#' @importFrom stats coef
# @export coef.cronbachAlpha
#' @export
coef.cronbachAlpha <- function(object, ...) object$alpha

#' @importFrom stats confint
# @export confint.cronbachAlpha
#' @export
confint.cronbachAlpha <- function(object, ...) {
  ci <- object$ci
  if (!length(ci)) stop('re-run ltm::cronbach.alpha with CI = TRUE')
  return(ci) # no need to turn to `dim = 1:2` 'matrix'
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
#' cronbach.alpha(LSAT, CI = TRUE, B = 500) |>
#'  cut()
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
#' @param x an object of class `'cronbachAlpha'`, 
#' returned from function \link[ltm]{cronbach.alpha}
#' 
#' @name S3_cronbachAlpha
#' @export endpoint.cronbachAlpha
#' @export
endpoint.cronbachAlpha <- function(x) 'Questionaire'

#' @rdname S3_cronbachAlpha
#' @export .pval.cronbachAlpha
#' @export
.pval.cronbachAlpha <- function(x) NA_real_

#' @rdname S3_cronbachAlpha
#' @export estName.cronbachAlpha
#' @export
estName.cronbachAlpha <- function(x) 'Cronbach\'s \u03b1'

#' @rdname S3_cronbachAlpha
#' @export
note_.cronbachAlpha <- function(x) x |> cut.cronbachAlpha() |> as.character.factor()


#cibeta.cronbachAlpha <- function(object, ...) {
#  
#  new(
#    Class = 'cibeta', 
#    betap = array(cbind(object$alpha, NA_real_), dim = 1:2, dimnames = list(object$name, NULL)),
#    null.value = NA_real_,
#    CI = list('.95' = array(object$ci, dim = 1:2)),
#    estName = 'Cronbach\'s \u03b1',
#    note = as.character(cut.cronbachAlpha(object$alpha)),
#    nobs = sprintf(fmt = 'n=%d', object$n),
#    endpoint = 'Questionaire'
#  )  
#  
#}



#' @title Sprintf.cronbachAlpha
#' 
#' @param model ..
#' 
#' @param ... ..
#' 
#' @export
Sprintf.cronbachAlpha <- function(model, ...) {
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


# @export
#cibeta.cronbachAlpha_list <- function(object, ...) {
#  nm1 <- names(object)
#  nm2 <- unname(vapply(object, FUN = `[[`, 'name', FUN.VALUE = NA_character_))
#  if (!length(nm1)) {
#    nm <- nm2
#  } else if (all(nzchar(nm1))) {
#    nm <- nm1
#  } else {
#    nm <- nm1
#    nm[id] <- nm2[id <- !nzchar(nm1)]
#  }
  
#  ret <- do.call(rbind.cibeta, args = lapply(object, FUN = cibeta.cronbachAlpha))
#  rownames(ret@betap) <- nm
#  return(ret)
#}



# @export rmd_.cronbachAlpha_list
# @export
#rmd_.cronbachAlpha_list <- function(x, xnm, ...) {
#  return(c(
#    Sprintf.cronbachAlpha(),
#    '```{r results = \'asis\'}', 
#    sprintf(fmt = 'as_flextable.cibeta(cibeta.cronbachAlpha_list(%s))', xnm),
#    '```', 
#    '<any-text>'
#  ))
#}


