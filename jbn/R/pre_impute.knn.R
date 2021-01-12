#' impute missing value by knn
#'
#' @param raw raw
#'
#' @return no missing value
#' @export
#'
pre_impute.knn <- function(raw){
    if (!anyNA(exprs(raw))){
        message('no missing value')
        raw
    }else{
        message('mask from impute::impute.knn')
        k <- impute::impute.knn(exprs(raw),maxp = nrow(exprs(raw)))
        exprs(raw) <- k$data
        raw
    }
}
