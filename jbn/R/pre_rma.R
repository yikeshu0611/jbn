#' preprocess for rma
#'
#' @param raw raw
#'
#' @return rma results
#' @export
#'
pre_rma <- function(raw){
    message('mask from affy::rma')
    if (class(raw) != 'AffyBatch'){
        AffyBatch <- oligo2affy(raw)
    }else{
        AffyBatch <- raw
    }
    affy::rma(AffyBatch)
}
