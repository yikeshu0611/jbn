#' background correct using rma from affy package
#'
#' @param raw raw
#'
#' @return background correct data
#' @export
#'
pre_bg.correct <- function(raw){
    message('mask from affy::bg.correct.rma')
    if (class(raw) != 'AffyBatch'){
        AffyBatch <- oligo2affy(raw)
    }else{
        AffyBatch <- raw
    }
    affy::bg.correct.rma(AffyBatch)
}
