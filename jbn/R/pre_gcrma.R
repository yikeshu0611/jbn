#' preprocess for gcrma
#'
#' @param raw raw
#'
#' @return gcrma results
#' @export
#'
pre_gcrma <- function(raw){
    message('mask from gcrma::gcrma')
    if (class(raw) != 'AffyBatch'){
        AffyBatch <- oligo2affy(raw)
    }else{
        AffyBatch <- raw
    }
    if (AffyBatch@cdfName == 'HuGene-1_0-st-v1'){
        message('gcrma::gcrma doest not work on HuGene-1_0-st-v1')
    }else{
        gcrma::gcrma(AffyBatch)
    }
}
