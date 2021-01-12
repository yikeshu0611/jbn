#' boxplot
#'
#' @param raw raw data
#'
#' @return boxplot
#' @export
#'
qa_boxplot <- function(raw){
    message('mask from affy::boxplot')
    if (! class(raw) %in% c('ExpressionSet','AffyBatch')){
        AffyBatch <- oligo2affy(raw)
    }else{
        AffyBatch <- raw
    }
    mai <- par()$mai
    par(mai=c(2,0.82,0.3,0.42))
    GSMs <- stringr::str_extract(sampleNames(AffyBatch),'GSM[0-9]{1,}')
    sampleNames(AffyBatch) <- GSMs

    affy::boxplot(AffyBatch,las=2,col='gray',main=NA)
    par(mai=mai)
}
