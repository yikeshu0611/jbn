#' qc stat
#'
#' @param raw raw
#'
#' @return plot
#' @export
#'
qa_RNAdeg.qc <- function(raw){
    message('mask from simpleaffy::qc')
    if (class(raw) != 'AffyBatch'){
        AffyBatch <- oligo2affy(raw)
    }else{
        AffyBatch <- raw
    }
    GSMs <- stringr::str_extract(sampleNames(AffyBatch),'GSM[0-9]{1,}')
    sampleNames(AffyBatch) <- GSMs
    if (AffyBatch@cdfName == 'HuGene-1_0-st-v1'){
        message('simpleaffy::qc doest not work on HuGene-1_0-st-v1')
    }else{
        qc <- simpleaffy::qc(AffyBatch)
        simpleaffy::plot(qc)

    }
}
