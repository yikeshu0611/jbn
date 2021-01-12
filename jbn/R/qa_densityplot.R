#' histgram plot
#'
#' @param raw raw data
#'
#' @return
#' @export
#'
qa_densityplot <- function(raw){
    message('mask from affy::hist')
    if (! class(raw) %in% c('ExpressionSet','AffyBatch')){
        AffyBatch <- oligo2affy(raw)
    }else{
        AffyBatch <- raw
    }
    GSMs <- stringr::str_extract(sampleNames(AffyBatch),'GSM[0-9]{1,}')
    cols=rainbow(length(GSMs))
    sampleNames(AffyBatch) <- GSMs
    mai <- par()$mai
    par(mai=c(1.02,0.82,0.82,1.7))
    affy::hist(AffyBatch,col=cols,lty=1)
    legend(x = par()$usr[2]+(par()$usr[2]-par()$xaxp[2])/5,
           y=par()$usr[4],
           legend = GSMs,
           col = cols,
           bty = 'n',
           lty = 1,
           xpd = TRUE)
    par(mai=mai)
}
