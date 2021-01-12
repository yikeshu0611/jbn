#' Affy RNA degration plot
#'
#' @param raw raw
#'
#' @return plot
#' @export
#'
qa_RNAdeg.pot <- function(raw){
    message('mask from affy::plotAffyRNAdeg')
    if (class(raw) != 'AffyBatch'){
        AffyBatch <- oligo2affy(raw)
    }else{
        AffyBatch <- raw
    }
    GSMs <- stringr::str_extract(sampleNames(AffyBatch),'GSM[0-9]{1,}')
    sampleNames(AffyBatch) <- GSMs
    cols <- rainbow(length(GSMs))

    deg <- AffyRNAdeg(AffyBatch)
    mai <- par()$mai
    par(mai=c(1.02,0.82,0.82,1.7))
    plotAffyRNAdeg(deg,cols = cols)
    legend(x = par()$usr[2]+(par()$usr[2]-par()$xaxp[2])/5,
           y=par()$usr[4],
           legend = GSMs,col = cols,lty = 1,bty='n',xpd = T)
    message("RNA的降解是从5端到3端降解的\n一般5端的信号弱一些，3端的信号强一些\n如果3端的信号明显强于5端，说明这个芯片降解的比较厉害\n数据不在统一水平")
    par(mai=mai)
}
