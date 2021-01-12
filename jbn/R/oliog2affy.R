#' Transform results of read.celfiles() in oligo package to AffyBatch format
#'
#' @param oligoRaw results of read.celfiles() in oligo package
#'
#' @return an S4 object with AffyBatch format
#' @export
#' @examples
#' \donttest{
#' library(oligo)
#' cels <- list.files()
#' gse118370_raw <- read.celfiles(cels)
#' oliog2affy(gse118370_raw)
#' }
oligo2affy <- function(oligoRaw){
    # setClass('AffyBatch',slots = list(cdfName='character',
    #                                   nrow='integer',
    #                                   ncol='integer',
    #                                   assayData='environment',
    #                                   phenoData='AnnotatedDataFrame',
    #                                   featureData='AnnotatedDataFrame',
    #                                   experimentData='MIAME',
    #                                   annotation='character',
    #                                   protocolData='AnnotatedDataFrame',
    #                                   .__classVersion__='Versions'
    # ))
    new('AffyBatch',
        cdfName=affyio::read.celfile.header(Biobase::sampleNames(oligoRaw))$cdfName,
        nrow=affyio::read.celfile.header(Biobase::sampleNames(oligoRaw))$`CEL dimensions`['Rows'],
        ncol=affyio::read.celfile.header(Biobase::sampleNames(oligoRaw))$`CEL dimensions`['Cols'],
        assayData=oligoRaw@assayData,
        phenoData=oligoRaw@phenoData,
        featureData=oligoRaw@featureData,
        experimentData=oligoRaw@experimentData,
        annotation=oligoRaw@annotation,
        protocolData=oligoRaw@protocolData,
        .__classVersion__=oligoRaw@.__classVersion__
    )

}


