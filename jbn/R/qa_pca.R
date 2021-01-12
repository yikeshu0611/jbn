#' pca plot
#'
#' @param raw raw
#'
#' @return plot
#' @export
#'
qa_pca <- function(raw){
    message('mask from factoextra::fviz_pca_var')
    sampleNames(raw) <- GSM_names(raw)
    pr=prcomp(exprs(raw),scale = TRUE)
    pheno <- raw@phenoData@data
    colnames(pheno) <- tolower(colnames(pheno))
    if (is.null(pheno$'group')){
        col.var='black'
    }else{
        col.var=pheno$'group'
    }
    factoextra::fviz_pca_var(X = pr,
                              col.var = col.var,
                              axes = c(1, 2),
                              geom = c('point','text'),
                              repel = TRUE)

}

