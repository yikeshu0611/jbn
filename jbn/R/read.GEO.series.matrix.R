#' read series matrix from GEO database
#'
#' @param matrix series matrix
#'
#' @return dataframe
#' @export
#'
#' @examples
#' \donttest{
#'     read.GEO.series.matrix('GSE42872_series_matrix.txt')
#' }
read.GEO.series.matrix <- function(matrix){
    t <- as.data.frame(readr::read_tsv(matrix,comment = '!',progress = FALSE,col_types = readr::cols()))
    rownames(t) <- t[,1]
    t[,-1]
}
