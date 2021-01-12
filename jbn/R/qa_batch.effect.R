#' batch effect
#'
#' @param raw raw data
#'
#' @return batch effect
#' @export
#' @examples
#' \donttest{
#' library(oligo)
#' cels <- list.files()
#' gse118370_raw <- read.celfiles(cels)
#' qa_batch.effect(gse118370_raw)
#' }
qa_batch.effect <- function(raw){
    date <- do::Replace0(raw@protocolData@data[["dates"]],c('[0-9]{2}:[0-9]{2}:[0-9]{2}','[A-Z]'))
    table(date)
}
