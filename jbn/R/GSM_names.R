
#' Extract GSM names
#'
#' @param raw raw
#'
#' @return GSM names
#' @export
#'
GSM_names <- function(raw){
    stringr::str_extract(sampleNames(raw),'GSM[0-9]+')
}
