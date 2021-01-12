#' Draw gene raw data image
#'
#' @param raw raw data
#'
#' @return write image
#' @export
#'
#' @examples
#' \donttest{
#' library(oligo)
#' cels <- list.files()
#' gse118370_raw <- read.celfiles(cels)
#' qa_raw.image(gse118370_raw)
#' }
qa_raw.image <- function(raw){
    if (!dir.exists('raw_image')) dir.create('raw_image')
    message('creating tiff images to ',getwd(),'/raw_image')
    sn <- stringr::str_extract(Biobase::sampleNames(raw),'GSM[0-9]+')
    for (i in 1:length(sn)) {
        tiff(filename = paste0('./raw_image/',sn[i],'.tif'),
             height = 10,
             width = 10,
             units = 'in',
             res = 300)
        message(length(sn),'---',i,'---',sn[i])
        image(raw[,i],
              main = sn[i])
        dev.off()
    }
    tryCatch(dev.off(),error = function(e) message('OK'))
}
