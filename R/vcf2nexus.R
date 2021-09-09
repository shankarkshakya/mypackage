#' @title Convert vcfR object to nexus format data..
#' @description The function converts vcfR format data to nexus format.
#' @param vcf a vcfR object.
#' @param outfile name of output file.
#' @return nexus format file.
#'
#' @details
#' This function converts the vcfR object to a nexus format file.
#'
#'
#' @export

vcf2nexus <- function(vcf, file = "file.nex") {

  vcf <- extract.indels(vcf)
  vcf <- vcf[is.biallelic(vcf),]

  gt.filtered <- extract.gt(vcf, element = "GT", as.numeric = T, convertNA = T)

  gt.filtered[is.na(gt.filtered)] <- "?"

  gt.filtered <- t(gt.filtered)

  ape::write.nexus.data(gt.filtered, file)

  nex.file <- scan(file, what = "character", sep = "\n",
                     quiet = TRUE)

  # bgn <- grep("BEGIN", snapp.file)
  # snapp.file[bgn] <- "BEGIN CHARACTERS;"
  fmt <- grep("FORMAT", nex.file)
  nex.file[fmt] <- "  FORMAT DATATYPE=INTEGER MISSING=? GAP=- SYMBOLS=\"012\" LABELS=LEFT TRANSPOSE=NO INTERLEAVE=NO;"

  #return(snapp.file)
  write(nex.file, file)

}
