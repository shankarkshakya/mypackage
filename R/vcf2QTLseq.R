#' @title Convert vcfR object to QTLseqR format.
#' @description The function converts vcf file to text format file that can be used by QTLseqR package.
#' @param vcfR object.
#' @return text file
#'
#' @details
#' Converts vcfR data format to text file to be used by QTLseqR.
#'
#'
#' @export

vcf2QTLseq <- function(vcf){
  require(vcfR)

  x <- vcfR2tidy(vcf, single_frame = TRUE, format_fields = c("AD", "DP"))
  mytable <- x$dat

  low_bulk_table <- mytable[seq(from = 1, to = nrow(mytable), by = 2) , c(1, 2, 4, 5, 26:27)]

  colnames(low_bulk_table)[5:6] <- paste(colnames(vcf@gt)[2], colnames(low_bulk_table)[5:6], sep = ".")

  high_bulk_table <- mytable[seq(from = 2, to = nrow(mytable), by = 2), c(1,2,4,5, 26:27)]

  colnames(high_bulk_table)[5:6] <- paste(colnames(vcf@gt)[3], colnames(high_bulk_table)[5:6], sep = ".")

  df <- cbind(low_bulk_table, high_bulk_table[, c(5:6)])
  colnames(df)
  return(df)
}


