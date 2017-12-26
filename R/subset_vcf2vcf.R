#' @title Subset a vcf file
#' @description The function subsets the larger vcf file by variants from other smaller vcf file.
#' @param Larger vcfR object.
#' @param Smaller vcfR object.
#' @return subsetted vcf file by small vcfR variants
#'
#' @details
#' Subset the vcfR object given the variants from smaller vcfR object
#'
#'
#' @export


subset_vcf2vcf <- function(large.vcf, small.vcf) {

  large.vcf@fix[,3] <- paste(large.vcf@fix[,1], large.vcf@fix[,2], sep = "_")
  small.vcf@fix[,3] <- paste(small.vcf@fix[,1], small.vcf@fix[,2], sep = "_")


  large.vcf.df <- as.data.frame(large.vcf@fix)
  small.vcf.df <- as.data.frame(small.vcf@fix)

  row_id <- as.numeric(rownames(subset(large.vcf.df, large.vcf.df$ID %in% small.vcf.df$ID)))

  new_vcf <- large.vcf[row_id, ]

  return(new_vcf)


}
