#' @title Subset vcfR by population
#' @description The function subsets vcfR object by population of interest.
#' @param vcf a vcfR object.
#' @param pop population assignment.
#' @param in_pop population of interest.
#' @return subsetted vcfR.
#'
#' @details
#' The function subsets vcfR object by population of interest.
#'
#'
#' @export

vcf_popsub <- function(vcf, pop, in_pop) {
  ids <- which(pop %in% in_pop)
  ids <- ids+1
  vcf <- vcf[, c(1,ids)]

  return(vcf)

}


# library(vcfR)
#
# vcf <- read.vcfR("Min10x_cov_203isolates_1027Variants.gz")
#
#
# id <- unlist(strsplit(colnames(vcf@gt)[-1], split = ".fq"))
# pcinna_pop <- read.csv("New Microsoft Excel Worksheet.csv", header = TRUE)
# pcinna_pop <- pcinna_pop[pcinna_pop$Isolate %in% id, ]
# pcinna_pop <- pcinna_pop[match(id, pcinna_pop$Isolate), ]
# pop_cinna <- pcinna_pop$Country
#
# pop <- pop_cinna
#
#
#
# in_pop <- c("Portugal", "Taiwan", "Australia", "USA", "Vietnam", "Chile", "South Africa")
#
#
# vcf_popsub(vcf, pop, in_pop)
