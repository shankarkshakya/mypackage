#' @title List vcfR by population
#' @description The function subsets vcfR object by population of interest to a list.
#' @param vcf a vcfR object.
#' @param pop population assignment.

#' @return subsetted vcfR.
#'
#' @details
#' The function subsets vcfR object by population of interest.
#'
#'
#' @export


sublist_vcfbypop <- function(vcf,pop){

  vcf_list <- vector("list", length(unique(pop)))
  names(vcf_list) <- unique(pop)

  for (i in (1:length(vcf_list))) {


    p <- match(pop, names(vcf_list)[i])
    p <- which(!is.na(p))
    p <- p+1

    vcf_list[[i]] <- vcf[ ,c(1, p)]

  }

  return(vcf_list)


}



