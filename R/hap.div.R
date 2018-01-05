#' @title Haplotype diversity
#' @description The function returns the haplotype diversity per population.
#' @param DNAbin object.
#' @param population.
#' @return Haplotype diversity per population
#'
#' @details
#' The function takes an advantage of other functions from package pegas. Pegas calculates number of haplotypes and haplotype frequency.
#' This function tries to summarize haplotype diversity by population if specified.
#'
#'
#' @export


hap.div <- function(dnabin, pop){
  
  rownames(dnabin) <- paste("Pop", pop, sep = "_")
  h <- pegas::haplotype(dnabin)
  h <- sort(h, what = "label")
  
  h.freq <- haploFreq(dnabin, haplo = h)
  rownames(h.freq) <- paste("Haplotype", 1:nrow(h.freq), sep = "_")
  
  mat.df <- as.data.frame(h.freq)
  
  hap_mat <- matrix(NA, nrow =1 , ncol = ncol(mat.df))
  rownames(hap_mat) <- "Haplotype Diversity"
  colnames(hap_mat) <- colnames(mat.df)
  
  for (i in 1:ncol(mat.df)){
    
    temp_pop <- mat.df[i] / colSums(mat.df[i])
    temp_pop <- temp_pop^2
    temp_pop_haplotypeddiv <- colSums(mat.df[i]) *(1- colSums(temp_pop)) / (colSums(mat.df[i])-1)
    hap_mat[1, i] <-  temp_pop_haplotypeddiv
    
  }
  return(hap_mat)
}





