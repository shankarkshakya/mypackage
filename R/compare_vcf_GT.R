#' Compare genotypes from two vcf files
#'
#' @param vcf1 first vcf file
#'
#' @param vcf2 second vcf file
#'
#' @return A matrix of logical
#' @export
#'
#' @examples
compare_vcfs_GT <- function(vcf1, vcf2){
  require(vcfR)
  gt1 <- extract.gt(vcf1)
  gt2a <- apply(gt1,2, function(x) gsub("1[/|]1","1",x))
  gt2b <- gsub("0[/|]0","0",gt2a)
  gt2c <- gsub("[10][/|][10]","0.5",gt2b)
  gt1 <- gt2c

  gt2 <- extract.gt(vcf2)
  gt2a <- apply(gt2,2, function(x) gsub("1[/|]1","1",x))
  gt2b <- gsub("0[/|]0","0",gt2a)
  gt2c <- gsub("[10][/|][10]","0.5",gt2b)
  gt2 <- gt2c

  mymat <- matrix(NA, nrow(gt1), ncol(gt1))
  colnames(mymat) <- colnames(gt1)
  rownames(mymat) <- rownames(gt1)

  for (i in 1:ncol(gt1)){
    ith_sample_gt1 <- gt1[,i , drop = F]
    ith_sample_gt2 <- gt2[,i , drop = F]

    for(j in 1:nrow(gt1)){
      test <- ith_sample_gt1[j] == ith_sample_gt2[j]
      mymat[j,i] <- test

    }
  }
  return(mymat)
}

