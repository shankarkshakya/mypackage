#' @title Count number of synonymous and non synonymous SNP.
#' @description The function counts the number of SNP changes.
#' @param DNAbin object.
#' @return list of SNPs count
#'
#' @details
#' Count the total number of SNPs and further anayses number of syn vs non-syn SNPs.
#'
#' @example
#' library(ape)
#' library(mypackage)
#' data("woodmouse")
#' count_SNPs(woodmouse)
#'
#' @export


count_SNPS <- function(dnabin) {

  x <- mypackage::seg.sites2codons(dnabin)

  if (length(x != 0 )){
    seg_mat <- vector("list", length = nrow(x))
    names(seg_mat) <- paste0("Site number", rownames(x))

      for (i in 1:nrow(x)) {
        seg_mat[[i]] <- unique(x[i, ])

      }

  return(list(Total_SNPS = nrow(x),
              SYN_SNPS= length(seg_mat[lapply(seg_mat, length) == 1]), SYN_SITES = names(seg_mat[lapply(seg_mat, length) == 1]),
              NON_SYN_SNPS = length(seg_mat[lapply(seg_mat, length) > 1]), NON_SYN_SITES = names(seg_mat[lapply(seg_mat, length) > 1])))

  }


}














# seq_dnabin <- readRDS("Pinf_conserved_RXLR_APS_with_refseq.RData")
# seq_dnabin <- readRDS("conserved_Core_APS.RData")
#
# seq_dnabin <- seq_dnabin[lapply(lapply(seq_dnabin, seg.sites), length) > 0]
#
# p <- lapply(seq_dnabin, myfunc)
#
# syn_change <- lapply(p, function(x) x[[2]])
# syn_change <- as.data.frame(unlist(syn_change))
#
#
# nonsyn_change <- lapply(p, function(x) x[[4]])
# nonsyn_change <- as.data.frame(unlist(nonsyn_change))
#
# df <- cbind(syn_change, nonsyn_change)
# colnames(df) <- c("Syn_change", "Non-syn_change")
#
# df.melt <- reshape2::melt(df)
# plot <- ggplot2::ggplot(data = df.melt, aes(x= variable, y = value, fill = variable)) + geom_boxplot()
#
# plot
#
#
