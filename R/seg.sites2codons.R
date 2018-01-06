

















seg.sites2codons <- function(y){
  library(ape)
  
  seg_pos <- seg.sites(y)
  if (length(seg_pos) != 0) {
    
    nt_subt <- vector("list", length(seg_pos))
    names(nt_subt) <- paste("Seg.site number", seg_pos)
    
    
    mod <- lapply(as.list(seg_pos), function(x) x %% 3)
    
    for (i in 1:length(mod)){
      if (mod[[i]] == 0){
        t <- seg_pos[i]- 2
        new <- y[, t:seg_pos[i]]
        nt_subt[[i]] <- new
      } 
      else if (mod[[i]] == 1){
        t <- seg_pos[i]+ 2
        new <- y[, seg_pos[i]:t]
        nt_subt[[i]] <- new
      }
      else {
        st <- seg_pos[i] - 1
        end <- seg_pos[i] + 1
        
        new <- y[, st:end]
        nt_subt[[i]] <- new
        
      }
      
    }  
    
    seg_codon <- nt_subt
    
    charlist <- vector("list", length(seg_codon))
    names(charlist) <- names(seg_codon)
    
    for (i in 1:length(seg_codon)){
      trans_gene <- trans(seg_codon[[i]])
      char <- as.character(trans_gene)
      charlist[[i]] <- char
      
      
    }
    charlist <- do.call(rbind, charlist)
    rownames(charlist) <- seg_pos
    colnames(charlist) <- labels(y)
    rownames(charlist) <- ceiling(as.numeric(rownames(charlist)) /3)
    return(charlist)
    
    
  } else {
    print("No seg sites")
  }
}





