modified_heatmap <- function (x, cbarplot = TRUE, rbarplot = TRUE, legend = TRUE, 
                  clabels = TRUE, rlabels = TRUE, na.rm = TRUE, scale = c("row", 
                                                                          "column", "none"), col.ramp = viridisLite::viridis(n = 100, 
                                                                                                                             alpha = 1), ...) 
{
  
  
  stopifnot(class(x) == "matrix")
  scale <- if (missing(scale)) 
    "none"
  else match.arg(scale)
  nrows <- 1
  ncols <- 1
  if (cbarplot == TRUE) {
    nrows <- nrows + 1
  }
  if (rbarplot == TRUE) {
    ncols <- ncols + 1
  }
  if (legend == TRUE) {
    ncols <- ncols + 1
  }
  if (scale == "row") {
    x <- sweep(x, 1L, rowMeans(x, na.rm = na.rm), check.margin = FALSE)
    sx <- apply(x, 1L, stats::sd, na.rm = na.rm)
    x <- sweep(x, 1L, sx, "/", check.margin = FALSE)
  }
  else if (scale == "column") {
    x <- sweep(x, 2L, colMeans(x, na.rm = na.rm), check.margin = FALSE)
    sx <- apply(x, 2L, stats::sd, na.rm = na.rm)
    x <- sweep(x, 2L, sx, "/", check.margin = FALSE)
  }
  if (is.null(colnames(x))) {
    colnames(x) <- 1:ncol(x)
  }
  if (is.null(rownames(x))) {
    rownames(x) <- 1:nrow(x)
  }
  userpar <- graphics::par(no.readonly = TRUE)
  on.exit({
    graphics::par(userpar)
  })
  if (cbarplot == FALSE & rbarplot == FALSE & legend == FALSE) {
  }
  if (cbarplot == FALSE & rbarplot == TRUE & legend == FALSE) {
    graphics::layout(matrix(1:2, nrow = nrows, ncol = ncols, 
                            byrow = TRUE), widths = c(4, 0.6))
  }
  if (cbarplot == FALSE & rbarplot == FALSE & legend == TRUE) {
    graphics::layout(matrix(1:2, nrow = nrows, ncol = ncols, 
                            byrow = TRUE), widths = c(4, 0.2))
  }
  if (cbarplot == FALSE & rbarplot == TRUE & legend == TRUE) {
    graphics::layout(matrix(1:3, nrow = nrows, ncol = ncols, 
                            byrow = TRUE), widths = c(4, 0.6, 0.2))
  }
  if (cbarplot == TRUE & rbarplot == FALSE & legend == FALSE) {
    graphics::layout(matrix(1:2, nrow = nrows, ncol = ncols, 
                            byrow = TRUE), heights = c(1, 4))
  }
  if (cbarplot == TRUE & rbarplot == TRUE & legend == FALSE) {
    graphics::layout(matrix(1:4, nrow = nrows, ncol = ncols, 
                            byrow = TRUE), widths = c(4, 0.6), heights = c(1, 
                                                                           4))
  }
  if (cbarplot == TRUE & rbarplot == FALSE & legend == TRUE) {
    graphics::layout(matrix(1:4, nrow = nrows, ncol = ncols, 
                            byrow = TRUE), widths = c(4, 0.2), heights = c(1, 
                                                                           4))
  }
  if (cbarplot == TRUE & rbarplot == TRUE & legend == TRUE) {
    graphics::layout(matrix(1:6, nrow = nrows, ncol = ncols, 
                            byrow = TRUE), widths = c(4, 0.6, 0.2), heights = c(1, 
                                                                                4))
  }
  graphics::par(mar = c(0, 0, 0, 0))
  graphics::par(oma = c(1, 1, 1, 1))
  if (cbarplot == TRUE) {
    graphics::barplot(colSums(x, na.rm = na.rm), space = 0, 
                      border = NA, axes = FALSE, names.arg = "", col = c("#FFFFFF", 
                                                                         "#FFFFFF"), xaxs = "i")
    if (clabels == TRUE & scale == "none") {
      graphics::text(c(1:ncol(x)) - 0.5, 0, colnames(x), 
                     adj = c(0, 0.5), srt = 90)
    }
    else if (clabels == TRUE & scale != "none") {
      graphics::text(c(1:ncol(x)) - 0.5, min(colSums(x, 
                                                     na.rm = na.rm), na.rm = na.rm), colnames(x), 
                     adj = c(0, 0.5), srt = 90)
    }
    if (rbarplot == TRUE) {
      plot(1, 1, type = "n", axes = FALSE, xlab = "", ylab = "")
    }
    if (legend == TRUE) {
      plot(1, 1, type = "n", axes = FALSE, xlab = "", ylab = "")
    }
  }
  graphics::image(t(x), col = col.ramp, axes = FALSE, frame.plot = TRUE)
  if (rbarplot == TRUE) {
    graphics::barplot(rowSums(x, na.rm = na.rm), space = 0, 
                      border = NA, horiz = TRUE, axes = FALSE, names.arg = "", 
                      col = c("#FFFFFF", "#FFFFFF"), yaxs = "i")
    if (rlabels == TRUE & scale == "none") {
      graphics::text(0, c(1:nrow(x)) - 0.5, rownames(x), 
                     adj = c(0, 0.5), srt = 0)
    }
    else if (rlabels == TRUE & scale != "none") {
      graphics::text(min(rowSums(x, na.rm = na.rm), na.rm = na.rm), 
                     c(1:nrow(x)) - 0.5, rownames(x), adj = c(0, 0.5), 
                     srt = 0)
    }
  }
  if (legend == TRUE) {
    mp <- graphics::barplot(rep(1, times = length(col.ramp)), 
                            space = 0, border = NA, horiz = TRUE, col = col.ramp, 
                            axes = FALSE)
    if (mp[nrow(mp), 1] - mp[1, 1] >= 1) {
      graphics::text(0.5, mp[1, 1], "Low", col = "#FFFFFF", 
                     adj = c(0.5, 0))
      graphics::text(0.5, mp[nrow(mp), 1], "High", col = "#FFFFFF", 
                     adj = c(0.5, 1))
    }
  }
  invisible(NULL)
}
