admix_plot <- function (q.mat, pop.col = 1, prob.col = 2, sort.probs = TRUE,
                        label.pops = TRUE, col = NULL, horiz = TRUE, legend.position = c("top",
                                                                                         "left", "right", "bottom", "none"))
{
  legend.position <- match.arg(legend.position)
  prob.cols <- prob.col:ncol(q.mat)
  qm <- data.frame(q.mat)
  qm[, pop.col] <- factor(qm[, pop.col], levels = sort(unique(qm[,
                                                                 pop.col]), decreasing = horiz))
  sort.cols <- c(pop.col, if (sort.probs) rev(prob.cols) else NULL)
  i <- do.call(order, qm[, sort.cols, drop = FALSE])
  qm <- qm[i, ]
  qm$x <- 1:nrow(qm)
  pop.freq <- table(qm[, pop.col])
  levels(qm[, pop.col]) <- paste(levels(qm[, pop.col]), "\n(n = ",
                                 pop.freq, ")", sep = "")
  pop.cntr <- tapply(qm$x, qm[, pop.col], mean)
  pop.div <- tapply(qm$x, qm[, pop.col], min)[-1] - 0.5
  df.cols <- colnames(qm)[c(pop.col, prob.cols)]
  df.cols <- c("x", df.cols)
  df <- melt(qm[, df.cols], id.vars = c(1, 2), variable.name = "Group",
             value.name = "probability")
  colnames(df)[1:2] <- c("x", "population")
  df <- df[order(-as.numeric(df$Group), df$probability), ]
  g <- ggplot(df, aes_string("x", "probability")) + geom_area(aes_string(fill = "Group"),
                                                              stat = "identity") + ylab("Pr(Group Membership)") + theme(axis.ticks.x = element_blank(),
                                                                                                                        legend.position = legend.position, legend.title = element_blank())
  if (label.pops) {
    g <- g + geom_vline(xintercept = pop.div) + scale_x_continuous(name = "",
                                                                   breaks = pop.cntr, labels = names(pop.cntr))
  }
  else {
    g <- g + xlab("") + theme(axis.text.x = element_blank())
  }
  if (horiz)
    g <- g + coord_flip()
  if (!is.null(col))
    g <- g + scale_fill_manual(values = col)
  #print(g)
  invisible(g)
}




