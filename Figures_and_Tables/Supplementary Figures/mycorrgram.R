
mycorrgram <-
function (x, type = NULL, order = FALSE, labels, panel = panel.shade, 
          lower.panel = panel, upper.panel = panel, diag.panel = NULL, 
          text.panel = textPanel, label.pos = c(0.5, 0.5), label.srt = 0, 
          cex.labels = NULL, font.labels = 1, row1attop = TRUE, dir = "", 
          gap = 0, abs = FALSE, col.regions = colorRampPalette(c("red", 
                                                                 "red","red","red", "navy", "navy", "navy", "navy")), cor.method = "pearson", 
          outer.labels = NULL, ...) 
{
  if (is.null(order)) 
    order <- FALSE
  if (cor.method != "pearson" & (deparse(substitute(upper.panel)) == 
                                 "panel.conf" | deparse(substitute(upper.panel)) == "panel.conf")) 
    stop("'panel.conf' only allows 'pearson' method. Try 'panel.cor' instead.")
  if (length(label.pos) < 2) 
    stop("label.pos needs a vector of length 2")
  if (dir == "") {
    if (row1attop) 
      dir <- "left"
    else dir <- "right"
  }
  if (dir == "\\") 
    dir <- "left"
  if (dir == "/") 
    dir <- "right"
  if (ncol(x) < 2) 
    stop("Only one column in the argument to 'corrgram'")
  if (is.matrix(x) && isSymmetric(unname(x))) {
    maybeCorr <- TRUE
    if (min(x, na.rm = TRUE) < -1 - .Machine$double.eps) {
      warning("Matrix symmetric, but some values < -1. Treated as data.frame.")
      maybeCorr <- FALSE
    }
    if (max(x, na.rm = TRUE) > 1 + .Machine$double.eps) {
      warning("Matrix symmetric, but some values > +1. Treated as data.frame.")
      maybeCorr <- FALSE
    }
  }
  else maybeCorr <- FALSE
  if (is.null(type)) {
    type <- if (maybeCorr) 
      "corr"
    else "data"
  }
  else if (type == "data") {
    if (maybeCorr) 
      warning("This looks like a correlation matrix.")
  }
  else if (type == "cor" || type == "corr") {
    type <- "corr"
    if (!maybeCorr) 
      warning("This is NOT a correlation matrix.")
  }
  else {
    stop("unknown data type in 'corrgram'")
  }
  if (type == "data" && !is.matrix(x)) 
    x <- x[, vapply(x, is.numeric, logical(1))]
  if (type == "data") {
    cmat <- cor(x, use = "pairwise.complete.obs", method = cor.method)
  }
  else {
    cmat <- x
  }
  cmat.return <- cmat
  cmat <- if (abs) 
    abs(cmat)
  else cmat
  if ((order %in% c("OLO", "GW", "HC")) && ("seriation" %in% 
                                            rownames(installed.packages())) == FALSE) 
    stop("Please use install.packages('seriation') for this 'order' option.")
  if (order == FALSE) 
    ord <- 1:nrow(cmat)
  if (order == TRUE || order == "PC" || order == "PCA") {
    x.eigen <- eigen(cmat)$vectors[, 1:2]
    e1 <- x.eigen[, 1]
    e2 <- x.eigen[, 2]
    alpha <- ifelse(e1 > 0, atan(e2/e1), atan(e2/e1) + pi)
    ord <- order(alpha)
    x <- if (type == "data") 
      x[, ord]
    else x[ord, ord]
    cmat.return <- cmat.return[ord, ord]
  }
  else if (order %in% c("OLO", "GW", "HC")) {
    distx <- dist(cmat)
    ss <- seriation::seriate(distx, method = order)
    ord <- seriation::get_order(ss)
    x <- if (type == "data") 
      x[, ord]
    else x[ord, ord]
    cmat.return <- cmat.return[ord, ord]
  }
  else if (order != FALSE) {
    stop("Unknown order argument in 'corrgram'.")
  }
  textPanel <- function(x = 0.5, y = 0.5, txt, cex, font, srt) {
    text(x, y, txt, cex = cex, font = font, srt = srt)
  }
  localPlot <- function(..., main, oma, font.main, cex.main) plot(...)
  localLowerPanel <- function(..., main, oma, font.main, cex.main) lower.panel(...)
  localUpperPanel <- function(..., main, oma, font.main, cex.main) upper.panel(...)
  localDiagPanel <- function(..., main, oma, font.main, cex.main) diag.panel(...)
  dots <- list(...)
  nmdots <- names(dots)
  if (!is.matrix(x)) {
    x <- as.data.frame(x)
    for (i in seq(along = names(x))) {
      if (is.factor(x[[i]]) || is.logical(x[[i]])) 
        x[[i]] <- as.numeric(x[[i]])
      if (!is.numeric(unclass(x[[i]]))) 
        stop("non-numeric argument to 'corrgram'")
    }
  }
  else if (!is.numeric(x)) 
    stop("non-numeric argument to 'corrgram'")
  panel <- match.fun(panel)
  if ((has.lower <- !is.null(lower.panel)) && !missing(lower.panel)) 
    lower.panel <- match.fun(lower.panel)
  if ((has.upper <- !is.null(upper.panel)) && !missing(upper.panel)) 
    upper.panel <- match.fun(upper.panel)
  has.diag <- !is.null(diag.panel)
  if (has.diag && !missing(diag.panel)) 
    diag.panel <- match.fun(diag.panel)
  if (dir == "left") {
    tmp <- lower.panel
    lower.panel <- upper.panel
    upper.panel <- tmp
    tmp <- has.lower
    has.lower <- has.upper
    has.upper <- tmp
  }
  nc <- ncol(x)
  has.labs <- TRUE
  if (missing(labels)) {
    labels <- colnames(x)
    if (is.null(labels)) 
      labels <- paste("var", 1:nc)
  }
  else if (order != FALSE) {
    labels <- labels[ord]
  }
  else if (is.null(labels)) 
    has.labs <- FALSE
  if (is.null(text.panel)) 
    has.labs <- FALSE
  oma <- if ("oma" %in% nmdots) 
    dots$oma
  else NULL
  main <- if ("main" %in% nmdots) 
    dots$main
  else NULL
  if (is.null(oma)) {
    oma <- c(4, 4, 4, 4)
    if (!is.null(main)) 
      oma[3] <- 6
  }
  opar <- par(mfrow = c(nc, nc), mar = rep.int(gap/2, 4), oma = oma)
  on.exit(par(opar))
  for (i in if (dir == "left") 
    1:nc
    else nc:1) for (j in 1:nc) {
      localPlot(x[, j], x[, i], xlab = "", ylab = "", axes = FALSE, 
                type = "n", ...)
      if (i == j || (i < j && has.lower) || (i > j && has.upper)) {
        if (i == j) {
          if (has.diag) {
            if (type == "data") 
              localDiagPanel(as.vector(x[, i]), NULL, ...)
            else localDiagPanel(NULL, x[i, i], ...)
          }
          if (has.labs) {
            par(usr = c(0, 1, 0, 1))
            if (is.null(cex.labels)) {
              l.wid <- strwidth(labels, "user")
              cex.labels <- max(0.8, min(2, 0.9/max(l.wid)))
            }
            text.panel(label.pos[1], label.pos[2], labels[i], 
                       cex = cex.labels, font = font.labels, srt = label.srt)
          }
        }
        else if (i < j) {
          if (type == "data") 
            localLowerPanel(as.vector(x[, j]), as.vector(x[, 
                                                           i]), NULL, col.regions, cor.method, ...)
          else localLowerPanel(NULL, NULL, x[j, i], col.regions, 
                               cor.method, ...)
        }
        else {
          if (type == "data") 
            localUpperPanel(as.vector(x[, j]), as.vector(x[, 
                                                           i]), NULL, col.regions, cor.method, ...)
          else localUpperPanel(NULL, NULL, x[j, i], col.regions, 
                               cor.method, ...)
        }
      }
      else {
        par(new = FALSE)
      }
    }
  if (!is.null(main)) {
    font.main <- if ("font.main" %in% nmdots) 
      dots$font.main
    else par("font.main")
    cex.main <- if ("cex.main" %in% nmdots) 
      dots$cex.main
    else par("cex.main")
    mtext(main, 3, 3, TRUE, 0.5, cex = cex.main, font = font.main)
  }
  corrgram.outer.labels(1, nc, ord, labels, outer.labels$bottom)
  corrgram.outer.labels(2, nc, ord, labels, outer.labels$left)
  corrgram.outer.labels(3, nc, ord, labels, outer.labels$top)
  corrgram.outer.labels(4, nc, ord, labels, outer.labels$right)
  invisible(cmat.return)
}
