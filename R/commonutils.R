#' Plot means by group(s)
#'
#'
#' This function is taken from John Fox's plotMeans function from the Rcmdr package
#' @param response the response variable
#' @param factor1 the first grouping variable
#' @param factor2 the second grouping variable
#' @param error.bars the type of error bars to plot
#' @param level the confidence level to use when plotting conf.int
#' @param xlab x axis label
#' @param ylab y axis label
#' @param legend.lab legend label
#' @param main main title of the plot
#' @param pch plotting character i.e. symbol to use
#' @param lty line type
#' @param col color palette to use
#' @param ... additional arguments. unused currently.
#' @export
plotMeans <- function (response, factor1, factor2, error.bars = c("se", "sd",
    "conf.int", "none"), level = 0.95, xlab = deparse(substitute(factor1)),
    ylab = paste("mean of", deparse(substitute(response))), legend.lab = deparse(substitute(factor2)),
    main = "Plot of Means", pch = 1:n.levs.2, lty = 1:n.levs.2,
    col = palette(), ...)
{
    if (!is.numeric(response))
        stop.Alteryx2("Argument response must be numeric.")
    xlab
    ylab
    legend.lab
    error.bars <- match.arg(error.bars)
    if (missing(factor2)) {
        if (!is.factor(factor1))
            stop.Alteryx2("Argument factor1 must be a factor.")
        valid <- complete.cases(factor1, response)
        factor1 <- factor1[valid]
        response <- response[valid]
        means <- tapply(response, factor1, mean)
        sds <- tapply(response, factor1, sd)
        ns <- tapply(response, factor1, length)
        if (error.bars == "se")
            sds <- sds/sqrt(ns)
        if (error.bars == "conf.int")
            sds <- qt((1 - level)/2, df = ns - 1, lower.tail = FALSE) *
                sds/sqrt(ns)
        sds[is.na(sds)] <- 0
        yrange <- if (error.bars != "none")
            c(min(means - sds, na.rm = TRUE), max(means + sds,
                na.rm = TRUE))
        else range(means, na.rm = TRUE)
        levs <- levels(factor1)
        n.levs <- length(levs)
        plot(c(1, n.levs), yrange, type = "n", xlab = xlab, ylab = ylab,
            axes = FALSE, main = main, ...)
        points(1:n.levs, means, type = "b", pch = 16, cex = 2)
        box()
        axis(2)
        axis(1, at = 1:n.levs, labels = levs)
        if (error.bars != "none")
            arrows(1:n.levs, means - sds, 1:n.levs, means + sds,
                angle = 90, lty = 2, code = 3, length = 0.125)
    }
    else {
        if (!(is.factor(factor1) | is.factor(factor2)))
            stop.Alteryx2("Arguments factor1 and factor2 must be factors.")
        valid <- complete.cases(factor1, factor2, response)
        factor1 <- factor1[valid]
        factor2 <- factor2[valid]
        response <- response[valid]
        means <- tapply(response, list(factor1, factor2), mean)
        sds <- tapply(response, list(factor1, factor2), sd)
        ns <- tapply(response, list(factor1, factor2), length)
        if (error.bars == "se")
            sds <- sds/sqrt(ns)
        if (error.bars == "conf.int")
            sds <- qt((1 - level)/2, df = ns - 1, lower.tail = FALSE) *
                sds/sqrt(ns)
        sds[is.na(sds)] <- 0
        yrange <- if (error.bars != "none")
            c(min(means - sds, na.rm = TRUE), max(means + sds,
                na.rm = TRUE))
        else range(means, na.rm = TRUE)
        levs.1 <- levels(factor1)
        levs.2 <- levels(factor2)
        n.levs.1 <- length(levs.1)
        n.levs.2 <- length(levs.2)
        if (length(pch) == 1)
            pch <- rep(pch, n.levs.2)
        if (length(col) == 1)
            col <- rep(col, n.levs.2)
        if (length(lty) == 1)
            lty <- rep(lty, n.levs.2)
        if (n.levs.2 > length(col))
            stop.Alteryx2(sprintf("Number of groups for factor2, %d, exceeds number of distinct colours, %d.",
                n.levs.2, length(col)))
        plot(c(1, n.levs.1 * 1.4), yrange, type = "n", xlab = xlab,
            ylab = ylab, axes = FALSE, main = main, ...)
        box()
        axis(2)
        axis(1, at = 1:n.levs.1, labels = levs.1)
        for (i in 1:n.levs.2) {
            points(1:n.levs.1, means[, i], type = "b", pch = pch[i],
                cex = 2, col = col[i], lty = lty[i])
            if (error.bars != "none")
                arrows(1:n.levs.1, means[, i] - sds[, i], 1:n.levs.1,
                  means[, i] + sds[, i], angle = 90, code = 3,
                  col = col[i], lty = lty[i], length = 0.125)
        }
        x.posn <- n.levs.1 * 1.1
        y.posn <- sum(c(0.1, 0.9) * par("usr")[c(3, 4)])
        text(x.posn, y.posn, legend.lab, adj = c(0, -0.5))
        legend(x.posn, y.posn, levs.2, pch = pch, col = col,
            lty = lty)
    }
    invisible(NULL)
}



#' Trim Blanks Function
#'
#' This function trims blanks from text input
#' @param text input text that may contain blanks
#' @export
trim.blanks <- function(text){
  gsub("^\ *", "", gsub("\ *$", "", text))
}

#' Convert row into a pipe delimited string
#'
#'
#' The function pipeDelim takes a table row that comes from capture.output and
#' turns it into a pipe delimited string for processing in Alteryx.
#'
#' @param capture.vec row vector to convert into pipe delimited string
#' @author Dan Putler
#' @export
pipeDelim <- function(capture.vec) {
  pD <- function(cv) {
    print(cv)
    cv1 <- unlist(strsplit(cv, " "))
    cv1 <- cv1[cv1 != ""]
    cv <- paste(cv1, collapse = "|")
    return(cv)
  }
  out.vec <- sapply(capture.vec, pD)
  out.vec <- sub("<2.2e-16", "< 2.2e-16", out.vec)
  names(out.vec) <- NULL
  return(out.vec)
}


#' Convert a matrix into a pipe delimited set of values
#'
#'
#' The matrixPipeDelim function takes a matrix and converts it into a character
#' vector in which each element is a pipe delimited set of values for a row in
#' the original matrix
#'
#' @param a.matrix matrix to convert
#' @param round.level rounding level to use
#' @author Dan Putler
#' @export
matrixPipeDelim <- function (a.matrix, round.level = 5e-07){
  splitFun <- function(string) {
    the.split <- unlist(strsplit(string, "e"))
    front <- substr(the.split[1], 1, 4)
    new.val <- paste(front, the.split[2], sep = "e")
    return(new.val)
  }
	rowText <- function(a.row, types = NULL) {
    row.txt <- as.character(a.row)
		row.txt.not.na <- row.txt[!is.na(a.row)]
		a.row.not.na <- a.row[!is.na(a.row)]
		if (!is.null(types))
			types.not.na <- types[!is.na(a.row)]
		if (length(a.row.not.na) > 0) {
		    if (class(a.row.not.na) != "character") {
				row.txt.not.na[abs(a.row.not.na) >= round.level] <-
					as.character(round(a.row.not.na[abs(a.row.not.na) >= round.level], 6))
				sci.not <- row.txt.not.na[abs(a.row.not.na) < round.level & a.row.not.na != 0]
				if (length(sci.not > 0)) {
					sci.not.new <- sapply(sci.not, splitFun, USE.NAMES = FALSE)
					row.txt.not.na[abs(a.row.not.na) < round.level & a.row.not.na != 0] <- sci.not.new
				}
				row.txt[!is.na(a.row)] <- row.txt.not.na
			} else {
				a.row.num <- as.numeric(a.row.not.na[types.not.na != "factor"])
				row.txt.num <- row.txt.not.na[types.not.na != "factor"]
				row.txt.num[abs(a.row.num) >= round.level] <-
					as.character(round(a.row.num[abs(a.row.num) >= round.level], 6))
				sci.not <- row.txt.num[abs(a.row.num) < round.level & a.row.num != 0]
				if (length(sci.not > 0)) {
					sci.not.new <- sapply(sci.not, splitFun, USE.NAMES = FALSE)
					row.txt.num[abs(a.row.num) < round.level & a.row.num != 0] <- sci.not.new
				}
				row.txt.not.na[types.not.na != "factor"] <- row.txt.num
				row.txt[!is.na(a.row)] <- row.txt.not.na
			}
		}
    row.out <- paste(row.txt, collapse = "|")
    names(row.out) <- NULL
    return(row.out)
  }

	if (class(a.matrix) == "data.frame") {
		the.types <- NULL
		for (i in names(a.matrix)) {
			the.types <- c(the.types, class(a.matrix[[i]]))
		}
		out.vec <- apply(a.matrix, 1, rowText, types = the.types)
	} else {
		out.vec <- apply(a.matrix, 1, rowText)
	}
	names(out.vec) <- NULL
	out.vec
}

#' Convert p-values to text.
#'
#' The function pStars takes a vector of p-values and (1) converts them to text
#' and formats the "top-end" to be consistent with standard R reporting of
#' p-values and (2) creates a text column of "stars" to signify significance
#' levels.
#' @param p.val vector of pvalues.
#' @author Dan Putler
#' @export
pStars <- function (p.val){
  splitFun <- function(string) {
    the.split <- unlist(strsplit(string, "e"))
    front <- substr(the.split[1], 1, 4)
    new.val <- paste(front, the.split[2], sep = "e")
    return(new.val)
  }
  the.stars <- rep(" ", length(p.val))
	order.var <- 1:length(p.val)
	p.val.good <- p.val[!is.na(p.val)]
	the.stars.na <- the.stars[is.na(p.val)]
	order.var.na <- order.var[is.na(p.val)]
	the.stars.good <- the.stars[!is.na(p.val)]
	order.var.good <- order.var[!is.na(p.val)]
  the.stars.good[p.val.good < 0.001] <- "***"
  the.stars.good[p.val.good >= 0.001 & p.val.good < 0.01] <- "**"
  the.stars.good[p.val.good >= 0.01 & p.val.good < 0.05] <- "*"
  the.stars.good[p.val.good >= 0.05 & p.val.good < 0.1] <- "."
  p.txt <- as.character(p.val)
	p.txt.na <- p.txt[is.na(p.val)]
	p.txt.good <- p.txt[!is.na(p.val)]
  p.txt.good[p.val.good < 2.2e-16] <- "< 2.2e-16"
  p.txt.good[p.val.good >= 5e-06] <- round(p.val.good[p.val.good >= 5e-06], 5)
  mid.p <- p.txt.good[p.val.good < 5e-06 & p.val.good > 2.2e-16]
  if (length(mid.p > 0)) {
    mid.p.new <- sapply(mid.p, splitFun, USE.NAMES = FALSE)
    p.txt.good[p.val.good < 5e-06 & p.val.good > 2.2e-16] <- mid.p.new
  }
	order.var <- c(order.var.good, order.var.na)
	p.txt <- c(p.txt.good, p.txt.na)
	p.txt <- p.txt[order(order.var)]
	the.stars <- c(the.stars.good, the.stars.na)
	the.stars <- the.stars[order(order.var)]
  out.df <- data.frame(p_txt = p.txt, Stars = the.stars)
  return(out.df)
}


#' Unit Scale Function
#'
#' The unitScale function standardizes variables so that their values fall in the
#' inclusive unit interval. The argument (x) must be an object that is, or can be
#' coerced to be, a numeric matrix. The function returns a matrix of values whose
#' columns have been standardized to have values that fall in the inclusive unit
#' interval.
#'
#' @param x a numeric matrix object
#' @export
#' @author Dan Putler
unitScale <- function(x) {
  if (!(class(x) %in% c("matrix", "data.frame", "numeric", "integer"))) {
    stop.Alteryx2("The function argument cannot be coerced to be a matrix")
  }
  if (class(x) == "data.frame") {
    x <- data.matrix(x)
  }
  if (!(class(x) == "matrix")) {
    x <- as.matrix(x)
  }
  if (class(as.vector(x[,1])) == "character") {
    stop.Alteryx2("The function argument must consist of numeric and/or integer data")
  }
  min.x <- apply(x, 2, min)
  max.x <- apply(x, 2, max)
  max.min <- max.x - min.x
  if (any(max.min == 0)) {
    stop.Alteryx2("One or more of the provided data columns has a single data value")
  }
  # Subtract the column minimums
  x <- sweep(x, 2, min.x)
  # Divide by the range
  x <- sweep(x, 2, max.min, FUN = "/")
  return(x)
}


#' Biplot of a Cluster Analysis Function
#'
#'
#' The bpCent function is from Dan Putler's BCA package, and is included here to avoid
#' the need of loading the Rcmdr package (with its GUI). The function creates a biplot
#' of a cluster analysis solution.
#'
#' @param pc The prcomp object of the data used in clustering.
#' @param clsAsgn A vector containing the cluster assignment for each record in
#'   the clustering data.
#' @param data.pts If TRUE the point for each record is plotted.
#' @param centroids If TRUE the centroid for each cluster is plotted.
#' @param choices	length 2 vector specifying the components to plot.
#' @param scale	The variables scaled by lambda ^ scale and the observations are
#'   scaled by lambda ^ (1-scale), where lambda are the eigen values of the
#'   principal components solution. scale should be between 0 and 1.
#' @param pc.biplot	If true, then lambda = 1 and the observations are are scaled
#'   up the sqrt(n) and the variables scaled down by sqrt(n). In this case the
#'   inner product between variables approximate covariances, and the distances
#'   between observations approximate Mahalanobis distance. Gabriel refers to
#'   this as a "principal component biplot".
#' @param var.axes	If TRUE the second set of points have arrows representing
#'   them as (unscaled) axes.
#' @param col	A vector of length 2 giving the colours for the first and second
#'   set of points respectively (and the corresponding axes). If a single colour
#'   is specified it will be used for both sets. If missing the default colour
#'   is looked for in the palette: if there it and the next colour as used,
#'   otherwise the first two colours of the paletter are used.
#' @param cex	The character expansion factor used for labelling the points. The
#'   labels can be of different sizes for the two sets by supplying a vector of
#'   length two.
#' @param xlabs	A vector of character strings to label the first set of points:
#'   the default is to use the row dimname of x, or 1:n is the dimname is NULL.
#' @param ylabs A vector of character strings to label the second set of points: the default is to use the row dimname of y, or 1:n is the dimname is NULL.
#' @param expand An expansion factor to apply when plotting the second set of points relative to the first. This can be used to tweak the scaling of the two sets to a physically comparable scale.
#' @param xlim Limits for the x axis in the units of the first set of variables.
#' @param ylim Limits for the y axis in the units of the first set of variables.
#' @param arrow.len The length of the arrow heads on the axes plotted in var.axes is true. The arrow head can be suppressed by arrow.len = 0.
#' @param main graphical parameter
#' @param sub graphical parameter
#' @param xlab graphical parameter
#' @param ylab graphical parameter
#' @param ... graphical parameters
#' @author Dan Putler
#' @export
bpCent <- function(pc, clsAsgn, data.pts = TRUE, centroids = TRUE,
  choices = 1:2, scale = 1, pc.biplot=FALSE, var.axes = TRUE, col=palette()[1:2],
  cex = rep(par("cex"), 2), xlabs = NULL, ylabs = NULL, expand=1, xlim = NULL,
  ylim = NULL, arrow.len = 0.1, main = NULL, sub = NULL, xlab = NULL,
  ylab = NULL, ...) {
    if(length(choices) != 2) stop.Alteryx2("length of choices must be 2")
    if(!length(scores <- pc$x))
	stop.Alteryx2(gettextf("object '%s' has no scores", deparse(substitute(x))),
             domain = NA)
    if(is.complex(scores))
        stop.Alteryx2("biplots are not defined for complex PCA")
    lam <- pc$sdev[choices]
    n <- NROW(scores)
    lam <- lam * sqrt(n)
    if(scale < 0 || scale > 1) warning("'scale' is outside [0, 1]")
    if(scale != 0) lam <- lam^scale else lam <- 1
    if(pc.biplot) lam <- lam / sqrt(n)
    cntrs <- apply(data.frame(scores[, choices]), MARGIN=2,
      function(x) tapply(x,as.factor(clsAsgn),mean))
    x <- t(t(scores[, choices]) / lam)
    cntrs <- t(t(cntrs) / lam)
    y <- t(t(pc$rotation[, choices]) * lam)
    n <- nrow(pc)
    ncls <- nrow(cntrs)
    p <- nrow(y)
    if (missing(xlabs)) {
	  xlabs <- dimnames(x)[[1]]
	  if (is.null(xlabs)) xlabs <- 1:n
    }
    xlabs <- as.character(xlabs)
    dimnames(x) <- list(xlabs, dimnames(x)[[2]])
    if (missing(ylabs)) {
	  ylabs <- dimnames(y)[[1]]
	  if (is.null(ylabs)) ylabs <- paste("Var", 1:p)
    }
    ylabs <- as.character(ylabs)
    dimnames(y) <- list(ylabs, dimnames(y)[[2]])

    if (length(cex) == 1) cex <- c(cex, cex)
    if (missing(col)) {
	  col <- par("col")
	  if (!is.numeric(col)) col <- match(col, palette(), nomatch=1)
	  col <- c(col, col + 1)
    } else {
	  if(length(col) == 1) {
	    col <- c(col, col)
      } else {
	    col <- col
	  }
	}

    unsigned.range <- function(x) c(-abs(min(x)), abs(max(x)))
    rangx1 <- unsigned.range(x[, 1])
    rangx2 <- unsigned.range(x[, 2])
    rangy1 <- unsigned.range(y[, 1])
    rangy2 <- unsigned.range(y[, 2])

    if (missing(xlim) && missing(ylim)) {
	  xlim <- ylim <- rangx1 <- rangx2 <- range(rangx1, rangx2)
	} else {
	  if(missing(xlim)) {
	    xlim <- rangx1
	  } else {
	    if(missing(ylim)) {
		  ylim <- rangx2
		}
	  }
	}
    ratio <- max(rangy1/rangx1, rangy2/rangx2)/expand
    on.exit(par(op))
    op <- par(pty = "s")
    if (!is.null(main)) {
        op <- c(op, par(mar = par("mar")+c(0,0,1,0)))
	}
    plot(x, type = "n", xlim = xlim, ylim = ylim, col = col[1],
         xlab = xlab, ylab = ylab, sub = sub, main = main, ...)
    if (data.pts) text(x, xlabs, cex = cex[1], col = col[1], ...)
    if (centroids) text(cntrs, as.character(1:ncls), col="blue", cex=1.5)

    par(new = TRUE)
    plot(y, axes = FALSE, type = "n", xlim = xlim*ratio, ylim = ylim*ratio,
	 xlab = "", ylab = "", col = col[1], ...)
    axis(3, col = col[2])
    axis(4, col = col[2])
    box(col = col[1])
    text(y, labels=ylabs, cex = cex[2], col = "red", ...)
    if(var.axes) {
	  arrows(0, 0, y[,1] * 0.8, y[,2] * 0.8, col = "red", length=arrow.len)
	}
    invisible()
}


#' Standardize a matrix
#'
#' The function takes a matrix and two standardization vectors and standardizes
#' the variables based on subtracting the first standardization vector from the
#' appropriate columns and then dividing by the second standarization parameter.
#' @param x matrix to standardize
#' @param std.vec1 vector to subtract from columns
#' @param std.vec2 vector to divide the columns
#' @author Dan Putler
#' @export
standardize <- function(x, std.vec1, std.vec2) {
  # Input checking
  if (!all(names(std.vec1) == names(std.vec2)))
    stop.Alteryx2("The two standarization vectors must have the same names")
  if (!all(dimnames(x)[[2]] == names(std.vec1)))
    stop.Alteryx2("The data matrix and the standaridization vectors must have the same names")
  x <- sweep(x, 2, std.vec1)
  x <- sweep(x, 2, std.vec2, FUN="/")
  x
}




#' Bootstrap Replicates of the Calinski-Harabas index for Cluster Validation
#'
#' This function calculates the Calinski-Harabas index for all solutions
#' contained in a bootFlexclust object. It does it for each Rand test paired
#' comparison, so 100 bootstrap replicates of the Rand index will result in 200
#' Calinski-Harabase index values. This code borrows from the function index.G1
#' of Marek Walesiak and Andrzej Dudek's clusterSim package in terms of
#' implementing the sum of squares components
#' @param xdat A numeric matrix of the data to be clustered.
#' @param k_vals An integer vector giving the set of clustering solutions to be
#'   examined.
#' @param clstr1 The cluster assignments from a bootFlexclust object for one
#'   side of the Rand index paired comparisons.
#' @param clstr2 The cluster assignments from a bootFlexclust object for the
#'   other side of the Rand index paired comparisons
#' @param cntrs1 The cluster centers from a bootFlexclust object for one side of
#'   the bootFlexclust Rand index paired comparisons.
#' @param cntrs2 The cluster centers from a bootFlexclust object for the other
#'   side of the bootFlexclust Rand index paired comparisons.
#' @param method The clustering method, one of "kmn" (K-Means), "kmd"
#'   (K-Medians), and "neuralgas" (neural gas).
#' @author Dan Putler
#' @export
bootCH <- function(xdat, k_vals, clstr1, clstr2, cntrs1, cntrs2,
  method = c("kmn", "kmd", "neuralgas")) {
  method = match.arg(method)
  if(method == "kmd") all_centers <- apply(xdat, 2, median)
  else all_centers <- apply(xdat, 2, mean)
  all_dif <- sweep(xdat, 2, all_centers,"-")
  tss <- sum(all_dif^2)
  n_solu <- dim(clstr1)[2] # total number of cluster solutions considered
  nboot <- dim(clstr1)[3]
  n_obs <- dim(clstr1)[1]
  ch_mat <- matrix(NA, nrow=2*nboot, ncol=n_solu)
  for(k_ind in 1:n_solu) {
    cent_array1 <- cntrs1[[k_ind]]
    cent_array2 <- cntrs2[[k_ind]]
    cls_asgn_m1 <- clstr1[,k_ind,]
    cls_asgn_m2 <- clstr2[,k_ind,]
    k <- k_vals[k_ind]
    ch_reps1 <- rep(NA, nboot)
    ch_reps2 <- rep(NA, nboot)
    for(b_ind in 1:nboot) {
      clus1 <- cls_asgn_m1[,b_ind]
      clus2 <- cls_asgn_m2[,b_ind]
      centrds1 <- cent_array1[,,b_ind]
      centrds2 <- cent_array2[,,b_ind]
      wss1a <- (xdat - centrds1[clus1,])^2
      wss2a <- (xdat - centrds2[clus2,])^2
      wss1 <- 0
      wss2 <- 0
      for(m in 1:k) {
        wss1 <- wss1 + sum(wss1a[clus1 == m,])
        wss2 <- wss2 + sum(wss2a[clus2 == m,])
      }
      bss1 <- tss - wss1
      bss2 <- tss - wss2
      ch_reps1[b_ind] <- ((n_obs - k)/(k - 1))*(bss1/wss1)
      ch_reps2[b_ind] <- ((n_obs - k)/(k - 1))*(bss2/wss2)
    }
    ch_mat[,k_ind] <- c(ch_reps1, ch_reps2)
    dimnames(ch_mat)[[2]] <- as.character(k_vals)
  }
  return(ch_mat)
}


# The function varImpPlot.Alteryx is a modified version of the randomForest
# varImpPlot() function to only do a single plot related to model error as
# opposed to the Gini or leaf purity
# Author: Dan Putler
varImpPlot.Alteryx <- function(x, sort=TRUE, n.var=min(30, nrow(x$importance)),
  type=NULL, class=NULL, scale=TRUE, main="Variable Importance Plot", ...) {
  if(!inherits(x, "randomForest")) {
    stop.Alteryx2("This function only works for objects of class `randomForest'")
  }
  imp <- randomForest::importance(x, class=class, scale=scale, type=type, ...)
  print(colnames(imp))
  # If there are two or more columns, just use the second to last column
  if(ncol(imp) > 1) {
    the_measure <- colnames(imp)[(ncol(imp) - 1)]
    imp <- as.vector(imp[, (ncol(imp) - 1)])
  } else {
    the_measure <- colnames(imp)[1]
    imp <- as.vector(imp[,1])
  }
  ord <- if(sort) rev(order(imp, decreasing=TRUE)[1:n.var]) else 1:n.var
  xmin <- if (the_measure %in%
    c("IncNodePurity", "MeanDecreaseGini")) 0 else min(imp[ord])
  print(colnames(imp))
  dotchart(imp[ord], xlab=the_measure, ylab="", xlim=c(xmin, max(imp)), ...)
  invisible(imp)
}

#' Computes width, height and dpi for a graph output
#'
#' This takes information about the desired graph size (in
#' inches or centemeters), the desired output resolution of 1x, 2x, 3x from a
#' base of 96 dpi, and and (in the case of 3x) whether high (576 dpi) resolution
#' should be used to determine the appropriate width, height, and res values to
#' provide the AlteryxGraph device.
#'
#' @param inches boolean indicating whether to measure dimensions in inches
#' @param in.w the width of the plot to produce in inches.
#' @param in.h the height of the plot to produce in inches.
#' @param cm.w the width of the plot to produce in cm.
#' @param cm.h the height of the plot to produce in cm.
#' @param graph.resolution the resolution of the plot to produce as a multiple
#'   of 96dpi
#' @param print.high whether to turn 3x into 6x (high, 576 dpi) resolution
#' @author Dan Putler
#' @export
graphWHR2 <- function(inches = TRUE, in.w = 5.5, in.h = 5.75,
   cm.w = NULL, cm.h = NULL, graph.resolution = c("1x", "2x", "3x"),
   print.high = FALSE){
  # Set the dpi
  graph.resolution <- match.arg(graph.resolution)
  dpi <- 96
  if (graph.resolution == "2x") dpi <- 192
  if (graph.resolution == "3x" && !print.high) dpi <- 288
  if (graph.resolution == "3x" && print.high) dpi <- 576

  # Set the width and height
  if (!inches) {
    in.w <- 0.393701*cm.w
    in.h <- 0.393701*cm.h
  }
  width = round(dpi*in.w)
  height = round(dpi*in.h)
  c(width, height, dpi)
}


#' Wrap a data frame object into a table.
#'
#' The function wrapTable takes a data frame object, or an object that can be
#' coerced to be a data frame (such as a matrix) and breaks it into two or more
#' data frames, each with a fixed number of columns, but which collectively have
#' all the columns of the original table. The rows of the corresponding tables
#' are then converted to pipe delimited strings, and the full collection of rows
#' is returned as a character vector.
#'
#' @param dframe A data frame (or an object that can be coerced to a data frame)
#' @param width The maximum number of columns to include in each table. If the
#'   first column contains rows names, this column is included in the width of
#'   the table.
#' @param first.rnames A logical value which indicates if the first column of
#'   the table contains row names that should be included in all the new tables
#'   as the first column of each table.
#' @param round.level level of rounding.
#' @return The function returns a character vector, each element of which
#'   contains a row of the table, including the column names, with the cell
#'   values in each row delimited by pipes.
#' @author Dan Putler
#' @export
wrapTable <- function(dframe, width = 6, first.rnames = TRUE, round.level = 0.0000005) {
  if (!is.data.frame(dframe))
    dframe <- as.data.frame(dframe)
  out.vec <- NULL
  if (ncol(dframe) <= width) {
    if (first.rnames)
      the.names <- paste(c("  ", names(dframe)[2:ncol(dframe)]), collapse = "|")
    else
      the.names <- paste(names(dframe), collapse = "|")
    out.vec <- c(the.names, matrixPipeDelim(dframe, round.level = round.level))
    return(out.vec)
  }
  if (first.rnames) {
    width <- width - 1
    ntables <- ceiling((ncol(dframe) - 1)/width)
    start.col <- 2
    end.col <- width + 1
    for (i in 1:ntables) {
      this.df <- dframe[, c(1, start.col:end.col)]
      the.names <- paste(c("  ", names(this.df)[2:ncol(this.df)]), collapse = "|")
      this.vec <- c(the.names, matrixPipeDelim(this.df, round.level = round.level))
      out.vec <- c(out.vec, this.vec)
      start.col <- i*width + 2
      if (i + 1 == ntables)
        end.col <- ncol(dframe)
      else
        end.col <- (i + 1)*width + 1
    }
  } else {
    ntables <- ceiling(ncol(dframe)/width)
    start.col <- 1
    end.col <- width
    for (i in 1:ntables) {
      if (start.col < end.col) {
        this.df <- dframe[, start.col:end.col]
        this.vec <- c(paste(names(this.df), collapse = "|"),
                      matrixPipeDelim(this.df, round.level = round.level))
        out.vec <- c(out.vec, this.vec)
      } else {
        this.vec <- c(names(dframe)[start.col], dframe[, start.col])
        out.vec <- c(out.vec, this.vec)
      }
      start.col <- i * width + 1
      if (i + 1 == ntables)
        end.col <- ncol(dframe)
      else end.col <- (i + 1) * width
    }
  }
  names(out.vec) <- NULL
  out.vec
}

#' GraphWHR Function
#'
#' The function graphWH takes information about the desired graph size (in
#' inches or centemeters), the desired output resolution of 1x, 2x, 3x from a
#' base of 96 dpi, and and (in the case of 3x) whether high (576 dpi) resolution
#' should be used to determine the appropriate width, height, and res values to
#' provide the AlteryxGraph device.
#' @param inches A flag whether the to measure dimensions using inches (as opposed to centimeters).
#' @param in.w The width of the plot to produce in inches.
#' @param in.h The height of the plot to produce in inches.
#' @param cm.w The width of the plot to produce in centimeters.
#' @param cm.h The height of the plot to produce in centimeters.
#' @param resolution The resolution of the plot to produce expressed as multiple of 96 ppi.
#' @param print.high Whether to actually turn 3x into 6x (high, 576 ppi) resolution.
#' @export
#' @author Dan Putler
graphWHR <- function(inches = c("True", "False"), in.w , in.h, cm.w = NULL,
    cm.h = NULL, resolution = c("1x", "2x", "3x"), print.high = FALSE) {
  # Set the dpi
  if (resolution == "")
    resolution <- "1x"
  else
    resolution <- match.arg(resolution)
  dpi <- 96
  if (resolution == "2x")
    dpi <- 192
  if (resolution == "3x" && !print.high)
    dpi <- 288
  if (resolution == "3x" && print.high)
    dpi <- 576

  # Set the width and height
  if (inches == "")
    inches <- "True"
  else
    inches <- match.arg(inches)

  if (in.w == "")
    in.w <- "5.50"
  in.w <- as.numeric(in.w)

  if (in.h == "")
    in.h <- "5.75"
  in.h <- as.numeric(in.h)

  if (inches == "False") {
    in.w <- 0.393701*as.numeric(cm.w)
    in.h <- 0.393701*as.numeric(cm.h)
  }
  width = round(dpi*in.w)
  height = round(dpi*in.h)
  c(width, height, dpi)
}


#' Create a valid name for an R object
#'
#' The function validName takes an R object name as its argument, then and (1)
#' determines if it is a valid R object name and (2) if not, make it one.
#' @param name name of the object
#' @author Dan Putler
#' @export
validName <- function(name) {
  if (!is.character(name) || length(name) > 1)
    stop.Alteryx2("The name provided needs to be a character string")
  if (name != make.names(name)) {
    old.name <- name
    name <- gsub("\\.", "\\_", make.names(name))
    AlteryxMessage2(
      paste("The invalid name", old.name, "has been replaced by", name),
        2, 2, 0
    )
  }
  name
}

