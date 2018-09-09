#' Make R plots in the console.
#'
#' @param x Numeric vector of data.
#' @param y Numeric vector of data.
#' @param groups Character/factor/integer values representing group.
#' @param main Title.
#' @param file Print to stdout or to file.
#' @param xlab X label.
#' @param ylab Y label.
#' @param type Plot type (p, l, b, h, s, S)
#' @param point Point symbols.
#' @param line Line symbols.
#' @param abline.x Draw a vertical line.
#' @param abline.y Draw a horizontal line.
#' @param abline.overlay Allow abline to hide data points/line.
#' @param horizontal Flip axes.
#' @param xlim Min and max for x axis.
#' @param ylim Min and max for y axis.
#' @param plot.width Width of plot.
#' @param plot.height Height of plot.
#' @param legend Show legend.
#' @param ascii Whether to draw plot using only ASCII characters.
#' @param output How to draw plot.
#'
#' @return Returns \code{NULL}, invisibly.
#'
#' @author Benjamin Tremblay, \email{b2tremblay@@uwaterloo.ca}
#' @export
console.plot <- function(x, y = NULL, groups = NULL, main = NULL, file = "",
                         xlab = NULL, ylab = NULL, type = "p", point = NULL,
                         line = NULL, abline.x = NULL, abline.y = NULL,
                         abline.overlay = FALSE, horizontal = FALSE,
                         xlim = NULL, ylim = NULL, plot.width = NULL,
                         plot.height = NULL, legend = NULL,
                         ascii = getOption("ascii"), output = "cat") {

  # types: p=point, l=line, b=line+point, h=point with vertical downward line,
  #        s=staircase, S=inverase staircase

  if (!type %in% c("p", "l", "b", "h", "s", "S")) warning("unknown 'type'")

  if (is.null(plot.width)) plot.width <- as.integer(options("width")$width / 1.35)
  if (is.null(plot.height)) plot.height <- as.integer(plot.width / 2)

  all.symbols <- c()
  if (!ascii) {
    all.symbols <- c(8226, 215, 43, 8718, 9670, 9650, 9744, 9671, 9651, 9737,
                     9733, 9734, 10035, 9746, 8865, 8857, 8853, 10023)
    all.symbols <- sapply(all.symbols, intToUtf8)
  }
  all.symbols <- c(all.symbols, "o", "@", "#", "$", "%", "&", ">", "<", "?",
                   letters[-15])

  all.lines <- c()
  if (!ascii) {
    all.lines <- c(9475, 9479, 9483, 9551, 9553, 9474, 9478, 9482, 9550)
    all.lines <- sapply(all.lines, intToUtf8)
  }
  all.lines <- c(all.lines, "|", "-", "/", "\\", ".", ",", "~")

  if (is.null(xlab)) xlab <- deparse(substitute(x))
  if (is.null(ylab)) ylab <- deparse(substitute(y))

  if (plot.height < 12) stop("'plot.height' must be greater than 11")
  if (plot.width < 30) stop("'plot.width' must be greater than 29")

  groups.original <- groups
  if (is.null(groups)) groups <- rep(1, length(x)) else {
    groups <- as.integer(as.factor(groups))
    if (length(groups) != length(x))
      stop("'groups' must have the same number of items as 'x', 'y'")
  }

  if (is.null(y)) {
    y <- x
    # x <- seq_len(max(table(x)))
    for (i in seq_along(unique(groups))) {
      m <- 1
      for (j in seq_along(groups[groups == unique(groups)[i]])) {
        x[groups == unique(groups)[i]][j] <- m
        m <- m + 1
      }
    }
    ylab <- xlab
    xlab <- "Index"
  } else if (length(y) != length(x))
    stop("x and y must have the same number of observations")

  # adjust data

  xlim.bot <- min(x)
  xlim.top <- max(x)
  xlim.mid <- (max(x) + min(x)) / 2

  x.original <- x
  y.original <- y

  if (is.null(ylim)) ylim <- c(min(y), max(y)) else {
    x <- x[y >= ylim[1]]
    groups <- groups[y >= ylim[1]]
    y <- y[y >= ylim[1]]
    x <- x[y <= ylim[2]]
    groups <- groups[y <= ylim[2]]
    y <- y[y <= ylim[2]]
  }

  if (is.null(xlim)) xlim <- c(min(x), max(x)) else {
    y <- y[x >= xlim[1]]
    groups <- groups[x >= xlim[1]]
    x <- x[x >= xlim[1]]
    y <- y[x <= xlim[2]]
    groups <- groups[x <= xlim[2]]
    x <- x[x <= xlim[2]]
  }

  x <- console.plot.fix.data(x, xlim[1], xlim[2], plot.width)
  y <- console.plot.fix.data(y, ylim[1], ylim[2], plot.height)
  if (ylim[1] < min(y)) y <- y + 1

  if (!is.null(abline.x)) {
    xlim.range <- seq(xlim[1], xlim[2], length.out = plot.width) + .Machine$double.xmin
    abline.x.t <- sort(c(abline.x, xlim.range))
    abline.x <- which(abline.x.t == abline.x)[1]
    if (abline.x > plot.width ||
        abline.x < min(xlim.range) - .Machine$double.xmin) abline.x <- NULL
  }
  if (!is.null(abline.y)) {
    ylim.range <- seq(ylim[1], ylim[2], length.out = plot.height) + .Machine$double.xmin
    abline.y.t <- sort(c(abline.y, ylim.range))
    abline.y <- which(abline.y.t == abline.y)[1]
    if (abline.y > plot.height ||
        abline.y < min(ylim.range) - .Machine$double.xmin) abline.y <- NULL
    abline.y <- plot.height - abline.y
  }

  # fix data - what about overlapping data points from different groups?

  xy <- data.frame(x = x, y = y)
  xy.keep <- which(!duplicated(xy))
  x <- x[xy.keep]
  y <- y[xy.keep]

  # take care of groups

  groups <- groups[xy.keep]
  groups.original <- groups.original[xy.keep]

  groups.original <- groups.original[!duplicated(groups)]  # use this for legend
  groups.uniq <- unique(groups)
  if (length(groups.uniq) > 30) stop("max number of possible groups is 30")

  if (is.null(point)) point <- all.symbols[seq_along(groups.uniq)] else {
    if (length(point) == 1) point <- rep(point, length(groups.uniq))
    else if (length(point) != length(groups.uniq))
      stop("length of 'point' must be one or equal to number of groups")
  }

  if (is.null(line)) line <- all.lines[seq_along(groups.uniq)] else {
    if (length(line) == 1) line <- rep(line, length(groups.uniq))
    else if (length(line) != length(groups.uniq))
      stop("length of 'line' must be one or equal to number of groups")
  }
  if (type %in% c("l", "b") && length(groups.uniq) > length(line))
    stop("line plots cannot have more ", length(all.lines), " groups")

  # generate plot

  plot.lines <- console.plot.types(x, y, groups, plot.width, plot.height, point,
                                   type, line, abline.x, abline.y, ascii,
                                   abline.overlay)

  # add axis lines

  plot.lines <- console.plot.axis(plot.lines, plot.width, plot.height,
                                  ylim, xlim, ascii)

  # fix abline

  if (!is.null(abline.x) && !ascii) {
    substr(plot.lines[1], abline.x + 14, abline.x + 14) <- intToUtf8(0x252C)
    substr(plot.lines[length(plot.lines) - 2], abline.x + 14,
           abline.x + 14) <- intToUtf8(0x2534)
    if (substr(plot.lines[length(plot.lines) - 1], abline.x + 14,
               abline.x + 14) == intToUtf8(0x2575)) {
      substr(plot.lines[length(plot.lines) - 2], abline.x + 14,
             abline.x + 14) <- intToUtf8(0x253C)
    }
  }
  if (!is.null(abline.y)) {
    if (!ascii) {
      substr(plot.lines[abline.y + 1], 14, 14) <- intToUtf8(0x2500)
      substr(plot.lines[abline.y + 1], 13, 13) <- intToUtf8(0x251C)
      substr(plot.lines[abline.y + 1], plot.width + 15,
             plot.width + 15) <- intToUtf8(0x2500)
      substr(plot.lines[abline.y + 1], plot.width + 16,
             plot.width + 16) <- intToUtf8(0x2524)
      if (substr(plot.lines[abline.y + 1], 12, 12) == intToUtf8(0x2500)) {
        substr(plot.lines[abline.y + 1], 13, 13) <- intToUtf8(0x253C)
      }
    } else {
      substr(plot.lines[abline.y + 1], 14, 14) <- "-"
      substr(plot.lines[abline.y + 1], plot.width + 15, plot.width + 15) <- "-"
    }
  }

  # add title

  if (!is.null(main)) {
    main <- paste0(paste(rep(" ", 13), collapse = ""),
                   main)
    plot.lines <- c("", main, plot.lines, "")
  } else plot.lines <- c("", plot.lines, "")

  # x-y labels

  if (!isFALSE(ylab)) {
    ylab <- strsplit(ylab, "")[[1]]
    ylab.start <- round(length(plot.lines) / 2) - round(length(ylab) / 2)

    for (i in seq_along(ylab)) {
      plot.lines[i + ylab.start - 1] <- paste0("  ", ylab[i], " ",
                                               plot.lines[i + ylab.start - 1])
    }

    plot.lines[-c(seq_along(ylab) +
                  ylab.start - 1)] <- paste0("    ", plot.lines[-c(seq_along(ylab) +
                                                                   ylab.start - 1)])
  }

  if (!isFALSE(xlab)) {
    xlab <- paste0("          ",
                   paste(rep(" ", 8 + round(plot.width / 2) - as.integer(nchar(xlab) / 2)),
                         collapse = ""),
                   xlab)
    plot.lines <- c(plot.lines, xlab, "")
  }

  if (output == "cat") cat(plot.lines, sep = "\n", file = file)
  else if (output == "writeLines") {
    if (file == "" || file == stdout()) con <- stdout() else con <- file(file)
    writeLines(plot.lines, con = con, sep = "\n")
    if (file != "" && file != stdout()) close(con)
  }
  else if (output == "message") message(paste(plot.lines, collapse = "\n"))
  else if (output != "none")
    stop("'output' must be one of 'cat', 'message', 'writeLines', 'none'")

  invisible(plot.lines)

}
