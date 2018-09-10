console.plot.axis <- function(plot.lines, plot.width, plot.height, ylim, xlim,
                              s) {

  plot.lines <- paste0(plot.lines, " ", s$vert)

  plot.lines[1] <- paste0(" ", s$hori, s$vert.left, " ", plot.lines[1])

  plot.lines[length(plot.lines)] <- paste0(" ", s$hori, s$vert.left, " ",
                                          plot.lines[length(plot.lines)])

  mid.y <- round(plot.height / 2 + 0.01)

  plot.lines[mid.y] <- paste0(" ", s$hori, s$vert.left, " ", plot.lines[mid.y])

  mid.y1 <- round(plot.height * 0.25)
  if (mid.y1 == 1) mid.y1 <- 2
  mid.y3 <- round(plot.height * 0.75)

  plot.lines[mid.y1] <- paste0("  ", s$vert.left, " ", plot.lines[mid.y1])

  plot.lines[mid.y3] <- paste0("  ", s$vert.left, " ", plot.lines[mid.y3])

  yaxis.skip <- c(1, length(plot.lines), mid.y, mid.y1, mid.y3)

  plot.lines[-yaxis.skip] <- paste0(paste0("  ", s$vert, " "),
                                    plot.lines[-yaxis.skip])

  mid.x <- round(plot.width / 2 + 0.01)
  mid.x1 <- round(plot.width * 0.25)
  mid.x3 <- round(plot.width * 0.75)

  xaxis <- paste0(paste0("  ", s$corner.bot.left, s$hori, s$hori.down),
                  paste(rep(s$hori, mid.x1 - 1), collapse = ""),
                  s$hori.down,
                  paste(rep(s$hori, mid.x - mid.x1 - 1), collapse = ""),
                  s$hori.down,
                  paste(rep(s$hori, mid.x3 - mid.x - 1), collapse = ""),
                  s$hori.down,
                  paste(rep(s$hori, plot.width - mid.x3 - 2), collapse = ""),
                  paste0(s$hori.down, s$hori, s$corner.bot.right))

  xaxis2 <- paste0("    ", s$tick.x.minor,
                   paste(rep(" ", mid.x1 - 1), collapse = ""), " ",
                   paste(rep(" ", mid.x - mid.x1 - 1), collapse = ""),
                   s$tick.x.minor,
                   paste(rep(" ", mid.x3 - mid.x - 1), collapse = ""), " ",
                   paste(rep(" ", plot.width - mid.x3 - 2), collapse = ""),
                   s$tick.x.minor)

  plot.lines <- c(plot.lines, xaxis, xaxis2)
  plot.top <- paste0("  ", s$corner.top.left, paste(rep(s$hori, plot.width + 2),
                                                    collapse = ""), s$corner.top.right)
  plot.lines <- c(plot.top, plot.lines)

  # add axis scales

  xaxis.1 <- formatC(xlim[1], digits = 3)
  xaxis.2 <- formatC(mean(xlim), digits = 3)
  xaxis.3 <- formatC(xlim[2], digits = 3)

  xaxis.scale <- c(format(as.character(xaxis.1), width = 5, justify = "centre"),
                   format(as.character(xaxis.2), width = 5, justify = "centre"),
                   format(as.character(xaxis.3), width = 5, justify = "centre"))
  yaxis.scale <- c(gsub(" ", "", formatC(ylim[1], digits = 4, width = 5)),
                   gsub(" ", "", formatC(mean(ylim), digits = 4, width = 5)),
                   gsub(" ", "", formatC(ylim[2], digits = 4, width = 5)))

  y.offset <- 8
  y.offset.scale <- y.offset - nchar(yaxis.scale)

  xaxis.scale.filler <- c("  ",
                          paste(rep(" ", mid.x - 5),
                                collapse = ""),
                          paste(rep(" ", mid.x - 6),
                                collapse = ""))
  xaxis.scale <- paste0(xaxis.scale.filler[1], xaxis.scale[1],
                        xaxis.scale.filler[2], xaxis.scale[2],
                        xaxis.scale.filler[3], xaxis.scale[3])

  yaxis.scale <- c(paste0(paste(rep(" ", 2 + y.offset.scale[3]), collapse = ""),
                          yaxis.scale[3]),
                   paste0(paste(rep(" ", 2 + y.offset.scale[2]), collapse = ""),
                          yaxis.scale[2]),
                   paste0(paste(rep(" ", 2 + y.offset.scale[1]), collapse = ""),
                          yaxis.scale[1]))

  plot.lines[2] <- paste0(yaxis.scale[1], plot.lines[2])
  plot.lines[1 + mid.y] <- paste0(yaxis.scale[2], plot.lines[1 + mid.y])
  plot.lines[length(plot.lines) - 2] <- paste0(yaxis.scale[3],
                                               plot.lines[length(plot.lines) - 2])

  for (i in seq_along(plot.lines)) {
    if (i %in% c(2, 1 + mid.y, length(plot.lines) - 2)) next
    plot.lines[i] <- paste0(paste(rep(" ", max(nchar(yaxis.scale))), collapse = ""),
                            plot.lines[i])
  }

  plot.lines <- c(plot.lines, paste0(paste(rep(" ", max(nchar(yaxis.scale))), collapse = ""),
                                     xaxis.scale))

  plot.lines

}
