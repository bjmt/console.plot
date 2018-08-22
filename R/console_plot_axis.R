console.plot.axis <- function(plot.lines, plot.width, plot.height, ylim, xlim) {

  plot.lines <- paste0(plot.lines, " ", intToUtf8(0x2502))

  plot.lines[1] <- paste0(" ", intToUtf8(0x2500), intToUtf8(0x2524), " ", plot.lines[1])
  plot.lines[length(plot.lines)] <- paste0(" ", intToUtf8(0x2500), intToUtf8(0x2524),
                                           " ", plot.lines[length(plot.lines)])

  mid.y <- round(plot.height / 2)
  plot.lines[mid.y] <- paste0(" ", intToUtf8(0x2500), intToUtf8(0x2524), " ",
                              plot.lines[mid.y])

  mid.y1 <- round(plot.height * 0.25)
  mid.y3 <- round(plot.height * 0.75)
  plot.lines[mid.y1] <- paste0("  ", intToUtf8(0x2524), " ", plot.lines[mid.y1])
  plot.lines[mid.y3] <- paste0("  ", intToUtf8(0x2524), " ", plot.lines[mid.y3])

  yaxis.skip <- c(1, length(plot.lines), mid.y, mid.y1, mid.y3)

  plot.lines[-yaxis.skip] <- paste0(paste0("  ", intToUtf8(0x2502), " "),
                                    plot.lines[-yaxis.skip])

  mid.x <- round(plot.width / 2)
  mid.x1 <- round(plot.width * 0.25)
  mid.x3 <- round(plot.width * 0.75)

  xaxis <- paste0(paste0("  ", intToUtf8(0x2514), intToUtf8(0x2500),
                         intToUtf8(0x252c)),
                  paste(rep(intToUtf8(0x2500), mid.x1 - 1), collapse = ""),
                  intToUtf8(0x252c),
                  paste(rep(intToUtf8(0x2500), mid.x - mid.x1 - 1), collapse = ""),
                  intToUtf8(0x252c),
                  paste(rep(intToUtf8(0x2500), mid.x3 - mid.x - 1), collapse = ""),
                  intToUtf8(0x252c),
                  paste(rep(intToUtf8(0x2500), plot.width - mid.x3 - 2), collapse = ""),
                  paste0(intToUtf8(0x252c), intToUtf8(0x2500), intToUtf8(0x2518)))

  xaxis2 <- paste0("    ", intToUtf8(0x2575), 
                   paste(rep(" ", mid.x1 - 1), collapse = ""), " ",
                   paste(rep(" ", mid.x - mid.x1 - 1), collapse = ""), 
                   intToUtf8(0x2575),
                   paste(rep(" ", mid.x3 - mid.x - 1), collapse = ""), " ",
                   paste(rep(" ", plot.width - mid.x3 - 2), collapse = ""),
                   intToUtf8(0x2575))

  plot.lines <- c(plot.lines, xaxis, xaxis2)
  plot.top <- paste0("  ", intToUtf8(0x250c),
                     paste(rep(intToUtf8(0x2500), plot.width + 2), collapse = ""),
                     intToUtf8(0x2510))
  plot.lines <- c(plot.top, plot.lines)

  # add axis scales

  xaxis.scale <- c(gsub(" ", "", formatC(xlim[1], digits = 4, width = 5)),
                   gsub(" ", "", formatC(mean(xlim), digits = 4, width = 5)),
                   gsub(" ", "", formatC(xlim[2], digits = 4, width = 5)))
  yaxis.scale <- c(gsub(" ", "", formatC(ylim[1], digits = 4, width = 5)),
                   gsub(" ", "", formatC(mean(ylim), digits = 4, width = 5)),
                   gsub(" ", "", formatC(ylim[2], digits = 4, width = 5)))

  y.offset <- 8
  y.offset.scale <- y.offset - nchar(yaxis.scale)

  xaxis.scale.filler <- c("    ",
                          paste(rep(" ", mid.x - sum(nchar(xaxis.scale[1:2])) +
                                         round(nchar(xaxis.scale[2]) / 2)),
                                collapse = ""),
                          paste(rep(" ", mid.x - nchar(xaxis.scale[3])),
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
