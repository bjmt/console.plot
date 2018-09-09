console.plot.types <- function(x, y, groups, plot.width, plot.height, point,
                               type, line, abline.x, abline.y,
                               abline.overlay, s) {

  plot.lines <- paste(rep(" ", plot.width), collapse = "")
  plot.lines <- rep(plot.lines, plot.height)

  y <- plot.height - y + 1  # not entirely sure why I have to do this
  
  if (type %in% c("p", "b", "h") && !type %in% c("s", "S")) {
    for (i in seq_along(x)) {
      substr(plot.lines[y[i]], x[i], x[i]) <- point[groups[i]]
    }
  }

  if (type == "h") {
    for (i in seq_along(x)) {

      h.index.i <- c(x[i], y[i])

      for (j in seq(h.index.i[2], length(plot.lines))) {

        if (length(substr(plot.lines[j], h.index.i[1], h.index.i[1])) > 0) {

          if (substr(plot.lines[j], h.index.i[1], h.index.i[1]) == " ") {

            substr(plot.lines[j], h.index.i[1], h.index.i[1]) <- s$vert

          }

        }

      }

    }
  }

  if (type %in% c("s", "S")) {
    for (i in seq_along(unique(groups))) {
    
      x.i <- x[groups == unique(groups)[i]]
      y.i <- y[groups == unique(groups)[i]]

      for (j in seq_along(x.i)[-length(x.i)]) {
      
        x.i.1 <- x.i[j]
        x.i.2 <- x.i[j + 1]
        y.i.1 <- y.i[j]
        y.i.2 <- y.i[j + 1]

        if (type == "s") {
        
          if (abs(x.i.1 - x.i.2) > 0) {
            substr(plot.lines[y.i.1], x.i.1,
                             x.i.2) <- paste(rep(s$hori, abs(x.i.1 - x.i.2)),
                                             collapse = "")
            if (x.i.1 != 1 && y.i.1 > y.i.2) substr(plot.lines[y.i.1], x.i.1,
                                   x.i.1) <- s$corner.top.left
            if (x.i.1 != 1 && y.i.1 < y.i.2) substr(plot.lines[y.i.1], x.i.1,
                                   x.i.1) <- s$corner.bot.left
          }

          if (abs(y.i.1 - y.i.2) > 0) {

            if (y.i.1 > y.i.2) substr(plot.lines[y.i.1], x.i.2,
                                                x.i.2) <- s$corner.bot.right
            else  substr(plot.lines[y.i.1], x.i.2,
                                    x.i.2) <- s$corner.top.right

            for (m in seq(y.i.1, y.i.2)[-c(1, y.i.1 + y.i.2)]) {
              substr(plot.lines[m], x.i.2, x.i.2) <- s$vert
            }
          }
        
        } else if (type == "S") {
        
          if (abs(x.i.1 - x.i.2) > 0) {
            substr(plot.lines[y.i.2], x.i.1, x.i.2) <- paste(rep(s$hori, abs(x.i.1 - x.i.2)),
                                                             collapse = "")
          }

          if (abs(y.i.1 - y.i.2) > 0) {
            for (m in seq(y.i.1, y.i.2)[-c(1, y.i.1 + y.i.2)]) {
              substr(plot.lines[m], x.i.1, x.i.1) <- s$vert
            }
          }
          if (x.i.1 == 1 && y.i.1 < y.i.2) substr(plot.lines[y.i.2], x.i.1,
                                 x.i.1) <- s$corner.bot.left
          if (x.i.1 == 1 && y.i.1 > y.i.2) substr(plot.lines[y.i.2], x.i.1,
                                 x.i.1) <- s$corner.top.left
          if (x.i.1 != 1 && y.i.1 > y.i.2) substr(plot.lines[y.i.2], x.i.1,
                                 x.i.1) <- s$corner.top.left
          if (x.i.1 != 1 && y.i.1 < y.i.2) substr(plot.lines[y.i.2], x.i.1,
                                 x.i.1) <- s$corner.bot.left
        
        }
      
      }
    
    }
    for (i in seq_along(x)) {
      substr(plot.lines[y[i]], x[i], x[i]) <- point[groups[i]]
    }
  }

  if (type %in% c("l", "b")) {
    for (i in seq_along(unique(groups))) {

      what <- line[i]

      x.i <- x[groups == unique(groups)[i]]
      y.i <- y[groups == unique(groups)[i]]

      for (j in seq_along(x.i)[-length(x.i)]) {
      
        x.i.1 <- x.i[j]
        x.i.2 <- x.i[j + 1]
        y.i.1 <- y.i[j]
        y.i.2 <- y.i[j + 1]

        x.y.new <- approx(c(x.i.1, x.i.2), c(y.i.1, y.i.2),
                          n = plot.width * plot.height)
        x.y.new <- unique(data.frame(x.y.new[[1]], x.y.new[[2]]))
        x.i.between <- round(x.y.new[, 1])
        y.i.between <- round(x.y.new[, 2])

        x.i.all <- c(x.i.1, x.i.between, x.i.2)
        y.i.all <- c(y.i.1, y.i.between, y.i.2)

        xy.i.all <- unique(data.frame(x.i.all, y.i.all))
        x.i.all <- xy.i.all[, 1]
        y.i.all <- xy.i.all[, 2]

        add.line <- function(plot.lines, m, what) {
          substr(plot.lines[y.i.all[m]], x.i.all[m], x.i.all[m]) <- what
          plot.lines
        }
        
        for (m in seq_along(x.i.all)) {
          if (type %in% c("b")) {

            if (length(substr(plot.lines[y.i.all[m]], x.i.all[m], x.i.all[m])) > 0) {

              if (substr(plot.lines[y.i.all[m]], x.i.all[m], x.i.all[m]) == " ") {

                plot.lines <- add.line(plot.lines, m, what)

              }

            }

          } else if (type == "l") {

            if (length(substr(plot.lines[y.i.all[m]],
                              x.i.all[m], x.i.all[m])) > 0) {

              if (substr(plot.lines[y.i.all[m]], x.i.all[m], x.i.all[m]) %in%
                  c(" ", point)) {

                plot.lines <- add.line(plot.lines, m, what)

              }

            }

          }
        }

      }

    }
  }

  if (!is.null(abline.y)) {
    if (abline.overlay) {
      plot.lines[abline.y] <- paste(rep(s$hori, plot.width), collapse = "")
    } else {
      for (i in seq_len(plot.width)) {
        if (substr(plot.lines[abline.y], i, i) == " ") {
          substr(plot.lines[abline.y], i, i) <- s$hori
        }
      }
    }
  }

  if (!is.null(abline.x)) {
    for (i in seq_len(plot.height)) {
      if (!abline.overlay) {
        if (substr(plot.lines[i], abline.x, abline.x) == s$hori) {
          substr(plot.lines[i], abline.x, abline.x) <- s$cross
        }
        if (substr(plot.lines[i], abline.x, abline.x) != " ") next
      }
      substr(plot.lines[i], abline.x, abline.x) <- s$vert
    }
  }

  plot.lines

}
