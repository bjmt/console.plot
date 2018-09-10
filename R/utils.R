get_symbols <- function(ascii = FALSE) {

  if (!ascii) {

    all.symbols <- c(8226, 215, 43, 8718, 9670, 9650, 9744, 9671, 9651, 9737,
                     9733, 9734, 10035, 9746, 8865, 8857, 8853, 10023)
    all.symbols <- sapply(all.symbols, intToUtf8)
    all.symbols <- c(all.symbols, "o", "@", "#", "$", "%", "&", ">", "<", "?",
                     letters[-15])

    all.lines <- c(9475, 9479, 9483, 9551, 9553, 9474, 9478, 9482, 9550)
    all.lines <- sapply(all.lines, intToUtf8)
    all.lines <- c(all.lines, "|", "-", "/", "\\", ".", ",", "~")

    vert <- intToUtf8(9474)
    vert.left <- intToUtf8(9508)
    vert.right <- intToUtf8(9500)
    hori <- intToUtf8(9472)
    hori.down <- intToUtf8(9516)
    hori.up <- intToUtf8(9524)
    corner.bot.left <- intToUtf8(9492)
    corner.bot.right <- intToUtf8(9496)
    corner.top.left <- intToUtf8(9484)
    corner.top.right <- intToUtf8(9488)
    tick.x.minor <- intToUtf8(9589)
    cross <- intToUtf8(9532)

  } else {

    all.symbols <- c("o", "@", "#", "$", "%", "&", ">", "<", "?",
                     letters[-15])

    all.lines <- c("|", "-", "/", "\\", ".", ",", "~")

    vert <- "|"
    vert.left <- "|"
    vert.right <- "|"
    hori <- "-"
    hori.down <- "-"
    hori.up <- "-"
    corner.bot.left <- "-"
    corner.bot.right <- "-"
    corner.top.left <- "-"
    corner.top.right <- "-"
    tick.x.minor <- "|"
    cross <- "+"

  }

  list(all.symbols = all.symbols, all.lines = all.lines, vert.right = vert.right,
       vert = vert, vert.left = vert.left, hori = hori, hori.down = hori.down,
       hori.up = hori.up, corner.bot.left = corner.bot.left,
       corner.bot.right = corner.bot.right, corner.top.left = corner.top.left,
       corner.top.right = corner.top.right, tick.x.minor = tick.x.minor,
       cross = cross)

}
