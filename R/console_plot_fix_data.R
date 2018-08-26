console.plot.fix.data <- function(input.data, input.lim.lower, input.lim.upper,
                                  final.range) {
  lim.range <- seq(input.lim.lower, input.lim.upper, length.out = final.range)
  out.data <- numeric(length(input.data))
  for (i in seq_along(input.data)) {
    tmp.all <- sort(c(input.data[i], lim.range))
    out.data[i] <- which(tmp.all == input.data[i])[1]
  }
  out.data
}
