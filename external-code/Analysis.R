localMinima <- function(x) {
  y <- diff(c(Inf, x)) < 0L
  rle(y)$lengths
  y <- cumsum(rle(y)$lengths)
  y <- y[seq.int(1L, length(y), 2L)]
  if (x[[1]] == x[[2]]) {
    y <- y[-1]
  }
  y
}

localMaxima <- function(x) {
  y <- diff(c(-Inf, x)) > 0L
  rle(y)$lengths
  y <- cumsum(rle(y)$lengths)
  y <- y[seq.int(1L, length(y), 2L)]
  if (x[[1]] == x[[2]]) {
    y <- y[-1]
  }
  y
}

analysis <- function(x) {
  min = localMinima(x)
  max = localMaxima(x)

  temp = c(min, max)

  inflectionPts = temp[order(temp)]
  trends = vector(mode = 'numeric', length = length(inflectionPts) - 1)
  trendPct = vector(mode = 'numeric', length = length(inflectionPts) - 1)

  prevPoint = -1
  i = 0

  for(point in inflectionPts) {
    if(prevPoint != -1) {

      price = x[point]
      prevPrice = x[prevPoint]

      trends[i] = price - prevPrice
      trendPct[i] = signif(trends[i]/prevPrice, 2)
      i = i + 1
    }
    prevPoint = point
  }

  return(trendPct)
}

smoothener <- function(x) {
  avg = mean(abs(x))

  for(point in x) {
    if(abs(point) < avg) {
      x = 0
    }
  }

  return(x)
}