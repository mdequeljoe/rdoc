
# line reflowing - takes ANSI sequences into consideration
# x - character()
# exclude - regular expression to filter lines to exclude from re-flow
reflow_lines <- function(x, exclude = NULL) {

  if (!is.null(exclude))
    exclude <- which(grepl(exclude, x))

  m <- if (length(exclude))
    max(nchar(x[-exclude]))
  else
    max(nchar(x))

  i <- 1
  while (i < length(x)) {

    if (any_blank(x[c(i, i + 1)]) || i %in% exclude) {
      i <- i + 1
      next
    }

    lx <- nchar(strip_style(x[i]))
    lx2 <- nchar(strip_style(x[i + 1]))
    open_space <- m - lx

    if (lx2 <= open_space) {
      x[i] <- paste(x[i], rm_ind(x[i + 1]))
      x <- x[-(i + 1)]
    } else {
      x_ <- rm_ind(x[i + 1])
      sx <- spaces(x_)
      s <- spaces(strip_style(x_))
      s <- s[s <= open_space]
      if (length(s)) {
        s <- sx[which(s == max(s))]
        x[i] <- paste(x[i], substr(x_, 1, s - 1L))
        x[i + 1] <-
          paste0(ind(x[i + 1]),
                 substr(x_, s + 1L, nchar(x_)))
      }
      i <- i + 1
    }
  }
  x
}

any_blank <- function(x) any(!nzchar(x))
rx <- function() "^([[:blank:]]+)?(.+)"
ind <- function(x) sub(rx(), "\\1", x)
rm_ind <- function(x) sub(rx(), "\\2", x)
spaces <- function(x){
  s <- gregexpr("\\s", x)[[1L]]
  if (s[1L] == -1L)
    return(numeric(0))
  s
}
