get_pkg <- function(path)
  basename(dirname(dirname(path)))

is_rd_file <- function(path)
  grepl(".+\\.[R|r]d$", path)

id_headers <- function(rdtxt){
  which(grepl("^([[:punct:]]?)_\\b", rdtxt))
}

as_title <- function(h){
  gsub("_\b|:", "", h)
}

set_styles <- function(l){
  lapply(l, function(x) if (is.null(x)) function(x) x else x)
}

format_args <- function(x, f){
  rx <- "^(\\s*)([[:alnum:]|\\._]+)(:)"
  arg <- grepl(rx, x)
  x[arg] <- gsub(rx, paste0("\\1", f("\\2"), "\\3"), x[arg])
  x
}
