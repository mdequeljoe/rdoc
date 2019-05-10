get_pkg <- function(path)
  basename(dirname(dirname(path)))

is_rd_file <- function(path)
  grepl(".+\\.[R|r]d$", path)

id_headers <- function(rdtxt){
  which(grepl("^([[:punct:]]?)_\\b", rdtxt))
}

set_section_title <- function(h){
  gsub("_\b|:", "", h)
}

# takes into account NULL options for overall style
set_styles <- function(l){
  lapply(l, function(x) if (is.null(x)) function(x) x else x)
}

# long titles may get cut by width via rd2txt
set_rd_title <- function(o){
  x <- min(which(!grepl("_\b", o))) - 1L
  if (x == 1L)
    return(o)
  x <- 1L:x
  title <- paste( o[x] , collapse = " ")
  c(title, o[-x])
}

format_args <- function(x, f){
  rx <- "^(\\s*)([[:alnum:]|\\._]+)(:)"
  arg <- grepl(rx, x)
  x[arg] <- gsub(rx, paste0("\\1", f("\\2"), "\\3"), x[arg])
  x
}

append_list <- function(l) Reduce(append, l)

send_out <- function(s, file = "")
  cat(append_list(s), file = file, sep = "\n")
