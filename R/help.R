
# find the closest help that is not rdoc's help
find_help <- function(f = c("help", "?"), exclude = "rdoc") {
  f <- match.arg(f)
  find_function(f, exclude)
}

find_function <- function(f, exclude = NULL){
  pkg <-
    vapply(methods::findFunction(f),
           attr,
           character(1),
           "name")

  if (!is.null(exclude)){
    exclude <- grepl(exclude, pkg)
    if (all(exclude))
      return()
    pkg <- pkg[!exclude]
  }

  get(f, pkg[1])
}

set_help <- function(k, topic){
  k <- tryCatch(eval(k), error = function(e) stop(e))
  if (!length(k))
    error_help(k)
  help_path <- set_help_path(k)
  topic <- as.character(topic)
  topic <- topic[length(topic)]
  list(topic = topic, help_path = help_path)
}

set_help_path <- function(x) {

  if (inherits(x, 'dev_topic'))
    return(x[][['path']])

  x[1:length(x)]
}

error_help <- function(k){
  stop(paste(utils::capture.output(k), collapse = " "))
}
