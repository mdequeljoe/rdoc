#stripped down version of utils::help
#returns the help path only
help_path <-
  function (topic,
            package = NULL,
            lib.loc = NULL,
            try.all.packages = getOption("help.try.all.packages")){

      ischar <- tryCatch(
        is.character(topic) && length(topic) ==
          1L,
        error = function(e)
          FALSE
      )
      if (!ischar) {
        reserved <- c(
          "TRUE",
          "FALSE",
          "NULL",
          "Inf",
          "NaN",
          "NA",
          "NA_integer_",
          "NA_real_",
          "NA_complex_",
          "NA_character_"
        )
        stopic <- deparse(substitute(topic))
        if (!is.name(substitute(topic)) && !stopic %in% reserved)
          stop("'topic' should be a name, length-one character vector or reserved word")
        topic <- stopic
      }
      paths <- index_search(topic,
                                    find.package(if (is.null(package))
                                      loadedNamespaces()
                                      else
                                        package, lib.loc, verbose = FALSE))
      try.all.packages <-
        !length(paths) && is.logical(try.all.packages) &&
        !is.na(try.all.packages) &&
        try.all.packages && is.null(package) &&
        is.null(lib.loc)
      if (try.all.packages) {
        for (lib in .libPaths()) {
          packages <- .packages(TRUE, lib)
          packages <- packages[is.na(match(packages, .packages()))]
          paths <- c(paths, index_search(topic, file.path(lib,
                                                          packages)))
        }
        paths <- paths[nzchar(paths)]
      }
      paths
  }

index_search <- getFromNamespace("index.search", "utils")
