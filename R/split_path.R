split_path <- function(x) if (dirname(x)==x) x else c(basename(x),split_path(dirname(x)))
