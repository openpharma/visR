### function that will find the "lhs" symbol in the pipeline, given that this function is called from with a pipeline
 ## https://github.com/tidyverse/magrittr/issues/115#issuecomment-173894787
the_lhs <- function() {
  parents <- lapply(sys.frames(), parent.env)

  is_magrittr_env <-
    vapply(parents, identical, logical(1), y = environment(`%>%`))

  if (any(is_magrittr_env)) {
    deparse(get("lhs", sys.frames()[[max(which(is_magrittr_env))]]))
  }
}