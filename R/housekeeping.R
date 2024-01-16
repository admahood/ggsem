utils::globalVariables(c("lhs", "rhs", "op", "pvalue", "est.std", "val", "hlab",
                         "metric", "x", 'y', "Relationship", "e", "Response",
                         "Predictor", "Std.Estimate", "P.Value"))

# solving the note on unused imports (we do in fact use these packages for)
ignore_unused_imports <- function(){
  utils::globalVariables
  ggplot2::ggplot
  ggpubr::ggarrange
  piecewiseSEM::psem
  stats::runif
}
