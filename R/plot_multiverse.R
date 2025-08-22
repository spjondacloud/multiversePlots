#' Full multiverse plot (density + heatmap strips)
#'
#' @param use_case Character. Placeholder kept for interface symmetry (e.g., "hurricane").
#' @param df Data.frame. If supplied, uses this directly (recommended in grading).
#' @param outcome_var Character name of outcome column in `df`.
#' @param outcome_var_label X-axis label for the density.
#' @param strip_vars Character vector of decision variables (factors).
#' @param variable_labels Named character vector of labels for strips.
#' @param save_path Optional file path to save the combined plot.
#'
#' @return A patchwork combined plot.
#' @importFrom patchwork plot_layout
#' @examples
#' # combined <- plot_multiverse(
#' #   df = df, outcome_var = "edif", outcome_var_label = "Extra deaths",
#' #   strip_vars = c("k1","k2","k3","k4","k5","k6","k7"),
#' #   variable_labels = c(k1="Outliers", k2="Leverage points", k3="Femininity",
#' #                      k4="Model", k5="Functional for damages",
#' #                      k6="Femininity: Main vs. interaction", k7="Controlling for year")
#' # )
#' @export
plot_multiverse <- function(use_case = "hurricane",
                            df,
                            outcome_var,
                            outcome_var_label,
                            strip_vars,
                            variable_labels,
                            save_path = NULL) {
  density_plot <- generate_density_plot(df, outcome_var, outcome_var_label)
  heatmap_plot <- generate_heatmap_strips(df, outcome_var, strip_vars, variable_labels)

  #
  strip_levels <- sapply(strip_vars, function(var) nlevels(df[[var]]))
  total_strip_levels <- sum(strip_levels)
  heights_vector <- c(0.5 * total_strip_levels, strip_levels + 1)

  combined <- density_plot / heatmap_plot + patchwork::plot_layout(heights = heights_vector)

  if (!is.null(save_path)) {
    ggplot2::ggsave(save_path, combined, width = 11.50, height = 6.12, dpi = 300)
  }
  combined
}
