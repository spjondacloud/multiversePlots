#' Density plot for multiverse outcomes
#'
#' Draws a density curve for the outcome and overlays a scaled density
#' for statistically significant estimates.
#'
#' @param data A data.frame containing outcome and a logical column `significant`.
#' @param outcome_var Character. Name of the outcome variable (numeric).
#' @param outcome_var_label Character. Label for the x-axis.
#'
#' @return A ggplot object.
#' @importFrom ggplot2 ggplot aes geom_line scale_color_manual labs coord_cartesian theme_minimal theme element_blank element_text
#' @importFrom dplyr bind_rows mutate filter
#' @importFrom stats density
#' @examples
#' # df <- read.csv("data/hurricane_mva_results.csv", sep=";")
#' # df$significant <- df$p < 0.05
#' # generate_density_plot(df, "edif", "Extra deaths")
#' @export
generate_density_plot <- function(data, outcome_var, outcome_var_label) {
  stopifnot(outcome_var %in% names(data))
  x_min <- min(data[[outcome_var]], na.rm = TRUE)
  x_max <- max(data[[outcome_var]], na.rm = TRUE)

  dens_all <- stats::density(data[[outcome_var]], na.rm = TRUE)
  dens_all <- data.frame(x = dens_all$x, y = dens_all$y, type = "All")

  if (!"significant" %in% names(data)) {
    stop("Column 'significant' not found in `data`.")
  }
  sig_data <- dplyr::filter(data, .data$significant)
  if (nrow(sig_data) == 0) {
    dens_sig <- data.frame(x = dens_all$x, y = 0, type = "Significant")
  } else {
    dens_sig <- stats::density(sig_data[[outcome_var]], na.rm = TRUE)
    sig_share <- mean(data$significant)
    # Clip to overall density length safely
    len <- min(length(dens_sig$y), length(dens_all$y))
    dens_sig$y[seq_len(len)] <- pmin(dens_sig$y[seq_len(len)] * sig_share, dens_all$y[seq_len(len)])
    dens_sig <- data.frame(x = dens_sig$x, y = dens_sig$y, type = "Significant")
  }

  df_dens <- dplyr::bind_rows(dens_all, dens_sig)

  x_buffer <- 0.1 * (x_max - x_min)
  common_xlim <- c(x_min, x_max + x_buffer)

  ggplot2::ggplot(df_dens, ggplot2::aes(x = .data$x, y = .data$y, color = .data$type)) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::scale_color_manual(values = c("All" = "steelblue", "Significant" = "tomato")) +
    ggplot2::labs(x = outcome_var_label, color = "Type") +
    ggplot2::coord_cartesian(xlim = common_xlim) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      legend.key.height = grid::unit(0.3, "cm"),
      legend.text = ggplot2::element_text(size = 9),
      legend.position = c(0.9, 0.95),
      legend.justification = c("right", "top"),
      legend.direction = "vertical",
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      axis.title.x = ggplot2::element_text(size = 10),
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor.y = ggplot2::element_blank()
    )
}
