#' Density plot with shaded significant area
#'
#' Draws the overall density curve of the outcome and shades the area under the
#' density for significant rows.
#'
#' @param data A data.frame containing `outcome_var` and a logical `significant` (or a p-value to derive it upstream).
#' @param outcome_var String. Name of the outcome variable.
#' @param outcome_var_label String. Label for the x-axis.
#' @return A ggplot object.
#' @examples
#' \dontrun{
#' df <- data.frame(edif = rnorm(200), p = runif(200))
#' df$significant <- df$p < 0.05
#' generate_density_plot(df, "edif", "Excess Fatalities")
#' }
#' @importFrom ggplot2 ggplot aes geom_area geom_line scale_color_manual scale_fill_manual
#' @importFrom ggplot2 labs coord_cartesian theme_minimal theme element_text element_blank
#' @importFrom stats density approx
#' @importFrom dplyr filter
#' @importFrom grid unit
#' @export
generate_density_plot <- function(data, outcome_var, outcome_var_label) {
  if (!outcome_var %in% names(data)) stop("`outcome_var` not found in `data`.")
  if (!"significant" %in% names(data)) stop("`data` must contain `significant` (logical).")

  x_min <- min(data[[outcome_var]], na.rm = TRUE)
  x_max <- max(data[[outcome_var]], na.rm = TRUE)

  dens_all <- stats::density(data[[outcome_var]], na.rm = TRUE)
  dens_all <- data.frame(x = dens_all$x, y = dens_all$y)

  sig_data <- dplyr::filter(data, .data$significant)
  if (nrow(sig_data) > 1) {
    dens_sig <- stats::density(sig_data[[outcome_var]], na.rm = TRUE)
    sig_share <- mean(data$significant, na.rm = TRUE)
    all_xy <- stats::approx(dens_all$x, dens_all$y, xout = dens_sig$x, rule = 2)
    sig_y <- pmin(dens_sig$y * sig_share, all_xy$y)
    dens_sig <- data.frame(x = dens_sig$x, y = sig_y)
  } else {
    dens_sig <- data.frame(x = numeric(0), y = numeric(0))
  }

  x_buffer <- 0.1 * (x_max - x_min)
  common_xlim <- c(x_min, x_max + x_buffer)

  ggplot2::ggplot() +
    ggplot2::geom_area(
      data = dens_sig,
      ggplot2::aes(x = .data$x, y = .data$y, fill = "Significant"),
      alpha = 0.5
    ) +
    ggplot2::geom_line(
      data = dens_all,
      ggplot2::aes(x = .data$x, y = .data$y, color = "All"),
      linewidth = 1
    ) +
    ggplot2::scale_color_manual(values = c(All = "steelblue")) +
    ggplot2::scale_fill_manual(values = c(Significant = "tomato")) +
    ggplot2::labs(x = outcome_var_label, color = NULL, fill = NULL) +
    ggplot2::coord_cartesian(xlim = common_xlim) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      # ggplot2 >= 3.5.0: numeric legend.position deprecated -> use legend.position.inside
      legend.position = "none",
      legend.position.inside = c(0.9, 0.95),
      legend.key.height = grid::unit(0.3, "cm"),
      legend.text = ggplot2::element_text(size = 9),
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
