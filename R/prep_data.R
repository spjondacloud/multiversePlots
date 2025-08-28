#' Prepare data for a given use-case
#'
#' @param use_case "hurricane" or "beauty".
#' @return A list: (df, outcome_var, outcome_var_label, strip_vars, variable_labels)
#' @examples
#' \dontrun{
#' prep <- prep_data("hurricane")
#' }
#' @export
prep_data <- function(use_case) {
  if (use_case == "hurricane") {
    # --- Load and Prepare Data ---
    df <- utils::read.csv("data/hurricane_mva_results.csv", sep = ";")
    df <- df %>% dplyr::mutate(significant = .data$p < 0.05)

    # Define outcome and predictors
    outcome_var <- "edif"  # additional deaths when a hurricane has a feminine name
    outcome_var_label <- "Excess Fatalities"

    # Decision variables
    strip_vars <- c("k1", "k2", "k3", "k4", "k5", "k6", "k7")

    variable_labels <- c(
      k1 = "Outliers",
      k2 = "Leverage points",
      k3 = "Femininity",
      k4 = "Model",
      k5 = "Functional for damages",
      k6 = "Femininity: Main vs. interaction",
      k7 = "Controlling for year"
    )

    df <- df %>% dplyr::mutate(
      k1 = factor(k1, levels = c(3, 1, 2),
                  labels = c("Drop 2 highest deaths", "Drop none", "Drop 1 highest deaths")),
      k2 = factor(k2, levels = c(1, 2, 3, 4),
                  labels = c("Drop none", "Drop 1 highest damage", "Drop 2 highest damage", "Drop 3 highest damage")),
      k3 = factor(k3, levels = c(2, 1),
                  labels = c("Femininity (1-11)", "Female (1/0)")),
      k4 = factor(k4, levels = c(2, 1),
                  labels = c("Negative Binomial", "Log(fatalities+1)")),
      k5 = factor(k5, levels = c(1, 2),
                  labels = c("Linear: $", "Log: ln($)")),
      k6 = factor(k6, levels = c(2, 1, 3, 4, 5, 6),
                  labels = c("X Damages & Min Pressure", "X Damages", "X Damages & Wind",
                             "X Damages & Hurricane Cat.", "X Damages & Pressure, Wind, Cat.", "Main effect")),
      k7 = factor(k7, levels = c(1, 2, 3),
                  labels = c("None", "Year*Damages", "Post 1979 (1/0)*Damages"))
    )
    return(list(df, outcome_var, outcome_var_label, strip_vars, variable_labels))

  } else if (use_case == "beauty") {
    # --- Load and Prepare Data ---
    df <- utils::read.csv("data/beauty_mva_results.csv", sep = ";")
    # p-value column is p_b in your dataset
    df <- df %>% dplyr::mutate(significant = .data$p_b < 0.05)

    # Define outcome
    outcome_var <- "b"
    outcome_var_label <- "Beauty premium"

    # Decision variables
    strip_vars <- c("k1", "k2", "k3", "k4", "k5", "k6")

    variable_labels <- c(
      k1 = "Age restriction",
      k2 = "Interviewer exclusions",
      k3 = "Controls",
      k4 = "Treatment",
      k5 = "Outcome",
      k6 = "Model"
    )

    df <- df %>% dplyr::mutate(
      k1 = factor(k1,
                  levels = c(1, 2),
                  labels = c("Age > 18", "All respondents")),
      k2 = factor(k2,
                  levels = c(1, 2, 3),
                  labels = c("Excluded if no variation",
                             "Excluded if little variation",
                             "All interviewers")),
      k3 = factor(k3,
                  levels = c(1, 2, 3, 4),
                  labels = c("All", "Personality", "Occupation", "Education")),
      k4 = factor(k4,
                  levels = c(1, 2),
                  labels = c("Binary", "Continuous")),
      k5 = factor(k5,
                  levels = c(1, 2, 3, 4, 5, 6),
                  labels = c("ln gross, trimmed",
                             "ln gross, winzorized",
                             "ln gross",
                             "ln net, trimmed",
                             "ln net, winzorized",
                             "ln net")),
      k6 = factor(k6,
                  levels = c(1, 2, 3),
                  labels = c("Interviewer Fixed Effects",
                             "Respondents Random Effects",
                             "Interviewer Random Effects"))
    )
    return(list(df, outcome_var, outcome_var_label, strip_vars, variable_labels))

  } else {
    stop("Unknown `use_case`. Use 'hurricane' or 'beauty'.")
  }
}
