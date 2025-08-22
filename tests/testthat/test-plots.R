test_that("density plot returns ggplot", {
  skip_on_cran()
  df <- data.frame(edif = rnorm(200), significant = sample(c(TRUE, FALSE), 200, TRUE))
  p <- generate_density_plot(df, "edif", "Extra deaths")
  expect_s3_class(p, "ggplot")
})

test_that("heatmap strips return patchwork object", {
  skip_on_cran()
  set.seed(1)
  df <- data.frame(
    edif = rnorm(200),
    k1 = factor(sample(c("A","B"), 200, TRUE)),
    k2 = factor(sample(c("X","Y","Z"), 200, TRUE))
  )
  labs <- c(k1="K1", k2="K2")
  p <- generate_heatmap_strips(df, "edif", c("k1","k2"), labs)
  # patchwork 对象是 ggplot，但 class 比较复杂；检查可打印
  expect_true(inherits(p, "patchwork") || inherits(p, "ggplot"))
})

test_that("plot_multiverse combines two panels", {
  skip_on_cran()
  df <- data.frame(
    edif = rnorm(200),
    significant = sample(c(TRUE, FALSE), 200, TRUE),
    k1 = factor(sample(c("A","B"), 200, TRUE)),
    k2 = factor(sample(c("X","Y","Z"), 200, TRUE))
  )
  labs <- c(k1="K1", k2="K2")
  p <- plot_multiverse(df = df, outcome_var = "edif", outcome_var_label = "Extra",
                       strip_vars = c("k1","k2"), variable_labels = labs)
  expect_true(inherits(p, "patchwork") || inherits(p, "ggplot"))
})
