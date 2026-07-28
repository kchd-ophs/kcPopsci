test_that("ess_config_alerts()", {
  set.seed(1)

  df <- data.frame(
    count = 1:10,
    color_id = sample(0:3, 10, TRUE)
  )

  act <- ess_config_alerts(df)

  exp <- dplyr::tribble(
    ~count, ~color_id, ~alert_status, ~alert_fill, ~alert_color, ~alert_symbol,
    1, 0, "Normal", "#0703fc", "#04029e", 21,
    2, 3, "Anomaly", "#ff0000", "#a30202", 24,
    3, 2, "Warning", "#f2c00a", "#a17f03", 23,
    4, 0, "Normal", "#0703fc", "#04029e", 21,
    5, 1, "Normal", "#0703fc", "#04029e", 21,
    6, 0, "Normal", "#0703fc", "#04029e", 21,
    7, 2, "Warning", "#f2c00a", "#a17f03", 23,
    8, 2, "Warning", "#f2c00a", "#a17f03", 23,
    9, 1, "Normal", "#0703fc", "#04029e", 21,
    10, 1, "Normal", "#0703fc", "#04029e", 21
  )

  exp$alert_status <- factor(
    exp$alert_status,
    levels = c("Normal", "Warning", "Anomaly")
  )

  exp <- as.data.frame(exp)

  expect_equal(act, exp)
})

test_that("ess_plot_ts()", {
  df <- data.frame(
    date = seq.Date(as.Date("2026-01-01"), as.Date("2026-01-03")),
    count = c(4, 7, 2),
    color_id = sample(0:3, 3, TRUE)
  )

  df <- ess_config_alerts(df)

  plot <- ess_plot_ts(df, title = "A title")

  expect_s3_class(plot, c("plotly", "htmlwidget"), exact = TRUE)
})

test_that("ess_plot_timeseries()", {
  df <- data.frame(
    date = seq.Date(as.Date("2026-01-01"), as.Date("2026-01-03")),
    count = c(4, 7, 2),
    color_id = sample(0:3, 3, TRUE)
  )

  act <- ess_plot_timeseries(df, title = "A title")

  act[[1]][[6]] <- NULL

  df <- ess_config_alerts(df)

  exp <- ess_plot_ts(df, title = "A title")

  exp[[1]][[6]] <- NULL

  expect_equal(act, exp, ignore_attr = TRUE, ignore_function_env = TRUE)
})
