test_that("ess_config_alerts()", {
  set.seed(1)

  df <- data.frame(color_id = sample(0:3, 10, TRUE))

  act <- ess_config_alerts(df)

  exp <- dplyr::tribble(
    ~color_id, ~alert_status, ~alert_fill, ~alert_color,
    ~alert_symbol, ~alert_radius, ~alert_line,
    0, "Normal", "#0703fc", "#04029e", "circle", 5, 1,
    3, "Anomaly", "#ff0000", "#a30202", "triangle", 5, 1,
    2, "Warning", "#f2c00a", "#a17f03", "diamond", 5, 1,
    0, "Normal", "#0703fc", "#04029e", "circle", 5, 1,
    1, "Normal", "#0703fc", "#04029e", "circle", 5, 1,
    0, "Normal", "#0703fc", "#04029e", "circle", 5, 1,
    2, "Warning", "#f2c00a", "#a17f03", "diamond", 5, 1,
    2, "Warning", "#f2c00a", "#a17f03", "diamond", 5, 1,
    1, "Normal", "#0703fc", "#04029e", "circle", 5, 1,
    1, "Normal", "#0703fc", "#04029e", "circle", 5, 1
  )

  exp$alert_status <- factor(
    exp$alert_status,
    levels = c("Normal", "Warning", "Anomaly")
  )

  exp <- as.data.frame(exp)

  expect_equal(act, exp)
})

test_that("ess_listify_ts()", {
  set.seed(1)

  df <- data.frame(
    date = seq.Date(as.Date("2026-01-01"), as.Date("2026-01-03")),
    count = c(4, 7, 2),
    color_id = sample(0:3, 3, TRUE)
  )

  df <- ess_config_alerts(df)

  act <- ess_listify_ts(df)

  df$date <- as.POSIXct(paste(df$date, "12:00:00"))

  exp <- list(
    list(
      data = lapply(1:nrow(df), \(r) {
        list(
          x = highcharter::datetime_to_timestamp(df[r, "date"]),
          y = df[r, "count"],
          color = df[r, "alert_fill"],
          marker = list(
            symbol = df[r, "alert_symbol"],
            radius = df[r, "alert_radius"],
            lineWidth = df[r, "alert_line"],
            lineColor = df[r, "alert_color"]
          ),
          alert_status = df[r, "alert_status"]
        )
      })
    )
  )

  expect_equal(act, exp)
})

test_that("ess_plot_ts()", {
  df <- data.frame(
    date = seq.Date(as.Date("2026-01-01"), as.Date("2026-01-03")),
    count = c(4, 7, 2),
    color_id = sample(0:3, 3, TRUE)
  )

  df <- ess_config_alerts(df)

  ls <- ess_listify_ts(df)

  plot <- ess_plot_ts(ls, title = "A title")

  expect_s3_class(plot, c("highchart", "htmlwidget"), exact = TRUE)
})

test_that("ess_plot_timeseries()", {
  df <- data.frame(
    date = seq.Date(as.Date("2026-01-01"), as.Date("2026-01-03")),
    count = c(4, 7, 2),
    color_id = sample(0:3, 3, TRUE)
  )

  plot <- ess_plot_timeseries(df)

  expect_s3_class(plot, c("highchart", "htmlwidget"), exact = TRUE)
})
