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

test_that("add_plot_date()", {
  df <- data.frame(
    date = c(
      seq(as.Date("2024-02-20"), as.Date("2024-03-10")),
      seq(as.Date("2025-02-20"), as.Date("2025-03-10"))
    )
  )

  act <- add_plot_date(df)

  df$year <- format(df$date, "%Y")

  df$plot_date <- as.Date(paste0("2024", "-", format(df$date, "%m-%d")))

  exp <- df

  expect_equal(act, exp)
})

test_that("no args", {
  df <- data.frame(
    date = seq(as.Date("2025-05-26"), as.Date("2025-06-14")),
    count = c(0, 0, 1, 3, 4, 2, 6, 9, 4, 5, 3, 2, 1, 1, 0, 0, 0, 1, 0, 0),
    color_id = c(0, 0, 0, 1, 2, 1, 3, 3, 2, 3, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0)
  )

  p <- ess_plot_ts(df)

  vdiffr::expect_doppelganger("no args", p)
})

test_that("with title", {
  df <- data.frame(
    date = seq(as.Date("2025-05-26"), as.Date("2025-06-14")),
    count = c(0, 0, 1, 3, 4, 2, 6, 9, 4, 5, 3, 2, 1, 1, 0, 0, 0, 1, 0, 0),
    color_id = c(0, 0, 0, 1, 2, 1, 3, 3, 2, 3, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0)
  )

  p <- ess_plot_ts(df, title = "Unspecified syndrome, May-Jun 2025")

  vdiffr::expect_doppelganger("with title", p)
})

test_that("no alerts", {
  df <- data.frame(
    date = seq(as.Date("2025-05-26"), as.Date("2025-06-14")),
    count = c(0, 0, 1, 3, 4, 2, 6, 9, 4, 5, 3, 2, 1, 1, 0, 0, 0, 1, 0, 0),
    color_id = c(0, 0, 0, 1, 2, 1, 3, 3, 2, 3, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0)
  )

  p <- ess_plot_ts(
    df, title = "Unspecified syndrome, May-Jun 2025",
    show_alerts = FALSE
  )

  vdiffr::expect_doppelganger("no alerts", p)
})

test_that("static", {
  df <- data.frame(
    date = seq(as.Date("2025-05-26"), as.Date("2025-06-14")),
    count = c(0, 0, 1, 3, 4, 2, 6, 9, 4, 5, 3, 2, 1, 1, 0, 0, 0, 1, 0, 0),
    color_id = c(0, 0, 0, 1, 2, 1, 3, 3, 2, 3, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0)
  )

  p <- ess_plot_ts(
    df, title = "Unspecified syndrome, May-Jun 2025",
    interactive = FALSE
  )

  vdiffr::expect_doppelganger("static", p)
})

test_that("static, no alerts", {
  df <- data.frame(
    date = seq(as.Date("2025-05-26"), as.Date("2025-06-14")),
    count = c(0, 0, 1, 3, 4, 2, 6, 9, 4, 5, 3, 2, 1, 1, 0, 0, 0, 1, 0, 0),
    color_id = c(0, 0, 0, 1, 2, 1, 3, 3, 2, 3, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0)
  )

  p <- ess_plot_ts(
    df, title = "Unspecified syndrome, May-Jun 2025",
    interactive = FALSE, show_alerts = FALSE
  )

  vdiffr::expect_doppelganger("static, no alerts", p)
})

test_that("wrapped, no title", {
  df <- data.frame(
    date = c(
      seq(as.Date("2024-05-26"), as.Date("2024-06-14")),
      seq(as.Date("2025-05-26"), as.Date("2025-06-14"))
    ),
    count = c(
      0, 0, 1, 3, 4, 2, 6, 9, 4, 5, 3, 2, 1, 1, 0, 0, 0, 1, 0, 0,
      1, 0, 1, 0, 0, 0, 2, 0, 0, 1, 3, 5, 4, 5, 3, 1, 1, 0, 1, 0
    ),
    color_id = c(
      0, 0, 0, 1, 2, 1, 3, 3, 2, 3, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 3, 2, 2, 1, 0, 0, 0, 0, 0
    )
  )

  p <- ess_plot_ts(df, wrap_years = TRUE)

  vdiffr::expect_doppelganger("wrapped, no title", p)
})

test_that("wrapped, with title", {
  df <- data.frame(
    date = c(
      seq(as.Date("2024-05-26"), as.Date("2024-06-14")),
      seq(as.Date("2025-05-26"), as.Date("2025-06-14"))
    ),
    count = c(
      0, 0, 1, 3, 4, 2, 6, 9, 4, 5, 3, 2, 1, 1, 0, 0, 0, 1, 0, 0,
      1, 0, 1, 0, 0, 0, 2, 0, 0, 1, 3, 5, 4, 5, 3, 1, 1, 0, 1, 0
    ),
    color_id = c(
      0, 0, 0, 1, 2, 1, 3, 3, 2, 3, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 3, 2, 2, 1, 0, 0, 0, 0, 0
    )
  )

  p <- ess_plot_ts(
    df, title = "Unspecified syndrome, May-Jun 2024 & 2025",
    wrap_years = TRUE
  )

  vdiffr::expect_doppelganger("wrapped, with title", p)
})

test_that("wrapped, static", {
  df <- data.frame(
    date = c(
      seq(as.Date("2024-05-26"), as.Date("2024-06-14")),
      seq(as.Date("2025-05-26"), as.Date("2025-06-14"))
    ),
    count = c(
      0, 0, 1, 3, 4, 2, 6, 9, 4, 5, 3, 2, 1, 1, 0, 0, 0, 1, 0, 0,
      1, 0, 1, 0, 0, 0, 2, 0, 0, 1, 3, 5, 4, 5, 3, 1, 1, 0, 1, 0
    ),
    color_id = c(
      0, 0, 0, 1, 2, 1, 3, 3, 2, 3, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 3, 2, 2, 1, 0, 0, 0, 0, 0
    )
  )

  p <- ess_plot_ts(
    df, title = "Unspecified syndrome, May-Jun 2024 & 2025",
    interactive = FALSE, wrap_years = TRUE
  )

  vdiffr::expect_doppelganger("wrapped, static", p)
})

test_that("wrapped, static, no alerts", {
  df <- data.frame(
    date = c(
      seq(as.Date("2024-05-26"), as.Date("2024-06-14")),
      seq(as.Date("2025-05-26"), as.Date("2025-06-14"))
    ),
    count = c(
      0, 0, 1, 3, 4, 2, 6, 9, 4, 5, 3, 2, 1, 1, 0, 0, 0, 1, 0, 0,
      1, 0, 1, 0, 0, 0, 2, 0, 0, 1, 3, 5, 4, 5, 3, 1, 1, 0, 1, 0
    ),
    color_id = c(
      0, 0, 0, 1, 2, 1, 3, 3, 2, 3, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 3, 2, 2, 1, 0, 0, 0, 0, 0
    )
  )

  p <- ess_plot_ts(
    df, title = "Unspecified syndrome, May-Jun 2024 & 2025",
    interactive = FALSE, show_alerts = FALSE, wrap_years = TRUE
  )

  vdiffr::expect_doppelganger("wrapped, static, no alerts", p)
})

