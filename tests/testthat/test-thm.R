set.seed(123)
df <- ggplot2::mpg |>
  dplyr::group_by(model) |>
  dplyr::summarize(hwy = mean(hwy, na.rm = TRUE)) |>
  dplyr::slice_sample(n = 4) |>
  dplyr::mutate(model2 = model)

p <- ggplot2::ggplot(data = df, mapping = ggplot2::aes(x = model, y = hwy)) +
  ggplot2::facet_wrap(
    ggplot2::vars(model), nrow = 1, scales = "free_x",
    labeller = \(x) df[, "model"] |> lapply(\(x) stringr::str_wrap(x, 10)),
    strip.position = "top"
  ) +
  ggplot2::geom_col(mapping = ggplot2::aes(fill = model)) +
  ggplot2::geom_point(mapping = ggplot2::aes(color = model2)) +
  ggplot2::labs(title = "Title", subtitle = "Subtitle", caption = "Caption")

test_that("thm and thm_gray", {
  
  p0 <- p + thm()
  expect_no_warning(p0)

  p0 <- p + thm(center.titles = TRUE)
  expect_no_warning(p0)

  p0 <- p + thm(14, title.hjust = 0.5, caption.hjust = 0.5, title.face = "bold", subtitle.hjust = 0.5)
  expect_no_warning(p0)

  p0 <- p + thm(base.size = 22)
  expect_no_warning(p0)

  p0 <- p + thm(5)
  expect_no_warning(p0)

  p0 <- p + thm_gray()
  expect_no_warning(p0)

  p0 <- p + thm_gray(center.titles = TRUE)
  expect_no_warning(p0)

  p0 <- p + thm_gray(14, title.hjust = 0.5, caption.hjust = 0.5, title.face = "bold", subtitle.hjust = 0.5)
  expect_no_warning(p0)

  p0 <- p + thm_gray(base.size = 22)
  expect_no_warning(p0)

  p0 <- p + thm_gray(5)
  expect_no_warning(p0)

  p0 <- p + thm_grey()
  expect_no_warning(p0)

  p0 <- p + thm_grey(center.titles = TRUE)
  expect_no_warning(p0)

  p0 <- p + thm_grey(14, title.hjust = 0.5, caption.hjust = 0.5, title.face = "bold", subtitle.hjust = 0.5)
  expect_no_warning(p0)

  expect_no_warning(p0)

  p0 <- p + thm_grey(base.size = 22)
  expect_no_warning(p0)

  p0 <- p + thm_grey(5)

  p0 <- p + thm(
    base.size = 20,
    bg = "#99CCFF",
    title.face = "bold",
    subtitle.face = "italic",
    caption.face = "plain",
    tag.face = "bold",
    title.hjust = 1,
    subtitle.hjust = 0.5,
    caption.hjust = 0,
    tag.hjust = 0,
    rel.title.text        = 18 / 20,
    rel.subtitle.text     = 17 / 20,
    rel.caption.text      = 11 / 20,
    rel.axis.title.text   = 13 / 20,
    rel.axis.text         = 11 / 20,
    rel.legend.title.text = 13 / 20,
    rel.legend.text       = 10 / 20,
    rel.strip.text        = 9 / 20,
    rel.tag.text          = 18 / 20,
    center.titles = NULL
  )
  expect_no_warning(p0)

  p0 <- p + thm_gray(
    base.size = 20,
    bg = "#99CCFF",
    title.face = "bold",
    subtitle.face = "italic",
    caption.face = "plain",
    tag.face = "bold",
    title.hjust = 1,
    subtitle.hjust = 0.5,
    caption.hjust = 0,
    tag.hjust = 0,
    rel.title.text        = 18 / 20,
    rel.subtitle.text     = 17 / 20,
    rel.caption.text      = 11 / 20,
    rel.axis.title.text   = 13 / 20,
    rel.axis.text         = 11 / 20,
    rel.legend.title.text = 13 / 20,
    rel.legend.text       = 10 / 20,
    rel.strip.text        = 9 / 20,
    rel.tag.text          = 18 / 20,
    center.titles = NULL
  )
  expect_no_warning(p0)

  p0 <- p + thm_grey(
    base.size = 20,
    bg = "#99CCFF",
    title.face = "bold",
    subtitle.face = "italic",
    caption.face = "plain",
    tag.face = "bold",
    title.hjust = 1,
    subtitle.hjust = 0.5,
    caption.hjust = 0,
    tag.hjust = 0,
    rel.title.text        = 18 / 20,
    rel.subtitle.text     = 17 / 20,
    rel.caption.text      = 11 / 20,
    rel.axis.title.text   = 13 / 20,
    rel.axis.text         = 11 / 20,
    rel.legend.title.text = 13 / 20,
    rel.legend.text       = 10 / 20,
    rel.strip.text        = 9 / 20,
    rel.tag.text          = 18 / 20,
    center.titles = NULL
  )
  expect_no_warning(p0)

})
