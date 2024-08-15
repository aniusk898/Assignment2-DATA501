plot_infmeasures <- function(model,
                             data,
                             measure = "cooks",
                             threshold = NULL) {
  valInputs(data, model)

  n <- nrow(data)
  p <- length(model$coefficients)

  infl <- switch(
    measure,
    cooks = {
      if (is.null(threshold))
        threshold <- 4 / n
      cooks_distance(model)
    },
    diffits = {
      if (is.null(threshold))
        threshold <- 2 * sqrt(p / n)
      diffits(model)
    },
    hadis_influences = {
      if (is.null(threshold))
        threshold <- 2 * sqrt(p / n)
      hadis_influence(model)
    },
    stop("not valid measure")
  )

  outliers <- if (measure == "diffits")
    abs(infl) > threshold
  else
    infl > threshold

  p <- ggplot() +
    geom_segment(aes(
      x = 1:length(infl),
      xend = 1:length(infl),
      y = 0,
      yend = infl,
      color = outliers
    ),
    size = 1) +
    scale_color_manual(values = c("FALSE" = "blue", "TRUE" = "red")) +
    geom_hline(yintercept = threshold,
               color = "red",
               size = 0.3) +
    geom_text(
      aes(
        x = 1:length(infl),
        y = infl,
        label = ifelse(outliers, 1:length(infl), "")
      ),
      vjust = -0.5,
      color = "black"
    ) +
    annotate(
      "text",
      x = Inf,
      y = Inf,
      label = paste("Threshold =", round(threshold, 3)),
      hjust = 1.1,
      vjust = 2,
      color = "red",
      size = 4
    ) +
    labs(
      title = paste("Measure of Influence:", measure),
      x = "Observation",
      y = measure) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))

  if (measure == "diffits") {
    p <- p + geom_hline(yintercept = -threshold, color = "red", size = 0.3)
  }

  print(p)
}
