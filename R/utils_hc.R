# Utils HC 

pal20 <- c(
  "#4E79A7FF", "#A0CBE8FF", "#F28E2BFF", "#FFBE7DFF", "#59A14FFF",
  "#8CD17DFF", "#B6992DFF", "#F1CE63FF", "#499894FF", "#86BCB6FF",
  "#E15759FF", "#FF9D9AFF", "#79706EFF", "#BAB0ACFF", "#D37295FF",
  "#FABFD2FF", "#B07AA1FF", "#D4A6C8FF", "#9D7660FF", "#D7B5A6FF"
)

pal10 <- leaflet.minicharts::d3.schemeCategory10
pal10[7] <- "#e377c2"

# pal10 <- c(
#   "#4E79A7FF", "#F28E2BFF", "#E15759FF", "#76B7B2FF", "#59A14FFF",
#   "#EDC948FF", "#B07AA1FF", "#FF9DA7FF", "#9C755FFF", "#BAB0ACFF"
# )

dark2 <- c(
  "steelblue", "#1B9E77", "#D95F02", "#7570B3", "#E7298A",
  "#66A61E", "#E6AB02", "#A6761D", "#666666"
)

fntfmly <- 'system-ui,-apple-system,BlinkMacSystemFont,"Segoe UI",Roboto,"Helvetica Neue",Arial,sans-serif,"Apple Color Emoji","Segoe UI Emoji","Segoe UI Symbol";'

hc_opts <- getOption("highcharter.chart")
hc_opts$colors <- pal10
hc_opts$plotOptions$column <- list(zIndex = 2, stacking = "normal", groupPadding = 0.05, pointPadding = 0.05, borderWidth = 1, borderColor = "white")
hc_opts$plotOptions$bar <- list(zIndex = 2, stacking = "normal", groupPadding = 0.05, pointPadding = 0.05, borderWidth = 0.05)
hc_opts$credits <- list(enabled = FALSE, href = "", style = list(fontFamily = fntfmly, fontSize = "10px", fontStyle = "italic", cursor = "default"))
hc_opts$exporting <- list(enabled = FALSE)
hc_opts$title <- list(text = NULL)

options(
  highcharter.chart = hc_opts,
  highcharter.theme =
    highcharter::hc_theme_smpl(
      chart = list(style = list(fontFamily = fntfmly)),
      title = list(style = list(fontFamily = fntfmly)),
      subtitle = list(style = list(fontFamily = fntfmly)),
      plotOptions = list(line = list(marker = list(enabled = FALSE)))
    )
)