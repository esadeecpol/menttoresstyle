#This is the EsadeEcPol theme & palette script!

##Theme

theme_menttores <- function(){
  theme(
    #text elements
    panel.grid.major = element_line(color = "#f0f0f0", size=.2),
    panel.grid.minor = element_line(color = "#f0f0f0", size=.1),
    plot.background = element_rect(color="#ffffff", fill="#ffffff"),
    panel.background = element_rect(color="#ffffff", fill="#ffffff"),
    plot.title = element_text(hjust = 0, size = 14,
                              family="Inter Semi-Bold",
                              color = "#000000"),
    plot.subtitle = element_text(hjust = 0, size = 14,
                                  family="Inter-Regular",
                                  color = "#000000"),
    plot.caption = element_text(hjust = 0, size = 11,
                                vjust = .5,
                                family="Inter-Regular",
                                color = "#777777"),
    strip.text = element_text(size=13.5),
    legend.position="top",
    legend.key = element_rect(fill = "transparent", colour = "transparent"),
    legend.title = element_blank(),
    legend.text = element_text(size=13),
    axis.title.x = element_text(hjust = 0.5, size = 13,
                                family="Inter-Regular",
                                color = "#444444"),
    axis.title.y = element_text(angle = 0, size= 13,
                                family="Inter-Regular",
                                color = "#444444"),
    axis.text.x = element_text(size = 13,
                               family="Inter-Regular"),
    axis.text.y = element_text(size = 13,
                               color = "grey27",
                               family="Inter-Regular"),
  )
}


##Palette

palettes_menttores <- list(
  onecolors = c("#6EAED5"),
  twocolors = c("#6EAED5","#EEC360"),
  threecolors = c("#6EAED5","#EEC360","#95CD94"),
  redgreen = c("#CF5F5B","#95CD94"),
  greenred = c("#95CD94","#CF5F5B"),
  semaforo_inverted = c("#95CD94","#6EAED5","#CF5F5B"),
  semaforo = c("#CF5F5B","#6EAED5","#95CD94"),
  fourcolors = c("#6EAED5","#EEC360","#95CD94","#CF5F5B"),
  fivecolors = c("#6EAED5","#EEC360","#95CD94","#CF5F5B","#6469BA"),
  sixcolors = c("#6EAED5","#EEC360","#95CD94","#CF5F5B","#6469BA","#1C2440")
)
palette_menttores <- function(name, n, type = c("discrete", "continuous")) {
  type <- match.arg(type)

  pal <- palettes_menttores[[name]]
  if (is.null(pal))
    stop("De que me hablas nano")

  if (missing(n)) {
    n <- length(pal)
  }

  if (type == "discrete" && n > length(pal)) {
    stop("No hay tanto color")
  }

  out <- switch(type,
                continuous = grDevices::colorRampPalette(pal)(n),
                discrete = pal[1:n]
  )
  structure(out, class = "palette", name = name)
}

print.palette <- function(x, ...) {
  n <- length(x)
  old <- par(mar = c(0.5, 0.5, 0.5, 0.5))
  on.exit(par(old))

  image(1:n, 1, as.matrix(1:n), col = x,
        ylab = "", xaxt = "n", yaxt = "n", bty = "n")

  rect(0, 0.9, n + 1, 1.1, col = rgb(1, 1, 1, 0.8), border = NA)
  text((n + 1) / 2, 1, labels = attr(x, "name"), cex = 1, family = "serif")
}

palette_one <- palette_menttores("onecolors")
palette_two <- palette_menttores("twocolors")
palette_semaforoinverted <- palette_menttores("semaforo_inverted")
palette_semaforo <- palette_menttores("semaforo")
palette_redgreen <- palette_menttores("redgreen")
palette_greenred <- palette_menttores("greenred")
palette_three <- palette_menttores("threecolors")
palette_four <- palette_menttores("fourcolors")
palette_five <- palette_menttores("fivecolors")
palette_six <- palette_menttores("sixcolors")


