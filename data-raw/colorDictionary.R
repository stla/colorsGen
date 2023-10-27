colorDictionary <- list()

defineColor <- function(name, hueRange, lowerBounds) {
  sMin <- lowerBounds[1L, 1L]
  sMax <- lowerBounds[nrow(lowerBounds), 1L]
  bMin <- lowerBounds[nrow(lowerBounds), 2L]
  bMax <- lowerBounds[1L, 2L]

  colorDictionary[[name]] <<- list(
    "hueRange"        = hueRange,
    "lowerBounds"     = lowerBounds,
    "saturationRange" = c(sMin, sMax),
    "brightnessRange" = c(bMin, bMax)
  )
}

defineColor(
  "monochrome",
  NULL,
  rbind(
    c(0, 0),
    c(100, 0)
  )
)

defineColor(
  "red",
  c(-26, 18),
  rbind(
    c(20, 100),
    c(30, 92),
    c(40, 89),
    c(50, 85),
    c(60, 78),
    c(70, 70),
    c(80, 60),
    c(90, 55),
    c(100, 50)
  )
)

defineColor(
  "orange",
  c(18, 46),
  rbind(
    c(20, 100),
    c(30, 93),
    c(40, 88),
    c(50, 86),
    c(60, 85),
    c(70, 70),
    c(100, 70)
  )
)

defineColor(
  "yellow",
  c(46, 62),
  rbind(
    c(25, 100),
    c(40, 94),
    c(50, 89),
    c(60, 86),
    c(70, 84),
    c(80, 82),
    c(90, 80),
    c(100, 75)
  )
)

defineColor(
  "green",
  c(62, 178),
  rbind(
    c(30, 100),
    c(40, 90),
    c(50, 85),
    c(60, 81),
    c(70, 74),
    c(80, 64),
    c(90, 50),
    c(100, 40)
  )
)

defineColor(
  "blue",
  c(178, 257),
  rbind(
    c(20, 100),
    c(30, 86),
    c(40, 80),
    c(50, 74),
    c(60, 60),
    c(70, 52),
    c(80, 44),
    c(90, 39),
    c(100, 35)
  )
)

defineColor(
  "purple",
  c(257, 282),
  rbind(
    c(20, 100),
    c(30, 87),
    c(40, 79),
    c(50, 70),
    c(60, 65),
    c(70, 59),
    c(80, 52),
    c(90, 45),
    c(100, 42)
  )
)

defineColor(
  "pink",
  c(282, 334),
  rbind(
    c(20, 100),
    c(30, 90),
    c(40, 86),
    c(60, 84),
    c(80, 80),
    c(90, 75),
    c(100, 73)
  )
)

# usethis::use_data(colorDictionary, internal = TRUE, overwrite = TRUE)
