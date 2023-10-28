#' @importFrom colorspace hex2RGB HSV
#' @importFrom grDevices rgb
NULL

colorsGenEnvir <- new.env()

randomWithin <- function(range) {
  range <- sort(range)
  floor(runif(1L, range[1L], range[2L]))
}

getColorInfo <- function(hue) {
  # Maps red colors to make picking hue easier
  if(hue >= 334 && hue <= 360) {
    hue <- hue - 360
  }
  #
  colorDictionary <- get0("colorDictionary", envir = asNamespace("colorsGen"))
  for(color in colorDictionary) {
    hueRange <- color[["hueRange"]]
    if(
      !is.null(hueRange) &&
      hue >= hueRange[1L] &&
      hue <= hueRange[2L]
    ) {
      return(color)
    }
  }
  return("Color not found")
}

hex2HSV <- function(x) {
  x_rgb <- hex2RGB(x)
  x_hsv <- as(x_rgb, "HSV")
  x_hsv@coords[1L, ] * c(1, 100, 100)
}

getSaturationRange <- function(hue) {
  getColorInfo(hue)[["saturationRange"]]
}

isHex <- function(x) {
  grepl("^#([0-9A-F]{3}|[0-9A-F]{6})$", x, ignore.case = TRUE)
}

getHueRange <- function(colorInput) {
  if(mode(colorInput) == "numeric") {
    if(colorInput < 360 && colorInput > 0) {
      return(c(colorInput, colorInput))
    }
  }
  if(typeof(colorInput) == "character") {
    colorDictionary <- get0("colorDictionary", envir = asNamespace("colorsGen"))
    if(colorInput %in% names(colorDictionary)) {
      color <- colorDictionary[[colorInput]]
      hueRange <- color[["hueRange"]]
      if(!is.null(hueRange)) {
        return(hueRange)
      }
    } else if(isHex(colorInput)) {
      hue <- hex2HSV(colorInput)[1L]
      return(c(hue, hue))
    }
  }
  return(c(0, 360))
}

getRealHueRange <- function(colorHue) {
  if(mode(colorHue) == "numeric") {
    if(colorHue < 360 && colorHue > 0) {
      return(getColorInfo(colorHue)[["hueRange"]])
    }
  } else if(typeof(colorHue) == "character") {
    colorDictionary <- get0("colorDictionary", envir = asNamespace("colorsGen"))
    if(colorHue %in% names(colorDictionary)) {
      color <- colorDictionary[[colorHue]]
      if(!is.null(hueRange <- color[["hueRange"]])) {
        return(hueRange)
      }
    } else if(isHex(colorHue)) {
      hue <- hex2HSV(colorHue)[1L]
      return(getColorInfo(hue)[["hueRange"]])
    }
  }
  return(c(0, 360))
}

getMinimumBrightness <- function(H, S) {
  lowerBounds <- getColorInfo(H)[["lowerBounds"]];
  for(i in seq_len(nrow(lowerBounds)-1L)) {
    s1 <- lowerBounds[i, 1L]
    v1 <- lowerBounds[i, 2L]
    s2 <- lowerBounds[i+1L, 1L]
    v2 <- lowerBounds[i+1L, 2L]
    if(S >= s1 && S <= s2) {
      m <- (v2 - v1) / (s2 - s1)
      b <- v1 - m * s1
      return(m * S + b)
    }
  }
  return(0)
}

pickBrightness <- function(H, S, opts) {
  luminosity <- opts[["luminosity"]]
  bMax <- 100
  if(luminosity == "random") {
    bMin <- 0
  } else {
    bMin <- getMinimumBrightness(H, S)
    if(luminosity == "dark") {
      bMax <- bMin + 20
    } else if(luminosity == "light") {
      bMin <- (bMax + bMin) / 2
    }
  }
  randomWithin(c(bMin, bMax))
}

pickSaturation <- function(hue, opts) {
  if(opts[["hue"]] == "monochrome") {
    return(0)
  }
  luminosity <- opts[["luminosity"]]
  if(luminosity == "random") {
    return(randomWithin(c(0, 100)))
  }
  saturationRange <- getSaturationRange(hue)
  sMin <- saturationRange[1L]
  sMax <- saturationRange[2L]
  if(luminosity == "dark") {
    sMin <- sMax - 10
  } else if(luminosity == "light") {
    sMax <- 55
  } else if(luminosity == "bright") {
    sMin <- 55
  }
  randomWithin(c(sMin, sMax))
}

pickHue <- function(opts) {
  colorRanges <- get0("colorRanges", envir = colorsGenEnvir)
  nranges <- length(colorRanges)
  if(nranges > 0L) {
    hueRange <- getRealHueRange(opts[["hue"]])
    hue <- randomWithin(hueRange)
    # Each of nranges ranges has a length equal approximately one step
    step <- (hueRange[2L] - hueRange[1L]) / nranges
    j <- as.integer((hue - hueRange[1L]) / step) + 1L
    # Check if the range j is taken
    if(colorRanges[j]) {
      j <- ((j + 2L) %% nranges) + 1L
    } else {
      colorRanges[j] <- TRUE
      assign("colorRanges", colorRanges, envir = colorsGenEnvir)
    }
    #
    hmin <- (hueRange[1L] + (j-1L) * step) %% 360
    hmax <- (hueRange[1L] + j * step) %% 360
    hueRange <- c(hmin, hmax)
  } else {
    hueRange <- getHueRange(opts[["hue"]])
    # Instead of storing red as two separate ranges,
    # we group them, using negative numbers
  }
  hue <- randomWithin(hueRange)
  if(hue < 0) {
    hue <- 360 + hue
  }
  return(hue)
}

HSV2hex <- function(h, s, v) {
  col_hsv <- HSV(h, s, v)
  col_rgb <- as(col_hsv, "RGB")@coords[1L, ]
  rgb(col_rgb[1L], col_rgb[2L], col_rgb[3L])
}

.randomColor <- function(count, opts) {
  # Check if we need to generate multiple colors
  if(count > 1L) {
    colors <- character(0L)
    # Value FALSE at index i means the range i is not taken yet
    assign("colorRanges", logical(count), envir = colorsGenEnvir)
    for(i in seq_len(count)) {
      colors[i] <- .randomColor(1L, opts)
    }
    rm("colorRanges", envir = colorsGenEnvir)
    return(colors)
  }
  # First we pick a hue (H)
  H <- pickHue(opts)
  # Then use H to determine saturation (S)
  S <- pickSaturation(H, opts)
  # Then use S and H to determine brightness (B).
  B <- pickBrightness(H, S, opts)
  # Then we return the HSB color in the desired format
  return(HSV2hex(H, S/100, B/100))
}

#' @title Random colors
#' @description Generate random colors.
#'
#' @param n number of colors to be generated
#' @param hue the desired hue; it can be a number between 0 and 360, a
#'   hexadecimal color code, or a string taken among the possibilities
#'   "random", "red", "orange", "yellow", "green", "blue", "purple", "pink",
#'   or "monochrome"
#' @param luminosity the desired luminosity, a string taken among the
#'   possible choices "random", "light", "bright", or "dark"
#'
#' @return A character vector of hexadecimal color codes.
#' @export
#'
#' @examples
#' n <- 20
#' clrs <- randomColor(n, hue = "red", luminosity = "bright")
#' opar <- par(mar=c(0, 0, 0, 0))
#' pie(rep(1, n), col = clrs)
#' par(opar)
randomColor <- function(n, hue = "random", luminosity = "random") {
  stopifnot(n >= 1)
  if(!is.numeric(hue) && !isHex(hue)) {
    hue <- match.arg(
      hue, choices = c(
        "random",
        "red",
        "orange",
        "yellow",
        "green",
        "blue",
        "purple",
        "pink",
        "monochrome"
      )
    )
  }
  luminosity <- match.arg(
    luminosity, c("random", "light", "bright", "dark")
  )
  .randomColor(as.integer(n), list("hue" = hue, "luminosity" = luminosity))
}
