#' Creates a color theme for ggsurvey.
#'
#'This function will create a list of vectors of Hex values associated with a variety of helpful mappings, including
#'a diverging palette, a primary palette, a qualitative palette, and a gradient palette. This is not as
#'flexible as other color options in R, in that colors are not automatically chosen. The requriement for a manual
#'input of Hex values reflects the fact that individuals or organizations may already have defined palettes.
#'The default values given in this function are from the Feeling Responsive Jekyll theme developed by Phlow.
#'
#' @param diverging.lows Two values for the low side of a diverging palette gradient. Stronger first.
#' @param diverging.neutral A neutral color for the center of a diverging palette.
#' @param diverging.highs Two values for the high side of a diverging palette gradient. Stronger last.
#' @param primary A single Hex value for plots with only one primary color.
#' @param qualitative A vector of Hex codes to be used sequentially in qualitative plots. Length must be greater than or equal to number of categories in survey table.
#' @param grandient Two values for a gradient palette. Stronger last.
#'
#' @return A list of vectors with Hex codes. List order is [1] diverging, [2] primary, [3] qualitative, and [4] gradient.
#'
#' @export

gg.svy.colors.manual <- function(diverging.lows = c("#DF4949", "#E27A3F"),
                          diverging.neutral = "#BEBEBE",
                          diverging.highs = c("#45B29D", "#334D5C"),
                          primary="#45B29D",
                          qualitative = c("#45B29D", "#334D5C","#A1D044", "#EFC94C", "#E27A3F"),
                          gradient = c("#334D5C", "#45B29D")){
  diverging <- c(diverging.lows,diverging.neutral,diverging.highs)
  primary <- primary
  qualitative <-qualitative
  type <- "manual"
  pal<-list(diverging, primary, qualitative, gradient,type)
  return(pal)
}

