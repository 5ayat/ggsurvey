#' Takes x-values, y-values, bubble-sizes and color category to make bubble plot from svystat objects.
#'
#'
#'
#' @param xtab X-values calculated with svyby().
#' @param ytab Y-values calculated with svyby().
#' @param stab Size values calculated with svymean().
#' @param cols A vector of values to color-by of length equal to number of categories in svyby.
#' @param alpha Transparency for bubble fill. Default is 0.3.
#' @param colors A color palette.
#' @param base_size Default font size.
#'
#' @return A ggplot object.
#'
#' @export
#'

gg.svy.bubble <- function(xtab, ytab, stab, cols, colors, base_size = 11, alpha = 0.3){
  stab <- data.frame(stab)
  data.tab <- data.frame(cbind(xtab[,1:2],ytab[,2],stab[,1]))
  colnames(data.tab) <- c("category","xvalue","yvalue","svalue")
  data.tab$cols <- cols
  data.tab$cols <- factor(data.tab$cols)

  nstats <- length(levels(data.tab$cols))
  pal<-colors[[3]][1:nstats]

  p <-  ggplot(data = data.tab, aes(x = xvalue, y = yvalue, label = category)) +
    geom_point(aes(size = svalue, color = cols, alpha = alpha)) +
    scale_color_manual(values=pal) +
    scale_size(range = c(5,25)) +
    geom_text_repel(aes(xvalue, yvalue, label = category)) +
    theme_gray(base_size = base_size) +
    labs(title="", y="",x="") +
    theme(panel.background = element_rect(fill = "#ffffff"),
          panel.grid.major = element_line(colour = "#CBCBCB")) +
    theme(legend.position = "none") +
    theme(plot.title = element_text(hjust = 0.5))

  return(p)
}
