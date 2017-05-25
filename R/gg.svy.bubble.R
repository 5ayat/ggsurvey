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
#' @param discrete If TRUE replace numbers on x and y-axes with factor levels. Must also provide x.var and y.var.
#' @param x.var A factor for x-axis, e.g. data$x.var.
#' @param y.var A factor for x-axis, e.g. data$x.var.
#'
#' @return A ggplot object.
#'
#' @export
#'

gg.svy.bubble <- function(xtab, ytab, stab, cols, colors, base_size = 11, alpha = 0.3, discrete = FALSE, x.var = NULL, y.var = NULL){
  stab <- data.frame(stab)
  data.tab <- data.frame(cbind(xtab[,1:2],ytab[,2],stab[,1]))
  colnames(data.tab) <- c("category","xvalue","yvalue","svalue")
  data.tab$cols <- cols
  data.tab$cols <- factor(data.tab$cols)

  nstats <- length(levels(data.tab$cols))
  pal<-colors[[3]][1:nstats]

if(discrete == FALSE){
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
 }
if (discrete == TRUE){

    xlim <- c(min(as.numeric(x.var), na.rm = TRUE), max(as.numeric(x.var), na.rm = TRUE))
    ylim <- c(min(as.numeric(y.var), na.rm = TRUE), max(as.numeric(y.var), na.rm = TRUE))

    p <-  ggplot(data = data.tab, aes(x = xvalue, y = yvalue, label = category)) +
      geom_point(aes(size = svalue, color = cols, alpha = .3)) +
      theme_gray(base_size = 11) +
      scale_color_manual(values=pal) +
      scale_size(range = c(5,25)) +
      scale_x_continuous(name = "", limits = xlim, breaks = seq(xlim[1],xlim[2],1), labels = levels(x.var)) +
      scale_y_continuous(name = "", limits = ylim, breaks = seq(ylim[1],ylim[2],1), labels = levels(y.var)) +
      geom_text_repel(aes(xvalue, yvalue, label = category)) +
      theme(panel.background = element_rect(fill = "#ffffff"), panel.grid.major = element_line(colour = "#CBCBCB")) +
      theme(legend.position = "none") +
      theme(plot.title = element_text(hjust = 0.5)) + theme(axis.text.y  = element_text(hjust=0, angle=0))
  }
  return(p)
}
