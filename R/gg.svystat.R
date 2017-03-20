#' Generates a barplot of survey means.
#'
#' This function will either initialize a basic ggplot
#' or generate a barblot with additional aesthetics. The \code{color}
#' parameter generated with \code{\link{gg.svy.colors}} can be used
#' to specify a color mapping. For barplots with additional arguments,
#' see \code{\link{gg.svyby}}, which is based on the \code{survey} packages's \code{svyby} function.
#' It is necessary to specify the variable name used in the previous call to \code{svymean}, because
#' this is not stored in the \code{svystat} object. This is used to get factor levels. Use
#' \code{basic = TRUE} to apply additional custom ggplot aesthetics.
#'
#'
#' @param tab The \code{svystat} object from the function \code{svymean}.
#' @param var.name The variable name used in the call to \code{svymean}, in quotes.
#' @param basic If TRUE, only a basic ggplot object is initialized.
#' @param color A color list generated with \code{\link{gg.svy.colors}} (no quotes) or a Hex color code (in quotes).
#' @param percent If TRUE, multiplies all decimal values by 100.
#' @param error.bars If TRUE, adds error bars for 95\% confidence intervals.
#'
#' @return A ggplot object, with or without additional aesthetics, depending on \code{basic} parameter.
#'
#' @export


gg.svystat <- function(tab, var.name, basic=FALSE, color=NULL, percent=FALSE, error.bars=TRUE){

    if(class(color) == "list") {
      primary <- color[[2]]
    } else {
      primary <- color
    }

  tab<-data.frame(tab)
  rownames(tab) <- gsub(var.name, "", rownames(tab))
  tab$variable <- factor(rownames(tab), levels = rownames(tab))
  colnames(tab) <- c("value","se","variable")

  if(percent == TRUE){
      tab$value <- tab$value*100
      tab$se <- tab$se*100
  }

  if(basic == TRUE){
    p <- ggplot(data=tab, aes(x = variable, y = value))
  } else if(error.bars == FALSE){
    p <- ggplot(data=tab, aes(x = variable, y = value)) + geom_bar(stat="identity", fill = primary) + theme_gray(base_size = 16) +
      theme(panel.background = element_rect(fill = "#ffffff")) + ylab("") + xlab("") +
      theme(panel.grid.major.y = element_line(colour="grey80", size=0.2))
  } else {
    p <- ggplot(data=tab, aes(x = variable, y = value)) + geom_bar(stat="identity", fill = primary) + theme_gray(base_size = 16) +
      theme(panel.background = element_rect(fill = "#ffffff")) + ylab("") + xlab("") +
      geom_errorbar(aes(ymax = value + 1.96*se, ymin = value - 1.96*se), width=0.2,color="grey20") +
      theme(panel.grid.major.y = element_line(colour="grey80", size=0.2))
  }

  return(p)
}


