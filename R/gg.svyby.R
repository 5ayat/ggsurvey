#' Generates barplot based on svyby stat
#'
#' This function will either initialize a basic ggplot
#' or generate a barblot with additional aesthetics. The \code{color}
#' parameter generated with \code{\link{gg.svy.colors}} can be used
#' to specify a color mapping.
#'
#' @param tab The \code{svystat} object from the function \code{svymean}.
#' @param basic If TRUE, only a basic ggplot object is initialized.
#' @param color A color list generated with \code{\link{gg.svy.colors}} (no quotes) or a Hex color codes (in quotes).
#' @param percent If TRUE, multiplies all decimal values by 100.
#' @param error.bars If TRUE, adds error bars for 95\% confidence intervals.
#'
#' @return A ggplot object, with or without additional aesthetics, depending on \code{basic} parameter.
#'
#' @export


gg.svyby <- function(tab, basic=FALSE, color=NULL, percent=FALSE, error.bars=TRUE,
                outcome.factor=TRUE, legend.position="right",legend.title="Response",
                drop.level=NULL, error.bar.bounds = FALSE){

  if(outcome.factor == TRUE){
    se.tab <- tab[, substr(names(tab), 1, 3 )== "se."]
    data.tab<-tab[,1:(length(tab)-length(se.tab))]
    var.name<-gsub(".*~|,.*", "", attr(tab,'call'))[2]
    nstats<-attr(tab, 'svyby')$nstats
    colnames(data.tab)<-gsub(var.name, "", colnames(data.tab))
    colnames(se.tab)<-gsub(paste0("se.",var.name), "", colnames(se.tab))
    if(!is.null(drop.level)){
      data.tab <- data.tab[, !(colnames(data.tab) %in% c(drop.level))]
      se.tab <- se.tab[, !(colnames(se.tab) %in% c(drop.level))]
      nstats <- nstats - 1
    }
    m.data.tab <- melt(data.tab)
    m.se.tab<-melt(se.tab)
    m.data.tab$se <- m.se.tab$value
  } else {
    m.data.tab<-tab
    colnames(m.data.tab)[length(m.data.tab[1,])-1]<-"value"
    colnames(m.data.tab)[1]<-"variable"
  }

    m.data.tab$lower <- m.data.tab$value - m.data.tab$se*1.96
    m.data.tab$upper <- m.data.tab$value + m.data.tab$se*1.96

    if(error.bar.bounds == TRUE){
      m.data.tab$lower[m.data.tab$lower < 0] <- 0
      m.data.tab$upper[m.data.tab$upper > 1] <- 1
    }

    if(percent == TRUE){
      m.data.tab$value <- m.data.tab$value*100
      m.data.tab$se <- m.data.tab$se*100
      m.data.tab$lower <- m.data.tab$lower*100
      m.data.tab$upper <- m.data.tab$upper*100
    }

    if(outcome.factor == TRUE){
      pal <- color[[3]][1:nstats]
        if (basic == TRUE){
          p <- ggplot(data=m.data.tab, aes(x=m.data.tab[,1],y=value,fill=variable))
        } else if (error.bars == FALSE){
          p <- ggplot(data=m.data.tab, aes(x=m.data.tab[,1],y=value,fill=variable)) +
            geom_bar(stat="identity", position="dodge") + theme(panel.background = element_rect(fill = "#ffffff")) + theme(legend.position=legend.position) +
            xlab("") + ylab("Percent\n")  + scale_fill_manual(legend.title, values=pal) +
            theme(panel.grid.major.y = element_line(colour="grey80", size=0.2))
        } else if (error.bars == TRUE & error.bar.bounds == FALSE){
          p <- ggplot(data=m.data.tab, aes(x=m.data.tab[,1],y=value, ymin=value-1.96*se, ymax=value+1.96*se, fill=variable)) +
            geom_bar(stat="identity", position="dodge") +
            geom_errorbar(position=position_dodge(width = 0.90), width=0.2, stat="identity") +
            theme(panel.background = element_rect(fill = "#ffffff")) + theme(legend.position=legend.position) +
            xlab("") + ylab("Percent\n")  + scale_fill_manual(legend.title, values=pal) +
            theme(panel.grid.major.y = element_line(colour="grey80", size=0.2))
        } else {
          p <- ggplot(data=m.data.tab, aes(x=m.data.tab[,1],y=value, ymin=lower, ymax=upper, fill=variable)) +
            geom_bar(stat="identity", position="dodge") +
            geom_errorbar(position=position_dodge(width = 0.90), width=0.2, stat="identity") +
            theme(panel.background = element_rect(fill = "#ffffff")) + theme(legend.position=legend.position) +
            xlab("") + ylab("Percent\n")  + scale_fill_manual(legend.title, values=pal) +
            theme(panel.grid.major.y = element_line(colour="grey80", size=0.2))
      }
      return(p)
    } else {
        if(length(attr(tab,'names')) == 3){
            pal <- color[[3]][1]
            if (basic == TRUE){
              p <- ggplot(data=m.data.tab, aes(x=variable,y=value))
            } else if (error.bars == FALSE){
              p <- ggplot(data=m.data.tab, aes(x=m.data.tab[,1],y=value)) +
                geom_bar(stat="identity", position="dodge", fill=pal) + theme(panel.background = element_rect(fill = "#ffffff")) + theme(legend.position=legend.position) +
                xlab("") + ylab("Mean") + theme(panel.grid.major.y = element_line(colour="grey80", size=0.2))
            } else if (error.bars == TRUE & error.bar.bounds == FALSE){
              p <- ggplot(data=m.data.tab, aes(x=variable,y=value, ymin=value-1.96*se, ymax=value+1.96*se)) +
                geom_bar(stat="identity", position="dodge", fill=pal) +
                geom_errorbar(position=position_dodge(width = 0.90), width=0.2, stat="identity") +
                theme(panel.background = element_rect(fill = "#ffffff")) + theme(legend.position=legend.position) +
                xlab("") + ylab("Mean") + theme(panel.grid.major.y = element_line(colour="grey80", size=0.2))
            } else {
              p <- ggplot(data=m.data.tab, aes(x=variable,y=value, ymin=lower, ymax=upper)) +
                geom_bar(stat="identity", position="dodge", fill=pal) +
                geom_errorbar(position=position_dodge(width = 0.90), width=0.2, stat="identity") +
                theme(panel.background = element_rect(fill = "#ffffff")) + theme(legend.position=legend.position) +
                xlab("") + ylab("Mean") + theme(panel.grid.major.y = element_line(colour="grey80", size=0.2))
            }
        } else if(length(attr(tab,'names')) >= 4){
          pal <- color[[3]][1:length(levels(m.data.tab[,2]))]
          colnames(m.data.tab)[2] <- "fill.by"
          if (basic == TRUE){
            p <- ggplot(data=m.data.tab, aes(x=variable,y=value, fill=fill.by))
          } else if (error.bars == FALSE){
              p <- ggplot(data=m.data.tab, aes(x=variable,y=value, fill=fill.by)) +
                geom_bar(stat="identity", position="dodge") + theme(panel.background = element_rect(fill = "#ffffff")) + theme(legend.position=legend.position) +
                xlab("") + ylab("") + scale_fill_manual(legend.title, values=pal) +
                theme(panel.grid.major.y = element_line(colour="grey80", size=0.2))
            } else if (error.bars == TRUE & error.bar.bounds == FALSE){
              p <- ggplot(data=m.data.tab, aes(x=variable,y=value, ymin=value-1.96*se, ymax=value+1.96*se, fill=fill.by)) +
                geom_bar(stat="identity", position="dodge") +
                geom_errorbar(position=position_dodge(width = 0.90), width=0.2, stat="identity") +
                theme(panel.background = element_rect(fill = "#ffffff")) + theme(legend.position=legend.position) +
                xlab("") + ylab("")  + scale_fill_manual(legend.title, values=pal) +
                theme(panel.grid.major.y = element_line(colour="grey80", size=0.2))
            } else {
              p <- ggplot(data=m.data.tab, aes(x=variable,y=value, ymin=lower, ymax=upper, fill = fill.by)) +
                geom_bar(stat="identity", position="dodge") +
                geom_errorbar(position=position_dodge(width = 0.90), width=0.2, stat="identity") +
                theme(panel.background = element_rect(fill = "#ffffff")) + theme(legend.position=legend.position) +
                xlab("") + ylab("")+ scale_fill_manual(legend.title, values=pal) +
                theme(panel.grid.major.y = element_line(colour="grey80", size=0.2))

            }
        }
        return(p)
        }
}



