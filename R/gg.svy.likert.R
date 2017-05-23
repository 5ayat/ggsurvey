#' Generates likert plot based on svyby stat
#'
#' Creates a likert-type plot.
#'
#' @param tab The \code{svystat} object from the function \code{svymean}.
#' @param order Plot bars in decending order? Do not use if factor is already in specific order, e.g. survey waves.
#' @param color A color list generated with \code{\link{gg.svy.colors}} (no quotes) or Hex color codes (in quotes).
#' @param line Line width. Default is 6.
#' @param legend.position Legend position.
#' @param legend.name Name for legend.
#'
#' @return A ggplot object.
#'
#' @export



gg.svy.likert <- function(tab,
                          order = FALSE,
                          colors = NULL,
                          line=8,
                          legend.position="bottom",
                          legend.name="",
                          wrap.levels = FALSE,
                          level.width = 15,
                          base_size = 11){

  se.tab <- tab[, substr(names(tab), 1, 3 )== "se."]
  data.tab<-tab[,1:(length(tab)-length(se.tab))]
  var.name<-gsub(".*~|,.*", "", attr(tab,'call'))[2]
  colnames(data.tab)<-gsub(var.name, "", colnames(data.tab))
  nstats<-attr(tab, 'svyby')$nstats

  colors <- colors[[1]]

  if(nstats%%2==1){
    mid<-ceiling(nstats/2)
    diffs<-as.matrix(data.tab)
    bottom.cat <- length(diffs[1,]) - nstats + 1
    diffs<-matrix(as.numeric(diffs[,bottom.cat:(bottom.cat+nstats-1)]),ncol=nstats) * 100
    out<-diffs
    get_sum <- function(v) sum(v * row.w)
    for(i in 1:length(diffs[,1]))
    {
      for(j in 1: length(diffs[1,])){
        if(j <= mid){
          a <- j-1
          b <- mid  - j
          c <- mid - 1
          row.w <- c(rep(0, a), rep(1, b), .5, rep(0,c))
          out[i,j] <- 0 - get_sum(diffs[i,])
        } else {
          a <- mid - 1
          b <- j - (mid + 1)
          c <- nstats - j + 1
          row.w <- c(rep(0, a), .5, rep(1, b), rep(0,c))
          out[i,j] <- 0 + get_sum(diffs[i,])
        }
      }
    }
    out<-data.frame(out)
    new.data<-cbind(data.tab[,1:(length(data.tab[1,])-nstats)],diffs,out)
    colnames(new.data)[1:length(data.tab[1,])]<-colnames(data.tab)
    colnames(new.data)[1] <- "category"
    if(order==TRUE) new.data$category<-factor(new.data$category, levels = new.data$category[order(-(new.data$X1))])
    mdfr <- melt(new.data)
    tl<-length(mdfr[,1])/2
    tc<-which(names(mdfr)=="value")
    mdfr<-cbind(mdfr[1:tl,],mdfr[(tl+1):(tl*2),tc])
    colnames(mdfr)[1]<-"category"
    colnames(mdfr)[tc+1]<-"start"
    mdfr$variable<-droplevels(mdfr$variable)
    pal<-c(colorRampPalette(c(colors[1:2]))(floor(nstats/2)),colors[3],colorRampPalette(c(colors[4:5]))(floor(nstats/2)))
  }

  if(nstats%%2==0){
    diffs<-as.matrix(data.tab)
    bottom.cat <- length(diffs[1,]) - nstats + 1
    diffs<-matrix(as.numeric(diffs[,bottom.cat:(bottom.cat+nstats-1)]),ncol=nstats) * 100
    out<-diffs
    get_sum <- function(v) sum(v * row.w)
    for(i in 1:length(diffs[,1]))
    {
      for(j in 1: length(diffs[1,])){
        if(j <= nstats/2){
          a <- j-1
          b <- nstats/2  - a
          c <- nstats/2
          row.w <- c(rep(0, a), rep(1, b), rep(0,c))
          out[i,j] <- 0 - get_sum(diffs[i,])
        } else {
          a <- nstats/2
          b <- j - (nstats/2) - 1
          c <- nstats - (a + b)
          row.w <- c(rep(0, a), rep(1, b), rep(0,c))
          out[i,j] <- 0 + get_sum(diffs[i,])
        }
      }
    }
    out<-data.frame(out)
    new.data<-cbind(data.tab[,1:(length(data.tab[1,])-nstats)],diffs,out)
    colnames(new.data)[1:length(data.tab[1,])]<-colnames(data.tab)
    colnames(new.data)[1] <- "category"
    if(order==TRUE) new.data$category<-factor(new.data$category, levels = new.data$category[order(-(new.data$X1))])
    mdfr <- melt(new.data)
    tl<-length(mdfr[,1])/2
    tc<-which(names(mdfr)=="value")
    mdfr<-cbind(mdfr[1:tl,],mdfr[(tl+1):(tl*2),tc])
    colnames(mdfr)[1]<-"category"
    colnames(mdfr)[tc+1]<-"start"
    mdfr$start[mdfr$start < -100] <- -100
    mdfr$variable<-droplevels(mdfr$variable)
    pal<-c(colorRampPalette(c(colors[1:2]))(floor(nstats/2)),colorRampPalette(c(colors[4:5]))(floor(nstats/2)))
  }

  roundUp <- function(x,to=10) to*(x%/%to + as.logical(x%%to))
  mymin <- roundUp(min(mdfr$start)*-1, 25)*-1
  mymax <- roundUp(max(mdfr$start+mdfr$value), 25)
  mymin[mymin< -100] <- -100

  if(wrap.levels == FALSE){
    p <-  ggplot(data=mdfr) +
      geom_segment(aes(x = category, y = start, xend = category, yend = start+value, colour = variable), size = line) +
      scale_color_manual(legend.name, values = pal, guide="legend") + theme_gray(base_size = base_size) +
      geom_hline(yintercept = 0, color =c("#646464")) +
      coord_flip() + labs(title="", y="",x="") +
      scale_y_continuous(breaks=seq(mymin,mymax,25), limits=c(mymin,mymax)) +
      theme(panel.background = element_rect(fill = "#ffffff"),
            panel.grid.major = element_line(colour = "#CBCBCB")) +
      theme(legend.position = legend.position) +  guides(colour = guide_legend(override.aes = list(size=8))) + theme(plot.title = element_text(hjust = 0.5)) +
      theme(axis.text.y  = element_text(hjust=0, angle=0)) + ylim(-100, 100)
  }

  if(wrap.levels == TRUE){

    some.levels <- levels(mdfr$variable)
    some.levels <- lapply(strwrap(as.character(some.levels), width=level.width, simplify=FALSE), paste, collapse="\n")
    some.levels<-unlist(some.levels)


    p <-  ggplot(data=mdfr) +
      geom_segment(aes(x = category, y = start, xend = category, yend = start+value, colour = variable), size = line) +
      scale_color_manual(legend.name, values = pal, guide="legend", labels = some.levels) + theme_gray(base_size = base_size) +
      geom_hline(yintercept = 0, color =c("#646464")) +
      coord_flip() + labs(title="", y="",x="") +
      scale_y_continuous(breaks=seq(mymin,mymax,25), limits=c(mymin,mymax)) +
      theme(panel.background = element_rect(fill = "#ffffff"),
            panel.grid.major = element_line(colour = "#CBCBCB")) +
      theme(legend.position = legend.position) +  guides(colour = guide_legend(override.aes = list(size=8))) + theme(plot.title = element_text(hjust = 0.5)) +
      theme(axis.text.y  = element_text(hjust=0, angle=0)) + ylim(-100, 100)
  }

  return(p)
}


