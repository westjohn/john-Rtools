#' A quantile plot function
#'
#' This function takes an atomic vector of numbers and
#' creates a quantile plot of the data. This function requires
#' GGally, ggplot2, reshape2 (probably)
#'
#' @param data An atomic vector of numbers
#' @param ylabel Label for the quantity being plotted; defaults to "Quantity"
#' @param quantile Vector of numeric vector of probabilities with values in [0,1]
#' representing the quantile boundaries. This defaults to quartiles, e.g.: seq(0,1,by=.25).
#' @keywords quantile plot, quant_plot
#' @export
#' @examples
#' quant_plot(data=df$height,ylabel="Height")


#library(ggplot2)
#library(ggthemes)
#library(plyr)
#library(dplyr)
#library(xlsx)
#library(reshape2)
#library(GGally)

#This creates a quartile plot of the data with lines at 25%, 50%, and 75%
quant_plot <- function(d,ylabel="Quantity",quantile=seq(0,1,by=0.25)) {
    d <- sort(d)
    q <- data.frame(quantile(d))
    names(q)<-"breaks"
    X<- seq(from=1,to=length(d))
    d <- data.frame("data"=d,X)

    q25 <- d[max(which(d$data<=q["25%","breaks"])),"data"]
    q50 <- d[max(which(d$data<=q["50%","breaks"])),"data"]
    q75 <- d[max(which(d$data<=q["75%","breaks"])),"data"]
    q100 <- d[max(which(d$data<=q["100%","breaks"])),"data"]
#     print(d[max(which(d$data>=.75)),"data"])
#     print(nrow(d))

    plt <- ggplot(data=d,aes(y=data,x=X)) +
        geom_point(size=2,alpha=.2,shape=20,size=2) +
        geom_hline(data=data.frame(),yintercept=q25,color="white") +
        geom_hline(data=data.frame(),yintercept=q50,color="white") +
        geom_hline(data=data.frame(),yintercept=q75,color="white") +
        geom_hline(data=data.frame(),yintercept=q100,color="white") +
#         geom_hline(data=data.frame(),yintercept=.75,color="white") +
#         geom_text(data=data.frame(),aes(x=1,y=q25,label="25%"),size=3) +
#         geom_text(data=data.frame(),x=1,y=q50,label="50%",size=3) +
#         geom_text(data=data.frame(),x=1,y=q75,label="75%",size=3) +
#         geom_text(data=data.frame(),x=1,y=q100,label="100%",size=3) +
#         geom_text(data=data.frame(),aes(x=1,y=4.24/32,label="25%"),size=3) +
#         geom_text(data=data.frame(),aes(x=1,y=5.72/32,label="50%"),size=3) +
#         geom_text(data=data.frame(),aes(x=1,y=8.86/32,label="75%"),size=3) +
#         geom_text(data=data.frame(),aes(x=1,y=31.32/32,label="100%"),size=3) +
        ggtitle(paste("Quartile Plot for ",ylabel,collapse = "",sep = "")) +
        scale_y_continuous(breaks=c(q25,q50,q75,q100),
                           labels=c(sprintf("%.2f",q25*32.),
                                    sprintf("%.2f",q50*32.),
                                    sprintf("%.2f",q75*32.),
#                                    sprintf("%.2f",.75*32.),
                                    sprintf("%.2f",q100*32.))) +
        theme(panel.grid.major.y = element_blank(),panel.grid.major.x = element_blank()) +
        theme(panel.grid.minor.y = element_blank(),panel.grid.minor.x = element_blank()) +
        ylab(ylabel) +
        xlab("")
    print(plt)
}
