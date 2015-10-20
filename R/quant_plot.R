#' A quantile plot function
#'
#' This function takes an atomic vector of numbers and
#' creates a quantile plot of the data. This function requires
#' GGally, ggplot2, reshape2 (probably)
#'
#' @param d An atomic vector of numbers to examine using the quantile plot
#' @param ylabel Label for the quantity being plotted; defaults to "Quantity"
#' @param quantiles Vector of numeric vector of probabilities with values in [0,1]
#' representing the quantile boundaries. This defaults to quartiles, e.g.: seq(0,1,by=.25).
#' @param color R color name (e.g., "blue") in which to render the colors indicating the quantiles; defaults to "red"
#' @keywords quantile plot, quant_plot
#' @export
#' @examples
#' quant_plot(d=df$height,ylabel="Height")
#' quant_plot(d=rnorm(1000),ylabel="Normal Distribution",quantiles=c(0,.33,.667,1),color="blue")


quant_plot <- function(d,ylabel="Quantity",quantiles=seq(0,1,by=0.25),color = "red") {
    d <- sort(d)
    quantile_data <- data.frame(breaks=quantile(d,probs = quantiles))
    quantile_data$quantiles <- rownames(quantile_data)
    rownames(quantile_data) <- NULL

    X <- seq(from=1,to=length(d))
    d <- data.frame("data"=d,X)
    
    if(nrow(d) > 5000) {
        PT_ALPHA <- .075
    }else if(nrow(d) > 999) {
        PT_ALPHA <- .1
    }else if(nrow(d) >499) {
        PT_ALPHA <- .5
    }else if(nrow(d) >99) {
        PT_ALPHA <- .75
    }else if(nrow(d) >9) {
        PT_ALPHA <- 1.
    }
    else {
        PT_ALPHA <- 1.
    }
    
    ALPHA_VAL <- .2
    
    plt <- ggplot(data=d,aes(y=data,x=X))
    for (bk in 1:(nrow(quantile_data)-1)) {
        plt <- plt + annotate("rect",xmin=min(which(d$data<=quantile_data[bk,"breaks"])),
                        ymin=d[min(which(d$data<=quantile_data[bk,"breaks"])),"data"],
                        xmax=max(which(d$data<=quantile_data[bk+1,"breaks"])),
                        ymax=d[max(which(d$data<=quantile_data[bk+1,"breaks"])),"data"],fill=color,alpha=ALPHA_VAL)
    }
    plt <- plt + scale_y_continuous(breaks=quantile_data$breaks,
                                    label=paste(" ",sprintf("%0.2f",quantile_data[,"breaks"]),
                                                "::",quantile_data[,"quantiles"]))
    plt <- plt + 
        geom_point(size=2,alpha=PT_ALPHA,shape=20,size=2) +
        ggtitle(paste("Quantile Plot for ",ylabel,collapse = "",sep = "")) +
        theme(panel.grid.major.y = element_blank(),panel.grid.major.x = element_blank()) +
        theme(panel.grid.minor.y = element_blank(),panel.grid.minor.x = element_blank()) +
        theme(panel.background = element_blank()) +
        theme(axis.ticks.y=element_blank()) +
        theme(axis.text.y=element_text(hjust=1.75)) +
        ylab(ylabel) +
        xlab("Index of Sorted Data Value")
    print(plt)
}
