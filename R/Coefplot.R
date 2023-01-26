## new coefplot
# x: time ATT CI.lower CI.upper p.value count CI.lower.90 CI.upper.90
coefplot <- function(data,# time ATT CI.lower CI.upper p.value count
                     Period, 
                     Estimate,
                     SE,
                     CI.lower = NULL,
                     CI.upper = NULL,
                     p.value = NULL,
                     Count = NULL,
                     fill.gap = TRUE,
                     placeboTest = FALSE,
                     placebo.period = NULL,
                     carryoverTest = FALSE,
                     carryover.period = NULL,
                     show.count = NULL,
                     stats = NULL, # "none", "F.p", "F.equiv.p", "placebo.p", "carryover.p", "equiv.p"
                     stats.labs = NULL,
                     main = NULL,
                     xlim = NULL,
                     ylim = NULL,
                     xlab = NULL, 
                     ylab = NULL,
                     gridOff = FALSE,
                     stats.pos = NULL,
                     theme.bw = TRUE,
                     cex.main = NULL,
                     cex.axis = NULL,
                     cex.lab = NULL, 
                     cex.text = NULL,
                     axis.adjust = FALSE,
                     ...){
  
  scaleFUN <- function(x) sprintf("%.f", x)
  
  # placeboTest
  if (is.logical(placeboTest) == FALSE & is.numeric(placeboTest)==FALSE) {
    stop("\"placeboTest\" is not a logical flag.")
  }
  if (is.null(placebo.period)==FALSE) {
    if (is.numeric(placebo.period)==FALSE) {
      stop("\"placebo.period\" is not numeric.")
    }
  }
  
  # carryoverTest
  if (is.logical(carryoverTest) == FALSE & is.numeric(carryoverTest)==FALSE) {
    stop("\"carryoverTest\" is not a logical flag.")
  }
  if (is.null(carryover.period)==FALSE) {
    if (is.numeric(carryover.period)==FALSE) {
      stop("\"carryover.period\" is not numeric.")
    }
  }
  
  # count
  if (is.logical(show.count) == FALSE & is.numeric(show.count)==FALSE & is.null(show.count)==FALSE) {
    stop("\"show.count\" is not a logical flag.")
  }
  if (is.null(show.count)==TRUE){
    show.count <- FALSE
  }
  
  # gridOff
  if (is.logical(gridOff) == FALSE & is.numeric(gridOff)==FALSE) {
    stop("\"gridOff\" is not a logical flag.")
  }
  
  # title
  if (is.null(main)==FALSE) {
    if (is.character(main) == FALSE) {
      stop("\"main\" is not a string.")
    } else {
      main <- main[1]
    }   
  }
  if (is.null(cex.main)==FALSE) {
    if (is.numeric(cex.main)==FALSE) {
      stop("\"cex.main\" is not numeric.")
    }
    cex.main <- 16 * cex.main
  } else {
    cex.main <- 16
  }
  
  # axis label
  if (is.null(xlab) == FALSE) {
    if (is.character(xlab) == FALSE) {
      stop("\"xlab\" is not a string.")
    }  
  }
  if (is.null(ylab) == FALSE) {
    if (is.character(ylab) == FALSE) {
      stop("\"ylab\" is not a string.")
    }  
  }
  if (is.null(cex.lab)==FALSE) {
    if (is.numeric(cex.lab)==FALSE) {
      stop("\"cex.lab\" is not numeric.")
    }
    cex.lab <- 14 * cex.lab
  } else {
    cex.lab <- 14
  }
  
  # axis number
  if (is.null(cex.axis)==FALSE) {
    if (is.numeric(cex.axis)==FALSE) {
      stop("\"cex.axis\" is not numeric.")
    }
    cex.axis <- 15 * cex.axis
  }  else {
    cex.axis <- 15
  }
  
  # text
  if (is.null(cex.text)==FALSE) {
    if (is.numeric(cex.text)==FALSE) {
      stop("\"cex.text\" is not numeric.")
    }
    cex.text <- 5 * cex.text
  }  else {
    cex.text <- 5
  }
  
  # names for all statistics
  if (is.null(stats)==TRUE){
    stats <- "none"
  }
  
  for (i in 1:length(stats)) {
    if (!stats[i] %in% c("none", "F.p", "F.equiv.p", "placebo.p", "carryover.p", "equiv.p")) {
      stop("\"stats\" option misspecified. Must be one of followings:\"none\", \"F.p\", \"F.equiv.p\", \"placebo.p\", \"carryover.p\", \"equiv.p\".")
    }
  }
  
  if (!("none" %in% stats)) {
    if (is.null(stats.labs)==FALSE) {
      if (length(stats.labs)!=length(stats)) {
        stop("\"stats.lab\" should have the same length as \"stats\".")
      }               
    } 
    else {
      stats.labs <- rep(NA, length(stats)) 
      for (i in 1:length(stats)) {
        if (stats[i] == "F.p") {
          stats.labs[i] <- "F test p-value"
        }
        if (stats[i] == "F.equiv.p") {
          stats.labs[i] <- "F equivalence test p-value"
        }
        if (stats[i] == "F.stat") {
          stats.labs[i] <- "F statistics"
        }
        if (stats[i] == "placebo.p") {
          stats.labs[i] <- "Placebo test p-value"
        }
        if (stats[i] == "carryover.p") {
          stats.labs[i] <- "Carryover effect test p-value"
        }
        if (stats[i] == "equiv.p") {
          if(placeboTest){
            stats.labs[i] <- "Placebo equivalence test p-value"
          }
          else if(carryoverTest){
            stats.labs[i] <- "Carryover effect equivalence test p-value"
          }
          else{
            stats.labs[i] <- "Equivalence test p-value"
          }
        }
      }
    }
  }
  
  # stats positions
  if (!is.null(stats.pos)) {
    if (length(stats.pos) != 2) {
      stop(" \"stats.pos\" must be of length 2. ")
    }
    if (is.numeric(stats.pos[0])==FALSE) {
      stop("Elements of \"stats.pos\" are not numeric.")
    }else{
      if (is.numeric(stats.pos[1])==FALSE) {
        stop("Elements of \"stats.pos\" are not numeric.")    
      }
    }
  }
  
  # axis.adjust
  if (is.logical(axis.adjust) == FALSE & is.numeric(axis.adjust)==FALSE) {
    stop("\"axis.adjust\" is not a logical flag.")
  }
  if (axis.adjust == TRUE) {
    angle <- 45
    x.v <- 1
    x.h <- 1
  } else {
    angle <- 0
    x.v <- 0
    x.h <- 0
  }
  
  if(carryoverTest== TRUE){
    switch.on <- FALSE
  }
  else{
    switch.on <- TRUE
  }
  
  # xlim&ylim
  if (is.null(xlim)==FALSE) {
    if (is.numeric(xlim)==FALSE) {
      stop("Some element in \"xlim\" is not numeric.")
    } else {
      if (length(xlim)!=2) {
        stop("xlim must be of length 2.")
      }
    }
  }
  if (is.null(ylim)==FALSE) {
    if (is.numeric(ylim)==FALSE) {
      stop("Some element in \"ylim\" is not numeric.")
    } else {
      if (length(ylim)!=2) {
        stop("ylim must be of length 2.")
      }
    }
  }
  
  # axes labels
  if (is.null(xlab) == TRUE) {
    if (switch.on == TRUE) {
      xlab <- paste("Time Relative to Treatment")
    } else {
      xlab <- paste("Time Relative to Exiting the Treatment")
    }            
  } else if (xlab == "") {
    if (switch.on == TRUE) {
      xlab <- NULL
    }
  }
    
  if (is.null(ylab) == TRUE) {
    ylab <- paste("Effect on Y")
  } else if (ylab == "") {
    ylab <- NULL
  }
  
  # y=0 line type
  lcolor <- "white"
  lwidth <- 2
  if (theme.bw == TRUE) {
    lcolor <- "#AAAAAA70"
    lwidth <- 1.5
  }
  
  # data
  data <- as.data.frame(data)
  if(!is.null(xlim)){
    data <- data[which(data[,Period]>=xlim[1] & data[,Period]<=xlim[2]),]
  }
  time <- data[,Period]
  ATT <- data[,Estimate]
  se <- data[,SE]
  if(!is.null(CI.lower)){
    CI.lower <- data[,CI.lower]
  }
  if(!is.null(CI.upper)){
    CI.upper <- data[,CI.upper]
  }

  if(!is.null(Count)){
    count.num <- data[,Count]
  }
  else{
    count.num <- rep(0,length(time))
  }
  
  if(!is.null(se)){
    if(is.null(CI.lower)){
      CI.lower <- ATT - 1.96*se
    }
    if(is.null(CI.upper)){
      CI.upper <- ATT + 1.96*se
    }
  }

  # add default zero
  time_lag <- max(time)-min(time)+1
  if(fill.gap){
    if(time_lag>dim(data)[1]){
      time.add <- setdiff(c(min(time):max(time)),time)
      time <- c(time,time.add)
      ATT <- c(ATT,rep(0,length(time.add)))
      se <- c(se,rep(0,length(time.add)))
      CI.lower <- c(CI.lower,rep(0,length(time.add)))
      CI.upper <- c(CI.upper,rep(0,length(time.add)))
      count.num <- c(count.num,rep(0,length(time.add)))
    }    
  }

  # Length of variables
  if (!(length(ATT)==length(time) & length(CI.lower)==length(time) & length(CI.upper)==length(time))){
    stop("The length of time, ATT, and uncertainty estimations must be the same.")
  }
  else{
    if (show.count == TRUE & !(length(count.num)==length(time))){
    stop("The length of time and observation counts must be the same.")
    }
  }
  
  # p.value
  if (length(p.value) < length(stats.labs)){
    stop(paste0("Missing statistics \"",stats.labs[length(p.value)+1],"\" in \"p.value\"."))
  }
  
  
  # count
  if (is.null(count.num) == TRUE){
    max.count <- NULL
    if (show.count == TRUE){
      stop("Input \"data\" must include variable \"Count\" if option \"show.count\" is TRUE.")
    }
  }
  else{
    max.count <- max(count.num)
  }
  
  # time
  #T0.fill <- NULL
  #if (min(time) >= 0){
  #  stop("\"time\" must be relative to treatment.")
  #}
  #if (0%in%time == FALSE){
  #  message("No coefficient for period 0.")
  #  T0.fill <- max(which(time<0))
  #  time <- append(time,0,T0.fill)
  #  ATT <- append(ATT,0,T0.fill)
  #  CI.lower <- append(CI.lower,0,T0.fill)
  #  CI.upper <- append(CI.upper,0,T0.fill)
  #  count.num <- append(count.num,0,T0.fill)
  #}
  
  p.value <- c(p.value,rep(0,length(time)-length(p.value)))
  
  data <- cbind.data.frame(time = time, ATT = ATT, CI.lower = CI.lower,
                           CI.upper=CI.upper,count=count.num,p.value = p.value)
  
  p <- ggplot(data)
  
  max.count.pos <- time[which(count.num == max.count)]
  
  if(carryoverTest== TRUE){
    best.pos <- 0
  }
  else{
    best.pos <- 1
  }
  
  if (length(max.count.pos)>1) {
    if (best.pos %in% max.count.pos) {
      max.count.pos <- best.pos
    } 
    else if ((1-best.pos) %in% max.count.pos) {
      max.count.pos <- 1-best.pos
    } 
    else {
      max.count.pos <- max.count.pos[1]
    }
  }
  
  # height of the histogram
  rect.length <- (max(data$CI.upper, na.rm = TRUE) - min(data$CI.upper, na.rm = TRUE))/2
  rect.min <- min(data$CI.lower, na.rm = TRUE) - rect.length 
  
  # xlab and ylab
  p <- p + xlab(xlab) +  ylab(ylab) 
  
  # theme
  if (theme.bw == TRUE) {
    p <- p + theme_bw() 
  }
  
  ## grid
  if (gridOff == TRUE) {
    p <- p + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  }
  
  # horizontal 0 line
  p <- p + geom_hline(yintercept = 0, 
                      colour = lcolor, 
                      size = lwidth)
  
  # vertical 0 line
  p <- p + geom_vline(xintercept = 0.5, colour=lcolor,size = lwidth*0.7)    
  p <- p + geom_pointrange(data = data, aes(x = time, y = ATT, ymin=CI.lower, ymax=CI.upper), lwd=0.6, color="black", fill="black",fatten = 2)
  
  T0 <- which(data[,"time"] == 0)
  #if (is.null(T0.fill) == TRUE){
  #  T0 <- which(data[,"time"] == 0)
  #}
  #else{
  #  T0 <- which(data[,"time"] == 0)-1
  #}
  
  
  maintext <- "Estimated ATT"
  
  # placeboTest
  if (placeboTest == TRUE){
    maintext <- "Placebo Test"
    if (is.null(placebo.period)){
      placebo.period <- 3
    }
    
    if (length(placebo.period)>2){
      stop("You misspecified \"placebo.period\".")
    }
    
    if (length(placebo.period)==1){
      placebo.period <- (T0-placebo.period+1):T0
    }else{
      if (length(placebo.period)==2){
        placebo.period <- (T0+placebo.period[1]):(T0+placebo.period[2])
      }
    }
    
    p <- p + geom_pointrange(data = data[placebo.period,],  aes(x = time, y = ATT, ymin=CI.lower, ymax=CI.upper), lwd=0.6, color="blue", fill="blue",fatten = 2)
  }
  
  # carryoverTest
  if (carryoverTest == TRUE){
    maintext <- "Carryover Test"
    if (is.null(carryover.period)){
      carryover.period <- 3
    }
    
    if (length(carryover.period)>2){
      stop("You misspecified \"carryover.period\".")
    }
    #if (is.null(T0.fill)!=TRUE){
    #  T0 <- T0+1
    #}
    if (length(carryover.period)==1){
      carryover.period <- (T0+1):(T0+carryover.period)
    }else{
      if (length(carryover.period)==2){
        carryover.period <- (T0+carryover.period[1]):(T0+carryover.period[2])
      }
    }

    p <- p + geom_pointrange(data = data[carryover.period,],  aes(x = time, y = ATT, ymin=CI.lower, ymax=CI.upper), lwd=0.6, color="red", fill="red",fatten = 2)
  }
  
  # axes
  p <- p + theme(axis.title=element_text(size=cex.lab),
                 axis.title.x = element_text(margin = margin(t = 8, r = 0, b = 0, l = 0)),
                 axis.title.y = element_text(margin = margin(t = 0, r = 0, b = 0, l = 0)),
                 axis.text = element_text(color="black", size=cex.axis),
                 axis.text.x = element_text(size = cex.axis, angle = angle, hjust=x.h, vjust=x.v),
                 axis.text.y = element_text(size = cex.axis),
                 plot.title = element_text(size = cex.main, hjust = 0.5, face="bold", margin = margin(10, 0, 10, 0)))
  
  # histogram
  if (show.count == TRUE) {
    data[,"xmin"] <- data[,"time"] - 0.2
    data[,"xmax"] <- data[,"time"] + 0.2
    data[,"ymin"] <- rep(rect.min, dim(data)[1])
    data[,"ymax"] <- rect.min + (data[,"count"]/max.count) * 0.6 * rect.length
    xx <- range(data$time)
    p <- p + geom_rect(data = data, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), 
                       fill = "grey70", colour = "grey69", alpha = 0.4, size = 0.2)
    p <- p + annotate("text", x = max.count.pos, #- 0.02 * (xx[2]-xx[1]), 
                      y = max(data$ymax) + 0.2 * rect.length, 
                      label = max.count, size = cex.text * 0.8, hjust = 0.5)                
  }
  
  # stats.pos
  if (!is.null(stats)){
    if (!"None" %in% stats){
      if (is.null(stats.pos)) {
        stats.pos[1] <- min(data[,"time"], na.rm = 1)
        stats.pos[2] <- ifelse(is.null(ylim), max(data[,"CI.upper"], na.rm = 1)*1.16, ylim[1])
      }
    }
  }
  
  # p.label
  p.label <- NULL
  for (i in 1:length(stats)) {
    if ("F.p" %in% stats[i]) {
      f.p <- data$p.value[1]
      p.label1 <- NULL
      p.label1 <- paste0(stats.labs[i],": ", sprintf("%.3f",f.p))
      p.label <- paste0(p.label, p.label1, "\n")
    }
    if ("F.equiv.p" %in% stats[i]) {
      f.equiv.p <- data$p.value[2]
      p.label1 <- NULL
      p.label1 <- paste0(stats.labs[i],": ", sprintf("%.3f",f.equiv.p))
      p.label <- paste0(p.label, p.label1, "\n")
    }
    if ("placebo.p" %in% stats[i]) {
      placebo.p <- data$p.value[1]
      p.label1 <- NULL
      p.label1 <- paste0(stats.labs[i],": ", sprintf("%.3f",placebo.p))
      p.label <- paste0(p.label, p.label1, "\n")
    }
    if ("carryover.p" %in% stats[i]) {
      carryover.p <- data$p.value[1]
      p.label1 <- NULL
      p.label1 <- paste0(stats.labs[i],": ", sprintf("%.3f",carryover.p))
      p.label <- paste0(p.label, p.label1, "\n")
    }
    if ("equiv.p" %in% stats[i]) {
      if (placeboTest==TRUE){
      placebo.equiv.p <- data$p.value[2]
      p.label1 <- NULL
      p.label1 <- paste0(stats.labs[i],": ", sprintf("%.3f", placebo.equiv.p))
      p.label <- paste0(p.label, p.label1, "\n")}
    
      if (carryoverTest==TRUE){
        carryover.equiv.p <- data$p.value[2]
        p.label1 <- NULL
        p.label1 <- paste0(stats.labs[i],": ", sprintf("%.3f", carryover.equiv.p))
        p.label <- paste0(p.label, p.label1, "\n")}
    }
  } 
  p <- p + annotate("text", x = stats.pos[1], y = stats.pos[2], 
                    label = p.label, size = cex.text * 0.8, hjust = 0) 
  
  # xlim & ylim
  if (is.null(ylim) == TRUE) {
    p <- p + ylim(c(NA,max(data[,"CI.upper"], na.rm = 1)*1.3))
  }
  else{
    p <- p + ylim(ylim = ylim)
  }

  # title
  if (is.null(main) == TRUE) {
    p <- p + ggtitle(maintext)
  } 
  else if (main!=""){
    p <- p + ggtitle(main)
  }
  
  if(dim(data)[1]>4){
    p <- p + scale_x_continuous(labels=scaleFUN)
  }
  else{
    p <- p + scale_x_continuous(breaks=c(data[,'time']))
  }

  
  
  return(p)
}

