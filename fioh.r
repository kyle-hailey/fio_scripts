#
#  example
#
#   graphit(m)
#
#   will graph read tests across a varying number of users loads found in m
#
#   graphit takes a number of optional parameters
#      i_poly=0 - turns off the diagraming of polygons around avg, 95% and 99% lat
#      i_hist=0 - turns off graphing the I/O histograms
#      i_plot_avg = 0 - turn off graphing average latency
#      i_plot_max = 0 - turn off graphing max latency
#      i_plot_95 = 0 - turn off graphing 95% latency
#      i_plot_max = 0 - turn off graphing 99% latency
#      i_plots = 2  - only plot 2 graphs (don't plot the scaling graph, middle graph)
#      i_scalelat = "avg" - latency used to graph latency in middle graph, options are
#                    "95", "99", "9999"

graphit <- function(
                    m,i_name="undefined",i_users=0,i_bs="undefined", i_title="default title",i_hist=1,i_poly=1,
                    i_plot_avg  = 1 ,
                    i_plots = 2
                    ) {

  #
  #  COLOR Definition
  #
     colors1 <- c(
            "#00007F", # 50u   1 blue
            "#0000BB", # 100u  5
            "#0000F7", # 250u
            "#00ACFF", # 500u  6
            "#00E8FF", # 1ms   7
            "#25FFD9", # 2ms   8
            "#61FF9D", # 4ms   9
            "#9DFF61", # 10ms  10
            "#FFE800", # 20ms  12 yellow
            "#FFAC00", # 50ms  13 orange
            "#FF7000", # 100ms 14 dark orang
            "#FF3400", # 250ms 15 red 1
            "#F70000", # 500ms 16 red 2
            "#BB0000", # 1s    17 dark red 1
            "#7F0000", # 2s    18 dark red 2
            "#4F0000") # 5s    18 dark red 2

     bw <- c(
            "#808080", # 50u   1 blue
            "#808080", # 100u  5
            "#808080", # 250u
            "#808080", # 500u  6
            "#808080", # 1ms   7
            "#808080", # 2ms   8
            "#808080", # 4ms   9
            "#808080", # 10ms  10
            "#808080", # 20ms  12 yellow
            "#808080", # 50ms  13 orange
            "#808080", # 100ms 14 dark orang
            "#808080", # 250ms 15 red 1
            "#808080", # 500ms 16 red 2
            "#808080", # 1s    17 dark red 1
            "#808080", # 2s    18 dark red 2
            "#808080") # 5s    18 dark red 2

      colors <- colors1;

       rr <- m ;

  # HISTOGRAM extract the histogram latency values out of rr
      hist <- cbind(rr['us50'],rr['us100'], rr['us250'],rr['us500'],rr['ms1'],
               rr['ms2'],rr['ms4'],rr['ms10'],rr['ms20'],rr['ms50'],
               rr['ms100'],rr['ms250'],rr['ms500'],rr['s1'],rr['s2'],rr['s5'])


  #  HISTOGRAM buckets for main graph
      thist  <- t(hist)
  #
  #  HISTOGRAM slices for MB/s bar graph
  #
      fhist   <- apply(hist, 1,as.numeric)
      fhist   <- fhist/100

  #
  # extract various columns from the data (in rr)
  #
 # extract various columns from the data (in rr)
  #
      lat   <- as.numeric(t(rr['lat']))
      users <- as.numeric(t(rr['users']))
      bs    <- as.character(t(rr['bs']))
      min   <- as.numeric(t(rr['min']))
      maxlat<- as.numeric(t(rr['max']))
      std   <- as.numeric(t(rr['std']))
      MB    <- as.numeric(t(rr['MB']))
      p95_00 <- as.numeric(t(rr['p95_00']))
      p99_00 <- as.numeric(t(rr['p99_00']))
      p99_50 <- as.numeric(t(rr['p99_50']))
      p99_90 <- as.numeric(t(rr['p99_90']))
      p99_95 <- as.numeric(t(rr['p99_95']))
      p99_99 <- as.numeric(t(rr['p99_99']))
      cols  <- 1:length(lat)
      minlat <- 0.05
      p95_00 <- pmax(p95_00 ,minlat)
      p99_00 <- pmax(p99_00, minlat)
      p99_50 <- pmax(p99_50, minlat)
      p99_90 <- pmax(p99_90, minlat)
      p99_95 <- pmax(p99_95, minlat)
      p99_99 <- pmax(p99_99, minlat)
      lat    <- pmax(lat, minlat)
      maxlat <- pmax(maxlat, p99_99)  # sometimes p99_99 is actaully larger than max
  #
  # widths used for overlaying the histograms
  #
      xmaxwidth <- length(lat)+1
      xminwidth <- .5
# doesn't look used
# looks like "cols" is used instead
      pts <- 1:nrow(thist)
      ymax=1000  # max can be adjusted, 1000 = 1sec, 5000 = 5 sec
      ymin=0.100 # ymin has to be 0.1 to get the histograms to line up with latency
      ylims <-  c(ymin,ymax)

  #
  #  LABEL= BLOCK SIZE
  #
      if ( i_users > 0 ) { col_lables <- bs }
  #
  #  LABEL = USERS
  #
      if ( i_bs != "undefined" ) { col_lables <- users }

  #
  # LAYOUT
  #
  #    top  :    large squarish     on top     for latency
  #    botom:    shorter rectangle  on bottom  for MB/s
  #
     if ( i_plots == 2 )  {
      #  matrix(data, nrow, ncol, byrow)
         nf <- layout(matrix(c(2:1)), widths = 13, heights = c(7, 3), respect = TRUE)
     }
     if ( i_plots == 3 )  {
         nf <- layout(matrix(c(3:1)), widths = 13, heights = c(7, 3, 3), respect = TRUE)
     }
  #
  # set margins (bottom, left, top, right)
  #   get rid of top, so the bottome graph is flush with one above
  #            B  L  T  R
     par(mar=c(2, 4, 0, 4))

  #
  # GRAPH  NEW  1
  #
  #     MB/s BARS in bottom graph
  #
     logMB <- log(MB+1)
  #  MBbars <- t(t(fhist)*MB)
     MBbars <- t(t(fhist)*logMB)
     colnames(MBbars) = col_lables
  #            B  L  T  R
     par(mar=c(2, 4, 0, 4))
     colors <- bw;
     op <- barplot(MBbars,col=colors,ylab="MB/s",border=NA,space=1, ylim=c(0,log(1200)),xlim=c(1,2*length(lat)+1),
            yaxt  = "n" )
     #text(op, pmin((logMB),log(400)),round(MB),adj=c(0.2,-.2),col="gray20")

    ypts  <-  c(    log(2),       log(11),    log(101),  log(1001));
    ylbs  <-  c(  "1",  "10", "100",  "1000");
    axis(2,at=ypts, labels=ylbs)
 # GRAPH  NEW   2
  #
  #      SCALING BARS in middle graph
  #
  #            B  L  T  R
    par(mar=c(1, 4, 1, 4))

  #
  # GRAPH  NEW  3
  #
  #  AVERGE latency  line
  #
  #  LOG SCALE
    mylog <- "y"


  # AVERGE get's ploted twice because there has to be something to initialize the graph
  # whether that something is really wanted or used, the graph has to be initialized
  # probably a better way to initialize it, will ook into later
  # sets up YAXIS in LOGSCALE
   plot(1,1, type = "l", xaxs = "i", lty = 1, col = "gray30", lwd = 0.5 , bty = "l",  xlim = c(xminwidth,xmaxwidth), ylim = ylims, ylab = "" , xlab="",log = mylog, yaxt = "n" , xaxt ="n")
  #  if ( i_plot_avg == 1 ) {
  #    plot(cols, lat, type = "l", xaxs = "i", lty = 1, col = "gray30", lwd = 0.5 , bty = "l",
  #         xlim = c(xminwidth,xmaxwidth), ylim = ylims, ylab = "" , xlab="",log = mylog, yaxt = "n" , xaxt ="n")
  #    # text(cols,lat,round(lat,1),adj=c(1,2))
  #    # title(main=i_title)
  #  }


 #
 #  HISTOGRAMS : overlay histograms on line graphs
 #
    colors <- colors1;
    alat = {}
    if ( i_hist == 1 ) {
      par(new = TRUE )
      for (i in 1:ncol(thist)){
          xmin <-   -i + xminwidth
          xmax <-   -i + xmaxwidth
          ser <- as.numeric(thist[, i])
          #cat(ser)
          #cat("\n")
          ser <- ser/100
          sims <- c(rtruncnorm( 100*i%%7, a=.1, b=1000, mean=.1,  sd=i%%7),
                    rtruncnorm( 50*i%%3,  a=.1, b=1000, mean=20,   sd=i%%3),
                    rtruncnorm(  5,       a=.1, b=1000, mean=500, sd=2))
          #sims <- c(rtruncnorm( 8, a=.1, b=1000, mean=.5,  sd=1))
          sims=round(sims,2)
          cat(i)
          cat("  i\n")
          # don't know why dividing by 10 seems to work
          avg = mean(sims)/10
          cat(avg)
          cat("  avg\n")
          cat(sims)
          cat("  sims\n")
          #h= hist(sims,  breaks = seq(from=0, to=170, by=10),  plot=FALSE)
          h= hist(sims,  breaks = c(.1,.25,.5,1,2,4,10,20,50,100,200,500,1000),  plot=FALSE)
          ser = h$counts/sum(h$counts)
          alat = c(alat,avg)
          col=ifelse(ser==0,"white","grey")
          bp <- barplot(ser, horiz = TRUE, axes = FALSE,
                xlim = c(xmin, xmax), ylim = c(0,nrow(thist)),
                border = NA, col = colors, space = 0, yaxt = "n")
          par(new = TRUE)
      }
    }

  #
  #  AVERGE latency  line
  #
    if ( i_plot_avg == 1 ) {
      par(new = TRUE)
#     plot(cols,  lat, type = "l", xaxs = "i", lty = 1, col = "gray30", lwd = 0.5, bty = "l",
      plot(cols, alat, type = "l", xaxs = "i", lty = 1, col = "gray30", lwd = 0.5, bty = "l",
           xlim = c(xminwidth,xmaxwidth), ylim = ylims, ylab = "" , xlab="",log = mylog, yaxt = "n" , xaxt ="n")
#
#     average numbers
#
#     cex is the font size ajdustment factor
#
#     text(cols,lat,round(lat,1),adj=c(1,2), cex=0.5)
#     title(main=i_title)
    }

  #
  # right hand tick lables
  #
    if ( i_hist == 1 ) {
      ypts  <- c(.05,.100,.250,.500,1,2,4,10,20,50,100,200,500,1000,2000,5000)
      ylbs=c("us50","us100","us250","us500","ms1","ms2","ms4","ms10","ms20","ms50","ms100","ms200","ms500","s1","s2","s5" )
      #axis(4,at=ypts, labels=ylbs,las=1,cex.axis=.75,lty=0,lwd=0?
      for ( j in 1:length(ypts) ) {
         axis(4,at=ypts[j], labels=ylbs[j],col=colors[j],las=1,cex.axis=.75,lty=1,lwd=5)
      }
   }

  #
  # left hand tick lables
  #
    ypts  <-  c(0.100,    1,       10,    100,  1000, 5000);
    ylbs  <-  c("100u"   ,"1m",  "10m", "100m",  "1s","5s");
    axis(2,at=ypts, labels=ylbs)

  #
  # reference dashed line at 10ms
  for ( i in  c(10)  )  {
  #  segments(0,   i, xmaxwidth,  i,  col="orange",   lwd=1,lty=2)
  }
  #

}
