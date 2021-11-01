#
#  example
#
#   graphit(m)
# 
#   where m is the data.frame with the fio data
#   example m data frames in the files data_[type].r
#   source one of the files to instantiate an m
#
#   to create a m data.frame from fio output files run
#   fioparse [ list of output files] > data_type.r
#   then source data_type.r in your R session
#      source("data_type.r")
#
#   default graphit(m) graphs random reads across varying user
#   loads, so the m data has to have at least one random read run
#
#   graphit(m) can also graph "read" tests or "write" tests
#   graphit will vary on the X axis the # of users or the I/O size
#   if I/O size is specified, then the X axis varies the number of users
#   if number of users is specified then the X axis is I/O sizes
#   thus the data should have several different user loads and/or I/O sizes
#   for example
#
#   graphit(m,i_name="read",i_bs="8K")
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
                    i_plot_max  = 1 ,
                    i_plot_95   = 1 ,
                    i_plot_99   = 0 ,
                    i_plot_9999 = 0 ,
                    i_scalelat  = "avg" ,
                    i_plots = 2
                    ) {

  # 
  #  COLOR Definition 
  #
     colors <- c(
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

  #
  #   example
  #
  #    rr <- m ;
  #    rr <- subset(rr,rr['bs'] == "8K" )
  #    rr <- subset(rr,rr['name'] == "randread" )

  #
  # rr will be the subset of m that is graphed
  #
       rr <- m ;

  #
  # filter by test name, if no test name make it 8K random read by default
  #
  #    DEFAULT : RANDOM READ 
  #
      if ( i_name != "undefined" ) {
         rr <- subset(rr,rr['name'] == i_name )
         cat("rr filtered for name=",i_name,"\n");
         print(rr)
      } else {
         rr <- subset(rr,rr['name'] == "randread" )
         i_bs = "8K"
         cat("no name\n");
         i_scalex = "users"
      }
  #   if ( i_name == "randread" ) {
  #      maxMBs = 100
  #   }
  #   if ( i_name == "read" ) {
  #      maxMBs = 400
  #   }
  #   if ( i_name == "write" ) {
  #      maxMBs = 100
  #   }
  # 
  # if i_users > 0 then users is defined as a single value
  # ie, block sizes vary 
  #
  #   XAXIS =  BLOCK SIZE    
  #
      if ( i_users > 0 ) {
        rr <- subset(rr,rr['users'] == i_users )
        cat("rr filterd for users=",i_users,"\n");
        print(rr)
        i_scalex = "bs"
      } else {
        cat("no users\n");
      }
  # 
  # if i_bs > 0 then block size is defined as a single value
  # ie, users vary 
  #
  #   XAXIS =   USERS   
  #
      if ( i_bs != "undefined" ) {
        rr <- subset(rr,rr['bs'] == i_bs )
        cat("rr filterd for bs=",i_bs,"\n");
        print(rr)
        i_scalex = "users"
      } else {
        cat("no block sise\n");
      }

  #
  # HISTOGRAM extract the histogram latency values out of rr
  #
      hist <- cbind(rr['us50'],rr['us100'], rr['us250'],rr['us500'],rr['ms1'],
               rr['ms2'],rr['ms4'],rr['ms10'],rr['ms20'],rr['ms50'],
               rr['ms100'],rr['ms250'],rr['ms500'],rr['s1'],rr['s2'],rr['s5']) 

  #
  #  > 10ms IOPS
  #
 #     ms10more <- as.numeric(t(rr['ms20'])) +
 #       as.numeric(t(rr['ms50'])) +
 #       as.numeric(t(rr['ms100'])) +
 #       as.numeric(t(rr['ms250'])) +
 #       as.numeric(t(rr['ms500'])) +
 #       as.numeric(t(rr['s1'])) +
 #       as.numeric(t(rr['s2'])) +
 #       as.numeric(t(rr['s5'])) 
#
#      ms1more <- 
#        as.numeric(t(rr['ms2'])) +
#        as.numeric(t(rr['ms4'])) +
#        as.numeric(t(rr['ms10'] ))
#
#      ms1less <- as.numeric(t(hist['us50'])) +
#        as.numeric(t(hist['us100'])) +
#        as.numeric(t(hist['us250'])) +
#        as.numeric(t(hist['us500'])) +
#        as.numeric(t(hist['ms1']))  
  #
  #  10ms IOPS matrix
  #
#      mstotal <- ms1less + ms1more + ms10more
#      ms1less  <- (ms1less/mstotal)
#      ms1more  <- (ms1more/mstotal)
#      ms10more <- (ms10more/mstotal)
#      ms10 <- rbind(ms1less,ms1more,ms10more)
#      print(ms10)

  #
  #  HISTOGRAM buckets for main graph
  #
      thist  <- t(hist)
  #
  #  HISTOGRAM slices for MB/s bar graph
  #
      fhist   <- apply(hist, 1,as.numeric)
      fhist   <- fhist/100
 
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
     op <- barplot(MBbars,col=colors,ylab="MB/s",border=NA,space=1, ylim=c(0,log(1200)),xlim=c(1,2*length(lat)+1),
            yaxt  = "n" )
     text(op, pmin((logMB),log(400)),round(MB),adj=c(0.2,-.2),col="gray20")

    ypts  <-  c(    log(2),       log(11),    log(101),  log(1001));
    ylbs  <-  c(  "1",  "10", "100",  "1000");
    axis(2,at=ypts, labels=ylbs)

  #
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

  #
  #
  #  Bottom to top light colored Bars per test 
  #
  #  op <- barplot(ms10, col=c("#C6D4F8", "#C9FACF",  "#FFF6A0"),ylim =c(0,1), xlab="", ylab="",border=NA,space=0,yaxt="n",xaxt="n") 
  #  par(new = TRUE )

  # AVERGE get's ploted twice because there has to be something to initialize the graph
  # whether that something is really wanted or used, the graph has to be initialized
  # probably a better way to initialize it, will ook into later
  # sets up YAXIS in LOGSCALE
    if ( i_plot_avg == 1 ) {
      plot(cols, lat, type = "l", xaxs = "i", lty = 1, col = "gray30", lwd = 1, bty = "l", 
           xlim = c(xminwidth,xmaxwidth), ylim = ylims, ylab = "" , xlab="",log = mylog, yaxt = "n" , xaxt ="n")
      text(cols,lat,round(lat,1),adj=c(1,2))
      # title(main=i_title)
    }

 #
 #  POLYGONS showing the 95%, 99%, 99.99%  curves
 #
 #    will only be in logscale if last plot is log scale
 # 
#   if ( i_poly == 1 ) {
#     if ( i_plot_95   == 1 ) {
#       polygon(c(cols,rev(cols)),c(   lat,rev(p95_00)), col="gray80",border=NA)
#     }
#     if ( i_plot_99   == 1 ) {
#       polygon(c(cols,rev(cols)),c(p95_00,rev(p99_00)), col="gray90",border=NA)
#     }
#     if ( i_plot_9999 == 1 ) {
#       polygon(c(cols,rev(cols)),c(p99_00,rev(p99_99)), col="gray95",border=NA)
#     }
#   }

 #
 #  HISTOGRAMS : overlay histograms on line graphs
 #
    if ( i_hist == 1 ) {
      par(new = TRUE )
      for (i in 1:ncol(thist)){
          xmin <-   -i + xminwidth 
          xmax <-   -i + xmaxwidth 
          ser <- as.numeric(thist[, i])
          ser <- ser/100 
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
      plot(cols, lat, type = "l", xaxs = "i", lty = 1, col = "gray30", lwd = 1, bty = "l", 
           xlim = c(xminwidth,xmaxwidth), ylim = ylims, ylab = "" , xlab="",log = mylog, yaxt = "n" , xaxt ="n")
      text(cols,lat,round(lat,1),adj=c(1,2))
      title(main=i_title)
    }

  #
  # 95% latency 
  #
    if ( i_plot_95 == 1 ) {
      par(new = TRUE)
      plot(cols, p95_00, type = "l", xaxs = "i", lty = 5, col = "grey40", lwd = 1, bty = "l", 
       xlim = c(xminwidth,xmaxwidth), ylim = ylims, ylab = "" , xlab="",log = mylog, yaxt = "n" , xaxt ="n") 
      #text(cols,p95_00,round(p95_00,1),adj=c(0,0),col="gray70")
      text(tail(cols,n=1),tail(p95_00, n=1),"95%",adj=c(0,0),col="gray20",cex=.7)
    }

   cat("hello 3\n")
  #
  # 99% latency 
  #
    if ( i_plot_99 == 1 ) {
      par(new = TRUE)
      plot(cols, p99_00, type = "l", xaxs = "i", lty = 2, col = "grey60", lwd = 1, bty = "l", 
         xlim = c(xminwidth,xmaxwidth), ylim = ylims, ylab = "" , xlab="",log = mylog, yaxt = "n" , xaxt ="n") 
      #text(cols,p99_00,round(p99_00,1),adj=c(0,0),col="gray70")
      text(tail(cols,n=1),tail(p99_00, n=1),"99%",adj=c(0,0),col="gray20",cex=.7)
    }

  #
  # 99.99% latency 
  #
    if ( i_plot_9999 == 1 ) {
      par(new = TRUE)
      plot(cols, p99_99, type = "l", xaxs = "i", lty = 3, col = "grey70", lwd = 1, bty = "l", 
          xlim = c(xminwidth,xmaxwidth), ylim = ylims, ylab = "" , xlab="",log = mylog, yaxt = "n" , xaxt ="n") 
      text(cols,p99_99,round(p99_99,0),adj=c(1,0),col="gray70")
      text(tail(cols,n=1),tail(p99_99, n=1),"99.99%",adj=c(0,0),col="gray20",cex=.7)
    }

  #
  # max latency 
  #
    if ( i_plot_max == 1 ) {
      cat("cols\n")
      print(cols)
      cat("max\n")
      print(maxlat)
      par(new = TRUE)
      plot(cols, maxlat, type = "l", xaxs = "i", lty = 3, col = "red", lwd = 1, bty = "l", 
       xlim = c(xminwidth,xmaxwidth), ylim = ylims, ylab = "" , log = mylog, xlab="",yaxt = "n" , xaxt ="n") 
      text(cols,maxlat,round(maxlat,1),adj=c(1,-1))
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
   segments(0,   i, xmaxwidth,  i,  col="orange",   lwd=1,lty=2)
  }
  #

  # reference dashed lines for all thie histogram buckets
  #
     j=1
     #for ( i in  c(.05,.100,.250,.500,1,2,4,10,20,50,100,200,500,1000,2000,5000)  )  {
     #    #cat("colors[",j,"] =",colors[j],"\n")
     #    segments(0,   i, xmaxwidth,  i,    lwd=2,lty=2, col= colors[j])
     #    j = j + 1
     #}

}
