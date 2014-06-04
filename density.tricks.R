dropboxdir <- c("C:/Users/Jeff/Documents/Dropbox/","C:/Users/Jeff/Documents/My Dropbox/","C:/Users/Jeff Rutter/Dropbox/"); dropboxdir <- dropboxdir[min(which(file.info(dropboxdir)$isdir))]

interpolate.y <- function( dens, x ) {
  y <- NA * x
  idx <- NA * x
  len.x <- length(x)
  for( x.idx in 1:len.x ){
    d.x2.idx<-idx[x.idx]<-min( c(length(dens$x),which( dens$x>=x[x.idx] )) )
    d.x1.idx<-max(1,d.x2.idx-1)
    d.int.len <- dens$x[d.x2.idx] - dens$x[d.x1.idx]
    if ( d.int.len == 0 ) 
      y[x.idx] <- dens$y[d.x1.idx]
    else {
      frac1 <- (dens$x[d.x2.idx]-x[x.idx])/d.int.len
      frac2 <- 1 - frac1
      y[x.idx] <- frac1*dens$y[d.x1.idx] + frac2*dens$y[d.x2.idx]
    }
  }
  return( data.frame( x=x, y=y, idx=idx ) )
}
polygon.points <- function( dens, x ) {
  x <- x[1:2]
  int.pts <- interpolate.y( dens, x )
  if( dens$x[int.pts$idx[2]] > x[2] )
    int.pts$idx[2] <- int.pts$idx[2] - 1
  return( data.frame( x=c(x[1],x[1],dens$x[int.pts$idx[1]:int.pts$idx[2]],x[2],x[2]), y=c(0,int.pts$y[1],dens$y[int.pts$idx[1]:int.pts$idx[2]],int.pts$y[2],0) ) )
}
interpolate.x <- function( dens, i, y ) {
  # find x between dens$x[i-1 & i] equal to y
  x <- NA * y
  idx <- NA * y
  len.y <- length(y)
  for( y.idx in 1:len.y ){
    #     d.y2.idx<-idx[y.idx]<-min( c(length(dens$y),which( dens$y>=y[y.idx] )) )
    #     d.y1.idx<-max(1,d.y2.idx-1)
    d.y2.idx<-i[y.idx]
    d.y1.idx<-max(1,d.y2.idx-1)
    d.int.len <- dens$y[d.y2.idx] - dens$y[d.y1.idx]
    if ( d.int.len == 0 ) 
      x[y.idx] <- dens$x[d.y1.idx]
    else {
      frac1 <- (dens$y[d.y2.idx]-y[y.idx])/d.int.len
      frac2 <- 1 - frac1
      x[y.idx] <- frac1*dens$x[d.y1.idx] + frac2*dens$x[d.y2.idx]
    }
  }
  return( data.frame( x=x, y=y, idx=idx ) )
}

dens.quantiles<-function( dens, q ){
  len <- length(dens$x)
  cum.dens<-NULL; cum.dens$y <- cumsum(dens$y); cum.dens$x <- dens$x
  j <- x <- q * NA
  for( i in 1:length(q) ){
    j[i] <- min(which(cum.dens$y>q[i]))
  }
  x<-(interpolate.x(cum.dens,j,q))$x
  return(x)
}  


plot.quantiles <- function(x,y.arr,base.col="gray10"){
  n.q <- dim(y.arr)[1]
  n.obs <- dim(y.arr)[2]
  n.pairs<-ceiling(n.q/2)
  alpha.delta <- 1/(1+n.pairs)
  x.f <- c(x,rev(x))
  for( idx1 in 1:n.pairs ){
    idx2 <- n.q - idx1+1
    polygon(x=x.f,y=c(y.arr[idx1,],rev(y.arr[idx2,])), col=alpha(base.col,alpha.delta),border=NA)
  }
}


shaded.contours <- function(x,y,z,...) {
  #   image(x=m1.set,y=m2.set,z=log10(1+over.est),log="y")
  #   contour(x=m1.set,y=m2.set,z=log10(1+over.est),log="y",add=T)
  image(x=x,y=y,z=z,...,col = alpha(heat.colors(96),0.5))
  contour(x=x,y=y,z=z,levels=1:9*10,...,add=T)  # ,nlevels=5
  box()
}


horiz.hist <- function(Data=NULL, breaks="Sturges", col="transparent", las=1, 
                       ylim=range(HBreaks), xlim=c(0,max(a$density)), labelat=pretty(ylim), labels=labelat, x.rev=F, hist.data=NULL, add=F, ... ){
  if( !is.null(Data) ){
    a <- hist(Data, plot=FALSE, breaks=breaks)
  }
  else
    a <- hist.data

  if(x.rev) xlim <- rev(xlim)
  
  if( !add ){
    plot(1,typ="n",ylim=ylim,xlim=xlim,xlab="",ylab="",main="",axes=F); box(); 
  }
  rect(xleft=0,xright=a$density,ybottom=a$breaks[-length(a$breaks)],ytop=a$breaks[-1], col=col,...)
}

coord.squares = function(x,y,...){
  plot(x=0,xlim=range(x)+c(-0.5,+0.5),ylim=range(y)+c(-0.5,+0.5))
  rect(xleft=x-0.5,xright=x+0.5,ybottom=y-0.5,ytop=y+0.5,...)
}
matrix.squares = function(mat,...){
  plot(x=0,xlim=c(1,nrow(mat))+c(-0.5,+0.5),ylim=c(1,ncol(mat))+c(-0.5,+0.5))
  for(i in 1:nrow(mat)){
    for(j in 1:ncol(mat)){
      if( mat[i,j] > 0 )
        for( k in 1:mat[i,j] ){
          rect(xleft=i-0.5,xright=i+0.5,ybottom=j-0.5,ytop=j+0.5,...)
        }
    }
  }
}
# matrix.squares(pop.grid,col=alpha(1,0.2),border=NA)


add.translucency<-function(c,t){
  c.rgb <- col2rgb(c)
  c.rgbo <- rgb( c.rgb[1], c.rgb[2], c.rgb[3], alpha=ceiling(255*(1-t)), maxColorValue=255 )
  return( c.rgbo )
}
ghost<-function(c,t){
  add.translucency(c,t)
}

alpha <- function (colour, alpha) 
{
    alpha[is.na(alpha)] <- 0
    col <- col2rgb(colour, TRUE)/255
    if (length(colour) != length(alpha)) {
        if (length(colour) > 1 && length(alpha) > 1) {
            stop("Only one of colour and alpha can be vectorised")
        }
        if (length(colour) > 1) {
            alpha <- rep(alpha, length.out = length(colour))
        }
        else if (length(alpha) > 1) {
            col <- col[, rep(1, length(alpha)), drop = FALSE]
        }
    }
    col[4, ] <- ifelse(col[4, ] == 1, alpha, col[4, ])
    new_col <- rgb(col[1, ], col[2, ], col[3, ], col[4, ])
    new_col[is.na(colour)] <- NA
    new_col
}

darken <- function(color,k=1){
  col <- col2rgb(color, T) / 255
  dark.col <- col * 0.7 ^ k
  rgb( dark.col[1, ], dark.col[2, ], dark.col[3, ], col[4,] )
}
# plot(type="l", 0:2,0:2, col=darken("green",0))
# rect(0,0,2,2,col=darken("green",0),border=NA)
# rect(0,0,1,1,col=darken("green",1),border=NA)
# rect(1,0,2,1,col=darken("green",2),border=NA)
# rect(1,1,2,2,col=darken("green",3),border=NA)
lighten <- function(color,k=1){
  col <- col2rgb(color, T) / 255
  light.col <- 1 - ( 1 - col ) * 0.7 ^ k
  print(light.col)
  rgb( light.col[1, ], light.col[2, ], light.col[3, ], col[4,] )
}
# plot(type="l", 0:2,0:2, col=darken("green",0))
# rect(0,0,2,2,col=lighten("green",0),border=NA)
# rect(0,0,1,1,col=lighten("green",1),border=NA)
# rect(1,0,2,1,col=lighten("green",2),border=NA)
# rect(1,1,2,2,col=lighten("green",3),border=NA)


# text in figure tricks
# par("usr")  # returns limits of current figure
figure.height <- function(){ par("usr")[4] - par("usr")[3] }
figure.width <- function(){ par("usr")[2] - par("usr")[1] }


## stat tricks
zscores <- function( x ) {
  x.mean <- mean(x,na.rm=T)
  x.sd <- sd(x,na.rm=T)
  Z <- (x-x.mean)/x.sd
#   print(x.mean)
#   print(x.sd)
  return(Z)
}


rdirichlet = function(n, alpha) {
  k = length(alpha)
  r = matrix(0, nrow=n, ncol=k) 
  for (i in 1:k) {
    r[,i] = rgamma(n, alpha[i], 1)
  }
  r = matrix(mapply(function(r, s) {return (r/s)}, r, rowSums(r)), ncol=k)
  return (r)
}


## file tricks
unique.filename <- function( fn, suf ) {
  idx <- 0
  try.fn <- sprintf("%s.%s",fn,suf)
  while( file.exists(try.fn) ) {
    idx <- idx+1
    try.fn <- sprintf("%s-%02d.%s",fn,idx,suf)
  }
  return( try.fn )
}

dated.filename <- function( fn, suf ) {
  ##' @param fn the base file name to which date-time will be added
  ##' @param suf suffix for file added after date-time (period not needed)
  sprintf( "%s %s.%s", fn, format(Sys.time(),"%Y%m%d-%H%M%S"), suf )  
}



## string tricks
rtstr <- function(strv,n){
  ix <- stringr::str_length(strv)
  stringr::str_sub(strv,start=ix-n+1,ix)
}
regextr <- function(text,pattern) {
  # returns the part of the text that matches the pattern.  works with text vectors
  # e.g. regextr(some.text,"^[^ ]*")  returns the first part of some.text up to the occurence of the first space
  regmatches(text,regexpr(pattern,text))
}



## date tricks
DOY <- function( date.in ){
  1 + as.numeric( date.in - as.Date(sprintf("01-01-%s",strftime(date.in,"%Y")), "%m-%d-%Y"), "days" )
}
days.since.year <- function( date.in, year=strftime(date.in,"%Y") ){  # including end-points
  1 + as.numeric( date.in - as.Date(sprintf("01-01-%s",year), "%d-%m-%Y"), "days" )
}
date.after.year <- function( year, days ){  # including end-points
  as.Date(sprintf("01-01-%s",year), "%m-%d-%Y") + days
}
days.between <- function( earlier.date=date1, later.date=date2, date1=NULL,date2=NULL ){  
  as.numeric( later.date - earlier.date, "days" )
}
year <- function( date.in ){
  as.numeric( strftime(date.in,"%Y"))
}
month <- function( date.in ){
  as.numeric( strftime(date.in,"%m"))
}
day <- function( date.in ){
  as.numeric( strftime(date.in,"%d"))
}
make.Date <- function(yyyy,mm,dd){
  as.Date(sprintf("%s-%s-%s",mm,dd,yyyy), "%m-%d-%Y")
}
days.in.month = function( year = as.numeric(format(Sys.Date(),'%Y')), month = NULL ){
  month = as.integer(month)
  dt = as.Date(paste(year, month, '01', sep = '-'))
  dates = seq(dt, by = 'month', length = 2)
  as.numeric(difftime(dates[2], dates[1], units = 'days'))
}

## time tricks
start.timer <- function(){
  timer.time <- Sys.time()
  timer.tic <- function(){
    return(Sys.time()-timer.time)
  }
  return( timer.tic )
}
# mytimer <- start.timer()
# mytimer()


## ggplot tricks
install.ggthemes <- function(){
  library("devtools")
  install_github("ggthemes","jrnold")
  install.packages(repos=NULL,pkgs=file.path("C:/Users/Jeff Rutter/Dropbox/MyR","ggthemes.zip"))
  install.packages(repos=NULL,pkgs="C:/Users/Jeff/Documents/My Dropbox/MyR/ggthemes.zip")
  install.packages(repos=NULL,pkgs="C:/Users/Jeff/Documents/My Dropbox/MyR/ggthemes_1.0.1.tar.gz")
}


## ADMB tricks
read.admbFit<-function(model.name){
  ## Function to read AD Model Builder fit from its par and cor
  ## files. I borrowed this from http://qfc.fw.msu.edu/userfun.asp
  ## and made some modifications as I needed. Note that this one
  ## reads the model output files with the model.name prefix, NOT
  ## the admodel.hes or admodel.cov. Thus this is a good way to
  ## check that my correlation calculations are coming out the same.
  
  ret<-list()
  parmodel.name<-as.numeric(scan(paste(model.name,'.par', sep=''),
                                 what='', n=16, quiet=TRUE)[c(6,11,16)])
  ret$nopar<-as.integer(parmodel.name[1])
  ret$nloglike<-parmodel.name[2]      #objective function value
  ret$maxgrad<-parmodel.name[3]
  model.name<-paste(model.name,'.cor', sep='') # read cor model.name
  lin<-readLines(model.name) # total parameter including sdreport variables
  ret$totPar<-length(lin)-2  #log of the determinant of the hessian
  ret$logDetHess<-as.numeric(strsplit(lin[1], '=')[[1]][2])
  sublin<-lapply(strsplit(lin[1:ret$totPar+2], ' '),function(x)x[x!=''])
  ret$names<-unlist(lapply(sublin,function(x)x[2]))
  ret$est<-as.numeric(unlist(lapply(sublin,function(x)x[3])))
  ret$std<-as.numeric(unlist(lapply(sublin,function(x)x[4])))
  ret$cor<-matrix(NA, ret$totPar, ret$totPar)
  corvec<-unlist(sapply(1:length(sublin), function(i)sublin[[i]][5:(4+i)]))
  ret$cor[upper.tri(ret$cor, diag=TRUE)]<-as.numeric(corvec)
  ret$cor[lower.tri(ret$cor)] <- t(ret$cor)[lower.tri(ret$cor)] # covariance matrix
  ret$cov<-ret$cor*(ret$std %o% ret$std)
  return(ret)
}


# function tricks
get.inverse.function <- function( fun, ..., lower=0, upper=1500 ){
  inverse = function(y){ uniroot((function(x){ fun(x,...) - y }), lower=lower, upper=upper)$root }
  vector.inverse <- function(v){vapply(X=v,FUN=inverse,FUN.VALUE=1)}
  return( vector.inverse )
}

construct.log.density.fn <- function( dfn, ... ){
  #   print( as.list(sys.call()) )
  #   print( match.call() )
  descr <-  paste( as.list(sys.call())[[2]], paste( sprintf( "%s=%s", names(list(...)), paste(list(...)) ), collapse="." ), sep="." )
  log.fn <- function(x){dfn(x=x,...,log=T)}
  
  return( list(descr=descr,log.fn=log.fn))
}
construct.cumulative.density.fn <- function( pfn, ... ){
  mc <- as.list(match.call())
  descr <-  paste( mc$pfn,"(", paste( sprintf( "%s=%s", names(list(...)), paste(list(...)) ), collapse="," ),")", sep="" )
  cum.fn <- function(x){pfn(q=x,...,lower.tail=T)}
  
  return( list(descr=descr,cum.fn=cum.fn))
}
# estimate.mature.frac <- construct.cumulative.density.fn(pnorm, mean=120,sd=10)
# estimate.mature.frac$cum.fn( 110:135)

# list tricks
list.insert <- function( l, n, o ){
  l.temp <- l[n:length(l)]
  l[n:length(l)] <- NULL
  l[[n]] <- o
  #   l[n:length(l)+1] <- l.temp
  l <- c( l, l.temp )
  l
}
# list.insert( list("happy","days", "here","again"), 3, "are" )
  