#- FLMeta

#-------------------------------------------------------------------------------
# setDimnames
#-------------------------------------------------------------------------------
setGeneric("setDimnames",function(object,dmns,...)
  standardGeneric("setDimnames"))
  
# setDimnames for FLStock
setMethod("setDimnames",signature(object="FLStock",dmns="list"),
  function(object,dmns,...){
    slotnames <- names(getSlots("FLStock")[getSlots("FLStock")=="FLQuant"])
    dmnsQ     <- dmns; dmnsQ$age <- "all"
    for(slotname in slotnames)
      if(dim(slot(object,slotname))[1] < length(dmns[[1]])){
        dimnames(slot(object,slotname)) <- dmnsQ
      } else {
        dimnames(slot(object,slotname)) <- dmns
      }
  return(object)}
)

# setDimnames for FLBiol
setMethod("setDimnames",signature(object="FLBiol",dmns="list"),
  function(object,dmns,...){
    slotnames <- names(getSlots("FLBiol")[getSlots("FLBiol")=="FLQuant"])
    dmnsQ     <- dmns; dmnsQ$age <- "all"
    for(slotname in slotnames)
      if(dim(slot(object,slotname))[1] < length(dmns[[1]])){
        dimnames(slot(object,slotname)) <- dmnsQ
      } else {
        dimnames(slot(object,slotname)) <- dmns
      }
  return(object)}
)

# setDimnames for FLCatch
setMethod("setDimnames",signature(object="FLCatch",dmns="list"),
  function(object,dmns,...){
    slotnames <- names(getSlots("FLCatch")[getSlots("FLCatch")=="FLQuant"])
    dmnsQ     <- dmns; dmnsQ$age <- "all"
    for(slotname in slotnames)
      if(dim(slot(object,slotname))[1] < length(dmns[[1]])){
        dimnames(slot(object,slotname)) <- dmnsQ
      } else {
        dimnames(slot(object,slotname)) <- dmns
      }
  return(object)}
)

#-------------------------------------------------------------------------------
# as.FLCatch
#-------------------------------------------------------------------------------
setGeneric("as.FLCatch", function(object, ...)
  standardGeneric("as.FLCatch"))

# as.FLCatch from FLCatch
setMethod("as.FLCatch", signature(object="FLCatch"),

  function(object, unit  =1:dim(object@landings.n)[3],
                   season=1:dim(object@landings.n)[4],
                   area  =1:dim(object@landings.n)[5]) {

    slotnames <- names(getSlots("FLCatch")[getSlots("FLCatch")=="FLQuant"])
    for(slotname in slotnames){
      s.d <- dim(slot(object, slotname))
      slot(object, slotname) <- slot(object, slotname)[,,pmin(unit,s.d[3]),
                                                         pmin(season,s.d[4]),
                                                         pmin(area,s.d[5])]
    }
    return(object)
  }
)

# as.FLCatch from FLStock
setMethod("as.FLCatch", signature(object="FLStock"), function(object,...){
	flc <- new("FLCatch")
	flc@name <- object@name
	flc@desc <- object@desc
	flc@range <- object@range

  flc@landings <- object@landings
	flc@landings.n <- object@landings.n
	flc@landings.wt <- object@landings.wt
	flc@landings.sel <- object@landings.n / object@catch.n
	flc@discards <- object@discards
	flc@discards.n <- object@discards.n
	flc@discards.sel <- object@discards.n / object@catch.n
	flc@discards.wt <- object@discards.wt
	flc@catch.q <- FLQuant(NA,dimnames=dimnames(object@landings))
	flc@price <- FLQuant(NA,dimnames=dimnames(object@landings.n))
    return(flc)
  }
)

#-------------------------------------------------------------------------------
# as.FLStock from FLBiol and FLCatch
#-------------------------------------------------------------------------------

# as.FLStock
setGeneric("as.FLStock", function(object,objectB='missing',...)
		standardGeneric("as.FLStock"))

setGeneric("as.FLStock", function(object,objectB,...)
  standardGeneric("as.FLStock"))

# as.FLStock from biol + fleet
# A stock assessment can have multiple areas, but not multiple units or seasons
setMethod("as.FLStock",signature(object="FLCatch",objectB="FLBiol"),
  function(object,objectB,units='missing',harvest.spwn=TRUE,...){

    if(identical(dims(objectB),dims(object))==F) stop("Dimensions of FLBiol not equal to FLCatch")

    fls <- new("FLStock")
    
    #- Fleet part
    fls@landings <- object@landings
    fls@landings.n <- object@landings.n
    fls@landings.wt <- object@landings.wt
    fls@discards  <- object@discards
    fls@discards.n <- object@discards.n
    fls@discards.wt <- object@discards.wt
    fls@catch.n <- fls@landings.n + fls@discards.n
    fls@catch.wt <- (fls@landings.n * fls@landings.wt + fls@discards.n * fls@discards.wt) /
                    (fls@landings.n + fls@discards.n)
    fls@catch.wt[is.na(fls@catch.wt)] <- 0
    fls@catch <- quantSums(fls@catch.n * fls@catch.wt)
    fls@harvest <- FLQuant(NA,dimnames=dimnames(object@landings.n))
    
    #- Biol part
    fls@stock.n  <- objectB@n
    fls@stock.wt  <- objectB@wt
    fls@m <- objectB@m
    fls@mat <- objectB@fec
    fls@m.spwn <- objectB@spwn
    if(harvest.spwn==T){
      fls@harvest.spwn <- fls@m.spwn
    } else {
      fls@harvest.spwn <- FLQuant(NA,dimnames=dimnames(objectB@spwn))
    }

    fls@stock <- quantSums(fls@stock.n * fls@stock.wt)

    #- Common part
    fls@name  <- object@name
    fls@desc  <- object@desc
    match(names(range(fls)),names(range(object)))
    fls@range[match(names(range(object)),names(range(fls)))] <- object@range

    if(missing(units))
      units(fls)[1:17] <- as.list(c(rep(c("tonnes","thousands","kg"),4),
                                rep("NA",2),"f",rep("NA",2)))

      return(fls)
  }
)

#-------------------------------------------------------------------------------
# FLAvail
#-------------------------------------------------------------------------------
validFLAvail  <-  function(object){
	# Make sure there are at least 6 dimensions in the array
	Dim  <-  dim(object)
	if (length(Dim) != 6)
		return("the array must have 6 dimensions")

	if (!is.numeric(object) && !is.na(object))
		return("array is not numeric")

	# check "units" slot
	if(!is.character(object@units))
		return("units must be a string")
	# Everything is fine
	return(TRUE)
}


setClass("FLAvail",
	representation("FLArray"),
	prototype(array(as.numeric(NA), dim=c(1,1,1,1,1,1),
		dimnames=list(quant="all", year="1", unit="unique", season="all",
		area="unique", iter="1")), units="NA"),
  validity=validFLAvail
) # }}}


setGeneric("FLAvail",function(objectF,objectB,...)
  standardGeneric("FLAvail"))
  
setMethod("FLAvail",signature(objectF="FLCatch",objectB="FLBiol"),
  function(objectF,objectB,...){

  # Get dimensions of biol and catch
  dmsB <- dims(objectB)
  dmsF <- dims(objectF)
  
  # Check dimensions on critical elements
  dmnSim <- c("quant","age","min","max","plusgroup")
  for(i in dmnSim)
    if((dmsB[[i]] != dmsF[[i]]) | (is.na(dmsB[[i]])==T | is.na(dmsF[[i]])))
      stop(paste("Dimension of ",i," in FLBiol not equal to ",i," in FLCatch",sep=""))

  dimnames  <- list(age=    as.character(sort(as.numeric(as.character(unique(c(dimnames(objectB@n)[[1]],dimnames(objectF@landings.n)[[1]])))))),
                    year=   as.character(sort(as.numeric(as.character(unique(c(dimnames(objectB@n)[[2]],dimnames(objectF@landings.n)[[2]])))))),
                    unit=   sort(unique(c(dimnames(objectB@n)[[3]],dimnames(objectF@landings.n)[[3]]))),
                    season= sort(unique(c(dimnames(objectB@n)[[4]],dimnames(objectF@landings.n)[[4]]))),
                    area=   sort(unique(c(dimnames(objectB@n)[[5]],dimnames(objectF@landings.n)[[5]]))),
                    iter=   as.character(sort(as.numeric(as.character(unique(c(dimnames(objectB@n)[[6]],dimnames(objectF@landings.n)[[6]])))))))

  for(i in 1:length(dimnames)){
    if(("all" %in% dimnames[[i]] | "unique" %in% dimnames[[i]]) & length(dimnames[[i]])>1)
      dimnames[[i]] <- dimnames[[i]][which(!dimnames[[i]] %in% c("all","unique"))]
  }

  dim       <- unlist(lapply(dimnames,length))
  fla   <- new("FLAvail", array(as.numeric(NA), dim=dim, dimnames=dimnames), units="NA")
  return(fla)}
)


setMethod('sweep', signature(x='FLAvail'),
  function(x, MARGIN, STATS, FUN, check.margin=TRUE, ...)
  {
    x   <- FLQuant(x,units=units(x))
    res <- callNextMethod()
    return(FLQuant(res, units=units(x)))
  }
) # }}}


## as.FLAvail      {{{
#setGeneric("as.FLAvail", function(x, ...)
#standardGeneric("as.FLAvail"))# }}}

# as.FLAvail(array){{{
#setMethod("as.FLAvail", signature(x="FLQuant"),
#function(x, ...) {
#return(FLAvail(x, ...))
#}
#)# }}}

#-------------------------------------------------------------------------------
# compress
#-------------------------------------------------------------------------------

setGeneric("compress",function(x,...)
  standardGeneric("compress"))
  
setMethod("compress", signature(x="FLBiol"),
  function(x,...) {

    args <- list(...)
    nargs <- names(args)

    # dimension names
    qnames <- names(dimnames(x@n))

    # check input names match dimnames
    if(!all(nargs%in%qnames))
      stop(paste("Wrong dimension name provided: ", nargs[!nargs%in%qnames]))

    # Check which dimension it is
    idx    <- which(qnames %in% names(args))
    if(idx %in% c(1,2,4,6))
      stop("compress not defined to work on dimension '",qnames[idx[which(idx %in% c(1,2,4,6))]],"'")
    
    # Create list with given standard elements in right position ...
    select <- args[match(qnames, nargs)]

    # get rid of unspecified dimensions
    select <- select[!unlist(lapply(select, is.null))]

    # turn into characters
    select <- lapply(select, as.character)

    # match specified dimensions and dimnames
    dimnames <- dimnames(x@n)

    # check new dimnames contain old ones
    dimnames[names(select)] <- select

    # output object
    qntA <- new(class(x@n), array(as.numeric(NA), dimnames=dimnames, dim= unlist(lapply(dimnames, length))), units=units(x@n))
    #dimnames$age <- "all"
    #qntNA<- new(class(x), array(as.numeric(NA), dimnames=dimnames, dim= unlist(lapply(dimnames, length))), units=units(x))
    
    res         <- FLBiol(n=qntA)
    res@n[]     <- apply(x@n,(1:6)[-idx],sum,na.rm=T)
    res@m[]     <- apply(x@n * x@m,(1:6)[-idx],sum,na.rm=T)   / apply(x@n,(1:6)[-idx],sum,na.rm=T)
    res@wt[]    <- apply(x@n * x@wt,(1:6)[-idx],sum,na.rm=T)  / apply(x@n,(1:6)[-idx],sum,na.rm=T)
    res@wt[is.na(res@wt)] <- 0
    res@fec[]   <- apply(x@n * x@fec,(1:6)[-idx],sum,na.rm=T) / apply(x@n,(1:6)[-idx],sum,na.rm=T)
    res@spwn[]  <- apply(x@n * x@spwn,(1:6)[-idx],sum,na.rm=T)/ apply(x@n,(1:6)[-idx],sum,na.rm=T)
    units(res)  <- units(x)
    range(res)  <- range(x)

    return(res)
  }
) # }}}

setMethod("compress", signature(x="FLCatch"),
  function(x,...) {

    args <- list(...)
    nargs <- names(args)

    # dimension names
    qnames <- names(dimnames(x@landings.n))

    # check input names match dimnames
    if(!all(nargs%in%qnames))
      stop(paste("Wrong dimension name provided: ", nargs[!nargs%in%qnames]))

    # Check which dimension it is
    idx    <- which(qnames %in% names(args))
    if(idx %in% c(1,2,4,6))
      stop("compress not defined to work on dimension '",qnames[idx[which(idx %in% c(1,2,4,6))]],"'")

    # Create list with given standard elements in right position ...
    select <- args[match(qnames, nargs)]

    # get rid of unspecified dimensions
    select <- select[!unlist(lapply(select, is.null))]

    # turn into characters
    select <- lapply(select, as.character)

    # match specified dimensions and dimnames
    dimnames <- dimnames(x@landings.n)

    # check new dimnames contain old ones
    dimnames[names(select)] <- select

    # output object
    qntA <- new(class(x@landings.n), array(as.numeric(NA), dimnames=dimnames, dim= unlist(lapply(dimnames, length))), units=units(x@landings.n))
    #dimnames$age <- "all"
    #qntNA<- new(class(x), array(as.numeric(NA), dimnames=dimnames, dim= unlist(lapply(dimnames, length))), units=units(x))

    res               <- FLCatch(landings.n=qntA)
    res@landings[]    <- apply(x@landings,(1:6)[-idx],sum,na.rm=T)
    res@landings.n[]  <- apply(x@landings.n,(1:6)[-idx],sum,na.rm=T)
    res@landings.wt[] <- apply(x@landings.n * x@landings.wt,(1:6)[-idx],sum,na.rm=T) / apply(x@landings.n,(1:6)[-idx],sum,na.rm=T)
    res@landings.wt[is.na(res@landings.wt)] <- 0
    res@landings.sel[]<- apply(x@landings.n * x@landings.sel,(1:6)[-idx],sum,na.rm=T) / apply(x@landings.n,(1:6)[-idx],sum,na.rm=T)
    res@discards[]    <- apply(x@discards,(1:6)[-idx],sum,na.rm=T)
    res@discards.n[]  <- apply(x@discards.n,(1:6)[-idx],sum,na.rm=T)
    res@discards.wt[] <- apply(x@discards.n * x@discards.wt,(1:6)[-idx],sum,na.rm=T) / apply(x@discards.n,(1:6)[-idx],sum,na.rm=T)
    res@discards.wt[is.na(res@discards.wt)] <- 0
    res@discards.sel[]<- apply(x@discards.n * x@landings.sel,(1:6)[-idx],sum,na.rm=T) / apply(x@discards.n,(1:6)[-idx],sum,na.rm=T)
    res@catch.q[]     <- quantSums(apply(sweep(catch.n(x),2:6,x@catch.q,"*"),(1:6)[-idx],sum,na.rm=T)       / apply(catch.n(x),(1:6)[-idx],sum,na.rm=T))
    res@price[]       <- apply(catch.n(x) * x@price,(1:6)[-idx],sum,na.rm=T)         / apply(catch.n(x),(1:6)[-idx],sum,na.rm=T)
    units(res)        <- units(x)
    range(res)        <- range(x)
    return(res)
  }
) # }}}


#-------------------------------------------------------------------------------
# areaSums
#-------------------------------------------------------------------------------

setMethod("areaSums",signature(x="FLQuant"),
  function(x,...){
  return(apply(x,c(1:4,6),sum,na.rm=T))})
  
#-------------------------------------------------------------------------------
# as.numeric
#-------------------------------------------------------------------------------

setGeneric("an",function(x,...)
  standardGeneric("an"))

setMethod("an",signature(x="character"),
  function(x,...){
  return(as.numeric(x))})
  
#-------------------------------------------------------------------------------
# catch.n of multiple fisheries on populations
#-------------------------------------------------------------------------------
setGeneric("catch.n",function(w,x,y,z,...)
  standardGeneric("catch.n"))
  
setMethod("catch.n",signature(w="FLStock",x="FLCatch",y="FLBiol",z="FLAvail"),
  function(w,x,y,z,type,...){
     catch.n(x,y,z,type="catch.n")
})

setMethod("catch.n", signature(w="FLStock",x='missing',y='missing',z='missing'),
  function(w,x,y,z,...){ return(w@catch.n)})
  

setMethod("catch.n", signature(w="FLBiol",x='missing',y='missing',z='missing'),
  function(w,x,y,z,...)
  {
    hrvst<-harvest(w)
    z <- hrvst+m(w)[,-dims(n(w))$year]
    res <- n(w)[,-dims(n(w))$year]*hrvst/z*(1-exp(-z))
    return(res)
   }
) # }}}

setMethod('catch.n', signature(w='FLCatch',x='missing',y='missing',z='missing'),
  function(w,x,y,z,...)
  {
    res <- landings.n(w) + discards.n(w)
    if (units(discards.n(w)) == units(landings.n(w)))
		  units(res) <- units(discards.n(w))
    else
      warning("units of discards.n and landings.n do not match")
    return(res)
  }
)

setMethod("catch.n",signature(w="FLCatch",x="FLBiol",y="FLAvail",z='missing'),
  function(w,x,y,z,type="catch.n",...){

    #- Define dimensions not in common between avail and biol
    idx <- which(dim(y) != dim(x@n))
    stopifnot(length(idx)>0 & idx==5)

    if(length(idx)>0){
      nBy <- sweep(y,(1:6)[-idx],x@n,"*")
    } else {
      nBy <- y * x@n
    }

    if(round((sum(nBy,na.rm=T) - sum(x@n,na.rm=T)))!=0)
      stop("FLAvail sums up to more than one over areas")

    #- Define dimensions not in common between avail and fishery
    idx <- which(dim(y) != dim(catch.n(w)))

    #- Calculate catch on an area basis
    #   first if fishery spans all units
    if(length(idx)>0){
      if(dimnames(catch.n(w))[[3]] == "all" | dimnames(catch.n(w))[[3]] == "unique"){
        for(iUnit in dimnames(x@n)[[3]]){
          if(iUnit == dimnames(x@n)[[3]][1]){
            landn  <- landings.sel(w) / sweep(landings.sel(w),c(1:4,6),m(x[,,iUnit]),"+") *
                           (1-exp(sweep(-landings.sel(w),c(1:4,6),m(x[,,iUnit]),"-"))) * nBy[,,iUnit]
            discn  <- discards.sel(w) / sweep(discards.sel(w),c(1:4,6),m(x[,,iUnit]),"+") *
                           (1-exp(sweep(-discards.sel(w),c(1:4,6),m(x[,,iUnit]),"-"))) * nBy[,,iUnit]
          } else {
            landn  <- landings.sel(w) / sweep(landings.sel(w),c(1:4,6),m(x[,,iUnit]),"+") *
                           (1-exp(sweep(-landings.sel(w),c(1:4,6),m(x[,,iUnit]),"-"))) * nBy[,,iUnit] + landn
            discn  <- discards.sel(w) / sweep(discards.sel(w),c(1:4,6),m(x[,,iUnit]),"+") *
                           (1-exp(sweep(-discards.sel(w),c(1:4,6),m(x[,,iUnit]),"-"))) * nBy[,,iUnit] + discn
            }
        }
      #   second, fishery spans certain units
      } else { #- Units not 'all'
          init <- 0
          for(iUnit in dimnames(x@n)[[3]]){
            if(iUnit %in% dimnames(catch.n(w))[[3]]){
              if(init == 0){
                landn  <- landings.sel(w)[,,iUnit] / sweep(landings.sel(w)[,,iUnit],c(1:4,6),m(x[,,iUnit]),"+") *
                           (1-exp(sweep(-landings.sel(w)[,,iUnit],c(1:4,6),m(x[,,iUnit]),"-"))) * nBy[,,iUnit]
                discn  <- discards.sel(w)[,,iUnit] / sweep(discards.sel(w)[,,iUnit],c(1:4,6),m(x[,,iUnit]),"+") *
                           (1-exp(sweep(-discards.sel(w)[,,iUnit],c(1:4,6),m(x[,,iUnit]),"-"))) * nBy[,,iUnit]
                init  <- 1
              } else {
                landn  <- landings.sel(w)[,,iUnit] / sweep(landings.sel(w)[,,iUnit],c(1:4,6),m(x[,,iUnit]),"+") *
                           (1-exp(sweep(-landings.sel(w)[,,iUnit],c(1:4,6),m(x[,,iUnit]),"-"))) * nBy[,,iUnit] + landn
                discn  <- discards.sel(w)[,,iUnit] / sweep(discards.sel(w)[,,iUnit],c(1:4,6),m(x[,,iUnit]),"+") *
                           (1-exp(sweep(-discards.sel(w)[,,iUnit],c(1:4,6),m(x[,,iUnit]),"-"))) * nBy[,,iUnit] + discn
                }
            }
          }
        }
        w@landings.n[] <- landn
        w@discards.n[] <- discn
      } else {
          if(!all(dimnames(catch.n(w))[[3]] %in% dimnames(x@n)[[3]])) stop("Units in fishery not the same as in biol")
          #- Second if fishery and biol have same units, then these equations
          w@landings.n[] <- landings.sel(w) / sweep(landings.sel(w),c(1:4,6),m(x),"+") *
                                       (1-exp(sweep(-landings.sel(w),c(1:4,6),m(x),"-"))) * nBy
          w@discards.n[] <- discards.sel(w) / sweep(discards.sel(w),c(1:4,6),m(x),"+") *
                                       (1-exp(sweep(-discards.sel(w),c(1:4,6),m(x),"-"))) * nBy
        }

      if(type == "landings.n") res <- w@landings.n
      if(type == "discards.n") res <- w@discards.n
      if(type == "catch.n")    res <- catch.n(w)
    return(res)}
)

#-------------------------------------------------------------------------------
# compute harvest of a biological or stock object given fisheries and biol data
#-------------------------------------------------------------------------------
setGeneric("computeHarvest",function(w,x,y,z,...)
  standardGeneric("computeHarvest"))

setMethod("computeHarvest",signature(w="FLStock",x="FLCatch",y="FLBiol",z="FLAvail"),
  function(w,x,y,z,...){
     computeHarvest(x,y,z)
})

setMethod("computeHarvest", signature(w="FLStock",x='missing',y='missing',z='missing'),
  function(w,x,y,z,...){

  require(minpack.lm)
  dims <- dims(w)
  hars <- w@harvest
  harb <- harvest(as.FLBiol(w))

  calcHarvest <- function(fs,ns,ms,cs){
    return(sqrt((cs - (fs/(fs+ms) * ns * (1-exp(-fs-ms))))^2))}

  for(iYr in ac(dims$minyear:dims$maxyear)){
    for(iTer in 1:dims$iter){
      for(iArea in 1:dims$area){
        for(iUnit in 1:dims$unit){

          if(iYr == ac(dims$maxyear)){
            strt<- c(harb[,ac(an(iYr)-1),iUnit,,iArea,iTer])
          } else {
            strt<- c(harb[,iYr,iUnit,,iArea,iTer])
          }
          if(length(which(is.na(strt)))>0)
            strt[which(is.na(strt))]     <- 0.25 #random value
          if(length(!is.finite(strt))>0)
            strt[which(!is.finite(strt))] <- 0.25
          
          res <- nls.lm(par=strt,fn=calcHarvest,
                       ns=stock.n(w)[,iYr,iUnit,,iArea,iTer,drop=T],
                       ms=m(w)[,iYr,iUnit,,iArea,iTer,drop=T],
                       cs=catch.n(w)[,iYr,iUnit,,iArea,iTer,drop=T],
                       nls.lm.control(ftol = (.Machine$double.eps),maxiter = 100),jac=NULL)$par
          hars[,iYr,iUnit,,iArea,iTer] <- pmax(pmin(res,5),0)
        }
      }
    }
  }
  w@harvest <- hars

  return(w@harvest)})

#-------------------------------------------------------------------------------
# Harmonize catch, F and N's
#-------------------------------------------------------------------------------

setGeneric("harmonize",function(x,...)
  standardGeneric("harmonize"))

setMethod("harmonize",signature(x="FLStock"),
  function(x,...){

  #- define a new stock object that will contain the updated Ns, Fs and Cs
  updateStock <- x

  require(minpack.lm)
  dims    <- c(dim(x@stock.n)[1:5],1)
  dmns    <- dimnames(x@stock.n); dmns$iter <- "1"
  ages    <- an(dimnames(x@stock.n)$age)
  minage  <- min(an(dimnames(x@stock.n)$age))
  maxage  <- max(an(dimnames(x@stock.n)$age))
  pgage   <- range(x)["plusgroup"]
  ltage   <- pgage - 1
  optimF  <- function(x,n,m,ca){
                      if(is.null(dim(n))==F){
                        lik <- sqrt((ca-(rowMeans(n,na.rm=T)*x/(rowSums(x,na.rm=T)+rowMeans(m,na.rm=T))*(1-exp(-rowSums(x,na.rm=T)-rowMeans(m,na.rm=T)))))^2)
                        pen <- ifelse(x<0,sweep(lik,1,rowSums(lik,na.rm=T),"/")*1e6,0)
                      } else {
                        lik <- sqrt((ca-(mean(n,na.rm=T)*x/(sum(x,na.rm=T)+mean(m,na.rm=T))*(1-exp(-sum(x,na.rm=T)-mean(m,na.rm=T)))))^2)
                        pen <- ifelse(x<0,lik/sum(lik,na.rm=T)*1e6,0)
                      }
             return(lik+pen)}

  #- Get starting values for Fs
  strt <- window(harvest(as.FLBiol(x)),end=range(x)["maxyear"])
  strt[,ac(range(x)["maxyear"])] <- strt[,ac(range(x)["maxyear"]-1)]
  if(length(which(is.na(strt)))>0)
    strt@.Data[which(is.na(strt))]      <- 0.25 #random value
  if(length(!is.finite(strt))>0)
    strt@.Data[which(!is.finite(strt))] <- 0.25
  strt <- array(strt,dim=dim(strt),dimnames=dimnames(strt))

  #- Start loop over iters
  for(iTer in 1:dims(x)$iter){
    ns      <- array(iter(x@stock.n,iTer),dim=dims,dimnames=dmns)
    ms      <- array(iter(x@m,iTer),dim=dims,dimnames=dmns)
    cs      <- array(iter(x@catch.n,iTer),dim=dims,dimnames=dmns)

    #- Create temporary objects on F, N and C
    nFs     <- array(NA,dim=dim(ns),dimnames=dimnames(ns))
    nNs     <- array(NA,dim=dim(ns),dimnames=dimnames(ns))

    #- Fill the nNs with first year and recruitment
    nNs[1,,,,,] <- ns[1,,,,,] # Add recruitment
    nNs[,1,,,,] <- ns[,1,,,,] # Add first year

    #- Start loop over years
    for(iYr in an(dmns$year)){
      #- Start loop over ages
      for(iAge in ages){
        nFs[ac(iAge),ac(iYr),,,,] <- nls.lm(strt[ac(iAge),ac(iYr),,,,iTer],
                                            optimF,
                                            n=nNs[ac(iAge),ac(iYr),,,,],
                                            m= ms[ac(iAge),ac(iYr),,,,],
                                            ca=cs[ac(iAge),ac(iYr),,,,],
                                            nls.lm.control(ftol = (.Machine$double.eps),maxiter = 100),jac=NULL)$par
        nFs[ac(iAge),ac(iYr),,,,] <- pmax(pmin(nFs[ac(iAge),ac(iYr),,,,],5),0)
      }
      #- Force +group F to be the same to last true age F
      nFs[ac(pgage),ac(iYr),,,,]  <- nFs[ac(ltage),ac(iYr),,,,]

      #- Predict Ns one year ahead
      if(iYr < an(max(dmns$year))){
        if(is.null(dim(nNs[ac(iAge),ac(iYr),,,,]))==F){
          for(iAge in minage:ltage){
            nNs[ac(iAge+1),ac(iYr+1),,,,]     <- rowMeans(nNs[ac(iAge),ac(iYr),,,,],na.rm=T) *
                                                 exp(-rowSums(nFs[ac(iAge),ac(iYr),,,,],na.rm=T) - rowMeans(ms[ac(iAge),ac(iYr),,,,],na.rm=T))
            if(iAge == ltage)
              nNs[ac(iAge+1),ac(iYr+1),,,,]   <- rowMeans(nNs[ac(iAge+1),ac(iYr+1),,,,],na.rm=T) +
                                                 rowMeans(nNs[ac(iAge+1),ac(iYr),,,,],na.rm=T) *
                                                 exp(-rowSums(nFs[ac(iAge+1),ac(iYr),,,,],na.rm=T) - rowMeans(ms[ac(iAge+1),ac(iYr),,,,],na.rm=T))
          }
        } else {
            for(iAge in minage:ltage){
              nNs[ac(iAge+1),ac(iYr+1),,,,]   <- mean(nNs[ac(iAge),ac(iYr),,,,],na.rm=T) *
                                                 exp(-sum(nFs[ac(iAge),ac(iYr),,,,],na.rm=T) - mean(ms[ac(iAge),ac(iYr),,,,],na.rm=T))
              if(iAge == ltage)
                nNs[ac(iAge+1),ac(iYr+1),,,,] <- mean(nNs[ac(iAge+1),ac(iYr+1),,,,],na.rm=T) +
                                                 mean(nNs[ac(iAge+1),ac(iYr),,,,],na.rm=T) *
                                                 exp(-sum(nFs[ac(iAge+1),ac(iYr),,,,],na.rm=T) - mean(ms[ac(iAge+1),ac(iYr),,,,],na.rm=T))
            }
          }
      }
    }
    updateStock@stock.n[,,,,,iTer] <- nNs
    updateStock@harvest[,,,,,iTer] <- nFs
  }
  updateStock@catch.n <- updateStock@stock.n * sweep(sweep(updateStock@harvest,c(1:4,6),areaSums(updateStock@harvest) +
                                                           areaSums(updateStock@m * updateStock@stock.n)/areaSums(updateStock@stock.n),"/"),c(1:4,6),
                         (1-exp(-areaSums(updateStock@harvest) - areaSums(updateStock@m * updateStock@stock.n)/areaSums(updateStock@stock.n))),"*")
return(updateStock)
})

#-------------------------------------------------------------------------------
# Split fisheries object and biol object by unit (to allow stock assessment)
#-------------------------------------------------------------------------------


setGeneric("splitByUnit",function(x,y,z,...)
  standardGeneric("splitByUnit"))

setMethod("splitByUnit",signature(x="FLBiol",y="FLAvail",z="FLCatch"),
  function(x,y,z,unit="all",...){

  res       <- new("FLCatches")
  for(iUnit in dimnames(x@n)[[3]]){
    if(dims(z)$unit > 1){
      res[[iUnit]]               <- FLCatch(landings.n=FLQuant(NA,dimnames=dimnames(z@landings.n[,,iUnit])))
      units(res[[iUnit]])        <- units(z)
      range(res[[iUnit]])        <- range(z)
      name(res[[iUnit]])         <- paste(z@name,"unit", dimnames(x@n)$unit[iUnit])
      res[[iUnit]]@landings.wt[] <- z@landings.wt[,,iUnit]
      res[[iUnit]]@discards.wt[] <- z@discards.wt[,,iUnit]
      res[[iUnit]]@landings.sel[]<- z@landings.sel[,,iUnit]
      res[[iUnit]]@discards.sel[]<- z@discards.sel[,,iUnit]
      res[[iUnit]]@catch.q[]     <- z@catch.q[,,iUnit]
      res[[iUnit]]@price[]       <- z@price[,,iUnit]
    } else {
        res[[iUnit]]               <- FLCatch(landings.n=FLQuant(NA,dimnames=dimnames(z@landings.n)))
        units(res[[iUnit]])        <- units(z)
        range(res[[iUnit]])        <- range(z)
        name(res[[iUnit]])         <- paste(z@name,"unit", dimnames(x@n)$unit[iUnit])
        res[[iUnit]]@landings.wt[] <- z@landings.wt
        res[[iUnit]]@discards.wt[] <- z@discards.wt
        res[[iUnit]]@landings.sel[]<- z@landings.sel
        res[[iUnit]]@discards.sel[]<- z@discards.sel
        res[[iUnit]]@catch.q[]     <- z@catch.q
        res[[iUnit]]@price[]       <- z@price
      }
  }
  
  #- Define dimensions not in common between avail and biol
  idx <- which(dim(y) != dim(x@n))
  stopifnot(length(idx)>0 & idx==5)

  if(length(idx)>0){
    nBy <- sweep(y,(1:6)[-idx],x@n,"*")
  } else {
    nBy <- y * x@n
  }

  #- Define dimensions not in common between avail and fishery
  idx <- which(dim(y) != dim(catch.n(z)))
  
  #- Calculate catch on a unit basis
  #   first if fishery spans all units
  if(length(idx)>0){
    if(dimnames(catch.n(z))[[3]] == "all" | dimnames(catch.n(z))[[3]] == "unique"){
      for(iUnit in dimnames(x@n)[[3]]){
        res[[iUnit]]@landings.n       <- landings.sel(z) / sweep(landings.sel(z),c(1:4,6),m(x[,,iUnit]),"+") *
                                                      (1-exp(sweep(-landings.sel(z),c(1:4,6),m(x[,,iUnit]),"-"))) * nBy[,,iUnit]
        res[[iUnit]]@discards.n       <- discards.sel(z) / sweep(discards.sel(z),c(1:4,6),m(x[,,iUnit]),"+") *
                                                      (1-exp(sweep(-discards.sel(z),c(1:4,6),m(x[,,iUnit]),"-"))) * nBy[,,iUnit]
        res[[iUnit]]@landings[]       <- computeLandings(res[[iUnit]])
        res[[iUnit]]@discards[]       <- computeDiscards(res[[iUnit]])
      }
    } else {
        # second if a fishery only spans one unit, but not necessarily the first one
        for(iUnit in dimnames(x@n)[[3]]){
          if(iUnit %in% dimnames(catch.n(z))[[3]]){
            res[[iUnit]]@landings.n   <- landings.sel(z) / sweep(landings.sel(z),c(1:4,6),m(x[,,iUnit]),"+") *
                                                          (1-exp(sweep(-landings.sel(z),c(1:4,6),m(x[,,iUnit]),"-"))) * nBy[,,iUnit]
            res[[iUnit]]@discards.n   <- discards.sel(z) / sweep(discards.sel(z),c(1:4,6),m(x[,,iUnit]),"+") *
                                                          (1-exp(sweep(-discards.sel(z),c(1:4,6),m(x[,,iUnit]),"-"))) * nBy[,,iUnit]
            res[[iUnit]]@landings[]   <- computeLandings(res[[iUnit]])
            res[[iUnit]]@discards[]   <- computeDiscards(res[[iUnit]])

          }
        }
      }
  } else {
      #- If fishery and biol have same units, then these equations
      for(iUnit in dimnames(x@n)[[3]]){
        if(iUnit %in% dimnames(catch.n(z))[[3]]){
          res[[iUnit]]@landings.n[,,iUnit]     <- landings.sel(z)[,,iUnit] / sweep(landings.sel(z)[,,iUnit],c(1:4,6),m(x[,,iUnit]),"+") *
                                                           (1-exp(sweep(-landings.sel(z)[,,iUnit],c(1:4,6),m(x[,,iUnit]),"-"))) * nBy[,,iUnit]
          res[[iUnit]]@discards.n[,,iUnit]     <- discards.sel(z)[,,iUnit] / sweep(discards.sel(z)[,,iUnit],c(1:4,6),m(x[,,iUnit]),"+") *
                                                           (1-exp(sweep(-discards.sel(z)[,,iUnit],c(1:4,6),m(x[,,iUnit]),"-"))) * nBy[,,iUnit]
        }
      }
      res[[iUnit]]@landings[]       <- computeLandings(res[[iUnit]])
      res[[iUnit]]@discards[]       <- computeDiscards(res[[iUnit]])
    }


  if(unit=="all") ret <- res
  if(unit!="all") ret <- res[unit]
  return(ret)
})

setMethod("splitByUnit",signature(x="FLBiol",y="missing",z="missing"),
  function(x,y,z,unit="all",...){

  res       <- new("FLBiols")
  for(iUnit in 1:dims(x)$unit){
    res[[iUnit]]        <- x[,,iUnit]
    units(res[[iUnit]]) <- units(x)
    range(res[[iUnit]]) <- range(x)
    name(res[[iUnit]])  <- paste(x@name,"unit", dimnames(x@n)$unit[iUnit])
  }
  names(res)            <- dimnames(x@n)$unit

  if(unit=="all") ret <- res
  if(unit!="all") ret <- res[unit]
  return(ret)
})


#-------------------------------------------------------------------------------
# Split fisheries object and biol object by area (to allow stock assessment)
#-------------------------------------------------------------------------------


setGeneric("splitByArea",function(x,y,z,...)
  standardGeneric("splitByArea"))

setMethod("splitByArea",signature(x="FLBiol",y="FLAvail",z="FLCatch"),
  function(x,y,z,area="all",...){


  res       <- new("FLBiols")
  for(iArea in dimnames(z@landings.n)$area){
    if(dims(x)$area > 1){
      res[[iArea]]                <- FLBiol(n=FLQuant(NA,dimnames=dimnames(x@n[,,,,iArea])))
      units(res[[iArea]])         <- units(x)
      range(res[[iArea]])         <- range(x)
      name(res[[iArea]])          <- paste(x@name,"area", dimnames(z@landings.n)$area[iArea])
      res[[iArea]]@m[]            <- x@m[,,,,iArea]
      res[[iArea]]@wt[]           <- x@wt[,,,,iArea]
      res[[iArea]]@fec[]          <- x@fec[,,,,iArea]
      res[[iArea]]@swpn[]         <- x@spwn[,,,,iArea]
    } else {
        res[[iArea]]              <- FLBiol(n=FLQuant(NA,dimnames=dimnames(x@n)))
        units(res[[iArea]])       <- units(x)
        range(res[[iArea]])       <- range(x)
        name(res[[iArea]])        <- paste(x@name,"area", dimnames(z@landings.n)$area[iArea])
        res[[iArea]]@m[]          <- x@m
        res[[iArea]]@wt[]         <- x@wt
        res[[iArea]]@fec[]        <- x@fec
        res[[iArea]]@swpn[]       <- x@spwn
      }
  }

  #- Define dimensions not in common between avail and biol
  idx <- which(dim(y) != dim(x@n))
  stopifnot(length(idx)>0 & idx==5)

  if(length(idx)>0){
    nBy <- sweep(y,(1:6)[-idx],x@n,"*")
  } else {
    nBy <- y * x@n
  }

  #- Define dimensions not in common between avail and fishery
  idx <- which(dim(y) != dim(catch.n(z)))

  for(iArea in dimnames(z@landings.n)$area)
    res[[iArea]]@n[]              <- unitSums(nBy[,,,,iArea])

  if(area=="all") ret <- res
  if(area!="all") ret <- res[area]
  return(ret)})

  
setMethod("splitByArea",signature(x="FLCatch",y="missing",z="missing"),
  function(x,y,z,area="all",...){
  
  res <- new("FLCatches")
  for(iArea in 1:dims(x)$area){
    res[[iArea]]        <- x[,,,,iArea]
    units(res[[iArea]]) <- units(x)
    range(res[[iArea]]) <- range(x)
    name(res[[iArea]])  <- paste(x@name,"area",dimnames(x@landings.n)$area[iArea])
  }
  names(res)            <- dimnames(x@landings.n)$area
  if(area=="all") ret   <- res
  if(area!="all") ret   <- res[area]
return(ret)})

#-------------------------------------------------------------------------------
# Class to represent 'from' - 'to' movement of fish matrix
#-------------------------------------------------------------------------------


setClass("FLConnect",
	representation("FLArray"),
	prototype(array(as.numeric(NA), dim=c(1,1,1,1,1,1),
		dimnames=list(quant="all", year="1", unitfrom="unique", season="all",
		unitto="unique", iter="1")), units="NA"),
  validity=NULL
) # }}}


setGeneric("FLConnect",function(x,...)
  standardGeneric("FLConnect"))

setMethod("FLConnect",signature(x="FLBiol"),
  function(x,...){

  dim         <- dim(x@n)
  dimnames    <- dimnames(x@n)
  #- Set dim of unitfrom same as unitto
  dim[5]      <- dim[3]
  dimnames[5] <- dimnames[3]
  names(dimnames)[c(3,5)] <- c("unitfrom","unitto")

  flc       <- new("FLConnect", array(as.numeric(NA), dim=dim, dimnames=dimnames), units="NA")

  #- Set default values on the diagonal to 1 (no migration / straying)
  flc[]     <- 0
  for(i in 1:dim[3])
    flc[,,i,,i,] <- 1
return(flc)})

setMethod('sweep', signature(x='FLConnect'),
  function(x, MARGIN, STATS, FUN, check.margin=TRUE, ...)
  {
    x   <- FLQuant(x,units=units(x))
    res <- callNextMethod()
    return(FLQuant(res, units=units(x)))
  }
) # }}}

#-------------------------------------------------------------------------------
# Movement of fish between population units
#-------------------------------------------------------------------------------

setGeneric("connectUnit",function(x,y,...)
  standardGeneric("connectUnit"))

setMethod("connectUnit",signature(x="FLBiol",y="FLConnect"),
  function(x,y,...){
  
  #- Move fish from one unit to another according to a connectivity matrix
  #- Update biols in weight etc according to the new composition
  nBy               <- sweep(unname(y,force=T),c(1:4,6),unname(x@n,       force=T),"*")
  mBy               <- sweep(unname(y,force=T),c(1:4,6),unname(x@m*x@n,   force=T),"*")
  wtBy              <- sweep(unname(y,force=T),c(1:4,6),unname(x@wt*x@n,  force=T),"*")
  fecBy             <- sweep(unname(y,force=T),c(1:4,6),unname(x@fec*x@n, force=T),"*")
  spwnBy            <- sweep(unname(y,force=T),c(1:4,6),unname(x@spwn*x@n,force=T),"*")
  for(iUnit in 1:dims(x)$unit){
    x@n[,,iUnit]    <- unitSums(nBy[,,,,iUnit])
    x@m[,,iUnit]    <- unitSums(mBy[,,,,iUnit])     / x@n[,,iUnit]
    x@wt[,,iUnit]   <- unitSums(wtBy[,,,,iUnit])    / x@n[,,iUnit]
    x@fec[,,iUnit]  <- unitSums(fecBy[,,,,iUnit])   / x@n[,,iUnit]
    x@spwn[,,iUnit] <- unitSums(spwnBy[,,,,iUnit])  / x@n[,,iUnit]
  }
  return(x)})

#-------------------------------------------------------------------------------
# Survival of fish from one year to the next
#-------------------------------------------------------------------------------

setGeneric("survivors",function(x,y,z,w,yr,...)
  standardGeneric("survivors"))
  
setMethod("survivors",signature(x="FLBiol",y="FLCatch",z="FLAvail",w="missing"),
  function(x,y,z,w,yr=NULL,...){
  
    #- Define dimensions not in common between avail and biol
    idx <- which(dim(z) != dim(x@n))
    stopifnot(length(idx)>0 & idx==5)

    if(length(idx)>0){
      nBy <- sweep(z,(1:6)[-idx],x@n,"*")
    } else {
      nBy <- z * x@n
    }

    if(dimnames(catch.n(y))[[3]] == "all" | dimnames(catch.n(y))[[3]] == "unique"){
      for(iUnit in dimnames(x@n)[[3]]){
        survs  <- areaSums(nBy[,yr,iUnit] * exp(-sweep(y@landings.sel[,yr],c(1:4,6),m(x[,yr,iUnit]),"+")))
        if (!is.na(range(x,"plusgroup"))){
          survs[ac(range(x,"plusgroup")-1),] <- quantSums(survs[ac((range(x,"plusgroup")-1):(range(x,"plusgroup"))),])
          survs                              <- trim(survs,age=range(x)["min"]:(range(x,"plusgroup")-1))
        }
        x@n[(range(x)["min"]+1):range(x,"plusgroup"),ac(an(yr)+1),iUnit] <- survs
      }
    } else {
        for(iUnit in dimnames(x@n)[[3]]){
          survs <- list()
          if(iUnit %in% dimnames(catch.n(y))[[3]]){
            survs[[iUnit]] <- areaSums(nBy[,yr,iUnit] * exp(-sweep(y@landings.sel[,yr,iUnit],c(1:4,6),m(x[,yr,iUnit]),"+")))
            if (!is.na(range(x,"plusgroup"))){
              survs[[iUnit]][ac(range(x,"plusgroup")-1),] <- quantSums(survs[[iUnit]][ac((range(x,"plusgroup")-1):(range(x,"plusgroup"))),])
              survs[[iUnit]]                              <- trim(survs[[iUnit]],age=range(x)["min"]:(range(x,"plusgroup")-1))
            }
            x@n[(range(x)["min"]+1):range(x,"plusgroup"),ac(an(yr)+1),iUnit] <- survs[[iUnit]]
          }
        }
      }
  return(x@n[,ac(an(yr)+1)])}
)


setMethod("survivors",signature(x="FLStock",y="missing",z="missing",w="missing"),
  function(x,y,z,w,yr=NULL,...){
  yr    <- ac(yr)
  survs <- stock.n(x[,yr]) * exp(-harvest(x[,yr])-m(x[,yr]))

  if (!is.na(range(x,"plusgroup"))){
    survs[ac(range(x,"plusgroup")-1),] <- quantSums(survs[ac((range(x,"plusgroup")-1):(range(x,"plusgroup"))),])
    survs                              <- trim(survs,age=range(x)["min"]:(range(x,"plusgroup")-1))
  }
  x@stock.n[(range(x)["min"]+1):range(x,"plusgroup"),ac(an(yr)+1),] <- survs
  return(x@stock.n[,ac(an(yr)+1)])}
)


#-------------------------------------------------------------------------------
# catch to harvest, take TAC and calculate landings.sel of each fishery
#-------------------------------------------------------------------------------


setGeneric("TAC2harvest",function(w,x,y,z,...)
  standardGeneric("TAC2harvest"))

setMethod("TAC2harvest",signature(w="FLQuant",x="FLBiol",y="FLCatch",z="FLAvail"),
  function(w,x,y,z,by="unit",scenario="equalShare",...){

  #- Define dimensions not in common between avail and biol
  idx <- which(dim(z) != dim(x@n))
  stopifnot(length(idx)>0 & idx==5)

  #- First get the numbers by area
  if(length(idx)>0){
      nBy <- sweep(z,(1:6)[-idx],x@n,"*")
    } else {
      nBy <- z * x@n
    }

  #- Make sure no fish are created out of nothing
  if(round((sum(nBy,na.rm=T) - sum(x@n,na.rm=T)))!=0)
    stop("FLAvail sums up to more than one over areas")

  #- TAC has been set at a unit level (not at area level)
  if(by == "unit"){
    #- Define object to store landings.sel multipliers
    scaleHarvest <- matrix(NA,nrow=dims(x)$iter,ncol=dims(y)$area,
                           dimnames=list(iter=dimnames(x@n)$iter,area=dimnames(y@landings.sel)$area))

    #- loop over iterations
    for(iTer in 1:nrow(scaleHarvest)){
      if(scenario == "equalShare")  stpar <- rep(1,  dims(y)$area)
      if(scenario == "minTAC")      stpar <- rep(0.5,dims(y)$area)
      if(scenario == "maxTAC")      stpar <- rep(2,  dims(y)$area)
      if(scenario == "equalShare"){
        sameUnit            <- list()
        for(iUnit in dimnames(z)$unit)
          sameUnit[[iUnit]] <- which(apply(iAvail[,,iUnit],2:6,sum)>0)
        stpar               <- stpar[length(sameUnit)]
      }
      
      scaleHarvest[iTer,] <- optim(par=stpar,fn=function(mult,iTac,iBiol,iFleet,inBy,iAvail,iScen,iSameUnit){

          fleetMult       <- numeric(dims(iFleet)$area)
          for(iUnit in dimnames(z)$unit)
            fleetMult[iSameUnit[[iUnit]]] <- rep(mult[which(iUnit == dimnames(z)$unit)],
                                                 length(which(iSameUnit[[iUnit]] %in% 1:dims(iFleet)$area)))

          landsel <- sweep(landings.sel(iFleet),5,fleetMult,"*")[,drop=T]

          #- Residuals per unit
          res   <- list()
          for(iUnit in dimnames(z)$unit){
            biolm   <- iBiol@m[,,iUnit,drop=T]

            #- Note that each fishery has a portion of the stock to its own (since a population can only occur (partly) in one area at a time)
            # As fisheries are unique per area, the fraction of F that results in catch can be calculated per fishery. The numbers of fish
            # available depend on the avail matrix
            res[[iUnit]] <- iTac[,,iUnit,drop=T] -
                              sum(
                                landsel / sweep(landsel,1,biolm,"+") *
                                inBy[,,iUnit,drop=T] * (1-exp(-(sweep(landsel,1,biolm,"+")))) *
                                landings.wt(iFleet)[,drop=T])
          }
          reslst  <- res
          res     <- unlist(res)
          sumres  <- sum(sqrt(res^2))
          if(scenario == "minTAC")       res[which(res < 0)] <- 10*sumres
          if(scenario == "maxTAC")       res[which(res > 0)] <- 10*sumres

          return(sum(sqrt(res^2)))},
        lower=rep(0,length(stpar)),upper=rep(5,length(stpar)),method="L-BFGS-B",iTac=iter(w,iTer),iBiol=iter(x,iTer),iFleet=iter(y,iTer),inBy=iter(nBy,iTer),iAvail=z[,,,,,iTer],iScen=scenario,iSameUnit=sameUnit)$par
     }
     landings.sel(y) <- sweep(landings.sel(y),5:6,t(scaleHarvest),"*")
  }

   #- TAC has been set at an area level (not at unit level)
  if(by == "area"){

    #- Define object to store landings.sel multipliers
    scaleHarvest <- matrix(NA,nrow=dims(x)$iter,ncol=dims(y)$area,
                           dimnames=list(iter=dimnames(x@n)$iter,area=dimnames(y@landings.sel)$area))

    #- loop over iterations
    for(iTer in 1:nrow(scaleHarvest)){
      #- We can loop over areas too, since for each fleet there is one TAC, the optization is easy
      for(iArea in dimnames(z)$area){
        scaleHarvest[iTer,iArea] <- optimize(f=function(mult,iTac,iBiol,iFleet,inBy,iAvail){

          landsel <- landings.sel(iFleet)[,drop=T]*mult
          biolm   <- rowSums(iBiol@m[,drop=T] * iAvail * inBy,na.rm=T) / rowSums(inBy * iAvail,na.rm=T)
          res     <- iTac - sum(landsel / (landsel + biolm) * rowSums(inBy,na.rm=T) * (1-exp(-landsel - biolm)) * landings.wt(iFleet))
          return(sqrt(res^2))},
        iTac=iter(w[,,,,iArea],iTer)[,drop=T],iBiol=iter(x,iTer),iFleet=iter(y,iTer)[,,,,iArea],inBy=iter(nBy,iTer)[,,,,iArea,drop=T],iAvail=z[,,,,iArea,iTer,drop=T],interval=c(0,10))$minimum
      }
    }
    landings.sel(y) <- sweep(landings.sel(y),5:6,t(scaleHarvest),"*")
  }
  return(y)
})

#-------------------------------------------------------------------------------
# FLAccesors
#-------------------------------------------------------------------------------


#FLMetacreateFLAccesors <- function(class, exclude=character(1), include=missing) {
#
#  object <- class
#
#  if(!missing(include))
#  	slots <- getSlots(class)[include]
#  else
#  	slots <- getSlots(class)[!names(getSlots(class))%in%exclude]
#
#	defined <- list()
#
#	for (x in names(slots)) {
#		# check method is defined already and signatures match
#  eval(
#		substitute(if(!is.null(getGeneric(x)) && names(formals(x)) != "object") {
#      warning(paste("Accesor method for", x, "conflicts with a differently defined
#      generic. Type", x, "for more information")); break}, list(x=x))
#	  )
#    # accessor
#		eval(
#		substitute(setMethod(x, signature(object=y),
#      function(object) return(slot(object, x))),
#      list(x=x, y=class))
#		)
#    # replacer
#		eval(
#		substitute(setReplaceMethod(x, signature(object=y, value=v),
#      function(object, value)
#			{slot(object, s) <- value; if(validObject(object)) object else
#        stop("Object not valid")}),
#      list(x=x, y=class, s=x, v=unname(slots[x])))
#		)
#    if(any(unname(slots[x]) %in% c('FLArray', 'FLQuant')))
#    eval(
#		substitute(setReplaceMethod(x, signature(object=y, value="numeric"),
#      function(object, value)
#			{slot(object, s)[] <- value; object}), list(x=x, y=object, s=x))
#		)
#    xr <- paste(x, "<-", sep="")
#		defined[[x]] <- c(x, xr, paste('alias{',x,',', class,'-method}', sep=''),
#			paste('\alias{',xr,',', class,',',unname(slots[x]), '-method}', sep=''),
#			paste('\alias{',x,'-methods}', sep=''),
#			paste('\alias{"',xr, '"-methods}', sep='')
#		)
#	}
#	return(defined)
#}	# }}}
#
#invisible(FLMetacreateFLAccesors("FLAvail", exclude=c("range","name","desc")))