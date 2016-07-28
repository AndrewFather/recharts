#' A lattice-like workaround function to generate Echarts
#'
#' A workaround function to generate echart objects.
#' @param data data.frame. No defaults.
#' @param x x variable, only omitable for histograms, pie, ring and rose charts.
#' @param y y variable. If y is omitted, assign x to y.
#' @param z z variable, only accept data/time variable to open time axis. Optional.
#' @param series series variable for grouping.
#' @param weight weight variable, used in histogram, bubble, etc.
#' @param lat Lattitude variable, only for point/line-marking map.
#' @param lng Longitude variable, only for point/line-marking map.
#' @param x1 Backup x variable, only for line-marking map, force andchord chart.
#' @param lat1 Backup lattitude variable, only for line-marking map.
#' @param lng1 Backup Longitude variable, only for line-marking map.
#' @param type options \code{'scatter', 'point', 'bubble', 'bar', 'line', 'map',
#' 'k', 'pie', 'ring', 'rose','area', 'chord', 'force', 'tree', 'treemap',
#' 'wordcloud', 'heatmap', 'histogram', 'funnel', 'pyramid', 'radar'} \cr
#' \itemize{
#'   \item If 'map' is chosen, the control option should be a vector of length 3:
#'   \code{c('map', mapType, markType)}. \code{mapType} could be either 'world' or 'china',
#'   of which simplified Chinese names are required for 'china'.
#'   When markType equals to 'area', the function colors polygons to show the effects;
#'   while equals to 'point', it ticks pins on the map; while equals to 'line',
#'   it ticks lines on the map.
#'   \item If 'line', 'area' are chosen, the control option should be a vector of
#'   length 2: \code{c(main type, type feature)}. \code{main type} is line or area,
#'   \code{type feature} is 'smooth'.
#'   \item If 'chord' is chosen, the control option should be a vector of
#'   length 2: \code{c('chord', 'riboon')}, which refers to chord chart with ribbon.
#'   \item If 'radar' is chosen, the control option should be a vector of
#'   length 2: \code{c('radar', 'fill')}, which refers to radar chart with color filled.
#' }
#' @param stack Default to FALSE (do not stack). Used in stacked column, bar, line
#' and area chart, etc.
#' @param xlab You can also omit xAxis, directly assign xAxis title.
#' xlab has a higher priority than xAxis[['lab']].
#' @param xAxis x Axis parameters in a list, default
#' \code{list(lab=NULL, color=NULL, splitLine=TRUE, banded=FALSE, rotate=0)}.
#' @param ylab You can also omit yAxis, directly assign yAxis title.
#' ylab has a higher priority than yAxis[['lab']].
#' @param yAxis parameters of y Axis. Refer to xAxis.
#' @param xlab1 Title of secondary x-axis, with higher priority than lab in xAxis1 list,
#' @param xAxis1 Secondary x axis, effecitve only if there are multiple series.
#' Written in a list, default \code{list(lab=NULL, series=NULL, reverse=FALSE,
#' color=NULL, splitLine=TRUE, banded=FALSE, rotate=0)}.
#' series set which series to put on secondary x-axis, which can be either a name
#' vector or an index vector, e.g., c(1,2) or c('male','female').
#' The data is fliped up when reverse is set TRUE.
#' @param ylab1 Title of secondary y-axis, with higher priority than lab in yAxis1 list,
#' @param yAxis1 Refer to xAxis1
#' @param xyflip Flip x, y-axies. Default FALSE.
#' @param AxisAtZero Axes (and sub axes) cross at zero. Default \code{c(FALSE, TRUE)}.
#' @param scale Rescale the axes based on min and max values. Default TRUE.
#' @param tooltip Mouse tip effects swtich. Default TRUE.
#' @param pos Position of image elements which are integers 1-12.
#' Title position default to 6 (o'clock), which means bottom middle.
#' Defaults to legend, toolbox, dataRange, dataZoom, roamController are
#' 11, 1, 6, 8, 2, respectively. Default pos=list(title=6, legend=11, toolbox=1,
#' dataZoom=6, dataRange=8, roam=2)
#' @param markLine Short form: \cr
#' \tabular{llll}{
#'  [col 1] series name|index \tab [col 2] line name \tab [col 3] Line type
#'  \tab [col 4] Light effect \cr
#'  String or number \tab String|NA \tab min|max|average|lm \tab TRUE|FALSE
#' }
#' Full form: \cr
#' \tabular{llllllll}{
#'  [col 1] series name|index \tab [col 2] line name \tab [col 3] Value
#'  \tab [col 4] P0 x \tab [col 5] P0 y \tab [col 6] P1 x \tab [col 7] P1 y
#'  \tab [col 8] Light effect \cr
#'  String|number \tab String|NA \tab num \tab x val \tab y val \tab x val
#'  \tab y val \tab TRUE|FALSE
#' }
#' Examples:
#' \itemize{
#'  \item E.g., both \code{t(c('male', NA, 'average', FALSE))} and \code{t(c(1, NA, 'average', FALSE))}
#'  refer to an average markline of the series 'male', only available for line,
#'  linesmooth, bar, scatter, bubble charts. 'lm' refers to linear regresson
#'  markline which is only available for scatters and bubbles.
#'  \code{t(c(1, NA, 'average', TRUE))} opens light effects of series 'male'. \cr
#'  \item In line, bar, k and scatter charts, 'P0 x','P0 y','P1 x','P1 y' are
#'  comprehended as coordinates. In map charts, these coordinates should be lattitudes
#'  and longitudes. \code{t(c('male', NA, 100, 0, 5, 100, 5, TRUE))} opens light effects of series 'male'.
#' }
#' @param markLinesmooth Used this instead of markLine for smooth marklines with
#' totally identical grammar.
#' @param markPoint Show markpoints, default to NULL.
#' The grammar is a data.frame or matrix with 4 or 6 columns:
#' Short form: \cr
#' \tabular{llll}{
#'  <col 1> series name|index \tab <col 2> Point name \tab <col 3> Point type \tab <col 4> Light effect \cr
#'  String or number \tab String|NA \tab min|max \tab TRUE|FALSE
#'  }
#' Full form: \cr
#' \tabular{llllll}{
#'  [col 1] series name|index \tab [col 2] Point name \tab [col 3] Value \tab [col 4] P x
#'  \tab [col 5] P y \tab [col 6] Light effect \cr
#'  String or number \tab String|NA \tab num \tab x val \tab y val \tab TRUE|FALSE
#' }
#' Examples: \cr
#' \itemize{
#'  \item E.g., both \code{t(c('male',NA,'min',FALSE))} and \code{t(c(1,NA,'min',FALSE))} refer
#'  to a min markpoint of the series 'male', only available for line, linesmooth, bar,
#'  scatter, bubble charts. \code{t(c('male',NA,'min',TRUE))} opens light effects of series 'male'.
#'  \item In line, bar, k and scatter charts, 'P x','P y',... are comprehended
#'  as coordinates. In map charts, these coordinates should be lattitudes and longitudes.
#'  \code{t(c('male',NA,100,0,5,TRUE))} opens light effects of series 'male'.
#' }
#' @param ... elipsis
#'
#' @return An echart object
#' @import Hmisc
#' @importFrom reshape2 dcast
#' @importFrom plyr join
#' @export
#'
#' @seealso \code{\link{ggthemes}} \code{\link{RColorBrewer}} \code{\link{echart}}
#' @examples
#' \dontrun{
#'
#' echartR(iris, x=Sepal.Width, y=Petal.Width, series=Species, type='scatter')
#'
#' echartR(iris, x=~Sepal.Width, y=~Petal.Width, series=~Species)
#' }
echartR<-function(data, x=NULL, y=x, z=NULL, series=NULL, weight=NULL,
                  lat=NULL, lng=NULL, x1=NULL, lat1=NULL, lng1=NULL,
                  type="auto", stack=FALSE,
                  xlab=NULL, xAxis=list(lab=xlab, color=NULL, splitLine=TRUE,
                                        banded=FALSE, rotate=0),
                  ylab=NULL, yAxis=list(lab=ylab, color=NULL, splitLine=TRUE,
                                        banded=FALSE, rotate=0),
                  xlab1=NULL, xAxis1=list(lab=xlab1, series=NULL, reverse=FALSE,
                                          color=NULL, splitLine=TRUE, banded=FALSE,
                                          rotate=0),
                  ylab1=NULL, yAxis1=list(lab=ylab1, series=NULL, reverse=FALSE,
                                          color=NULL, splitLine=TRUE,
                                          banded=FALSE, rotate=0),
                  xyflip=FALSE, AxisAtZero=c(FALSE,TRUE), scale=TRUE,
                  tooltip=TRUE,
                  pos=list(title=6, legend=11, toolbox=1, dataZoom=6,
                           dataRange=8, roam=2),
                  markLine=markLinesmooth, markLinesmooth=NULL, markPoint=NULL,
                  ...){

    #--------recognize variable names--------------------------
    vArgs <- match.arg()
    vArgs <- list(x=substitute(x), y=substitute(y), z=substitute(z),
                  x1=substitute(x1), series=substitute(series),
                  weight=substitute(weight), lat=substitute(lat),
                  lng=substitute(lng), lat1=substitute(lat1),
                  lng1=substitute(lng1))
    .getArgTag <- function(v){
        if (inherits(v, 'character')) return(v)
        else if (inherits(v, "name")) return(deparse(v))
        else if (inherits(v, c("call", "formula")))
            return(gsub("^.*~(.+)$", "\\1", deparse(v)))
    }
    vArgs <- lapply(vArgs, .getArgTag)

    #---------if there is timeline, loop over z-------------
    if (!is.null(vArgs$z)) {
        zvar <- vArgs$z
        z <- data[,zvar]
        timeslice <- unique(z)
    }

    #-----------variables----------------
    Data <- data
    MarkLine <- markLine
    MarkPoint <- markPoint
    yvar <- vArgs$y
    if (!is.null(vArgs$x)) {
        xvar <- vArgs$x
        ## get x levels
        if (is.factor(data[,xvar])) {
            lvlx <- levels(data[,xvar])
        }else if (is.character(data[,xvar])){
            lvlx <- unique(data[,xvar])
        }
    }
    if (!is.null(vArgs$series)) {
        svar <- vArgs$series
        ## get series levels
        if (is.factor(data[,svar])){
            lvlseries <- levels(data[,svar])
        }else{
            lvlseries <- unique(data[,svar])
        }
    }else{
        lvlseries <- NULL
    }

    #---------define graph type--------------
    type <- tolower(type)
    if (type == 'auto'){
        if (!is.null(vArgs$y)) y <- data[,yvar] else y <- NULL
        if (!is.null(vArgs$x)) x <- data[,xvar] else x <- NULL
        type <- determineType(x, y)
    }
    supportedTypes <- c(
        'scatter', 'bar', 'line', 'map', 'k', 'pie', 'ring', 'rose', 'chord',
        'area', 'force', 'bubble', 'funnel', 'pyramid', 'tree', 'treemap',
        'wordcloud', 'heatmap', 'histogram', 'radar', 'gauge'
    )
    if (!type[1] %in% supportedTypes){
        stop("The chart type is not supported! ",
             "we now only support the following charts:\n",
             supportedTypes)
    }

    #----data preProcess-----------
    for (var in names(Data)){  # transform all factors to char
        if (is.factor(Data[,var])) Data[,var]<-as.character(Data[,var])
    }
    if (!is.null(vArgs$weight))  wvar <- vArgs$weight
    if (!is.null(vArgs$lat))     latvar <- vArgs$lat
    if (!is.null(vArgs$x1))      xvar1 <- vArgs$x1
    if (!is.null(vArgs$lat1))    latvar1 <- vArgs$lat1
    if (!is.null(vArgs$lng))     lngvar <- vArgs$lng
    if (!is.null(vArgs$lng1))    lngvar1 <- vArgs$lng1

    if (type[1] %in% c('line','scatter','point','bubble','area','bar')){
        # only these charts can use double axes
        xAxis1 <- mergeList(list(lab=xlab1,series=NULL,reverse=FALSE,color=NULL,
                                 splitLine=TRUE,banded=FALSE,rotate=0), xAxis1)
        yAxis1 <- mergeList(list(lab=ylab1,series=NULL,reverse=FALSE,color=NULL,
                                 splitLine=TRUE,banded=FALSE,rotate=0), yAxis1)

        for (lstName in c("xAxis1","yAxis1")){
            objAxis <- eval(parse(text=lstName))
            if (!is.null(lvlseries)){
                if (is.null(objAxis[['series']])){
                    objAxis <- NULL
                }else{
                    for (i in 1:length(objAxis[['series']])){
                        if (!is.na(as.numeric(objAxis[['series']][i]))){ # numeric
                            if (as.numeric(objAxis[['series']][i])>length(lvlseries)){
                                objAxis[['series']][i]<-NA
                            }else{
                                objAxis[['series']][i]<-
                                    lvlseries[as.numeric(objAxis[['series']][i])]
                            }
                        }else{
                            if ((!objAxis[['series']][i] %in% lvlseries)){ # char
                                objAxis[['series']][i]<-NA
                            }
                        }
                    }
                    objAxis[['series']] <- objAxis[['series']][!is.na(objAxis[['series']])]
                }
            }else{
                objAxis <- NULL
            }
            if (lstName=='xAxis1') {
                xAxis1 <- objAxis
                if (is.null(objAxis[['series']]) |
                    length(objAxis[['series']])==0) xAxis1 <- NULL
            }
            if (lstName=='yAxis1') {
                yAxis1 <- objAxis
                if (is.null(objAxis[['series']]) |
                    length(objAxis[['series']])==0) yAxis1 <- NULL
            }
        }
    }else{
        xAxis1 <- NULL
        yAxis1 <- NULL
    }
    if (!is.null(xAxis1)) {
        if (xAxis1[['reverse']]){
            if (is.numeric(data[,xvar])) {
                data[data[,svar] %in% xAxis1[['series']],xvar] <-
                    -data[data[,svar] %in% xAxis1[['series']],xvar]
            }
        }
    }
    if (!is.null(yAxis1)){
        if (yAxis1[['reverse']]){
            if (is.numeric(data[,yvar])) {
                data[data[,svar] %in% yAxis1[['series']],yvar] <-
                    -data[data[,svar] %in% yAxis1[['series']],yvar]
            }
        }
    }
    #---------------------------------------------------------#
    #                       Loop over Z                       |
    #---------------------------------------------------------#
    for (t in 1:ifelse(is.null(z),1,length(timeslice)))  {

        #-------pre-process of data-----------
        if (!is.null(vArgs$z)) data <- Data[Data[,zvar]==timeslice[t],]
        if (!is.null(vArgs$y)) y <- data[,yvar] else y <- NULL
        if (!is.null(vArgs$x)) x <- data[,xvar] else x <- NULL
        if (!is.null(vArgs$series)) series <- data[,svar] else series <- NULL
        if (!is.null(vArgs$weight)) weight <- data[,wvar] else weight <- NULL
        if (!is.null(vArgs$lat)) lat <- data[,latvar]
        if (!is.null(vArgs$x1)) x1 <- data[,xvar1]
        if (!is.null(vArgs$lat1)) lat1 <- data[,latvar1]
        if (!is.null(vArgs$lng)) lng <- data[,lngvar]
        if (!is.null(vArgs$lng1)) lng1 <- data[,lngvar1]
        if (type[1] %in% c('bubble') & is.null(weight)){
            wvar <- yvar
            weight <- y
        }

        if (type[1] %in% c('pie','ring','rose','funnel','pyramid')){
            if (is.null(series) & !is.null(x)){
                svar <- xvar
                data[,svar] <- x
                series <- x
            }
            series <- as.factor(series)
            lvlseries <- levels(series)
            data <- data[,c(svar,yvar)]
            if (is.factor(y) | is.character(y)){
                data <- dcast(data, data[,1]~., value.var=yvar, length)
            }else{
                data <- dcast(data, data[,1]~., value.var=yvar, sum)
            }
            names(data) <- c(svar, yvar)
        }else if (type[1] %in% c("histogram")){
            if (is.null(splitNumber)){
                nbreaks=10
            }else{
                nbreaks=ifelse(splitNumber[1]==1,10,splitNumber[1]+1)
            }
            interval <- (max(y)-min(y)) / (nbreaks-1)
            cut <- seq(from=min(y),to=max(y), length.out=nbreaks)
            cut <- round(cut,ifelse(interval>1, 1, 1+ceiling(log10(1/interval))))
            hist <- hist(data[,yvar], breaks=cut, plot=FALSE)
            hist_def <- hist(y, breaks=nbreaks-1, plot=FALSE)
            valRange <- max(hist_def$breaks) - min(hist_def$breaks)
            x1 <- vector()
            for (i in 1:length(hist$breaks)-1){
                x1[i] <- paste(hist$breaks[i], hist$breaks[i+1],sep="-")
            }
            data <- data.frame(x=hist$mids, y=hist$counts,x1=x1)
            xvar <- yvar; yvar <- "Freq"; xvar1 <- "Breaks"
            names(data) <- c(xvar,yvar,xvar1)
            x <- data[,xvar]; y <- data[,yvar]
        }else if (type[1] %in% c('line')){
            # if (is.numeric(x)) {
            #     data[,xvar] <- x <- as.character(x)
            # }
        }else if (type[1] %in% c('force')){
            dtlink <- as.data.frame(matrix(unlist(strsplit(x,"/")),
                                           byrow=TRUE,nrow=nrow(data)),stringsAsFactors=FALSE)
            dtnodeval <- as.data.frame(matrix(unlist(strsplit(x1,"/")),
                                              byrow=TRUE,nrow=nrow(data)),stringsAsFactors=FALSE)
            dtcatg <- as.data.frame(matrix(unlist(strsplit(series,"/")),
                                           byrow=TRUE,nrow=nrow(data)),stringsAsFactors=FALSE)
            dtlink <- cbind(dtlink,y)
            names(dtlink) <- c("from","to","relation","y")
            names(dtnodeval) <- c("value1","value2")
            names(dtcatg) <- c("catg1","catg2")
            lvlseries <- unique(c(unique(dtcatg[,1]),unique(dtcatg[,2])))
            dtnode <- cbind(dtlink,dtnodeval,dtcatg)
            dtnode <- rbind(as.matrix(dtnode[,c('from','value1','catg1')]),
                            as.matrix(dtnode[,c('to','value2','catg2')]))
            dtnode <- unique(as.data.frame(dtnode))
            names(dtnode) <- c("name","value","category")
            rm(dtnodeval,dtcatg)
        }
        #-------check invalid input---------
        if (!all(as.numeric(pos) %in% 1:12)) stop("list pos must be all integers 1-12")
        pos <- mergeList(list(title=6, legend=11, toolbox=1, dataZoom=6, dataRange=8,
                              roam=2),pos)

        #---------timeline--------------
        if (!is.null(z)){
            lstTimeline <- list(data=timeslice, autoPlay=TRUE)
            attr(lstTimeline, 'sliceby') <- zvar
            if (!is.null(title) & pos[['title']] %in% 5:7) lstTimeline[['y2']] <- 50
        }

        #-------Tooltip--------------
        if (tooltip){
            lstTooltip <- list(
                trigger = ifelse(type[1] %in% c('pie','ring','funnel','pyramid','map',
                                                'rose','wordcloud','radar',
                                                'chord','force','gauge'),
                                 'item', 'axis'),
                axisPointer = list(
                    show = TRUE,lineStyle = list(type = 'dashed',width = 1)
                )
            )
            if (inherits(x,c('POSIXlt','POSIXct','Date'))){
                lstTooltip[['trigger']] <- 'item'
                lstTooltip[['formatter']] <- JS(tooltipJS('time'))
            }
            if (type[1] %in% c('scatter','bubble')){
                if (!is.null(series)){
                    lstTooltip[['formatter']] <- JS(tooltipJS('scatter'))
                }
                lstTooltip[['axisPointer']] <- list(
                    show= TRUE,type='cross',lineStyle= list(type= 'dashed',width= 1)
                )
            }else if (type[1] %in% c('ring','pie')){
                lstTooltip[['formatter']] <- tooltipJS('pie')
            }else if (type[1] %in% c('chord','chordribbon','force')){
                if (!is.null(series)){
                    lstTooltip[['formatter']] <- JS(tooltipJS('chord_mono'))
                }else{
                    lstTooltip[['formatter']] <- JS(tooltipJS('chord_multi'))
                }
            }else if (type[1]=='k'){
                lstTooltip[['formatter']] <- JS(tooltipJS('k'))
            }else if (type[1]=='histogram'){
                lstTooltip[['formatter']] <- JS(tooltipJS('hist'))
            }
        }else{
            lstTooltip = list(show=FALSE)
        }

        #------------Axis-------------
        if (length(AxisAtZero)==1) AxisAtZero <- rep(AxisAtZero,2)
        AxisAtZero <- as.logical(AxisAtZero)
        for (i in 1:2) if(is.na(AxisAtZero[i])) AxisAtZero[i] <- switch(i,FALSE,TRUE)
        xAxis <- mergeList(list(lab=xlab,color=NULL,splitLine=TRUE,banded=FALSE,rotate=0),
                           xAxis)
        yAxis <- mergeList(list(lab=ylab,color=NULL,splitLine=TRUE,banded=FALSE,rotate=0),
                           yAxis)
        #         if (!is.null(xlab)) xAxis[['lab']]<-xlab
        #         if (!is.null(ylab)) yAxis[['lab']]<-ylab
        if (!is.null(xlab1) & !is.null(xAxis1)) xAxis1[['lab']]<-xlab1
        if (!is.null(ylab1) & !is.null(yAxis1)) yAxis1[['lab']]<-ylab1

        for (i in 1:ifelse(is.null(xAxis1),1,2)){  # xAxises
            if (i==1) {
                varXAxis <- xAxis
            }else if (i==2){
                varXAxis <- xAxis1
            }
            tmpXAxis = list(
                name = ifelse(is.null(varXAxis[['lab']]),xvar,varXAxis[['lab']]),
                type = ifelse(inherits(x,c('POSIXct','POSIXlt','Date')),'time',
                              ifelse(!is.numeric(x),'category','value')),
                scale = scale)
            if (tmpXAxis[['type']]=='category'){
                if (length(unique(as.character(x)))==1){
                    tmpXAxis[['data']] <- list(unique(as.character(x)))
                }else{
                    tmpXAxis[['data']] <- unique(as.character(x))
                }
            }else{
                if (min(x,na.rm=TRUE)>0 & AxisAtZero[1]) tmpXAxis[['min']] <- 0
            }
            if (type[1] %in% c('line','linesmooth','area','areasmooth')){
                tmpXAxis[['boundaryGap']] <- FALSE
            }else if (type[1] %in% c('k')){
                tmpXAxis[['axisTick']] <- list(onGap=FALSE)
            }else if (type[1]=='histogram'){
                tmpXAxis[['min']] <- min(hist_def$breaks)
                tmpXAxis[['max']] <- max(hist_def$breaks)
            }
            tmpXAxis[['axisLabel']] <- list(interval=0)
            if (!is.null(varXAxis[['reverse']])){
                if (varXAxis[['reverse']]) {
                    tmpXAxis[['axisLabel']][['formatter']] <- JS('function(v){return -v;}')
                }
            }
            if (is.null(varXAxis[['color']])) {
                varXAxis[['color']] <- '#4488bb'
                tmpXAxis[['axisLine']] <- list(
                    show=TRUE, onZero=AxisAtZero[1],
                    lineStyle=list(color=varXAxis[['color']]))
            }else {
                if (varXAxis[['color']]=='none'){
                    tmpXAxis[['axisLine']] <- list(show=FALSE)
                    if (!is.null(lstGrid)) lstGrid[['borderWidth']] <- 0
                }else if (!is(try(col2rgb(varXAxis[['color']])),'try-error')){
                    tmpXAxis[['axisLine']] <- list(
                        show=TRUE, onZero=AxisAtZero[1],
                        lineStyle=list(color=varXAxis[['color']]))
                }
            }
            tmpXAxis[['splitArea']]<-list(show=ifelse(varXAxis[['banded']],TRUE,FALSE))
            if (abs(as.numeric(varXAxis[['rotate']]))<=90){
                if (is.null(tmpXAxis[['axisLabel']])) tmpXAxis[['axisLabel']]<-list()
                tmpXAxis[['axisLabel']][['rotate']] <- varXAxis[['rotate']]
            }
            if (is.null(xAxis1)) {
                lstXAxis <- tmpXAxis
            }else{
                if (i==1) lstXAxis <- list(list())
                lstXAxis[[i]] <- tmpXAxis
            }
        }

        for (i in 1:ifelse(is.null(yAxis1),1,2)){ # yAxises
            if (i==1) {
                varYAxis <- yAxis
            }else if (i==2){
                varYAxis <- yAxis1
            }
            tmpYAxis = list(
                name = ifelse(is.null(varYAxis[['lab']]),yvar,varYAxis[['lab']]),
                type = 'value',
                scale = scale
            )
            if (min(y,na.rm=TRUE)>0 && AxisAtZero[2]) tmpYAxis[['min']] <- 0
            if (max(y,na.rm=TRUE)==1 && min(y,na.rm=TRUE)>=0) {
                tmpYAxis[['max']] <- 1
                tmpYAxis[['min']] <- 0
            }

            if (!is.null(varYAxis[['reverse']])){
                if (varYAxis[['reverse']]) {
                    if (is.null(tmpYAxis[['axisLabel']])) tmpYAxis[['axisLabel']] <- list()
                    tmpYAxis[['axisLabel']][['formatter']] <- JS('function(v){return -v;}')
                }
            }
            if (is.null(varYAxis[['color']])) {
                varYAxis[['color']] <- '#4488bb'
                tmpYAxis[['axisLine']] <- list(
                    show=TRUE, onZero=AxisAtZero[2],
                    lineStyle=list(color=varYAxis[['color']]))
            }else {
                if (varYAxis[['color']]=='none'){
                    tmpYAxis[['axisLine']] <- list(show=FALSE)
                    if (is.null(xAxis1)) {
                        lstXAxis[['splitLine']]<-list(show=FALSE)
                    }else{
                        lstXAxis[[i]][['splitLine']]<-list(show=FALSE)
                    }
                    if (!is.null(lstGrid)) lstGrid[['borderWidth']] <- 0
                }else if (!is(try(col2rgb(varYAxis[['color']])),'try-error')){
                    tmpYAxis[['axisLine']] <- list(
                        show=TRUE, onZero=AxisAtZero[2],
                        lineStyle=list(color=varYAxis[['color']]))
                    if (is.null(xAxis1)) {
                        lstXAxis[['splitLine']]<-list(show=ifelse(varYAxis[['splitLine']],TRUE,FALSE))
                    }else{
                        lstXAxis[[i]][['splitLine']]<-list(show=(varYAxis[['splitLine']]))
                    }
                }
            }
            tmpYAxis[['splitArea']]<-list(show=(varYAxis[['banded']]))

            if (abs(as.numeric(varYAxis[['rotate']])) <= 90){
                if (is.null(tmpYAxis[['axisLabel']])) tmpYAxis[['axisLabel']] <- list()
                tmpYAxis[['axisLabel']][['rotate']] <- varYAxis[['rotate']]
            }
            if (is.null(yAxis1)) {
                lstYAxis <- tmpYAxis
            }else{
                if (i==1) lstYAxis <- list(list())
                lstYAxis[[i]] <- tmpYAxis
            }
        }

        for (i in 1:ifelse(is.null(xAxis1),1,2)){  # Continue to revise yAxis
            if (i==1) {
                varXAxis <- xAxis
            }else if (i==2){
                varXAxis <- xAxis1
            }
            if (!is.null(varXAxis[['color']])){
                if (varXAxis[['color']]=='none'){
                    if (is.null(yAxis1)) {
                        lstYAxis[['splitLine']]<-list(show=FALSE)
                    }else{
                        lstYAxis[[i]][['splitLine']]<-list(show=FALSE)
                    }
                }else if (!is(try(col2rgb(varXAxis[['color']])),'try-error')){
                    if (is.null(yAxis1)) {
                        if (is.null(lstYAxis)) lstYAxis <- list()
                        lstYAxis[['splitLine']]<-list(show=ifelse(varXAxis[['splitLine']],T,F))
                    }else{
                        if (is.null(lstYAxis)) lstYAxis <- list(list(),list())
                        lstYAxis[[i]][['splitLine']]<-list(show=ifelse(varXAxis[['splitLine']],T,F))
                    }
                }
            }
        }
        if (!is.null(xAxis1)) lstYAxis[['axisLine']][['onZero']] <- FALSE
        else lstYAxis[['axisLine']][['onZero']] <- AxisAtZero[2]
        if (!is.null(yAxis1)) lstXAxis[['axisLine']][['onZero']] <- FALSE
        else lstXAxis[['axisLine']][['onZero']] <- AxisAtZero[1]

        if (xyflip){ #exchange settings of xAxis and yAxis
            tmp <- lstYAxis
            lstYAxis <- lstXAxis
            lstXAxis <- tmp
            rm(tmp)
        }

        #if (lstXAxis[['type']]=='time' | lstYAxis[['type']]=='time'){
        #    lstToolbox[['feature']][['magicType']] <- list(show=FALSE)
        #    if (lstXAxis[['type']]=='time') {
        #        lstXAxis[['boundaryGap']] <- NULL
        #    }
        #    if (lstYAxis[['type']]=='time') {
        #        lstYAxis[['boundaryGap']] <- NULL
        #    }
        #}



        #----------polar---------------
        if (type[1] %in% c('radar')){
            x <- factor(x,levels=unique(x))
            indicator <- levels(x)
            lstPolar <- list(list(radius='70%', indicator=list()))
            for (i in 1:length(indicator)){
                lstPolar[[1]][['indicator']][[i]] <- list(
                    text = as.character(indicator[i]),
                    max = max(data[data[,xvar]==indicator[i],yvar]) * 1.25
                )
            }
        }

        #------------Series---------------
        lstSeries <- list()
        if (inherits(x,c('POSIXct','POSIXlt','Date'))) {
            x<- data[,xvar] <- format(data[,xvar],tz="GMT+8",format=
                                          "%a %b %d %Y %H:%M:%S GMT+0800 (China Standard Time)")
        }
        y[is.na(y)] <- data[is.na(data[,yvar]),yvar] <- '-'
        if (is.null(weight)){
            symbolSizeFold <- 1
        }else{
            symbolSizeFold <- ifelse(max(weight)>50,1/ceiling(max(weight)/50),
                                     ceiling(2/min(weight)))
        }

        if (type[1] %in% c('scatter','bubble')){
            if (is.null(series)){
                lstSeries[[1]] <- list(
                    type='scatter', name=ifelse(is.null(xAxis$lab),xvar,xAxis$lab),
                    data=as.matrix(data[,c(xvar,yvar)]),
                    large=ifelse(nrow(data)>2000,TRUE,FALSE)
                )
                if (type[1]=='bubble'){
                    lstSeries[[1]][['data']] <-
                        as.matrix(data[,c(xvar,yvar,wvar)])
                    lstSeries[[1]][['symbolSize']] <-
                        JS('function (value){
                           return Math.round(value[2]*',symbolSizeFold,');}')
            }
                }else{
                    for (i in 1:ifelse(is.null(series),1,length(lvlseries))){
                        lstSeries[[i]] <- list(
                            type='scatter',name=lvlseries[i],
                            data=as.matrix(data[data[,svar]==
                                                    lvlseries[i],
                                                c(xvar,yvar)]),
                            large=ifelse(nrow(data)>2000,TRUE,FALSE)
                        )
                        if (length(lvlseries)>1){
                            #lstSeries[[i]][['name']] <- as.vector(lvlseries[i])
                            if (lvlseries[i] %in% xAxis1[['series']]){
                                lstSeries[[i]][['xAxisIndex']] <-1
                            }
                            if (lvlseries[i] %in% yAxis1[['series']]){
                                lstSeries[[i]][['yAxisIndex']] <-1
                            }
                        }
                        if (type[1]=='bubble'){
                            lstSeries[[i]][['data']] <-
                                as.matrix(data[data[,svar]==
                                                   lvlseries[i],
                                               c(xvar,yvar,wvar)])
                            lstSeries[[i]][['symbolSize']] <-
                                JS('function (value){
                                   return Math.round(value[2]*',symbolSizeFold,');}')
                    }
                        }
                    }
            }else if (type[1] %in% c('ring','pie','rose')){
                lstSeries[[1]] <- list(
                    name=svar,
                    type='pie',
                    data=list()
                )
                if (type[1]=='ring'){
                    lstSeries[[1]][['radius']] <- c('50%','70%')
                    if (!is.null(z)) {
                        lstSeries[[1]][['center']] <- c('50%','45%')
                        lstSeries[[1]][['radius']] <- c('40%','60%')
                    }
                    lstSeries[[1]][['itemStyle']] <- list(
                        emphasis = list(
                            label=list(
                                show=TRUE, position='center',
                                textStyle=list(fontSize='30',fontWeight='bold')
                            )
                        )
                    )
                }else{
                    lstSeries[[1]][['radius']] <- '70%'
                    lstSeries[[1]][['center']] <- c('50%','50%')
                    if (!is.null(z)) {
                        lstSeries[[1]][['center']] <- c('50%','45%')
                        lstSeries[[1]][['radius']] <- '60%'
                    }
                    if (type[1]=='rose'){
                        lstSeries[[1]][['roseType']] <- 'radius'
                    }
                }
                for (i in 1:nrow(data)){
                    lstSeries[[1]][['data']][[i]]<- list(
                        value=data[i,yvar],name=as.character(data[i,svar])
                    )
                }
    }else if (type[1] %in% c('funnel','pyramid')){
        lstSeries[[1]] <- list(
            name=svar,
            type='funnel',
            data=list()
        )
        if (type[1]=='funnel'){
            lstSeries[[1]][['x']] <- '10%'
        }else{
            lstSeries[[1]][['x']] <- '25%'
            lstSeries[[1]][['sort']] <- 'ascending'
        }
        for (i in 1:nrow(data)){
            lstSeries[[1]][['data']][[i]]<- list(
                value=data[i,yvar],name=as.character(data[i,svar])
            )
        }
    }else if (type[1] %in% c('wordcloud')){
        lstSeries[[1]] <- list(
            name=ifelse(is.null(xAxis$lab),xvar,xAxis$lab),
            type='wordCloud',
            size=c('80%','80%'),
            textRotation=c(0,45,90,-45),
            textPadding=0,
            autoSize=list(enable=TRUE,minSize=10),
            data=list()
        )
        for (i in 1:nrow(data)){
            lstSeries[[1]][['data']][[i]]<- list(
                value=data[i,yvar],name=as.character(data[i,xvar])
                #,itemStyle=list(normal=list(color=sample(unlist(lstColor),1)))
            )
        }
    }else if (type[1] %in% c('line','area')){
        #---------reformat missing value----------
        #y[is.na(y)] <- data[is.na(data[,yvar]),yvar] <- '-'
        #print(yvar)
        #print(t)
        #print(data[,yvar])
        if (is.null(series)){ # single serie
            lstSeries[[1]] <- list(
                type='line',name=ifelse(is.null(xAxis$lab),xvar,xAxis$lab),
                data=data[,yvar]
            )
            if (type[1] %in% c("area",'areasmooth')){
                lstSeries[[1]][['itemStyle']] <-
                    list(normal=list(areaStyle=list(type='default')))
            }
            if (type[1] %in% c('linesmooth','areasmooth')){
                lstSeries[[1]][['smooth']] <- TRUE
            }
            #if (lstXAxis[['type']]=='time' || lstYAxis[['type']]=='time'){
            #    lstSeries[[1]][['name']] <- yvar
            #    #lstSeries[[1]][['showAllSymbol']] <- T
            #    lstSeries[[1]][['data']] <- as.matrix(data[,c(xvar,yvar)])
            #}
        }else{  # multiple series
            for (i in 1:ifelse(is.null(series),1,length(lvlseries))){
                lstSeries[[i]] <- list(
                    name=as.vector(lvlseries[i]),
                    type='line',
                    data= data[data[,svar]==lvlseries[i],yvar]
                )
                if (lvlseries[i] %in% xAxis1[['series']]){
                    lstSeries[[i]][['xAxisIndex']] <-1
                }
                if (lvlseries[i] %in% yAxis1[['series']]){
                    lstSeries[[i]][['yAxisIndex']] <-1
                }
                if (stack) lstSeries[[i]][['stack']] <- 'Stack'
                if (type[1] %in% c("area",'areasmooth')){
                    lstSeries[[i]][['itemStyle']] <-
                        list(normal=list(areaStyle=list(type='default')))
                }
                if (type[1] %in% c('linesmooth','areasmooth')) {
                    lstSeries[[i]][['smooth']] <- TRUE
                }
                #if (lstXAxis[['type']]=='time' || lstYAxis[['type']]=='time'){
                #    lstSeries[[i]][['name']] <- lvlseries[i]
                #    #lstSeries[[i]][['showAllSymbol']] <- T
                #    lstSeries[[i]][['data']] <- as.matrix(
                #        data[data[,svar]==lvlseries[i],
                #             c(xvar,yvar)])
                #}
            }
        }
    }else if (type[1] =='k'){
        for (i in 1:ifelse(is.null(series),1,length(lvlseries))){
            if (is.null(series)){
                dset <- data
            }else{
                dset <- data[data[,svar]==lvlseries[i],]
            }
            # dset <- dcast(dset,as.formula(paste(xvar,"~",xvar1)),sum,value.var=yvar)
            dset <- dcast(dset,as.formula(paste(xvar,"~",xvar1)),value.var=yvar)
            dset[,xvar] <- factor(as.character(dset[,xvar]),levels=lvlx)
            dset <- dset[order(dset[,xvar]),]
            lstSeries[[i]] <-
                list(type='k',name=ifelse(is.null(series),yvar,lvlseries[i]),
                     data=as.matrix(dset[,2:5]))
        }
    }else if (type[1] =='gauge'){
        for (i in 1:ifelse(is.null(series),1,length(lvlseries))){
            if (is.null(series)){
                dset <- data[data[,xvar]!='axisStyle',]
            }else{
                dset <- data[data[,svar]==lvlseries[i] & data[,xvar]!='axisStyle',]
            }
            axisStyle <- data[data[,xvar]=='axisStyle',]
            lstSeries[[i]] <-
                list(type='gauge',name=ifelse(is.null(series),yvar,lvlseries[i]),
                     title=list(show=TRUE,offsetCenter=c(0,'-40%'),
                                textStyle=list(fontWeight='bolder')),
                     pointer=list(width=5),axisLine=list(lineStyle=list(width=8)),
                     detail=list(textStyle=list(fontWeight='bolder'),color='auto'),
                     axisTick=list(length=12,lineStyle=list(color='auto')),
                     splitLine=list(show=TRUE,length=30,lineStyle=list(color='auto')))
            if (!is.null(splitNumber)) {
                lstSeries[[i]][['splitNumber']] <- splitNumber
                lstSeries[[i]][['axisTick']][['splitNumber']] <- splitNumber
            }
            if (!is.null(dset[,xvar1])){
                lstSeries[[i]][['detail']][['formatter']] <-
                    paste0("{value}", dset[1,xvar1])
            }
            for (j in 1:nrow(dset)){
                lstSeries[[i]][['data']][[j]] <- list(value=dset[j,yvar],
                                                      name=as.character(dset[j,xvar]))
            }
        }
        # axis Style
        if (nrow(axisStyle)>0){
            axisStyle[,xvar1] <- as.character(axisStyle[,xvar1])
            for (j in 1:nrow(axisStyle)){
                if (substr(col2rgb(axisStyle[j,xvar1]),1,1)!="#"){
                    colvec <- as.vector(col2rgb(axisStyle[j,xvar1]))
                    axisStyle[j,xvar1] <- rgba(colvec)
                }
            }
            lstSeries[[1]][['axisLine']][['lineStyle']][['color']] <-
                as.matrix(axisStyle[,c(yvar,xvar1)])
        }
    }else if (type[1] %in% c('radar','radarfill')){
        if (is.null(series)){
            lstSeries[[1]] <- list(
                name=ifelse(is.null(xAxis$lab),xvar,xAxis$lab),
                type='radar',
                data=list(value=data[,yvar],
                          name=yvar)
            )
            if (type[1]=='radarfill'){
                lstSeries[[1]][['itemStyle']] <- list(
                    normal=list(areaStyle=list(type='default'))
                )
            }else{
                lstSeries[[1]][['itemStyle']] <- list(
                    emphasis=list(areaStyle=list(color='rgba(0,250,0,0.3)'))
                )
            }
        }else{
            lstSeries[[1]] <- list(
                name=ifelse(is.null(xAxis$lab),xvar,xAxis$lab),
                type='radar',
                data=list()
            )
            if (type[1]=='radarfill'){
                lstSeries[[1]][['itemStyle']] <- list(
                    normal=list(areaStyle=list(type='default'))
                )
            }else{
                lstSeries[[1]][['itemStyle']] <- list(
                    emphasis=list(areaStyle=list(color='rgba(0,250,0,0.3)'))
                )
            }
            for (i in 1:length(lvlseries)){
                lstSeries[[1]][['data']][[i]]<-list(
                    value = data[data[,svar]==lvlseries[i],yvar],
                    name = as.vector(lvlseries[i])
                )
            }
        }
    }else if (type[1] %in% c('map')){
        mapType <- ifelse(is.null(type[2]),'china',tolower(type[2]))
        mapMode <- ifelse(is.null(type[3]),'area',tolower(type[3]))
        for (i in 1:ifelse(is.null(series),1,length(lvlseries))){
            lstSeries[[i]] <- list(
                type='map',
                mapType=mapType,
                roam=TRUE,
                data=list()
            )
            if (is.null(series) | length(lvlseries)==1){
                dset <- data
                lstSeries[[i]][['name']] <- yvar
            }else{
                dset <- data[data[,svar]==lvlseries[i],]
                lstSeries[[i]][['name']] <- lvlseries[i]
            }

            if (mapMode=='area'){  #area mode
                lstSeries[[i]][['itemStyle']]=list(
                    normal=list(label=list(show=FALSE)),
                    emphasis=list(label=list(show=TRUE))
                )
                for (j in 1:nrow(dset)){
                    lstSeries[[i]][['data']][[j]]<- list(
                        value=dset[j,yvar],name=as.character(dset[j,xvar])
                    )
                }
            }else if (mapMode=='point'){             #point mode
                lstSeries[[i]][['data']] <- vector(mode='numeric')
                lstSeries[[i]][['hoverable']] <- FALSE
                lstSeries[[i]][['markPoint']] <- list(
                    symbolSize=ceiling(10/log10(nrow(dset))),
                    itemStyle=list(
                        normal=list(borderColor='#87cefa',
                                    borderWidth=1,
                                    label=list(show=FALSE)),
                        emphasis=list(borderColor='#1e90ff',
                                      borderWidth=3,
                                      label=list(show=FALSE))
                    )
                )
                for (j in 1:nrow(dset)){
                    lstSeries[[i]][['markPoint']][['data']][[j]] <- list(
                        value=dset[j,yvar],name=as.character(dset[j,xvar])
                    )
                    lstSeries[[1]][['geoCoord']][[dset[j,xvar]]] <-
                        c(dset[j,lngvar],dset[j,latvar])
                } # all geoCoord append to series 1
            }else if (mapMode=='line'){ #line mode
                lstSeries[[i]][['data']] <- vector(mode='numeric')
                lstSeries[[i]][['hoverable']] <- FALSE
                lstSeries[[i]][['markLine']] <- list(
                    itemStyle=list(normal=list(borderWidth=1))
                )
                if (!is.null(markLinesmooth)){
                    lstSeries[[i]][['markLine']][['smooth']] <- T
                }
                for (j in 1:nrow(dset)){
                    lstSeries[[i]][['markLine']][['data']][[j]] <- list(
                        list(name=as.character(dset[j,xvar])),
                        list(name=as.character(dset[j,xvar1]))
                    )
                    if (!is.na(dset[j,yvar])){
                        lstSeries[[i]][['markLine']][['data']][[j]][[2]][['value']] <-
                            dset[j,yvar]
                    }
                    lstSeries[[1]][['geoCoord']][[dset[j,xvar]]] <-
                        c(dset[j,lngvar],dset[j,latvar])
                    lstSeries[[1]][['geoCoord']][[dset[j,xvar1]]] <-
                        c(dset[j,lngvar1],dset[j,latvar1])
                }
            }
        }
    }else if (type[1] %in% c('chord','chordribbon')){
        # data must be ordered
        data[,xvar1] <- factor(data[,xvar1],levels=unique(data[,xvar1]))
        data <- data[order(data[,xvar1]),]
        data[,xvar1] <-as.character(data[,xvar1])

        for (i in 1:ifelse(is.null(series),1,length(lvlseries))){
            lstSeries[[i]] <- list(
                type='chord',sort='ascending',sortSub='descending',
                itemStyle=list(normal=list(label=list(
                    rotate=ifelse(xAxis[['rotate']]!=0,TRUE,FALSE))))
            )
            if (!is.null(series)){
                lstSeries[[i]][['name']] <- lvlseries[i]
            }
            if (i>1){
                lstSeries[[i]][['insertToSerie']] <- lstSeries[[1]][['name']]
            }
            if (type[1]=='chord'){ # no ribbons
                lstSeries[[i]][['ribbonType']] <- F
                lstSeries[[i]][['radius']] <- '60%'
            }
            # matrix || node/link mode
            if (identical(levels(as.factor(data[,xvar])),
                          levels(as.factor(data[,xvar1])))){ # is a squared matrix
                for (j in 1:length(unique(data[,xvar1]))){
                    lstSeries[[i]][['data']][[j]] <- list(
                        name=unique(data[,xvar1])[j])
                }
                tmpD <- data
                #tmpD <- tmpD[order(tmpD[,xvar],tmpD[,xvar1]),]
                #tmpD[is.na(tmpD[,yvar]),yvar] <- 0
                if (!is.null(series)){
                    tmpD[is.na(tmpD[,svar]) | tmpD[,svar]!=lvlseries[i],yvar] <- 0
                }

                tmpD[,xvar] <- factor(tmpD[,xvar],levels=unique(data[,xvar1]))
                tmpD[,xvar1] <- factor(tmpD[,xvar1],levels=unique(data[,xvar1]))
                tmpM <- dcast(tmpD,eval(parse(text=paste(xvar,'~',xvar1))),
                              value.var=yvar,sum)
                row.names(tmpM) <- tmpM[,1]
                tmpM <- tmpM[,2:ncol(tmpM)]
                tmpM <- tmpM[unique(x1),unique(x1)]
                tmpM <- as.matrix(tmpM)
                #dimnames(tmpM) <- list(unique(x1),unique(x1))
                #print(lvlseries[i])
                #print(tmpD)
                #print(matrix(tmpD[,yvar],nrow=sqrt(nrow(tmpD))))
                #lstSeries[[i]][['matrix']] <-matrix(tmpD[,yvar],
                #                                   nrow=sqrt(nrow(tmpD)))
                lstSeries[[i]][['matrix']] <- tmpM
            }else{ # nodes and links
                for (j in 1:length(c(unique(data[,xvar]),
                                     unique(data[,xvar1])))){
                    lstSeries[[i]][['nodes']][[j]] <- list(
                        name=c(unique(data[,xvar]),unique(data[,xvar1]))[j])
                }
                dset <- data
                if (!is.null(series)){
                    dset[is.na(dset[,svar]) | dset[,svar]!=lvlseries[i],yvar] <- 0
                }
                for (j in 1:nrow(dset)){
                    lstSeries[[i]][['links']][[j]] <- list(
                        source=dset[j,xvar1],target=dset[j,xvar],
                        weight=dset[j,yvar],
                        name=ifelse(is.null(series),xvar1,lvlseries[i])
                    )
                }
                if (type[1]=='chordribbon'){#chordribbon: must be mutual links
                    for (j in 1:nrow(dset)){
                        lstSeries[[i]][['links']][[nrow(dset)+j]] <- list(
                            target=dset[j,xvar1],source=dset[j,xvar],
                            weight=dset[j,yvar]
                        )
                    }
                }
            }
            if (length(levels(as.factor(data[,yvar])))>nrow(data)/2){
                lstSeries[[i]][['showScale']]<-T
                lstSeries[[i]][['showScaleText']]<-T
            }else{
                lstSeries[[i]][['showScale']]<-F
            }
        }
        # lstLegend <- list(show=TRUE,
        #                   x=vecPos(pos[['legend']])[1],
        #                   y=vecPos(pos[['legend']])[2],
        #                   orient=vecPos(pos[['legend']])[3])
        if (is.null(series)){
            lvlseries <- as.vector(unique(x1))
            #lstLegend[['data']] <- list(unique(x1))
        }else{
            #lstLegend[['data']] <- c(as.vector(unique(x1)),'',lvlseries)
        }
    }else if (type[1]=='force'){  # force chart
        lstSeries <- list()
        lstSeries[[1]] <- list(type='force',ribbonType=FALSE,roam='move')
        for (i in 1:length(lvlseries)){
            lstSeries[[1]][['categories']][[i]] <- list(name=lvlseries[i])
        }
        lstSeries[[1]][['itemStyle']] <-
            list(normal=list(label=list(textStyle=list(color='#333')),
                             nodeStyle=list(brushType='both'),
                             linkStyle=list(type='curve')))
        if (nlevels(as.factor((dtnode[,'name'])))>=nrow(dtnode)/2){
            lstSeries[[1]][['itemStyle']][['normal']][['label']][['show']] <- T
        }
        for (i in 1:nrow(dtnode)){
            lstSeries[[1]][['nodes']][[i]] <- list(
                category=which(lvlseries==dtnode[i,'category'])-1,
                name=as.character(dtnode[i,'name']),
                value=as.numeric(as.character(dtnode[i,'value']))
            )
        }
        for (i in 1:nrow(dtlink)){
            lstSeries[[1]][['links']][[i]] <- list(
                source=dtlink[i,'from'],target=dtlink[i,'to'],
                weight=dtlink[i,'y'],name=dtlink[i,'relation']
            )
        }
    }else{              # the rest charts: bar, column, scatter, hist
        if (is.null(series)){
            lstSeries[[1]] <- list(
                type=type[1],name=ifelse(is.null(xAxis$lab),xvar,xAxis$lab),
                data=as.vector(data[,yvar])
            )
            if (length(data[,yvar])==1){
                lstSeries[[1]][['data']] <- list(data[,yvar])
            }else{
                lstSeries[[1]][['data']] <- data[,yvar]
            }
            if (type[1]=='histogram'){
                lstSeries[[1]][['type']] <- 'bar'
                lstSeries[[1]][['data']] <- as.matrix(data[,c(xvar,yvar,xvar1)])
                lstSeries[[1]][['barGap']] <- 1
                lstSeries[[1]][['barWidth']] <-
                    ((dev.size('px')[1]-160)*
                         (max(x)-min(x))/valRange)/nrow(data)
                lstSeries[[1]][['itemStyle']]<-list(
                    normal=list(barBorderWidth=1))
            }
        }else{
            for (i in 1:ifelse(is.null(series),1,length(lvlseries))){
                lstSeries[[i]] <- list(
                    name=as.vector(lvlseries[i]),
                    type=type[1]
                )
                if (length(data[data[,svar]==lvlseries[i],yvar])==1){
                    lstSeries[[i]]$data <- list(data[data[,svar]==lvlseries[i],yvar])
                }else{
                    lstSeries[[i]]$data <- data[data[,svar]==lvlseries[i],yvar]
                }
                if (stack){
                    lstSeries[[i]][['stack']] <- 'Stack'
                }
                if (type[1]=='histogram'){
                    lstSeries[[i]][['type']] <- 'bar'
                    lstSeries[[i]][['data']] <-
                        data[data[,svar]==lvlseries[i],c(xvar,yvar,xvar1)]
                    lstSeries[[i]][['barGap']] <- 1
                    lstSeries[[i]][['barWidth']] <-
                        ((dev.size('px')[1]-160)*
                             (max(x)-min(x))/valRange)/nrow(data)
                    lstSeries[[i]][['itemStyle']]<-list(
                        normal=list(barBorderWidth=1))
                }
            }
        }
    }

        #-------markLine-----------------
        if (!is.null(MarkLine)){
            if (!is.null(z)) if (ncol(MarkLine) %in% c(5,9))
                markLine <- matrix(MarkLine[which(as.character(MarkLine[,ncol(MarkLine)])
                                                  == as.character(timeslice[t])),
                                            1:(ncol(MarkLine)-1)],ncol=ncol(MarkLine)-1)
            if (! is.data.frame(markLine) & ! is.matrix(markLine)){
                stop("markLine should be a data.frame or a matrix.")
                if (!ncol(markLine) %in% c(4,8)) {
                    stop("markLine should be of 4 or 8 columns")
                }
            }
            markLine <- as.data.frame(markLine,stringsAsFactors=FALSE)
            if (nrow(markLine)>0){
                if (ncol(markLine)==8){
                    markLine[,6] <- gsub("^[Mm][Aa][Xx].*$",
                                         ifelse(is.numeric(x),max(Data[,xvar]*2,na.rm=TRUE),
                                                length(unique(Data[,xvar]))+1),
                                         markLine[,6])
                    for (col in 3:7) markLine[,col]<-as.numeric(markLine[,col])
                }
                sermarkLine <- data.frame(name=unique(markLine[,1]),
                                          ser=NA)
                names(sermarkLine) <- c(names(markLine)[1],'ser')
                for (i in 1:nrow(sermarkLine)){
                    # locate the index of lstseries to update markline
                    if (!is.na(as.numeric(as.character(sermarkLine[i,1])))){ # series is index
                        if (as.numeric(as.character(sermarkLine[i,1])) <=
                            ifelse(is.null(series),1,length(lvlseries))){
                            sermarkLine[i,2] <-
                                ifelse(is.null(series),1,as.numeric(as.character(sermarkLine[i,1])))
                        }
                    }else{ #series is char
                        if (!is.null(lvlseries)){
                            if (sermarkLine[i,1] %in% lvlseries){
                                sermarkLine[i,2] <- which(lvlseries==sermarkLine[i,1])
                            }
                        }
                        for (hor in 1:length(lstSeries)){
                            if (sermarkLine[i,1]==ifelse(is.null(lstSeries[[hor]][['name']]),
                                                         "",lstSeries[[hor]][['name']])){
                                sermarkLine[i,2] <- hor
                            }
                        }
                    }
                    if (is.na(sermarkLine[i,2])){ # new markLine series
                        sermarkLine[i,2] <- length(lstSeries)+1
                        lstSeries[[sermarkLine[i,2]]] <- list(
                            name=as.character(sermarkLine[i,1]),
                            type="line",symbol='none',
                            itemStyle=list(normal=list(lineStyle=list(type='none'))),
                            data=vector(mode='numeric')
                        )
                        if (ncol(markLine)==8){
                            if (lstXAxis[['type']] == 'category'){
                                lstSeries[[sermarkLine[i,2]]][['data']]<-
                                    rep(markLine[markLine[,1]==
                                                     sermarkLine[i,1],3],
                                        ifelse(length(lstXAxis[['data']])==1,2,
                                               length(lstXAxis[['data']])))
                            }else{
                                lstSeries[[sermarkLine[i,2]]][['data']]<-
                                    as.matrix(markLine[markLine[,1]==
                                                           sermarkLine[i,1],c(5,7)],
                                              dimnames=FALSE)
                            }
                        }
                        #lstLegend[['data']][[sermarkLine[i,2]]] <- markLine[i,1]
                        if (type[1]=='map'){
                            lstSeries[[sermarkLine[i,2]]][['hoverable']] <- FALSE
                            lstSeries[[sermarkLine[i,2]]][['type']] <- type[1]
                            lstSeries[[sermarkLine[i,2]]][['mapType']] <- type[2]
                        }
                    }
                }
                markLine <- plyr::join(markLine,sermarkLine,by=names(markLine)[1])
                for (i in 1:nrow(sermarkLine)){
                    if (type[1]=='map'){
                        lstSeries[[sermarkLine[i,2]]][['markLine']] <- list(
                            data=list(),
                            itemStyle=list(normal=list(borderWidth=1,
                                                       lineStyle=list(type='solid',shadowBlur=10))
                            )
                        )
                    }
                    if (!is.null(markLinesmooth)){
                        lstSeries[[sermarkLine[i,2]]][['markLine']][['smooth']] <- TRUE
                    }
                }
                for (i in 1:nrow(markLine)){  # loop over markLine
                    if (ncol(markLine) %in% c(9)){ # full form
                        serIdx <- markLine[i,9]
                        if (serIdx==1 || serIdx<=length(lstSeries)){
                            nLines <- length(lstSeries[[serIdx]][['markLine']][['data']])
                            lstSeries[[serIdx]][['markLine']][['data']][[nLines+1]] <-
                                list(list(name=ifelse(is.na(markLine[i,2]),
                                                      paste("P(",round(markLine[i,4],2),",",
                                                            round(markLine[i,5],2),")"),
                                                      markLine[i,2]),
                                          value=ifelse(is.na(markLine[i,3]),"-",markLine[i,3]),
                                          x=markLine[i,4],
                                          y=markLine[i,5]),
                                     list(name=ifelse(is.na(markLine[i,2]),
                                                      paste("P(",round(markLine[i,6],2),",",
                                                            round(markLine[i,7],2),")"),
                                                      ""),
                                          x=markLine[i,6],
                                          y=markLine[i,7]))
                            if (type[1] %in% c('line','linesmooth','bar','k','scatter','bubble')){
                                lstSeries[[serIdx]][['markLine']][['data']][[nLines+1]] <-
                                    list(list(name=ifelse(is.na(markLine[i,2]),
                                                          paste("P(",round(markLine[i,4],2),",",
                                                                round(markLine[i,5],2),")"),
                                                          markLine[i,2]),
                                              value=markLine[i,3],
                                              xAxis=markLine[i,4],
                                              yAxis=markLine[i,5]),
                                         list(name=ifelse(is.na(markLine[i,2]),
                                                          paste("P(",round(markLine[i,6],2),",",
                                                                round(markLine[i,7],2),")"),
                                                          ""),
                                              xAxis=markLine[i,6],
                                              yAxis=markLine[i,7]))
                            }else if (type[1]=='map'){
                                geoFrom <- unlist(strsplit(as.character(markLine[i,2]),"[/|]",perl=TRUE))[1]
                                geoTo <- unlist(strsplit(as.character(markLine[i,2]),"[/|]",perl=TRUE))[2]
                                lstSeries[[serIdx]][['markLine']][['data']][[nLines+1]] <- list(
                                    list(name=geoFrom),
                                    list(name=geoTo)
                                )
                                if (!is.na(markLine[i,3])){
                                    lstSeries[[serIdx]][['markLine']][['data']][[nLines+1]][[2]][['value']]<-
                                        markLine[i,3]
                                }
                                lstSeries[[serIdx]][['geoCoord']][[geoFrom]] <-
                                    c(markLine[i,5],markLine[i,4])
                                lstSeries[[serIdx]][['geoCoord']][[geoTo]]<-
                                    c(markLine[i,7],markLine[i,6])
                            }else{

                            }
                        }
                        if (markLine[i,8]==TRUE) { # effect
                            lstSeries[[serIdx]][['markLine']][['effect']] <-
                                list(show=TRUE, period=30, shadowBlur=10)
                        }
                    }else if (ncol(markLine) %in% c(5)){  # short form
                        serIdx <- markLine[i,5]
                        if (type[1] %in% c('line','linesmooth','bar','scatter','bubble')){
                            if (tolower(markLine[i,3]) %in% c('min','max','average')){
                                if (serIdx==1 | serIdx<=length(lvlseries)){
                                    nLines <- length(lstSeries[[serIdx]][['markLine']][['data']])
                                    lstSeries[[serIdx]][['markLine']][['data']][[nLines+1]] <-
                                        list(name=ifelse(is.na(markLine[i,2]),
                                                         tolower(as.character(markLine[i,3])),
                                                         as.character(markLine[i,2])),
                                             type=tolower(as.character(markLine[i,3])))
                                }
                            }else if (type[1] %in% c('bubble','scatter') &
                                      tolower(markLine[i,3]) == 'lm'){
                                nLines <- length(lstSeries[[serIdx]][['markLine']][['data']])

                                if (is.null(series)){
                                    lmfit <- lm(as.formula(paste(yvar,'~',xvar)),data)
                                }else{
                                    dset <- subset(data,data[,svar]==lvlseries[serIdx])
                                    lmfit <- lm(as.formula(paste(yvar,'~',xvar)),dset)
                                }
                                x1 <- min(data[,xvar])
                                x2 <- max(data[,xvar])
                                xhat <- data.frame(x=c(x1,x2))
                                names(xhat) <- xvar
                                yhat <- predict(lmfit,newdata=xhat)
                                k <- lmfit$coefficients[[2]]
                                lstSeries[[serIdx]][['markLine']][['data']][[nLines+1]] <-
                                    list(list(name=ifelse(is.na(markLine[i,2]),
                                                          paste("P(",round(x1,2),",",
                                                                round(yhat[[1]],2),")"),
                                                          markLine[i,2]),
                                              value=ifelse(is.na(k),"-",round(k,2)),
                                              xAxis=x1,
                                              yAxis=yhat[[1]]),
                                         list(name=ifelse(is.na(markLine[i,2]),
                                                          paste("P(",round(x2,2),",",
                                                                round(yhat[[2]],2),"), slope"),
                                                          ""),
                                              xAxis=x2,
                                              yAxis=yhat[[2]]))
                            }
                            if (markLine[i,4]==TRUE) { # effect
                                lstSeries[[serIdx]][['markLine']][['effect']] <-
                                    list(show=TRUE, period=30)
                            }
                        }
                    }
                }
            }
        }

        #-------markPoint-----------------
        if (!is.null(markPoint)){
            if (!is.null(z)) if (ncol(markPoint) %in% c(5,7))
                markPoint <- as.matrix(MarkPoint[which(as.character(MarkPoint[,ncol(MarkPoint)])
                                                       == as.character(timeslice[t])),
                                                 1:(ncol(MarkPoint)-1)],ncol=ncol(MarkPoint)-1)

            if (! is.data.frame(markPoint) & ! is.matrix(markPoint)){
                stop("markPoint should be a data.frame or a matrix.")
                if (!ncol(markPoint) %in% c(4,6)) {
                    stop("markPoint should be of 4 or 6 columns")
                }else if(ncol(markPoint)==6){
                    for (col in 3:5) markPoint[,col]<-as.numeric(markPoint[,col])
                }
            }
            markPoint <- as.data.frame(markPoint,stringsAsFactors=FALSE)
            sermarkPoint <- data.frame(name=levels(as.factor(markPoint[,1])),
                                       ser=NA)
            names(sermarkPoint) <- c(names(markPoint)[1],'ser')
            for (i in 1:nrow(sermarkPoint)){
                # locate the index of lstseries to update markPoint
                if (!is.na(as.numeric(as.character(sermarkPoint[i,1])))){  # series index
                    if (as.numeric(as.character(sermarkPoint[i,1])) <=
                        ifelse(is.null(series),1,length(lvlseries))){
                        sermarkPoint[i,2] <-
                            ifelse(is.null(series),1,as.numeric(as.character(sermarkPoint[i,1])))
                    }
                }else{
                    if (!is.null(lvlseries)){
                        if (sermarkPoint[i,1] %in% lvlseries){
                            sermarkPoint[i,2] <- which(lvlseries==sermarkPoint[i,1])
                        }
                    }
                    for (hor in 1:length(lstSeries)){
                        if (sermarkPoint[i,1]==ifelse(is.null(lstSeries[[hor]][['name']]),
                                                      "",lstSeries[[hor]][['name']])){
                            sermarkPoint[i,2] <- hor
                        }
                    }
                }
                if (is.na(sermarkPoint[i,2])){ #new markPoint series
                    sermarkPoint[i,2] <- length(lstSeries)+1
                    lstSeries[[sermarkPoint[i,2]]] <- list(
                        name=as.character(sermarkPoint[i,1]),
                        type="scatter",
                        data=vector(mode="numeric")
                    )
                    #lstLegend[['data']][[sermarkPoint[i,2]]] <- markPoint[i,1]
                }
                if (type[1]=='map'){
                    lstSeries[[sermarkPoint[i,2]]][['type']] <- "map"
                    lstSeries[[sermarkPoint[i,2]]][['mapType']] <- type[2]
                    lstSeries[[sermarkPoint[i,2]]][['markPoint']] <- list(
                        symbol='emptyCircle',
                        itemStyle=list(normal=list(label=list(show=FALSE))),
                        data=list()
                    )
                }
            }
            markPoint <- plyr::join(markPoint,sermarkPoint,by=names(markPoint)[1])

            for (i in 1:nrow(markPoint)){  # loop over markPoint
                if (ncol(markPoint) %in% c(7)){ # full form
                    serIdx <- markPoint[i,7]
                    if (serIdx==1 | serIdx<=length(lstSeries)){
                        nPoints <- length(lstSeries[[serIdx]][['markPoint']][['data']])
                        if (!is.na(markPoint[i,3])){
                            lstSeries[[serIdx]][['markPoint']][['data']][[nPoints+1]] <-
                                list(name=ifelse(is.na(markPoint[i,2]),
                                                 paste("P(",round(markPoint[i,4],2),",",
                                                       round(markPoint[i,5],2),")"),
                                                 markPoint[i,2]),
                                     value=ifelse(is.numeric(markPoint[i,3]),
                                                  round(as.numeric(markPoint[i,3]),2),
                                                  markPoint[i,3]),
                                     x=markPoint[i,4],
                                     y=markPoint[i,5])
                        }
                        if (type[1] %in% c('line','linesmooth','bar','k','scatter')){
                            lstSeries[[serIdx]][['markPoint']][['data']][[nPoints+1]] <-
                                list(name=ifelse(is.na(markPoint[i,2]),
                                                 paste("P(",round(markPoint[i,4],2),",",
                                                       round(markPoint[i,5],2),")"),
                                                 markPoint[i,2]),
                                     value=ifelse(is.na(markPoint[i,3]),"-",
                                                  round(as.numeric(markPoint[i,3]),2)),
                                     xAxis=markPoint[i,4],
                                     yAxis=markPoint[i,5])
                        }else if (type[1] =='map'){
                            if (!is.na(markPoint[i,3])){
                                lstSeries[[serIdx]][['markPoint']][['data']][[nPoints+1]] <-
                                    list(name=markPoint[i,2],
                                         value=ifelse(is.numeric(markPoint[i,3]),
                                                      round(as.numeric(markPoint[i,3]),2),
                                                      markPoint[i,3]))
                            }
                            if (!any(is.na(markPoint[i,4:5]))){
                                lstSeries[[1]][['geoCoord']][[markPoint[i,2]]] <-
                                    c(markPoint[i,5],markPoint[i,4])
                            }  # all geoCoords append to series 1
                        }
                    }

                    if (markPoint[i,6]==TRUE) { # effect
                        lstSeries[[serIdx]][['markPoint']][['effect']] <-
                            list(show=TRUE, shadowBlur=0)
                    }

                }else if (ncol(markPoint) %in% c(5)){  # short form
                    serIdx <- markPoint[i,5]
                    if (type[1] %in% c('line','linesmooth','bar','scatter','bubble')){
                        if (tolower(markPoint[i,2]) %in% c('min','max')){
                            if (serIdx==1 | serIdx<=length(lvlseries)){
                                nPoints <- length(lstSeries[[serIdx]][['markPoint']][['data']])
                                lstSeries[[serIdx]][['markPoint']][['data']][[nPoints+1]] <-
                                    list(name=ifelse(is.na(markPoint[i,2]),
                                                     tolower(markPoint[i,3]),
                                                     markPoint[i,2]),
                                         type=tolower(markPoint[i,3]))
                            }
                        }
                        if (markPoint[i,4]==TRUE) { # effect
                            lstSeries[[serIdx]][['markPoint']][['effect']] <- list(show=TRUE)
                        }
                    }
                }
                if (length(markPoint)>1000) {
                    lstSeries[[serIdx]][['markPoint']][['large']] <- T
                }
            }
            if (ncol(markPoint)==7) { #JS func symbolSize, max 20, min 10
                sizeFold <- 10/(1+diff(range(markPoint[,3])))
                for (i in 1:nrow(sermarkPoint)){
                    lstSeries[[sermarkPoint[i,2]]][['markPoint']][['symbolSize']] <-
                        JS('function (value) {
                           return 10+(value-',min(markPoint[,3]),')*',
                           sizeFold,'}')
                }
        }
            }

        #-------Make plot-------------
        if (is.null(z)){
            chartobj <- list(
                tooltip=lstTooltip,
                series=lstSeries
            )

            if (!is.null(lstbackgroundColor)) chartobj[['backgroundColor']] <- lstbackgroundColor
            #if (!is.null(lstColor)) chartobj[['color']] <- lstColor
            #if (try(exists("lstGrid"),T)) chartobj[['grid']] <- lstGrid
            #if (!is.null(lstSymbol)) chartobj[['symbolList']] <- lstSymbol
            #if (!is.null(lstdataZoom)) chartobj[['dataZoom']] <- lstdataZoom
            #if (!is.null(lstdataRange)) chartobj[['dataRange']] <- lstdataRange
            #if (!is.null(lstSeries[[1]][['name']]))   chartobj[['legend']] <- lstLegend
            #if (!is.null(lstLegend))   chartobj[['legend']] <- lstLegend
            if (type[1] %in% c('scatter','bubble','line','bar','linesmooth','histogram',
                               'area','areasmooth','k')){
                chartobj[['xAxis']] <- lstXAxis
                chartobj[['yAxis']] <- lstYAxis
            }else if(type[1] %in% c('map')){
                chartobj[['roamController']] <- list(show=TRUE,
                                                     mapTypeControl=list(),
                                                     width=60, height=90,
                                                     x=vecPos(pos[['roam']])[1],
                                                     y=vecPos(pos[['roam']])[2]
                )
                chartobj[['roamController']][['mapTypeControl']][[mapType]] <- T
            }else if (type[1] %in% c('radar','radarfill')){
                chartobj[['polar']] <- lstPolar
            }
        }else{
            if (t==1){ # the 1st timeslice
                chartobj <- list(list(
                    tooltip=lstTooltip,
                    series=lstSeries
                ))

                if (!is.null(lstbackgroundColor)) chartobj[[t]][['backgroundColor']] <-
                        lstbackgroundColor
                #if (!is.null(lstColor)) chartobj[[t]][['color']] <- lstColor
                #if (try(exists("lstGrid"),TRUE)) chartobj[[t]][['grid']] <- lstGrid
                #if (!is.null(lstSymbol)) chartobj[[t]][['symbolList']] <- lstSymbol
                #if (!is.null(lstdataZoom)) chartobj[[t]][['dataZoom']] <- lstdataZoom
                #if (!is.null(lstdataRange)) chartobj[[t]][['dataRange']] <- lstdataRange
                #if (!is.null(lstSeries[[1]][['name']]))   chartobj[[t]][['legend']] <- lstLegend
                #if (!is.null(lstLegend))  chartobj[[t]][['legend']] <- lstLegend
                if (type[1] %in% c('scatter','bubble','line','bar','histogram',
                                   'area')){
                    chartobj[[t]][['xAxis']] <- lstXAxis
                    chartobj[[t]][['yAxis']] <- lstYAxis
                }else if(type[1] %in% c('map')){
                    chartobj[[t]][['roamController']] <-
                        list(show=TRUE,
                             mapTypeControl=list(),
                             width=60, height=90,
                             x=vecPos(pos[['roam']])[1],
                             y=vecPos(pos[['roam']])[2]
                        )
                    chartobj[[t]][['roamController']][['mapTypeControl']][[mapType]] <- TRUE
                }else if (type[1] %in% c('radar','radarfill')){
                    chartobj[[t]][['polar']] <- lstPolar
                }
            }else if (t>1){
                chartobj[[t]] <- list(series=lstSeries)
            }
        }
        }# loop end over z
    #----------Finally plot it---------
    if (!is.null(z)) {
        output <- echart(list(timeline=lstTimeline, options=chartobj))
    }else{
        output <- echart(chartobj)
    }
    output <- output %>% setToolbox() %>% setLegend(show=TRUE)
    if (!is.null(theme$width)) if (is.numeric(theme$width)) output$width <- theme$width
    if (!is.null(theme$height)) if (is.numeric(theme$height)) output$height <- theme$height
    if (all(is.na(Data[,vArgs$y]))) return('') else return(output)
}

