#' Create an ECharts widget
#'
#' Create an HTML widget for ECharts that can be rendered in the R console, R
#' Markdown documents, or Shiny apps. You can add more components to this widget
#' and customize options later. \code{eChart()} is an alias of \code{echart()}.
#' @param data a data object (usually a data frame or a list)
#' @rdname eChart
#' @export
#' @examples library(recharts)
#' echart(iris, ~ Sepal.Length, ~ Sepal.Width)
#' echart(iris, ~ Sepal.Length, ~ Sepal.Width, series = ~ Species)
echart = function(data, ...) {
    UseMethod('echart')
}

#' Create an ECharts widget
#'
#' echart method for list
#' @param width width
#' @param height height
#' @export
#' @rdname eChart
echart.list = function(data, width = NULL, height = NULL,  ...) {
    htmlwidgets::createWidget(
        'echarts', x = data, width = width, height = height,
        package = 'recharts'
    )
}

#' Create an ECharts widget
#'
#' echart method for data.frame
#' @param x the x variable
#' @param y the y variable
#' @param series series
#' @param type type, default 'auto'
#' @export
#'
#' @rdname eChart
echart.data.frame = function(
    data = NULL, x = NULL, y = NULL, series = NULL, type = 'auto',
    width = NULL, height = NULL, ...
) {

    xlab = autoArgLabel(x, deparse(substitute(x)))
    ylab = autoArgLabel(y, deparse(substitute(y)))

    x = evalFormula(x, data)
    y = evalFormula(y, data)
    if (type == 'auto') type = determineType(x, y)
    if (type == 'bar') {
        x = as.factor(x)
        if (is.null(y)) ylab = 'Frequency'
    }

    series = evalFormula(series, data)
    data_fun = getFromNamespace(paste0('data_', type), 'recharts')

    params = structure(list(
        series = data_fun(x, y, series),
        xAxis = list(), yAxis = list()
    ), meta = list(
        x = x, y = y
    ))

    if (!is.null(series)) {
        params$legend = list(data = levels(as.factor(series)))
    }

    chart = htmlwidgets::createWidget(
        'echarts', x = params, width = width, height = height,
        package = 'recharts', dependencies = getDependency(type)
    )

    chart %>% eAxis('x', name = xlab) %>% eAxis('y', name = ylab)

}

#' @export
#' @rdname eChart
echart.default = echart.data.frame

#' @export
#' @rdname eChart
eChart = echart
# from the planet of "Duo1 Qiao1 Yi1 Ge4 Jian4 Hui4 Si3" (will die if having to
# press one more key, i.e. Shift in this case)


#' @export
echartr = function(
    data, x = NULL, y = NULL, z = NULL, series = NULL, weight = NULL,
    lat = NULL, lng = NULL, type = 'auto', ...
) {
    # experimental function
    #------------- get all arguments as a list-----------------
    vArgs <- as.list(match.call(expand.dots=TRUE))
    dataVars <- intersect(names(vArgs),
                          c('x', 'y', 'z', 'series', 'weight', 'lat', 'lng'))
    vArgs <- vArgs[dataVars]

    # ------------extract var names and values-----------------
    xvar = yvar = NULL
    eval(parse(text=paste0(names(vArgs), "var <- evalVarArg(",
                           sapply(vArgs, deparse), ", data, eval=FALSE)")))
    eval(parse(text=paste0(names(vArgs), " <- evalVarArg(",
                           sapply(vArgs, deparse), ", data)")))
    hasZ <- ! is.null(z)

    # ------------------x, y lab(s)----------------------------
    xlab = sapply(xvar, autoArgLabel, auto=deparse(substitute(xvar)))
    ylab = sapply(yvar, autoArgLabel, auto=deparse(substitute(yvar)))
    if (length(ylab) == 0) ylab = "Freq"

    # -------------split multi-timeline df to lists-----------

    .makeMetaDataList <- function(df) {
        # assignment <- paste0(dataVars, " = ", substitute(df, parent.frame()),
        #                      "[ ,", paste0(dataVars, "var"), ", drop=FALSE]")
        vars <- sapply(dataVars, function(x) {
            eval(parse(text=paste0(x, 'var')))}, simplify=TRUE)
        assignment <- paste0(dataVars, " = evalVarArg(", vars, ", ",
                            substitute(df, parent.frame()), ")")
        eval(parse(text=paste0("list(", paste(assignment, collapse=", "), ")")))
    }
    if (hasZ){
        uniZ <- unique(z[,1])
        if (is.factor(uniZ)) uniZ <- as.character(uniZ)
        dataByZ <- split(data, as.factor(z[,1]))
        metaData <- lapply(dataByZ, .makeMetaDataList)
        names(metaData) <- uniZ
        if (! identical(unique(z[,1]), sort(unique(z[,1]))) &&
            ! identical(unique(z[,1]), sort(unique(z[,1]), TRUE)))
            warning("z is not in order, the chart may not show properly!")
    }else{
        metaData <- .makeMetaDataList(data)
    }

    # -----------------determine types---------------------------
    type <- tolower(type)
    stopifnot(any(sapply(c('auto', validChartTypes$name), grepl, x=type)))
    if (!is.null(series)) lvlSeries <- levels(as.factor(series[,1]))
    if (!is.null(series)) nSeries <- length(lvlSeries) else nSeries <- 1
    if (type[1] == 'auto')  type = determineType(x[,1], y[,1])

    ## type vector: one series, one type
    if (length(type) >= nSeries){
        type <- type[1:nSeries]
    }else{
        type <- c(type, rep(type[length(type)], nSeries-length(type)))
    }

    ## type is converted to a data.frame, colnames:
    ## name type stack xyflip smooth fill sort ribbon roseType mapType mapMode
    dfType <- t(sapply(validChartTypes$name, function(x) grepl(x, type)))
    typeIdx <- sapply(seq_len(ncol(dfType)), function(i) which(dfType[,i]))
    dfType <- validChartTypes[typeIdx,]

    ## check types
    if (nlevels(as.factor(dfType$type)) > 1){
        if (!all(grepl("^(line|bar|scatter|k)", dfType$type)))
            stop(paste("Only Cartesion Coordinates charts (scatter/point/bubble,",
                       "line, area, bar, k) support mixed types"))
    }
    # if (nlevels(as.factor(dfType$xyflip)) > 1)
    #     warning(paste("xyflip is not consistent across the types given.\n",
    #                   dfType[, "xyflip"]))

    # ---------------------------params list----------------------
    .makeSeriesList <- function(z){  # each timeline create a options list
        series_fun = getFromNamespace(paste0('series_', dfType$type[1]),
                                    'recharts')
        if (is.null(z)){  # no timeline
            out <- structure(list(
                series = series_fun(metaData, type=dfType)
            ), meta = metaData)
        }else{
            out <- structure(list(
                series = series_fun(metaData[[z]], type=dfType)
            ), meta = metaData[[z]])
        }

        return(out)
    }

    if (hasZ){  ## has timeline
        params = list(
            timeline=list(),
            options=lapply(1:length(uniZ), .makeSeriesList)
        )
        if (!is.null(series))
            params$options[[1]]$legend <- list(
                data = as.list(levels(as.factor(series[,1])))
            )
    }else{
        params = .makeSeriesList(NULL)
        if (!is.null(series))
            params$legend <- list(
                data = as.list(levels(as.factor(series[,1])))
            )
    }

    # -------------------output-------------------------------
    chart = htmlwidgets::createWidget(
        'echarts', params, width = NULL, height = NULL, package = 'recharts',
        dependencies = lapply(c('base', unique(dfType$type)), getDependency)
    )

    if (hasZ){
        chart <- chart %>% setTimeline(show=TRUE, y2=50, data=uniZ)
    }



    if (any(dfType$type %in% c('line', 'bar', 'scatter', 'k'))){
        chart <- chart %>% setXAxis(name = xlab[[1]]) %>%
            setYAxis(name = ylab[[1]]) %>%
            setTooltip() %>% setToolbox() %>% setLegend() %>%
            flipAxis(flip=any(dfType$xyflip))
    }else{
        chart <- chart %>% setTooltip() %>% setToolbox() %>% setLegend()
    }
}


determineType = function(x, y) {
    if (is.numeric(x) && is.numeric(y)) return('scatter')
    # when y is numeric, plot y against x; when y is NULL, treat x as a
    # categorical variable, and plot its frequencies
    if ((is.factor(x) || is.character(x)) && (is.numeric(y) || is.null(y)))
        return('bar')
    if (is.null(x) && is.ts(y)) return('line')
    # FIXME: 'histogram' is not a standard plot type of ECharts
    # http://echarts.baidu.com/doc/doc.html
    if ((is.numeric(x) && is.null(y)) || (is.numeric(y) && is.null(x)))
        return('histogram')
    message('The structure of x:')
    str(x)
    message('The structure of y:')
    str(y)
    stop('Unable to determine the chart type from x and y automatically')
}

# not usable yet; see https://github.com/ecomfe/echarts/issues/1065
getDependency = function(type) {
    if (is.null(type)) return()
    htmltools::htmlDependency(
        'echarts-module', EChartsVersion,
        src = system.file('htmlwidgets/lib/echarts', package = 'recharts'),
        script = sprintf('%s.js', type)
    )
}

getMeta = function(obj) {
    if (inherits(obj, "echarts"))
        attr(obj$x, 'meta', exact = TRUE)
    else
        attr(obj, "meta", exact = TRUE)
}
