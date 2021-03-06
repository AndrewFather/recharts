#' @importFrom data.table melt
series_scatter <- function(lst, type, return=NULL, ...){
    lst <- mergeList(list(weight=NULL, series=NULL), lst)
    data <- cbind(lst$y[,1], lst$x[,1])

    if (!is.null(lst$weight)){  # weight as symbolSize
        data <- cbind(data, lst$weight[,1])
        minWeight <- min(abs(lst$weight[,1]), na.rm=TRUE)
        maxWeight <- max(abs(lst$weight[,1]), na.rm=TRUE)
        range <- maxWeight - minWeight
        folds <- maxWeight / minWeight
        if (abs(folds) < 50){  # max/min < 50, linear
            jsSymbolSize <- JS(paste0('function (value){
                return ', ifelse(abs(folds) < 5, 4, ifelse(abs(folds) < 10, 2, 1)),
                '*Math.round(Math.abs(value[2]/', minWeight,'));
                }'))
        }else{  # max/min >= 50, normalize
            jsSymbolSize <- JS(paste0('function (value){
                return Math.round(1+29*(Math.abs(value[2])-', minWeight,')/', range, ');
            }'))
        }
    }
    obj <- list()
    if (is.null(lst$series)) {  # no series
        if (is.null(lst$weight))
            obj <- list(list(type=type$type[1], data=data[,1:2]))
        else
            obj <- list(list(type=type$type[1], data=data[,1:3],
                             symbolSize=jsSymbolSize))
    }else{  # series-specific
        data <- cbind(data, lst$series[,1])
        data <- split(as.data.frame(data), lst$series[,1])
        if (is.null(lst$weight)){
            obj <- lapply(seq_along(data), function(i){
                list(name = names(data)[i], type = type$type[i],
                     data = unname(as.matrix(data[[i]])[,1:2]),
                     large = nrow(data) > 2000) ## > 2000 points, large = TRUE
            })  ## only fetch col 1-2 of data, col 3 is series
        }else{
            obj <- lapply(seq_along(data), function(i){
                list(name = names(data)[i], type = type$type[i],
                     data = unname(as.matrix(data[[i]])[,c(1:3)]),
                     symbolSize=jsSymbolSize, large = nrow(data) > 2000)
            })  ## fetch col 1-2 and 3 (x, y, weight)
        }
    }

    if (is.null(return)){
        return(obj)
    }else{
        return(obj[intersect(names(obj), return)])
    }
}

series_bar <- function(lst, type, return=NULL, ...){
    # example:
    # echartr(mtcars, hp, mpg, series=factor(am,labels=c('Manual','Automatic')),
    #               type=c('hbar','scatter'))
    lst <- mergeList(list(series=NULL), lst)
    data <- cbind(lst$y[,1], lst$x[,1])
    if (!'y' %in% names(lst)) {  # y is null, freq of x
        if (is.numeric(data[,1])){
            data <- unname(as.matrix(as.data.frame(table(data[,1]))))
        }else{
            data <- unname(as.matrix(table(data[,1])))
        }
    }

    obj <- list()
    if (is.null(lst$series)) {  # no series
        if (is.numeric(lst$x[,1])){
            obj <- list(list(type=type$type[1], data=data[,1:2]))
            if (any(type$xyflip)) obj[[1]]$barHeight=10
        }else{
            obj <- list(list(type=type$type[1], data=data[,1]))
        }
    }else{  # series-specific
        dataCross <- tapply(data[,1], list(1:nrow(data), lst$series[,1]),
                            function(x) x)
        rownames(dataCross) <- data[,2]
        data <- dataCross
        data[is.na(data)] = 'nan'
        obj <- lapply(seq_len(ncol(data)), function(i){
            if (is.numeric(lst$x[,1])){
                o = list(name = colnames(data)[i], type = type$type[i],
                         data = unname(cbind(as.numeric(rownames(data)),
                                                        data[,i])))
                if (any(type$xyflip))
                    o <- mergeList(o, list(barHeight=10))
            }else{
                o = list(name = colnames(data)[i], type = type$type[i],
                         data = unname(data[,i]))
            }
            if (type$stack[i]) o[['stack']] = 'stack'
            return(o)
        })
    }

    if (is.null(return)){
        return(obj)
    }else{
        return(obj[intersect(names(obj), return)])
    }
}

series_line = function(lst, type, return=NULL, ...) {
    lst <- mergeList(list(series=NULL), lst)
    data <- cbind(lst$y[,1], lst$x[,1])

    if (is.null(lst$x[,1]) && is.ts(lst$y[,1])) {
        lst$x[,1] = as.numeric(time(lst$y[,1]))
        lst$y[,1] = as.numeric(lst$y[,1])
    }
    if (is.numeric(lst$x[,1])) {
        obj = series_scatter(lst, type = type)
    }
    if (is.null(series)) {
        obj = list(list(type = 'line', data = lst$y[,1]))
    }
    if (is.null(obj)) obj = series_bar(lst, type = type)

    if (is.null(return)){
        return(obj)
    }else{
        return(obj[intersect(names(obj), return)])
    }

}


#---------------------------legacy functions-----------------------------------
# split the data matrix for a scatterplot by series
data_scatter = function(x, y, series = NULL, type = 'scatter') {
  xy = unname(cbind(x, y))
  if (is.null(series)) return(list(list(type = type, data = xy)))
  xy = split(as.data.frame(xy), series)
  nms = names(xy)
  obj = list()
  for (i in seq_along(xy)) {
    obj[[i]] = list(name = nms[i], type = type, data = unname(as.matrix(xy[[i]])))
  }
  obj
}

data_bar = function(x, y, series = NULL, type = 'bar') {

  # plot the frequencies of x when y is not provided
  if (is.null(y)) {

    if (is.null(series)) {
      y = table(x)
      return(list(list(type = type, data = unname(c(y)))))
    }

    y = table(x, series)
    nms = colnames(y)
    obj = list()
    for (i in seq_len(ncol(y))) {
      obj[[i]] = list(name = nms[i], type = type, data = unname(y[, i]))
    }
    return(obj)

  }

  # when y is provided, use y as the height of bars
  if (is.null(series)) {
    return(list(list(type = type, data = y)))
  }

  xy = tapply(y, list(x, series), function(z) {
    if (length(z) == 1) return(z)
    stop('y must only have one value corresponding to each combination of x and series')
  })
  xy[is.na(xy)] = 0
  nms = colnames(xy)
  obj = list()
  for (i in seq_len(ncol(xy))) {
    obj[[i]] = list(name = nms[i], type = type, data = unname(xy[, i]))
  }
  obj

}

data_line = function(x, y, series = NULL) {
  if (is.null(x) && is.ts(y)) {
    x = as.numeric(time(y))
    y = as.numeric(y)
  }
  if (is.numeric(x)) {
    return(data_scatter(x, y, series, type = 'line'))
  }
  if (is.null(series)) {
    return(list(list(type = 'line', data = y)))
  }
  data_bar(x, y, series, type = 'line')
}
