#' @importFrom data.table melt
series_scatter <- function(lst, type, return=NULL, ...){
    lst <- mergeList(list(weight=NULL, series=NULL), lst)
    data <- cbind(lst$y[,1], lst$x[,1])

    if (!is.null(lst$weight)){  # weight as symbolSize
        minWeight <- min(lst$weight[,1], na.rm=TRUE)
        range <- max(lst$weight[,1], na.rm=TRUE) - minWeight
        jsSymbolSize <- JS(paste('function (value){
                return Math.round(1+9*(value[2]-', minWeight,')/', range, ');
                }'))
    }
    obj <- list()
    if (is.null(lst$series)) {
        if (is.null(lst$weight))
            obj <- list(list(type=type[1], data=data))
        else
            obj <- list(list(type=type[1], data=data, symbolSize=jsSymbolSize))
    }else{
        data <- cbind(data, lst$series[,1])
        data = split(as.data.frame(data), lst$series[,1])
        if (is.null(lst$weight)){
            for (i in seq_along(data)) {
                obj[[i]] = list(name = names(data)[i], type = type[i],
                                data = unname(as.matrix(data[[i]])))
            }
        }else{
            for (i in seq_along(data)) {
                obj[[i]] = list(name = names(data)[i], type = type[i],
                                data = unname(as.matrix(data[[i]])),
                                symbolSize=jsSymbolSize)
            }
        }
    }

    if (is.null(return)){
        return(obj)
    }else{
        return(obj[intersect(names(obj), return)])
    }
}




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
