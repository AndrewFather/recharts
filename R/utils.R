# make sure htmlwidgets:::toJSON() turns list() to {} in JSON, instead of []
.emptyList = setNames(list(), character())
emptyList = function() .emptyList

# evaluate a formula using `data` as the environment, e.g. evalFormula(~ z + 1,
# data = data.frame(z = 1:10))
evalFormula = function(x, data) {
  if (!inherits(x, 'formula')) return(x)
  if (length(x) != 2) stop('The formula must be one-sided: ', deparse(x))
  eval(x[[2]], data, environment(x))
}

# merge two lists by names, e.g. x = list(a = 1, b = 2), mergeList(x, list(b =
# 3)) => list(a = 1, b = 3)
mergeList = function(x, y) {
  if (!is.list(y) || length(y) == 0) return(x)
  yn = names(y)
  if (length(yn) == 0 || any(yn == '')) {
    warning('The second list to be merged into the first must be named')
    return(x)
  }
  for (i in yn) {
    xi = x[[i]]
    yi = y[[i]]
    if (is.list(xi)) {
      if (is.list(yi)) x[[i]] = mergeList(xi, yi)
    } else x[[i]] = yi
  }
  x
}

# automatic labels from function arguments
autoArgLabel = function(arg, auto) {
  if (is.null(arg)) return('')
  if (inherits(arg, 'formula')) return(deparse(arg[[2]]))
  auto
}


# -------------Lazy functions to judge class-------------------
isDate <- function(x, format=NULL){
    if (!is.null(format)){
        if (!is(try(as.Date(x),TRUE),"try-error")) TRUE else FALSE
    }else{
        if (!is(try(as.Date(x,format=format),TRUE),"try-error")) TRUE else FALSE
    }
}
isTime <- function(x,origin=NULL,tz='CST'){
    if (is.null(origin)){
        return(FALSE)
    }else{
        if (!is(try(as.POSIXct,T),"try-error")) T else F
    }
}
isLatin <- function(x){
    if (is.factor(x)) x <- as.character(x)
    return(all(grepl("^[[:alnum:][:space:][:punct:]]+$",x,perl=TRUE)))
}

#--------Other functions for position, color, HTML table conversion------------

#' Get A String Containing 'rgba' Function
#'
#' Echarts uses rgba function heavily. You can convert color vectors into rgba function
#' in string form to pass to an echarts object.
#' @param vecrgb A vector of RGB elements, or simply red int.
#' @param ... If vecrgb is simply red int, you can pass green, blue, alpha int here.
#'
#' @return A character string. E.g, 'rgba(125, 125, 125, 0.6)' or '#FFFFFF'
#' @export
#'
#' @examples
#' \dontrun{
#' rgba(c(123, 123, 124, 125))  # return 'rgba(123,123,124,0.490196078431373)'
#' rgba(123, 123, 124, 0.5) # return 'rgba(123,123,124,0.5)'
#' rgba(123, 123, 124)  # return '#7B7B7C'
#' }
rgba <- function(vecrgb, ...){
    if (is.matrix(vecrgb) && dim(vecrgb) == c(3,1)) vecrgb <- vecrgb[,1]
    ## vecrgb is yielded from col2rgb()

    if (is.list(vecrgb)) rgb <- as.vector(unlist(vecrgb))
    if (length(vecrgb) == 1) vecrgb <- c(vecrgb, unlist(list(...)))
    if (min(vecrgb,na.rm=TRUE)<0 | max(vecrgb,na.rm=TRUE)>255) {
        stop("All elements should be numeric 0-255!")
    }
    if (length(vecrgb[!is.na(vecrgb)]) == 3){
        return(rgb(red=vecrgb[1], green=vecrgb[2], blue=vecrgb[3], max=255))
    }else if (length(vecrgb[!is.na(vecrgb)])==4){
        #return(rgb(red=vecrgb[1],green=vecrgb[2],blue=vecrgb[3],alpha=vecrgb[4],
        #           max=255))
        return(paste0('rgba(',vecrgb[1],',',vecrgb[2],',',vecrgb[3],',',
                      as.numeric(ifelse(vecrgb[4]<=1, vecrgb[4], vecrgb[4]/255)),
                      ')'))
    }else{
        stop("Must be of length 3 or 4!")
    }
}

#' Text Position and Direction
#'
#' Converts text postion from clock digits to c(x, y, direction) vector
#' @param pos 1-12, clock digits.
#'
#' @return A vector of x-alignment, y-alignment and direction.
#' @export
#'
#' @examples
#' \dontrun{
#' vecPos(2) ## returns c("right", "top", "vertical")
#' }
#' @note
#' # Postion of Clock Numbers 1-12 \cr
#' \tabular{lllll}{
#'  10(l, t, v) \tab 11(l, t, h) \tab 12(c, t, h) \tab 1(r, t, h) \tab 2(r, t, v) \cr
#'  9(l, c, v) \tab \tab \tab \tab 3(r, c, v) \cr
#'  8(l, b, v) \tab 7(l, b, h) \tab 6(c, b, h) \tab 5(r, b, h) \tab 4(r, b, v)
#' }
#'
vecPos <- function(pos){
    TblPos=as.data.frame(rbind(c("right","top","horizontal"),
                               c("right","top","vertical"),
                               c("right","center","vertical"),
                               c("right","bottom","vertical"),
                               c("right","bottom","horizontal"),
                               c("center","bottom","horizontal"),
                               c("left","bottom","horizontal"),
                               c("left","bottom","vertical"),
                               c("left","center","vertical"),
                               c("left","top","vertical"),
                               c("left","top","horizontal"),
                               c("center","top","horizontal")),
                         stringsAsFactors=FALSE)
    names(TblPos) <- c("x","y","z")
    return(as.vector(unlist(TblPos[pos,])))
}

#-------table format-----------
#' Reformat HTML Table
#'
#' Convert a data frame to an HTML table object and reformat it.
#' @param dataset The dataset to draw table.
#' @param heading The heading you want to input.
#'        '|' indicates colspan, '=' indicates rowspan.
#' @param footRows The last several rows as <tfoot>.
#' @param align Alignment of columns.
#' @param concatCol Index of columns to concatenate,
#'        to make the table look hierachical.
#' @param caption Table caption.
#' @param tableWidth Width of the table.
#'
#' @return A table in HTML.
#' @importFrom knitr kable
#' @seealso \code{\link{knitr::kable}}
#'
#' @examples
#' \dontrun{
#' ## A reformatted table with colspan/colrow=2
#' heading <- matrix(c("Sepal", NA, "Petal", NA, "Species", "Length", "Width",
#'                     "Length", "Width", NA), byrow=TRUE, nrow=2)
#' reheadHTMLTable(head(iris), heading)
#' }
reheadHTMLTable <- function(dataset, heading, footRows=0,
                            align=c('left', rep('center', ncol-1)),
                            concatCol=NULL, caption=NULL,
                            tableWidth='100%'){
    if ((!is.null(dataset) & !is.data.frame(dataset)) |
        !(is.data.frame(heading) | is.matrix(heading) | is.vector(heading))){
        stop(paste0('`dataset` must be a data.frame, while you gave a ',
                    class(dataset),
                    '\n`heading` must be a vector/matrix/data.frame, ',
                    'while you gave a ', class(heading),"."))
    }else{
        if (is.vector(heading)) heading <- t(matrix(heading))
        if (!is.null(dataset)){
            ncol <- ncol(dataset)
            if (ncol!=ncol(heading))
                stop(paste("Not equal counts of columns! Dataset has",
                           ncol, "cols, while heading has",ncol(heading), '.'))
        }else{
            ncol <- sub('(^[dhr]+?)[^dhr].+$','\\1',gsub('.+?<t([dhr]).+?',
                                                         '\\1',htmltable))
            ncol <- table(strsplit(ncol,"")[[1]])
            ncol <- floor((ncol[['h']]+ncol[['d']])/ncol[['r']])
        }
        align_simp <- substr(tolower(align),1,1)
        if (!all(align_simp %in% c('l','c','r'))){
            stop('`align` only accepts values of "l(eft)", "c(enter)" and "r(ight)".')
        }else{
            align[align_simp=="l"] <- "left"
            align[align_simp=="c"] <- "center"
            align[align_simp=="r"] <- "right"
        }
        if (length(align) > ncol(heading)){
            align <- align[1:ncol(heading)]
        }else if (length(align)<ncol(heading)){
            align <- c(align[1:length(align)],
                       rep(align[length(align)],ncol(heading)-length(align)))
        }
        align_simp <- substr(tolower(align),1,1)
        # loadPkg('knitr')

        dataset <- as.data.frame(dataset)
        if (!is.null(concatCol)){
            for (icol in concatCol){
                col <- as.character(dataset[,icol])
                lag <- c(NA,as.character(dataset[1:(nrow(dataset)-1),icol]))
                col[col==lag] <- ""
                dataset[,icol] <- col
            }
        }

        if (!(is.null(footRows) | footRows==0)){
            if (footRows>=nrow(dataset))
                stop("footRows cannot be >= number of datatable rows!")
            htmlBody <- knitr::kable(dataset[1:(nrow(dataset)-footRows),],
                                     format='html',align=align_simp,row.names=FALSE)
            htmlFoot <- knitr::kable(dataset[(nrow(dataset)-footRows+1):
                                                 nrow(dataset),],
                                     format='html',align=align_simp,row.names=FALSE)
            htmlBody <- gsub("(^.+</tbody>).+$","\\1",htmlBody)
            htmlFoot <- gsub("^.+<tbody>(.+)</tbody>.+$","<tfoot>\\1</tfoot>",htmlFoot)
            htmltable <- paste0(htmlBody,"\n",htmlFoot,"\n</table>")
        }else{
            htmltable <- knitr::kable(dataset,format='html',
                                      align=align_simp, row.names=FALSE)
        }

        if (!is.null(caption)){
            htmltable <- gsub("<table>",paste0("<table>\n<caption>",caption,"</caption>"),
                              htmltable)
        }
        class(htmltable) <- 'knitr_kable'
        attributes(htmltable) <- list(format='html',class='knitr_kable')
        rehead <- '<thead>'
        for (j in 1:ncol(heading)){
            if (all(is.na(heading[,j]))){
                heading[1,j] <- '$'
                if (nrow(heading)>1) heading[2:nrow(heading),j] <- "|"
            }
        }
        heading[1,][is.na(heading[1,])] <- "="
        if (nrow(heading)>1){
            heading[2:nrow(heading),][is.na(heading[2:nrow(heading),])] <- "|"
        }
        dthead <- heading
        for (i in 1:nrow(heading)){
            for (j in 1:ncol(heading)){
                dthead[i,j] <- ifelse(heading[i,j] %in% c('|','='),"",
                                      paste0('   <th style="text-align:',align[j],';"> ',
                                             heading[i,j],' </th>\n'))
                if (! heading[i,j] %in% c("|","=")){
                    if (i==1 & heading[i,j]=="$"){
                        dthead[i,j] <- paste0('   <th rowspan="',nrow(heading),
                                              '" style="text-align:',align[j],
                                              ';">&nbsp;&nbsp;&nbsp;</th>\n')
                    }
                    if (j<ncol(heading)) {
                        if (heading[i,j+1] == "="){
                            colspan <- paste0(heading[i,(j+1):ncol(heading)],
                                              collapse="")
                            ncolspan <- nchar(sub("^(=+).*$","\\1",colspan))+1
                            dthead[i,j] <- sub('<th ',paste0('<th colspan="',
                                                             ncolspan,'" '),
                                               dthead[i,j])
                            dthead[i,j] <- sub('align: *?(left|right)',
                                               paste0('align:center'),
                                               dthead[i,j])
                        }
                    }
                    if (i<nrow(heading)){
                        if (heading[i+1,j] == "|"){
                            rowspan <- paste0(heading[(i+1):nrow(heading),j],
                                              collapse="")
                            nrowspan <- nchar(sub("^(\\|+).*$","\\1",rowspan))+1
                            if (grepl("colspan",dthead[i,j])){
                                if (sum(!heading[i:(i+nrowspan-1),j:(j+ncolspan-1)]
                                        %in% c('=','|'))==1){
                                    dthead[i,j] <- sub('<th ',paste0('<th rowspan="',
                                                                     nrowspan,'" '),
                                                       dthead[i,j])
                                }else{
                                    dthead[i,j] <- sub('colspan.+?style',
                                                       paste0('rowspan="',
                                                              nrowspan,'" style'),
                                                       dthead[i,j])
                                }
                            }else{
                                dthead[i,j] <- sub('<th ',paste0('<th rowspan="',
                                                                 nrowspan,'" '),
                                                   dthead[i,j])
                            }
                        }
                    }
                }
            }
        }
        for (i in 1:nrow(heading)){
            rehead <- paste0(rehead,'\n  <tr>\n', paste0(dthead[i,],collapse=""),
                             '  </tr>\n', collapse="")
        }
        rehead <- paste0(rehead,'</thead>')
        rehead <- gsub('<thead>.+</thead>', rehead, htmltable)
        class(rehead) <- class(htmltable)
        attributes(rehead) <- attributes(htmltable)
        return(sub('<table', paste0('<table width=', as.character(tableWidth)),
                   rehead))
    }
}
#------------percent format---------------
pct <- function(vector,digits=0){
    if (is.na(digits)) digits=0
    if (is.numeric(vector)){
        vec <- vector
        vec[which(!vec %in% c(NaN,Inf))] <-
            sprintf(paste0("%.",digits,"f%%"),100*vector[which(!vec %in% c(NaN,Inf))])
        return(vec)
    }else{
        return(vector)
    }
}
de_pct <- function(vector){
    if (any(grepl("[:space:]*((^\\d+[\\d\\.]\\d+)|\\d+)%$",vector))){
        vec <- vector
        which <- which(grepl("[:space:]*((^\\d+[\\d\\.]\\d+)|\\d+)%$",vector))
        vec[which] <- as.numeric(gsub("[:space:]*(.+)%$","\\1",vector[which]))/100
        vec[!seq_len(length(vec)) %in% which] <- NA
        return(as.numeric(vec))
    }else{
        return(rep(NA,length(vector)))
    }
}
initCap <- function(x) {
    if (is.factor(x)) x <- as.character(x)
    s <- strsplit(x, " ")[[1]]
    paste(toupper(substring(s, 1, 1)), substring(s, 2),
          sep = "", collapse = " ")
}
