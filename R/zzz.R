validChartTypes <- read.csv(
        system.file('validChartTypes.csv', package = 'recharts'), header=TRUE,
        stringsAsFactors=FALSE)

validSymbols <- c('circle', 'rectangle', 'triangle', 'diamond', 'emptyCircle',
                  'emptyRectangle', 'emptyTriangle', 'emptyDiamond', 'heart',
                  'droplet', 'pin', 'arrow', 'star5', 'star6', 'star7', 'star8',
                  'none')

.onLoad <- function(libname, pkgname='recharts'){
    if (Sys.info()[['sysname']] == 'Windows'){
        Sys.setlocale('LC_CTYPE', 'Chs')
    }else{
        Sys.setlocale('LC_CTYPE', 'zh_CN.utf-8')
    }
}
