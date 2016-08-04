.onLoad <- function(libname, pkgname='recharts'){
    if (Sys.info()[['sysname']] == 'Windows'){
        Sys.setlocale('LC_CTYPE', 'Chs')
    }else{
        Sys.setlocale('LC_CTYPE', 'zh_CN.utf-8')
    }
    validChartTypes <<- read.csv(
        system.file('validChartTypes.csv', package = 'recharts'), header=TRUE)

}
