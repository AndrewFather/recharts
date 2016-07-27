.onLoad <- function(libname, pkgname='recharts'){
    if (Sys.info()[['sysname']] == 'windows'){
        Sys.setlocale('LC_CTYPE', 'Chs')
    }else{
        Sys.setlocale('LC_CTYPE', 'zh_CN.utf-8')
    }
}

