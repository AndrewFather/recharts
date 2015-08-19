# Function echartR with package recharts
Author: `r Sys.info()[['user']]`  
Edited: `r format(Sys.time(),'%x %X')`  

# Intro ǰ��

��������Դ�ڰٶȿ����Ĺ��ڶ���ˮƽ�Ŀ�Դ`d3-js`������Ŀ[Echarts](http://echarts.baidu.com/)([Github Repo](https://github.com/ecomfe/echarts))��Yang Zhou��Taiyun Wei���ڸù��߿�����[recharts](https://github.com/taiyun/recharts)������Yihui Xie[�޸�](https://github.com/yihui/recharts)�󣬿�ͨ��`htmlwidgets`����js�����������˿����Ѷȡ����˰�������δ��ɡ�Ϊ�˸Ͻ������ã����ڸð�����һ������`echartR`��������������Echart����ͼ����ҪR�汾>=3.2.0.

This tool originates from a top-tier `d3-js` visualization project of China: [Baidu Echarts](http://echarts.baidu.com/)([Github Repo](https://github.com/ecomfe/echarts)). Yang Zhou and Taiyun Wei developed an experimental R package [recharts](https://github.com/taiyun/recharts) based on it, which then evoluted into [yihui/recharts](https://github.com/yihui/recharts) by Yihui Xie to pass js parameters through `htmlwidgets`. The package is sill uder development. I developed a function `echartR` based on this package to make basic Echarts interation charts. This function requires R>=3.2.0.

`echartR`����Ҫ�����ǽ�Echarts������װ��list��Yihui Xie��ԭ�ͺ���`echart`�������������list��`echart`������[�����÷�](http://yihui.name/recharts)���£��������ݱ�����û���ṩ����������λ�á�

`echartR` majorly packs Echarts parameters into a list while `echart`, the prototype function developed by Yihui Xie, is used to parse the list. The [basic examples](http://yihui.name/recharts) of `echart` is as follows, which does not provide parameters entry other than dataset itself.

```r
if (! 'recharts' %in% installed.packages()[,1]){
    install.packages('recharts',
                     repos = c('http://yihui.name/xran', 'http://cran.rstudio.com')
                     )
}
library(recharts)
echart(iris, ~Sepal.Length, ~Sepal.Width)
```

![](files/figure-html/intro1.png)

```r
echart(iris, ~Sepal.Length, ~Sepal.Width, series = ~Species)
```

![](files/figure-html/intro2.png)

# Usage �÷�
- ���ȣ���װ�����°��[R](http://www.r-project.org)��[Rstudio](http://www.rstudio.com)
- �˽��������R���Լ��ɣ��ܹ���R������д���ݼ�
- ��û���`knitr`����д[`rmarkdown`](http://rmarkdown.rstudio.com/)

## Installation ��װ
- ��װ Install `devtools` (`install.packages('devtools')`)
- ��װ Insall recharts (`install_github('yihui/recharts')`)
- ���� Download `echartR.R`�ű��ļ�������λ�� script file to local disk:  [https://github.com/madlogos/recharts/blob/master/R/echartR.R](https://github.com/madlogos/recharts/blob/master/R/echartR.R))
- ���� Source `echartR` �ű� script (�����ҽ��ű����ڱ��� suppose I stored the script to local GitHub Repo: `source("~/Github/recharts/R/echartR.R")`)

## Grammar �﷨

```
echartR(data, x=NULL, y, z=NULL, series=NULL, weight=NULL, 
        xcoord=NULL, ycoord=NULL,
        type="scatter", stack=FALSE,
        title=NULL, subtitle=NULL, title_pos=c('center','bottom'),
        title_url=NULL, subtitle_url=NULL,
        symbolList=NULL, dataZoom=NULL, dataZoomRange=NULL,
        dataRange=NULL, splitNumber=NULL, dataRangePalette=NULL,
        xlab=NULL, ylab=NULL, xyflip=FALSE, AxisAtZero=TRUE, scale=TRUE,
        palette='aetnagreen', tooltip=TRUE, 
        legend=TRUE, legend_pos=c('center','top'), 
        toolbox=TRUE, toolbox_pos=c('right','top'), 
        calculable=TRUE, asImage=FALSE,
        markLine=NULL, markPoint=NULL, ...))
```

- **data**: ���ݼ� dataset
- x: x������ֱ��ͼ��ʡ�ԡ�x variable, only omitable for histograms��
- **y**: y���� y variable
- z: z������ֻ����ʱ��/���ڱ���������ʱ���ᡣz variable, only accept data/time variable to open time axis
- series: Series(ϵ��)���� series variable
- weight: Ȩ�ر�����������ֱ��ͼ������ͼ�� weight variable, used in histogram, bubble, etc
- xcoord: γ����������������ڵ��ע��map��Lattitude variable, only for point-marking map.
- ycoord: ������������������ڵ��ע��map��Longitude variable, only for point-marking map.
- type: Ĭ�� default `scatter`����ѡ options 'scatter', 'bubble', 'bar', 'line', 'linesmooth', 'map', 'k', 'pie', 'ring', 'rose','area', 'areasmooth', 'chord', 'force', 'tree', 'treemap', 'wordcloud', 'heatmap', 'histogram', 'funnel', 'pyramid', 'radar', 'radarfill'
    - ��ѡ��map������������д��һ������Ϊ3��������c('map',`mapType`,`area/point`)��mapType��ѡ'world'��'china'����������ı�ʾ�ľ����й�������area/pointΪareaʱ����������ɫ��ʾЧӦ��С��Ϊpointʱ���õ��ڵ�ͼ������ע��Ĭ��Ϊc('map','china','area')��If `map` was chosen, the control option should be a vector of length 3: c('map',`mapType`,`area/point`). `mapType` could be either 'world' or 'china', of which simplified Chinese names are required for 'china'. When `area/point` equals to 'area', the function colors polygons to show the effects; while equals to 'point', it ticks droplets on the map.
- stack: Ĭ��FALSE���Ƿ�ѻ������������ѻ���ͼ����ͼ����ͼ�����ͼ��ֱ������ϵͼ�Ρ�Default to FALSE (do not stack). Used in stacked column, bar, line and area chart, etc.
- title: ���� 
- subtitle: ������
- title_pos: ����λ�ã�����c('left|center|right','top|center|bottom')��Ĭ��c("center","bottom")��ʾ�ײ�����λ�á�Legend position which is a vector of length 2 (c('left|center|right','top|center|bottom')). Default to c('center','bottom') which means bottom meiddle.
- title_url: �������ӣ�url of the title
- subtitle_url: ����������, url of the subtitle
- symbolList: ͼ�α�־����ʹ������ѭ��ʹ�ã������鳤��С��seriesˮƽ�����������һ����־��䡣������ΪNULL�����ã���ѭ����ʾEchartsĬ�ϵı�־ͼ���б�c('circle','rectangle','triangle','diamond','emptyCircle','emptyRectangle','emptyTriangle','emptyDiamond')��Ҳ������ָ��'heart','droplet','pin','arrow','star5','star6'�ȷǱ�ͼ�Ρ���Ϊ'none'����ʾ��A vector assigning symbols. You can use an array of symbols. If the length of the symbols array is smaller than number of levels of the series, the last symbol will be used to extend the array. If you set symbolList NULL or leave it unset, the function circulates the default symbol list of Echarts: c('circle','rectangle','triangle','diamond','emptyCircle','emptyRectangle','emptyTriangle','emptyDiamond'). You can also assign non-standard symbols, such as 'heart','droplet','pin','arrow','star5','star6', 'star7', etc. When assigned to 'none', no symbols are shown.
- dataZoom: ���������ᣬĬ��FALSE. The axis to zoom data. Default to FALSE.
- dataZoomRange: ��`dataZoom=TRUE`��Ĭ�Ϸ�ΧΪ0-100%������һ������Ϊ2���������Ƴ�ʼ��Χ����`c(30,70)`��ʾ��ʼ30-70%��If `dataZoom=TRUE`, the default range is 0-100%. You can assign a vector with length of 2 to `dataZoomRange` to control the initial range. E.g.,`c(30,70)` means from 30% to 70% at the initial view.
- dataRange: ���ݷ�Χ���η�Χ��Ĭ�ϲ��򿪡���Ҫ�򿪣�����dataRange=c(`��ֵ��ǩ`,`��ֵ��ǩ`) The range to zoom the data. Default to FALSE. Set dataRange=c(`High value label`,`Low value label`) to enable dataRange.
- splitNumber: ����������Σ���ָ�����ݷ�Χ�зֶ�����Ĭ��Ϊ����������(0)����ֱ��ͼ����趨splitNumber���������зֳ�splitNumber���顣When dataRange is on, assign splitNumber to cut the range into discrete sections. Default to 0 (continuous range). In histogram, if splitNumber is set, the y variable will be cut into splitNumber groups.
- dataRangePalette: ����������Σ��ɵ���ָ������ɫ��(ͬpalette����)���������EchartsĬ��ֵ��You can independently assign palettes to dataRange (similar to overall palette). Default to NULL (applies echarts defaults).
- xlab: x����� title of x-axis
- ylab: y����� title of y-axis
- xyflip: Ĭ��FALSE���Ƿ�ת�����ᡣFlip x,y-axies. Default to FALSE.
- AxisAtZero: Ĭ��FALSE���������Ƿ񽻲�����㡣Axes cross at zero. Default to FALSE.
- scale: Ĭ��TRUE���Ƿ���������Сֵ��������߶ȡ�Rescale the axes based on min and max values. Default to TRUE.
- palette: ��ɫ�塣Ĭ��aetnagreen��Overall palette. Default to 'aetnagreen'.
    - ���� Palette names:
        - Aetna palettes: ���� Including 'aetnagreen', 'aetnablue', 'aetnaviolet', 'aetnaorange', 'aetnateal', 'aetnacranberry'
        - RColorBrewer palettes: ���� Including 'BrBG', 'PiYG', 'PRGn', 'PuOr', 'RdBu', 'RdGy', 'RdYlBu', 'RdYlGn', 'Spectral', 'Accent', 'Dark2', 'Paired', 'Pastel1', 'Pastel2', 'Set1', 'Set2', 'Set3', 'Blues', 'BuGn', 'BuPu', 'GnBu', 'Greens', 'Greys', 'Oranges', 'OrRd', 'PuBu', 'PuBuGn', 'PuRd', 'Purples', 'RdPu', 'Reds', 'YlGn', 'YlGnBu', 'YlOrBr', 'YlOrRd'
        - ggthemes palettes: 'calc', 'economist', 'economist_white', 'economist_stata','excel', 'exel_fill', 'excel_old', 'excel_new', 'few', 'fivethirtyeight', 'gdocs', 'pander', 'tableau', 'stata', 'stata1','stata1r','statamono', 'tableau20', 'tableau10medium', 'tableaugray', 'tableauprgy', 'tableaublrd', 'tableaugnor', 'tableaucyclic', 'tableau10light', 'tableaublrd12', 'tableauprgy12', 'tableaugnor12','hc','darkunica', 'solarized','solarized_red', 'solarized_yellow', 'solarized_orange','solarized_magenta','solarized_violet', 'solarized_blue', 'solarized_cyan', 'solarized_green', 'wsj', 'wsj_rgby', 'wsj_red_green', 'wsj_black_green', 'wsj_dem_rep', 'colorblind', 'trafficlight'
        - ����Other palettes: 'rainbow', 'terrain', 'topo', 'heat', 'cm'
    - �÷� Usage:
        - ���Բ�ָ����ʹ�ú���Ĭ�ϡ�Do not set the value and function defaults will be loaded
        - ����`palette=NULL`��ʹ��EchartsĬ�ϡ�Set `palette=NULL` to use Echarts defaults
        - ����`palette=palette name`ָ�������κ�һ��ɫ�塣Set `palette=palette name` to assign any palette listed above
        - �涨ɫ���ͬʱ�������޶����޶�ɫ����ɫ�ĸ�������`palette='calc(3)'`�����calcɫ����**���**ȡ3����ɫ��Set `palette=palette name(number)` to restrict number of colors within the palette (e.g., `palette='calc(3)'` picks 3 colors out of 'calc' **RANDOMLY**)
        - ����`palette=c(color1,color2,color3,...)`�Զ���ɫ��������������������ɫ����Ҳ������Hex���ʽ��������`colors()`�����鿴����֧�ֵ���ɫ���ƣ�`demo(colors)`�鿴��ɫЧ����Set `palette=c(color1,color2,color3,...)` to define a palette vector, made of which either color names or Hex expressions. Use `colors()` to check available color names and check the effects using `demo(colors)`.
- tooltip: Ĭ��TRUE�����ָ����Ч��Mouse tip effects swtich. Default to TRUE.
- legend: Ĭ��TRUE���Ƿ���ʾͼ����Whether show the legend. Default to TRUE.
- legend_pos: ͼ��λ�ã�����Ϊ2�����������÷����ο�`title_pos`��Legend position, a vector of length 2. Refer to `title_pos` for configuration.
- toolbox: Ĭ��TRUE���Ƿ���ʾ�����䡣Echarts Tool box switch. Default to TRUE.
- toolbox_pos: ������λ�ã�Ĭ�����Ϸ����ο�`title_pos`�����÷�����Toolbox position, default to c('right','top'). Refer to `title_pos` for configuration.
- calculable: Ĭ��TRUE���Ƿ�֧����ҷ����(Echartsר��) Calculable switch (Echarts patent).
- asImage: Ĭ��FALSE���Ƿ���ʾΪ��̬ͼ��renderAsImage switch.Deafult to FALSE.
- markLine: ��ʾ���ߣ�Ĭ�ϲ���ʾ����ʽд��һ���б� Show markline, default to NULL. The grammar is a list:
    - ���Ը�ʽ Short form (����3��4 length 3 or 4)��
    `list(c('data series 1 name|index','line name','min|max|average|lm',TRUE|NULL),c('data series 2 name|index','line name','min|max|average|lm',TRUE|NULL),...)`
    ��list('male',NA,'average')��list(1,NA,'average')���ɱ�ʾmale����ϵ��ƽ��ֵ���ߣ�ֻ����line, linesmooth, bar, scatter, bubble��`lm`�ɳ����Իع���ߣ�ֻ����ɢ�������ͼ����list('male',NA,'average',T)���ʾmaleϵ�п����Ź���Ч��
    E.g., both list('male',NA,'average') and list(1,NA,'average') could refer to an average markline of the series 'male', only available for line, linesmooth, bar, scatter, bubble charts. 'lm' refers to linear regresson markline which is only available for scatters and bubbles. list('male',NA,'average',T) opens aurora effects of series 'male'.
    - ������ʽ Full form (����7��8 length 7 or 8): 
    list(c('data series 1 name|index','line name','value 1','x1 begin','y1 begin','x1 end','y1 end',TRUE|NULL),c('data series 2 name|index','line name','value 2','x2 begin','y2 begin','x2 end','y2 end',TRUE|NULL),...)
    ��list(c('male',NA,100,0,5,100,5))��ʾ��'male'����ϵ���л�һ����ԽP(0,5)��P(100,5)��ֱ�ߡ�E.g., list(c('male',NA,100,0,5,100,5)) refers to a markline through P(0,5) and P(100,5) as of sereis 'male'. ��line, bar, k, scatterͼ�У�'x1 begin','y1 begin','x1 end','y1 end'�������Ϊֱ������ϵ�Ķ�λ����map�У���Щ����ֵ����д����γ�ȡ� list(c('male',NA,100,0,5,100,5,T))�ɴ�maleϵ�е��Ź���Ч�� In line, bar, k and scatter charts, 'x1 begin','y1 begin','x1 end','y1 end' are comprehended as coordinates. In map charts, these coordinates should be lattitudes and longitudes. list(c('male',NA,100,0,5,100,5,T)) opens aurora effects of series 'male'.
- markPoint: ��ʾ��ע�㣬Ĭ�ϲ���ʾ����ʽд��һ���б� Show markline, default to NULL. The grammar is a list:
    - ���Ը�ʽ Short form (����3��4 length 3 or 4)��
    `list(c('data series 1 name|index','point name','min|max',TRUE|NULL),c('data series 2 name|index','point name','min|max',TRUE|NULL),...)`
    ��list('male',,'min')��list(1,,'min')���ɱ�ʾmale����ϵ����Сֵ��ע��ֻ����line, linesmooth, bar, scatter, bubble��list('male',,'min',T)���ʾmaleϵ�п����Ź���Ч��
    E.g., both list('male',NA,'min') and list(1,NA,'min') refer to a min markpoint of the series 'male', only available for line, linesmooth, bar, scatter, bubble charts. list('male',NA,'min',T) opens aurora effects of series 'male'.
    - ������ʽ Full form (����5��6 length 5 or 6): 
    list(c('data series 1 name|index','point name','value 1','x1','y1',TRUE|NULL),c('data series 2 name|index',;point name','value 2','x2','y2',TRUE|NULL),...)
    ��list(c('male',NA,100,0,5))��ʾ��'male'����ϵ���б�ע��P(0,5)��E.g., list(c('male',NA,100,0,5)) refers to a markpoint at P(0,5) as of sereis 'male'. ��line, bar, k, scatterͼ�У�'x1','y1',...�������Ϊֱ������ϵ�Ķ�λ����map�У���Щ����ֵ����д����γ�ȡ� list(c('male',NA,100,0,5,T))�ɴ�maleϵ�е��Ź���Ч�� In line, bar, k and scatter charts, 'x1','y1',... are comprehended as coordinates. In map charts, these coordinates should be lattitudes and longitudes. list(c('male',NA,100,0,5,T)) opens aurora effects of sereis 'male'.
    
# Examples ʾ��


```r
Sys.setlocale("LC_CTYPE","Chs")
source("~/Github/recharts/R/echartR.R")
#source("C:/HMSProjects/Data Analytics/R_scripts/CommonFunctions.R")
knitr::opts_chunk$set(message=FALSE,warning=FALSE,results='asis')
```

## Scatter ɢ��ͼ

### Mono-series Scatter ��ϵ��ɢ��ͼ
û������ϵ�У����ָ����ʾ��1��ϵ�еľ�����(`markLine=list(c(1,NA,'average'`)��


```r
echartR(data = iris, x = ~Sepal.Width, y = ~Petal.Width, 
        type = 'scatter', palette='solarized_magenta',
        title = 'Scatter - Sepal Width vs Petal Width', 
        subtitle = "(source: iris)", xlab = 'Sepal Width', ylab = 'Petal Width',
        markLine=list(c(1,NA,"average")))
```

![](files/figure-html/scatter1.png)

### Multi-series Scatter ��ϵ��ɢ��ͼ

ָ��series������ʾ��Χ����㿪ʼ(`scale=FALSE`)���ڵ�2������ϵ��(versicolor)�д����(max)����С(min)������(average)�������ߣ��ڵ�1��2��3����ϵ���зֱ��עmax��min��max�㣬�Ҵ򿪵�3��ϵ�е�ѣ����Ч(`markPoint=list(c(1,'Max','max'),c(2,'Min','min'),c(3,'Max','max',T))`)��


```r
echartR(data = iris, x = ~Sepal.Width, y = ~Petal.Width, series = ~Species,
        type = 'scatter', palette='wsj_dem_rep', symbolList='circle',
        scale=F, xlab = 'Sepal Width', ylab = 'Petal Width',
        title = 'Scatter - Sepal Width vs Petal Width, by Species',
        subtitle ='(source: iris)', 
        markLine=list(c(2,'average'),c('versicolor','max'),c(2,'min')),
        markPoint=list(c(1,'Max','max'),c(2,'Min','min'),c(3,'Max','max',T)))
```

![](files/figure-html/scatter2.png)

ʹ�����׷Ǳ�׼ͼ��(��ͷ�����Ρ��˽���)��������ϵ��(`c('arrow','heart','star8')`)������ָ�����ѡ��excel_oldɫ���е�1����ɫ�����������ݼ���3��ˮƽ(series)����ɫ����������ִ���б����ԡ�

ͬʱ���3���ع���(markLine�б�д��short form������ָ��Ϊ'lm'���������Ź���Ч)��


```r
echartR(data = iris, x = ~Sepal.Width, y = ~Petal.Width, series = ~Species,
        type = 'scatter', palette='excel_old(1)',
        symbolList=c('arrow','heart','star8'),
        title = 'Scatter - Sepal Width vs Petal Width, by Species',
        subtitle = '(source: iris)', xlab = 'Sepal Width', ylab = 'Petal Width',
        markLine = list(c(1,NA,'lm',T),c(2,NA,'lm',T),c(3,NA,'lm',T)))
```

![](files/figure-html/scatter3.png)

���ģɢ��ͼ(2000��������)ͬ�������뼶��ͼ(��html�ļ����úܴ�)��


```r
x <- rnorm(2001)*2
e <- vector()
for (i in 1:2001)  e <- c(e,rnorm(1,0,x[i]+abs(min(x))))
df <- data.frame(x, sin=sin(x)+e/20, cos=cos(x)+e/20)
df <- melt(df,id="x")
echartR(df,x=~x,y=~value,series=~variable,type='scatter',
        palette='wsj_red_green',symbolList='circle',
        title='Scatter of 2,001 points', subtitle = 'Large-scale scatter')
```

![](files/figure-html/scatter4.png)

## Bubble ����ͼ

����ͼͬ����Դ��ɢ��ͼ��type��Ϊ`bubble`���ɣ��粻ָ��`weight`����������Ĭ��ָ��yΪ����Ȩ�ء�

### Mono-series Bubble ��ϵ������ͼ


```r
echartR(data = iris, x = ~Sepal.Width, y = ~Petal.Width, weight = ~Petal.Length,
        type = 'bubble', palette='solarized_cyan',
        title = paste("Bubble - Sepal Width vs Petal Width,",
                      "weighed by Petal Length"),
        subtitle = '(source: iris)',
        xlab = 'Sepal Width', ylab = 'Petal Width')
```

![](files/figure-html/bubble1.png)

### Multi-series Bubble ��ϵ������ͼ

��`symbolList=c('circle','emptyCircle')`���õ�1������ϵ��Ϊʵ��Բ��ʣ�µ�����ϵ���Կ���Բ��Ϊ��־ͼ�Ρ�palette����ΪtableauGnOr(3)��ֻ���ȡ��ɫ���3����ɫ��

����һ������ȫ�������Իع�(`lm()`)�������޷���ʾ���κ�һ��ϵ���ϣ����echartR������һ���µ�����ϵ�С�ǰһ�ڶ�ϵ��ɢ��ͼ�У�����ϵ��Sepal Width��Petal Width������أ��������У��ϲ���������ȴ�ʸ���ء�������������չʾ�˷ֲ��������Ҫ�ԡ�


```r
fit <- lm(Petal.Width~Sepal.Width,iris)
pred <- data.frame(Sepal.Width=c(min(iris$Sepal.Width),max(iris$Sepal.Width)))
yhat <- round(predict(fit,pred),2)
echartR(data = iris, x = ~Sepal.Width, y = ~Petal.Width, 
        weight = ~Petal.Length, series = ~Species, 
        symbolList=c('emptyCircle','circle'),
        type = 'bubble', palette='tableaugnor(4)',
        title = paste('Bubble - Sepal Width vs Petal Width, by Species,',
                      'weighed by Petal Length'), 
        subtitle = '(source: iris)',
        xlab = 'Sepal Width', ylab = 'Petal Width',
        markLine=list(c('Coef.','Linerar Reg',round(fit$coefficients[[2]],2),
                        pred[1,1],yhat[[1]],pred[2,1],yhat[[2]])))
```

![](files/figure-html/bubble2.png)

## Column ��ͼ

������һ���������ݼ�`dtiris`��


```r
library(reshape2)
dfiris <- iris
dfiris$id <- row.names(iris)
dfiris <- melt(dfiris,id=c("Species","id"))
names(dfiris) <- c("Species","id","Param","Value")
dtiris <- dcast(dfiris[,c(1,3,4)],Species+Param~.,value.var="Value",mean)
names(dtiris) <- c("Species","Param","Mean")
knitr::kable(dcast(dtiris,Param~Species,sum,value.var="Mean"),
             format='html',caption="Table: Mean of parameters (iris)")
```

<table>
<caption>Table: Mean of parameters (iris)</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> Param </th>
   <th style="text-align:right;"> setosa </th>
   <th style="text-align:right;"> versicolor </th>
   <th style="text-align:right;"> virginica </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Sepal.Length </td>
   <td style="text-align:right;"> 5.006 </td>
   <td style="text-align:right;"> 5.936 </td>
   <td style="text-align:right;"> 6.588 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Sepal.Width </td>
   <td style="text-align:right;"> 3.428 </td>
   <td style="text-align:right;"> 2.770 </td>
   <td style="text-align:right;"> 2.974 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Petal.Length </td>
   <td style="text-align:right;"> 1.462 </td>
   <td style="text-align:right;"> 4.260 </td>
   <td style="text-align:right;"> 5.552 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Petal.Width </td>
   <td style="text-align:right;"> 0.246 </td>
   <td style="text-align:right;"> 1.326 </td>
   <td style="text-align:right;"> 2.026 </td>
  </tr>
</tbody>
</table>

### Tiled Column ƽ����ͼ


```r
echartR(data = dtiris, x = ~Param, y = ~Mean,  series = ~Species,
        type = 'bar', palette='fivethirtyeight',
        title = "Column - Parameter Mean by Species", 
        subtitle = '(source: iris)',legend_pos=c('right','center'),
        xlab = 'Parameter', ylab = 'Mean')
```

![](files/figure-html/column1.png)

### Stacked Column �ѻ���ͼ


```r
echartR(data = dtiris, x = ~Param, y = ~Mean, 
        series = ~Species, stack=T,
        type = 'bar', palette='pander',
        title = "Column - Parameter Mean by Species",
        subtitle = '(source: iris)',
        xlab = 'Parameter', ylab = 'Mean', legend_pos=c('right','center'))
```

![](files/figure-html/column2.png)

�ѻ����Ҳ���Լ򵥵�ͨ����������`ƽ��`��`�ѻ�`��ť�л����ǳ�ǿ��(��Ҳ���ں�������������ȷ���������)��

## Bar ��ͼ

��ͼ����ͼ������ֻ����`xyflip`����ѡ�

### Tiled Bar ƽ����ͼ


```r
echartR(data = dtiris, x = ~Species, y = ~Mean, series = ~Param, 
        type = 'bar', palette='stata1', xyflip=T,
        title = 'Species-specific Mean by Parameters',
        subtitle = '(source: iris)', legend_pos=c('right','center'),
        xlab = 'Species', ylab = 'Mean')
```

![](files/figure-html/bar1.png)

### Stacked Bar �ѻ���ͼ

palette��Ϊcalc�����4����ɫ����stack (`stack=TRUE`)��


```r
echartR(data = dtiris, x = ~Param, y = ~Mean, 
        series = ~Species, stack=T, xyflip=T,
        type = 'bar', palette='calc(5)',
        title = 'Parameter Mean by Species',
        subtitle = '(source: iris)',
        xlab = 'Parameter', ylab = 'Mean', legend_pos=c('right','center'))
```

![](files/figure-html/bar2.png)

## Histogram ֱ��ͼ

ֱ��ͼ����ͼ��һ��������ֻ��Ҫָ��y��������ͨ��`splitNumber`ָ��ֱ����(Ĭ��9)��`xyflip`��ΪTRUE���Ϊ�ȼ۵ĺ���ͼ��

> Echarts�涨��/��ͼ��ɢ��ͼ���Ա������Ϊcategory���ͣ������ʱ�޷��򵥵�������ͳ��۵�ֱ��ͼ��


```r
echartR(airquality, y=~Temp, type='histogram', splitNumber=13,
        palette='pastel2', title='Histogram of temperature',
        subtitle = '(source: airquality)')
```

![](files/figure-html/hist.png)

## Pie ��ͼ

��`mtcars`��Ϊ��ͼ���ݼ���


```r
dtcars <- mtcars
dtcars$car <- row.names(dtcars)
dtcars$transmission <- as.factor(dtcars$am)
levels(dtcars$transmission) <- c("Automatic","Manual")
dtcars$cylinder <- as.factor(dtcars$cyl)
dtcars$carburetor <-as.factor(dtcars$carb)
```


```r
echartR(dtcars, x = ~transmission,  y = ~car, type='pie',
        palette='darkunica', 
        title='Number of cars by transmission',
        subtitle = '(source: mtcars)')
```

![](files/figure-html/pie.png)

## Ring ��ͼ

����ͼ�Ǳ�ͼ�ı��Σ�ֻ�轫`type`��Ϊ'ring'��Echarts��ֻ��Ҫ�ѱ�ͼ�İ뾶������չΪ�����ڡ��⾶�ĳ���Ϊ2���������ɡ�


```r
echartR(dtcars, x = ~cylinder,  y = ~car, type='ring',
        palette='hc', title='Number of Cylinders',
        subtitle = '(source: mtcars)')
```

![](files/figure-html/ring.png)

### Rose Nightingaleõ��ͼ


```r
echartR(dtcars, x = ~cylinder,  y = ~car, type='rose',
        palette='colorblind', title='Number of Cylinders',
        subtitle = '(source: mtcars)')
```

![](files/figure-html/rose.png)

## Line ��ͼ

### Unstacked Line ƽ����ͼ

����������`dataZoom=T`


```r
airquality$Date <- strptime(paste(2015,airquality$Month,airquality$Day,sep="-"),
                            format="%F", tz="Asia/Taipei")
airquality$strDate <- with(airquality,paste(2015,Month,Day,sep="-"))
airquality$TempG <- cut(airquality$Temp,breaks=c(0,60,70,80,100))
#a=echartR(airquality, x = ~Date, y= ~Wind,
#          type='linesmooth', dataZoom=T, 
#        xlab = 'Date', ylab = 'Wind',
#        title='Wind by day',
#        subtitle = 'source: airquality')
echartR(airquality, x = ~Day, y= ~Wind, series=~Month, type='line', 
        dataZoom=T, dataZoomRange=c(30,70), symbolList='none',
        palette='tableauBlRd', xlab = 'Days', ylab = 'Wind',
        title='Day-specific Wind by month (airquality)')
```

![](files/figure-html/line1.png)

�߶�ƽ��(`type='linesmooth'`)������ʾ��־ͼ��(`symbolList='none'`)��Echarts��ȱʧֵĬ�ϲ��������кܶ���ߡ���Ҫ������ǰ����ʱ��������ֵ���㡣


```r
airq <- melt(airquality[,c("Ozone","Solar.R","Wind","Temp","strDate")],
             id=c("strDate"))
echartR(airq, x = ~strDate, y= ~value, series= ~variable, type='linesmooth',
        symbolList='none', dataZoom=T, dataZoomRange=c(20,50),
        palette='tableauPrGy', xlab = 'Date', ylab = 'Measure',
        title='Climate measures by day', subtitle = 'source: airquality')
```

![](files/figure-html/line2.png)

### Stacked Line �ѻ���ͼ

```r
echartR(dfiris, x = ~id, y= ~Value, series= ~Param, type='line',stack=T,
        palette='tableauBlRd12', xlab = 'Sample ID', ylab = 'Measure',
        title='Parameter measures', subtitle = '(source: iris)')
```

![](files/figure-html/line3.png)

�߶�ƽ��������ʾ��־ͼ��

```r
echartR(dfiris, x = ~id, y= ~Value, series= ~Param, type='linesmooth',stack=T,
        palette='tableauGnOr12', xlab = 'Sample ID', ylab = 'Measure',
        symbolList='none',
        title='Parameter measures', subtitle = '(source: iris)')
```

![](files/figure-html/line4.png)

## Area ���ͼ
Echarts�У����ͼ�����ϱ�����Ϊ��ͼ��ֻ��ͨ��`itemStyle`������Ⱦ��ɫ��

### Tiled Area ƽ�����ͼ

```r
echartR(dfiris, x = ~id, y= ~Value, series= ~Param, type='area',
        palette='brbg', xlab = 'Sample ID', ylab = 'Measure',
        symbolList='emptyDiamond',title='Parameter measures',
        subtitle = '(source: iris)')
```

![](files/figure-html/area1.png)

�߶�ƽ��`type='areasmooth'`��������������`dataZoom=TRUE`����ʼ��ʾ40%-80%��

```r
echartR(dfiris, x = ~id, y= ~Value, series= ~Param, type='areasmooth',
        palette='PiYG', xlab = 'Sample ID', ylab = 'Measure', 
        symbolList='none', dataZoom=T, dataZoomRange=c(40,80),
        title='Parameter measures', subtitle = '(source: iris)')
```

![](files/figure-html/area2.png)

### Stacked Area �ѻ����ͼ

```r
echartR(dfiris, x = ~id, y= ~Value, series= ~Param, type='area',stack=T,
        palette='PRGn', xlab = 'Sample ID', ylab = 'Measure',
        symbolList='emptyCircle',
        title='Parameter measures', subtitle = '(source: iris)')
```

![](files/figure-html/area3.png)

�߶�ƽ��(`type='areasmooth'`)���Զ���ɫ��������

```r
echartR(dfiris, x = ~id, y= ~Value, series= ~Param, type='areasmooth',stack=T,
        palette=c('red','yellow','limegreen','skyblue'), 
        xlab = 'Sample ID', ylab = 'Measure', 
        symbolList='none',
        title='Parameter measures', subtitle = '(source: iris)')
```

![](files/figure-html/area4.png)

## Funnel ©��ͼ

### Funnel ��ͨ©��ͼ


```r
echartR(dtcars, x = ~carburetor,  y = ~car, type='funnel',
        palette='RdBu', title='Number of carburetors of cars',
        subtitle = '(source: mtcars)')
```

![](files/figure-html/funnel.png)

### Pyramid ������ͼ
������ͼ������©��ͼ��


```r
echartR(dtcars, x = ~carburetor,  y = ~car, type='pyramid',
        palette='RdGy', title='Number of carburetors of cars',
        subtitle = '(source: mtcars)')
```

![](files/figure-html/pyramid.png)

## Radar �״�ͼ

�״�ͼ���Ǽ�����ϵ�µ���ͼ/���ͼ��ͨ��Echarts��`polar`����ģ����ơ�

### Hollow Radar �����״�


```r
browser <- as.data.frame(
    matrix(c(2001,390,208,15,9,0.5,2002,380,204,20,18,2,2003,370,200,25,27,4.5,
    2004,360,196,30,36,8,2005,350,192,35,45,12.5,2006,340,188,40,54,18,
    2007,330,184,45,63,24.5,2008,320,180,50,72,32,2009,310,176,55,81,40.5,
    2010,300,172,60,90,50,2011,290,168,65,99,60.5,2012,280,164,70,108,72,
    2013,270,160,75,117,84.5,2014,260,156,80,126,98,2015,250,152,85,135,112.5,
    2016,240,148,90,144,128,2017,230,144,95,153,144.5,2018,220,140,100,162,162,
    2019,210,136,105,171,180.5,2020,200,132,110,180,200,2021,190,128,115,189,220.5,
    2022,180,124,120,198,242,2023,170,120,125,207,264.5,2024,160,116,130,216,288,
    2025,150,112,135,225,312.5,2026,140,108,140,234,338,2027,130,104,145,243,364.5,
    2028,120,100,150,252,392),byrow=T,ncol=6)
    )
names(browser) <- c("Year","IE8-","IE9+","Safari","Firefox","Chrome")
browser <- melt(browser,id="Year")
echartR(browser, x= ~variable, y= ~value, series= ~Year, type='radar',
        palette=paste0('heat(',nlevels(as.factor(browser$Year)),")"),
        legend_pos=c('left','center'), symbolList='none',
        title='Browser Mkt Occup Ratio', subtitle= 'Totally virtual data')
```

![](files/figure-html/radar1.png)

### Solid Radar ʵ���״�


```r
player <- data.frame(name=c(rep("Philipp Lahm",8),rep("Dani Alves",8)),
                     para=rep(c("Passing%","Key passing","Comp crosses",
                                "Crossing%","Successful dribbles",
                                "Dispossessed","Dribbled past","Fouls"),2),
                     value=c(89.67, 1.51, 0.97, 24.32, 0.83, 0.86, 1.15, 0.47,
                            86.62, 2.11, 0.99, 20.78, 1.58, 1.64, 0.9, 1.71))
echartR(player, x= ~para, y= ~value, series= ~name, type='radarfill',
        symbolList='none', palette=c('firebrick1','dodgerblue'),
        title='Lahm vs Alves', subtitle= '(by @mixedknuts)')
```

![](files/figure-html/radar2.png)

## Map ��ͼ

R��Rstudio������ת����Windows��һֱ���ϴ��ѡ��ڱ��ĵ���Rstudio��Ĭ�ϱ������CP936���룬�ĵ���UTF-8����ֱ���ڳ����ڶ����ݼ�����iconvת����䵥�ĺ�����Ȼ����ʾΪ���롣��������[ChinaGDP.txt](https://raw.githubusercontent.com/madlogos/Shared_Doc/master/Shared_Documents/ChinaGDP.txt)�����أ�`readLines`���롣

������orz�ĵط���Chrome������ȷ��ʾ��ͼ�������һ�Ѿ�ݡ�


```r
#gdp <- readLines("https://raw.githubusercontent.com/madlogos/Shared_Doc/master/Shared_Documents/ChinaGDP.txt")
gdp <- readLines("ChinaGDP.txt")
dtgdp <- unlist(strsplit(gdp,split=","))
dtgdp <- as.data.frame(t(matrix(dtgdp,nrow=3)),stringsAsFactors=F)
names(dtgdp) <- c('Year','Prov',"GDP")
dtgdp$GDP <- as.numeric(dtgdp$GDP) 
knitr::kable(dcast(dtgdp,Prov~Year,sum,value.var="GDP"), format='html',
         caption="Table: 2012-2014 GDP of Provinces in China (Million USD)")
```

<table>
<caption>Table: 2012-2014 GDP of Provinces in China (Million USD)</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> Prov </th>
   <th style="text-align:right;"> 2012 </th>
   <th style="text-align:right;"> 2013 </th>
   <th style="text-align:right;"> 2014 </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> ���� </td>
   <td style="text-align:right;"> 272666 </td>
   <td style="text-align:right;"> 307416 </td>
   <td style="text-align:right;"> 339401 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ���� </td>
   <td style="text-align:right;"> 283238 </td>
   <td style="text-align:right;"> 314871 </td>
   <td style="text-align:right;"> 347249 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ���� </td>
   <td style="text-align:right;"> 312107 </td>
   <td style="text-align:right;"> 351347 </td>
   <td style="text-align:right;"> 391609 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ���� </td>
   <td style="text-align:right;"> 89508 </td>
   <td style="text-align:right;"> 101208 </td>
   <td style="text-align:right;"> 111273 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> �㶫 </td>
   <td style="text-align:right;"> 904046 </td>
   <td style="text-align:right;"> 1003746 </td>
   <td style="text-align:right;"> 1103605 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ���� </td>
   <td style="text-align:right;"> 206497 </td>
   <td style="text-align:right;"> 232158 </td>
   <td style="text-align:right;"> 255144 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ���� </td>
   <td style="text-align:right;"> 108550 </td>
   <td style="text-align:right;"> 129284 </td>
   <td style="text-align:right;"> 150599 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ���� </td>
   <td style="text-align:right;"> 45236 </td>
   <td style="text-align:right;"> 50805 </td>
   <td style="text-align:right;"> 56989 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> �ӱ� </td>
   <td style="text-align:right;"> 420990 </td>
   <td style="text-align:right;"> 456976 </td>
   <td style="text-align:right;"> 478953 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ���� </td>
   <td style="text-align:right;"> 468900 </td>
   <td style="text-align:right;"> 519212 </td>
   <td style="text-align:right;"> 568786 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ������ </td>
   <td style="text-align:right;"> 216896 </td>
   <td style="text-align:right;"> 232237 </td>
   <td style="text-align:right;"> 244829 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ���� </td>
   <td style="text-align:right;"> 352482 </td>
   <td style="text-align:right;"> 398316 </td>
   <td style="text-align:right;"> 445514 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ���� </td>
   <td style="text-align:right;"> 350958 </td>
   <td style="text-align:right;"> 395622 </td>
   <td style="text-align:right;"> 440328 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ���� </td>
   <td style="text-align:right;"> 189136 </td>
   <td style="text-align:right;"> 209608 </td>
   <td style="text-align:right;"> 224715 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ���� </td>
   <td style="text-align:right;"> 856368 </td>
   <td style="text-align:right;"> 955269 </td>
   <td style="text-align:right;"> 1059587 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ���� </td>
   <td style="text-align:right;"> 205131 </td>
   <td style="text-align:right;"> 231520 </td>
   <td style="text-align:right;"> 255724 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ���� </td>
   <td style="text-align:right;"> 393607 </td>
   <td style="text-align:right;"> 437216 </td>
   <td style="text-align:right;"> 466018 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ���ɹ� </td>
   <td style="text-align:right;"> 251574 </td>
   <td style="text-align:right;"> 271788 </td>
   <td style="text-align:right;"> 289274 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ���� </td>
   <td style="text-align:right;"> 37090 </td>
   <td style="text-align:right;"> 41417 </td>
   <td style="text-align:right;"> 44802 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> �ຣ </td>
   <td style="text-align:right;"> 29997 </td>
   <td style="text-align:right;"> 33925 </td>
   <td style="text-align:right;"> 37460 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ɽ�� </td>
   <td style="text-align:right;"> 792289 </td>
   <td style="text-align:right;"> 882974 </td>
   <td style="text-align:right;"> 967419 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ɽ�� </td>
   <td style="text-align:right;"> 191886 </td>
   <td style="text-align:right;"> 203485 </td>
   <td style="text-align:right;"> 207714 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ���� </td>
   <td style="text-align:right;"> 228969 </td>
   <td style="text-align:right;"> 259078 </td>
   <td style="text-align:right;"> 287978 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> �Ϻ� </td>
   <td style="text-align:right;"> 319710 </td>
   <td style="text-align:right;"> 348804 </td>
   <td style="text-align:right;"> 383554 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> �Ĵ� </td>
   <td style="text-align:right;"> 378183 </td>
   <td style="text-align:right;"> 424026 </td>
   <td style="text-align:right;"> 464555 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ��� </td>
   <td style="text-align:right;"> 204259 </td>
   <td style="text-align:right;"> 232031 </td>
   <td style="text-align:right;"> 255950 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ���� </td>
   <td style="text-align:right;"> 11105 </td>
   <td style="text-align:right;"> 13041 </td>
   <td style="text-align:right;"> 14990 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> �½� </td>
   <td style="text-align:right;"> 118896 </td>
   <td style="text-align:right;"> 134991 </td>
   <td style="text-align:right;"> 150812 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ���� </td>
   <td style="text-align:right;"> 163318 </td>
   <td style="text-align:right;"> 189255 </td>
   <td style="text-align:right;"> 208612 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> �㽭 </td>
   <td style="text-align:right;"> 549154 </td>
   <td style="text-align:right;"> 606609 </td>
   <td style="text-align:right;"> 653668 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ���� </td>
   <td style="text-align:right;"> 180746 </td>
   <td style="text-align:right;"> 204364 </td>
   <td style="text-align:right;"> 232230 </td>
  </tr>
</tbody>
</table>

```r
dtgdp$Prov <- as.factor(enc2native(dtgdp$Prov))
dtgdp$Year<- as.factor(dtgdp$Year)
```

### Area �����ע

�����������Σ�������ɫ�塣`subtitle_url`����Ϊ��������Դ��


```r
echartR(dtgdp, x = ~Prov, y = ~GDP, series= ~Year, 
        type=c('map','china','area'), palette='gdocs',
        title="GDPs of China Provinces, 2012-2014 (Million USD)",
        subtitle='(source: Wikipedia)',
        subtitle_url="https://raw.githubusercontent.com/madlogos/Shared_Doc/master/Shared_Documents/ChinaGDP.txt",
        dataRangePalette=c('red','orange','yellow','green','limegreen'),
        dataRange=c('High',"Low"),toolbox_pos=c('right','center'))
```

![](files/figure-html/map1.png)

������GDPǰ20�ֲ���ʾ�����ͼ���������з�Ϊ10��(`splitNumber=10`)��


```r
worldgdp <- data.frame(
    country=c('United States of America','China','Japan','Germany',
              'United Kingdom','France','Brazil', 'Italy','India','Russia',
              'Canada','Australia','South Korea','Spain','Mexico','Indonesia',
              'Netherlands','Turkey','Saudi Arabia','Switzerland'),
    GDP=c(17418925,10380380,4616335,3859547,2945146,2846889,2353025,2147952,
          2049501,1857461,1788717,1444189,1416949,1406855,1282725,888648,866354,
          806108,752459,712050))
echartR(worldgdp, x = ~country, y = ~GDP, type=c('map','world','area'),
        title="Nations with top 20 GDPs, 2014 (Million USD)",
        subtitle = '(source: Wikipedia)', 
        subtitle_url="https://en.wikipedia.org/wiki/List_of_countries_by_GDP_(nominal)",
        dataRangePalette='rainbow(5)', dataRange=c("High","Low"), 
        splitNumber=10, toolbox_pos=c('right','center'))
```

![](files/figure-html/map2.png)

### Point ���ע
��[China PM2.5���ݼ�](https://raw.githubusercontent.com/madlogos/Shared_Doc/master/Shared_Documents/China%20PM2.5.txt)���ص����أ�`readLines`���롣


```r
#chinapm2 <- readLines('https://raw.githubusercontent.com/madlogos/Shared_Doc/master/Shared_Documents/China%20PM2.5.txt')
chinapm25 <- enc2native(unlist(strsplit(paste0(readLines("China PM2.5.txt"), collapse=","),",")))
chinapm25 <- as.data.frame(matrix(chinapm25,byrow=T,ncol=4),stringsAsFactors=F)
names(chinapm25) <- c("City","PM25","ycoord","xcoord")
for (i in 2:4) chinapm25[,i] <- as.numeric(chinapm25[,i])
top5 <- head(chinapm25[order(chinapm25$PM25,decreasing=T),],5)
top5$Name <- "Top 5"
top5$effect <- T
top5 <- top5[,c(5,1,2,4,3,6)]
m <- list()
for (i in 1:5){
    m[[i]] <- unlist(top5[i,])
}

echartR(chinapm25, x=~City, y=~PM25, xcoord=~xcoord, ycoord=~ycoord,
        type=c('map','china','point'),title='PM2.5 in Chinese cities',
        subtitle="(source: PM25.in)",subtitle_url="http://pm25.in/",
        dataRange=c("High","Low"),
        dataRangePalette=c('maroon','red','orange','yellow','lightgreen','green'))
```

![](files/figure-html/map3.png)

## Wordcloud ����
ֱ��knitrʱreadLines��ȡ��ҳ�ᱨ����������ҳ 'http://top.baidu.com/buzz?b=1' ��Դ���뱣��Ϊ����txt���ٶ�ȡ��


```r
#baiduhot <- paste0(readLines("http://top.baidu.com/buzz?b=1"),collapse="")
baiduhot <- paste0(readLines("Baidu Hot Words.txt"),collapse="")
hotword <- gsub(".+?<a class=\"list-title\"[^>]+?>([^<>]+?)</a>.+?<span class=\"icon-(rise|fair|fall)\">(\\d+?)</span>.+?","\\1\t\\3\t",baiduhot)
hotword <- enc2native(gsub("^(.+?)\t{4,}.+$","\\1",hotword))
hotword <- t(matrix(unlist(strsplit(hotword,"\t")),nrow=2))
hotword <- as.data.frame(hotword,stringsAsFactors=F)
names(hotword) <- c("Keyword","Freq")
hotword$Freq <- as.numeric(hotword$Freq)
hotword <- hotword[order(hotword$Freq,decreasing=T),]
knitr::kable(hotword, format='html', row.names=F,
             caption="Table: Baidu hot words Aug 18 (descending order by Freq)")
```

<table>
<caption>Table: Baidu hot words Aug 18 (descending order by Freq)</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> Keyword </th>
   <th style="text-align:right;"> Freq </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> �����������ﲱ�� </td>
   <td style="text-align:right;"> 545761 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ���³���ǣ���� </td>
   <td style="text-align:right;"> 329896 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ��ͯ�ڳ������� </td>
   <td style="text-align:right;"> 194836 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ���췴�������˿ </td>
   <td style="text-align:right;"> 187192 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> �Ϻ���ָǿ������ </td>
   <td style="text-align:right;"> 185177 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ���Ӳ��⼷ѹ���� </td>
   <td style="text-align:right;"> 178935 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> �и��������� </td>
   <td style="text-align:right;"> 106216 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> �����ı��ձ��� </td>
   <td style="text-align:right;"> 103025 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ����Ϊ��ˢ��ռ�� </td>
   <td style="text-align:right;"> 96443 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Ů�Ӽ��ſڱ����� </td>
   <td style="text-align:right;"> 85931 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> �ܼ��쵼΢�ű��� </td>
   <td style="text-align:right;"> 83213 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ����˼�Ͻ�����Ȧ </td>
   <td style="text-align:right;"> 76823 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Ů�����о��� </td>
   <td style="text-align:right;"> 54671 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ���Ӵ�����ʧ���� </td>
   <td style="text-align:right;"> 43599 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> �й�����С����� </td>
   <td style="text-align:right;"> 39775 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ȫ���˾ӳ������� </td>
   <td style="text-align:right;"> 33747 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ��ս����ҷ��� </td>
   <td style="text-align:right;"> 33563 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ���ըͷ�� </td>
   <td style="text-align:right;"> 28963 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 9��3��˫������ </td>
   <td style="text-align:right;"> 27298 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ����Ů�ܽ�ͣ���� </td>
   <td style="text-align:right;"> 22560 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 65��Ů�Ӳ�4��̥ </td>
   <td style="text-align:right;"> 21674 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Ӣ������״Ԫ�ʼ� </td>
   <td style="text-align:right;"> 17742 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ��˼��������ʵ </td>
   <td style="text-align:right;"> 16641 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ������� </td>
   <td style="text-align:right;"> 16311 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ������ָ������ </td>
   <td style="text-align:right;"> 15738 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ���������ı�ը </td>
   <td style="text-align:right;"> 15444 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ��ͯ���뿪ˮ�� </td>
   <td style="text-align:right;"> 14827 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> �ǹ�����ִ�� </td>
   <td style="text-align:right;"> 13619 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ����������Ƭ�� </td>
   <td style="text-align:right;"> 12979 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2������ҹ������ </td>
   <td style="text-align:right;"> 12249 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ���������������� </td>
   <td style="text-align:right;"> 11928 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ����Ӧ�������� </td>
   <td style="text-align:right;"> 11305 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> �����緶������˿ </td>
   <td style="text-align:right;"> 9470 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ���غ��������� </td>
   <td style="text-align:right;"> 9115 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Ԭӽ�Ƕ�����ѧ </td>
   <td style="text-align:right;"> 7282 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ���ӵ���ע�䶾Ʒ </td>
   <td style="text-align:right;"> 6530 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> �����ѧѧ������ </td>
   <td style="text-align:right;"> 6488 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ֣��С������ƶ� </td>
   <td style="text-align:right;"> 6454 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ��Ծ�Ա����� </td>
   <td style="text-align:right;"> 6232 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ������û� </td>
   <td style="text-align:right;"> 6123 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ������������ĭ </td>
   <td style="text-align:right;"> 5884 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> �����Ϻ������� </td>
   <td style="text-align:right;"> 5137 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> �������ѱ�Ϳ�� </td>
   <td style="text-align:right;"> 4990 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> �ѳ���ɱ�������� </td>
   <td style="text-align:right;"> 4301 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ���̱�ָ�������� </td>
   <td style="text-align:right;"> 3722 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ��ʦ�ý��ͼ���ͯ </td>
   <td style="text-align:right;"> 3683 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ���ذ�������ʧ�� </td>
   <td style="text-align:right;"> 3538 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Ů�ӱ�Сѧ������ </td>
   <td style="text-align:right;"> 3245 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ȫ���˸����� </td>
   <td style="text-align:right;"> 3010 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ������������籭 </td>
   <td style="text-align:right;"> 2981 </td>
  </tr>
</tbody>
</table>


```r
echartR(hotword[1:30,], x=~Keyword, y=~Freq, type="wordcloud", 
        title="Baidu Word Search Top 30", palette=NULL,
        title_url="http://top.baidu.com/buzz?b=1", 
        subtitle="Tuesday, Auguest 18, 2015")
```

![](files/figure-html/wordcloud.png)

## Chord ����ͼ


## Force �����򲼾�ͼ


# Recognized Issues ��֪������

1. `echartR`�ȽϽӽ���`lattice`��������`ggplot2`ͼ����ӵ�˼·��ÿ��ʹ�ö�Ҫһ���Ա༭������Ҫ�Ĳ�����
1. ��������д�ñȽϱ��أ����������У�
1. ��û������ϵ�е�ʱ������ʾͼ�����ᱻ���һ�����ֽ��ַ���
1. δʵ�ֵĹ��ܣ�
    1. Force��Chord��candlestick(k)�͵��ע��ͼ��û�п�����
    1. ��Ȼ��֧��ʱ���ʽ�������ᣨseries�����ݽṹ�����⣩��
    1. ��Ȼ��֧�ֶ�̬ʱ���᣻
    1. tooltip�������ܣ�
    1. ���׹��ܣ�������ͼ������˫������ȣ���δ������
1. **ע��**����Ҫ�Ľ���ǧ��Ҫ�ں���������`set.seed()`�����ȫ������������������knitrʱÿ��һͼ���������������`htmlwidget id`�����յ��ĵ��У�ĳЩͼ���ܻ��޷���ָ�������ͼ�������ظ�������ͼ����id����
