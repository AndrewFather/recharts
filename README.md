# recharts

[![Build Status](https://travis-ci.org/yihui/recharts.svg)](https://travis-ci.org/yihui/recharts)

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

![](files/figure-html/1.png)

```r
echart(iris, ~Sepal.Length, ~Sepal.Width, series = ~Species)
```


![](files/figure-html/2.png)

# Usage �÷�
## Installation ��װ
- ��װ Install `devtools` (`install.packages('devtools')`)
- ��װ Insall recharts (`install_github('yihui/recharts')`)
- ���� Download `echartR.R`�ű��ļ�������λ�� script file to local disk:  [https://github.com/madlogos/recharts/blob/master/R/echartR.R](https://github.com/madlogos/recharts/blob/master/R/echartR.R))
- ���� Source `echartR` �ű� script (�����ҽ��ű����ڱ��� suppose I stored the script to local GitHub Repo: `source("~/Github/recharts/R/echartR.R")`)

## Grammar �﷨

```
echartR(data, x=NULL, y, z=NULL, series=NULL, weight=NULL, 
        type="scatter", stack=FALSE,
        title=NULL, subtitle=NULL, title_pos=c('center','bottom'),
        symbolList=NULL, dataZoom=NULL, dataZoomRange=NULL,
        dataRange=NULL, splitNumber=NULL, dataRangePalette=NULL,
        xlab=NULL, ylab=NULL, xyflip=FALSE, AxisAtZero=TRUE, scale=TRUE,
        palette='aetnagreen', tooltip=TRUE, legend=TRUE,
        legend_pos=c('center','top'), 
        toolbox=TRUE, toolbox_pos=c('right','top'), 
        calculable=TRUE, asImage=FALSE))
```

- **data**: ���ݼ� dataset
- x: x������ֱ��ͼ��ʡ�ԡ�x variable, only omitable for histograms��
- **y**: y���� y variable
- z: z������ֻ����ʱ��/���ڱ���������ʱ���ᡣz variable, only accept data/time variable to open time axis
- series: Series(ϵ��)���� series variable
- weight: Ȩ�ر�����������ֱ��ͼ������ͼ�� weight variable, used in histogram, bubble, etc
- type: Ĭ�� default `scatter`����ѡ options 'scatter', 'bubble', 'bar', 'line', 'linesmooth', 'map', 'k', 'pie', 'ring', 'area', 'areasmooth', 'chord', 'force', 'tree', 'treemap', 'wordcloud', 'heatmap', 'histogram', 'funnel', 'pyramid', 'radar', 'radarfill'
    - ��ѡ��map������������д��һ������Ϊ3��������c('map',`mapType`,`area/point`)��mapType��ѡ'world'��'china'����������ı�ʾ�ľ����й�������area/pointΪareaʱ����������ɫ��ʾЧӦ��С��Ϊpointʱ���õ��ڵ�ͼ������ע��Ĭ��Ϊc('map','china','area')��If `map` was chosen, the control option should be a vector of length 3: c('map',`mapType`,`area/point`). `mapType` could be either 'world' or 'china', of which simplified Chinese names are required for 'china'. When `area/point` equals to 'area', the function colors polygons to show the effects; while equals to 'point', it ticks droplets on the map.
- stack: Ĭ��FALSE���Ƿ�ѻ������������ѻ���ͼ����ͼ����ͼ�����ͼ�ȡ�Default to FALSE (do not stack). Used in stacked hbar/vbar, line and area chart, etc.
- title: ���� 
- subtitle: ������
- title_pos: ����λ�ã�����c('left|center|right','top|center|bottom')��Ĭ��c("center","bottom")��ʾ�ײ�����λ�á�Legend position which is a vector of length 2 (c('left|center|right','top|center|bottom')). Default to c('center','bottom') which means bottom meiddle.
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
- palette: Ĭ��aetnagreen��ʹ�õ�ɫ�塣Overall palette. Default to 'aetnagreen'.
    - ���� Palette names:
        - Aetna palettes: ���� Including 'aetnagreen', 'aetnablue', 'aetnaviolet', 'aetnaorange', 'aetnateal', 'aetnacranberry'
        - RColorBrewer palettes: ���� Including 'BrBG', 'PiYG', 'PRGn', 'PuOr', 'RdBu', 'RdGy', 'RdYlBu', 'RdYlGn', 'Spectral', 'Accent', 'Dark2', 'Paired', 'Pastel1', 'Pastel2', 'Set1', 'Set2', 'Set3', 'Blues', 'BuGn', 'BuPu', 'GnBu', 'Greens', 'Greys', 'Oranges', 'OrRd', 'PuBu', 'PuBuGn', 'PuRd', 'Purples', 'RdPu', 'Reds', 'YlGn', 'YlGnBu', 'YlOrBr', 'YlOrRd'
        - ggthemes palettes: 'calc', 'economist', 'economist_white', 'excel', 'few', 'fivethirtyeight', 'gdocs', 'pander', 'tableau', 'stata', 'tableau20', 'tableau10medium', 'tableaugray', 'tableauprgy', 'tableaublrd', 'tableaugnor', 'tableaucyclic', 'tableau10light', 'tableaublrd12','tableauprgy12', 'tableaugnor12','hc','darkunica', 'solarized','solarized_red', 'solarized_yellow', 'solarized_orange','solarized_magenta','solarized_violet', 'solarized_blue', 'solarized_cyan', 'solarized_green', 'wsj','colorblind', 'trafficlight'
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
- asImage: Ĭ��FALSE���Ƿ����̬ͼ��renderAsImage switch.Deafult to FALSE.

# Examples ʾ��

**GitHub��ֻ�ܷž�̬ͼ����ʵ`d3-js`Ч��ͼ����[�������](http://pan.baidu.com/s/1o6pDTf8)**

```r
Sys.setlocale("LC_CTYPE","Chs")
source("~/Github/recharts/R/echartR.R")
#source("C:/HMSProjects/Data Analytics/R_scripts/CommonFunctions.R")
knitr::opts_chunk$set(message=FALSE,warning=FALSE,results='asis')
```

## Scatter ɢ��ͼ

### Singular-series Scatter ��ϵ��ɢ��ͼ


```r
echartR(data = iris, x = ~Sepal.Width, y = ~Petal.Width, 
        type = 'scatter', palette='solarized_magenta',
        title = 'Scatter - Sepal Width vs Petal Width (iris)', 
        xlab = 'Sepal Width', ylab = 'Petal Width')
```

![](files/figure-html/3.png)

### Multi-series Scatter ��ϵ��ɢ��ͼ

ָ��series������ʾ��Χ����㿪ʼ(`scale=FALSE`)


```r
echartR(data = iris, x = ~Sepal.Width, y = ~Petal.Width, series = ~Species,
        type = 'scatter', palette='aetnaviolet', symbolList='circle', scale=F,
        title = 'Scatter - Sepal Width vs Petal Width, by Species (iris)',
        xlab = 'Sepal Width', ylab = 'Petal Width')
```

![](files/figure-html/4.png)

ʹ�����׷Ǳ�׼ͼ��(��ͷ�����Ρ��˽���)��������ϵ��


```r
echartR(data = iris, x = ~Sepal.Width, y = ~Petal.Width, series = ~Species,
        type = 'scatter', palette='aetnateal',
        symbolList=c('arrow','heart','star8'),
        title = 'Scatter - Sepal Width vs Petal Width, by Species (iris)',
        xlab = 'Sepal Width', ylab = 'Petal Width')
```

![](files/figure-html/5.png)

## Bubble ����ͼ

����ͼͬ����Դ��ɢ��ͼ��type��Ϊ`bubble`���ɣ��粻ָ��`weight`����������Ĭ��ָ��yΪ����Ȩ�ء�

### Singular-series Bubble ��ϵ������ͼ


```r
echartR(data = iris, x = ~Sepal.Width, y = ~Petal.Width, 
        weight = ~Petal.Length,
        type = 'bubble', palette='solarized_cyan',
        title = paste("Bubble - Sepal Width vs Petal Width,",
                      "weighed by Petal Length (iris)"),
        xlab = 'Sepal Width', ylab = 'Petal Width')
```

![](files/figure-html/6.png)

### Multi-series Bubble ��ϵ������ͼ

��`symbolList=c('circle','emptyCircle')`���õ�1������ϵ��Ϊʵ��Բ��ʣ�µ�����ϵ���Կ���Բ��Ϊ��־ͼ�Ρ�palette����ΪtableauGnOr(3)��ֻ���ȡ��ɫ���3����ɫ��


```r
echartR(data = iris, x = ~Sepal.Width, y = ~Petal.Width, 
        weight = ~Petal.Length, series = ~Species, 
        symbolList=c('emptyCircle','circle'),
        type = 'bubble', palette='tableaugnor(3)',
        title = paste('Bubble - Sepal Width vs Petal Width, by Species,',
                      'weighed by Petal Length (iris)'), 
        xlab = 'Sepal Width', ylab = 'Petal Width')
```

![](files/figure-html/7.png)

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
        title = paste("VBar - Parameter Mean by Species", "(iris)"), 
        xlab = 'Parameter', ylab = 'Mean', legend_pos=c('right','center'))
```

![](files/figure-html/8.png)

### Stacked Column �ѻ���ͼ


```r
echartR(data = dtiris, x = ~Param, y = ~Mean, 
        series = ~Species, stack=T,
        type = 'bar', palette='pander',
        title = paste("VBar - Parameter Mean by Species", "(iris)"), 
        xlab = 'Parameter', ylab = 'Mean', legend_pos=c('right','center'))
```

![](files/figure-html/9.png)

�ѻ����Ҳ���Լ򵥵�ͨ����������`ƽ��`��`�ѻ�`��ť�л����ǳ�ǿ��(��Ҳ���ں�������������ȷ���������)��

## Bar ��ͼ

��ͼ����ͼ������ֻ����`xyflip`����ѡ�

### Tiled Bar ƽ����ͼ


```r
echartR(data = dtiris, x = ~Species, y = ~Mean, series = ~Param, 
        type = 'bar', palette='stata', xyflip=T,
        title = 'Species-specific Mean by Parameters (iris)', 
        xlab = 'Species', ylab = 'Mean', legend_pos=c('right','center'))
```

![](files/figure-html/10.png)

### Stacked Bar �ѻ���ͼ

palette��Ϊcalc�����4����ɫ����stack (`stack=TRUE`)��


```r
echartR(data = dtiris, x = ~Param, y = ~Mean, 
        series = ~Species, stack=T, xyflip=T,
        type = 'bar', palette='calc',
        title = 'Parameter Mean by Species (iris)', 
        xlab = 'Parameter', ylab = 'Mean', legend_pos=c('right','center'))
```

![](files/figure-html/11.png)

## Histogram ֱ��ͼ

ֱ��ͼ����ͼ��һ��������ֻ��Ҫָ��y��������ͨ��`splitNumber`ָ��ֱ����(Ĭ��9)��`xyflip`��ΪTRUE���Ϊ�ȼ۵ĺ���ͼ��

> Echarts�涨��/��ͼ��ɢ��ͼ���Ա������Ϊcategory���ͣ������ʱ�޷��򵥵�������ͳ��۵�ֱ��ͼ��


```r
echartR(airquality, y=~Temp, type='histogram', splitNumber=13,
        palette='pastel2', title='Histogram of temperature (airquality)')
```

![](files/figure-html/12.png)

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
        title='Number of cars by transmission (mtcars)')
```

![](files/figure-html/13.png)

## Ring ��ͼ

����ͼ�Ǳ�ͼ�ı��Σ�ֻ�轫`type`��Ϊ'ring'��Echarts��ֻ��Ҫ�ѱ�ͼ�İ뾶������չΪ�����ڡ��⾶�ĳ���Ϊ2���������ɡ�


```r
echartR(dtcars, x = ~cylinder,  y = ~car, type='ring',
        palette='hc', title='Number of Cylinders (mtcars)')
```

![](files/figure-html/14.png)

## Line ��ͼ

### Unstacked Line ƽ����ͼ

����������`dataZoom=T`


```r
data(airquality)
airquality$Date <- strptime(paste(2015,airquality$Month,airquality$Day,sep="-")
                            ,format="%Y-%m-%d")
airquality$strDate <- with(airquality,paste(2015,Month,Day,sep="-"))
airquality$TempG <- cut(airquality$Temp,breaks=c(0,60,70,80,100))
echartR(airquality, x = ~Day, y= ~Wind, series=~Month,
          type='line', dataZoom=T, dataZoomRange=c(30,70),
        palette='tableauBlRd', xlab = 'Days', ylab = 'Wind',
        title='Day-specific Wind by month (airquality)', symbolList='none')
```

![](files/figure-html/15.png)

�߶�ƽ��(`type='linesmooth'`)������ʾ��־ͼ��(`symbolList='none'`)��Echarts��ȱʧֵĬ�ϲ��������кܶ���ߡ���Ҫ������ǰ����ʱ��������ֵ���㡣


```r
airq <- melt(airquality[,c("Ozone","Solar.R","Wind","Temp","strDate")],
             id=c("strDate"))
echartR(airq, x = ~strDate, y= ~value, series= ~variable, type='linesmooth',
        symbolList='none', dataZoom=T, dataZoomRange=c(20,50),
        palette='tableauPrGy', xlab = 'Date', ylab = 'Measure',
        title='Climate measures by day (airquality)')
```

![](files/figure-html/16.png)

### Stacked Line �ѻ���ͼ

```r
echartR(dfiris, x = ~id, y= ~Value, series= ~Param, type='line',stack=T,
        palette='tableauBlRd12', xlab = 'Sample ID', ylab = 'Measure',
        title='Parameter measures (iris)')
```

![](files/figure-html/17.png)

�߶�ƽ��������ʾ��־ͼ��

```r
echartR(dfiris, x = ~id, y= ~Value, series= ~Param, type='linesmooth',stack=T,
        palette='tableauGnOr12', xlab = 'Sample ID', ylab = 'Measure',
        symbolList='none',
        title='Parameter measures (iris)')
```

![](files/figure-html/18.png)

## Area ���ͼ
Echarts�У����ͼ�����ϱ�����Ϊ��ͼ��ֻ��ͨ��`itemStyle`������Ⱦ��ɫ��

### Tiled Area ƽ�����ͼ

```r
echartR(dfiris, x = ~id, y= ~Value, series= ~Param, type='area',
        palette='brbg', xlab = 'Sample ID', ylab = 'Measure',
        symbolList='emptyDiamond',title='Parameter measures (iris)')
```


![](files/figure-html/19.png)

�߶�ƽ��`type='areasmooth'`��������������`dataZoom=TRUE`����ʼ��ʾ40%-80%��

```r
echartR(dfiris, x = ~id, y= ~Value, series= ~Param, type='areasmooth',
        palette='PiYG', xlab = 'Sample ID', ylab = 'Measure', 
        symbolList='none', dataZoom=T, dataZoomRange=c(40,80),
        title='Parameter measures (iris)')
```


![](files/figure-html/20.png)

### Stacked Area �ѻ����ͼ

```r
echartR(dfiris, x = ~id, y= ~Value, series= ~Param, type='area',stack=T,
        palette='PRGn', xlab = 'Sample ID', ylab = 'Measure',
        symbolList='emptyCircle',
        title='Parameter measures (iris)')
```

![](files/figure-html/21.png)

�߶�ƽ��(`type='areasmooth'`)���Զ���ɫ��������

```r
echartR(dfiris, x = ~id, y= ~Value, series= ~Param, type='areasmooth',stack=T,
        palette=c('red','yellow','limegreen','skyblue'), 
        xlab = 'Sample ID', ylab = 'Measure', 
        symbolList='none',
        title='Parameter measures (iris)')
```

![](files/figure-html/22.png)

## Funnel ©��ͼ

### Funnel ��ͨ©��ͼ


```r
echartR(dtcars, x = ~carburetor,  y = ~car, type='funnel',
        palette='RdBu', title='Number of carburetors of cars (mtcars)')
```

![](files/figure-html/23.png)

### Pyramid ������ͼ
������ͼ������©��ͼ��


```r
echartR(dtcars, x = ~carburetor,  y = ~car, type='pyramid',
        palette='RdGy', title='Number of carburetors of cars (mtcars)')
```

![](files/figure-html/24.png)

## Radar �״�ͼ

�״�ͼ���Ǽ�����ϵ�µ���ͼ/���ͼ��ͨ��Echarts��`polar`����ģ����ơ�

```r
player <- data.frame(name=c(rep("Philipp Lahm",8),rep("Dani Alves",8)),
                     para=rep(c("Passing%","Key passing","Comp crosses",
                                "Crossing%","Successful dribbles",
                                "Dispossessed","Dribbled past","Fouls"),2),
                     value=c(89.67, 1.51, 0.97, 24.32, 0.83, 0.86, 1.15, 0.47,
                            86.62, 2.11, 0.99, 20.78, 1.58, 1.64, 0.9, 1.71))
```

### Hollow Radar �����״�


```r
echartR(player, x= ~para, y= ~value, series= ~name, type='radar',
        symbolList='none', palette=c('red','blue'),
        title='Lahm vs Alves (by @mixedknuts)')
```

![](files/figure-html/25.png)

### Solid Radar ʵ���״�


```r
echartR(player, x= ~para, y= ~value, series= ~name, type='radarfill',
        symbolList='none', palette=c('firebrick1','dodgerblue'),
        title='Lahm vs Alves (by @mixedknuts)')
```

![](files/figure-html/26.png)

## Map ��ͼ

R��Rstudio������ת����Windows��һֱ���ϴ��ѡ��ڱ��ĵ���Rstudio��Ĭ�ϱ������CP936���룬�ĵ���UTF-8����ֱ���ڳ����ڶ����ݼ�����iconvת����䵥�ĺ�����Ȼ����ʾΪ���롣��������[ChinaGDP.txt](https://raw.githubusercontent.com/madlogos/Shared_Doc/master/Shared_Documents/ChinaGDP.txt)�����أ�`readLines`���롣


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

�����������Σ�������ɫ�塣


```r
echartR(dtgdp, x = ~Prov, y = ~GDP, series= ~Year, 
        type=c('map','china','area'), palette='gdocs',
        title="GDPs of China Provinces, 2012-2014 (Million USD)",
        dataRangePalette=c('red','orange','yellow','green','limegreen'),
        dataRange=c('High',"Low"),toolbox_pos=c('right','center'))
```

![](files/figure-html/27.png)

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
        title="Nations with top 20 GDPs, 2014 (Million USD) - Wikipedia",
        dataRangePalette='rainbow(5)', dataRange=c("High","Low"), 
        splitNumber=10, toolbox_pos=c('right','center'))
```

![](files/figure-html/28.png)

### Point ���ע


## Chord ����ͼ


## Force �����򲼾�ͼ

# Recognized Issues ��֪������

1. `echartR`�ȽϽӽ���`lattice`��������`ggplot2`ͼ����ӵ�˼·��ÿ��ʹ�ö�Ҫһ���Ա༭�ܶ������
1. ��������д�ñȽϱ��أ����������У�
1. δʵ�ֵĹ��ܣ�
    1. Force��Chord��Wordcloud��candlestick(k)�͵��ע��ͼ��û�п�����
    1. ��Ȼ��֧��ʱ���ʽ�������ᣨseries�����ݸ�ʽ�����⣩��
    1. ��Ȼ��֧�ֶ�̬ʱ���᣻
    1. tooltip�������ܣ�
    1. ���׹��ܣ�������ͼ������˫������ȣ���δ������
1. **ע��**����Ҫ�Ľ���ǧ��Ҫ�ں���������`set.seed()`�����ȫ������������������knitrʱÿ��һͼ���������������`htmlwidget id`�����յ��ĵ��У�ĳЩͼ���޷���ָ�������ͼ�������ظ�������ͼ����id����