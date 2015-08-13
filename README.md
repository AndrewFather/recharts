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

<!--html_preserve--><div id="htmlwidget-1616" style="width:768px;height:480px;" class="echarts"></div>
<script type="application/json" data-for="htmlwidget-1616">{"x":{"series":[{"type":"scatter","data":[[5.1,3.5],[4.9,3],[4.7,3.2],[4.6,3.1],[5,3.6],[5.4,3.9],[4.6,3.4],[5,3.4],[4.4,2.9],[4.9,3.1],[5.4,3.7],[4.8,3.4],[4.8,3],[4.3,3],[5.8,4],[5.7,4.4],[5.4,3.9],[5.1,3.5],[5.7,3.8],[5.1,3.8],[5.4,3.4],[5.1,3.7],[4.6,3.6],[5.1,3.3],[4.8,3.4],[5,3],[5,3.4],[5.2,3.5],[5.2,3.4],[4.7,3.2],[4.8,3.1],[5.4,3.4],[5.2,4.1],[5.5,4.2],[4.9,3.1],[5,3.2],[5.5,3.5],[4.9,3.6],[4.4,3],[5.1,3.4],[5,3.5],[4.5,2.3],[4.4,3.2],[5,3.5],[5.1,3.8],[4.8,3],[5.1,3.8],[4.6,3.2],[5.3,3.7],[5,3.3],[7,3.2],[6.4,3.2],[6.9,3.1],[5.5,2.3],[6.5,2.8],[5.7,2.8],[6.3,3.3],[4.9,2.4],[6.6,2.9],[5.2,2.7],[5,2],[5.9,3],[6,2.2],[6.1,2.9],[5.6,2.9],[6.7,3.1],[5.6,3],[5.8,2.7],[6.2,2.2],[5.6,2.5],[5.9,3.2],[6.1,2.8],[6.3,2.5],[6.1,2.8],[6.4,2.9],[6.6,3],[6.8,2.8],[6.7,3],[6,2.9],[5.7,2.6],[5.5,2.4],[5.5,2.4],[5.8,2.7],[6,2.7],[5.4,3],[6,3.4],[6.7,3.1],[6.3,2.3],[5.6,3],[5.5,2.5],[5.5,2.6],[6.1,3],[5.8,2.6],[5,2.3],[5.6,2.7],[5.7,3],[5.7,2.9],[6.2,2.9],[5.1,2.5],[5.7,2.8],[6.3,3.3],[5.8,2.7],[7.1,3],[6.3,2.9],[6.5,3],[7.6,3],[4.9,2.5],[7.3,2.9],[6.7,2.5],[7.2,3.6],[6.5,3.2],[6.4,2.7],[6.8,3],[5.7,2.5],[5.8,2.8],[6.4,3.2],[6.5,3],[7.7,3.8],[7.7,2.6],[6,2.2],[6.9,3.2],[5.6,2.8],[7.7,2.8],[6.3,2.7],[6.7,3.3],[7.2,3.2],[6.2,2.8],[6.1,3],[6.4,2.8],[7.2,3],[7.4,2.8],[7.9,3.8],[6.4,2.8],[6.3,2.8],[6.1,2.6],[7.7,3],[6.3,3.4],[6.4,3.1],[6,3],[6.9,3.1],[6.7,3.1],[6.9,3.1],[5.8,2.7],[6.8,3.2],[6.7,3.3],[6.7,3],[6.3,2.5],[6.5,3],[6.2,3.4],[5.9,3]]}],"xAxis":{"type":"value","show":true,"position":"bottom","name":"Sepal.Length","nameLocation":"end","nameTextStyle":{},"boundaryGap":[0,0],"scale":true,"axisLine":{"show":true,"onZero":false},"axisTick":{"show":false},"axisLabel":{"show":true},"splitLine":{"show":true},"splitArea":{"show":false},"data":[]},"yAxis":{"type":"value","show":true,"position":"left","name":"Sepal.Width","nameLocation":"end","nameTextStyle":{},"boundaryGap":[0,0],"scale":true,"axisLine":{"show":true,"onZero":false},"axisTick":{"show":false},"axisLabel":{"show":true},"splitLine":{"show":true},"splitArea":{"show":false},"data":[]}},"evals":[]}</script><!--/html_preserve-->

```r
echart(iris, ~Sepal.Length, ~Sepal.Width, series = ~Species)
```

<!--html_preserve--><div id="htmlwidget-686" style="width:768px;height:480px;" class="echarts"></div>
<script type="application/json" data-for="htmlwidget-686">{"x":{"series":[{"name":"setosa","type":"scatter","data":[[5.1,3.5],[4.9,3],[4.7,3.2],[4.6,3.1],[5,3.6],[5.4,3.9],[4.6,3.4],[5,3.4],[4.4,2.9],[4.9,3.1],[5.4,3.7],[4.8,3.4],[4.8,3],[4.3,3],[5.8,4],[5.7,4.4],[5.4,3.9],[5.1,3.5],[5.7,3.8],[5.1,3.8],[5.4,3.4],[5.1,3.7],[4.6,3.6],[5.1,3.3],[4.8,3.4],[5,3],[5,3.4],[5.2,3.5],[5.2,3.4],[4.7,3.2],[4.8,3.1],[5.4,3.4],[5.2,4.1],[5.5,4.2],[4.9,3.1],[5,3.2],[5.5,3.5],[4.9,3.6],[4.4,3],[5.1,3.4],[5,3.5],[4.5,2.3],[4.4,3.2],[5,3.5],[5.1,3.8],[4.8,3],[5.1,3.8],[4.6,3.2],[5.3,3.7],[5,3.3]]},{"name":"versicolor","type":"scatter","data":[[7,3.2],[6.4,3.2],[6.9,3.1],[5.5,2.3],[6.5,2.8],[5.7,2.8],[6.3,3.3],[4.9,2.4],[6.6,2.9],[5.2,2.7],[5,2],[5.9,3],[6,2.2],[6.1,2.9],[5.6,2.9],[6.7,3.1],[5.6,3],[5.8,2.7],[6.2,2.2],[5.6,2.5],[5.9,3.2],[6.1,2.8],[6.3,2.5],[6.1,2.8],[6.4,2.9],[6.6,3],[6.8,2.8],[6.7,3],[6,2.9],[5.7,2.6],[5.5,2.4],[5.5,2.4],[5.8,2.7],[6,2.7],[5.4,3],[6,3.4],[6.7,3.1],[6.3,2.3],[5.6,3],[5.5,2.5],[5.5,2.6],[6.1,3],[5.8,2.6],[5,2.3],[5.6,2.7],[5.7,3],[5.7,2.9],[6.2,2.9],[5.1,2.5],[5.7,2.8]]},{"name":"virginica","type":"scatter","data":[[6.3,3.3],[5.8,2.7],[7.1,3],[6.3,2.9],[6.5,3],[7.6,3],[4.9,2.5],[7.3,2.9],[6.7,2.5],[7.2,3.6],[6.5,3.2],[6.4,2.7],[6.8,3],[5.7,2.5],[5.8,2.8],[6.4,3.2],[6.5,3],[7.7,3.8],[7.7,2.6],[6,2.2],[6.9,3.2],[5.6,2.8],[7.7,2.8],[6.3,2.7],[6.7,3.3],[7.2,3.2],[6.2,2.8],[6.1,3],[6.4,2.8],[7.2,3],[7.4,2.8],[7.9,3.8],[6.4,2.8],[6.3,2.8],[6.1,2.6],[7.7,3],[6.3,3.4],[6.4,3.1],[6,3],[6.9,3.1],[6.7,3.1],[6.9,3.1],[5.8,2.7],[6.8,3.2],[6.7,3.3],[6.7,3],[6.3,2.5],[6.5,3],[6.2,3.4],[5.9,3]]}],"xAxis":{"type":"value","show":true,"position":"bottom","name":"Sepal.Length","nameLocation":"end","nameTextStyle":{},"boundaryGap":[0,0],"scale":true,"axisLine":{"show":true,"onZero":false},"axisTick":{"show":false},"axisLabel":{"show":true},"splitLine":{"show":true},"splitArea":{"show":false},"data":[]},"yAxis":{"type":"value","show":true,"position":"left","name":"Sepal.Width","nameLocation":"end","nameTextStyle":{},"boundaryGap":[0,0],"scale":true,"axisLine":{"show":true,"onZero":false},"axisTick":{"show":false},"axisLabel":{"show":true},"splitLine":{"show":true},"splitArea":{"show":false},"data":[]},"legend":{"data":["setosa","versicolor","virginica"]}},"evals":[]}</script><!--/html_preserve-->

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

<!--html_preserve--><div id="htmlwidget-7888" style="width:768px;height:480px;" class="echarts"></div>
<script type="application/json" data-for="htmlwidget-7888">{"x":{"title":{"text":"Scatter - Sepal Width vs Petal Width (iris)","subtext":"","padding":[25,5,5,5],"x":"center","y":"bottom"},"tooltip":{"trigger":"axis","axisPointer":{"show":true,"type":"cross","lineStyle":{"type":"dashed","width":1}}},"toolbox":{"show":true,"feature":{"mark":{"show":true,"title":{"mark":"辅助线开�? Auxiliary conductor switch","markUndo":"删除辅助�? Undo auxiliary conductor","markClear":"清空辅助�? Clear auxiliary conductor"}},"dataView":{"show":true,"readOnly":false,"title":"数据视图 Data view"},"magicType":{"show":false},"restore":{"show":true,"title":"还原 Restore"},"saveAsImage":{"show":true,"title":"保存为图�? Save as image"}},"x":"right","y":"top"},"calculable":true,"series":[{"type":"scatter","data":[[3.5,0.2],[3,0.2],[3.2,0.2],[3.1,0.2],[3.6,0.2],[3.9,0.4],[3.4,0.3],[3.4,0.2],[2.9,0.2],[3.1,0.1],[3.7,0.2],[3.4,0.2],[3,0.1],[3,0.1],[4,0.2],[4.4,0.4],[3.9,0.4],[3.5,0.3],[3.8,0.3],[3.8,0.3],[3.4,0.2],[3.7,0.4],[3.6,0.2],[3.3,0.5],[3.4,0.2],[3,0.2],[3.4,0.4],[3.5,0.2],[3.4,0.2],[3.2,0.2],[3.1,0.2],[3.4,0.4],[4.1,0.1],[4.2,0.2],[3.1,0.2],[3.2,0.2],[3.5,0.2],[3.6,0.1],[3,0.2],[3.4,0.2],[3.5,0.3],[2.3,0.3],[3.2,0.2],[3.5,0.6],[3.8,0.4],[3,0.3],[3.8,0.2],[3.2,0.2],[3.7,0.2],[3.3,0.2],[3.2,1.4],[3.2,1.5],[3.1,1.5],[2.3,1.3],[2.8,1.5],[2.8,1.3],[3.3,1.6],[2.4,1],[2.9,1.3],[2.7,1.4],[2,1],[3,1.5],[2.2,1],[2.9,1.4],[2.9,1.3],[3.1,1.4],[3,1.5],[2.7,1],[2.2,1.5],[2.5,1.1],[3.2,1.8],[2.8,1.3],[2.5,1.5],[2.8,1.2],[2.9,1.3],[3,1.4],[2.8,1.4],[3,1.7],[2.9,1.5],[2.6,1],[2.4,1.1],[2.4,1],[2.7,1.2],[2.7,1.6],[3,1.5],[3.4,1.6],[3.1,1.5],[2.3,1.3],[3,1.3],[2.5,1.3],[2.6,1.2],[3,1.4],[2.6,1.2],[2.3,1],[2.7,1.3],[3,1.2],[2.9,1.3],[2.9,1.3],[2.5,1.1],[2.8,1.3],[3.3,2.5],[2.7,1.9],[3,2.1],[2.9,1.8],[3,2.2],[3,2.1],[2.5,1.7],[2.9,1.8],[2.5,1.8],[3.6,2.5],[3.2,2],[2.7,1.9],[3,2.1],[2.5,2],[2.8,2.4],[3.2,2.3],[3,1.8],[3.8,2.2],[2.6,2.3],[2.2,1.5],[3.2,2.3],[2.8,2],[2.8,2],[2.7,1.8],[3.3,2.1],[3.2,1.8],[2.8,1.8],[3,1.8],[2.8,2.1],[3,1.6],[2.8,1.9],[3.8,2],[2.8,2.2],[2.8,1.5],[2.6,1.4],[3,2.3],[3.4,2.4],[3.1,1.8],[3,1.8],[3.1,2.1],[3.1,2.4],[3.1,2.3],[2.7,1.9],[3.2,2.3],[3.3,2.5],[3,2.3],[2.5,1.9],[3,2],[3.4,2.3],[3,1.8]],"large":false}],"renderAsImage":true,"color":["#d33682","#b58900","#cb4b16","#dc322f","#6c71c4","#268bd2","#2aa198","#859900"],"xAxis":{"name":"Sepal Width","type":"value","boundaryGap":[0,0],"scale":true,"axisLine":{"show":true,"onZero":false}},"yAxis":{"name":"Petal Width","type":"value","scale":true,"axisLine":{"show":true,"onZero":false}}},"evals":[]}</script><!--/html_preserve-->

### Multi-series Scatter ��ϵ��ɢ��ͼ

ָ��series������ʾ��Χ����㿪ʼ(`scale=FALSE`)


```r
echartR(data = iris, x = ~Sepal.Width, y = ~Petal.Width, series = ~Species,
        type = 'scatter', palette='aetnaviolet', symbolList='circle', scale=F,
        title = 'Scatter - Sepal Width vs Petal Width, by Species (iris)',
        xlab = 'Sepal Width', ylab = 'Petal Width')
```

<!--html_preserve--><div id="htmlwidget-3279" style="width:768px;height:480px;" class="echarts"></div>
<script type="application/json" data-for="htmlwidget-3279">{"x":{"title":{"text":"Scatter - Sepal Width vs Petal Width, by Species (iris)","subtext":"","padding":[25,5,5,5],"x":"center","y":"bottom"},"tooltip":{"trigger":"axis","axisPointer":{"show":true,"type":"cross","lineStyle":{"type":"dashed","width":1}},"formatter":"function (params) {\n                                                if (params.value.length > 1) {\n                                                return params.seriesName + \" :<br/>\"\n                                                + params.value[0] + \" ,    \" +\n                                                + params.value[1];\n                                                } else {\n                                                return params.seriesName + \" :<br/>\"\n                                                + params.name + \" : \"\n                                                + params.value;\n                                                }}"},"toolbox":{"show":true,"feature":{"mark":{"show":true,"title":{"mark":"辅助线开�? Auxiliary conductor switch","markUndo":"删除辅助�? Undo auxiliary conductor","markClear":"清空辅助�? Clear auxiliary conductor"}},"dataView":{"show":true,"readOnly":false,"title":"数据视图 Data view"},"magicType":{"show":false},"restore":{"show":true,"title":"还原 Restore"},"saveAsImage":{"show":true,"title":"保存为图�? Save as image"}},"x":"right","y":"top"},"calculable":true,"series":[{"type":"scatter","data":[[3.5,0.2],[3,0.2],[3.2,0.2],[3.1,0.2],[3.6,0.2],[3.9,0.4],[3.4,0.3],[3.4,0.2],[2.9,0.2],[3.1,0.1],[3.7,0.2],[3.4,0.2],[3,0.1],[3,0.1],[4,0.2],[4.4,0.4],[3.9,0.4],[3.5,0.3],[3.8,0.3],[3.8,0.3],[3.4,0.2],[3.7,0.4],[3.6,0.2],[3.3,0.5],[3.4,0.2],[3,0.2],[3.4,0.4],[3.5,0.2],[3.4,0.2],[3.2,0.2],[3.1,0.2],[3.4,0.4],[4.1,0.1],[4.2,0.2],[3.1,0.2],[3.2,0.2],[3.5,0.2],[3.6,0.1],[3,0.2],[3.4,0.2],[3.5,0.3],[2.3,0.3],[3.2,0.2],[3.5,0.6],[3.8,0.4],[3,0.3],[3.8,0.2],[3.2,0.2],[3.7,0.2],[3.3,0.2]],"large":false,"name":"setosa"},{"type":"scatter","data":[[3.2,1.4],[3.2,1.5],[3.1,1.5],[2.3,1.3],[2.8,1.5],[2.8,1.3],[3.3,1.6],[2.4,1],[2.9,1.3],[2.7,1.4],[2,1],[3,1.5],[2.2,1],[2.9,1.4],[2.9,1.3],[3.1,1.4],[3,1.5],[2.7,1],[2.2,1.5],[2.5,1.1],[3.2,1.8],[2.8,1.3],[2.5,1.5],[2.8,1.2],[2.9,1.3],[3,1.4],[2.8,1.4],[3,1.7],[2.9,1.5],[2.6,1],[2.4,1.1],[2.4,1],[2.7,1.2],[2.7,1.6],[3,1.5],[3.4,1.6],[3.1,1.5],[2.3,1.3],[3,1.3],[2.5,1.3],[2.6,1.2],[3,1.4],[2.6,1.2],[2.3,1],[2.7,1.3],[3,1.2],[2.9,1.3],[2.9,1.3],[2.5,1.1],[2.8,1.3]],"large":false,"name":"versicolor"},{"type":"scatter","data":[[3.3,2.5],[2.7,1.9],[3,2.1],[2.9,1.8],[3,2.2],[3,2.1],[2.5,1.7],[2.9,1.8],[2.5,1.8],[3.6,2.5],[3.2,2],[2.7,1.9],[3,2.1],[2.5,2],[2.8,2.4],[3.2,2.3],[3,1.8],[3.8,2.2],[2.6,2.3],[2.2,1.5],[3.2,2.3],[2.8,2],[2.8,2],[2.7,1.8],[3.3,2.1],[3.2,1.8],[2.8,1.8],[3,1.8],[2.8,2.1],[3,1.6],[2.8,1.9],[3.8,2],[2.8,2.2],[2.8,1.5],[2.6,1.4],[3,2.3],[3.4,2.4],[3.1,1.8],[3,1.8],[3.1,2.1],[3.1,2.4],[3.1,2.3],[2.7,1.9],[3.2,2.3],[3.3,2.5],[3,2.3],[2.5,1.9],[3,2],[3.4,2.3],[3,1.8]],"large":false,"name":"virginica"}],"renderAsImage":true,"color":["#7D3F98","#7AC143","#F47721","#00A78E","#00BCE4","#D20962","#F58F9F","#B8D936","#FDB933","#60C3AE","#5F78BB","#EE3D94","#5E9732","#CEA979","#EF4135","#7090A5"],"symbolList":["circle","circle","circle"],"legend":{"show":true,"data":["setosa","versicolor","virginica"],"x":"left","y":"top"},"xAxis":{"name":"Sepal Width","type":"value","boundaryGap":[0,0],"scale":false,"axisLine":{"show":true,"onZero":false}},"yAxis":{"name":"Petal Width","type":"value","scale":false,"axisLine":{"show":true,"onZero":false}}},"evals":["tooltip.formatter"]}</script><!--/html_preserve-->

ʹ�����׷Ǳ�׼ͼ��(��ͷ�����Ρ��˽���)��������ϵ��


```r
echartR(data = iris, x = ~Sepal.Width, y = ~Petal.Width, series = ~Species,
        type = 'scatter', palette='aetnateal',
        symbolList=c('arrow','heart','star8'),
        title = 'Scatter - Sepal Width vs Petal Width, by Species (iris)',
        xlab = 'Sepal Width', ylab = 'Petal Width')
```

<!--html_preserve--><div id="htmlwidget-9141" style="width:768px;height:480px;" class="echarts"></div>
<script type="application/json" data-for="htmlwidget-9141">{"x":{"title":{"text":"Scatter - Sepal Width vs Petal Width, by Species (iris)","subtext":"","padding":[25,5,5,5],"x":"center","y":"bottom"},"tooltip":{"trigger":"axis","axisPointer":{"show":true,"type":"cross","lineStyle":{"type":"dashed","width":1}},"formatter":"function (params) {\n                                                if (params.value.length > 1) {\n                                                return params.seriesName + \" :<br/>\"\n                                                + params.value[0] + \" ,    \" +\n                                                + params.value[1];\n                                                } else {\n                                                return params.seriesName + \" :<br/>\"\n                                                + params.name + \" : \"\n                                                + params.value;\n                                                }}"},"toolbox":{"show":true,"feature":{"mark":{"show":true,"title":{"mark":"辅助线开�? Auxiliary conductor switch","markUndo":"删除辅助�? Undo auxiliary conductor","markClear":"清空辅助�? Clear auxiliary conductor"}},"dataView":{"show":true,"readOnly":false,"title":"数据视图 Data view"},"magicType":{"show":false},"restore":{"show":true,"title":"还原 Restore"},"saveAsImage":{"show":true,"title":"保存为图�? Save as image"}},"x":"right","y":"top"},"calculable":true,"series":[{"type":"scatter","data":[[3.5,0.2],[3,0.2],[3.2,0.2],[3.1,0.2],[3.6,0.2],[3.9,0.4],[3.4,0.3],[3.4,0.2],[2.9,0.2],[3.1,0.1],[3.7,0.2],[3.4,0.2],[3,0.1],[3,0.1],[4,0.2],[4.4,0.4],[3.9,0.4],[3.5,0.3],[3.8,0.3],[3.8,0.3],[3.4,0.2],[3.7,0.4],[3.6,0.2],[3.3,0.5],[3.4,0.2],[3,0.2],[3.4,0.4],[3.5,0.2],[3.4,0.2],[3.2,0.2],[3.1,0.2],[3.4,0.4],[4.1,0.1],[4.2,0.2],[3.1,0.2],[3.2,0.2],[3.5,0.2],[3.6,0.1],[3,0.2],[3.4,0.2],[3.5,0.3],[2.3,0.3],[3.2,0.2],[3.5,0.6],[3.8,0.4],[3,0.3],[3.8,0.2],[3.2,0.2],[3.7,0.2],[3.3,0.2]],"large":false,"name":"setosa"},{"type":"scatter","data":[[3.2,1.4],[3.2,1.5],[3.1,1.5],[2.3,1.3],[2.8,1.5],[2.8,1.3],[3.3,1.6],[2.4,1],[2.9,1.3],[2.7,1.4],[2,1],[3,1.5],[2.2,1],[2.9,1.4],[2.9,1.3],[3.1,1.4],[3,1.5],[2.7,1],[2.2,1.5],[2.5,1.1],[3.2,1.8],[2.8,1.3],[2.5,1.5],[2.8,1.2],[2.9,1.3],[3,1.4],[2.8,1.4],[3,1.7],[2.9,1.5],[2.6,1],[2.4,1.1],[2.4,1],[2.7,1.2],[2.7,1.6],[3,1.5],[3.4,1.6],[3.1,1.5],[2.3,1.3],[3,1.3],[2.5,1.3],[2.6,1.2],[3,1.4],[2.6,1.2],[2.3,1],[2.7,1.3],[3,1.2],[2.9,1.3],[2.9,1.3],[2.5,1.1],[2.8,1.3]],"large":false,"name":"versicolor"},{"type":"scatter","data":[[3.3,2.5],[2.7,1.9],[3,2.1],[2.9,1.8],[3,2.2],[3,2.1],[2.5,1.7],[2.9,1.8],[2.5,1.8],[3.6,2.5],[3.2,2],[2.7,1.9],[3,2.1],[2.5,2],[2.8,2.4],[3.2,2.3],[3,1.8],[3.8,2.2],[2.6,2.3],[2.2,1.5],[3.2,2.3],[2.8,2],[2.8,2],[2.7,1.8],[3.3,2.1],[3.2,1.8],[2.8,1.8],[3,1.8],[2.8,2.1],[3,1.6],[2.8,1.9],[3.8,2],[2.8,2.2],[2.8,1.5],[2.6,1.4],[3,2.3],[3.4,2.4],[3.1,1.8],[3,1.8],[3.1,2.1],[3.1,2.4],[3.1,2.3],[2.7,1.9],[3.2,2.3],[3.3,2.5],[3,2.3],[2.5,1.9],[3,2],[3.4,2.3],[3,1.8]],"large":false,"name":"virginica"}],"renderAsImage":true,"color":["#00A78E","#F47721","#7AC143","#00BCE4","#D20962","#7D3F98","#60C3AE","#FDB933","#B8D936","#5F78BB","#F58F9F","#EE3D94","#5E9732","#CEA979","#EF4135","#7090A5"],"symbolList":["arrow","heart","star8"],"legend":{"show":true,"data":["setosa","versicolor","virginica"],"x":"left","y":"top"},"xAxis":{"name":"Sepal Width","type":"value","boundaryGap":[0,0],"scale":true,"axisLine":{"show":true,"onZero":false}},"yAxis":{"name":"Petal Width","type":"value","scale":true,"axisLine":{"show":true,"onZero":false}}},"evals":["tooltip.formatter"]}</script><!--/html_preserve-->

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

<!--html_preserve--><div id="htmlwidget-9217" style="width:768px;height:480px;" class="echarts"></div>
<script type="application/json" data-for="htmlwidget-9217">{"x":{"title":{"text":"Bubble - Sepal Width vs Petal Width, weighed by Petal Length (iris)","subtext":"","padding":[25,5,5,5],"x":"center","y":"bottom"},"tooltip":{"trigger":"axis","axisPointer":{"show":true,"type":"cross","lineStyle":{"type":"dashed","width":1}}},"toolbox":{"show":true,"feature":{"mark":{"show":true,"title":{"mark":"辅助线开�? Auxiliary conductor switch","markUndo":"删除辅助�? Undo auxiliary conductor","markClear":"清空辅助�? Clear auxiliary conductor"}},"dataView":{"show":true,"readOnly":false,"title":"数据视图 Data view"},"magicType":{"show":false},"restore":{"show":true,"title":"还原 Restore"},"saveAsImage":{"show":true,"title":"保存为图�? Save as image"}},"x":"right","y":"top"},"calculable":true,"series":[{"type":"scatter","data":[[3.5,0.2,1.4],[3,0.2,1.4],[3.2,0.2,1.3],[3.1,0.2,1.5],[3.6,0.2,1.4],[3.9,0.4,1.7],[3.4,0.3,1.4],[3.4,0.2,1.5],[2.9,0.2,1.4],[3.1,0.1,1.5],[3.7,0.2,1.5],[3.4,0.2,1.6],[3,0.1,1.4],[3,0.1,1.1],[4,0.2,1.2],[4.4,0.4,1.5],[3.9,0.4,1.3],[3.5,0.3,1.4],[3.8,0.3,1.7],[3.8,0.3,1.5],[3.4,0.2,1.7],[3.7,0.4,1.5],[3.6,0.2,1],[3.3,0.5,1.7],[3.4,0.2,1.9],[3,0.2,1.6],[3.4,0.4,1.6],[3.5,0.2,1.5],[3.4,0.2,1.4],[3.2,0.2,1.6],[3.1,0.2,1.6],[3.4,0.4,1.5],[4.1,0.1,1.5],[4.2,0.2,1.4],[3.1,0.2,1.5],[3.2,0.2,1.2],[3.5,0.2,1.3],[3.6,0.1,1.4],[3,0.2,1.3],[3.4,0.2,1.5],[3.5,0.3,1.3],[2.3,0.3,1.3],[3.2,0.2,1.3],[3.5,0.6,1.6],[3.8,0.4,1.9],[3,0.3,1.4],[3.8,0.2,1.6],[3.2,0.2,1.4],[3.7,0.2,1.5],[3.3,0.2,1.4],[3.2,1.4,4.7],[3.2,1.5,4.5],[3.1,1.5,4.9],[2.3,1.3,4],[2.8,1.5,4.6],[2.8,1.3,4.5],[3.3,1.6,4.7],[2.4,1,3.3],[2.9,1.3,4.6],[2.7,1.4,3.9],[2,1,3.5],[3,1.5,4.2],[2.2,1,4],[2.9,1.4,4.7],[2.9,1.3,3.6],[3.1,1.4,4.4],[3,1.5,4.5],[2.7,1,4.1],[2.2,1.5,4.5],[2.5,1.1,3.9],[3.2,1.8,4.8],[2.8,1.3,4],[2.5,1.5,4.9],[2.8,1.2,4.7],[2.9,1.3,4.3],[3,1.4,4.4],[2.8,1.4,4.8],[3,1.7,5],[2.9,1.5,4.5],[2.6,1,3.5],[2.4,1.1,3.8],[2.4,1,3.7],[2.7,1.2,3.9],[2.7,1.6,5.1],[3,1.5,4.5],[3.4,1.6,4.5],[3.1,1.5,4.7],[2.3,1.3,4.4],[3,1.3,4.1],[2.5,1.3,4],[2.6,1.2,4.4],[3,1.4,4.6],[2.6,1.2,4],[2.3,1,3.3],[2.7,1.3,4.2],[3,1.2,4.2],[2.9,1.3,4.2],[2.9,1.3,4.3],[2.5,1.1,3],[2.8,1.3,4.1],[3.3,2.5,6],[2.7,1.9,5.1],[3,2.1,5.9],[2.9,1.8,5.6],[3,2.2,5.8],[3,2.1,6.6],[2.5,1.7,4.5],[2.9,1.8,6.3],[2.5,1.8,5.8],[3.6,2.5,6.1],[3.2,2,5.1],[2.7,1.9,5.3],[3,2.1,5.5],[2.5,2,5],[2.8,2.4,5.1],[3.2,2.3,5.3],[3,1.8,5.5],[3.8,2.2,6.7],[2.6,2.3,6.9],[2.2,1.5,5],[3.2,2.3,5.7],[2.8,2,4.9],[2.8,2,6.7],[2.7,1.8,4.9],[3.3,2.1,5.7],[3.2,1.8,6],[2.8,1.8,4.8],[3,1.8,4.9],[2.8,2.1,5.6],[3,1.6,5.8],[2.8,1.9,6.1],[3.8,2,6.4],[2.8,2.2,5.6],[2.8,1.5,5.1],[2.6,1.4,5.6],[3,2.3,6.1],[3.4,2.4,5.6],[3.1,1.8,5.5],[3,1.8,4.8],[3.1,2.1,5.4],[3.1,2.4,5.6],[3.1,2.3,5.1],[2.7,1.9,5.1],[3.2,2.3,5.9],[3.3,2.5,5.7],[3,2.3,5.2],[2.5,1.9,5],[3,2,5.2],[3.4,2.3,5.4],[3,1.8,5.1]],"large":false,"symbolSize":"function (value){\n                       return Math.round(value[2]*\n2\n);}"}],"renderAsImage":true,"color":["#2aa198","#b58900","#cb4b16","#dc322f","#d33682","#6c71c4","#268bd2","#859900"],"xAxis":{"name":"Sepal Width","type":"value","boundaryGap":[0,0],"scale":true,"axisLine":{"show":true,"onZero":false}},"yAxis":{"name":"Petal Width","type":"value","scale":true,"axisLine":{"show":true,"onZero":false}}},"evals":["series.0.symbolSize"]}</script><!--/html_preserve-->

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

<!--html_preserve--><div id="htmlwidget-2979" style="width:768px;height:480px;" class="echarts"></div>
<script type="application/json" data-for="htmlwidget-2979">{"x":{"title":{"text":"Bubble - Sepal Width vs Petal Width, by Species, weighed by Petal Length (iris)","subtext":"","padding":[25,5,5,5],"x":"center","y":"bottom"},"tooltip":{"trigger":"axis","axisPointer":{"show":true,"type":"cross","lineStyle":{"type":"dashed","width":1}},"formatter":"function (params) {\n                                                if (params.value.length > 1) {\n                                                return params.seriesName + \" :<br/>\"\n                                                + params.value[0] + \" ,    \" +\n                                                + params.value[1];\n                                                } else {\n                                                return params.seriesName + \" :<br/>\"\n                                                + params.name + \" : \"\n                                                + params.value;\n                                                }}"},"toolbox":{"show":true,"feature":{"mark":{"show":true,"title":{"mark":"辅助线开�? Auxiliary conductor switch","markUndo":"删除辅助�? Undo auxiliary conductor","markClear":"清空辅助�? Clear auxiliary conductor"}},"dataView":{"show":true,"readOnly":false,"title":"数据视图 Data view"},"magicType":{"show":false},"restore":{"show":true,"title":"还原 Restore"},"saveAsImage":{"show":true,"title":"保存为图�? Save as image"}},"x":"right","y":"top"},"calculable":true,"series":[{"type":"scatter","data":[[3.5,0.2,1.4],[3,0.2,1.4],[3.2,0.2,1.3],[3.1,0.2,1.5],[3.6,0.2,1.4],[3.9,0.4,1.7],[3.4,0.3,1.4],[3.4,0.2,1.5],[2.9,0.2,1.4],[3.1,0.1,1.5],[3.7,0.2,1.5],[3.4,0.2,1.6],[3,0.1,1.4],[3,0.1,1.1],[4,0.2,1.2],[4.4,0.4,1.5],[3.9,0.4,1.3],[3.5,0.3,1.4],[3.8,0.3,1.7],[3.8,0.3,1.5],[3.4,0.2,1.7],[3.7,0.4,1.5],[3.6,0.2,1],[3.3,0.5,1.7],[3.4,0.2,1.9],[3,0.2,1.6],[3.4,0.4,1.6],[3.5,0.2,1.5],[3.4,0.2,1.4],[3.2,0.2,1.6],[3.1,0.2,1.6],[3.4,0.4,1.5],[4.1,0.1,1.5],[4.2,0.2,1.4],[3.1,0.2,1.5],[3.2,0.2,1.2],[3.5,0.2,1.3],[3.6,0.1,1.4],[3,0.2,1.3],[3.4,0.2,1.5],[3.5,0.3,1.3],[2.3,0.3,1.3],[3.2,0.2,1.3],[3.5,0.6,1.6],[3.8,0.4,1.9],[3,0.3,1.4],[3.8,0.2,1.6],[3.2,0.2,1.4],[3.7,0.2,1.5],[3.3,0.2,1.4]],"large":false,"name":"setosa","symbolSize":"function (value){\n                           return Math.round(value[2]*\n2\n);\n                     }"},{"type":"scatter","data":[[3.2,1.4,4.7],[3.2,1.5,4.5],[3.1,1.5,4.9],[2.3,1.3,4],[2.8,1.5,4.6],[2.8,1.3,4.5],[3.3,1.6,4.7],[2.4,1,3.3],[2.9,1.3,4.6],[2.7,1.4,3.9],[2,1,3.5],[3,1.5,4.2],[2.2,1,4],[2.9,1.4,4.7],[2.9,1.3,3.6],[3.1,1.4,4.4],[3,1.5,4.5],[2.7,1,4.1],[2.2,1.5,4.5],[2.5,1.1,3.9],[3.2,1.8,4.8],[2.8,1.3,4],[2.5,1.5,4.9],[2.8,1.2,4.7],[2.9,1.3,4.3],[3,1.4,4.4],[2.8,1.4,4.8],[3,1.7,5],[2.9,1.5,4.5],[2.6,1,3.5],[2.4,1.1,3.8],[2.4,1,3.7],[2.7,1.2,3.9],[2.7,1.6,5.1],[3,1.5,4.5],[3.4,1.6,4.5],[3.1,1.5,4.7],[2.3,1.3,4.4],[3,1.3,4.1],[2.5,1.3,4],[2.6,1.2,4.4],[3,1.4,4.6],[2.6,1.2,4],[2.3,1,3.3],[2.7,1.3,4.2],[3,1.2,4.2],[2.9,1.3,4.2],[2.9,1.3,4.3],[2.5,1.1,3],[2.8,1.3,4.1]],"large":false,"name":"versicolor","symbolSize":"function (value){\n                           return Math.round(value[2]*\n2\n);\n                     }"},{"type":"scatter","data":[[3.3,2.5,6],[2.7,1.9,5.1],[3,2.1,5.9],[2.9,1.8,5.6],[3,2.2,5.8],[3,2.1,6.6],[2.5,1.7,4.5],[2.9,1.8,6.3],[2.5,1.8,5.8],[3.6,2.5,6.1],[3.2,2,5.1],[2.7,1.9,5.3],[3,2.1,5.5],[2.5,2,5],[2.8,2.4,5.1],[3.2,2.3,5.3],[3,1.8,5.5],[3.8,2.2,6.7],[2.6,2.3,6.9],[2.2,1.5,5],[3.2,2.3,5.7],[2.8,2,4.9],[2.8,2,6.7],[2.7,1.8,4.9],[3.3,2.1,5.7],[3.2,1.8,6],[2.8,1.8,4.8],[3,1.8,4.9],[2.8,2.1,5.6],[3,1.6,5.8],[2.8,1.9,6.1],[3.8,2,6.4],[2.8,2.2,5.6],[2.8,1.5,5.1],[2.6,1.4,5.6],[3,2.3,6.1],[3.4,2.4,5.6],[3.1,1.8,5.5],[3,1.8,4.8],[3.1,2.1,5.4],[3.1,2.4,5.6],[3.1,2.3,5.1],[2.7,1.9,5.1],[3.2,2.3,5.9],[3.3,2.5,5.7],[3,2.3,5.2],[2.5,1.9,5],[3,2,5.2],[3.4,2.3,5.4],[3,1.8,5.1]],"large":false,"name":"virginica","symbolSize":"function (value){\n                           return Math.round(value[2]*\n2\n);\n                     }"}],"renderAsImage":true,"color":["#3CB7CC","#39737C","#32A251"],"symbolList":["emptyCircle","circle","circle"],"legend":{"show":true,"data":["setosa","versicolor","virginica"],"x":"left","y":"top"},"xAxis":{"name":"Sepal Width","type":"value","boundaryGap":[0,0],"scale":true,"axisLine":{"show":true,"onZero":false}},"yAxis":{"name":"Petal Width","type":"value","scale":true,"axisLine":{"show":true,"onZero":false}}},"evals":["tooltip.formatter","series.0.symbolSize","series.1.symbolSize","series.2.symbolSize"]}</script><!--/html_preserve-->

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

<!--html_preserve--><div id="htmlwidget-5381" style="width:768px;height:480px;" class="echarts"></div>
<script type="application/json" data-for="htmlwidget-5381">{"x":{"title":{"text":"VBar - Parameter Mean by Species (iris)","subtext":"","padding":[25,5,5,5],"x":"center","y":"bottom"},"tooltip":{"trigger":"axis","axisPointer":{"show":true,"lineStyle":{"type":"dashed","width":1}}},"toolbox":{"show":true,"feature":{"mark":{"show":true,"title":{"mark":"辅助线开�? Auxiliary conductor switch","markUndo":"删除辅助�? Undo auxiliary conductor","markClear":"清空辅助�? Clear auxiliary conductor"}},"dataView":{"show":true,"readOnly":false,"title":"数据视图 Data view"},"magicType":{"show":true,"type":["line","bar","tiled","stack"],"title":{"line":"折线图切�? Switch to line chart","bar":"柱形图切�? Switch to bar chart","stack":"堆积 Stack","tiled":"平铺 Tile","force":"力导向布局图切�? Switch to force chart","pie":"饼图切换 Switch to pie chart","funnel":"漏斗图切�? Switch to funnel chart"}},"restore":{"show":true,"title":"还原 Restore"},"saveAsImage":{"show":true,"title":"保存为图�? Save as image"}},"x":"right","y":"top"},"calculable":true,"series":[{"name":"setosa","type":"bar","data":[5.006,3.428,1.462,0.246]},{"name":"versicolor","type":"bar","data":[5.936,2.77,4.26,1.326]},{"name":"virginica","type":"bar","data":[6.588,2.974,5.552,2.026]}],"renderAsImage":true,"color":["#008FD5","#FF2700","#77AB43"],"legend":{"show":true,"data":["setosa","versicolor","virginica"],"x":"right","y":"center","orient":"vertical"},"xAxis":{"name":"Parameter","type":"category","boundaryGap":[0,0],"scale":true,"axisLine":{"show":true,"onZero":false},"data":["Sepal.Length","Sepal.Width","Petal.Length","Petal.Width"]},"yAxis":{"name":"Mean","type":"value","scale":true,"axisLine":{"show":true,"onZero":false}}},"evals":[]}</script><!--/html_preserve-->

### Stacked Column �ѻ���ͼ


```r
echartR(data = dtiris, x = ~Param, y = ~Mean, 
        series = ~Species, stack=T,
        type = 'bar', palette='pander',
        title = paste("VBar - Parameter Mean by Species", "(iris)"), 
        xlab = 'Parameter', ylab = 'Mean', legend_pos=c('right','center'))
```

<!--html_preserve--><div id="htmlwidget-8911" style="width:768px;height:480px;" class="echarts"></div>
<script type="application/json" data-for="htmlwidget-8911">{"x":{"title":{"text":"VBar - Parameter Mean by Species (iris)","subtext":"","padding":[25,5,5,5],"x":"center","y":"bottom"},"tooltip":{"trigger":"axis","axisPointer":{"show":true,"lineStyle":{"type":"dashed","width":1}}},"toolbox":{"show":true,"feature":{"mark":{"show":true,"title":{"mark":"辅助线开�? Auxiliary conductor switch","markUndo":"删除辅助�? Undo auxiliary conductor","markClear":"清空辅助�? Clear auxiliary conductor"}},"dataView":{"show":true,"readOnly":false,"title":"数据视图 Data view"},"magicType":{"show":true,"type":["line","bar","tiled","stack"],"title":{"line":"折线图切�? Switch to line chart","bar":"柱形图切�? Switch to bar chart","stack":"堆积 Stack","tiled":"平铺 Tile","force":"力导向布局图切�? Switch to force chart","pie":"饼图切换 Switch to pie chart","funnel":"漏斗图切�? Switch to funnel chart"}},"restore":{"show":true,"title":"还原 Restore"},"saveAsImage":{"show":true,"title":"保存为图�? Save as image"}},"x":"right","y":"top"},"calculable":true,"series":[{"name":"setosa","type":"bar","data":[5.006,3.428,1.462,0.246],"stack":"Stack"},{"name":"versicolor","type":"bar","data":[5.936,2.77,4.26,1.326],"stack":"Stack"},{"name":"virginica","type":"bar","data":[6.588,2.974,5.552,2.026],"stack":"Stack"}],"renderAsImage":true,"color":["#56B4E9","#009E73","#F0E442","#0072B2","#D55E00","#CC79A7","#999999","#E69F00"],"legend":{"show":true,"data":["setosa","versicolor","virginica"],"x":"right","y":"center","orient":"vertical"},"xAxis":{"name":"Parameter","type":"category","boundaryGap":[0,0],"scale":true,"axisLine":{"show":true,"onZero":false},"data":["Sepal.Length","Sepal.Width","Petal.Length","Petal.Width"]},"yAxis":{"name":"Mean","type":"value","scale":true,"axisLine":{"show":true,"onZero":false}}},"evals":[]}</script><!--/html_preserve-->

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

<!--html_preserve--><div id="htmlwidget-3028" style="width:768px;height:480px;" class="echarts"></div>
<script type="application/json" data-for="htmlwidget-3028">{"x":{"title":{"text":"Species-specific Mean by Parameters (iris)","subtext":"","padding":[25,5,5,5],"x":"center","y":"bottom"},"tooltip":{"trigger":"axis","axisPointer":{"show":true,"lineStyle":{"type":"dashed","width":1}}},"toolbox":{"show":true,"feature":{"mark":{"show":true,"title":{"mark":"辅助线开�? Auxiliary conductor switch","markUndo":"删除辅助�? Undo auxiliary conductor","markClear":"清空辅助�? Clear auxiliary conductor"}},"dataView":{"show":true,"readOnly":false,"title":"数据视图 Data view"},"magicType":{"show":true,"type":["line","bar","tiled","stack"],"title":{"line":"折线图切�? Switch to line chart","bar":"柱形图切�? Switch to bar chart","stack":"堆积 Stack","tiled":"平铺 Tile","force":"力导向布局图切�? Switch to force chart","pie":"饼图切换 Switch to pie chart","funnel":"漏斗图切�? Switch to funnel chart"}},"restore":{"show":true,"title":"还原 Restore"},"saveAsImage":{"show":true,"title":"保存为图�? Save as image"}},"x":"right","y":"top"},"calculable":true,"series":[{"name":"Sepal.Length","type":"bar","data":[5.006,5.936,6.588]},{"name":"Sepal.Width","type":"bar","data":[3.428,2.77,2.974]},{"name":"Petal.Length","type":"bar","data":[1.462,4.26,5.552]},{"name":"Petal.Width","type":"bar","data":[0.246,1.326,2.026]}],"renderAsImage":true,"color":["#1a476f","#90353b","#55752f","#e37e00","#6e8e84","#c10534","#938dd2","#cac27e","#a0522d","#7b92a8","#2d6d66","#9c8847","#bfa19c","#ffd200","#d9e6eb"],"legend":{"show":true,"data":["Sepal.Length","Sepal.Width","Petal.Length","Petal.Width"],"x":"right","y":"center","orient":"vertical"},"xAxis":{"name":"Mean","type":"value","scale":true,"axisLine":{"show":true,"onZero":false}},"yAxis":{"name":"Species","type":"category","boundaryGap":[0,0],"scale":true,"axisLine":{"show":true,"onZero":false},"data":["setosa","versicolor","virginica"]}},"evals":[]}</script><!--/html_preserve-->

### Stacked Bar �ѻ���ͼ

palette��Ϊcalc�����4����ɫ����stack (`stack=TRUE`)��


```r
echartR(data = dtiris, x = ~Param, y = ~Mean, 
        series = ~Species, stack=T, xyflip=T,
        type = 'bar', palette='calc',
        title = 'Parameter Mean by Species (iris)', 
        xlab = 'Parameter', ylab = 'Mean', legend_pos=c('right','center'))
```

<!--html_preserve--><div id="htmlwidget-3882" style="width:768px;height:480px;" class="echarts"></div>
<script type="application/json" data-for="htmlwidget-3882">{"x":{"title":{"text":"Parameter Mean by Species (iris)","subtext":"","padding":[25,5,5,5],"x":"center","y":"bottom"},"tooltip":{"trigger":"axis","axisPointer":{"show":true,"lineStyle":{"type":"dashed","width":1}}},"toolbox":{"show":true,"feature":{"mark":{"show":true,"title":{"mark":"辅助线开�? Auxiliary conductor switch","markUndo":"删除辅助�? Undo auxiliary conductor","markClear":"清空辅助�? Clear auxiliary conductor"}},"dataView":{"show":true,"readOnly":false,"title":"数据视图 Data view"},"magicType":{"show":true,"type":["line","bar","tiled","stack"],"title":{"line":"折线图切�? Switch to line chart","bar":"柱形图切�? Switch to bar chart","stack":"堆积 Stack","tiled":"平铺 Tile","force":"力导向布局图切�? Switch to force chart","pie":"饼图切换 Switch to pie chart","funnel":"漏斗图切�? Switch to funnel chart"}},"restore":{"show":true,"title":"还原 Restore"},"saveAsImage":{"show":true,"title":"保存为图�? Save as image"}},"x":"right","y":"top"},"calculable":true,"series":[{"name":"setosa","type":"bar","data":[5.006,3.428,1.462,0.246],"stack":"Stack"},{"name":"versicolor","type":"bar","data":[5.936,2.77,4.26,1.326],"stack":"Stack"},{"name":"virginica","type":"bar","data":[6.588,2.974,5.552,2.026],"stack":"Stack"}],"renderAsImage":true,"color":["#004586","#FF420E","#FFD320","#579D1C","#7E0021","#83CAFF","#314004","#AECF00","#4B1F6F","#FF950E","#C5000B","#0084D1"],"legend":{"show":true,"data":["setosa","versicolor","virginica"],"x":"right","y":"center","orient":"vertical"},"xAxis":{"name":"Mean","type":"value","scale":true,"axisLine":{"show":true,"onZero":false}},"yAxis":{"name":"Parameter","type":"category","boundaryGap":[0,0],"scale":true,"axisLine":{"show":true,"onZero":false},"data":["Sepal.Length","Sepal.Width","Petal.Length","Petal.Width"]}},"evals":[]}</script><!--/html_preserve-->

## Histogram ֱ��ͼ

ֱ��ͼ����ͼ��һ��������ֻ��Ҫָ��y��������ͨ��`splitNumber`ָ��ֱ����(Ĭ��9)��`xyflip`��ΪTRUE���Ϊ�ȼ۵ĺ���ͼ��

> Echarts�涨��/��ͼ��ɢ��ͼ���Ա������Ϊcategory���ͣ������ʱ�޷��򵥵�������ͳ��۵�ֱ��ͼ��


```r
echartR(airquality, y=~Temp, type='histogram', splitNumber=13,
        palette='pastel2', title='Histogram of temperature (airquality)')
```

<!--html_preserve--><div id="htmlwidget-2944" style="width:768px;height:480px;" class="echarts"></div>
<script type="application/json" data-for="htmlwidget-2944">{"x":{"title":{"text":"Histogram of temperature (airquality)","subtext":"","padding":[25,5,5,5],"x":"center","y":"bottom"},"tooltip":{"trigger":"axis","axisPointer":{"show":true,"lineStyle":{"type":"dashed","width":1}}},"toolbox":{"show":true,"feature":{"mark":{"show":true,"title":{"mark":"辅助线开�? Auxiliary conductor switch","markUndo":"删除辅助�? Undo auxiliary conductor","markClear":"清空辅助�? Clear auxiliary conductor"}},"dataView":{"show":true,"readOnly":false,"title":"数据视图 Data view"},"magicType":{"show":true,"type":["line","bar","tiled","stack"],"title":{"line":"折线图切�? Switch to line chart","bar":"柱形图切�? Switch to bar chart","stack":"堆积 Stack","tiled":"平铺 Tile","force":"力导向布局图切�? Switch to force chart","pie":"饼图切换 Switch to pie chart","funnel":"漏斗图切�? Switch to funnel chart"}},"restore":{"show":true,"title":"还原 Restore"},"saveAsImage":{"show":true,"title":"保存为图�? Save as image"}},"x":"right","y":"top"},"calculable":true,"series":[{"type":"bar","data":[7,5,5,11,7,12,26,22,18,17,8,10,4],"barGap":"1%"}],"renderAsImage":true,"color":["#B3E2CD","#FDCDAC","#CBD5E8","#F4CAE4","#E6F5C9","#FFF2AE","#F1E2CC","#CCCCCC"],"xAxis":{"name":"Temp","type":"category","boundaryGap":[0,0],"scale":true,"axisLine":{"show":true,"onZero":false},"data":["~59.2","59.2~","62.3~","65.5~","68.6~","71.8~","74.9~","78.1~","81.2~","84.4~","87.5~","90.7~","93.8~"]},"yAxis":{"name":"Freq","type":"value","scale":true,"axisLine":{"show":true,"onZero":false}}},"evals":[]}</script><!--/html_preserve-->

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

<!--html_preserve--><div id="htmlwidget-223" style="width:768px;height:480px;" class="echarts"></div>
<script type="application/json" data-for="htmlwidget-223">{"x":{"title":{"text":"Number of cars by transmission (mtcars)","subtext":"","padding":[25,5,5,5],"x":"center","y":"bottom"},"tooltip":{"trigger":"item","axisPointer":{"show":true,"lineStyle":{"type":"dashed","width":1}},"formatter":"{a} <br/>{b} : {c} ({d}%)"},"toolbox":{"show":true,"feature":{"mark":{"show":true,"title":{"mark":"辅助线开�? Auxiliary conductor switch","markUndo":"删除辅助�? Undo auxiliary conductor","markClear":"清空辅助�? Clear auxiliary conductor"}},"dataView":{"show":true,"readOnly":false,"title":"数据视图 Data view"},"magicType":{"show":true,"type":["pie","funnel"],"option":{"funnel":{"x":"25%","width":"80%","funnelAlign":"center"}},"title":{"line":"折线图切�? Switch to line chart","bar":"柱形图切�? Switch to bar chart","stack":"堆积 Stack","tiled":"平铺 Tile","force":"力导向布局图切�? Switch to force chart","pie":"饼图切换 Switch to pie chart","funnel":"漏斗图切�? Switch to funnel chart"}},"restore":{"show":true,"title":"还原 Restore"},"saveAsImage":{"show":true,"title":"保存为图�? Save as image"}},"x":"right","y":"top"},"calculable":true,"series":[{"name":"transmission","type":"pie","data":[{"value":19,"name":"Automatic"},{"value":13,"name":"Manual"}],"radius":"70%","center":["50%","50%"]}],"renderAsImage":true,"color":["#2b908f","#90ee7e","#f45b5b","#7798BF","#aaeeee","#ff0066","#eeaaee","#55BF3B","#DF5353","#7798BF","#aaeeee"],"legend":{"show":true,"data":["Automatic","Manual"],"x":"left","y":"top"}},"evals":[]}</script><!--/html_preserve-->

## Ring ��ͼ

����ͼ�Ǳ�ͼ�ı��Σ�ֻ�轫`type`��Ϊ'ring'��Echarts��ֻ��Ҫ�ѱ�ͼ�İ뾶������չΪ�����ڡ��⾶�ĳ���Ϊ2���������ɡ�


```r
echartR(dtcars, x = ~cylinder,  y = ~car, type='ring',
        palette='hc', title='Number of Cylinders (mtcars)')
```

<!--html_preserve--><div id="htmlwidget-8328" style="width:768px;height:480px;" class="echarts"></div>
<script type="application/json" data-for="htmlwidget-8328">{"x":{"title":{"text":"Number of Cylinders (mtcars)","subtext":"","padding":[25,5,5,5],"x":"center","y":"bottom"},"tooltip":{"trigger":"item","axisPointer":{"show":true,"lineStyle":{"type":"dashed","width":1}},"formatter":"{a} <br/>{b} : {c} ({d}%)"},"toolbox":{"show":true,"feature":{"mark":{"show":true,"title":{"mark":"辅助线开�? Auxiliary conductor switch","markUndo":"删除辅助�? Undo auxiliary conductor","markClear":"清空辅助�? Clear auxiliary conductor"}},"dataView":{"show":true,"readOnly":false,"title":"数据视图 Data view"},"magicType":{"show":true,"type":["pie","funnel"],"option":{"funnel":{"x":"25%","width":"80%","funnelAlign":"center"}},"title":{"line":"折线图切�? Switch to line chart","bar":"柱形图切�? Switch to bar chart","stack":"堆积 Stack","tiled":"平铺 Tile","force":"力导向布局图切�? Switch to force chart","pie":"饼图切换 Switch to pie chart","funnel":"漏斗图切�? Switch to funnel chart"}},"restore":{"show":true,"title":"还原 Restore"},"saveAsImage":{"show":true,"title":"保存为图�? Save as image"}},"x":"right","y":"top"},"calculable":true,"series":[{"name":"cylinder","type":"pie","data":[{"value":11,"name":"4"},{"value":7,"name":"6"},{"value":14,"name":"8"}],"radius":["60%","80%"],"itemStyle":{"emphasis":{"label":{"show":true,"position":"center","textStyle":{"fontSize":"30","fontWeight":"bold"}}}}}],"renderAsImage":true,"color":["#7cb5ec","#434348","#90ed7d","#f7a35c","#8085e9","#f15c80","#e4d354","#8085e8"],"legend":{"show":true,"data":["4","6","8"],"x":"left","y":"top"}},"evals":[]}</script><!--/html_preserve-->

## Line ��ͼ

### Unstacked Line ƽ����ͼ

����������`dataZoom=T`


```r
data(airquality)
airquality$Date <- strptime(paste(2015,airquality$Month,airquality$Day,sep="-")
                            ,format="%Y-%m-%d")
airquality$strDate <- with(airquality,paste(2015,Month,Day,sep="-"))
airquality$TempG <- cut(airquality$Temp,breaks=c(0,60,70,80,100))
#echartR(airquality, x = ~Date, y= ~Wind,
#          type='line', dataZoom=T,
#        palette='tableauBlRd', xlab = 'Date', ylab = 'Wind',
#        title='Wind by day (airquality)')
echartR(airquality, x = ~Day, y= ~Wind, series=~Month,
          type='line', dataZoom=T, dataZoomRange=c(30,70),
        palette='tableauBlRd', xlab = 'Days', ylab = 'Wind',
        title='Day-specific Wind by month (airquality)', symbolList='none')
```

<!--html_preserve--><div id="htmlwidget-9589" style="width:768px;height:480px;" class="echarts"></div>
<script type="application/json" data-for="htmlwidget-9589">{"x":{"title":{"text":"Day-specific Wind by month (airquality)","subtext":"","padding":[25,5,5,5],"x":"center","y":"bottom"},"tooltip":{"trigger":"axis","axisPointer":{"show":true,"lineStyle":{"type":"dashed","width":1}}},"toolbox":{"show":true,"feature":{"mark":{"show":true,"title":{"mark":"辅助线开�? Auxiliary conductor switch","markUndo":"删除辅助�? Undo auxiliary conductor","markClear":"清空辅助�? Clear auxiliary conductor"}},"dataView":{"show":true,"readOnly":false,"title":"数据视图 Data view"},"magicType":{"show":true,"type":["line","bar","tiled","stack"],"title":{"line":"折线图切�? Switch to line chart","bar":"柱形图切�? Switch to bar chart","stack":"堆积 Stack","tiled":"平铺 Tile","force":"力导向布局图切�? Switch to force chart","pie":"饼图切换 Switch to pie chart","funnel":"漏斗图切�? Switch to funnel chart"}},"restore":{"show":true,"title":"还原 Restore"},"saveAsImage":{"show":true,"title":"保存为图�? Save as image"}},"x":"right","y":"top"},"calculable":true,"series":[{"name":"5","type":"line","data":["7.4","8","12.6","11.5","14.3","14.9","8.6","13.8","20.1","8.6","6.9","9.7","9.2","10.9","13.2","11.5","12","18.4","11.5","9.7","9.7","16.6","9.7","12","16.6","14.9","8","12","14.9","5.7","7.4"]},{"name":"6","type":"line","data":["8.6","9.7","16.1","9.2","8.6","14.3","9.7","6.9","13.8","11.5","10.9","9.2","8","13.8","11.5","14.9","20.7","9.2","11.5","10.3","6.3","1.7","4.6","6.3","8","8","10.3","11.5","14.9","8"]},{"name":"7","type":"line","data":["4.1","9.2","9.2","10.9","4.6","10.9","5.1","6.3","5.7","7.4","8.6","14.3","14.9","14.9","14.3","6.9","10.3","6.3","5.1","11.5","6.9","9.7","11.5","8.6","8","8.6","12","7.4","7.4","7.4","9.2"]},{"name":"8","type":"line","data":["6.9","13.8","7.4","6.9","7.4","4.6","4","10.3","8","8.6","11.5","11.5","11.5","9.7","11.5","10.3","6.3","7.4","10.9","10.3","15.5","14.3","12.6","9.7","3.4","8","5.7","9.7","2.3","6.3","6.3"]},{"name":"9","type":"line","data":["6.9","5.1","2.8","4.6","7.4","15.5","10.9","10.3","10.9","9.7","14.9","15.5","6.3","10.9","11.5","6.9","13.8","10.3","10.3","8","12.6","9.2","10.3","10.3","16.6","6.9","13.2","14.3","8","11.5"]}],"renderAsImage":true,"color":["#2C69B0","#F02720","#AC613C","#6BA3D6","#AC8763","#BD0A36"],"symbolList":["none","none","none","none","none"],"dataZoom":{"show":true,"start":30,"end":70},"legend":{"show":true,"data":["5","6","7","8","9"],"x":"left","y":"top"},"xAxis":{"name":"Days","type":"category","boundaryGap":[0,0],"scale":true,"axisLine":{"show":true,"onZero":false},"data":["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31"]},"yAxis":{"name":"Wind","type":"value","scale":true,"axisLine":{"show":true,"onZero":false}}},"evals":[]}</script><!--/html_preserve-->

�߶�ƽ��(`type='linesmooth'`)������ʾ��־ͼ��(`symbolList='none'`)��Echarts��ȱʧֵĬ�ϲ��������кܶ���ߡ���Ҫ������ǰ����ʱ��������ֵ���㡣


```r
airq <- melt(airquality[,c("Ozone","Solar.R","Wind","Temp","strDate")],
             id=c("strDate"))
echartR(airq, x = ~strDate, y= ~value, series= ~variable, type='linesmooth',
        symbolList='none', dataZoom=T, dataZoomRange=c(20,50),
        palette='tableauPrGy', xlab = 'Date', ylab = 'Measure',
        title='Climate measures by day (airquality)')
```

<!--html_preserve--><div id="htmlwidget-3582" style="width:768px;height:480px;" class="echarts"></div>
<script type="application/json" data-for="htmlwidget-3582">{"x":{"title":{"text":"Climate measures by day (airquality)","subtext":"","padding":[25,5,5,5],"x":"center","y":"bottom"},"tooltip":{"trigger":"axis","axisPointer":{"show":true,"lineStyle":{"type":"dashed","width":1}}},"toolbox":{"show":true,"feature":{"mark":{"show":true,"title":{"mark":"辅助线开�? Auxiliary conductor switch","markUndo":"删除辅助�? Undo auxiliary conductor","markClear":"清空辅助�? Clear auxiliary conductor"}},"dataView":{"show":true,"readOnly":false,"title":"数据视图 Data view"},"magicType":{"show":true,"type":["line","bar","tiled","stack"],"title":{"line":"折线图切�? Switch to line chart","bar":"柱形图切�? Switch to bar chart","stack":"堆积 Stack","tiled":"平铺 Tile","force":"力导向布局图切�? Switch to force chart","pie":"饼图切换 Switch to pie chart","funnel":"漏斗图切�? Switch to funnel chart"}},"restore":{"show":true,"title":"还原 Restore"},"saveAsImage":{"show":true,"title":"保存为图�? Save as image"}},"x":"right","y":"top"},"calculable":true,"series":[{"name":"Ozone","type":"line","data":["41","36","12","18","-","28","23","19","8","-","7","16","11","14","18","14","34","6","30","11","1","11","4","32","-","-","-","23","45","115","37","-","-","-","-","-","-","29","-","71","39","-","-","23","-","-","21","37","20","12","13","-","-","-","-","-","-","-","-","-","-","135","49","32","-","64","40","77","97","97","85","-","10","27","-","7","48","35","61","79","63","16","-","-","80","108","20","52","82","50","64","59","39","9","16","78","35","66","122","89","110","-","-","44","28","65","-","22","59","23","31","44","21","9","-","45","168","73","-","76","118","84","85","96","78","73","91","47","32","20","23","21","24","44","21","28","9","13","46","18","13","24","16","13","23","36","7","14","30","-","14","18","20"],"smooth":true},{"name":"Solar.R","type":"line","data":["190","118","149","313","-","-","299","99","19","194","-","256","290","274","65","334","307","78","322","44","8","320","25","92","66","266","-","13","252","223","279","286","287","242","186","220","264","127","273","291","323","259","250","148","332","322","191","284","37","120","137","150","59","91","250","135","127","47","98","31","138","269","248","236","101","175","314","276","267","272","175","139","264","175","291","48","260","274","285","187","220","7","258","295","294","223","81","82","213","275","253","254","83","24","77","-","-","-","255","229","207","222","137","192","273","157","64","71","51","115","244","190","259","36","255","212","238","215","153","203","225","237","188","167","197","183","189","95","92","252","220","230","259","236","259","238","24","112","237","224","27","238","201","238","14","139","49","20","193","145","191","131","223"],"smooth":true},{"name":"Wind","type":"line","data":["7.4","8","12.6","11.5","14.3","14.9","8.6","13.8","20.1","8.6","6.9","9.7","9.2","10.9","13.2","11.5","12","18.4","11.5","9.7","9.7","16.6","9.7","12","16.6","14.9","8","12","14.9","5.7","7.4","8.6","9.7","16.1","9.2","8.6","14.3","9.7","6.9","13.8","11.5","10.9","9.2","8","13.8","11.5","14.9","20.7","9.2","11.5","10.3","6.3","1.7","4.6","6.3","8","8","10.3","11.5","14.9","8","4.1","9.2","9.2","10.9","4.6","10.9","5.1","6.3","5.7","7.4","8.6","14.3","14.9","14.9","14.3","6.9","10.3","6.3","5.1","11.5","6.9","9.7","11.5","8.6","8","8.6","12","7.4","7.4","7.4","9.2","6.9","13.8","7.4","6.9","7.4","4.6","4","10.3","8","8.6","11.5","11.5","11.5","9.7","11.5","10.3","6.3","7.4","10.9","10.3","15.5","14.3","12.6","9.7","3.4","8","5.7","9.7","2.3","6.3","6.3","6.9","5.1","2.8","4.6","7.4","15.5","10.9","10.3","10.9","9.7","14.9","15.5","6.3","10.9","11.5","6.9","13.8","10.3","10.3","8","12.6","9.2","10.3","10.3","16.6","6.9","13.2","14.3","8","11.5"],"smooth":true},{"name":"Temp","type":"line","data":["67","72","74","62","56","66","65","59","61","69","74","69","66","68","58","64","66","57","68","62","59","73","61","61","57","58","57","67","81","79","76","78","74","67","84","85","79","82","87","90","87","93","92","82","80","79","77","72","65","73","76","77","76","76","76","75","78","73","80","77","83","84","85","81","84","83","83","88","92","92","89","82","73","81","91","80","81","82","84","87","85","74","81","82","86","85","82","86","88","86","83","81","81","81","82","86","85","87","89","90","90","92","86","86","82","80","79","77","79","76","78","78","77","72","75","79","81","86","88","97","94","96","94","91","92","93","93","87","84","80","78","75","73","81","76","77","71","71","78","67","76","68","82","64","71","81","69","63","70","77","75","76","68"],"smooth":true}],"renderAsImage":true,"color":["#7B66D2","#DC5FBD","#5F5A41","#995688","#AB6AD5","#8B7C6E"],"symbolList":["none","none","none","none"],"dataZoom":{"show":true,"start":20,"end":50},"legend":{"show":true,"data":["Ozone","Solar.R","Wind","Temp"],"x":"left","y":"top"},"xAxis":{"name":"Date","type":"category","boundaryGap":[0,0],"scale":true,"axisLine":{"show":true,"onZero":false},"data":["2015-5-1","2015-5-2","2015-5-3","2015-5-4","2015-5-5","2015-5-6","2015-5-7","2015-5-8","2015-5-9","2015-5-10","2015-5-11","2015-5-12","2015-5-13","2015-5-14","2015-5-15","2015-5-16","2015-5-17","2015-5-18","2015-5-19","2015-5-20","2015-5-21","2015-5-22","2015-5-23","2015-5-24","2015-5-25","2015-5-26","2015-5-27","2015-5-28","2015-5-29","2015-5-30","2015-5-31","2015-6-1","2015-6-2","2015-6-3","2015-6-4","2015-6-5","2015-6-6","2015-6-7","2015-6-8","2015-6-9","2015-6-10","2015-6-11","2015-6-12","2015-6-13","2015-6-14","2015-6-15","2015-6-16","2015-6-17","2015-6-18","2015-6-19","2015-6-20","2015-6-21","2015-6-22","2015-6-23","2015-6-24","2015-6-25","2015-6-26","2015-6-27","2015-6-28","2015-6-29","2015-6-30","2015-7-1","2015-7-2","2015-7-3","2015-7-4","2015-7-5","2015-7-6","2015-7-7","2015-7-8","2015-7-9","2015-7-10","2015-7-11","2015-7-12","2015-7-13","2015-7-14","2015-7-15","2015-7-16","2015-7-17","2015-7-18","2015-7-19","2015-7-20","2015-7-21","2015-7-22","2015-7-23","2015-7-24","2015-7-25","2015-7-26","2015-7-27","2015-7-28","2015-7-29","2015-7-30","2015-7-31","2015-8-1","2015-8-2","2015-8-3","2015-8-4","2015-8-5","2015-8-6","2015-8-7","2015-8-8","2015-8-9","2015-8-10","2015-8-11","2015-8-12","2015-8-13","2015-8-14","2015-8-15","2015-8-16","2015-8-17","2015-8-18","2015-8-19","2015-8-20","2015-8-21","2015-8-22","2015-8-23","2015-8-24","2015-8-25","2015-8-26","2015-8-27","2015-8-28","2015-8-29","2015-8-30","2015-8-31","2015-9-1","2015-9-2","2015-9-3","2015-9-4","2015-9-5","2015-9-6","2015-9-7","2015-9-8","2015-9-9","2015-9-10","2015-9-11","2015-9-12","2015-9-13","2015-9-14","2015-9-15","2015-9-16","2015-9-17","2015-9-18","2015-9-19","2015-9-20","2015-9-21","2015-9-22","2015-9-23","2015-9-24","2015-9-25","2015-9-26","2015-9-27","2015-9-28","2015-9-29","2015-9-30"]},"yAxis":{"name":"Measure","type":"value","scale":true,"axisLine":{"show":true,"onZero":false}}},"evals":[]}</script><!--/html_preserve-->

### Stacked Line �ѻ���ͼ

```r
echartR(dfiris, x = ~id, y= ~Value, series= ~Param, type='line',stack=T,
        palette='tableauBlRd12', xlab = 'Sample ID', ylab = 'Measure',
        title='Parameter measures (iris)')
```

<!--html_preserve--><div id="htmlwidget-4048" style="width:768px;height:480px;" class="echarts"></div>
<script type="application/json" data-for="htmlwidget-4048">{"x":{"title":{"text":"Parameter measures (iris)","subtext":"","padding":[25,5,5,5],"x":"center","y":"bottom"},"tooltip":{"trigger":"axis","axisPointer":{"show":true,"lineStyle":{"type":"dashed","width":1}}},"toolbox":{"show":true,"feature":{"mark":{"show":true,"title":{"mark":"辅助线开�? Auxiliary conductor switch","markUndo":"删除辅助�? Undo auxiliary conductor","markClear":"清空辅助�? Clear auxiliary conductor"}},"dataView":{"show":true,"readOnly":false,"title":"数据视图 Data view"},"magicType":{"show":true,"type":["line","bar","tiled","stack"],"title":{"line":"折线图切�? Switch to line chart","bar":"柱形图切�? Switch to bar chart","stack":"堆积 Stack","tiled":"平铺 Tile","force":"力导向布局图切�? Switch to force chart","pie":"饼图切换 Switch to pie chart","funnel":"漏斗图切�? Switch to funnel chart"}},"restore":{"show":true,"title":"还原 Restore"},"saveAsImage":{"show":true,"title":"保存为图�? Save as image"}},"x":"right","y":"top"},"calculable":true,"series":[{"name":"Sepal.Length","type":"line","data":["5.1","4.9","4.7","4.6","5","5.4","4.6","5","4.4","4.9","5.4","4.8","4.8","4.3","5.8","5.7","5.4","5.1","5.7","5.1","5.4","5.1","4.6","5.1","4.8","5","5","5.2","5.2","4.7","4.8","5.4","5.2","5.5","4.9","5","5.5","4.9","4.4","5.1","5","4.5","4.4","5","5.1","4.8","5.1","4.6","5.3","5","7","6.4","6.9","5.5","6.5","5.7","6.3","4.9","6.6","5.2","5","5.9","6","6.1","5.6","6.7","5.6","5.8","6.2","5.6","5.9","6.1","6.3","6.1","6.4","6.6","6.8","6.7","6","5.7","5.5","5.5","5.8","6","5.4","6","6.7","6.3","5.6","5.5","5.5","6.1","5.8","5","5.6","5.7","5.7","6.2","5.1","5.7","6.3","5.8","7.1","6.3","6.5","7.6","4.9","7.3","6.7","7.2","6.5","6.4","6.8","5.7","5.8","6.4","6.5","7.7","7.7","6","6.9","5.6","7.7","6.3","6.7","7.2","6.2","6.1","6.4","7.2","7.4","7.9","6.4","6.3","6.1","7.7","6.3","6.4","6","6.9","6.7","6.9","5.8","6.8","6.7","6.7","6.3","6.5","6.2","5.9"],"stack":"Stack"},{"name":"Sepal.Width","type":"line","data":["3.5","3","3.2","3.1","3.6","3.9","3.4","3.4","2.9","3.1","3.7","3.4","3","3","4","4.4","3.9","3.5","3.8","3.8","3.4","3.7","3.6","3.3","3.4","3","3.4","3.5","3.4","3.2","3.1","3.4","4.1","4.2","3.1","3.2","3.5","3.6","3","3.4","3.5","2.3","3.2","3.5","3.8","3","3.8","3.2","3.7","3.3","3.2","3.2","3.1","2.3","2.8","2.8","3.3","2.4","2.9","2.7","2","3","2.2","2.9","2.9","3.1","3","2.7","2.2","2.5","3.2","2.8","2.5","2.8","2.9","3","2.8","3","2.9","2.6","2.4","2.4","2.7","2.7","3","3.4","3.1","2.3","3","2.5","2.6","3","2.6","2.3","2.7","3","2.9","2.9","2.5","2.8","3.3","2.7","3","2.9","3","3","2.5","2.9","2.5","3.6","3.2","2.7","3","2.5","2.8","3.2","3","3.8","2.6","2.2","3.2","2.8","2.8","2.7","3.3","3.2","2.8","3","2.8","3","2.8","3.8","2.8","2.8","2.6","3","3.4","3.1","3","3.1","3.1","3.1","2.7","3.2","3.3","3","2.5","3","3.4","3"],"stack":"Stack"},{"name":"Petal.Length","type":"line","data":["1.4","1.4","1.3","1.5","1.4","1.7","1.4","1.5","1.4","1.5","1.5","1.6","1.4","1.1","1.2","1.5","1.3","1.4","1.7","1.5","1.7","1.5","1","1.7","1.9","1.6","1.6","1.5","1.4","1.6","1.6","1.5","1.5","1.4","1.5","1.2","1.3","1.4","1.3","1.5","1.3","1.3","1.3","1.6","1.9","1.4","1.6","1.4","1.5","1.4","4.7","4.5","4.9","4","4.6","4.5","4.7","3.3","4.6","3.9","3.5","4.2","4","4.7","3.6","4.4","4.5","4.1","4.5","3.9","4.8","4","4.9","4.7","4.3","4.4","4.8","5","4.5","3.5","3.8","3.7","3.9","5.1","4.5","4.5","4.7","4.4","4.1","4","4.4","4.6","4","3.3","4.2","4.2","4.2","4.3","3","4.1","6","5.1","5.9","5.6","5.8","6.6","4.5","6.3","5.8","6.1","5.1","5.3","5.5","5","5.1","5.3","5.5","6.7","6.9","5","5.7","4.9","6.7","4.9","5.7","6","4.8","4.9","5.6","5.8","6.1","6.4","5.6","5.1","5.6","6.1","5.6","5.5","4.8","5.4","5.6","5.1","5.1","5.9","5.7","5.2","5","5.2","5.4","5.1"],"stack":"Stack"},{"name":"Petal.Width","type":"line","data":["0.2","0.2","0.2","0.2","0.2","0.4","0.3","0.2","0.2","0.1","0.2","0.2","0.1","0.1","0.2","0.4","0.4","0.3","0.3","0.3","0.2","0.4","0.2","0.5","0.2","0.2","0.4","0.2","0.2","0.2","0.2","0.4","0.1","0.2","0.2","0.2","0.2","0.1","0.2","0.2","0.3","0.3","0.2","0.6","0.4","0.3","0.2","0.2","0.2","0.2","1.4","1.5","1.5","1.3","1.5","1.3","1.6","1","1.3","1.4","1","1.5","1","1.4","1.3","1.4","1.5","1","1.5","1.1","1.8","1.3","1.5","1.2","1.3","1.4","1.4","1.7","1.5","1","1.1","1","1.2","1.6","1.5","1.6","1.5","1.3","1.3","1.3","1.2","1.4","1.2","1","1.3","1.2","1.3","1.3","1.1","1.3","2.5","1.9","2.1","1.8","2.2","2.1","1.7","1.8","1.8","2.5","2","1.9","2.1","2","2.4","2.3","1.8","2.2","2.3","1.5","2.3","2","2","1.8","2.1","1.8","1.8","1.8","2.1","1.6","1.9","2","2.2","1.5","1.4","2.3","2.4","1.8","1.8","2.1","2.4","2.3","1.9","2.3","2.5","2.3","1.9","2","2.3","1.8"],"stack":"Stack"}],"renderAsImage":true,"color":["#2C69B0","#B5C8E2","#F02720","#FFB6B0","#AC613C","#E9C39B","#6BA3D6","#B5DFFD","#AC8763","#DDC9B4","#BD0A36","#F4737A"],"legend":{"show":true,"data":["Sepal.Length","Sepal.Width","Petal.Length","Petal.Width"],"x":"left","y":"top"},"xAxis":{"name":"Sample ID","type":"category","boundaryGap":[0,0],"scale":true,"axisLine":{"show":true,"onZero":false},"data":["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32","33","34","35","36","37","38","39","40","41","42","43","44","45","46","47","48","49","50","51","52","53","54","55","56","57","58","59","60","61","62","63","64","65","66","67","68","69","70","71","72","73","74","75","76","77","78","79","80","81","82","83","84","85","86","87","88","89","90","91","92","93","94","95","96","97","98","99","100","101","102","103","104","105","106","107","108","109","110","111","112","113","114","115","116","117","118","119","120","121","122","123","124","125","126","127","128","129","130","131","132","133","134","135","136","137","138","139","140","141","142","143","144","145","146","147","148","149","150"]},"yAxis":{"name":"Measure","type":"value","scale":true,"axisLine":{"show":true,"onZero":false}}},"evals":[]}</script><!--/html_preserve-->

�߶�ƽ��������ʾ��־ͼ��

```r
echartR(dfiris, x = ~id, y= ~Value, series= ~Param, type='linesmooth',stack=T,
        palette='tableauGnOr12', xlab = 'Sample ID', ylab = 'Measure',
        symbolList='none',
        title='Parameter measures (iris)')
```

<!--html_preserve--><div id="htmlwidget-2962" style="width:768px;height:480px;" class="echarts"></div>
<script type="application/json" data-for="htmlwidget-2962">{"x":{"title":{"text":"Parameter measures (iris)","subtext":"","padding":[25,5,5,5],"x":"center","y":"bottom"},"tooltip":{"trigger":"axis","axisPointer":{"show":true,"lineStyle":{"type":"dashed","width":1}}},"toolbox":{"show":true,"feature":{"mark":{"show":true,"title":{"mark":"辅助线开�? Auxiliary conductor switch","markUndo":"删除辅助�? Undo auxiliary conductor","markClear":"清空辅助�? Clear auxiliary conductor"}},"dataView":{"show":true,"readOnly":false,"title":"数据视图 Data view"},"magicType":{"show":true,"type":["line","bar","tiled","stack"],"title":{"line":"折线图切�? Switch to line chart","bar":"柱形图切�? Switch to bar chart","stack":"堆积 Stack","tiled":"平铺 Tile","force":"力导向布局图切�? Switch to force chart","pie":"饼图切换 Switch to pie chart","funnel":"漏斗图切�? Switch to funnel chart"}},"restore":{"show":true,"title":"还原 Restore"},"saveAsImage":{"show":true,"title":"保存为图�? Save as image"}},"x":"right","y":"top"},"calculable":true,"series":[{"name":"Sepal.Length","type":"line","data":["5.1","4.9","4.7","4.6","5","5.4","4.6","5","4.4","4.9","5.4","4.8","4.8","4.3","5.8","5.7","5.4","5.1","5.7","5.1","5.4","5.1","4.6","5.1","4.8","5","5","5.2","5.2","4.7","4.8","5.4","5.2","5.5","4.9","5","5.5","4.9","4.4","5.1","5","4.5","4.4","5","5.1","4.8","5.1","4.6","5.3","5","7","6.4","6.9","5.5","6.5","5.7","6.3","4.9","6.6","5.2","5","5.9","6","6.1","5.6","6.7","5.6","5.8","6.2","5.6","5.9","6.1","6.3","6.1","6.4","6.6","6.8","6.7","6","5.7","5.5","5.5","5.8","6","5.4","6","6.7","6.3","5.6","5.5","5.5","6.1","5.8","5","5.6","5.7","5.7","6.2","5.1","5.7","6.3","5.8","7.1","6.3","6.5","7.6","4.9","7.3","6.7","7.2","6.5","6.4","6.8","5.7","5.8","6.4","6.5","7.7","7.7","6","6.9","5.6","7.7","6.3","6.7","7.2","6.2","6.1","6.4","7.2","7.4","7.9","6.4","6.3","6.1","7.7","6.3","6.4","6","6.9","6.7","6.9","5.8","6.8","6.7","6.7","6.3","6.5","6.2","5.9"],"stack":"Stack","smooth":true},{"name":"Sepal.Width","type":"line","data":["3.5","3","3.2","3.1","3.6","3.9","3.4","3.4","2.9","3.1","3.7","3.4","3","3","4","4.4","3.9","3.5","3.8","3.8","3.4","3.7","3.6","3.3","3.4","3","3.4","3.5","3.4","3.2","3.1","3.4","4.1","4.2","3.1","3.2","3.5","3.6","3","3.4","3.5","2.3","3.2","3.5","3.8","3","3.8","3.2","3.7","3.3","3.2","3.2","3.1","2.3","2.8","2.8","3.3","2.4","2.9","2.7","2","3","2.2","2.9","2.9","3.1","3","2.7","2.2","2.5","3.2","2.8","2.5","2.8","2.9","3","2.8","3","2.9","2.6","2.4","2.4","2.7","2.7","3","3.4","3.1","2.3","3","2.5","2.6","3","2.6","2.3","2.7","3","2.9","2.9","2.5","2.8","3.3","2.7","3","2.9","3","3","2.5","2.9","2.5","3.6","3.2","2.7","3","2.5","2.8","3.2","3","3.8","2.6","2.2","3.2","2.8","2.8","2.7","3.3","3.2","2.8","3","2.8","3","2.8","3.8","2.8","2.8","2.6","3","3.4","3.1","3","3.1","3.1","3.1","2.7","3.2","3.3","3","2.5","3","3.4","3"],"stack":"Stack","smooth":true},{"name":"Petal.Length","type":"line","data":["1.4","1.4","1.3","1.5","1.4","1.7","1.4","1.5","1.4","1.5","1.5","1.6","1.4","1.1","1.2","1.5","1.3","1.4","1.7","1.5","1.7","1.5","1","1.7","1.9","1.6","1.6","1.5","1.4","1.6","1.6","1.5","1.5","1.4","1.5","1.2","1.3","1.4","1.3","1.5","1.3","1.3","1.3","1.6","1.9","1.4","1.6","1.4","1.5","1.4","4.7","4.5","4.9","4","4.6","4.5","4.7","3.3","4.6","3.9","3.5","4.2","4","4.7","3.6","4.4","4.5","4.1","4.5","3.9","4.8","4","4.9","4.7","4.3","4.4","4.8","5","4.5","3.5","3.8","3.7","3.9","5.1","4.5","4.5","4.7","4.4","4.1","4","4.4","4.6","4","3.3","4.2","4.2","4.2","4.3","3","4.1","6","5.1","5.9","5.6","5.8","6.6","4.5","6.3","5.8","6.1","5.1","5.3","5.5","5","5.1","5.3","5.5","6.7","6.9","5","5.7","4.9","6.7","4.9","5.7","6","4.8","4.9","5.6","5.8","6.1","6.4","5.6","5.1","5.6","6.1","5.6","5.5","4.8","5.4","5.6","5.1","5.1","5.9","5.7","5.2","5","5.2","5.4","5.1"],"stack":"Stack","smooth":true},{"name":"Petal.Width","type":"line","data":["0.2","0.2","0.2","0.2","0.2","0.4","0.3","0.2","0.2","0.1","0.2","0.2","0.1","0.1","0.2","0.4","0.4","0.3","0.3","0.3","0.2","0.4","0.2","0.5","0.2","0.2","0.4","0.2","0.2","0.2","0.2","0.4","0.1","0.2","0.2","0.2","0.2","0.1","0.2","0.2","0.3","0.3","0.2","0.6","0.4","0.3","0.2","0.2","0.2","0.2","1.4","1.5","1.5","1.3","1.5","1.3","1.6","1","1.3","1.4","1","1.5","1","1.4","1.3","1.4","1.5","1","1.5","1.1","1.8","1.3","1.5","1.2","1.3","1.4","1.4","1.7","1.5","1","1.1","1","1.2","1.6","1.5","1.6","1.5","1.3","1.3","1.3","1.2","1.4","1.2","1","1.3","1.2","1.3","1.3","1.1","1.3","2.5","1.9","2.1","1.8","2.2","2.1","1.7","1.8","1.8","2.5","2","1.9","2.1","2","2.4","2.3","1.8","2.2","2.3","1.5","2.3","2","2","1.8","2.1","1.8","1.8","1.8","2.1","1.6","1.9","2","2.2","1.5","1.4","2.3","2.4","1.8","1.8","2.1","2.4","2.3","1.9","2.3","2.5","2.3","1.9","2","2.3","1.8"],"stack":"Stack","smooth":true}],"renderAsImage":true,"color":["#32A251","#ACD98D","#FF7F0F","#FFB977","#3CB7CC","#98D9E4","#B85A0D","#FFD94A","#39737C","#86B4A9","#82853B","#CCC94D"],"symbolList":["none","none","none","none"],"legend":{"show":true,"data":["Sepal.Length","Sepal.Width","Petal.Length","Petal.Width"],"x":"left","y":"top"},"xAxis":{"name":"Sample ID","type":"category","boundaryGap":[0,0],"scale":true,"axisLine":{"show":true,"onZero":false},"data":["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32","33","34","35","36","37","38","39","40","41","42","43","44","45","46","47","48","49","50","51","52","53","54","55","56","57","58","59","60","61","62","63","64","65","66","67","68","69","70","71","72","73","74","75","76","77","78","79","80","81","82","83","84","85","86","87","88","89","90","91","92","93","94","95","96","97","98","99","100","101","102","103","104","105","106","107","108","109","110","111","112","113","114","115","116","117","118","119","120","121","122","123","124","125","126","127","128","129","130","131","132","133","134","135","136","137","138","139","140","141","142","143","144","145","146","147","148","149","150"]},"yAxis":{"name":"Measure","type":"value","scale":true,"axisLine":{"show":true,"onZero":false}}},"evals":[]}</script><!--/html_preserve-->

## Area ���ͼ
Echarts�У����ͼ�����ϱ�����Ϊ��ͼ��ֻ��ͨ��`itemStyle`������Ⱦ��ɫ��

### Tiled Area ƽ�����ͼ

```r
echartR(dfiris, x = ~id, y= ~Value, series= ~Param, type='area',
        palette='brbg', xlab = 'Sample ID', ylab = 'Measure',
        symbolList='emptyDiamond',title='Parameter measures (iris)')
```

<!--html_preserve--><div id="htmlwidget-479" style="width:768px;height:480px;" class="echarts"></div>
<script type="application/json" data-for="htmlwidget-479">{"x":{"title":{"text":"Parameter measures (iris)","subtext":"","padding":[25,5,5,5],"x":"center","y":"bottom"},"tooltip":{"trigger":"axis","axisPointer":{"show":true,"lineStyle":{"type":"dashed","width":1}}},"toolbox":{"show":true,"feature":{"mark":{"show":true,"title":{"mark":"辅助线开�? Auxiliary conductor switch","markUndo":"删除辅助�? Undo auxiliary conductor","markClear":"清空辅助�? Clear auxiliary conductor"}},"dataView":{"show":true,"readOnly":false,"title":"数据视图 Data view"},"magicType":{"show":true,"type":["line","bar","tiled","stack"],"title":{"line":"折线图切�? Switch to line chart","bar":"柱形图切�? Switch to bar chart","stack":"堆积 Stack","tiled":"平铺 Tile","force":"力导向布局图切�? Switch to force chart","pie":"饼图切换 Switch to pie chart","funnel":"漏斗图切�? Switch to funnel chart"}},"restore":{"show":true,"title":"还原 Restore"},"saveAsImage":{"show":true,"title":"保存为图�? Save as image"}},"x":"right","y":"top"},"calculable":true,"series":[{"name":"Sepal.Length","type":"line","data":["5.1","4.9","4.7","4.6","5","5.4","4.6","5","4.4","4.9","5.4","4.8","4.8","4.3","5.8","5.7","5.4","5.1","5.7","5.1","5.4","5.1","4.6","5.1","4.8","5","5","5.2","5.2","4.7","4.8","5.4","5.2","5.5","4.9","5","5.5","4.9","4.4","5.1","5","4.5","4.4","5","5.1","4.8","5.1","4.6","5.3","5","7","6.4","6.9","5.5","6.5","5.7","6.3","4.9","6.6","5.2","5","5.9","6","6.1","5.6","6.7","5.6","5.8","6.2","5.6","5.9","6.1","6.3","6.1","6.4","6.6","6.8","6.7","6","5.7","5.5","5.5","5.8","6","5.4","6","6.7","6.3","5.6","5.5","5.5","6.1","5.8","5","5.6","5.7","5.7","6.2","5.1","5.7","6.3","5.8","7.1","6.3","6.5","7.6","4.9","7.3","6.7","7.2","6.5","6.4","6.8","5.7","5.8","6.4","6.5","7.7","7.7","6","6.9","5.6","7.7","6.3","6.7","7.2","6.2","6.1","6.4","7.2","7.4","7.9","6.4","6.3","6.1","7.7","6.3","6.4","6","6.9","6.7","6.9","5.8","6.8","6.7","6.7","6.3","6.5","6.2","5.9"],"itemStyle":{"normal":{"areaStyle":{"type":"default"}}}},{"name":"Sepal.Width","type":"line","data":["3.5","3","3.2","3.1","3.6","3.9","3.4","3.4","2.9","3.1","3.7","3.4","3","3","4","4.4","3.9","3.5","3.8","3.8","3.4","3.7","3.6","3.3","3.4","3","3.4","3.5","3.4","3.2","3.1","3.4","4.1","4.2","3.1","3.2","3.5","3.6","3","3.4","3.5","2.3","3.2","3.5","3.8","3","3.8","3.2","3.7","3.3","3.2","3.2","3.1","2.3","2.8","2.8","3.3","2.4","2.9","2.7","2","3","2.2","2.9","2.9","3.1","3","2.7","2.2","2.5","3.2","2.8","2.5","2.8","2.9","3","2.8","3","2.9","2.6","2.4","2.4","2.7","2.7","3","3.4","3.1","2.3","3","2.5","2.6","3","2.6","2.3","2.7","3","2.9","2.9","2.5","2.8","3.3","2.7","3","2.9","3","3","2.5","2.9","2.5","3.6","3.2","2.7","3","2.5","2.8","3.2","3","3.8","2.6","2.2","3.2","2.8","2.8","2.7","3.3","3.2","2.8","3","2.8","3","2.8","3.8","2.8","2.8","2.6","3","3.4","3.1","3","3.1","3.1","3.1","2.7","3.2","3.3","3","2.5","3","3.4","3"],"itemStyle":{"normal":{"areaStyle":{"type":"default"}}}},{"name":"Petal.Length","type":"line","data":["1.4","1.4","1.3","1.5","1.4","1.7","1.4","1.5","1.4","1.5","1.5","1.6","1.4","1.1","1.2","1.5","1.3","1.4","1.7","1.5","1.7","1.5","1","1.7","1.9","1.6","1.6","1.5","1.4","1.6","1.6","1.5","1.5","1.4","1.5","1.2","1.3","1.4","1.3","1.5","1.3","1.3","1.3","1.6","1.9","1.4","1.6","1.4","1.5","1.4","4.7","4.5","4.9","4","4.6","4.5","4.7","3.3","4.6","3.9","3.5","4.2","4","4.7","3.6","4.4","4.5","4.1","4.5","3.9","4.8","4","4.9","4.7","4.3","4.4","4.8","5","4.5","3.5","3.8","3.7","3.9","5.1","4.5","4.5","4.7","4.4","4.1","4","4.4","4.6","4","3.3","4.2","4.2","4.2","4.3","3","4.1","6","5.1","5.9","5.6","5.8","6.6","4.5","6.3","5.8","6.1","5.1","5.3","5.5","5","5.1","5.3","5.5","6.7","6.9","5","5.7","4.9","6.7","4.9","5.7","6","4.8","4.9","5.6","5.8","6.1","6.4","5.6","5.1","5.6","6.1","5.6","5.5","4.8","5.4","5.6","5.1","5.1","5.9","5.7","5.2","5","5.2","5.4","5.1"],"itemStyle":{"normal":{"areaStyle":{"type":"default"}}}},{"name":"Petal.Width","type":"line","data":["0.2","0.2","0.2","0.2","0.2","0.4","0.3","0.2","0.2","0.1","0.2","0.2","0.1","0.1","0.2","0.4","0.4","0.3","0.3","0.3","0.2","0.4","0.2","0.5","0.2","0.2","0.4","0.2","0.2","0.2","0.2","0.4","0.1","0.2","0.2","0.2","0.2","0.1","0.2","0.2","0.3","0.3","0.2","0.6","0.4","0.3","0.2","0.2","0.2","0.2","1.4","1.5","1.5","1.3","1.5","1.3","1.6","1","1.3","1.4","1","1.5","1","1.4","1.3","1.4","1.5","1","1.5","1.1","1.8","1.3","1.5","1.2","1.3","1.4","1.4","1.7","1.5","1","1.1","1","1.2","1.6","1.5","1.6","1.5","1.3","1.3","1.3","1.2","1.4","1.2","1","1.3","1.2","1.3","1.3","1.1","1.3","2.5","1.9","2.1","1.8","2.2","2.1","1.7","1.8","1.8","2.5","2","1.9","2.1","2","2.4","2.3","1.8","2.2","2.3","1.5","2.3","2","2","1.8","2.1","1.8","1.8","1.8","2.1","1.6","1.9","2","2.2","1.5","1.4","2.3","2.4","1.8","1.8","2.1","2.4","2.3","1.9","2.3","2.5","2.3","1.9","2","2.3","1.8"],"itemStyle":{"normal":{"areaStyle":{"type":"default"}}}}],"renderAsImage":true,"color":["#543005","#8C510A","#BF812D","#DFC27D","#F6E8C3","#F5F5F5","#C7EAE5","#80CDC1","#35978F","#01665E","#003C30"],"symbolList":["emptyDiamond","emptyDiamond","emptyDiamond","emptyDiamond"],"legend":{"show":true,"data":["Sepal.Length","Sepal.Width","Petal.Length","Petal.Width"],"x":"left","y":"top"},"xAxis":{"name":"Sample ID","type":"category","boundaryGap":[0,0],"scale":true,"axisLine":{"show":true,"onZero":false},"data":["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32","33","34","35","36","37","38","39","40","41","42","43","44","45","46","47","48","49","50","51","52","53","54","55","56","57","58","59","60","61","62","63","64","65","66","67","68","69","70","71","72","73","74","75","76","77","78","79","80","81","82","83","84","85","86","87","88","89","90","91","92","93","94","95","96","97","98","99","100","101","102","103","104","105","106","107","108","109","110","111","112","113","114","115","116","117","118","119","120","121","122","123","124","125","126","127","128","129","130","131","132","133","134","135","136","137","138","139","140","141","142","143","144","145","146","147","148","149","150"]},"yAxis":{"name":"Measure","type":"value","scale":true,"axisLine":{"show":true,"onZero":false}}},"evals":[]}</script><!--/html_preserve-->

�߶�ƽ��`type='areasmooth'`��������������`dataZoom=TRUE`����ʼ��ʾ40%-80%��

```r
echartR(dfiris, x = ~id, y= ~Value, series= ~Param, type='areasmooth',
        palette='PiYG', xlab = 'Sample ID', ylab = 'Measure', 
        symbolList='none', dataZoom=T, dataZoomRange=c(40,80),
        title='Parameter measures (iris)')
```

<!--html_preserve--><div id="htmlwidget-6258" style="width:768px;height:480px;" class="echarts"></div>
<script type="application/json" data-for="htmlwidget-6258">{"x":{"title":{"text":"Parameter measures (iris)","subtext":"","padding":[25,5,5,5],"x":"center","y":"bottom"},"tooltip":{"trigger":"axis","axisPointer":{"show":true,"lineStyle":{"type":"dashed","width":1}}},"toolbox":{"show":true,"feature":{"mark":{"show":true,"title":{"mark":"辅助线开�? Auxiliary conductor switch","markUndo":"删除辅助�? Undo auxiliary conductor","markClear":"清空辅助�? Clear auxiliary conductor"}},"dataView":{"show":true,"readOnly":false,"title":"数据视图 Data view"},"magicType":{"show":true,"type":["line","bar","tiled","stack"],"title":{"line":"折线图切�? Switch to line chart","bar":"柱形图切�? Switch to bar chart","stack":"堆积 Stack","tiled":"平铺 Tile","force":"力导向布局图切�? Switch to force chart","pie":"饼图切换 Switch to pie chart","funnel":"漏斗图切�? Switch to funnel chart"}},"restore":{"show":true,"title":"还原 Restore"},"saveAsImage":{"show":true,"title":"保存为图�? Save as image"}},"x":"right","y":"top"},"calculable":true,"series":[{"name":"Sepal.Length","type":"line","data":["5.1","4.9","4.7","4.6","5","5.4","4.6","5","4.4","4.9","5.4","4.8","4.8","4.3","5.8","5.7","5.4","5.1","5.7","5.1","5.4","5.1","4.6","5.1","4.8","5","5","5.2","5.2","4.7","4.8","5.4","5.2","5.5","4.9","5","5.5","4.9","4.4","5.1","5","4.5","4.4","5","5.1","4.8","5.1","4.6","5.3","5","7","6.4","6.9","5.5","6.5","5.7","6.3","4.9","6.6","5.2","5","5.9","6","6.1","5.6","6.7","5.6","5.8","6.2","5.6","5.9","6.1","6.3","6.1","6.4","6.6","6.8","6.7","6","5.7","5.5","5.5","5.8","6","5.4","6","6.7","6.3","5.6","5.5","5.5","6.1","5.8","5","5.6","5.7","5.7","6.2","5.1","5.7","6.3","5.8","7.1","6.3","6.5","7.6","4.9","7.3","6.7","7.2","6.5","6.4","6.8","5.7","5.8","6.4","6.5","7.7","7.7","6","6.9","5.6","7.7","6.3","6.7","7.2","6.2","6.1","6.4","7.2","7.4","7.9","6.4","6.3","6.1","7.7","6.3","6.4","6","6.9","6.7","6.9","5.8","6.8","6.7","6.7","6.3","6.5","6.2","5.9"],"itemStyle":{"normal":{"areaStyle":{"type":"default"}}},"smooth":true},{"name":"Sepal.Width","type":"line","data":["3.5","3","3.2","3.1","3.6","3.9","3.4","3.4","2.9","3.1","3.7","3.4","3","3","4","4.4","3.9","3.5","3.8","3.8","3.4","3.7","3.6","3.3","3.4","3","3.4","3.5","3.4","3.2","3.1","3.4","4.1","4.2","3.1","3.2","3.5","3.6","3","3.4","3.5","2.3","3.2","3.5","3.8","3","3.8","3.2","3.7","3.3","3.2","3.2","3.1","2.3","2.8","2.8","3.3","2.4","2.9","2.7","2","3","2.2","2.9","2.9","3.1","3","2.7","2.2","2.5","3.2","2.8","2.5","2.8","2.9","3","2.8","3","2.9","2.6","2.4","2.4","2.7","2.7","3","3.4","3.1","2.3","3","2.5","2.6","3","2.6","2.3","2.7","3","2.9","2.9","2.5","2.8","3.3","2.7","3","2.9","3","3","2.5","2.9","2.5","3.6","3.2","2.7","3","2.5","2.8","3.2","3","3.8","2.6","2.2","3.2","2.8","2.8","2.7","3.3","3.2","2.8","3","2.8","3","2.8","3.8","2.8","2.8","2.6","3","3.4","3.1","3","3.1","3.1","3.1","2.7","3.2","3.3","3","2.5","3","3.4","3"],"itemStyle":{"normal":{"areaStyle":{"type":"default"}}},"smooth":true},{"name":"Petal.Length","type":"line","data":["1.4","1.4","1.3","1.5","1.4","1.7","1.4","1.5","1.4","1.5","1.5","1.6","1.4","1.1","1.2","1.5","1.3","1.4","1.7","1.5","1.7","1.5","1","1.7","1.9","1.6","1.6","1.5","1.4","1.6","1.6","1.5","1.5","1.4","1.5","1.2","1.3","1.4","1.3","1.5","1.3","1.3","1.3","1.6","1.9","1.4","1.6","1.4","1.5","1.4","4.7","4.5","4.9","4","4.6","4.5","4.7","3.3","4.6","3.9","3.5","4.2","4","4.7","3.6","4.4","4.5","4.1","4.5","3.9","4.8","4","4.9","4.7","4.3","4.4","4.8","5","4.5","3.5","3.8","3.7","3.9","5.1","4.5","4.5","4.7","4.4","4.1","4","4.4","4.6","4","3.3","4.2","4.2","4.2","4.3","3","4.1","6","5.1","5.9","5.6","5.8","6.6","4.5","6.3","5.8","6.1","5.1","5.3","5.5","5","5.1","5.3","5.5","6.7","6.9","5","5.7","4.9","6.7","4.9","5.7","6","4.8","4.9","5.6","5.8","6.1","6.4","5.6","5.1","5.6","6.1","5.6","5.5","4.8","5.4","5.6","5.1","5.1","5.9","5.7","5.2","5","5.2","5.4","5.1"],"itemStyle":{"normal":{"areaStyle":{"type":"default"}}},"smooth":true},{"name":"Petal.Width","type":"line","data":["0.2","0.2","0.2","0.2","0.2","0.4","0.3","0.2","0.2","0.1","0.2","0.2","0.1","0.1","0.2","0.4","0.4","0.3","0.3","0.3","0.2","0.4","0.2","0.5","0.2","0.2","0.4","0.2","0.2","0.2","0.2","0.4","0.1","0.2","0.2","0.2","0.2","0.1","0.2","0.2","0.3","0.3","0.2","0.6","0.4","0.3","0.2","0.2","0.2","0.2","1.4","1.5","1.5","1.3","1.5","1.3","1.6","1","1.3","1.4","1","1.5","1","1.4","1.3","1.4","1.5","1","1.5","1.1","1.8","1.3","1.5","1.2","1.3","1.4","1.4","1.7","1.5","1","1.1","1","1.2","1.6","1.5","1.6","1.5","1.3","1.3","1.3","1.2","1.4","1.2","1","1.3","1.2","1.3","1.3","1.1","1.3","2.5","1.9","2.1","1.8","2.2","2.1","1.7","1.8","1.8","2.5","2","1.9","2.1","2","2.4","2.3","1.8","2.2","2.3","1.5","2.3","2","2","1.8","2.1","1.8","1.8","1.8","2.1","1.6","1.9","2","2.2","1.5","1.4","2.3","2.4","1.8","1.8","2.1","2.4","2.3","1.9","2.3","2.5","2.3","1.9","2","2.3","1.8"],"itemStyle":{"normal":{"areaStyle":{"type":"default"}}},"smooth":true}],"renderAsImage":true,"color":["#8E0152","#C51B7D","#DE77AE","#F1B6DA","#FDE0EF","#F7F7F7","#E6F5D0","#B8E186","#7FBC41","#4D9221","#276419"],"symbolList":["none","none","none","none"],"dataZoom":{"show":true,"start":40,"end":80},"legend":{"show":true,"data":["Sepal.Length","Sepal.Width","Petal.Length","Petal.Width"],"x":"left","y":"top"},"xAxis":{"name":"Sample ID","type":"category","boundaryGap":[0,0],"scale":true,"axisLine":{"show":true,"onZero":false},"data":["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32","33","34","35","36","37","38","39","40","41","42","43","44","45","46","47","48","49","50","51","52","53","54","55","56","57","58","59","60","61","62","63","64","65","66","67","68","69","70","71","72","73","74","75","76","77","78","79","80","81","82","83","84","85","86","87","88","89","90","91","92","93","94","95","96","97","98","99","100","101","102","103","104","105","106","107","108","109","110","111","112","113","114","115","116","117","118","119","120","121","122","123","124","125","126","127","128","129","130","131","132","133","134","135","136","137","138","139","140","141","142","143","144","145","146","147","148","149","150"]},"yAxis":{"name":"Measure","type":"value","scale":true,"axisLine":{"show":true,"onZero":false}}},"evals":[]}</script><!--/html_preserve-->

### Stacked Area �ѻ����ͼ

```r
echartR(dfiris, x = ~id, y= ~Value, series= ~Param, type='area',stack=T,
        palette='PRGn', xlab = 'Sample ID', ylab = 'Measure',
        symbolList='emptyCircle',
        title='Parameter measures (iris)')
```

<!--html_preserve--><div id="htmlwidget-3021" style="width:768px;height:480px;" class="echarts"></div>
<script type="application/json" data-for="htmlwidget-3021">{"x":{"title":{"text":"Parameter measures (iris)","subtext":"","padding":[25,5,5,5],"x":"center","y":"bottom"},"tooltip":{"trigger":"axis","axisPointer":{"show":true,"lineStyle":{"type":"dashed","width":1}}},"toolbox":{"show":true,"feature":{"mark":{"show":true,"title":{"mark":"辅助线开�? Auxiliary conductor switch","markUndo":"删除辅助�? Undo auxiliary conductor","markClear":"清空辅助�? Clear auxiliary conductor"}},"dataView":{"show":true,"readOnly":false,"title":"数据视图 Data view"},"magicType":{"show":true,"type":["line","bar","tiled","stack"],"title":{"line":"折线图切�? Switch to line chart","bar":"柱形图切�? Switch to bar chart","stack":"堆积 Stack","tiled":"平铺 Tile","force":"力导向布局图切�? Switch to force chart","pie":"饼图切换 Switch to pie chart","funnel":"漏斗图切�? Switch to funnel chart"}},"restore":{"show":true,"title":"还原 Restore"},"saveAsImage":{"show":true,"title":"保存为图�? Save as image"}},"x":"right","y":"top"},"calculable":true,"series":[{"name":"Sepal.Length","type":"line","data":["5.1","4.9","4.7","4.6","5","5.4","4.6","5","4.4","4.9","5.4","4.8","4.8","4.3","5.8","5.7","5.4","5.1","5.7","5.1","5.4","5.1","4.6","5.1","4.8","5","5","5.2","5.2","4.7","4.8","5.4","5.2","5.5","4.9","5","5.5","4.9","4.4","5.1","5","4.5","4.4","5","5.1","4.8","5.1","4.6","5.3","5","7","6.4","6.9","5.5","6.5","5.7","6.3","4.9","6.6","5.2","5","5.9","6","6.1","5.6","6.7","5.6","5.8","6.2","5.6","5.9","6.1","6.3","6.1","6.4","6.6","6.8","6.7","6","5.7","5.5","5.5","5.8","6","5.4","6","6.7","6.3","5.6","5.5","5.5","6.1","5.8","5","5.6","5.7","5.7","6.2","5.1","5.7","6.3","5.8","7.1","6.3","6.5","7.6","4.9","7.3","6.7","7.2","6.5","6.4","6.8","5.7","5.8","6.4","6.5","7.7","7.7","6","6.9","5.6","7.7","6.3","6.7","7.2","6.2","6.1","6.4","7.2","7.4","7.9","6.4","6.3","6.1","7.7","6.3","6.4","6","6.9","6.7","6.9","5.8","6.8","6.7","6.7","6.3","6.5","6.2","5.9"],"stack":"Stack","itemStyle":{"normal":{"areaStyle":{"type":"default"}}}},{"name":"Sepal.Width","type":"line","data":["3.5","3","3.2","3.1","3.6","3.9","3.4","3.4","2.9","3.1","3.7","3.4","3","3","4","4.4","3.9","3.5","3.8","3.8","3.4","3.7","3.6","3.3","3.4","3","3.4","3.5","3.4","3.2","3.1","3.4","4.1","4.2","3.1","3.2","3.5","3.6","3","3.4","3.5","2.3","3.2","3.5","3.8","3","3.8","3.2","3.7","3.3","3.2","3.2","3.1","2.3","2.8","2.8","3.3","2.4","2.9","2.7","2","3","2.2","2.9","2.9","3.1","3","2.7","2.2","2.5","3.2","2.8","2.5","2.8","2.9","3","2.8","3","2.9","2.6","2.4","2.4","2.7","2.7","3","3.4","3.1","2.3","3","2.5","2.6","3","2.6","2.3","2.7","3","2.9","2.9","2.5","2.8","3.3","2.7","3","2.9","3","3","2.5","2.9","2.5","3.6","3.2","2.7","3","2.5","2.8","3.2","3","3.8","2.6","2.2","3.2","2.8","2.8","2.7","3.3","3.2","2.8","3","2.8","3","2.8","3.8","2.8","2.8","2.6","3","3.4","3.1","3","3.1","3.1","3.1","2.7","3.2","3.3","3","2.5","3","3.4","3"],"stack":"Stack","itemStyle":{"normal":{"areaStyle":{"type":"default"}}}},{"name":"Petal.Length","type":"line","data":["1.4","1.4","1.3","1.5","1.4","1.7","1.4","1.5","1.4","1.5","1.5","1.6","1.4","1.1","1.2","1.5","1.3","1.4","1.7","1.5","1.7","1.5","1","1.7","1.9","1.6","1.6","1.5","1.4","1.6","1.6","1.5","1.5","1.4","1.5","1.2","1.3","1.4","1.3","1.5","1.3","1.3","1.3","1.6","1.9","1.4","1.6","1.4","1.5","1.4","4.7","4.5","4.9","4","4.6","4.5","4.7","3.3","4.6","3.9","3.5","4.2","4","4.7","3.6","4.4","4.5","4.1","4.5","3.9","4.8","4","4.9","4.7","4.3","4.4","4.8","5","4.5","3.5","3.8","3.7","3.9","5.1","4.5","4.5","4.7","4.4","4.1","4","4.4","4.6","4","3.3","4.2","4.2","4.2","4.3","3","4.1","6","5.1","5.9","5.6","5.8","6.6","4.5","6.3","5.8","6.1","5.1","5.3","5.5","5","5.1","5.3","5.5","6.7","6.9","5","5.7","4.9","6.7","4.9","5.7","6","4.8","4.9","5.6","5.8","6.1","6.4","5.6","5.1","5.6","6.1","5.6","5.5","4.8","5.4","5.6","5.1","5.1","5.9","5.7","5.2","5","5.2","5.4","5.1"],"stack":"Stack","itemStyle":{"normal":{"areaStyle":{"type":"default"}}}},{"name":"Petal.Width","type":"line","data":["0.2","0.2","0.2","0.2","0.2","0.4","0.3","0.2","0.2","0.1","0.2","0.2","0.1","0.1","0.2","0.4","0.4","0.3","0.3","0.3","0.2","0.4","0.2","0.5","0.2","0.2","0.4","0.2","0.2","0.2","0.2","0.4","0.1","0.2","0.2","0.2","0.2","0.1","0.2","0.2","0.3","0.3","0.2","0.6","0.4","0.3","0.2","0.2","0.2","0.2","1.4","1.5","1.5","1.3","1.5","1.3","1.6","1","1.3","1.4","1","1.5","1","1.4","1.3","1.4","1.5","1","1.5","1.1","1.8","1.3","1.5","1.2","1.3","1.4","1.4","1.7","1.5","1","1.1","1","1.2","1.6","1.5","1.6","1.5","1.3","1.3","1.3","1.2","1.4","1.2","1","1.3","1.2","1.3","1.3","1.1","1.3","2.5","1.9","2.1","1.8","2.2","2.1","1.7","1.8","1.8","2.5","2","1.9","2.1","2","2.4","2.3","1.8","2.2","2.3","1.5","2.3","2","2","1.8","2.1","1.8","1.8","1.8","2.1","1.6","1.9","2","2.2","1.5","1.4","2.3","2.4","1.8","1.8","2.1","2.4","2.3","1.9","2.3","2.5","2.3","1.9","2","2.3","1.8"],"stack":"Stack","itemStyle":{"normal":{"areaStyle":{"type":"default"}}}}],"renderAsImage":true,"color":["#40004B","#762A83","#9970AB","#C2A5CF","#E7D4E8","#F7F7F7","#D9F0D3","#A6DBA0","#5AAE61","#1B7837","#00441B"],"symbolList":["emptyCircle","emptyCircle","emptyCircle","emptyCircle"],"legend":{"show":true,"data":["Sepal.Length","Sepal.Width","Petal.Length","Petal.Width"],"x":"left","y":"top"},"xAxis":{"name":"Sample ID","type":"category","boundaryGap":[0,0],"scale":true,"axisLine":{"show":true,"onZero":false},"data":["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32","33","34","35","36","37","38","39","40","41","42","43","44","45","46","47","48","49","50","51","52","53","54","55","56","57","58","59","60","61","62","63","64","65","66","67","68","69","70","71","72","73","74","75","76","77","78","79","80","81","82","83","84","85","86","87","88","89","90","91","92","93","94","95","96","97","98","99","100","101","102","103","104","105","106","107","108","109","110","111","112","113","114","115","116","117","118","119","120","121","122","123","124","125","126","127","128","129","130","131","132","133","134","135","136","137","138","139","140","141","142","143","144","145","146","147","148","149","150"]},"yAxis":{"name":"Measure","type":"value","scale":true,"axisLine":{"show":true,"onZero":false}}},"evals":[]}</script><!--/html_preserve-->

�߶�ƽ��(`type='areasmooth'`)���Զ���ɫ��������

```r
echartR(dfiris, x = ~id, y= ~Value, series= ~Param, type='areasmooth',stack=T,
        palette=c('red','yellow','limegreen','skyblue'), 
        xlab = 'Sample ID', ylab = 'Measure', 
        symbolList='none',
        title='Parameter measures (iris)')
```

<!--html_preserve--><div id="htmlwidget-994" style="width:768px;height:480px;" class="echarts"></div>
<script type="application/json" data-for="htmlwidget-994">{"x":{"title":{"text":"Parameter measures (iris)","subtext":"","padding":[25,5,5,5],"x":"center","y":"bottom"},"tooltip":{"trigger":"axis","axisPointer":{"show":true,"lineStyle":{"type":"dashed","width":1}}},"toolbox":{"show":true,"feature":{"mark":{"show":true,"title":{"mark":"辅助线开�? Auxiliary conductor switch","markUndo":"删除辅助�? Undo auxiliary conductor","markClear":"清空辅助�? Clear auxiliary conductor"}},"dataView":{"show":true,"readOnly":false,"title":"数据视图 Data view"},"magicType":{"show":true,"type":["line","bar","tiled","stack"],"title":{"line":"折线图切�? Switch to line chart","bar":"柱形图切�? Switch to bar chart","stack":"堆积 Stack","tiled":"平铺 Tile","force":"力导向布局图切�? Switch to force chart","pie":"饼图切换 Switch to pie chart","funnel":"漏斗图切�? Switch to funnel chart"}},"restore":{"show":true,"title":"还原 Restore"},"saveAsImage":{"show":true,"title":"保存为图�? Save as image"}},"x":"right","y":"top"},"calculable":true,"series":[{"name":"Sepal.Length","type":"line","data":["5.1","4.9","4.7","4.6","5","5.4","4.6","5","4.4","4.9","5.4","4.8","4.8","4.3","5.8","5.7","5.4","5.1","5.7","5.1","5.4","5.1","4.6","5.1","4.8","5","5","5.2","5.2","4.7","4.8","5.4","5.2","5.5","4.9","5","5.5","4.9","4.4","5.1","5","4.5","4.4","5","5.1","4.8","5.1","4.6","5.3","5","7","6.4","6.9","5.5","6.5","5.7","6.3","4.9","6.6","5.2","5","5.9","6","6.1","5.6","6.7","5.6","5.8","6.2","5.6","5.9","6.1","6.3","6.1","6.4","6.6","6.8","6.7","6","5.7","5.5","5.5","5.8","6","5.4","6","6.7","6.3","5.6","5.5","5.5","6.1","5.8","5","5.6","5.7","5.7","6.2","5.1","5.7","6.3","5.8","7.1","6.3","6.5","7.6","4.9","7.3","6.7","7.2","6.5","6.4","6.8","5.7","5.8","6.4","6.5","7.7","7.7","6","6.9","5.6","7.7","6.3","6.7","7.2","6.2","6.1","6.4","7.2","7.4","7.9","6.4","6.3","6.1","7.7","6.3","6.4","6","6.9","6.7","6.9","5.8","6.8","6.7","6.7","6.3","6.5","6.2","5.9"],"stack":"Stack","itemStyle":{"normal":{"areaStyle":{"type":"default"}}},"smooth":true},{"name":"Sepal.Width","type":"line","data":["3.5","3","3.2","3.1","3.6","3.9","3.4","3.4","2.9","3.1","3.7","3.4","3","3","4","4.4","3.9","3.5","3.8","3.8","3.4","3.7","3.6","3.3","3.4","3","3.4","3.5","3.4","3.2","3.1","3.4","4.1","4.2","3.1","3.2","3.5","3.6","3","3.4","3.5","2.3","3.2","3.5","3.8","3","3.8","3.2","3.7","3.3","3.2","3.2","3.1","2.3","2.8","2.8","3.3","2.4","2.9","2.7","2","3","2.2","2.9","2.9","3.1","3","2.7","2.2","2.5","3.2","2.8","2.5","2.8","2.9","3","2.8","3","2.9","2.6","2.4","2.4","2.7","2.7","3","3.4","3.1","2.3","3","2.5","2.6","3","2.6","2.3","2.7","3","2.9","2.9","2.5","2.8","3.3","2.7","3","2.9","3","3","2.5","2.9","2.5","3.6","3.2","2.7","3","2.5","2.8","3.2","3","3.8","2.6","2.2","3.2","2.8","2.8","2.7","3.3","3.2","2.8","3","2.8","3","2.8","3.8","2.8","2.8","2.6","3","3.4","3.1","3","3.1","3.1","3.1","2.7","3.2","3.3","3","2.5","3","3.4","3"],"stack":"Stack","itemStyle":{"normal":{"areaStyle":{"type":"default"}}},"smooth":true},{"name":"Petal.Length","type":"line","data":["1.4","1.4","1.3","1.5","1.4","1.7","1.4","1.5","1.4","1.5","1.5","1.6","1.4","1.1","1.2","1.5","1.3","1.4","1.7","1.5","1.7","1.5","1","1.7","1.9","1.6","1.6","1.5","1.4","1.6","1.6","1.5","1.5","1.4","1.5","1.2","1.3","1.4","1.3","1.5","1.3","1.3","1.3","1.6","1.9","1.4","1.6","1.4","1.5","1.4","4.7","4.5","4.9","4","4.6","4.5","4.7","3.3","4.6","3.9","3.5","4.2","4","4.7","3.6","4.4","4.5","4.1","4.5","3.9","4.8","4","4.9","4.7","4.3","4.4","4.8","5","4.5","3.5","3.8","3.7","3.9","5.1","4.5","4.5","4.7","4.4","4.1","4","4.4","4.6","4","3.3","4.2","4.2","4.2","4.3","3","4.1","6","5.1","5.9","5.6","5.8","6.6","4.5","6.3","5.8","6.1","5.1","5.3","5.5","5","5.1","5.3","5.5","6.7","6.9","5","5.7","4.9","6.7","4.9","5.7","6","4.8","4.9","5.6","5.8","6.1","6.4","5.6","5.1","5.6","6.1","5.6","5.5","4.8","5.4","5.6","5.1","5.1","5.9","5.7","5.2","5","5.2","5.4","5.1"],"stack":"Stack","itemStyle":{"normal":{"areaStyle":{"type":"default"}}},"smooth":true},{"name":"Petal.Width","type":"line","data":["0.2","0.2","0.2","0.2","0.2","0.4","0.3","0.2","0.2","0.1","0.2","0.2","0.1","0.1","0.2","0.4","0.4","0.3","0.3","0.3","0.2","0.4","0.2","0.5","0.2","0.2","0.4","0.2","0.2","0.2","0.2","0.4","0.1","0.2","0.2","0.2","0.2","0.1","0.2","0.2","0.3","0.3","0.2","0.6","0.4","0.3","0.2","0.2","0.2","0.2","1.4","1.5","1.5","1.3","1.5","1.3","1.6","1","1.3","1.4","1","1.5","1","1.4","1.3","1.4","1.5","1","1.5","1.1","1.8","1.3","1.5","1.2","1.3","1.4","1.4","1.7","1.5","1","1.1","1","1.2","1.6","1.5","1.6","1.5","1.3","1.3","1.3","1.2","1.4","1.2","1","1.3","1.2","1.3","1.3","1.1","1.3","2.5","1.9","2.1","1.8","2.2","2.1","1.7","1.8","1.8","2.5","2","1.9","2.1","2","2.4","2.3","1.8","2.2","2.3","1.5","2.3","2","2","1.8","2.1","1.8","1.8","1.8","2.1","1.6","1.9","2","2.2","1.5","1.4","2.3","2.4","1.8","1.8","2.1","2.4","2.3","1.9","2.3","2.5","2.3","1.9","2","2.3","1.8"],"stack":"Stack","itemStyle":{"normal":{"areaStyle":{"type":"default"}}},"smooth":true}],"renderAsImage":true,"color":["#FF0000","#FFFF00","#32CD32","#87CEEB"],"symbolList":["none","none","none","none"],"legend":{"show":true,"data":["Sepal.Length","Sepal.Width","Petal.Length","Petal.Width"],"x":"left","y":"top"},"xAxis":{"name":"Sample ID","type":"category","boundaryGap":[0,0],"scale":true,"axisLine":{"show":true,"onZero":false},"data":["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32","33","34","35","36","37","38","39","40","41","42","43","44","45","46","47","48","49","50","51","52","53","54","55","56","57","58","59","60","61","62","63","64","65","66","67","68","69","70","71","72","73","74","75","76","77","78","79","80","81","82","83","84","85","86","87","88","89","90","91","92","93","94","95","96","97","98","99","100","101","102","103","104","105","106","107","108","109","110","111","112","113","114","115","116","117","118","119","120","121","122","123","124","125","126","127","128","129","130","131","132","133","134","135","136","137","138","139","140","141","142","143","144","145","146","147","148","149","150"]},"yAxis":{"name":"Measure","type":"value","scale":true,"axisLine":{"show":true,"onZero":false}}},"evals":[]}</script><!--/html_preserve-->

## Funnel ©��ͼ

### Funnel ��ͨ©��ͼ


```r
echartR(dtcars, x = ~carburetor,  y = ~car, type='funnel',
        palette='RdBu', title='Number of carburetors of cars (mtcars)')
```

<!--html_preserve--><div id="htmlwidget-2032" style="width:768px;height:480px;" class="echarts"></div>
<script type="application/json" data-for="htmlwidget-2032">{"x":{"title":{"text":"Number of carburetors of cars (mtcars)","subtext":"","padding":[25,5,5,5],"x":"center","y":"bottom"},"tooltip":{"trigger":"item","axisPointer":{"show":true,"lineStyle":{"type":"dashed","width":1}}},"toolbox":{"show":true,"feature":{"mark":{"show":true,"title":{"mark":"辅助线开�? Auxiliary conductor switch","markUndo":"删除辅助�? Undo auxiliary conductor","markClear":"清空辅助�? Clear auxiliary conductor"}},"dataView":{"show":true,"readOnly":false,"title":"数据视图 Data view"},"magicType":{"show":false},"restore":{"show":true,"title":"还原 Restore"},"saveAsImage":{"show":true,"title":"保存为图�? Save as image"}},"x":"right","y":"top"},"calculable":true,"series":[{"name":"carburetor","type":"funnel","data":[{"value":7,"name":"1"},{"value":10,"name":"2"},{"value":3,"name":"3"},{"value":10,"name":"4"},{"value":1,"name":"6"},{"value":1,"name":"8"}],"x":"10%"}],"renderAsImage":true,"color":["#67001F","#B2182B","#D6604D","#F4A582","#FDDBC7","#F7F7F7","#D1E5F0","#92C5DE","#4393C3","#2166AC","#053061"],"legend":{"show":true,"data":["1","2","3","4","6","8"],"x":"left","y":"top"}},"evals":[]}</script><!--/html_preserve-->

### Pyramid ������ͼ
������ͼ������©��ͼ��


```r
echartR(dtcars, x = ~carburetor,  y = ~car, type='pyramid',
        palette='RdGy', title='Number of carburetors of cars (mtcars)')
```

<!--html_preserve--><div id="htmlwidget-8549" style="width:768px;height:480px;" class="echarts"></div>
<script type="application/json" data-for="htmlwidget-8549">{"x":{"title":{"text":"Number of carburetors of cars (mtcars)","subtext":"","padding":[25,5,5,5],"x":"center","y":"bottom"},"tooltip":{"trigger":"item","axisPointer":{"show":true,"lineStyle":{"type":"dashed","width":1}}},"toolbox":{"show":true,"feature":{"mark":{"show":true,"title":{"mark":"辅助线开�? Auxiliary conductor switch","markUndo":"删除辅助�? Undo auxiliary conductor","markClear":"清空辅助�? Clear auxiliary conductor"}},"dataView":{"show":true,"readOnly":false,"title":"数据视图 Data view"},"magicType":{"show":false},"restore":{"show":true,"title":"还原 Restore"},"saveAsImage":{"show":true,"title":"保存为图�? Save as image"}},"x":"right","y":"top"},"calculable":true,"series":[{"name":"carburetor","type":"funnel","data":[{"value":7,"name":"1"},{"value":10,"name":"2"},{"value":3,"name":"3"},{"value":10,"name":"4"},{"value":1,"name":"6"},{"value":1,"name":"8"}],"x":"25%","sort":"ascending"}],"renderAsImage":true,"color":["#67001F","#B2182B","#D6604D","#F4A582","#FDDBC7","#FFFFFF","#E0E0E0","#BABABA","#878787","#4D4D4D","#1A1A1A"],"legend":{"show":true,"data":["1","2","3","4","6","8"],"x":"left","y":"top"}},"evals":[]}</script><!--/html_preserve-->

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

<!--html_preserve--><div id="htmlwidget-6657" style="width:768px;height:480px;" class="echarts"></div>
<script type="application/json" data-for="htmlwidget-6657">{"x":{"title":{"text":"Lahm vs Alves (by @mixedknuts)","subtext":"","padding":[25,5,5,5],"x":"center","y":"bottom"},"tooltip":{"trigger":"axis","axisPointer":{"show":true,"lineStyle":{"type":"dashed","width":1}}},"toolbox":{"show":true,"feature":{"mark":{"show":true,"title":{"mark":"辅助线开�? Auxiliary conductor switch","markUndo":"删除辅助�? Undo auxiliary conductor","markClear":"清空辅助�? Clear auxiliary conductor"}},"dataView":{"show":true,"readOnly":false,"title":"数据视图 Data view"},"magicType":{"show":false},"restore":{"show":true,"title":"还原 Restore"},"saveAsImage":{"show":true,"title":"保存为图�? Save as image"}},"x":"right","y":"top"},"calculable":true,"series":[{"name":"value","type":"radar","data":[{"value":[86.62,2.11,0.99,20.78,1.58,1.64,0.9,1.71],"name":"Dani Alves"},{"value":[89.67,1.51,0.97,24.32,0.83,0.86,1.15,0.47],"name":"Philipp Lahm"}]}],"renderAsImage":true,"color":["#FF0000","#0000FF"],"symbolList":["none","none"],"legend":{"show":true,"data":["Dani Alves","Philipp Lahm"],"x":"left","y":"top"},"polar":[{"indicator":[{"text":"Passing%","max":112.0875},{"text":"Key passing","max":2.6375},{"text":"Comp crosses","max":1.2375},{"text":"Crossing%","max":30.4},{"text":"Successful dribbles","max":1.975},{"text":"Dispossessed","max":2.05},{"text":"Dribbled past","max":1.4375},{"text":"Fouls","max":2.1375}]}]},"evals":[]}</script><!--/html_preserve-->

### Solid Radar ʵ���״�


```r
echartR(player, x= ~para, y= ~value, series= ~name, type='radarfill',
        symbolList='none', palette=c('firebrick1','dodgerblue'),
        title='Lahm vs Alves (by @mixedknuts)')
```

<!--html_preserve--><div id="htmlwidget-1271" style="width:768px;height:480px;" class="echarts"></div>
<script type="application/json" data-for="htmlwidget-1271">{"x":{"title":{"text":"Lahm vs Alves (by @mixedknuts)","subtext":"","padding":[25,5,5,5],"x":"center","y":"bottom"},"tooltip":{"trigger":"axis","axisPointer":{"show":true,"lineStyle":{"type":"dashed","width":1}}},"toolbox":{"show":true,"feature":{"mark":{"show":true,"title":{"mark":"辅助线开�? Auxiliary conductor switch","markUndo":"删除辅助�? Undo auxiliary conductor","markClear":"清空辅助�? Clear auxiliary conductor"}},"dataView":{"show":true,"readOnly":false,"title":"数据视图 Data view"},"magicType":{"show":false},"restore":{"show":true,"title":"还原 Restore"},"saveAsImage":{"show":true,"title":"保存为图�? Save as image"}},"x":"right","y":"top"},"calculable":true,"series":[{"name":"value","type":"radar","data":[{"value":[86.62,2.11,0.99,20.78,1.58,1.64,0.9,1.71],"name":"Dani Alves"},{"value":[89.67,1.51,0.97,24.32,0.83,0.86,1.15,0.47],"name":"Philipp Lahm"}],"itemStyle":{"normal":{"areaStyle":{"type":"default"}}}}],"renderAsImage":true,"color":["#FF3030","#1E90FF"],"symbolList":["none","none"],"legend":{"show":true,"data":["Dani Alves","Philipp Lahm"],"x":"left","y":"top"},"polar":[{"indicator":[{"text":"Passing%","max":112.0875},{"text":"Key passing","max":2.6375},{"text":"Comp crosses","max":1.2375},{"text":"Crossing%","max":30.4},{"text":"Successful dribbles","max":1.975},{"text":"Dispossessed","max":2.05},{"text":"Dribbled past","max":1.4375},{"text":"Fouls","max":2.1375}]}]},"evals":[]}</script><!--/html_preserve-->

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

<!--html_preserve--><div id="htmlwidget-5054" style="width:768px;height:480px;" class="echarts"></div>
<script type="application/json" data-for="htmlwidget-5054">{"x":{"title":{"text":"GDPs of China Provinces, 2012-2014 (Million USD)","subtext":"","padding":[25,5,5,5],"x":"center","y":"bottom"},"tooltip":{"trigger":"item","axisPointer":{"show":true,"lineStyle":{"type":"dashed","width":1}}},"toolbox":{"show":true,"feature":{"mark":{"show":true,"title":{"mark":"辅助线开�? Auxiliary conductor switch","markUndo":"删除辅助�? Undo auxiliary conductor","markClear":"清空辅助�? Clear auxiliary conductor"}},"dataView":{"show":true,"readOnly":false,"title":"数据视图 Data view"},"magicType":{"show":false},"restore":{"show":true,"title":"还原 Restore"},"saveAsImage":{"show":true,"title":"保存为图�? Save as image"}},"x":"right","y":"center","orient":"vertical"},"calculable":true,"series":[{"type":"map","mapType":"china","roam":true,"data":[{"value":904046,"name":"�㶫"},{"value":856368,"name":"����"},{"value":792289,"name":"ɽ��"},{"value":549154,"name":"�㽭"},{"value":468900,"name":"����"},{"value":420990,"name":"�ӱ�"},{"value":393607,"name":"����"},{"value":378183,"name":"�Ĵ�"},{"value":352482,"name":"����"},{"value":350958,"name":"����"},{"value":319710,"name":"�Ϻ�"},{"value":312107,"name":"����"},{"value":283238,"name":"����"},{"value":272666,"name":"����"},{"value":251574,"name":"���ɹ�"},{"value":228969,"name":"����"},{"value":216896,"name":"������"},{"value":206497,"name":"����"},{"value":205131,"name":"����"},{"value":204259,"name":"���"},{"value":191886,"name":"ɽ��"},{"value":189136,"name":"����"},{"value":180746,"name":"����"},{"value":163318,"name":"����"},{"value":118896,"name":"�½�"},{"value":108550,"name":"����"},{"value":89508,"name":"����"},{"value":45236,"name":"����"},{"value":37090,"name":"����"},{"value":29997,"name":"�ຣ"},{"value":11105,"name":"����"}],"name":"2012","itemStyle":{"normal":{"label":{"show":false}},"emphasis":{"label":{"show":true}}}},{"type":"map","mapType":"china","roam":true,"data":[{"value":1003746,"name":"�㶫"},{"value":955269,"name":"����"},{"value":882974,"name":"ɽ��"},{"value":606609,"name":"�㽭"},{"value":519212,"name":"����"},{"value":456976,"name":"�ӱ�"},{"value":437216,"name":"����"},{"value":424026,"name":"�Ĵ�"},{"value":398316,"name":"����"},{"value":395622,"name":"����"},{"value":351347,"name":"����"},{"value":348804,"name":"�Ϻ�"},{"value":314871,"name":"����"},{"value":307416,"name":"����"},{"value":271788,"name":"���ɹ�"},{"value":259078,"name":"����"},{"value":232237,"name":"������"},{"value":232158,"name":"����"},{"value":232031,"name":"���"},{"value":231520,"name":"����"},{"value":209608,"name":"����"},{"value":204364,"name":"����"},{"value":203485,"name":"ɽ��"},{"value":189255,"name":"����"},{"value":134991,"name":"�½�"},{"value":129284,"name":"����"},{"value":101208,"name":"����"},{"value":50805,"name":"����"},{"value":41417,"name":"����"},{"value":33925,"name":"�ຣ"},{"value":13041,"name":"����"}],"name":"2013","itemStyle":{"normal":{"label":{"show":false}},"emphasis":{"label":{"show":true}}}},{"type":"map","mapType":"china","roam":true,"data":[{"value":1103605,"name":"�㶫"},{"value":1059587,"name":"����"},{"value":967419,"name":"ɽ��"},{"value":653668,"name":"�㽭"},{"value":568786,"name":"����"},{"value":478953,"name":"�ӱ�"},{"value":466018,"name":"����"},{"value":464555,"name":"�Ĵ�"},{"value":445514,"name":"����"},{"value":440328,"name":"����"},{"value":391609,"name":"����"},{"value":383554,"name":"�Ϻ�"},{"value":347249,"name":"����"},{"value":339401,"name":"����"},{"value":289274,"name":"���ɹ�"},{"value":287978,"name":"����"},{"value":255950,"name":"���"},{"value":255724,"name":"����"},{"value":255144,"name":"����"},{"value":244829,"name":"������"},{"value":232230,"name":"����"},{"value":224715,"name":"����"},{"value":208612,"name":"����"},{"value":207714,"name":"ɽ��"},{"value":150812,"name":"�½�"},{"value":150599,"name":"����"},{"value":111273,"name":"����"},{"value":56989,"name":"����"},{"value":44802,"name":"����"},{"value":37460,"name":"�ຣ"},{"value":14990,"name":"����"}],"name":"2014","itemStyle":{"normal":{"label":{"show":false}},"emphasis":{"label":{"show":true}}}}],"renderAsImage":true,"color":["#3366CC","#DC3912","#FF9900","#109618","#990099","#0099C6","#DD4477","#66AA00","#B82E2E","#316395","#994499","#22AA99","#AAAA11","#6633CC","#E67300","#8B0707","#651067","#329262","#5574A6","#3B3EAC"],"dataRange":{"show":true,"calculable":[],"text":["High","Low"],"itemGap":5,"min":0,"max":3308624,"color":["#FF0000","#FFA500","#FFFF00","#00FF00","#32CD32"],"splitNumber":0},"legend":{"show":true,"data":["2012","2013","2014"],"x":"left","y":"top"},"roamController":{"show":true,"mapTypeControl":{"china":true},"x":"right","width":60,"height":90}},"evals":[]}</script><!--/html_preserve-->

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

<!--html_preserve--><div id="htmlwidget-5893" style="width:768px;height:480px;" class="echarts"></div>
<script type="application/json" data-for="htmlwidget-5893">{"x":{"title":{"text":"Nations with top 20 GDPs, 2014 (Million USD) - Wikipedia","subtext":"","padding":[25,5,5,5],"x":"center","y":"bottom"},"tooltip":{"trigger":"item","axisPointer":{"show":true,"lineStyle":{"type":"dashed","width":1}}},"toolbox":{"show":true,"feature":{"mark":{"show":true,"title":{"mark":"辅助线开�? Auxiliary conductor switch","markUndo":"删除辅助�? Undo auxiliary conductor","markClear":"清空辅助�? Clear auxiliary conductor"}},"dataView":{"show":true,"readOnly":false,"title":"数据视图 Data view"},"magicType":{"show":false},"restore":{"show":true,"title":"还原 Restore"},"saveAsImage":{"show":true,"title":"保存为图�? Save as image"}},"x":"right","y":"center","orient":"vertical"},"calculable":true,"series":[{"type":"map","mapType":"world","roam":true,"data":[{"value":17418925,"name":"United States of America"},{"value":10380380,"name":"China"},{"value":4616335,"name":"Japan"},{"value":3859547,"name":"Germany"},{"value":2945146,"name":"United Kingdom"},{"value":2846889,"name":"France"},{"value":2353025,"name":"Brazil"},{"value":2147952,"name":"Italy"},{"value":2049501,"name":"India"},{"value":1857461,"name":"Russia"},{"value":1788717,"name":"Canada"},{"value":1444189,"name":"Australia"},{"value":1416949,"name":"South Korea"},{"value":1406855,"name":"Spain"},{"value":1282725,"name":"Mexico"},{"value":888648,"name":"Indonesia"},{"value":866354,"name":"Netherlands"},{"value":806108,"name":"Turkey"},{"value":752459,"name":"Saudi Arabia"},{"value":712050,"name":"Switzerland"}],"name":"GDP","itemStyle":{"normal":{"label":{"show":false}},"emphasis":{"label":{"show":true}}}}],"renderAsImage":true,"color":["#7AC143","#7D3F98","#F47721","#D20962","#00A78E","#00BCE4","#B8D936","#EE3D94","#FDB933","#F58F9F","#60C3AE","#5F78BB","#5E9732","#CEA979","#EF4135","#7090A5"],"dataRange":{"show":true,"calculable":false,"text":["High","Low"],"itemGap":5,"min":0,"max":19089613,"color":["#FF0000","#CCFF00","#00FF66","#0066FF","#CC00FF"],"splitNumber":10},"roamController":{"show":true,"mapTypeControl":{"world":true},"x":"right","width":60,"height":90}},"evals":[]}</script><!--/html_preserve-->

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