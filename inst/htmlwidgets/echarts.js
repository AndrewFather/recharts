//require.config ({
//    paths:{
//        echarts: "http://echarts.baidu.com/echarts2/build/dist",
//        ethemes: "http://echarts.baidu.com/echarts2/doc/example/theme"
//    }
//});

//function _getTheme(theme){
//    var myTheme;
//    require(["echarts", "ethemes" + theme], function(theme){ myTheme = theme;});
//    return myTheme;
//}

HTMLWidgets.widget({
  name: 'echarts',
  type: 'output',

  initialize: function(el, width, height) {
    return echarts.init(el);
  },


  renderValue: function(el, x, instance) {
    var theme = 'default';
    if (typeof(x.theme) != 'undefined'){ theme = x.theme; }

    if (theme === "default" || theme === "macarons" ||
       theme === "infographic"){
        instance.setTheme(theme);
    }else{
        //var themeObj = _getTheme(theme);
        //alert(typeof themeObj);
        //instance.setTheme(themeObj);
        instance.setTheme(theme);
    }
	instance.setOption(x);
  },

  resize: function(el, width, height, instance) {

  }


});
