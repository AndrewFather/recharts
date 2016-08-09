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

        var themeObj = require(['../lib/echarts/echarts-all',
           '../lib/echarts/themes/' + theme],
           function(theme){ return theme;});
        //var themeObj = JSON.parse(theme);

        alert(themeObj);
        instance.setTheme(themeObj);
    }
	instance.setOption(x);
  },

  resize: function(el, width, height, instance) {

  }


});
