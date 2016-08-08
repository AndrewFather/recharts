HTMLWidgets.widget({
  name: 'echarts',
  type: 'output',

  initialize: function(el, width, height) {
    return echarts.init(el);
  },

  renderValue: function(el, x, instance) {
    var theme = x.theme;
    //require(['lib/themes/' + theme], function(theme){instance.setTheme(theme);});
    if (theme  === "default" || theme === "macarons" || theme === "infographic"){
       instance.setTheme(theme);
    }else{
        //var themeObj = eval(JSON.parser('lib/themes/' + theme + '.js'));
        require('lib/themes/' + theme);
        instance.setTheme(theme);
    }
	instance.setOption(x);
  },

  resize: function(el, width, height, instance) {
  }


});
