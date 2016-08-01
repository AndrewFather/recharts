
tooltipJS <- function(type) {
    # tooltipJS templates for echarts
    js <- list()
    js[['time']] <- 'function (params) {
    var date = new Date(params.value[0]);
    data = date.getFullYear() + "-"
        + (date.getMonth() + 1) + "-"
        + date.getDate() + " "
        + date.getHours() + ":"
        + date.getMinutes();
    if (param.value.length > 2) {
        return data + "<br/>"
        + params.value[1] + ", "
        + params.value[2];
    } else {
        return data + "<br/>"
        + params.value[1];
    }
    }'

    js[['scatter']] <- 'function (params) {
    var i;
    var text;
    if (params.seriesName === null || params.seriesName === ""){
        if (params.value.length > 1) {
            text = params.value[0];
            for (i = 1; i < params.value.length; i++){
                text += " ,    " + params.value[i];
            }
            return text;
        } else {
            return params.name + " : " + params.value;
        }
    } else {
        if (params.value.length > 1) {
            text = params.seriesName + " :<br/>" + params.value[0];
            for (i = 1; i < params.value.length; i++) {
                text += " ,    " + params.value[i];
            }
            return text;
        } else {
            return params.seriesName + " :<br/>"
            + params.name + " : " + params.value;
        }
    }
    }'

    js[['chord_mono']] <- 'function (params) {
    if (params.name && params.name.indexOf("-") != -1) {
    return params.name.replace("-", " " + params.seriesName + " ")
    }
    else {
    return params.name ? params.name : params.data.id
    }
    }'

    js[['chord_multi']] <- 'function (params) {
    if (params.indicator2) {    // is edge
    return params.indicator2 + " " +
    params.name + " " + params.indicator + " : " +
    params.value.weight;
    } else {    // is node
    return params.name
    }
    }'

    js[['pie']] <- '{a} <br/>{b} : {c} ({d}%)'

    js[['k']] <- 'function (params) {
        var res = params[0].name;
        res += "<br/>  Open : " + params[0].value[0] +
        "  High : " + params[0].value[3];
        res += "<br/>  Close : " + params[0].value[1] +
        "  Low : " + params[0].value[2];
        return res;
    }'

    js[['hist']] <- 'function (params){
        return params.value[2] + "<br/>Count:" +
        params.value[1];
    }'
    switch(type,
           time = js$time, scatter = js$scatter, chord_mono = js$chord_mono,
           chord_multi = js$chord_multi, pie = js$pie, k = js$k,
           hist = js$hist)
    }
