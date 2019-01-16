$(document).on("shiny:connected", function(){
    $.get("http://ipinfo.io", function(response) {
        Shiny.onInputChange("getIP", response);
    }, "json");
});