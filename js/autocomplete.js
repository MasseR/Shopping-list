$(document).ready(function() {
    $("#input").autocomplete("ostoslista.cgi", {"extraParams" : {"mode" : "autocomplete"}});
    $("#input")[0].focus();
});
