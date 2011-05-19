$(document).ready(function() {
    $("#append").submit(function() {
	$.post("ostoslista.cgi?mode=ajaxadd", $(this).serialize(), function(data) {
	    $("#items").html(data);
	});
	return false;
    });
});
