function hideSelected()
{
    var checked = $("#items > li > input:checked");
    $("#hiddenitems").show().text(checked.size() + " items hidden");
    $("#show").show();
    checked.each(function() {
	$(this).parent().hide();
    });
}

function showAll()
{
    $("#items > li").each(function() {
	$(this).show();
    });
    $("#hiddenitems").hide();
    $("#show").hide();
}

$(document).ready(function() {
    $("#hiddenitems").hide();
    $("#show").hide();
    $("#items > li > input").each(function(idx) {
	$(this).change(function() {
	    hideSelected();
	})
    });
    $("#show").click(function() {
	showAll();
    });
});
