show = false;
function hideSelected()
{
    if(show) return;
    var checked = $("#items > li > input:checked");
    $("#hiddenitems").show().text(checked.size() + " items hidden");
    checked.each(function() {
	$(this).parent().hide();
    });
}

function showAll()
{
    $("#items > li").each(function() {
	$(this).show();
    });
}

function bindHide()
{
    $("#items > li > input").each(function(idx) {
	$(this).change(function() {
	    hideSelected();
	})
    });
}

function toggle()
{
    show = !show;
    if(show)
	showAll();
    else
	hideSelected();
}

function post()
{
}

$(document).ready(function() {
    $("#show").click(toggle);
    bindHide();
    $("#append").submit(function() {
	$.post("ostoslista.cgi?mode=ajax", $(this).serialize(), function(data) {
	    $("#items").html(data);
	    $("#input").attr("value", "");
	    bindHide();
	});
	return false;
    });
    $("#check").submit(function() { /* Need to figure out how to fix the this issue. */
	$.post("ostoslista.cgi?mode=ajax", $(this).serialize(), function(data) {
	    $("#items").html(data);
	    bindHide();
	});
	return false;
    });
});
