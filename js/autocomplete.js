$(document).ready(function() {
    $("#input").autocompleteArray(["Maito", "Kahvi", "Kissanruoka"]).bind("activate.autocomplete", function(e,d) { alert(d) });
});
