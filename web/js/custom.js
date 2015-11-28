/** 
 * A hack to highlight the currently open page's button in nav bar 
 */
var pathname = window.location.pathname;
var aMap = { 
    "/": "#home-button",
    "/about.html": "#about-button",
    "/contact.html" : "#contact-button",
    "/miscellany/miscellany.html": "#misc-button",
    "/archive.html": "#archive-button"
};

var bid = aMap[window.location.pathname];
if (bid) {
    $(bid).css("color", "white")
} else if (pathname.indexOf("miscellany") > -1) {
    $("#misc-button").css("color", "white");
}
