// © 2013 Xah Lee, http://xahlee.info/
"use strict";

/*
comment 
*/

function countChars (inputString) {
    // returns (json), the keys are chars, the values are count of occurance of each char.
    //  that's  some-lingual thing
    var hashtable = {};

    for (var i = 0; i < inputString.length; i++) {
        var c = inputString[i];
        if (  hashtable.hasOwnProperty(c) ) {
            hashtable[c] += 1;
        } else {
            hashtable[c] = 1;
        }
    }
    return hashtable;
}


// example of using Date object
console.log( (new Date("2014-12-31T23:08")).getFullYear() ) // 2014
console.log( (new Date("2014-12-31T23:08")).getMonth() ) // 11
console.log( (new Date("2014-12-31T23:08")).getDate() ) // 31
console.log( (new Date("2014-12-31T23:08")).getDay() ) // 3
console.log( (new Date("2014-12-31T23:08")).getHours() ) // 23
console.log( (new Date("2014-12-31T23:08")).getMinutes() ) // 8
console.log( (new Date("2014-12-31T23:08")).getSeconds() ) // 0

// create tooltip element.
var ttBox = document.createElement("div");

// set style
ttBox.style.visibility="hidden"; // ← CRITICAL trick: make it hidden till mouse over
ttBox.style.position="fixed";
ttBox.style.top="1ex";
ttBox.style.left="1ex";
ttBox.style.borderRadius="30px";
ttBox.style.backgroundColor="silver";

// insert into DOM
document.body.appendChild(ttBox);

function ttActivate (evt) {
    // get the position of the hover element
    var boundBox = evt.target.getBoundingClientRect();
    var coordX = boundBox.left;
    var coordY = boundBox.top;

    // adjust bubble position
    ttBox.style.left= (coordX + 40).toString() + "px";
    ttBox.style.top= (coordY + 40).toString() + "px";

    // add bubble content. Can be any HTML
    ttBox.innerHTML = "<span style='font-size:12ex;color:red'>" + "♥" + "</span>";

    // make bubble VISIBLE
    ttBox.style.visibility="visible";
}

function ttDeactivate(evt) {
    ttBox.style.visibility="hidden";
}

var hoverEle = document.getElementById("t42310");
// assign handler
hoverEle.addEventListener("mouseover", ttActivate , false);
hoverEle.addEventListener("mouseout", ttDeactivate , false);

var unicodedata = { 32:"SPACE", 33:"EXCLAMATION MARK"};

// from https://developer.mozilla.org/en-US/docs/JavaScript/Reference/Global_Objects/String/charCodeAt
function fixedCharCodeAt (str, idx) {
    // ex. fixedCharCodeAt ('\uD800\uDC00', 0); // 65536
    // ex. fixedCharCodeAt ('\uD800\uDC00', 1); // 65536
    idx = idx || 0;
    var code = str.charCodeAt(idx);
    var hi, low;
    if (0xD800 <= code && code <= 0xDBFF) { // High surrogate (could change last hex to 0xDB7F to treat high private surrogates as single characters)
        hi = code;
        low = str.charCodeAt(idx+1);
        if (isNaN(low)) {
            throw 'High surrogate not followed by low surrogate in fixedCharCodeAt()';
        }
        return ((hi - 0xD800) * 0x400) + (low - 0xDC00) + 0x10000;
    }
    if (0xDC00 <= code && code <= 0xDFFF) { // Low surrogate
        // We return false to allow loops to skip this iteration since should have already handled high surrogate above in the previous iteration
        return false;
        /*hi = str.charCodeAt(idx-1);
        low = code;
        return ((hi - 0xD800) * 0x400) + (low - 0xDC00) + 0x10000;*/
    }
    return code;
}

if (!String.fromCodePoint) {
// ES6 Unicode Shims 0.1 , © 2012 Steven Levithan http://slevithan.com/ , MIT License
    String.fromCodePoint = function fromCodePoint () {
        var chars = [], point, offset, units, i;
        for (i = 0; i < arguments.length; ++i) {
            point = arguments[i];
            offset = point - 0x10000;
            units = point > 0xFFFF ? [0xD800 + (offset >> 10), 0xDC00 + (offset & 0x3FF)] : [point];
            chars.push(String.fromCharCode.apply(null, units));
        }
        return chars.join("");
    };
}

// activate tooltip
function popupActivate (φevt) {
    resetEleStyle(lastEle);

    φevt.target.style.color="maroon";
    φevt.target.style.backgroundColor="yellow";

    var uChar = φevt.target.firstChild.nodeValue;
    // var uNum = uChar.charCodeAt(0);
    var uNum = fixedCharCodeAt(uChar,0);
    var uName = unicodedata[uNum];
    var uHex = (uNum).toString(16);

    var boundBox = φevt.target.getBoundingClientRect();
    var coordX = boundBox.left;
    var coordY = boundBox.top;

    balloon.style.visibility="visible";
    balloon.style.color="maroon";
    balloon.style.position="fixed";
    balloon.style.left= (coordX + 40).toString() + "px";
    balloon.style.top= (coordY + 40).toString() + "px";
    balloon.innerHTML = "<table><tr><td><span style='font-size:9ex'>" + uChar + "</span></td><td>" + uNum + "<br />x" + uHex + "</td></tr><tr><td colspan='2'>" + uName + "</td></tr></table>";

    lastEle = φevt.target;
}

function resetEleStyle(φele) {
    if (typeof φele !== "undefined") {
        φele.style.color="initial";
        φele.style.backgroundColor="#faf0e6";
    }
}

function popupOff(φevt) {
    resetEleStyle(φevt.target);
    balloon.style.visibility="hidden";
}

function doSearch (φkeyupEvent) {
    var codePoint;
    var codePointStr;
    var ii;
    var srchText = searchBox.value;
    srchText = srchText.trim();

    // var rightKey = function (kk) {
    //     if (
    //         kk === 13 ||                    // enter
    //         kk === 8 ||                     // backspace
    //         kk === 32 ||                     // space
    //         (kk >= 48  && kk <= 57) ||           // digits
    //         (kk >= 65  && kk <= 90) ||           // uppercase letters
    //         (kk >= 97  && kk <= 122)            // lowercase letters
    //     ) { return true;} else { return false;}
    // }

    // if ( ! rightKey(φkeyupEvent.keyCode)) {
    //     if ( srchText.length > 1  ) {
    //         return;
    //     }
    // }

    foundCodepoints = [];        // clear it

    if (srchText.length === 0) { }
    // if search text 1st char is non-ascii
    else if (fixedCharCodeAt(srchText,0) > 127 ) { for (ii = 0; ii < srchText.length ; ii++) { if ( srchText.slice(ii, ii+1) === " " ) { } else { foundCodepoints.push(fixedCharCodeAt(srchText,ii));} } }
    // if search text is a # followed by number, just return the unicode with that codepoint
    else if ( /#[0-9]+/.test(srchText) ) {
        codePoint = parseInt(srchText.substring(1));
        if ( unicodedata[codePoint] !== undefined ) {
            for (ii = codePoint; ii < (codePoint + maxNumOfResults) ; ii++) {
                if ( unicodedata[ii] !== undefined ) {
                    foundCodepoints.push(ii);
                } } } }
    // search text is #x followed by hex
    else if ( /#x[0-9a-f]+/i.test(srchText) ) {
        codePoint = parseInt( "0" + srchText.substring(1));
        if ( unicodedata[codePoint] !== undefined ) {
            for (ii = codePoint; ii < (codePoint + maxNumOfResults) ; ii++) {
                if ( unicodedata[ii] !== undefined ) {
                    foundCodepoints.push(ii);
                } } } }
    else                        // normal search on unicode name
    {
        var myRegex = new RegExp(srchText,"i");

        for (ii = 0; ii < codepointsArray.length ; ii++) {
            codePointStr = codepointsArray[ii];
            if ( myRegex.test(unicodedata[codePointStr]) ) {
                foundCodepoints.push(parseInt(codePointStr));
            }
            if (foundCodepoints.length >= maxNumOfResults ) {
                break;
            } } }

    displaySearchResult(foundCodepoints, resultBox);

}

// ampersand encode these < > &
function htmlEncode(φstr) { return document.createElement("div").appendChild( document.createTextNode(φstr) ).parentNode.innerHTML; };

// function htmlEncode2( φstr ) { return document.createElement("div").appendChild( document.createTextNode( φstr ) ).parentNode.innerHTML;};

// display unicode char results
// charArray is a array of numbers (unicode code point)
// anchorNode is a elemet to add result elements as children
function displaySearchResult (φcharArray, φanchorNode) {

    var createOneResult = function (codePoint) {
        var rslt = document.createElement("table");
        rslt.innerHTML = "<tr><td><span style='font-size:6ex;color:red'>" + String.fromCodePoint(codePoint) + "</span></td><td>" + codePoint + "<br />x" + codePoint.toString(16) + "</td></tr><tr><td colspan='2'>" + unicodedata[codePoint] + "</td></tr>";

        rslt.style.display="inline-block";
        rslt.style.border="solid thin gray";
        rslt.style.width="18ex";
        rslt.style.margin="0.2ex";
        rslt.style.padding="0.2ex";
        rslt.style.borderRadius="14px";

        return rslt;
    };

    // clear the result first
    while (φanchorNode.hasChildNodes()) { φanchorNode.removeChild(φanchorNode.lastChild); }

    if ( φcharArray.length === 0 && searchBox.value.length !== 0) {
        var noResult = document.createElement("span");
        noResult.innerHTML = "None found. Control chars, invisible chars, are not included. Charset = Unicode 6.";
        noResult.style.color="red";
        φanchorNode.appendChild(noResult);
    } else { φcharArray.forEach( function (x) { φanchorNode.appendChild(createOneResult(x));} ); }

    φanchorNode.appendChild(
        (function () {
            var r38 = document.createElement("p");
            r38.innerHTML = ( φcharArray.length === maxNumOfResults ) ? "<b> Only first " + maxNumOfResults + " number of items shown.</b>" : "";
            return r38;})()
    );

    φanchorNode.appendChild(
        (function () {
            var r10 = document.createElement("p");
            r10.innerHTML = φcharArray.map(function (x) {return String.fromCodePoint(x);}).join(" ");
            return r10;})()
    );

}

function init93369 () {

    // create balloon element, insert as first child of φrefNode
    function createBalloon (φrefNode) {
        // create balloon element to display info
        balloon = document.createElement("div");
        balloon.setAttribute("id", "t26283");
        balloon.style.visibility="hidden";
        balloon.style.position="fixed";
        balloon.style.top="1ex";
        balloon.style.left="1ex";
        balloon.style.textAlign="left";
        balloon.style.border="solid thin red";
        balloon.style.borderRadius="30px";
        balloon.style.maxWidth="25ex";
        balloon.style.padding="1ex";
        balloon.style.paddingTop="0";
        balloon.style.backgroundColor="hsl(0,0%,70%)";
        balloon.style.boxShadow="3px 3px 8px black";
        balloon.style.zIndex="341";
        balloon.style.opacity = 1;
        balloon.appendChild(document.createTextNode("nothing"));
        // insert into DOM
        φrefNode.insertBefore(balloon, φrefNode.firstChild);
    }

    var myList = document.getElementsByClassName("unicode");

    // assign handler to hot elements
    if ( myList.length > 0 ) {
        for (var ii = 0; ii < myList.length; ii++) {
            var myNode = myList[ii];
            if (
                myNode.tagName === "MARK"
            ) {
                myNode.addEventListener("mouseover", popupActivate , false);
                myNode.addEventListener("mousedown", popupActivate , false);
                myNode.addEventListener("mouseout", popupOff , false);
                myNode.setAttribute("title", "");
            } } }
    createBalloon(document.body);

    // add event listenter for search box
    searchBox.addEventListener("keyup", doSearch , false);

    // set default text to search hox
    searchBox.setAttribute("title", "Type “star”, decimal search “#97”, hexadecimal search “#x61”, or a Unicode “♥”");

    // insert resultBox into DOM, after searchBox
    searchBox.parentNode.appendChild(document.createElement("br"));
    searchBox.parentNode.appendChild(document.createTextNode("Type star or #97 or #x61 or paste ♥¥α©"));

    // create a element as anchor point for search results
    resultBox.appendChild(document.createTextNode(""));

    // insert resultBox into DOM, after searchBox
    searchBox.parentNode.appendChild(resultBox);
}

var balloon;
var lastEle;

var searchBox = document.getElementById("unicode-search-box-94154");
var resultBox = document.createElement("div");
var maxNumOfResults = 200;
var codepointsArray = Object.keys(unicodedata); // note, each is a string, not int

// each element is a integer (the unicode code point)
var foundCodepoints = [];

init93369();
