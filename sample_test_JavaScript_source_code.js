/* JavaScript syntax coloring test */
typeof "abc"; // ⇒ string
typeof 3;     // ⇒ number
typeof 3.5;   // ⇒ number
typeof NaN;   // ⇒ number
typeof Infinity;   // ⇒ number
typeof false; // ⇒ boolean
typeof null;  // ⇒ object. (this is Historical baggage. Should be “null”)
typeof undefined; // ⇒ undefined
console.log( Boolean(false) );  // false
console.log( Boolean(null) );   // false
console.log( Boolean(undefined) ); // false
console.log( Boolean(NaN) );    // false
console.log( Boolean(Infinity) ); // true

alert("yes");                   // comment
console.log("yes");
var s1 = "jane's cat" + 'jane\'s cat';
"abc".length;

3 + 4 - 4 * 4 / 4 + 10 % 3; -(3+4);

Math.pow(2,3);    // 8 → exponential
Math.pow(2,1/2);  // 1.4142135623730951 → square root
var f = Math.floor( x );
var c = Math.ceil( x );
var a = 1, b = 2, c = 3;

function f ()  {
var n = 3;
{
var n = 4;
}
console.log(n);
};

f(); // prints 4

// “function” is a special object
typeof function () {return 3;}; // ⇒ function

true && true || !true;

3 < 4;         // ⇒ true
3 > 4;         // ⇒  false

3 == 3;        // ⇒ true.
3 == "3";      // ⇒ true.  note: 「==」 does automatic type conversion
3 === "3";     // ⇒ false.  True if both sides are same type and equal

3 != 4;        // ⇒ true. The 「!=」 is the negation of 「==」
3 !== 4;       // ⇒ true. The 「!==」 is the negation of 「===」

if (3 < 4) {console.log("yes");};

if (3 <= 4) {console.log("yes");}
else {console.log("no");}

var x = 3;
if (x == 1) {console.log("is 1");}
else if (x == 2) {console.log("is 2");}
else if (x == 3) {console.log("is 3");}
else {console.log("not found");}

if (3 > 4) console.log("yes"); else console.log("no"); // prints 「no」

var y = (4 > 5) ? "yes" : "no";

var x = "a"; // change the value to test

switch(x) {
    case "w":
        console.log("is w");
        break; // without “break”, it'll continue to run rest without testing
    case "a":
        console.log("is a");
        break;
    case 3:
        console.log("is 3");
        break;
    default:
        console.log("none of the above");
}

for (var i=0; i < 9; i++) { console.log(i); }
var x = 0;
while (x != 5) { console.log(x); x++;} // prints 0 to 4
var x = 0;
do { console.log(x); x++} while (x != 5) // prints 0 to 4

for (var i=0; i <= 5; i++) {
    if (i==3) {continue;}       // skip 3
    console.log(i);
}                               // prints 0 to 5, but not 3

for (var i=0; i < 5; i++) {
    console.log(i);
    if (i===3) {break;}
}                               // prints 0 to 3

var aa = ["one", "two", 3];
console.log(aa); // [ 'one', 'two', 3 ]

var aa = [];  // define a array
aa[0] = "zero"; // assign a value to a element
aa[1] = "one";
aa[3] = "more"; // non-existent element automatically extend the array

console.log(aa);           // [ 'zero', 'one', , 'more' ]

var aa = [7, 8, 2];  // define a array

console.log(aa.length); // 3

var aa = [2, 4, 1];

// access a element
console.log(aa[0]);     // 2

var aa = [2, 4, 1];

aa[0]= "no";

console.log(aa);     // [ 'no', 4, 1 ]

var aa = ["pa", ["deep", [4,5]], 3];
console.log(aa[1][1][0]); // prints 4

// loop thru arry
aa = [3,7,4];
for (var i in aa) {
    console.log(aa[i]);
}

var nn = {"mary":19, "jane":20, "john":25};

nn["mary"]              // ⇒ 19, bracket notation
nn.mary                 // ⇒ 19, Dot notation also works

〔☛ JavaScript: Dot Notation vs Bracket Notation for Accessing Properties〕

To delete a property, use the operator delete.

var nn = {"mary":19, "jane":20, "john":25};

delete nn["mary"]  // delete a property

console.log(nn);   // { jane: 20, john: 25 }

var nn = {"a":19, "c":20, "b":25};

for (var x in nn) {
console.log(x);     // prints each key
console.log(nn[x]); // prints each value
}

var j = {"a":19, "c":20, "b":25};

console.log( Object.keys(j) );     // prints [ 'a', 'c', 'b' ]

var x = {"a":19, "b":20, "c":[3,4]};
console.log( x["c"][0]); // prints 3

var y = [3,4];
var x = {"a":19, "b":20, "c":y}; // the y is array
console.log( x["c"][0] ); // prints 3
console.log(
 {"a":19, "b":20, "c":[3,4]}["c"][0]
);
 // prints 3

var myStructure = {
  name: {
    first: "Mary",
    last: "Smith"
  },
  age: 19,
  hobbies: [ "shopping", "dancing" ]
};

aa = [3,7];
aa["h"] = "whoa!";
aa["x"] = "huh?";

for (var i in aa) { console.log(aa[i]); }

console.log(aa);                // [ 3, 7, h: 'whoa!', x: 'huh?' ]

console.log(aa.length);         // 2

var xx = "jane@example.com";

console.log(xx.search(/@.+com/)); // 4

console.log(xx.search(/z/)); // -1  not found

var xlink = "http://google.com/";

var search_result = xlink.search(/oo/);

if ( search_result !== -1) {
    console.log("found match");
    console.log(search_result); // 8 ← the index of match
} else {
    console.log("not found");
}

var x1 = '<img src="cat.jpg">';

var x2 = x1.replace(/<img src="([-_\w]+)\.jpg">/, '<img src="$1.jpg" alt="$1">');

console.log(x2); // prints <img src="cat.jpg" alt="cat">
var xx = '<img class="pict" src="cat.jpg" alt="my cat" width="600" height="400">';

var result = xx.match(/<img class="([^"]+)" src="([^"]+)" alt="([^"]+)" width="([^"]+)" height="([^"]+)">/);

console.log(result[0]); // <img class="i" src="cat.jpg" alt="my cat" width="600" height="400">
console.log(result[1]); // pict
console.log(result[2]); // cat.jpg
console.log(result[3]); // my cat
console.log(result[4]); // 600
console.log(result[5]); // 400

function ff(x, y) { return x + y;}
console.log(ff(3, 4)); // 7

function ff() {
    return arguments[0]; // returns the first argument
}

console.log(ff(3,4,5)); // prints 3

// creating a object.
var bb = {};
console.log(typeof bb);         // → 「"object"」

// creating a object with some properties
var cc = {"p1":1, "p2":2};
console.log(typeof cc);         // → 「"object"」
console.log(cc);                // → { p1: 1, p2: 2 }

oj = {};     // creating a object
oj.c1= 3;    // creating a “property” c1 for object oj
oj.c2= 4;

console.log(oj.c1); // prints 3

oj = {"p1":1};                  // creating a object with property p1
oj.p2 = 2;                      // creating a property p2
oj["p3"] = 3;                   // creating a property p3

console.log(oj);                // { p1: 1, p2: 2, p3: 3 }
console.log(oj.p1);             // 1
console.log(oj["p2"]);          // 2
console.log(oj["p3"]);          // 3

j = {};                            // create a object
j.m = function (x) {return x + 1;}; // create a method named m

// calling the method
var y = j.m(3);

console.log(y);                 // prints 4

// create a object, with one property p1
var oo = {"p1":3};

// set property p2 to a function, which simply return 「this」
oo.p2 = function () { return this; };

// show oo
console.log( oo );               // { p1: 3, p2: [Function] }

console.log( oo === oo.p2() );   // true

// returns true because p2 is a function that returns 「this」, which is oo

"use strict";
// from https://developer.mozilla.org/en-US/docs/JavaScript/Reference/Global_Objects/String/charCodeAt
function fixedCharCodeAt (str, idx) {

    idx = idx || 0;
    var code = str.charCodeAt(idx);
    var hi, low;
    if (0xD800 <= code && code <= 0xDBFF) {
        hi = code;
        low = str.charCodeAt(idx+1);
        if (isNaN(low)) {
            throw 'High surrogate not followed by low surrogate in fixedCharCodeAt()';
        }
        return ((hi - 0xD800) * 0x400) + (low - 0xDC00) + 0x10000;
    }
    if (0xDC00 <= code && code <= 0xDFFF) {
        return false;
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

var unicodedata= [];

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

