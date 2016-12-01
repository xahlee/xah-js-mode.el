;;; xah-js-mode.el --- Major mode for editing JavaScript.

;; Copyright © 2013 by Xah Lee

;; Author: Xah Lee ( http://xahlee.info/ )
;; Version: 0.9.1
;; Created: 23 March 2013
;; Keywords: languages, JavaScript
;; URL: http://ergoemacs.org/emacs/xah-js-mode.html

;; This file is not part of GNU Emacs.

;; You can redistribute this program and/or modify it under the terms of the GNU General Public License version 2.

;;; Commentary:

;; Major mode for editing JavaScript code.

;; alpha stage. used by me. Works fine for me. lots to do yet.

;;; HISTORY

;; version 0.1, 2013-08-21 first version

;; TODO
;; add new faces
;; separate diff types of keywords to use diff face
;; add indentation
;; add support for autocomplete


;;; Code:

(require 'js) ; temp, borrow js-indent-line

(require 'newcomment) ; in emacs
(require 'ido)        ; in emacs
;; (require 'lisp-mode) ; in emacs. for indent-sexp. temp hack. todo


(defvar xah-js-mode-hook nil "Standard hook for `xah-js-mode'")



(defface xah-js-function-param-fc
  '((t :foreground "DarkGreen" :weight bold))
  "face for function parameters."
  :group 'xah-js-mode )

(defface xah-js-user-variable-fc
  '((t :foreground "magenta"
      :weight bold
      ))
  "face for user variables."
  :group 'xah-js-mode )

(defface xah-js-identifier-ε
  '((t :foreground "red"
      :weight bold
      ))
  "face for user variables."
  :group 'xah-js-mode )

(face-spec-set
 'xah-js-identifier-ε
 '((t :foreground "red"
      :weight bold
      ))
 'face-defface-spec
 )

(defface xah-js-identifier-ƒ
  '((t :foreground "purple"
      :weight bold
      ))
  "face for user variables."
  :group 'xah-js-mode )

(face-spec-set
 'xah-js-identifier-ƒ
 '((t :foreground "purple"
      :weight bold
      ))
 'face-defface-spec
 )

(defface xah-js-identifier-γ
  '((t :foreground "#104e8b"
      :weight bold
      ))
  "face for user variables."
  :group 'xah-js-mode )

(face-spec-set
 'xah-js-identifier-γ
 '((t :foreground "#104e8b"
      :weight bold
      ))
 'face-defface-spec
 )

(defface xah-js-cap-word
  '(
    (t :foreground "firebrick" :weight bold))
  "Face for capitalized word."
  :group 'xah-js-mode )


;; abbrev

(defun xah-js-abbrev-enable-function ()
  "Return t if not in string or comment. Else nil.
This is for abbrev table property `:enable-function'.
Version 2016-10-24"
  (let ((-syntax-state (syntax-ppss)))
    (not (or (nth 3 -syntax-state) (nth 4 -syntax-state)))))

(defun xah-js-expand-abbrev ()
  "Expand the symbol before cursor,
if cursor is not in string or comment.
Returns the abbrev symbol if there's a expansion, else nil.
Version 2016-10-24"
  (interactive)
  (when (xah-js-abbrev-enable-function) ; abbrev property :enable-function doesn't seem to work, so check here instead
    (let (
          -p1 -p2
          -abrStr
          -abrSymbol
          )
      (save-excursion
        (forward-symbol -1)
        (setq -p1 (point))
        (forward-symbol 1)
        (setq -p2 (point)))
      (setq -abrStr (buffer-substring-no-properties -p1 -p2))
      (setq -abrSymbol (abbrev-symbol -abrStr))
      (if -abrSymbol
          (progn
            (abbrev-insert -abrSymbol -abrStr -p1 -p2 )
            (xah-js--abbrev-position-cursor -p1)
            -abrSymbol)
        nil))))

(defun xah-js--abbrev-position-cursor (&optional *pos)
  "Move cursor back to ▮ if exist, else put at end.
Return true if found, else false.
Version 2016-10-24"
  (interactive)
  (let ((-found-p (search-backward "▮" (if *pos *pos (max (point-min) (- (point) 100))) t )))
    (when -found-p (delete-char 1) )
    -found-p
    ))

(defun xah-js--abbrev-hook-f ()
  "Abbrev hook function, used for `define-abbrev'.
 Our use is to prevent inserting the char that triggered expansion. Experimental.
 the “ahf” stand for abbrev hook function.
Version 2016-10-24"
  t)

(put 'xah-js--abbrev-hook-f 'no-self-insert t)

(setq xah-js-mode-abbrev-table nil)
(define-abbrev-table 'xah-js-mode-abbrev-table
  '(

 ("oa" "Object.assign ( target▮, source1, source2, etc )" xah-js--abbrev-hook-f)
 ("assign" "assign ( target▮, source1, source2, etc )" xah-js--abbrev-hook-f)
 ("oc" "Object.create (▮, {pname, { value : 3, writable: true, enumerable: false, configurable: true, set: function, get: function }}, etc)" xah-js--abbrev-hook-f)
 ("create" "create (▮, {pname, { value : 3, writable: true, enumerable: false, configurable: true, set: function, get: function }}, etc)" xah-js--abbrev-hook-f)
 ("odps" "Object.defineProperties (▮, {pname, { value : 3, writable: true, enumerable: false, configurable: true, set: function, get: function }}, etc)" xah-js--abbrev-hook-f)
 ("defineProperties" "Object.defineProperties (▮, {pname, { value : 3, writable: true, enumerable: false, configurable: true, set: function, get: function }}, etc)" xah-js--abbrev-hook-f)
 ("defineProperties" "defineProperties(▮, {pname, { value : 3, writable: true, enumerable: false, configurable: true, set: function, get: function }}, etc)" xah-js--abbrev-hook-f)
 ("odp" "Object.defineProperty(▮, pname, { value : 3, writable: true, enumerable: false, configurable: true, set: function, get: function })" xah-js--abbrev-hook-f)
 ("defineProperty" "defineProperty(▮, pname, { value : 3, writable: true, enumerable: false, configurable: true, set: function, get: function })" xah-js--abbrev-hook-f)
 ("dp" "defineProperty(▮, pname, { value : 3, writable: true, enumerable: false, configurable: true, set: function, get: function })" xah-js--abbrev-hook-f)
 ("of" "Object.freeze ( ▮ )" xah-js--abbrev-hook-f)
 ("ogopd" "Object.getOwnPropertyDescriptor ( ▮, P )" xah-js--abbrev-hook-f)
 ("getOwnPropertyDescriptor" "getOwnPropertyDescriptor ( ▮, P )" xah-js--abbrev-hook-f)
 ("gopd" "getOwnPropertyDescriptor (▮)" xah-js--abbrev-hook-f)
 ("ogopn" "Object.getOwnPropertyNames ( ▮ )" xah-js--abbrev-hook-f)
 ("getOwnPropertyNames" "getOwnPropertyNames ( ▮ )" xah-js--abbrev-hook-f)
 ("gopn" "getOwnPropertyNames ( ▮ )" xah-js--abbrev-hook-f)
 ("ogops" "Object.getOwnPropertySymbols ( ▮ )" xah-js--abbrev-hook-f)
 ("gops" "getOwnPropertySymbols ( ▮ )" xah-js--abbrev-hook-f)
 ("getOwnPropertySymbols" "getOwnPropertySymbols ( ▮ )" xah-js--abbrev-hook-f)
 ("ogpo" "Object.getPrototypeOf ( ▮ )" xah-js--abbrev-hook-f)
 ("getPrototypeOf" "getPrototypeOf ( ▮ )" xah-js--abbrev-hook-f)
 ("gpto" "getPrototypeOf ( ▮ )" xah-js--abbrev-hook-f)

 ("oi" "Object.is ( value1▮, value2 )" xah-js--abbrev-hook-f)
 ("oie" "Object.isExtensible ( ▮ )" xah-js--abbrev-hook-f)
 ("oif" "Object.isFrozen ( ▮ )" xah-js--abbrev-hook-f)
 ("ois" "Object.isSealed ( ▮ )" xah-js--abbrev-hook-f)
 ("ok" "Object.keys ( ▮ )" xah-js--abbrev-hook-f)
 ("ope" "Object.preventExtensions ( ▮ )" xah-js--abbrev-hook-f)
 ("op" "Object.prototype" xah-js--abbrev-hook-f)
 ("os" "Object.seal ( ▮ )" xah-js--abbrev-hook-f)
 ("ospo" "Object.setPrototypeOf ( ▮, proto )" xah-js--abbrev-hook-f)

;; Object.prototype
("hop" "hasOwnProperty ( ▮ )" xah-js--abbrev-hook-f)
("hasOwnProperty" "hasOwnProperty (▮)" xah-js--abbrev-hook-f)
("ipo" "isPrototypeOf ( ▮ )" xah-js--abbrev-hook-f)
("isPrototypeOf" "isPrototypeOf ( ▮ )" xah-js--abbrev-hook-f)
("pie" "propertyIsEnumerable ( ▮ )" xah-js--abbrev-hook-f)
("propertyIsEnumerable" "propertyIsEnumerable ( ▮ )" xah-js--abbrev-hook-f)
("tls" "toLocaleString (▮)" xah-js--abbrev-hook-f)
("toLocaleString" "toLocaleString (▮)" xah-js--abbrev-hook-f)
("ts" "toString ()" xah-js--abbrev-hook-f)
("toString" "toString ()" xah-js--abbrev-hook-f)
("vo" "valueOf ( ▮ )" xah-js--abbrev-hook-f)
("valueOf" "valueOf ( ▮ )" xah-js--abbrev-hook-f)

;; Array
 ("ap" "Array.prototype" xah-js--abbrev-hook-f)
 ("aia" "Array.isArray ( ▮ )" xah-js--abbrev-hook-f)
 ("afm" "Array.from ( items▮ , mapfn, thisArg )" xah-js--abbrev-hook-f)
 ("map" "map( f▮ , thisBinding)" xah-js--abbrev-hook-f)

;; Array.prototype
("concat" "concat ( args1▮, args2, etc )" xah-js--abbrev-hook-f)
("copyWithin" "copyWithin (target▮, start , end  )" xah-js--abbrev-hook-f)
("cw" "copyWithin ( target▮, start , end  )" xah-js--abbrev-hook-f)
("entries" "entries ()" xah-js--abbrev-hook-f)
("every" "every ( f▮, thisArg )" xah-js--abbrev-hook-f)
("fill" "fill ( value▮, start , end )" xah-js--abbrev-hook-f)
("filter" "filter ( f▮, thisArg )" xah-js--abbrev-hook-f)
("find" "find ( predicate▮ , thisArg )" xah-js--abbrev-hook-f)
("findIndex" "findIndex ( predicate▮ , thisArg )" xah-js--abbrev-hook-f)
 ("forEach" "forEach ( f▮ , thisBinding)" xah-js--abbrev-hook-f)
("fe" "forEach ( f▮ , thisBinding)" xah-js--abbrev-hook-f)
("indexOf" "indexOf ( searchElement▮, fromIndex )" xah-js--abbrev-hook-f)
("indexOf" "indexOf ( searchElement▮, fromIndex )" xah-js--abbrev-hook-f)
("io" "indexOf ( searchElement▮, fromIndex )" xah-js--abbrev-hook-f)
("join" "join ('separator')" xah-js--abbrev-hook-f)
("keys" "keys ()" xah-js--abbrev-hook-f)
("lastIndexOf" "lastIndexOf ( searchElement▮, fromIndex )" xah-js--abbrev-hook-f)
("le" "length" xah-js--abbrev-hook-f)
("map" "map ( f▮, thisArg )" xah-js--abbrev-hook-f)
("pop" "pop ()" xah-js--abbrev-hook-f)
("push" "push ( items▮, … )" xah-js--abbrev-hook-f)
("reduce" "reduce ( f▮, initialValue )" xah-js--abbrev-hook-f)
("reduceRight" "reduceRight ( f▮, initialValue )" xah-js--abbrev-hook-f)
("rr" "reduceRight ( f▮, initialValue )" xah-js--abbrev-hook-f)
("reverse" "reverse ()" xah-js--abbrev-hook-f)
("shift" "shift ()" xah-js--abbrev-hook-f)
("slice" "slice (start▮, end)" xah-js--abbrev-hook-f)
("some" "some ( f▮, thisArg )" xah-js--abbrev-hook-f)
("sort" "sort (comparefn)" xah-js--abbrev-hook-f)
("splice" "splice (start▮, deleteCount , items … )" xah-js--abbrev-hook-f)
("toLocaleString" "toLocaleString ()" xah-js--abbrev-hook-f)
("toString" "toString ()" xah-js--abbrev-hook-f)
("unshift" "unshift ( items▮ … )" xah-js--abbrev-hook-f)
("values" "values ( )" xah-js--abbrev-hook-f)

;; lang syntax
 ("af" "(x▮ => { 3; })" xah-js--abbrev-hook-f)
 ("af2" "((x▮, x2) => ({ 3 }))" xah-js--abbrev-hook-f)
 ("c" "const ▮ = 3" xah-js--abbrev-hook-f)
 ("case" "case ▮: x; break" xah-js--abbrev-hook-f)
 ("cst" "constructor" xah-js--abbrev-hook-f)
 ("cl" "console.log(▮)" xah-js--abbrev-hook-f)
 ("class" "class A▮ {\n  constructor(x) {\n    this.p = x;\n  }\n}" xah-js--abbrev-hook-f :system t)
 ("cls" "class A▮ {\n  constructor(x) {\n    this.p = x;\n  }\n}" xah-js--abbrev-hook-f :system t)
 ("cm" "/* ▮ */" xah-js--abbrev-hook-f)
 ("cmt" "/**/n * desc▮./n * @param {string} title The title of the book./n * @return {number} The circumference of the circle./n */" xah-js--abbrev-hook-f)
 ("do" "do { ▮; x++} while (x != 5)" xah-js--abbrev-hook-f)
 ("ei" "else if (▮) { 3 }" xah-js--abbrev-hook-f)
 ("else" "else { ▮ }" xah-js--abbrev-hook-f)
 ("eq" "=== " xah-js--abbrev-hook-f)
 ("fi" "for (let p▮ in obj) { }" xah-js--abbrev-hook-f)
 ("finally" "finally {\n▮\n}" xah-js--abbrev-hook-f)
 ("fo" "for (let p▮ of obj) { }" xah-js--abbrev-hook-f)
 ("for" "for (let i = 0; i < ▮.length; i++) { }" xah-js--abbrev-hook-f)
 ("function" "function ▮ () { return 3 }" xah-js--abbrev-hook-f)
 ("f" "function ▮ () { 3 }" xah-js--abbrev-hook-f :system t)
 ("gf" "function* ▮ () { yield 3;}" xah-js--abbrev-hook-f :system t)
 ("switch" "switch(▮) {\n    case 3:\n3\n        break\n    case 3:\n3\n        break\n    default:\n        3\n}" xah-js--abbrev-hook-f)
 ("te" "( test▮ ? expr1 : expr2 )" xah-js--abbrev-hook-f)
 ("to" "typeof " xah-js--abbrev-hook-f)
 ("try" "try {\n▮\n} catch (error) {\n▮\n}" xah-js--abbrev-hook-f)
 ("u" "undefined" xah-js--abbrev-hook-f)
 ("us" "\"use strict\"" xah-js--abbrev-hook-f)
 ("v" "var ▮ = 3" xah-js--abbrev-hook-f)
 ("while" "while (i<10) { ▮; i++ }" xah-js--abbrev-hook-f)
 ("yi" "yield ▮;" xah-js--abbrev-hook-f)
 ("if" "if ( ▮ ) {\n}" xah-js--abbrev-hook-f)
 ("l" "let ▮ = 3" xah-js--abbrev-hook-f)
 ("p" "+" xah-js--abbrev-hook-f)
 ("r" "return ▮;" xah-js--abbrev-hook-f)

 ("o" "Object." xah-js--abbrev-hook-f)
 ("pt" "prototype." xah-js--abbrev-hook-f)
 ("a" "Array" xah-js--abbrev-hook-f)

 ("rok" "Reflect.ownKeys ()" xah-js--abbrev-hook-f)

 ("sy" "Symbol" xah-js--abbrev-hook-f)

 ("reduce" "reduce(f▮, first_value)" xah-js--abbrev-hook-f)
 ("reduceRight" "reduceRight(f▮, first_value)" xah-js--abbrev-hook-f)

;; dom
 ("d" "document." xah-js--abbrev-hook-f)
 ("addEventListener" "addEventListener(\"click\", ▮ , false)" xah-js--abbrev-hook-f)
 ("ael" "addEventListener" xah-js--abbrev-hook-f)
 ("gebi" "getElementById(\"▮\" xah-js--abbrev-hook-f)" xah-js--abbrev-hook-f)
 ("getElementById" "getElementById(\"▮\" xah-js--abbrev-hook-f)" xah-js--abbrev-hook-f)
 ("setInterval" "setInterval(func, delay, param1, param2)" xah-js--abbrev-hook-f)
 ("setTimeout" "setTimeout(func, delay, param1, param2)" xah-js--abbrev-hook-f) ;
 ("si" "setInterval(func, delay, param1, param2)" xah-js--abbrev-hook-f)
 ("st" "setTimeout(func, delay, param1, param2)" xah-js--abbrev-hook-f)
 ("w" "window." xah-js--abbrev-hook-f)

 ;;
 )

  "abbrev table for `xah-js-mode'"
  )

(abbrev-table-put xah-js-mode-abbrev-table :regexp "\\([_0-9A-Za-z$]+\\)")
(abbrev-table-put xah-js-mode-abbrev-table :case-fixed t)
(abbrev-table-put xah-js-mode-abbrev-table :system t)
(abbrev-table-put xah-js-mode-abbrev-table :enable-function 'xah-js-abbrev-enable-function)


(defvar xah-js-keyword-builtin nil "List of js names")
(setq xah-js-keyword-builtin '(
"break"
"case"
"catch"
"continue"
"debugger"
"default"
"delete"
"do"
"else"
"finally"
"for"
"of"
"function"
"function*"
"if"
"in"
"instanceof"
"new"
"return"
"switch"
"this"
"throw"
"try"
"typeof"
"var"
"void"
"while"
"with"

"done"
"next"

) )

(defvar xah-js-lang-words nil "List of JavaScript keywords.")
(setq xah-js-lang-words '(

"JSON"
"Number"
"Object"
"Math"
"Array"
"Boolean"
"Date"
"Error"
"Function"
"Reflect"
"Set"
"Map"
"WeakMap"
"WeakSet"
"Proxy"
"Symbol"
"iterator"

;; Set.prototype
"size"
"has"
"clear"
"add"

;; Map.prototype
"set"
"get"
"values"

"ownKeys" ; Reflect

"insertBefore" ; hack. todo

"EvalError"
"RangeError"
"ReferenceError"
"RegExp"
"String"
"SyntaxError"
"TypeError"
"URIError"
"abstract"
"arguments"
"boolean"
"byte"
"char"
"class"
"const"
"decodeURI"
"decodeURIComponent"
"double"
"encodeURI"
"encodeURIComponent"
"enum"
"eval"
"export"
"extends"
"final"
"float"
"goto"
"implements"
"import"
"int"
"interface"
"isFinite"
"isNaN"
"let"
"long"
"native"
"package"
"parseFloat"
"parseInt"
"private"
"protected"
"public"
"short"
"static"
"super"
"synchronized"
"throws"
"transient"
"undefined"
"volatile"
"yield"

"writable"
"enumerable"
"configurable"

;; --------------------

 ;; todo remove when all js object and properties are classified properly
"toFixed"
"substring"
"value"

"apply"
"call"
"arguments"

"JSON.stringify"
"JSON.parse"
"__proto__"

"isSafeInteger"
"isInteger"
"isNaN"
"EPSILON"
"MIN_SAFE_INTEGER"
"MAX_SAFE_INTEGER"
) )

(defvar xah-js-Object-properties nil "List of Object properties.")
(setq xah-js-Object-properties '(
"assign"
"create"
"defineProperties"
"defineProperty"
"freeze"
"getOwnPropertyDescriptor"
"getOwnPropertyNames"
"getOwnPropertySymbols"
"getPrototypeOf"
"is"
"isExtensible"
"isFrozen"
"isSealed"
"keys"
"preventExtensions"
"prototype"
"seal"
"setPrototypeOf"
 ))

(defvar xah-js-Object-prototype-properties nil "List of Object.prototype properties.")
(setq xah-js-Object-prototype-properties '(
"constructor"
"hasOwnProperty"
"isPrototypeOf"
"propertyIsEnumerable"
"toLocaleString"
"toString"
"valueOf"
) )

(defvar xah-js-Array-properties nil "List of Array properties.")
(setq xah-js-Array-properties '(
 ;; 2016-11-30 complete
 "from"
 "isArray"
 "length"
 "of"
 "prototype"
 ))

(defvar xah-js-Array-prototype-properties nil "List of Array.prototype properties.")
(setq xah-js-Array-prototype-properties '(
"concat"
"constructor"
"copyWithin"
"entries"
"every"
"fill"
"filter"
"find"
"findIndex"
"forEach"
"indexOf"
"join"
"keys"
"lastIndexOf"
"map"
"pop"
"push"
"reduce"
"reduceRight"
"reverse"
"shift"
"slice"
"some"
"sort"
"splice"
"toLocaleString"
"toString"
"unshift"
"values"
) )

(defvar xah-js-str-methods nil "List of JavaScript string methods.")
(setq xah-js-str-methods '(
"length"
"concat"
"trim"
"slice"
"substr"
"substring"
"indexOf"
"lastIndexOf"
"search"
"replace"
"match"
"split"
"toUpperCase"
"toLowerCase"
"toLocaleUpperCase"
"toLocaleLowerCase"
"charAt"
"charCodeAt"
"codePointAt"

"fromCharCode"
"fromCodePoint"

"toValueOf"
"localeCompare"
) )

(defvar xah-js-Math-properties nil "List of JavaScript Math properties.")
(setq xah-js-Math-properties '(
"E"
"LN10"
"LN2"
"LOG10E"
"LOG2E"
"PI"
"SQRT1_2"
"SQRT2"
"abs"
"acos"
"acosh"
"asin"
"asinh"
"atan"
"atan2"
"atanh"
"cbrt"
"ceil"
"clz32"
"cos"
"cosh"
"exp"
"expm1"
"floor"
"fround"
"hypot"
"imul"
"log"
"log10"
"log1p"
"log2"
"max"
"min"
"pow"
"random"
"round"
"sign"
"sin"
"sinh"
"sqrt"
"tan"
"tanh"
"toSource"
"trunc"
) )

(defvar xah-js-dom-words nil "List of keywords from DOM or browser.")
(setq xah-js-dom-words '(

"alert"

"write"
"Worker"
"postMessage"

"location"
"href"
"protocol"
"body"
"window"
"children"
"nodeName"
"nodeValue"
"nodeType"
"tagName"
"querySelector"
"querySelectorAll"
"createTextNode"

"addEventListener"
"removeEventListener"
"getElementById"
"getElementsByTagName"
"getElementsByClassName"
"getElementsByName"

"getAttribute"
"setAttribute"
"hasAttribute"
"removeAttribute"
"attributes"

"textContent"

"localStorage"
"localStorage.getItem"
"localStorage.setItem"
"localStorage.removeItem"
"localStorage.clear"
"localStorage.key"

"navigator.appName"
"navigator.appCodeName"
"navigator.appVersion"
"navigator.platform"
"navigator.appCodeName"
"navigator.product"
"navigator.userAgent"
"navigator.cookieEnabled"
"navigator.javaEnabled"
"navigator.mimeTypes"
"navigator.plugins"

"cookie"
"cookie.path"
"cookie.domain"
"cookie.max-age"
"cookie.expires"
"cookie.secure"

"Image"
"src"
"open"
"focus"

"classList"
"classList.add"
"classList.remove"
"classList.toggle"
"classList.contains"

"target"

"getTime"
"getFullYear"
"getYear"
"getMonth"
"getDate"
"getDay"
"getHours"
"getMinutes"
"getSeconds"
"getMilliseconds"

"getUTCMonth"
"getUTCDate"
"getUTCDay"
"getUTCHours"
"getUTCMinutes"
"getUTCSeconds"
"getUTCMilliseconds"

"setTime"
"setFullYear"
"setYear"
"setMonth"
"setDate"
"setHours"
"setMinutes"
"setSeconds"
"setMilliseconds"

"setUTCFullYear"
"setUTCMonth"
"setUTCDate"
"setUTCHours"
"setUTCMinutes"
"setUTCSeconds"
"setUTCMilliseconds"

;; Date stuff
"toLocaleString"
"toLocaleDateString"
"toLocaleTimeString"
"toDateString"
"toTimeString"
"toISOString"
"toUTCString"
"toGMTString"
"toJSON"

"setTimeout"
"setInterval"
"clearInterval"

"innerHTML"
"innerText"
"textContent"

"console"
"console.dir"
"createElement"
"createElementNS"
"createDocumentFragment"

"hasChildNodes"
"childNodes"
"removeChild"
"replaceChild"
"document"

"previousElementSibling"
"nextElementSibling"
"previousSibling"
"nextSibling"
"firstChild"
"lastChild"
"parentNode"
"parentElement"

"documentElement"
"clientWidth"
"clientHeight"

"pageXOffset"
"pageYOffset"

"innerWidth"
"innerHeight"

"appendChild"
"cloneNode"

"getBoundingClientRect"

"WebSocket"
"onmessage"
"onopen"
"onclose"
"onerror"
"send"

"close"
"data"
"attributes"
"currentScript"

) )

(defvar xah-js-dom-style-obj-words nil "List of constants")
(setq xah-js-dom-style-obj-words '(
"style.alignContent"
"style.alignItems"
"style.alignSelf"
"style.animation"
"style.animationDelay"
"style.animationDirection"
"style.animationDuration"
"style.animationFillMode"
"style.animationIterationCount"
"style.animationName"
"style.animationTimingFunction"
"style.animationPlayState"
"style.background"
"style.backgroundAttachment"
"style.backgroundColor"
"style.backgroundImage"
"style.backgroundPosition"
"style.backgroundRepeat"
"style.backgroundClip"
"style.backgroundOrigin"
"style.backgroundSize"
"style.backfaceVisibility"
"style.border"
"style.borderBottom"
"style.borderBottomColor"
"style.borderBottomLeftRadius"
"style.borderBottomRightRadius"
"style.borderBottomStyle"
"style.borderBottomWidth"
"style.borderCollapse"
"style.borderColor"
"style.borderImage"
"style.borderImageOutset"
"style.borderImageRepeat"
"style.borderImageSlice"
"style.borderImageSource"
"style.borderImageWidth"
"style.borderLeft"
"style.borderLeftColor"
"style.borderLeftStyle"
"style.borderLeftWidth"
"style.borderRadius"
"style.borderRight"
"style.borderRightColor"
"style.borderRightStyle"
"style.borderRightWidth"
"style.borderSpacing"
"style.borderStyle"
"style.borderTop"
"style.borderTopColor"
"style.borderTopLeftRadius"
"style.borderTopRightRadius"
"style.borderTopStyle"
"style.borderTopWidth"
"style.borderWidth"
"style.bottom"
"style.boxDecorationBreak"
"style.boxShadow"
"style.boxSizing"
"style.captionSide"
"style.clear"
"style.clip"
"style.color"
"style.columnCount"
"style.columnFill"
"style.columnGap"
"style.columnRule"
"style.columnRuleColor"
"style.columnRuleStyle"
"style.columnRuleWidth"
"style.columns"
"style.columnSpan"
"style.columnWidth"
"style.content"
"style.counterIncrement"
"style.counterReset"
"style.cursor"
"style.direction"
"style.display"
"style.emptyCells"
"style.flex"
"style.flexBasis"
"style.flexDirection"
"style.flexFlow"
"style.flexGrow"
"style.flexShrink"
"style.flexWrap"
"style.cssFloat"
"style.font"
"style.fontFamily"
"style.fontSize"
"style.fontStyle"
"style.fontVariant"
"style.fontWeight"
"style.fontSizeAdjust"
"style.fontStretch"
"style.hangingPunctuation"
"style.height"
"style.hyphens"
"style.icon"
"style.imageOrientation"
"style.justifyContent"
"style.left"
"style.letterSpacing"
"style.lineHeight"
"style.listStyle"
"style.listStyleImage"
"style.listStylePosition"
"style.listStyleType"
"style.margin"
"style.marginBottom"
"style.marginLeft"
"style.marginRight"
"style.marginTop"
"style.maxHeight"
"style.maxWidth"
"style.minHeight"
"style.minWidth"
"style.navDown"
"style.navIndex"
"style.navLeft"
"style.navRight"
"style.navUp"
"style.opacity"
"style.order"
"style.orphans"
"style.outline"
"style.outlineColor"
"style.outlineOffset"
"style.outlineStyle"
"style.outlineWidth"
"style.overflow"
"style.overflowX"
"style.overflowY"
"style.padding"
"style.paddingBottom"
"style.paddingLeft"
"style.paddingRight"
"style.paddingTop"
"style.pageBreakAfter"
"style.pageBreakBefore"
"style.pageBreakInside"
"style.perspective"
"style.perspectiveOrigin"
"style.position"
"style.quotes"
"style.resize"
"style.right"
"style.tableLayout"
"style.tabSize"
"style.textAlign"
"style.textAlignLast"
"style.textDecoration"
"style.textDecorationColor"
"style.textDecorationLine"
"style.textDecorationStyle"
"style.textIndent"
"style.textJustify"
"style.textOverflow"
"style.textShadow"
"style.textTransform"
"style.top"
"style.transform"
"style.transformOrigin"
"style.transformStyle"
"style.transition"
"style.transitionProperty"
"style.transitionDuration"
"style.transitionTimingFunction"
"style.transitionDelay"
"style.unicodeBidi"
"style.verticalAlign"
"style.visibility"
"style.whiteSpace"
"style.width"
"style.wordBreak"
"style.wordSpacing"
"style.wordWrap"
"style.widows"
"style.zIndex"

) )

(defvar xah-js-constants nil "List of constants")
(setq xah-js-constants '(
"NaN"
"Infinity"
"null"
"undefined"
"true"
"false"

) )

(defvar xah-js-all-js-keywords nil "List all js words.")
(setq xah-js-all-js-keywords
      (append xah-js-keyword-builtin
              xah-js-lang-words
              xah-js-Object-properties
              xah-js-Array-properties
              xah-js-Object-prototype-properties
              xah-js-Array-prototype-properties
              xah-js-str-methods
              xah-js-Math-properties
              xah-js-dom-words
              xah-js-constants
              xah-js-dom-style-obj-words
              ))


;; syntax coloring related

(setq xah-js-font-lock-keywords
      (let (
            (capVars "\\_<[A-Z][-_?0-9A-Za-z]*" ))
        `(
          ("φ[$_0-9A-Za-z]+" . 'xah-js-function-param-fc)
          ("ξ[$_0-9A-Za-z]+" . 'xah-js-user-variable-fc)
          ("ε[$_0-9A-Za-z]+" . 'xah-js-identifier-ε)
          ("ƒ[$_0-9A-Za-z]+" . 'xah-js-identifier-ƒ)
          ("γ[$_0-9A-Za-z]+" . 'xah-js-identifier-γ)
          ("\\(\\.replace\\|\\.search\\|\\.match\\)[ ]*([ ]*\\(/[^/]+/\\)" . (2 font-lock-string-face t)) ; regex
          (,(regexp-opt xah-js-constants 'symbols) . font-lock-constant-face)
          (,(regexp-opt xah-js-dom-words 'symbols) . font-lock-function-name-face)
          (,(regexp-opt xah-js-keyword-builtin 'symbols) . font-lock-keyword-face)
          (,(regexp-opt xah-js-lang-words 'symbols) . font-lock-keyword-face)
          (,(regexp-opt xah-js-Object-properties 'symbols) . font-lock-keyword-face)
          (,(regexp-opt xah-js-Array-properties 'symbols) . font-lock-keyword-face)
          (,(regexp-opt xah-js-Object-prototype-properties 'symbols) . font-lock-keyword-face)
          (,(regexp-opt xah-js-Array-prototype-properties 'symbols) . font-lock-keyword-face)
          (,(regexp-opt xah-js-str-methods 'symbols) . font-lock-keyword-face)
          (,(regexp-opt xah-js-Math-properties 'symbols) . font-lock-function-name-face)
          (,(regexp-opt xah-js-dom-style-obj-words) . font-lock-function-name-face)
          ;; font-lock-variable-name-face
          (,capVars . 'xah-js-cap-word)
          )))


;; keybinding

(defvar xah-js-mode-map nil "Keybinding for `xah-js-mode'")
(progn
  (setq xah-js-mode-map (make-sparse-keymap))

  (define-key xah-js-mode-map (kbd "<menu> e TAB") 'xah-js-complete-symbol-ido)
  (define-key xah-js-mode-map (kbd "TAB") 'xah-js-complete-or-indent)

  )


;; syntax table

(defvar xah-js-mode-syntax-table nil "Syntax table for `xah-js-mode'.")

(setq xah-js-mode-syntax-table
      (let ((synTable (make-syntax-table)))

        (modify-syntax-entry ?\! "." synTable)
        (modify-syntax-entry ?\# "." synTable)
        (modify-syntax-entry ?\$ "." synTable)
        (modify-syntax-entry ?\% "." synTable)
        (modify-syntax-entry ?\& "." synTable)
        (modify-syntax-entry ?\+ "." synTable)
        (modify-syntax-entry ?\, "." synTable)
        (modify-syntax-entry ?\- "." synTable)
        (modify-syntax-entry ?\. "." synTable)
        (modify-syntax-entry ?\: "." synTable)
        (modify-syntax-entry ?\; "." synTable)
        (modify-syntax-entry ?\< "." synTable)
        (modify-syntax-entry ?\= "." synTable)
        (modify-syntax-entry ?\> "." synTable)
        (modify-syntax-entry ?\? "." synTable)
        (modify-syntax-entry ?\@ "." synTable)
        (modify-syntax-entry ?^ "." synTable) ; can't use blackslash, because it became control
        (modify-syntax-entry ?\_ "_" synTable)
        (modify-syntax-entry ?\` "." synTable)
        (modify-syntax-entry ?\| "." synTable)
        (modify-syntax-entry ?\~ "." synTable)
        (modify-syntax-entry ?\\ "\\" synTable)

        (modify-syntax-entry ?\/ ". 124" synTable)
        (modify-syntax-entry ?* "w 23b" synTable)
        (modify-syntax-entry ?\n ">" synTable)

        (modify-syntax-entry ?\" "\"" synTable)
        (modify-syntax-entry ?\' "." synTable)

        (modify-syntax-entry ?\( "()" synTable)
        (modify-syntax-entry ?\) ")(" synTable)
        (modify-syntax-entry ?\[ "(]" synTable)
        (modify-syntax-entry ?\] ")[" synTable)
        (modify-syntax-entry ?\{ "(}" synTable)
        (modify-syntax-entry ?\} "){" synTable)

        (modify-syntax-entry '(?0 . ?9) "w" synTable)
        (modify-syntax-entry '(?A . ?Z) "w" synTable)
        (modify-syntax-entry '(?a . ?z) "w" synTable)

        synTable))


;; indent

(defun xah-js-goto-outer-bracket (&optional pos)
  "Move cursor to the beginning of left {}, starting at pos.
Version 2016-10-18"
  (interactive)
  (let (
        (-p0 (if (number-or-marker-p pos) pos (point)))
        (-p1 -p0))
    (goto-char -p1)
    (search-backward "{" nil t)
    ;; (while
    ;;     ;; (setq -p1 (point))
    ;;     )
    ))

(defun xah-js-indent-root-block ()
  "Prettify format current root sexp group.
Root sexp group is the outmost sexp unit."
  (interactive)
  (save-excursion
    (let (-p1 -p2)
      (xah-js-goto-outer-bracket)
      (setq -p1 (point))
      (setq -p2 (forward-sexp 1))
      (progn
        (goto-char -p1)
        ;; (indent-sexp)
        (message "xah-js-indent-root-block called " )
        ;; (js-indent-line)
        ;; (indent-region -p1 -p2)
        ;; (c-indent-region -p1 -p2)
        ))))

(defun xah-js-complete-or-indent ()
  "Do keyword completion or indent/prettify-format.

If char before point is letters and char after point is whitespace or punctuation, then do completion, except when in string or comment. In these cases, do `xah-js-indent-root-block'.
Version 2016-10-24"
  (interactive)
  (let ( (-syntax-state (syntax-ppss)))
    (if (or (nth 3 -syntax-state) (nth 4 -syntax-state))
        (progn
          ;; (insert "★")
          (message "tried indent")
          (js-indent-line)
          ;; (xah-js-indent-root-block)
          )
      (if
          (and (looking-back "[[:graph:]]" 1)
               ;; (or (eobp) (looking-at "[\n[:blank:][:punct:]]"))
               )
          (progn
            (xah-js-complete-symbol-ido))
        (progn
          ;; (insert "★")
          (js-indent-line)
          (message "tried indent")
          ;; (xah-js-indent-root-block)
          )))))

(defun xah-js-complete-symbol-ido ()
  "Perform keyword completion on current word.

This uses `ido-mode' user interface style for completion.
Version 2016-10-24"
  (interactive)
  (let* (
         (-bds (bounds-of-thing-at-point 'symbol))
         -p1
         -p2
         -word
         -result)
    (if (and (not (null (car -bds)))
             (not (null (cdr -bds))))
        (progn
          (setq -p1 (car -bds))
          (setq -p2 (cdr -bds))
          (setq -word (buffer-substring-no-properties -p1 -p2)))
      (progn
        (setq -p1 (point))
        (setq -p2 (point))
        (setq -word "")))
    (setq -result
          (ido-completing-read "" xah-js-all-js-keywords nil nil -word ))
    (delete-region -p1 -p2)
    (insert -result)))



;;;###autoload
(define-derived-mode xah-js-mode prog-mode "ξjs"
  "A major mode for JavaScript.

URL `http://ergoemacs.org/emacs/xah-js-mode.html'

\\{xah-js-mode-map}"

  (setq font-lock-defaults '((xah-js-font-lock-keywords)))

  (setq-local comment-start "// ")
  (setq-local comment-end "")
  (setq-local comment-column 2)

  (make-local-variable 'abbrev-expand-function)
  (if (or
       (and (>= emacs-major-version 24)
            (>= emacs-minor-version 4))
       (>= emacs-major-version 25))
      (progn
        (setq abbrev-expand-function 'xah-js-expand-abbrev))
    (progn (add-hook 'abbrev-expand-functions 'xah-js-expand-abbrev nil t)))

  (abbrev-mode 1)

  (setq-local indent-line-function 'js-indent-line)
  ;; (setq-local tab-always-indent t)
  ;; (add-hook 'completion-at-point-functions 'xah-js-complete-symbol-ido nil 'local)

  (setq indent-tabs-mode nil) ; don't mix space and tab

  :group 'xah-js-mode

  )

;; (autoload 'xah-js-mode "xah-js-mode" "load xah-js-mode for JavaScript file" t)
(add-to-list 'auto-mode-alist '("\\.js\\'" . xah-js-mode))

(provide 'xah-js-mode)

;; Local Variables:
;; coding: utf-8
;; End:

;;; xah-js-mode.el ends here
