;;; xah-js-mode.el --- Major mode for editing JavaScript. -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright © 2013-2017 by Xah Lee

;; Author: Xah Lee ( http://xahlee.info/ )
;; Version: 1.8.20180227
;; Created: 23 March 2013
;; Package-Requires: ((emacs "24.1"))
;; Keywords: languages, JavaScript
;; License: GPL v3
;; URL: http://ergoemacs.org/emacs/xah-js-mode.html

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Major mode for editing JavaScript code.

;; alpha stage. used by me. Works fine for me. lots to do yet.

;;; HISTORY

;; version 0.1, 2013-08-21 first version

;; TODO
;; 2018-02-18 need to add indentation


;;; Code:

(require 'js) ; in emacs. temp, borrow js-indent-line, and js-syntax-propertize
(provide 'thingatpt)  ; in emacs
(require 'newcomment) ; in emacs
(require 'ido)        ; in emacs
;; (require 'lisp-mode) ; in emacs. for indent-sexp. temp hack. todo


(defvar xah-js-mode-hook nil "Standard hook for `xah-js-mode'")


(defvar xah-js-keyword-builtin nil "List of js language words, such as: if else for in case new break.")
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

"EvalError"
"RangeError"
"ReferenceError"
"SyntaxError"
"TypeError"
"URIError"

"__proto__"
"abstract"
"arguments"
"boolean"
"byte"
"char"
"class"
"configurable"
"const"
"decodeURI"
"decodeURIComponent"
"double"
"encodeURI"
"encodeURIComponent"
"enum"
"enumerable"
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
"iterator"
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
"value" ; from property descriptor
"volatile"
"writable"
"yield"

) )

(defvar xah-js-big-obj-names nil "List of standard object names. e.g. Object, Array Math, Date, JSON, etc.")
(setq
 xah-js-big-obj-names
 '(
   "Array"
   "Boolean"
   "Date"
   "Error"
   "Function"
   "JSON"
   "Map"
   "Math"
   "Number"
   "Object"
   "Promise"
   "Proxy"
   "Reflect"
   "RegExp"
   "Set"
   "String"
   "Symbol"
   "WeakMap"
   "WeakSet"
   ))

(defvar xah-js-Object-props nil "List of Object properties.")
(setq xah-js-Object-props '( "assign" "create" "defineProperties" "defineProperty" "freeze" "getOwnPropertyDescriptor" "getOwnPropertyNames" "getOwnPropertySymbols" "getPrototypeOf" "is" "isExtensible" "isFrozen" "isSealed" "keys" "preventExtensions" "prototype" "seal" "setPrototypeOf" ))

(defvar xah-js-Object-props-fullword nil "List of Object properties full words, e.g. \"Object.create\".")
(setq xah-js-Object-props-fullword (mapcar (lambda (x)  (concat "Object." x)) xah-js-Object-props))

(defvar xah-js-Reflect-props nil "List of Reflect properties.")
(setq xah-js-Reflect-props '( "apply" "construct" "defineProperty" "deleteProperty" "get" "getOwnPropertyDescriptor" "getPrototypeOf" "has" "isExtensible" "ownKeys" "preventExtensions" "set" "setPrototypeOf" ) )

(defvar xah-js-Reflect-props-fullword nil "List of Reflect properties full words, e.g. \"Reflect.apply\".")
(setq xah-js-Reflect-props-fullword (mapcar (lambda (x)  (concat "Reflect." x)) xah-js-Reflect-props))

(defvar xah-js-Set-props nil "List of Set properties.")
(setq xah-js-Set-props '( "length" ) )

(defvar xah-js-Set-props-fullword nil "List of Set properties full words, e.g. \"Set.length\".")
(setq xah-js-Set-props-fullword (mapcar (lambda (x)  (concat "Set." x)) xah-js-Set-props))

(defvar xah-js-Map-props nil "List of Map properties.")
(setq xah-js-Map-props '( "prototype" ))

(defvar xah-js-Map-props-fullword nil "List of Map properties full words, e.g. \"Map.prototype\".")
(setq xah-js-Map-props-fullword (mapcar (lambda (x)  (concat "Map." x)) xah-js-Map-props))

(defvar xah-js-Array-props nil "List of Array properties.")
(setq xah-js-Array-props '( "from" "isArray" "length" "of" "prototype" ))

(defvar xah-js-Array-props-fullword nil "List of Array properties full words, e.g. \"Array.prototype\".")
(setq xah-js-Array-props-fullword (mapcar (lambda (x)  (concat "Array." x)) xah-js-Array-props))

(defvar xah-js-Function-props nil "List of Function properties.")
(setq xah-js-Function-props '( "length" "prototype" ))

(defvar xah-js-Function-props-fullword nil "List of Function properties full words, e.g. \"Function.create\".")
(setq xah-js-Function-props-fullword (mapcar (lambda (x)  (concat "Function." x)) xah-js-Function-props))

(defvar xah-js-RegExp-props nil "List of RegExp properties.")
(setq xah-js-RegExp-props '( "prototype" ))

(defvar xah-js-RegExp-props-fullword nil "List of RegExp properties full words, e.g. \"RegExp.prototype\".")
(setq xah-js-RegExp-props-fullword (mapcar (lambda (x)  (concat "RegExp." x)) xah-js-RegExp-props))

(defvar xah-js-String-props nil "List of String properties.")
(setq xah-js-String-props '( "prototype" "fromCodePoint" "fromCharCode" "raw" ))

(defvar xah-js-String-props-fullword nil "List of String properties full words, e.g. \"String.fromCodePoint\".")
(setq xah-js-String-props-fullword (mapcar (lambda (x)  (concat "String." x)) xah-js-String-props))

(defvar xah-js-Number-props nil "List of Number properties.")
(setq xah-js-Number-props '(
    "EPSILON"
    "MAX_SAFE_INTEGER"
    "MAX_VALUE"
    "MIN_SAFE_INTEGER"
    "MIN_VALUE"
    "NEGATIVE_INFINITY"
    ;; "NaN"
    "POSITIVE_INFINITY"
    "isFinite"
    "isInteger"
    "isNaN"
    "isSafeInteger"
    "parseFloat"
    "parseInt"
    "prototype"
))

(defvar xah-js-Number-props-fullword nil "List of Number properties full words, e.g. \"Number.parseIntPoint\".")
(setq xah-js-Number-props-fullword (mapcar (lambda (x)  (concat "Number." x)) xah-js-Number-props))

(defvar xah-js-Promise-props nil "List of Promise constructor properties.")
(setq xah-js-Promise-props '( "all" "prototype" "race" "reject" "resolve" ))

(defvar xah-js-Promise-props-fullword nil "List of Promise constructor properties full words, e.g. \"Promise.reject\".")
(setq xah-js-Promise-props-fullword (mapcar (lambda (x)  (concat "Promise." x)) xah-js-Promise-props))

(defvar xah-js-JSON-props nil "List of Array properties.")
(setq xah-js-JSON-props '( "stringify" "parse" ))

(defvar xah-js-JSON-props-fullword nil "List of JSON properties full words, e.g. \"JSON.parse\".")
(setq xah-js-JSON-props-fullword (mapcar (lambda (x)  (concat "JSON." x)) xah-js-JSON-props))

(defvar xah-js-Math-props nil "List of JavaScript Math properties.")
(setq xah-js-Math-props '( "E" "LN10" "LN2" "LOG10E" "LOG2E" "PI" "SQRT1_2" "SQRT2" "abs" "acos" "acosh" "asin" "asinh" "atan" "atan2" "atanh" "cbrt" "ceil" "clz32" "cos" "cosh" "exp" "expm1" "floor" "fround" "hypot" "imul" "log" "log10" "log1p" "log2" "max" "min" "pow" "random" "round" "sign" "sin" "sinh" "sqrt" "tan" "tanh" "toSource" "trunc" ) )

(defvar xah-js-Math-props-fullword nil "List of Math properties full words, e.g. \"Math.parse\".")
(setq xah-js-Math-props-fullword (mapcar (lambda (x)  (concat "Math." x)) xah-js-Math-props))

(defvar xah-js-Symbol-props nil "List of JavaScript Symbol properties.")
(setq
 xah-js-Symbol-props
 '(

   "for"
   "hasInstance"
   "isConcatSpreadable"
   "iterator"
   "keyFor"
   "match"
   "prototype"
   "replace"
   "search"
   "species"
   "split"
   "toPrimitive"
   "toStringTag"
   "unscopables"

   ))

(defvar xah-js-Symbol-props-fullword nil "List of Symbol properties full words, e.g. \"Symbol.for\".")
(setq xah-js-Symbol-props-fullword (mapcar (lambda (x)  (concat "Symbol." x)) xah-js-Symbol-props))

(defvar xah-js-Date-props nil "List of Date properties.")
(setq xah-js-Date-props '( "now" "parse" "UTC" "prototype" ) )

(defvar xah-js-Date-props-fullword nil "List of Date properties full words, e.g. \"Date.parse\".")
(setq xah-js-Date-props-fullword (mapcar (lambda (x)  (concat "Date." x)) xah-js-Date-props))

(defvar xah-js-Object-proto-props nil "List of Object.prototype properties.")
(setq xah-js-Object-proto-props '(
"constructor"
"hasOwnProperty"
"isPrototypeOf"
"propertyIsEnumerable"
"toLocaleString"
"toString"
"valueOf"
) )

(defvar xah-js-Function-proto-props nil "List of Function.prototype properties.")
(setq xah-js-Function-proto-props '(
"apply"
"bind"
"call"
"toString"
"constructor"
) )

(defvar xah-js-Promise-proto-props nil "List of Promise.prototype properties.")
(setq xah-js-Promise-proto-props '( "constructor" "catch" "then" ) )

(defvar xah-js-Symbol-proto-props nil "List of Symbol.prototype properties.")
(setq
 xah-js-Symbol-proto-props
 '(
   "constructor"
   "toString"
   "valueOf"
   ))

(defvar xah-js-Array-proto-props nil "List of Array.prototype properties.")
(setq xah-js-Array-proto-props '( "concat" "constructor" "copyWithin" "entries" "every" "fill" "filter" "find" "findIndex" "forEach" "indexOf" "join" "keys" "lastIndexOf" "map" "pop" "push" "reduce" "reduceRight" "reverse" "shift" "slice" "some" "sort" "splice" "toLocaleString" "toString" "unshift" "values" ) )

(defvar xah-js-Set-proto-props nil "List of Set.prototype properties.")
(setq xah-js-Set-proto-props '( "add" "clear" "constructor" "delete" "entries" "forEach" "has" "keys" "size" "values" ) )

(defvar xah-js-Map-proto-props nil "List of Map.prototype properties.")
(setq xah-js-Map-proto-props '( "clear" "constructor" "delete" "entries" "forEach" "get" "has" "keys" "set" "size" "values"))

(defvar xah-js-String-proto-props nil "List of JavaScript String.prototype properties.")
(setq
 xah-js-String-proto-props
 '( "charAt" "charCodeAt" "codePointAt" "concat" "endsWith" "includes" "indexOf" "lastIndexOf" "length" "localeCompare" "localeCompare" "match" "normalize" "repeat" "replace" "search" "slice" "split" "startsWith" "substr" "substring" "toLocaleLowerCase" "toLocaleUpperCase" "toLowerCase" "toString" "toUpperCase" "toValueOf" "trim" ))

(defvar xah-js-Number-proto-props nil "List of JavaScript Number.prototype properties.")
(setq
 xah-js-Number-proto-props
 '(

"constructor"
"toFixed"
"toPrecision"
"toExponential"
"valueOf"
"toString"
"toLocaleString"

   ))

(defvar xah-js-RegExp-proto-props nil "List of JavaScript RegExp.prototype methods.")
(setq
 xah-js-RegExp-proto-props
 '(
   "test"
   "exec"
   "source"
   "global"
   "ignoreCase"
   "multiline"
   "lastIndex"
   "flags"
   "sticky"
   "unicode"
   ))

(defvar xah-js-Date-proto-props nil "List of Date.prototype properties.")
(setq xah-js-Date-proto-props '(
"constructor"
"getDate"
"getDay"
"getFullYear"
"getHours"
"getMilliseconds"
"getMinutes"
"getMonth"
"getSeconds"
"getTime"
"getTimezoneOffset"
"getUTCDate"
"getUTCDay"
"getUTCFullYear"
"getUTCHours"
"getUTCMilliseconds"
"getUTCMinutes"
"getUTCMonth"
"getUTCSeconds"
"setDate"
"setFullYear"
"setHours"
"setMilliseconds"
"setMinutes"
"setMonth"
"setSeconds"
"setTime"
"setUTCDate"
"setUTCFullYear"
"setUTCHours"
"setUTCMilliseconds"
"setUTCMinutes"
"setUTCMonth"
"setUTCSeconds"
"toDateString"
"toISOString"
"toJSON"
"toLocaleDateString"
"toLocaleString"
"toLocaleTimeString"
"toString"
"toTimeString"
"toUTCString"
"valueOf"
"[Symbol.toPrimitive]"
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

"setTimeout"
"setInterval"
"clearTimeout"
"clearInterval"

"innerHTML"
"innerText"
"textContent"

;; hack. todo
"insertBefore"
"insertAdjacentElement"

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
"screen.height"
"screen.width"
"offsetWidth"
"offsetHeight"

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

"checked"

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

(defvar xah-js-fullwords nil "List all js fully qualified static function names. e.g. Object.create, Object.getPrototypeOf, Array.isArray, Math.abs.")
(setq xah-js-fullwords
      (append
       xah-js-Object-props-fullword
       xah-js-Reflect-props-fullword
       xah-js-Set-props-fullword
       xah-js-Map-props-fullword
       xah-js-Array-props-fullword
       xah-js-Function-props-fullword
       xah-js-RegExp-props-fullword
       xah-js-String-props-fullword
       xah-js-Number-props-fullword
       xah-js-Promise-props-fullword
       xah-js-JSON-props-fullword
       xah-js-Math-props-fullword
       xah-js-Symbol-props-fullword
       xah-js-Date-props-fullword
       ))

(defvar xah-js-all-js-keywords nil "List all js words.")
(setq xah-js-all-js-keywords
      (append
       xah-js-fullwords
       xah-js-keyword-builtin
       xah-js-lang-words
       xah-js-big-obj-names
       xah-js-Object-proto-props
       xah-js-Array-proto-props
       xah-js-Set-proto-props
       xah-js-Map-proto-props
       xah-js-Function-proto-props
       xah-js-Symbol-proto-props
       xah-js-String-proto-props
       xah-js-Number-proto-props
       xah-js-RegExp-proto-props
       xah-js-Date-proto-props
       xah-js-dom-words
       xah-js-constants
       xah-js-dom-style-obj-words
       ))


;; syntax coloring related

(defface xah-js-dollar-name
  '((t :foreground "purple" :weight bold ))
  "face for user variables."
  :group 'xah-js-mode )

(face-spec-set
 'xah-js-dollar-name
 '((t :foreground "purple" :weight bold )))

(defface xah-js-identifier-caps
  '((t :foreground "firebrick" :weight bold))
  "Face for capitalized word."
  :group 'xah-js-mode )

(face-spec-set
 'xah-js-identifier-caps
 '((t :foreground "firebrick" :weight bold)))

(defface xah-js-func-param
  '((t :foreground "red" :weight bold))
  "face for function parameters."
  :group 'xah-js-mode )

(face-spec-set
 'xah-js-func-param
 '((t :foreground "red" :weight bold)))

(defface xah-js-greek-phi-φ
  '((t :foreground "red" :weight bold))
  "face for function parameters."
  :group 'xah-js-mode )

(face-spec-set
 'xah-js-greek-phi-φ
 '((t :foreground "red" :weight bold)))

(defface xah-js-user-var
  '((t :foreground "DarkGreen" :weight bold ))
  "face for user variables."
  :group 'xah-js-mode )

(face-spec-set
 'xah-js-user-var
 '((t :foreground "DarkGreen" :weight bold )))

(defface xah-js-greek-xi-ξ
  '((t :foreground "DarkGreen" :weight bold ))
  "face for user variables."
  :group 'xah-js-mode )

(face-spec-set
 'xah-js-greek-xi-ξ
 '((t :foreground "DarkGreen" :weight bold )))

(defface xah-js-func-name
  '((t :foreground "blue" :weight bold ))
  "face for user variables."
  :group 'xah-js-mode )

(face-spec-set
 'xah-js-func-name
 '((t :foreground "blue" :weight bold )))

(defface xah-js-name-global
  '((t :foreground "#104e8b" :weight bold ))
  "face for user variables."
  :group 'xah-js-mode )

(face-spec-set
 'xah-js-name-global
 '((t :foreground "#104e8b" :weight bold )))

(defvar xah-js-font-lock-keywords nil "gist for `font-lock-defaults'")
(setq xah-js-font-lock-keywords
      (let (
            (capVars "\\_<[A-Z][-_?0-9A-Za-z]*" ))
        `(
          ("\\(\\.replace\\|\\.search\\|\\.match\\)[ ]*([ ]*\\(/[^/]+/\\)" . (2 font-lock-string-face t)) ; regex

          (,(regexp-opt xah-js-Object-props-fullword 'symbols) . font-lock-keyword-face)
          (,(regexp-opt xah-js-Reflect-props-fullword 'symbols) . font-lock-keyword-face)
          (,(regexp-opt xah-js-Array-props-fullword 'symbols) . font-lock-keyword-face)
          (,(regexp-opt xah-js-Function-props-fullword 'symbols) . font-lock-keyword-face)
          (,(regexp-opt xah-js-RegExp-props-fullword 'symbols) . font-lock-keyword-face)
          (,(regexp-opt xah-js-String-props-fullword 'symbols) . font-lock-keyword-face)
          (,(regexp-opt xah-js-Number-props-fullword 'symbols) . font-lock-keyword-face)
          (,(regexp-opt xah-js-Promise-props-fullword 'symbols) . font-lock-keyword-face)
          (,(regexp-opt xah-js-JSON-props-fullword 'symbols) . font-lock-keyword-face)
          (,(regexp-opt xah-js-Math-props-fullword 'symbols) . font-lock-function-name-face)
          (,(regexp-opt xah-js-Symbol-props-fullword 'symbols) . font-lock-function-name-face)
          (,(regexp-opt xah-js-Date-props-fullword 'symbols) . font-lock-function-name-face)
          (,(regexp-opt xah-js-Set-props-fullword 'symbols) . font-lock-keyword-face)
          (,(regexp-opt xah-js-Map-props-fullword 'symbols) . font-lock-keyword-face)
          (,(regexp-opt xah-js-big-obj-names 'symbols) . font-lock-keyword-face)

          (,(regexp-opt xah-js-Object-props 'symbols) . font-lock-keyword-face)
          (,(regexp-opt xah-js-Reflect-props 'symbols) . font-lock-keyword-face)
          (,(regexp-opt xah-js-Array-props 'symbols) . font-lock-keyword-face)
          (,(regexp-opt xah-js-Function-props 'symbols) . font-lock-keyword-face)
          (,(regexp-opt xah-js-RegExp-props 'symbols) . font-lock-keyword-face)
          (,(regexp-opt xah-js-String-props 'symbols) . font-lock-keyword-face)
          (,(regexp-opt xah-js-Number-props 'symbols) . font-lock-keyword-face)
          (,(regexp-opt xah-js-Promise-props 'symbols) . font-lock-keyword-face)
          (,(regexp-opt xah-js-JSON-props 'symbols) . font-lock-keyword-face)
          (,(regexp-opt xah-js-Math-props 'symbols) . font-lock-function-name-face)
          (,(regexp-opt xah-js-Symbol-props 'symbols) . font-lock-function-name-face)
          (,(regexp-opt xah-js-Date-props 'symbols) . font-lock-function-name-face)
          (,(regexp-opt xah-js-Set-props 'symbols) . font-lock-keyword-face)

          (,(regexp-opt xah-js-Object-proto-props 'symbols) . font-lock-keyword-face)
          (,(regexp-opt xah-js-Array-proto-props 'symbols) . font-lock-keyword-face)
          (,(regexp-opt xah-js-Function-proto-props 'symbols) . font-lock-keyword-face)
          (,(regexp-opt xah-js-Promise-proto-props 'symbols) . font-lock-keyword-face)
          (,(regexp-opt xah-js-Symbol-proto-props 'symbols) . font-lock-keyword-face)
          (,(regexp-opt xah-js-String-proto-props 'symbols) . font-lock-keyword-face)
          (,(regexp-opt xah-js-Number-proto-props 'symbols) . font-lock-keyword-face)
          (,(regexp-opt xah-js-RegExp-proto-props 'symbols) . font-lock-keyword-face)
          (,(regexp-opt xah-js-Date-proto-props 'symbols) . font-lock-keyword-face)
          (,(regexp-opt xah-js-dom-style-obj-words 'symbols) . font-lock-function-name-face)
          (,(regexp-opt xah-js-Set-proto-props 'symbols) . font-lock-keyword-face)
          (,(regexp-opt xah-js-Map-proto-props 'symbols) . font-lock-keyword-face)
          (,(regexp-opt xah-js-keyword-builtin 'symbols) . font-lock-keyword-face)

          (,(regexp-opt xah-js-constants  'symbols) . font-lock-constant-face)
          (,(regexp-opt xah-js-dom-words  'symbols) . font-lock-function-name-face)
          (,(regexp-opt xah-js-lang-words  'symbols) . font-lock-keyword-face)

          ;; font-lock-variable-name-face
          ("\\_<$[$_0-9A-Za-z]+" . 'xah-js-dollar-name)
          ("\\_<f_[$_0-9A-Za-z]+" . 'xah-js-func-name)
          ("\\_<p_[$_0-9A-Za-z]+" . 'xah-js-func-param)
          ("\\_<φ[$_0-9A-Za-z]+" . 'xah-js-greek-phi-φ)
          ("\\_<v_[$_0-9A-Za-z]+" . 'xah-js-user-var)
          ("\\_<g_[$_0-9A-Za-z]+" . 'xah-js-name-global)
          ("\\_<ξ[$_0-9A-Za-z]+" . 'xah-js-greek-xi-ξ)
          (,capVars . 'xah-js-identifier-caps))
        ;;
        ))


;; keybinding

(defvar xah-js-mode-map nil "Keybinding for `xah-js-mode'")
(progn
  (setq xah-js-mode-map (make-sparse-keymap))

  (define-key xah-js-mode-map (kbd "<menu> e TAB") 'xah-js-complete-symbol-ido)
  (define-key xah-js-mode-map (kbd "TAB") 'xah-js-complete-or-indent)
  (define-key xah-js-mode-map (kbd "<C-return>") 'xah-js-insert-semicolon)
  (define-key xah-js-mode-map (kbd "RET") 'xah-js-smart-newline))


;; syntax table

(defvar xah-js-mode-syntax-table nil "Syntax table for `xah-js-mode'.")

(setq xah-js-mode-syntax-table
      (let ((synTable (make-syntax-table)))

        (modify-syntax-entry '(?0 . ?9) "w" synTable)
        (modify-syntax-entry '(?A . ?Z) "w" synTable)
        (modify-syntax-entry '(?a . ?z) "w" synTable)

        (modify-syntax-entry ?\_ "_" synTable)
        (modify-syntax-entry ?\$ "_" synTable)

        (modify-syntax-entry ?\\ "\\" synTable)
        (modify-syntax-entry ?\" "\"" synTable)
        (modify-syntax-entry ?\' "\"" synTable)

        (modify-syntax-entry ?\( "()" synTable)
        (modify-syntax-entry ?\) ")(" synTable)
        (modify-syntax-entry ?\[ "(]" synTable)
        (modify-syntax-entry ?\] ")[" synTable)
        (modify-syntax-entry ?\{ "(}" synTable)
        (modify-syntax-entry ?\} "){" synTable)

        (modify-syntax-entry ?\. "." synTable)
        (modify-syntax-entry ?\! "." synTable)
        (modify-syntax-entry ?\# "." synTable)
        (modify-syntax-entry ?\% "." synTable)
        (modify-syntax-entry ?\& "." synTable)
        (modify-syntax-entry ?\+ "." synTable)
        (modify-syntax-entry ?\, "." synTable)
        (modify-syntax-entry ?\- "." synTable)
        (modify-syntax-entry ?\: "." synTable)
        (modify-syntax-entry ?\; "." synTable)
        (modify-syntax-entry ?\< "." synTable)
        (modify-syntax-entry ?\= "." synTable)
        (modify-syntax-entry ?\> "." synTable)
        (modify-syntax-entry ?\? "." synTable)
        (modify-syntax-entry ?\@ "." synTable)
        (modify-syntax-entry ?^ "." synTable) ; can't use blackslash, because it became control
        (modify-syntax-entry ?\` "." synTable)
        (modify-syntax-entry ?\| "." synTable)
        (modify-syntax-entry ?\~ "." synTable)

        (modify-syntax-entry ?\/ "./124" synTable)
        (modify-syntax-entry ?* "w 23b" synTable)
        (modify-syntax-entry ?\n ">" synTable)

        synTable))


;; indent

(defun xah-js-goto-outer-bracket (&optional pos)
  "Move cursor to the beginning of left {}, starting at pos.
Version 2016-10-18"
  (interactive)
  (let* (
        ($p0 (if (number-or-marker-p pos) pos (point)))
        ($p1 $p0))
    (goto-char $p1)
    (search-backward "{" nil t)
    ;; (while
    ;;     ;; (setq $p1 (point))
    ;;     )
    ))

(defun xah-js-indent-root-block ()
  "Prettify format current root sexp group.
Root sexp group is the outmost sexp unit."
  (interactive)
  (save-excursion
    (let ($p1 )
      (xah-js-goto-outer-bracket)
      (setq $p1 (point))
      (forward-sexp 1)
      (progn
        (goto-char $p1)
        ;; (indent-sexp)
        (message "xah-js-indent-root-block called " )
        ;; (js-indent-line)
        ;; (indent-region $p1 $p2)
        ;; (c-indent-region $p1 $p2)
        ))))

(defun xah-js-complete-or-indent ()
  "Do keyword completion or indent/prettify-format.

If char before point is letters and char after point is whitespace or punctuation, then do completion, except when in string or comment. In these cases, do `xah-js-indent-root-block'.
Version 2016-10-24"
  (interactive)
  (let ( ($syntax-state (syntax-ppss)))
    (if (or (nth 3 $syntax-state) (nth 4 $syntax-state))
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

(defun xah-js--get-bounds-of-glyph ( )
  "Return the boundary of chars under cursor.
Return a cons cell (START . END).
Version 2016-12-09"
  (let (p1 p2)
    (save-excursion
      (progn
           (skip-chars-backward "[:graph:]")
           (setq p1 (point))
           (skip-chars-forward "[:graph:]")
           (setq p2 (point))))
    (cons p1 p2 )))

(defun xah-js-complete-symbol-ido ()
  "Perform keyword completion on current word.

This uses `ido-mode' user interface style for completion.
Version 2017-01-27"
  (interactive)
  (let* (
         ($bds (xah-js--get-bounds-of-glyph))
         $p1
         $p2
         $input
         $inputPrefixStr
         ($matchedIndex nil)
         ($isObjectName-p nil)
         $result)
    (if (or (not (car $bds)) (not (cdr $bds)))
        (progn
          (setq $p1 (point) $p2 (point))
          (setq $input ""))
      (progn
        (setq $p1 (car $bds) $p2 (cdr $bds))
        (setq $input (buffer-substring-no-properties $p1 $p2))))

    ;; 2016-12-10
    ;; if the input word contains at least one dot,
    ;; then, if it match at the beginning to one of the big object prefix such as Object, Array, Date
    ;; then, do completion with the pool of fullwords (fully qualified words eg Object.create)
    ;; else, set input word to not contain the string after the last dot, do completion with that

    (setq $matchedIndex (string-match "\\." $input))
    (when $matchedIndex
      (progn
        (setq $inputPrefixStr (substring $input 0 $matchedIndex))
        (setq $isObjectName-p
              (catch 'TAG
                (dolist ( $objName xah-js-big-obj-names nil) (when (equal $objName $inputPrefixStr) (throw 'TAG $objName)))))))
    (if $isObjectName-p
        (progn ; input start with Object or Array etc.
          (setq $result (ido-completing-read "" xah-js-fullwords nil nil $input ))
          (delete-region $p1 $p2)
          (insert $result))
      (progn ; input does not start with Object or Array etc.
        (let* (($bounds (bounds-of-thing-at-point 'symbol))
               ($p3 (car $bounds))
               ($p4 (cdr $bounds))
               ($input2 (buffer-substring-no-properties $p3 $p4 )))
          (setq $result (ido-completing-read "" xah-js-all-js-keywords nil nil $input2 ))
          (delete-region $p3 $p4)
          (insert $result))))))



(defun xah-js-insert-semicolon ()
  "insert a semicolon and return"
  (interactive)
  (insert ";\n"))

(defun xah-js-smart-newline ()
  "Insert a newline, maybe add a semicolon before.
Version 2018-02-27"
  (interactive)
  (backward-char )
  (if
      (looking-at ")\n\\|]\n\\|}\n\\|`\n\\|'\n")
      (progn
        (forward-char )
        (insert ";")
        (newline))
    (progn
      (forward-char )
      (newline))))


;; abbrev

(defun xah-js-abbrev-enable-function ()
  "Return t if not in string or comment. Else nil.
This is for abbrev table property `:enable-function'.
Version 2017-02-05"
  (let (($syntax-state (syntax-ppss)))
    (if (or (nth 3 $syntax-state) (nth 4 $syntax-state))
        nil
      t)))

;; (if (or (looking-at " \\|\n\\|\t") (eobp)) t nil)

(defun xah-js-expand-abbrev ()
  "Expand the symbol before cursor,
if cursor is not in string or comment.

Note: emacs tries to expand abbrev on every insertion of chars that's not word or symbol syntax. So, when return is pressed, normally it also tries to expand, and this function will be called if `abbrev-expand-functions' is set to it.
Returns the abbrev symbol if there's a expansion, else nil.
Version 2017-02-05"
  (interactive)
  (when (xah-js-abbrev-enable-function)
    (let (
          ($p0 (point))
          $p1 $p2
          $inputStr
          $abrSymbol
          )
      (progn
        ;; first try expansion with char sequence including the dot
        (skip-chars-backward "[._0-9A-Za-z]+" (min (- (point) 100) (point-min)))
        (setq $p1 (point) $p2 $p0)
        (goto-char $p0))
      (setq $inputStr (buffer-substring-no-properties $p1 $p2))
      ;; (message "first $inputStr is %s" $inputStr)
      (setq $abrSymbol (abbrev-symbol $inputStr))
      (if $abrSymbol
          (progn
            (abbrev-insert $abrSymbol $inputStr $p1 $p2 )
            (xah-js--abbrev-position-cursor $p1)
            $abrSymbol)
        (progn
          ;; if expansion with char sequence including the dot come out no result, try without the dot
          (progn
            (goto-char $p0)
            (skip-chars-backward "[_0-9A-Za-z]+" (min (- (point) 100) (point-min)))
            (setq $p1 (point) $p2 $p0)
            (goto-char $p0))
          (setq $inputStr (buffer-substring-no-properties $p1 $p2))
          ;; (message "2nd $inputStr is %s" $inputStr)
          (setq $abrSymbol (abbrev-symbol $inputStr))
          (if $abrSymbol
              (progn
                (abbrev-insert $abrSymbol $inputStr $p1 $p2 )
                (xah-js--abbrev-position-cursor $p1)
                $abrSymbol)
            nil
            ))))))

(defun xah-js--abbrev-position-cursor (&optional @pos)
  "Move cursor back to ▮ if exist, else put at end.
Return true if found, else false.
Version 2016-10-24"
  (interactive)
  (let (($found-p (search-backward "▮" (if @pos @pos (max (point-min) (- (point) 100))) t )))
    (when $found-p (delete-char 1) )
    $found-p
    ))

(defun xah-js--abbrev-hook-f ()
  "Abbrev hook function, used for `define-abbrev'.
 Our use is to prevent inserting the char that triggered expansion. Experimental.
 the “ahf” stand for abbrev hook function.
Version 2016-10-24"
  t)

(put 'xah-js--abbrev-hook-f 'no-self-insert t)

(setq xah-js-mode-abbrev-table nil)
;; (define-abbrev-table 'xah-js-mode-abbrev-table '( ("w" "window." ) ) "abbrev table for `xah-js-mode'" )
;; (clear-abbrev-table xah-js-mode-abbrev-table)
(define-abbrev-table 'xah-js-mode-abbrev-table
  '(

    ;; Object
    ("Object.create" "Object.create ( Object.prototype▮, {\n    'p1': { value : 3, writable: true, enumerable: true, configurable: true },\n    'p2': { value : 3, writable: true, enumerable: false, configurable: true }})" xah-js--abbrev-hook-f)
    ("Object.assign" "Object.assign ( target▮, source1, source2, etc )" xah-js--abbrev-hook-f)
    ("Object.getPrototypeOf" "Object.getPrototypeOf ( ▮ )" xah-js--abbrev-hook-f)
    ("Object.setPrototypeOf" "Object.setPrototypeOf ( ▮, proto )" xah-js--abbrev-hook-f)
    ("Object.getOwnPropertyDescriptor" "Object.getOwnPropertyDescriptor ( ▮, P )" xah-js--abbrev-hook-f)
    ("Object.defineProperty" "Object.defineProperty ( ▮, pname, { value : 3, writable: true, enumerable: false, configurable: true, 〔set: function, get: function〕 })" xah-js--abbrev-hook-f)
    ("Object.defineProperties" "Object.defineProperties ( ▮, {pname, { value : 3, writable: true, enumerable: false, configurable: true, 〔set: function, get: function〕 }}, etc)" xah-js--abbrev-hook-f)
    ("Object.getOwnPropertyNames" "Object.getOwnPropertyNames ( ▮ )" xah-js--abbrev-hook-f)
    ("Object.getOwnPropertySymbols" "Object.getOwnPropertySymbols ( ▮ )" xah-js--abbrev-hook-f)
    ("Object.keys" "Object.keys ( ▮ )" xah-js--abbrev-hook-f)
    ("Object.is" "Object.is ( value1▮, value2 )" xah-js--abbrev-hook-f)
    ("Object.isExtensible" "Object.isExtensible ( ▮ )" xah-js--abbrev-hook-f)
    ("Object.isFrozen" "Object.isFrozen ( ▮ )" xah-js--abbrev-hook-f)
    ("Object.isSealed" "Object.isSealed ( ▮ )" xah-js--abbrev-hook-f)

    ("oc" "Object.create ( Object.prototype▮, {\n    'p1': { value : 3, writable: true, enumerable: true, configurable: true },\n    'p2': { value : 3, writable: true, enumerable: false, configurable: true }})" xah-js--abbrev-hook-f)
    ("oa" "Object.assign ( target▮, source1, source2, etc )" xah-js--abbrev-hook-f)
    ("ogpo" "Object.getPrototypeOf ( ▮ )" xah-js--abbrev-hook-f)
    ("ospo" "Object.setPrototypeOf ( ▮, proto )" xah-js--abbrev-hook-f)
    ("ogopd" "Object.getOwnPropertyDescriptor ( ▮, P )" xah-js--abbrev-hook-f)
    ("odp" "Object.defineProperty ( ▮, pname, { value : 3, writable: true, enumerable: false, configurable: true, 〔set: function, get: function〕 })" xah-js--abbrev-hook-f)
    ("odps" "Object.defineProperties ( ▮, {pname, { value : 3, writable: true, enumerable: false, configurable: true, set: function, get: function }}, etc)" xah-js--abbrev-hook-f)
    ("ogopn" "Object.getOwnPropertyNames ( ▮ )" xah-js--abbrev-hook-f)
    ("ogops" "Object.getOwnPropertySymbols ( ▮ )" xah-js--abbrev-hook-f)
    ("ok" "Object.keys ( ▮ )" xah-js--abbrev-hook-f)
    ("oi" "Object.is ( value1▮, value2 )" xah-js--abbrev-hook-f)
    ("oie" "Object.isExtensible ( ▮ )" xah-js--abbrev-hook-f)
    ("oif" "Object.isFrozen ( ▮ )" xah-js--abbrev-hook-f)
    ("ois" "Object.isSealed ( ▮ )" xah-js--abbrev-hook-f)
    ("ope" "Object.preventExtensions ( ▮ )" xah-js--abbrev-hook-f)
    ("of" "Object.freeze ( ▮ )" xah-js--abbrev-hook-f)
    ("os" "Object.seal ( ▮ )" xah-js--abbrev-hook-f)
    ("op" "Object.prototype" xah-js--abbrev-hook-f)

    ;; Array
    ("Array.isArray" "Array.isArray ( ▮ )" xah-js--abbrev-hook-f)
    ("aia" "Array.isArray ( ▮ )" xah-js--abbrev-hook-f)
    ("Array.from" "Array.from ( items▮ , ?mapfn, ?this )" xah-js--abbrev-hook-f)

    ("Array.of" "Array.of ( e1▮ , e2, etc )" xah-js--abbrev-hook-f)
    ("ao" "Array.of ( e1▮ , e2, etc )" xah-js--abbrev-hook-f)

    ("ap" "Array.prototype" xah-js--abbrev-hook-f)

    ;; Function
    ("fp" "Function.prototype" xah-js--abbrev-hook-f)
    ("fl" "Function.length" xah-js--abbrev-hook-f)

    ;; Promise
    ("pp" "Promise.prototype" xah-js--abbrev-hook-f)
    ("Promise.all" "Promise.all (iterable▮)" xah-js--abbrev-hook-f)
    ("Promise.race" "Promise.race (iterable▮)" xah-js--abbrev-hook-f)
    ("Promise.reject" "Promise.reject (r▮)" xah-js--abbrev-hook-f)
    ("Promise.resolve" "Promise.resolve (x▮)" xah-js--abbrev-hook-f)

    ;; RegExp
    ("RegExp.prototype" "RegExp.prototype" xah-js--abbrev-hook-f)

    ("rep" "RegExp.prototype" xah-js--abbrev-hook-f)

    ;; String
    ("String.raw" "String.raw ( template▮ , …substitutions )" xah-js--abbrev-hook-f)
    ("String.fromCharCode" "String.fromCharCode ( int▮, etc )" xah-js--abbrev-hook-f)
    ("String.fromCodePoint" "String.fromCodePoint ( int▮, etc )" xah-js--abbrev-hook-f)

    ("sp" "String.prototype" xah-js--abbrev-hook-f)
    ("sr" "String.raw ( template▮ , …substitutions )" xah-js--abbrev-hook-f)
    ("sfcc" "String.fromCharCode ( int▮, etc )" xah-js--abbrev-hook-f)
    ("sfcp" "String.fromCodePoint ( int▮, etc )" xah-js--abbrev-hook-f)

    ;; Number
    ("np" "Number.prototype" xah-js--abbrev-hook-f)
    ("Number.isFinite" "Number.isFinite ( num▮ )" xah-js--abbrev-hook-f)
    ("nif" "Number.isFinite ( num▮ )" xah-js--abbrev-hook-f)
    ("Number.isInteger" "Number.isInteger ( num▮ )" xah-js--abbrev-hook-f)
    ("nii" "Number.isInteger ( num▮ )" xah-js--abbrev-hook-f)
    ("Number.isNaN" "Number.isNaN ( num▮ )" xah-js--abbrev-hook-f)
    ("nin" "Number.isNaN ( num▮ )" xah-js--abbrev-hook-f)
    ("Number.isSafeInteger" "Number.isSafeInteger ( num▮ )" xah-js--abbrev-hook-f)
    ("nisi" "Number.isSafeInteger ( num▮ )" xah-js--abbrev-hook-f)
    ("Number.parseFloat" "Number.parseFloat ( num▮ )" xah-js--abbrev-hook-f)
    ("npf" "Number.parseFloat ( num▮ )" xah-js--abbrev-hook-f)
    ("Number.parseInt" "Number.parseInt ( num▮, ?base )" xah-js--abbrev-hook-f)
    ("npi" "Number.parseInt ( num▮, ?base )" xah-js--abbrev-hook-f)

    ;; JSON
    ("JSON.stringify" "JSON.stringify ( ▮ )" xah-js--abbrev-hook-f)
    ("JSON.parse" "JSON.parse ( ▮ )" xah-js--abbrev-hook-f)

    ("js" "JSON.stringify ( ▮ )" xah-js--abbrev-hook-f)
    ("jp" "JSON.parse ( ▮ )" xah-js--abbrev-hook-f)

    ;; Reflect
    ("ra" "Reflect.apply ( f▮ , this, arglist )" xah-js--abbrev-hook-f)
    ("rc" "Reflect.construct ( Constructor▮ , arglist, newTarget )" xah-js--abbrev-hook-f)
    ("rdp" "Reflect.defineProperty ( ▮ , key, attributes )" xah-js--abbrev-hook-f)
    ("rg" "Reflect.get ( ▮ , key [ , receiver ])" xah-js--abbrev-hook-f)
    ("rgopd" "Reflect.getOwnPropertyDescriptor ( ▮ , key )" xah-js--abbrev-hook-f)
    ("rgpo" "Reflect.getPrototypeOf ( ▮ )" xah-js--abbrev-hook-f)
    ("rh" "Reflect.has ( ▮ , key )" xah-js--abbrev-hook-f)
    ("rie" "Reflect.isExtensible ( ▮ )" xah-js--abbrev-hook-f)
    ("rok" "Reflect.ownKeys ( ▮ )" xah-js--abbrev-hook-f)
    ("rpe" "Reflect.preventExtensions ( ▮ )" xah-js--abbrev-hook-f)
    ("rs" "Reflect.set ( ▮ , key, V [ , receiver ] )" xah-js--abbrev-hook-f)
    ("rspo" "Reflect.setPrototypeOf ( ▮ , proto )" xah-js--abbrev-hook-f)

    ("Reflect.apply" "Reflect.apply ( f▮ , this, args )" xah-js--abbrev-hook-f)
    ("Reflect.construct" "Reflect.construct ( Constructor▮ , args [, newTarget▮ ] )" xah-js--abbrev-hook-f)
    ("Reflect.defineProperty" "Reflect.defineProperty ( ▮ , key, attributes )" xah-js--abbrev-hook-f)
    ("Reflect.deleteProperty" "Reflect.deleteProperty ( ▮ , key )" xah-js--abbrev-hook-f)
    ("Reflect.get" "Reflect.get ( ▮ , key [ , receiver ])" xah-js--abbrev-hook-f)
    ("Reflect.getOwnPropertyDescriptor" "Reflect.getOwnPropertyDescriptor ( ▮ , key )" xah-js--abbrev-hook-f)
    ("Reflect.getPrototypeOf" "Reflect.getPrototypeOf ( ▮ )" xah-js--abbrev-hook-f)
    ("Reflect.has" "Reflect.has ( ▮ , key )" xah-js--abbrev-hook-f)
    ("Reflect.isExtensible" "Reflect.isExtensible ( ▮ )" xah-js--abbrev-hook-f)
    ("Reflect.ownKeys" "Reflect.ownKeys ( ▮ )" xah-js--abbrev-hook-f)
    ("Reflect.preventExtensions" "Reflect.preventExtensions ( ▮ )" xah-js--abbrev-hook-f)
    ("Reflect.set" "Reflect.set ( ▮ , key, V [ , receiver ] )" xah-js--abbrev-hook-f)
    ("Reflect.setPrototypeOf" "Reflect.setPrototypeOf ( ▮ , proto )" xah-js--abbrev-hook-f)

    ;; Date

    ("Date.prototype" "Date.prototype▮" xah-js--abbrev-hook-f)
    ("Date.UTC" "Date.UTC ( year▮, month , date , hours , minutes , seconds , ms )" xah-js--abbrev-hook-f)
    ("Date.parse" "Date.parse ( string▮ )" xah-js--abbrev-hook-f)
    ("Date.now" "Date.now ()" xah-js--abbrev-hook-f)

    ("dp" "Date.prototype▮" xah-js--abbrev-hook-f)
    ("du" "Date.UTC ( year▮, month , date , hours , minutes , seconds , ms )" xah-js--abbrev-hook-f)
    ("dp" "Date.parse ( string▮ )" xah-js--abbrev-hook-f)
    ("dn" "Date.now ()" xah-js--abbrev-hook-f)

    ;; Math
    ("Math.abs" "Math.abs (▮)" xah-js--abbrev-hook-f)
    ("Math.acos" "Math.acos (▮)" xah-js--abbrev-hook-f)
    ("Math.acosh" "Math.acosh (▮)" xah-js--abbrev-hook-f)
    ("Math.asin" "Math.asin (▮)" xah-js--abbrev-hook-f)
    ("Math.asinh" "Math.asinh (▮)" xah-js--abbrev-hook-f)
    ("Math.atan" "Math.atan (▮)" xah-js--abbrev-hook-f)
    ("Math.atan2" "Math.atan2 (▮)" xah-js--abbrev-hook-f)
    ("Math.atanh" "Math.atanh (▮, y)" xah-js--abbrev-hook-f)
    ("Math.cbrt" "Math.cbrt (▮)" xah-js--abbrev-hook-f)
    ("Math.ceil" "Math.ceil (▮)" xah-js--abbrev-hook-f)
    ("Math.clz32" "Math.clz32 (▮)" xah-js--abbrev-hook-f)
    ("Math.cos" "Math.cos (▮)" xah-js--abbrev-hook-f)
    ("Math.cosh" "Math.cosh (▮)" xah-js--abbrev-hook-f)
    ("Math.exp" "Math.exp (▮)" xah-js--abbrev-hook-f)
    ("Math.expm1" "Math.expm1 (▮)" xah-js--abbrev-hook-f)
    ("Math.floor" "Math.floor (▮)" xah-js--abbrev-hook-f)
    ("Math.fround" "Math.fround (▮)" xah-js--abbrev-hook-f)
    ("Math.hypot" "Math.hypot (v1▮, v2, etc)" xah-js--abbrev-hook-f)
    ("Math.imul" "Math.imul (▮, v2)" xah-js--abbrev-hook-f)
    ("Math.log" "Math.log (▮)" xah-js--abbrev-hook-f)
    ("Math.log10" "Math.log10 (▮)" xah-js--abbrev-hook-f)
    ("Math.log1p" "Math.log1p (▮)" xah-js--abbrev-hook-f)
    ("Math.log2" "Math.log2 (▮)" xah-js--abbrev-hook-f)
    ("Math.max" "Math.max (▮)" xah-js--abbrev-hook-f)
    ("Math.min" "Math.min (▮)" xah-js--abbrev-hook-f)
    ("Math.pow" "Math.pow (▮, y)" xah-js--abbrev-hook-f)
    ("Math.random" "Math.random ()" xah-js--abbrev-hook-f)
    ("Math.round" "Math.round (▮)" xah-js--abbrev-hook-f)
    ("Math.sign" "Math.sign (▮)" xah-js--abbrev-hook-f)
    ("Math.sin" "Math.sin (▮)" xah-js--abbrev-hook-f)
    ("Math.sinh" "Math.sinh (▮)" xah-js--abbrev-hook-f)
    ("Math.sqrt" "Math.sqrt (▮)" xah-js--abbrev-hook-f)
    ("Math.tan" "Math.tan (▮)" xah-js--abbrev-hook-f)
    ("Math.tanh" "Math.tanh (▮)" xah-js--abbrev-hook-f)
    ("Math.toSource" "Math.toSource (▮)" xah-js--abbrev-hook-f)
    ("Math.trunc" "Math.trunc (▮)" xah-js--abbrev-hook-f)

    ;; Object.prototype
    ("hasOwnProperty" "hasOwnProperty (▮)" xah-js--abbrev-hook-f)
    ("isPrototypeOf" "isPrototypeOf ( ▮ )" xah-js--abbrev-hook-f)
    ("propertyIsEnumerable" "propertyIsEnumerable ( ▮ )" xah-js--abbrev-hook-f)
    ("toLocaleString" "toLocaleString (▮)" xah-js--abbrev-hook-f)
    ("toString" "toString ()" xah-js--abbrev-hook-f)
    ("valueOf" "valueOf ( ▮ )" xah-js--abbrev-hook-f)

    ("hop" "hasOwnProperty ( ▮ )" xah-js--abbrev-hook-f)
    ("ipo" "isPrototypeOf ( ▮ )" xah-js--abbrev-hook-f)
    ("pie" "propertyIsEnumerable ( ▮ )" xah-js--abbrev-hook-f)
    ("tls" "toLocaleString (▮)" xah-js--abbrev-hook-f)
    ("ts" "toString ()" xah-js--abbrev-hook-f)
    ("vo" "valueOf ( ▮ )" xah-js--abbrev-hook-f)

    ;; String.prototype
    ("charAt" "charAt (pos▮)" xah-js--abbrev-hook-f)
    ("charCodeAt" "charCodeAt (pos▮)" xah-js--abbrev-hook-f)
    ("codePointAt" "codePointAt (pos▮)" xah-js--abbrev-hook-f)
    ("concat" "concat (arg1▮, arg2, etc)" xah-js--abbrev-hook-f) ; clash with array
    ("endsWith" "endsWith (str▮, pos)" xah-js--abbrev-hook-f)
    ("includes" "includes (str▮, pos)" xah-js--abbrev-hook-f)
    ("indexOf" "indexOf (str▮, pos)" xah-js--abbrev-hook-f)
    ("lastIndexOf" "lastIndexOf (str▮, pos)" xah-js--abbrev-hook-f)
    ("length" "length (▮)" xah-js--abbrev-hook-f)
    ("localeCompare" "localeCompare (▮)" xah-js--abbrev-hook-f)
    ("localeCompare" "localeCompare (▮)" xah-js--abbrev-hook-f)
    ("match" "match (/regex▮/g)" xah-js--abbrev-hook-f)
    ("normalize" "normalize (▮)" xah-js--abbrev-hook-f)
    ("repeat" "repeat (n▮)" xah-js--abbrev-hook-f)
    ("replace" "replace (stringOrRegex▮, replaceStrOrFunc)" xah-js--abbrev-hook-f)
    ("search" "search (regex▮)" xah-js--abbrev-hook-f)
    ("slice" "slice (pos1▮, pos2)" xah-js--abbrev-hook-f)
    ("split" "split ('seperator' or /regex/, ?maxlength)" xah-js--abbrev-hook-f)
    ("startsWith" "startsWith (str▮, endpos)" xah-js--abbrev-hook-f)
    ("substr" "substr (pos1▮, pos2)" xah-js--abbrev-hook-f)
    ("substring" "substring (pos1▮, pos2)" xah-js--abbrev-hook-f)
    ("toLocaleLowerCase" "toLocaleLowerCase ()" xah-js--abbrev-hook-f)
    ("toLocaleUpperCase" "toLocaleUpperCase ()" xah-js--abbrev-hook-f)
    ("toLowerCase" "toLowerCase ()" xah-js--abbrev-hook-f)
    ("toString" "toString ()" xah-js--abbrev-hook-f)
    ("toUpperCase" "toUpperCase ()" xah-js--abbrev-hook-f)
    ("trim" "trim ()" xah-js--abbrev-hook-f)

    ;; Promise.prototype
    ("Promise.prototype.constructor" "Promise.prototype.constructor" xah-js--abbrev-hook-f)
    ("Promise.prototype.catch" "Promise.prototype.catch (onrejected▮)" xah-js--abbrev-hook-f)
    ("ppc" "Promise.prototype.catch (onrejected▮)" xah-js--abbrev-hook-f)
    ("Promise.prototype.then" "Promise.prototype.then (onfullfilled▮, onrejected)" xah-js--abbrev-hook-f)
    ("ppt" "Promise.prototype.then (onfullfilled▮, onrejected)" xah-js--abbrev-hook-f)

    ;; Number.prototype
    ("toFixed" "toFixed ( n▮ )" xah-js--abbrev-hook-f)
    ("tf" "toFixed ( n▮ )" xah-js--abbrev-hook-f)
    ("toPrecision" "toPrecision ( n▮ )" xah-js--abbrev-hook-f)
    ("tp" "toPrecision ( n▮ )" xah-js--abbrev-hook-f)
    ("toExponential" "toExponential ( n▮ )" xah-js--abbrev-hook-f)
    ("toString" "toString ( ?radix▮ )" xah-js--abbrev-hook-f)
    ("toLocaleString" "toLocaleString ( ?reserved )" xah-js--abbrev-hook-f)

    ;; Array.prototype
    ("concat" "concat ( args1▮, args2, etc )" xah-js--abbrev-hook-f)
    ("copyWithin" "copyWithin (target▮, start , end )" xah-js--abbrev-hook-f)
    ("cw" "copyWithin ( target▮, start , end )" xah-js--abbrev-hook-f)
    ("entries" "entries ()" xah-js--abbrev-hook-f)
    ("every" "every ( f▮, this )" xah-js--abbrev-hook-f)
    ("fill" "fill ( value▮, start , end )" xah-js--abbrev-hook-f)
    ("filter" "filter ( f▮, this )" xah-js--abbrev-hook-f)
    ("find" "find ( predicate▮ , this )" xah-js--abbrev-hook-f)
    ("findIndex" "findIndex ( predicate▮ , this )" xah-js--abbrev-hook-f)
    ("forEach" "forEach ( f▮ , this)" xah-js--abbrev-hook-f)
    ("fe" "forEach ( f▮ , this)" xah-js--abbrev-hook-f)
    ("indexOf" "indexOf ( searchElement▮, fromIndex )" xah-js--abbrev-hook-f)
    ("indexOf" "indexOf ( searchElement▮, fromIndex )" xah-js--abbrev-hook-f)
    ("io" "indexOf ( searchElement▮, fromIndex )" xah-js--abbrev-hook-f)
    ("join" "join ('separator')" xah-js--abbrev-hook-f)
    ("keys" "keys ()" xah-js--abbrev-hook-f)
    ("lastIndexOf" "lastIndexOf ( searchElement▮, fromIndex )" xah-js--abbrev-hook-f)
    ("le" "length" xah-js--abbrev-hook-f)
    ("map" "map ( f▮, this )" xah-js--abbrev-hook-f)
    ("pop" "pop ()" xah-js--abbrev-hook-f)
    ("push" "push ( items▮, … )" xah-js--abbrev-hook-f)
    ("reduce" "reduce ( f▮, initialValue )" xah-js--abbrev-hook-f)
    ("reduceRight" "reduceRight ( f▮, initialValue )" xah-js--abbrev-hook-f)
    ("rr" "reduceRight ( f▮, initialValue )" xah-js--abbrev-hook-f)
    ("reverse" "reverse ()" xah-js--abbrev-hook-f)
    ("shift" "shift ()" xah-js--abbrev-hook-f)
    ("slice" "slice (start▮, end)" xah-js--abbrev-hook-f)
    ("some" "some ( f▮, this )" xah-js--abbrev-hook-f)
    ("sort" "sort (comparefn)" xah-js--abbrev-hook-f)
    ("Array.prototype.splice" "splice (start▮, deleteCount , items … )" xah-js--abbrev-hook-f)
    ("splice" "splice (start▮, deleteCount , items … )" xah-js--abbrev-hook-f)
    ("toLocaleString" "toLocaleString ()" xah-js--abbrev-hook-f)
    ("toString" "toString ()" xah-js--abbrev-hook-f)
    ("unshift" "unshift ( items▮ … )" xah-js--abbrev-hook-f)
    ("values" "values ( )" xah-js--abbrev-hook-f)

    ;; Set.prototype
    ("add" "add ( value▮ )" xah-js--abbrev-hook-f)
    ("delete" "delete ( value▮ )" xah-js--abbrev-hook-f)
    ("has" "has ( value▮ )" xah-js--abbrev-hook-f)
    ("clear" "clear ( )" xah-js--abbrev-hook-f)

    ;; Date.prototype
    ("getDate" "getDate ( ▮ )" xah-js--abbrev-hook-f)
    ("getDay" "getDay ( ▮ )" xah-js--abbrev-hook-f)
    ("getFullYear" "getFullYear ( ▮ )" xah-js--abbrev-hook-f)
    ("getHours" "getHours ( ▮ )" xah-js--abbrev-hook-f)
    ("getMilliseconds" "getMilliseconds ( ▮ )" xah-js--abbrev-hook-f)
    ("getMinutes" "getMinutes ( ▮ )" xah-js--abbrev-hook-f)
    ("getMonth" "getMonth ( ▮ )" xah-js--abbrev-hook-f)
    ("getSeconds" "getSeconds ( ▮ )" xah-js--abbrev-hook-f)
    ("getTime" "getTime ( ▮ )" xah-js--abbrev-hook-f)
    ("getTimezoneOffset" "getTimezoneOffset ( ▮ )" xah-js--abbrev-hook-f)
    ("getUTCDate" "getUTCDate ( ▮ )" xah-js--abbrev-hook-f)
    ("getUTCDay" "getUTCDay ( ▮ )" xah-js--abbrev-hook-f)
    ("getUTCFullYear" "getUTCFullYear ( ▮ )" xah-js--abbrev-hook-f)
    ("getUTCHours" "getUTCHours ( ▮ )" xah-js--abbrev-hook-f)
    ("getUTCMilliseconds" "getUTCMilliseconds ( ▮ )" xah-js--abbrev-hook-f)
    ("getUTCMinutes" "getUTCMinutes ( ▮ )" xah-js--abbrev-hook-f)
    ("getUTCMonth" "getUTCMonth ( ▮ )" xah-js--abbrev-hook-f)
    ("getUTCSeconds" "getUTCSeconds ( ▮ )" xah-js--abbrev-hook-f)
    ("setDate" "setDate ( date▮ )" xah-js--abbrev-hook-f)
    ("setFullYear" "setFullYear ( year▮ [ , month [ , date ] ] )" xah-js--abbrev-hook-f)
    ("setHours" "setHours ( hour▮ [ , min [ , sec [ , ms ] ] ] )" xah-js--abbrev-hook-f)
    ("setMilliseconds" "setMilliseconds ( ms▮ )" xah-js--abbrev-hook-f)
    ("setMinutes" "setMinutes ( min▮ [ , sec [ , ms ] ] )" xah-js--abbrev-hook-f)
    ("setMonth" "setMonth ( month▮ [ , date ] )" xah-js--abbrev-hook-f)
    ("setSeconds" "setSeconds ( sec▮ [ , ms ] )" xah-js--abbrev-hook-f)
    ("setTime" "setTime ( time▮ )" xah-js--abbrev-hook-f)
    ("setUTCDate" "setUTCDate ( date▮ )" xah-js--abbrev-hook-f)
    ("setUTCFullYear" "setUTCFullYear ( year▮ [ , month [ , date ] ] )" xah-js--abbrev-hook-f)
    ("setUTCHours" "setUTCHours ( hour▮ [ , min [ , sec [ , ms ] ] ] )" xah-js--abbrev-hook-f)
    ("setUTCMilliseconds" "setUTCMilliseconds ( ms▮ )" xah-js--abbrev-hook-f)
    ("setUTCMinutes" "setUTCMinutes ( min▮ [ , sec [, ms ] ] )" xah-js--abbrev-hook-f)
    ("setUTCMonth" "setUTCMonth ( month▮ [ , date ] )" xah-js--abbrev-hook-f)
    ("setUTCSeconds" "setUTCSeconds ( sec▮ [ , ms ] )" xah-js--abbrev-hook-f)
    ("toDateString" "toDateString ( ▮ )" xah-js--abbrev-hook-f)
    ("toISOString" "toISOString ( ▮ )" xah-js--abbrev-hook-f)
    ("toJSON" "toJSON ( key▮ )" xah-js--abbrev-hook-f)
    ("toLocaleDateString" "toLocaleDateString ( [ reserved1 [ , reserved2 ] ] )" xah-js--abbrev-hook-f)
    ("toLocaleString" "toLocaleString ( [ reserved1 [ , reserved2 ] ] )" xah-js--abbrev-hook-f)
    ("toLocaleTimeString" "toLocaleTimeString ( [ reserved1 [ , reserved2 ] ] )" xah-js--abbrev-hook-f)
    ("toString" "toString ( ▮ )" xah-js--abbrev-hook-f)
    ("toTimeString" "toTimeString ( ▮ )" xah-js--abbrev-hook-f)
    ("toUTCString" "toUTCString ( ▮ )" xah-js--abbrev-hook-f)
    ("[Symbol.toPrimitive]" "[Symbol.toPrimitive] ( hint▮ )" xah-js--abbrev-hook-f)

    ;; 1 letter abbrevs

    ("a" "Array" xah-js--abbrev-hook-f)
    ("d" "document." xah-js--abbrev-hook-f)
    ("f" "false" xah-js--abbrev-hook-f)
    ("l" "let ▮ = 3;" xah-js--abbrev-hook-f)
    ("o" "Object." xah-js--abbrev-hook-f)
    ("p" "prototype." xah-js--abbrev-hook-f)
    ("r" "return ▮;" xah-js--abbrev-hook-f)
    ("s" "Symbol" xah-js--abbrev-hook-f)
    ("t" "true" xah-js--abbrev-hook-f)
    ("u" "undefined" xah-js--abbrev-hook-f)
    ("w" "window." xah-js--abbrev-hook-f)
    ("c" "const ▮ = 3;" xah-js--abbrev-hook-f)

    ;; lang syntax
    ("af" "((x▮) => { 3 })" xah-js--abbrev-hook-f)
    ("af2" "((x▮, x2) => ({ 3 }))" xah-js--abbrev-hook-f)
    ("caf" "const f = ((x▮) => { 3 });" xah-js--abbrev-hook-f)
    ("case" "case ▮: x; break" xah-js--abbrev-hook-f)
    ("con" "constructor" xah-js--abbrev-hook-f)
    ("cl" "console.log ( ▮ );" xah-js--abbrev-hook-f)
    ("class" "class A▮ {\n  constructor(x) {\n    this.p = x;\n  }\n}" xah-js--abbrev-hook-f)
    ("cls" "class A▮ {\n  constructor(x) {\n    this.p = x;\n  }\n}" xah-js--abbrev-hook-f)
    ("cm" "/* [ ▮ ] */" xah-js--abbrev-hook-f)
    ("cmt" "/**\n * desc▮.\n * @param {string} title The title of the book.\n * @return {number} The circumference of the circle.\n */" xah-js--abbrev-hook-f)
    ("do" "do { ▮; x++} while (x != 5)" xah-js--abbrev-hook-f)
    ("ei" "else if (▮) { 3 }" xah-js--abbrev-hook-f)
    ("else" "else { ▮ }" xah-js--abbrev-hook-f)
    ("eq" "=== " xah-js--abbrev-hook-f)
    ("ne" "!== " xah-js--abbrev-hook-f)
    ("fi" "for (let p▮ in obj) { }" xah-js--abbrev-hook-f)
    ("finally" "finally {\n▮\n}" xah-js--abbrev-hook-f)
    ("fo" "for (let p▮ of iterable) { }" xah-js--abbrev-hook-f)
    ("for" "for (let i = 0; i < ▮.length; i++) { }" xah-js--abbrev-hook-f)
    ("function" "function ▮ () { return 3 }" xah-js--abbrev-hook-f)
    ("fu" "function ▮ () { 3 }" xah-js--abbrev-hook-f)
    ("gf" "function* ▮ () { yield 3;}" xah-js--abbrev-hook-f)
    ("switch" "switch(▮) {\n    case 3:\n3\n        break\n    case 3:\n3\n        break\n    default:\n        3\n}" xah-js--abbrev-hook-f)
    ("ie" "( ( test▮ ) ? 1 : 0 )" xah-js--abbrev-hook-f)
    ("to" "typeof " xah-js--abbrev-hook-f)
    ("try" "try {\n▮\n} catch (error) {\n▮\n}" xah-js--abbrev-hook-f)
    ("us" "'use strict';\n" xah-js--abbrev-hook-f)
    ("var" "var ▮ = 3;" xah-js--abbrev-hook-f)
    ("while" "while (i<10) { ▮; i++ }" xah-js--abbrev-hook-f)
    ("yi" "yield ▮;" xah-js--abbrev-hook-f)
    ("if" "if ( ▮ ) {\n}" xah-js--abbrev-hook-f)
    ("ps" "+" xah-js--abbrev-hook-f)

    ("getter" "get keyname▮ () {body};" xah-js--abbrev-hook-f)
    ("setter" "get keyname▮ (x) {body};" xah-js--abbrev-hook-f)

    ("nan" "NaN" xah-js--abbrev-hook-f)
    ("inf" "Infinity" xah-js--abbrev-hook-f)
    ("ud" "undefined" xah-js--abbrev-hook-f)
    ("fc" "firstChild" xah-js--abbrev-hook-f)

    ("parseInt" "parseInt ( num▮, ?base )" xah-js--abbrev-hook-f)

    ;; dom
    ("ce" "document.createElement('div')" xah-js--abbrev-hook-f)

    ("addEventListener" "addEventListener ('click', ▮ , false)" xah-js--abbrev-hook-f)
    ("ael" "addEventListener" xah-js--abbrev-hook-f)
    ("gebi" "getElementById ('▮')" xah-js--abbrev-hook-f)
    ("getElementById" "getElementById ('▮')" xah-js--abbrev-hook-f)
    ("setInterval" "setInterval (func, delay, param1, param2)" xah-js--abbrev-hook-f)
    ("setTimeout" "setTimeout (func, delay, param1, param2)" xah-js--abbrev-hook-f)

    ("ci" "clearInterval (id▮)" xah-js--abbrev-hook-f)
    ("ct" "clearTimeout (id▮)" xah-js--abbrev-hook-f)

    ("si" "setInterval (func, delay, param1, param2)" xah-js--abbrev-hook-f)
    ("st" "setTimeout (func, delay, param1, param2)" xah-js--abbrev-hook-f)
    ("sa" "setAttribute ('style', ▮ )" xah-js--abbrev-hook-f)

    ("ac" "appendChild ( ▮ )" xah-js--abbrev-hook-f)
    ("iae" "insertAdjacentElement('beforebegin' 'afterbegin' 'beforeend' 'afterend' , new▮ )" xah-js--abbrev-hook-f)
    ("insertAdjacentElement" "insertAdjacentElement('beforebegin' 'afterbegin' 'beforeend' 'afterend' , new▮ )" xah-js--abbrev-hook-f)

    ;;
    )

  "abbrev table for `xah-js-mode'"

  )

;; (abbrev-table-put xah-js-mode-abbrev-table :regexp "\\([._0-9A-Za-z]+\\)") ; override by xah-js-expand-abbrev
(abbrev-table-put xah-js-mode-abbrev-table :case-fixed t)
(abbrev-table-put xah-js-mode-abbrev-table :system t)
;; (abbrev-table-put xah-js-mode-abbrev-table :enable-function 'xah-js-abbrev-enable-function) ; override by xah-js-expand-abbrev



;;;###autoload
(define-derived-mode xah-js-mode prog-mode "∑js"
  "A major mode for JavaScript.

URL `http://ergoemacs.org/emacs/xah-js-mode.html'

\\{xah-js-mode-map}"

  (setq font-lock-defaults '((xah-js-font-lock-keywords)))

  (setq-local comment-start "// ")
  (setq-local comment-end "")
  (setq-local comment-column 2)

  (setq-local syntax-propertize-function #'js-syntax-propertize)

  (make-local-variable 'abbrev-expand-function)
  (if (version< emacs-version "24.4")
      (add-hook 'abbrev-expand-functions 'xah-js-expand-abbrev nil t)
    (setq abbrev-expand-function 'xah-js-expand-abbrev))

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

;;; xah-js-mode.el ends here
