;;; xah-js-mode.el --- Major mode for editing JavaScript. -*- coding: utf-8 -*-

;; Copyright © 2013 by Xah Lee

;; Author: Xah Lee <xah@xahlee.org> ( http://xahlee.org/ )
;; Created: 2013-03-23
;; Keywords: languages, convenience

;; You can redistribute this program and/or modify it. Please give credit and link. Thanks.

;;; Commentary:
;; Major mode for editing JavaScript code. Beta stage.
;; home page:

;;; HISTORY

;; version 0.2, 2013-12-25 added array methods and string method highlighting
;; version 0.1, 2013-08-21 first version

;; TODO
;; rid of calling js-mode
;; make sure the comment works
;; create complete syntax table
;; add new faces
;; separate diff types of keywords to use diff face
;; add completion
;; add indentation
;; add support for autocomplete

(defvar xah-js-mode-hook nil "Standard hook for `xah-js-mode'")

(defvar xjs-js-lang-words nil "a list of JavaScript keywords.")
(setq xjs-js-lang-words '(

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
"function"
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

"call"
"isExtensible"
"preventExtensions"
"getOwnPropertyDescriptor"
"isSealed"
"isFrozen"
"freeze"

"substring"

"Array"
"Boolean"
"Date"
"Error"
"EvalError"
"Function"
"Infinity"
"JSON"
"NaN"
"Number"
"Object"
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

"alert"

"writable"
"enumerable"
"configurable"

;; --------------------

"toString"

"create"
"getPrototypeOf"
"isPrototypeOf"
"defineProperty"
"value"

"prototype"

"getOwnPropertyNames"
"hasOwnProperty"

"apply"
"arguments"
"getBoundingClientRect"
"indexOf"
"firstChild"
"nodeValue"

"constructor"
"length"

"JSON.stringify"

) )

(defvar xjs-js-array-methods nil "a list of JavaScript array methods.")
(setq xjs-js-array-methods '(
"concat"
"every"
"filter"
"forEach"
"indexOf"
"join"
"lastIndexOf"
"length"
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
"toLocalString"

"unshift"
) )

(defvar xjs-js-str-methods nil "a list of JavaScript string methods.")
(setq xjs-js-str-methods '(
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

"toValueOf"
"localeCompare"
) )

(defvar xjs-js-math-methods nil "a list of JavaScript Math methods.")
(setq xjs-js-math-methods '(
"Math.abs"
"Math.acos"
"Math.acosh"
"Math.asin"
"Math.asinh"
"Math.atan"
"Math.atanh"
"Math.atan2"
"Math.cbrt"
"Math.ceil"
"Math.clz32"
"Math.cos"
"Math.cosh"
"Math.exp"
"Math.expm1"
"Math.floor"
"Math.fround"
"Math.hypot"
"Math.imul"
"Math.log"
"Math.log1p"
"Math.log10"
"Math.log2"
"Math.max"
"Math.min"
"Math.pow"
"Math.random"
"Math.round"
"Math.sign"
"Math.sin"
"Math.sinh"
"Math.sqrt"
"Math.tan"
"Math.tanh"
"Math.toSource"
"Math.trunc"
) )

(defvar xjs-dom-words nil "a list of keywords from DOM or browser.")
(setq xjs-dom-words '(

"style"
"setAttribute"
"createTextNode"

"addEventListener"
"removeEventListener"
"getElementById"
"getElementsByTagName"
"getElementsByClassName"

"onmouseover"

"console.log"
"createElement"
"innerHTML"
"hasChildNodes"
"lastChild"
"removeChild"
"document"

"parentNode"
"appendChild"
) )

(defvar xjs-keyword-builtin nil "a list of js  names")
(setq xjs-keyword-builtin '(
) )

(defvar xjs-constants nil "a list of constants")
(setq xjs-constants '(
"null"
"undefined"
"true"
"false"
"Math.E"
"Math.LN2"
"Math.LN10"
"Math.LOG2E"
"Math.LOG10E"
"Math.PI"
"Math.SQRT1_2"
"Math.SQRT2"
) )

(defvar xjs-js-vars-1 nil "a list js variables names")
(setq xjs-js-vars-1 '(
) )


;; syntax coloring related

(setq xjs-font-lock-keywords
      (let (
          (jsMathMethods (regexp-opt xjs-js-math-methods 'symbols) )
          (domWords (regexp-opt xjs-dom-words 'symbols) )
          (jsBuildins (regexp-opt xjs-keyword-builtin 'symbols) )
          (jsLangWords (regexp-opt xjs-js-lang-words 'symbols) )
          (jsVars1 (regexp-opt xjs-js-vars-1 'symbols) )
          (jsArrayMethods (regexp-opt xjs-js-array-methods 'symbols) )
          (jsStrMethods (regexp-opt xjs-js-str-methods 'symbols) )
          (jsConstants (regexp-opt xjs-constants 'symbols) )
          )
        `(
          (,jsMathMethods . font-lock-keyword-face)
          (,jsConstants . font-lock-constant-face)
          (,domWords . font-lock-function-name-face)
          (,jsBuildins . font-lock-type-face)
          (,jsLangWords . font-lock-keyword-face)
          (,jsArrayMethods . font-lock-keyword-face)
          (,jsStrMethods . font-lock-keyword-face)
          (,jsVars1 . font-lock-variable-name-face)
          ) ) )

;; fontfont-lock-builtin-face
;; font-lock-comment-delimiter-face
;; font-lock-comment-face
;; font-lock-constant-face
;; font-lock-doc-face
;; font-lock-function-name-face
;; font-lock-keyword-face
;; font-lock-negation-char-face
;; font-lock-preprocessor-face
;; font-lock-reference-face
;; font-lock-string-face
;; font-lock-syntactic-face-function
;; font-lock-type-face
;; font-lock-variable-name-face
;; font-lock-warning-face


;; keybinding

(defvar xjs-keymap nil "Keybinding for `xah-js-mode'")
(progn
  (setq xjs-keymap (make-sparse-keymap))
;  (define-key xjs-keymap [remap comment-dwim] 'xjs-comment-dwim)
)


;; syntax table
(defvar xjs-syntax-table nil "Syntax table for `xah-js-mode'.")

 ;; (setq xjs-syntax-table
 ;;       (let ((synTable (make-syntax-table)))
 ;;   (modify-syntax-entry ?\/ ". 12b" synTable)
 ;;   (modify-syntax-entry ?\n "> b" synTable)
 ;;         synTable))

(require 'cc-mode)

(setq xjs-syntax-table
     (let ((synTable (make-syntax-table)))
       (c-populate-syntax-table synTable) ; todo: rid of dependence
       (modify-syntax-entry ?$ "_" synTable)
       synTable))



;; define the mode
(define-derived-mode xah-js-mode fundamental-mode
  "ξXJS"
  "A simple major mode for JavaScript.

JavaScript keywords are colored. Basically that's it.

\\{xjs-keymap}"
  (js-mode)
  (setq mode-name "ξXJS")
  (setq font-lock-defaults '((xjs-font-lock-keywords)))

(setq comment-start "//")
(setq comment-end "")

  (set-syntax-table xjs-syntax-table)
  (use-local-map xjs-keymap)
  (run-mode-hooks 'xah-js-mode-hook)
)

(when (featurep 'auto-complete )
  (add-to-list 'ac-modes 'xah-js-mode)
  (add-hook 'xah-js-mode-hook 'ac-emacs-lisp-mode-setup)
  )

(provide 'xah-js-mode)
