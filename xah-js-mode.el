;;; xah-js-mode.el --- Major mode for editing emacs lisp. -*- coding: utf-8 -*-

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
"false"
"finally"
"for"
"function"
"if"
"in"
"instanceof"
"new"
"null"
"return"
"switch"
"this"
"throw"
"true"
"try"
"typeof"
"var"
"void"
"while"
"with"

"substring"
"undefined"

"Array"
"Boolean"
"Date"
"Error"
"EvalError"
"Function"
"Infinity"
"JSON"
"Math"
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

;; --------------------
"charCodeAt"
"apply"
"arguments"
"getBoundingClientRect"
"indexOf"
"firstChild"
"nodeValue"

 "length"

"forEach"

"Math.floor"

"console.log"
"document.createElement"
"innerHTML"
"hasChildNodes"
"lastChild"
"removeChild"
"document"

"parentNode"
"appendChild"

"String.fromCodePoint"
"toString"
"style"
"setAttribute"
"createTextNode"

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
"toString"
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
"toString"
"toValueOf"
"localeCompare"
) )

(defvar xjs-dom-words nil "a list of keywords more or less related to emacs system.")
(setq xjs-dom-words '(
"addEventListener"
"removeEventListener"
"document.getElementById"
"document.getElementsByTagName"
) )

(defvar xjs-keyword-builtin nil "a list of js  names")
(setq xjs-keyword-builtin '(

) )

(defvar xjs-js-vars-1 nil "a list js variables names")
(setq xjs-js-vars-1 '(
) )


;; syntax coloring related

(setq xjs-font-lock-keywords
      (let (
          (domWords (regexp-opt xjs-dom-words 'symbols) )
          (emacsBuiltins (regexp-opt xjs-keyword-builtin 'symbols) )
          (jsLangWords (regexp-opt xjs-js-lang-words 'symbols) )
          (jsVars1 (regexp-opt xjs-js-vars-1 'symbols) )
          (jsArrayMethods (regexp-opt xjs-js-array-methods 'symbols) )
          (jsStrMethods (regexp-opt xjs-js-str-methods 'symbols) )

)
        `(
          (,domWords . font-lock-function-name-face)
          (,emacsBuiltins . font-lock-type-face)
          (,jsLangWords . font-lock-keyword-face)
          (,jsArrayMethods . font-lock-keyword-face)
          (,jsStrMethods . font-lock-keyword-face)
          (,jsVars1 . font-lock-variable-name-face)
          ) ) )

;;font-lock-comment-delimiter-face
;;font-lock-comment-face
;;font-lock-doc-face
;;font-lock-negation-char-face
;;font-lock-preprocessor-face
;;font-lock-reference-face
;;font-lock-string-face
;;font-lock-type-face
;;font-lock-variable-name-face
;;font-lock-warning-face


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
