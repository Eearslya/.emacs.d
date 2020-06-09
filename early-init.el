;;; early-init.el --- -*- lexical-binding: t -*-
;;
;; Filename: early-init.el
;; Description: Emacs early initialization
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; With Emacs version 27, they introduced a new early-init.el file,
;; which is run before package and UI initialization. We can take
;; advantage of this file to add some more startup optimzations.
;; We use this file to pave the way for our true star, the el file
;; tangled together from our Org file.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

;; Defer garbage collection until after initialization is complete
(setq gc-cons-threshold 100000000)

;; Unset file name handlers until after initialization is complete
(defvar file-name-handler-alist-original file-name-handler-alist)
(setq file-name-handler-alist nil)

;; Defer package initialization until we're ready
(setq package-enable-at-startup nil)

;; Ignore any site-start files
(setq site-run-file nil)

;; Ensure we're always running the newest Elisp code, whether that's
;; byte-compiled or source.
(setq load-prefer-newer t)

;; Disable all UI elements before they even initialize
(menu-bar-mode -1)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; Set up a hook to restore garbage collection and file name handlers
;; once initialization is complete.
(add-hook 'emacs-startup-hook
	  (lambda ()
	    (setq gc-cons-threshold 67108864) ; 64MB
	    (setq file-name-handler-alist file-name-handler-alist-original)
	    (makunbound 'file-name-handler-alist-original)))

;; Byte-compile our actual init.el for whatever sliver of performance
;; that gets us.
(let ((elfile (expand-file-name "init.el" user-emacs-directory))
      (elcfile (expand-file-name "init.elc" user-emacs-directory)))
  (when (file-newer-than-file-p elfile elcfile)
    (byte-compile-file elfile)))

(provide 'early-init)
;;; early-init.el ends here
