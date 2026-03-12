;;; early-init.el --- Early initialization -*- lexical-binding: t -*-

;;; Commentary:
;; Runs before package system and frame initialization.
;; Speeds up startup by deferring GC, disabling UI elements early,
;; and preventing double package initialization.

;;; Code:

;; Defer garbage collection during startup
(setq gc-cons-threshold 100000000)

;; Prevent package.el from initializing before init.el handles it
(setq package-enable-at-startup nil)

;; Disable UI elements before frame draws to avoid flicker
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(setq frame-inhibit-implied-resize t)

;;; early-init.el ends here
