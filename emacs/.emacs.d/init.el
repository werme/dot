;;; init.el --- Initialization file for Emacs  -*- lexical-binding: t -*-
;;; Commentary:
;;; Emacs Startup File --- initialization for Emacs
;;; Package --- Summary
;;; Code:

(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(setq fill-column 80 ; toggle wrapping text at the 80th character
      scroll-conservatively 101
      ispell-program-name "aspell")

(setq inhibit-startup-screen t)
(setq initial-scratch-message "")
(setq enable-recursive-minibuffers t)
(setq column-number-mode t)
;; Don't make backup files.
(setq make-backup-files nil)
;; Don't make autosave files.
(setq auto-save-default nil)
;; Don't make lockfiles.
(setq create-lockfiles nil)
(setq-default indent-tabs-mode nil)

(with-eval-after-load 'display-line-numbers
  (setq display-line-numbers-type 'relative
        display-line-numbers-width-start t))

(if (display-graphic-p)
    (progn
      (tool-bar-mode -1)
      (scroll-bar-mode -1)))

(when (string= system-type "darwin")
  (setq dired-use-ls-dired nil))

(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(setq-default indent-tabs-mode nil)
(global-hl-line-mode 1)
(menu-bar-mode -1)
(blink-cursor-mode 0)
(winner-mode 1)
(put 'narrow-to-region 'disabled nil)
(prefer-coding-system 'utf-8)
;; (desktop-save-mode 1)

(defvar bootstrap-version)
(let ((bootstrap-file
              (expand-file-name
               "straight/repos/straight.el/bootstrap.el"
               user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
    (load bootstrap-file nil 'nomessage))

;; (setq package-enable-at-startup nil)
(let ((default-directory "~/.emacs.d/elpa"))
  (normal-top-level-add-subdirs-to-load-path))

(package-initialize)

(straight-use-package 'use-package)

;; (unless (package-installed-p 'use-package) ; unless it is already installed
;;   (package-refresh-contents) ; updage packages archive
;;   (package-install 'use-package)) ; and install the most recent version of use-package

(eval-when-compile
  (require 'use-package))
  ;; (setq use-package-always-ensure t))

(use-package package
  :config
  (setq package-check-signature nil)
  (setq package-enable-at-startup nil)
  (setq package-archives '(("gnu"          . "https://elpa.gnu.org/packages/")
                           ("melpa"        . "https://melpa.org/packages/")
                           ("melpa-stable" . "https://stable.melpa.org/packages/")
                           ("repo-org"     . "https://orgmode.org/elpa/"))))

(use-package selectrum
  :straight (:host github :repo "raxod502/selectrum")
  :defer t
  :init
  ;; This doesn't actually load Selectrum.
  (selectrum-mode +1))

;; Package `prescient' is a library for intelligent sorting and
;; filtering in various contexts.
(use-package prescient
  :config
  ;; Remember usage statistics across Emacs sessions.
  (prescient-persist-mode +1))

;; Package `selectrum-prescient' provides intelligent sorting and
;; filtering for candidates in Selectrum menus.
(use-package selectrum-prescient
  :straight (:host github :repo "raxod502/prescient.el"
                                      :files
                                      ("selectrum-prescient.el"))
  :demand t
  :after selectrum
  :config
  (selectrum-prescient-mode +1))

;; Feature `saveplace' provides a minor mode for remembering the
;; location of point in each file you visit, and returning it there
;; when you find the file again.
(use-package saveplace
  :ensure t
  :demand t
  :config
  (save-place-mode +1))

(use-package exec-path-from-shell
  :hook (prog-mode . exec-path-from-shell-initialize))

(use-package which-key
  :config (which-key-mode 1))

(use-package general
  :after which-key
  :config
  (general-override-mode 1)

  (defun find-user-init-file ()
    "Edit the `user-init-file', in same window."
    (interactive)
    (find-file user-init-file))
  (defun load-user-init-file ()
    "Load the `user-init-file', in same window."
    (interactive)
    (load-file user-init-file))

  (general-create-definer tyrant-def
    :states '(normal visual insert motion emacs)
    :keymaps 'override
    :prefix "SPC"
    :non-normal-prefix "C-SPC")

  (general-def
    "C-x x" 'eval-defun)

  (tyrant-def

    ""     nil
    "c"   (general-simulate-key "C-c")
    "h"   (general-simulate-key "C-h")
    "u"   (general-simulate-key "C-u")
    "x"   (general-simulate-key "C-x")
    "s"   (general-simulate-key "C-s")

    "SPC" (general-simulate-key "M-x")

    ;; Quit operations
    "q"	  '(:ignore t :which-key "quit emacs")
    "qq"  'kill-emacs

    ;; Buffer operations
    "b"   '(:ignore t :which-key "buffer")
    "bb"  'mode-line-other-buffer
    "bd"  'kill-this-buffer
    "bn"  'next-buffer
    "bp"  'previous-buffer
    "bq"  'kill-buffer-and-window
    "br"  'revert-buffer
    "bm"  'ibuffer

    ;; Window operations
    "w"   '(:ignore t :which-key "window")
    "wo"  'maximize-window
    "wx"  'split-window-horizontally
    "ws"  'split-window-vertically
    "wu"  'winner-undo
    "wr"  'winner-redo
    "ww"  'ace-window
    "wd"  'delete-window
    "wD"  'delete-other-windows

    ;; File operations
    "f"   '(:ignore t :which-key "files")
    "fc"  'write-file
    "fe"  '(:ignore t :which-key "emacs")
    "fed" 'find-user-init-file
    "feR" 'load-user-init-file
    "fj"  'dired-jump
    "fl"  'find-file-literally
    "fr"  'rename-file
    "fs"  'save-buffer

    ;; Applications
    "a"   '(:ignore t :which-key "Applications")
    "ad"  'dired
    ":"   'shell-command)
                                        ;    ";"   'eval-expression
                                        ; "ac"  'calendar
                                        ;    "oa"  'org-agenda)

  (general-def 'normal doc-view-mode-map
    "j"   'doc-view-next-line-or-next-page
    "k"   'doc-view-previous-line-or-previous-page
    "gg"  'doc-view-first-page
    "G"   'doc-view-last-page
    "C-d" 'doc-view-scroll-up-or-next-page
    "C-f" 'doc-view-scroll-up-or-next-page
    "C-b" 'doc-view-scroll-down-or-previous-page))

                                        ;  (general-def '(normal visual) outline-minor-mode-map
                                        ;    "zn"  'outline-next-visible-heading
                                        ;    "zp"  'outline-previous-visible-heading
                                        ;    "zf"  'outline-forward-same-level
                                        ;    "zB"  'outline-backward-same-level)

(use-package evil
  :hook (after-init . evil-mode)
  :config
  (evil-set-initial-state 'shell-mode 'insert)
  (setq doc-view-continuous t)
  :general
  (tyrant-def
    "wh"  'evil-window-left
    "wl"  'evil-window-right
    "wj"  'evil-window-down
    "wk"  'evil-window-up
    "bN"  'evil-buffer-new))

(use-package evil-surround
  :after evil
  :config (global-evil-surround-mode 1))

(use-package avy
  :general
  (tyrant-def
    "gc" 'avy-goto-char-timer))

(use-package evil-escape
  :after evil
  :config
  (setq-default evil-escape-key-sequence "fd")
  (evil-escape-mode 1))

(use-package evil-commentary
  :after evil
  :config (evil-commentary-mode 1))

(use-package evil-visualstar
  :after evil
  :config
  (setq evilmi-always-simple-jump t)
  (global-evil-visualstar-mode 1))

(use-package ace-window)
;; (use-package deadgrep
;;   :load-path "~/.emacs.d/private/deadgrep"
;;   :bind* (("C-c /" . deadgrep)))

(use-package company
  :hook (after-init . global-company-mode)
  :config
  (define-key company-active-map (kbd "M-n") nil)
  (define-key company-active-map (kbd "M-p") nil)
  (define-key company-active-map (kbd "C-n") #'company-select-next-or-abort)
  (define-key company-active-map (kbd "C-p") #'company-select-previous-or-abort)
  (setq company-minimum-prefix-length 1)
  (setq company-idle-delay 0.1)
  (setq company-dabbrev-downcase nil)
  ;; Always display the entire suggestion list onscreen, placing it
  ;; above the cursor if necessary.
  (setq company-tooltip-minimum company-tooltip-limit)
  ;; Always display suggestions in the tooltip, even if there is only
  ;; one. Also, don't display metadata in the echo area.
  (setq company-frontends '(company-pseudo-tooltip-frontend))
  (setq company-backends '((company-capf
                            company-files
                            company-dabbrev-code
                            company-keywords
                            company-dabbrev
                            company-yasnippet))))


(use-package company-prescient
  :straight t
  :hook 'company-mode-hook)

;; (use-package company-quickhelp
;;   :defer 5
;;   :config (company-quickhelp-mode))

;; (use-package company-statistics
;;   :defer 5
;;   :config (company-statistics-mode))

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-l")
  (setq lsp-diagnostic-package :flymake)
  :hook ((js-mode . lsp-deferred)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands (lsp lsp-deferred))

(use-package flymake
  :config
  (set-face-attribute 'flymake-error nil :foreground "#ff6655" :background "#100")
  ;; (define-key flymake-mode-map (kbd "M-]") 'flymake-goto-next-error)
  ;; (define-key flymake-mode-map (kbd "M-[") 'flymake-goto-prev-error)
  (tyrant-def
    "e"  '(:ignore t :which-key "Errors")
    "]"  'flymake-goto-next-error
    "["  'flymake-goto-previous-error))
;; "en" 'flycheck-next-error
;; "ep" 'flycheck-previous-error
;; "ee" 'counsel-flycheck
;; "bc" 'flycheck-buffer))

(use-package company-lsp
  :after company
  :commands company-lsp
  :config (add-to-list 'company-backends 'company-lsp))

(use-package projectile
  :commands (projectile-mode)
  :general
  (tyrant-def
    "p"     '(:ignore t :which-key "projectile")
    "pd"    'projectile-find-dir
    "ps"    'projectile-switch-project
    "pf"    'projectile-find-file
    "pg"    'projectile-ripgrep
    "pb"    'projectile-switch-to-buffer
    "j"     'projectile-find-file)
  :init
  (let ((project-root-paths '("~/dev/" "~/code/" "~/kry/code/" "~/.config/")))
    (setq projectile-project-search-path (seq-filter #'file-directory-p
                                                     project-root-paths)))

  :config
  (setq projectile-completion-system 'default
        projectile-enable-caching t
        projectile-switch-project-action 'projectile-dired)
  (projectile-mode 1))


;; (use-package helm-flyspell
;;   :commands (helm-flyspell-correct))
;;   ;; :general
;;   ;;  (:keymaps '(flyspell-mode-map)
;;   ;;   :states '(normal visual)
;;   ;;   "zs" 'helm-flyspell-correct
;;   ;;   "z=" 'flyspell-buffer))

;; (use-package prettier-js
;;   :commands (prettier-js-mode))

;; (eval-after-load 'js-mode
;;   '(progn
;;      (add-hook 'js-mode-hook #'add-node-modules-path)))
;; (add-hook 'js-mode-hook #'prettier-js-mode)))

(use-package isearch
  :config
  (setq lazy-highlight-initial-delay 0))

(use-package ctrlf
  :straight (:host github :repo "raxod502/ctrlf")
  :config
  (ctrlf-mode +1))

(use-package magit
  :commands (magit-status)
  :general
  (tyrant-def
    "g"   '(:ignore t :which-key "git")
    "gs"  'magit-status
    "gg"  'magit-status))

(use-package evil-magit
  :hook (magit-mode . evil-magit-init))

;; (use-package yasnippet)

(use-package transpose-frame
  :commands (transpose-frame)
  :general
  (tyrant-def "wt" 'transpose-frame))

(use-package shell-pop
  :commands (shell-pop)
  :config (setq shell-pop-shell-type '("shell"
                                       "*shell*"
                                       (lambda nil (shell))))
  :general
  (tyrant-def "'" 'shell-pop))

(setq whitespace-style '(face trailing))
(setq scroll-margin 10)

;; Package `apheleia' implements a sophisticated algorithm for
;; applying code formatters asynchronously on save without moving
;; point or modifying the scroll position.
(use-package apheleia
  :straight (:host github :repo "raxod502/apheleia")
  :init
  (apheleia-global-mode +1))

(use-package editorconfig
  :config
  (editorconfig-mode 1))

(defun my-prog-mode-hook ()
  (auto-fill-mode)
  (show-paren-mode)
  (whitespace-mode)
  ;; (electric-pair-mode)
  ;; (yas-global-mode 1)
  ;; (display-line-numbers-mode)
  )

(add-hook 'prog-mode-hook 'my-prog-mode-hook)
(setq before-save-hook 'nil)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(use-package doom-modeline
  :config
  (setq doom-modeline-unicode-fallback t)
  :hook (after-init . doom-modeline-mode))

(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t)
  (setq doom-themes-enable-italic t)
  (load-theme 'doom-one t))

(eval-when-compile
  (setq-default custom-file (expand-file-name "custom.el"
                                              user-emacs-directory))

  (when (file-exists-p custom-file)
    (load custom-file)))

;; (eval-and-compile
;;   (add-hook 'emacs-startup-hook '(lambda ()
;;                                    (setq gc-cons-threshold 16777216
;;                                          gc-cons-percentage 0.1
;;                                          file-name-handler-alist
;;                                          temp--file-name-handler-alist))))

(setq initial-scratch-message (concat "Startup time: " (emacs-init-time)))

(provide 'init)
;;; init ends here
