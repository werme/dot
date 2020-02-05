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


;; (setq package-enable-at-startup nil)
(let ((default-directory "~/.emacs.d/elpa"))
  (normal-top-level-add-subdirs-to-load-path))
(package-initialize)

(unless (package-installed-p 'use-package) ; unless it is already installed
  (package-refresh-contents) ; updage packages archive
  (package-install 'use-package)) ; and install the most recent version of use-package

(eval-when-compile
  (require 'use-package)
  (setq use-package-always-ensure t))

(use-package package
  :config
  (setq package-check-signature nil)
  (setq package-enable-at-startup nil)
  (setq package-archives '(("gnu"          . "https://elpa.gnu.org/packages/")
                           ("melpa"        . "https://melpa.org/packages/")
                           ("melpa-stable" . "https://stable.melpa.org/packages/")
                           ("repo-org"     . "https://orgmode.org/elpa/"))))

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

(use-package suggest
  :general (tyrant-def "as" 'suggest))

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
                                        ; "fd"  'evil-save-and-close)
                                        ; ('motion override-global-map
                                        ;   "]b"  'evil-next-buffer
                                        ;   "[b"  'evil-prev-buffer))

                                        ; (use-package evil-numbers
                                        ;   :after evil
                                        ;   :general
                                        ;   ('normal "C-=" 'evil-numbers/inc-at-pt
                                        ;            "C--" 'evil-numbers/dec-at-pt))

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
  (setq company-minimum-prefix-length 2)
  (setq company-idle-delay 0)
  (setq company-dabbrev-downcase nil)
  (setq company-frontends '(company-echo-metadata-frontend
                            company-pseudo-tooltip-unless-just-one-frontend
                            company-preview-frontend))
  (setq company-backends '((company-capf
                            company-files)
                           (company-dabbrev-code company-keywords)
                           company-dabbrev company-yasnippet)))

;; (use-package company-quickhelp
;;   :defer 5
;;   :config (company-quickhelp-mode))

;; (use-package company-statistics
;;   :defer 5
;;   :config (company-statistics-mode))

(set-face-attribute 'hl-line nil :background "#3f3f3f")

(use-package flycheck
  :commands (flycheck-mode)
  :config
  ;; (setq flycheck-highlighting-mode 'lines)
  (setq flycheck-check-syntax-automatically '(save idle-change mode-enabled))
  ;; (set-face-attribute 'flycheck-error nil :background "red")
  ;; (set-face-attribute 'flycheck-warning nil :background "yellow")
  (global-flycheck-mode)
  :general
  (tyrant-def
    "e"  '(:ignore t :which-key "Errors")
    "]"  'flycheck-next-error
    "["  'flycheck-previous-error
    "en" 'flycheck-next-error
    "ep" 'flycheck-previous-error
    "ee" 'counsel-flycheck
    "bc" 'flycheck-buffer))

(use-package lsp-mode
  :hook (js-mode . lsp)
  :commands lsp
  :config
  (setq lsp-prefer-flymake nil))

(use-package lsp-ui
  :commands lsp-ui-mode
  :config
  (flycheck-add-next-checker 'lsp-ui 'javascript-eslint)
  (setq lsp-ui-doc-enable nil
        lsp-ui-peek-enable nil
        lsp-ui-sideline-enable nil
        lsp-ui-imenu-enable nil
        lsp-ui-flycheck-enable t))

(use-package company-lsp
  :after company
  :commands company-lsp
  :config (add-to-list 'company-backends 'company-lsp))

(use-package swiper
  :commands swiper
  :general
  (tyrant-def
    "s" 'swiper))

(use-package counsel
  ;; :commands (counsel-load-theme
  ;;            counsel-bookmark)
  ;; :bind* (("C-c i" . counsel-imenu)
  ;;         ("C-x C-f" . counsel-find-file)
  ;;         ("C-x C-b" . ivy-switch-buffer)
  ;;         ("C-c C-/" . counsel-rg)
  ;;         ("s-<backspace>" . ivy-switch-buffer)
  ;;         ("M-x" . counsel-M-x))
  ;; :config
  ;; (setq counsel-locate-cmd 'counsel-locate-cmd-mdfind)
  ;; (setq counsel-find-file-at-point t)
  :general
  (tyrant-def
    "SPC" 'counsel-M-x
    "bm"  'ivy-switch-buffer
    "ff"  'counsel-find-file
    "fr"  'counsel-recentf))
;; "fL"  'helm-locate))

(use-package ivy
  :after (counsel swiper)
  :custom
  (ivy-display-style 'fancy)
  ;; (ivy-count-format "(%d/%d) ")
  (ivy-use-virtual-buffers t)
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-initial-inputs-alist nil)
  (setq ivy-re-builders-alist
        '((swiper . ivy--regex-plus)
          (t      . ivy--regex-ignore-order))))
;; (setq ivy-display-function nil))


(use-package counsel-projectile)
;; :after (projectile)
;; :hook projectile-mode)

(use-package projectile
  :commands (projectile-mode)
  :general
  (tyrant-def
    "p"     '(:ignore t :which-key "projectile")
    "p SPC" 'counsel-projectile
    "pd"    'counsel-projectile-find-dir
    "ps"    'counsel-projectile-switch-project
    "pf"    'counsel-projectile-find-file
    "pg"    'counsel-projectile-rg
    "pb"    'counsel-projectile-switch-to-buffer
    "j"     'counsel-projectile-find-file)
  :init
  (setq projectile-project-search-path '("~/dev/" "~/kry/code/" "~/.config/"))
  :config
  (projectile-mode 1)
  (setq projectile-switch-project-action 'projectile-dired)
  (setq projectile-completion-system 'ivy))

;; (use-package helm-flyspell
;;   :commands (helm-flyspell-correct))
;;   ;; :general
;;   ;;  (:keymaps '(flyspell-mode-map)
;;   ;;   :states '(normal visual)
;;   ;;   "zs" 'helm-flyspell-correct
;;   ;;   "z=" 'flyspell-buffer))

(use-package exec-path-from-shell
  :init (exec-path-from-shell-initialize))

(use-package add-node-modules-path)

(eval-after-load 'js-mode
  '(add-hook 'js-mode-hook #'add-node-modules-path))

(use-package prettier-js
  :hook (js-mode . prettier-js-mode))

(use-package magit
  :commands (magit-status)
  :general
  (tyrant-def
    "g"   '(:ignore t :which-key "git")
    "gs"  'magit-status
    "gg"  'magit-status))

(use-package evil-magit
  :hook (magit-mode . evil-magit-init))

(use-package yasnippet)

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

(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

(defun my-prog-mode-hook ()
  (auto-fill-mode)
  (show-paren-mode)
  (whitespace-mode)
  (electric-pair-mode)
  (yas-global-mode 1)
  (flycheck-mode)
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
