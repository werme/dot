;; -*- lexical-binding: t; -*-
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

;; (eval-and-compile
;;   (setq gc-cons-threshold 402653184
;;         gc-cons-percentage 0.6))

;; (defvar temp--file-name-handler-alist file-name-handler-alist)
;; (setq file-name-handler-alist nil)

(setq backup-directory-alist `(("." . "~/.emacs.d/backups")) ; which directory to put backups file
      auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)) ; transform backups file name
      fill-column 80 ; toggle wrapping text at the 80th character
      scroll-conservatively 101
      ispell-program-name "aspell")

(with-eval-after-load 'display-line-numbers
  (setq display-line-numbers-type 'relative
        display-line-numbers-width-start t))

(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(setq-default indent-tabs-mode nil)
(global-hl-line-mode 1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(blink-cursor-mode 0)
(winner-mode 1)
(put 'narrow-to-region 'disabled nil)

;;;We’re going to set the load-path ourselves and avoid calling (package-initilize) (for performance reasons) so we need to set package--init-file-ensured to true to tell package.el to not automatically call it on our behalf. Additionally we’re setting package-enable-at-startup to nil so that packages will not automatically be loaded for us since use-package will be handling that.
; (eval-and-compile
;   (setq load-prefer-newer t
;         package-user-dir "~/.emacs.d/elpa"
;         package--init-file-ensured t
;         package-enable-at-startup nil)

;   (unless (file-directory-p package-user-dir)
;     (make-directory package-user-dir t))

;   (setq load-path (append load-path (directory-files package-user-dir t "^[^.]" t))))

(eval-when-compile
  (require 'package)
  ;; tells emacs not to load any packages before starting up
  ;; the following lines tell emacs where on the internet to look up
  ;; for new packages.
  (setq package-archives '(("gnu"          . "https://elpa.gnu.org/packages/")
                           ("melpa"        . "https://melpa.org/packages/")
                           ("melpa-stable" . "https://stable.melpa.org/packages/")
                           ("repo-org"     . "https://orgmode.org/elpa/")))
  (package-initialize)
  ; (unless package--initialized (package-initialize t))

  ;; Bootstrap `use-package'
  (unless (package-installed-p 'use-package) ; unless it is already installed
    (package-refresh-contents) ; updage packages archive
    (package-install 'use-package)) ; and install the most recent version of use-package

  (require 'use-package)
  (setq use-package-always-ensure t))

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

;  ;;Taken from http://emacsredux.com/blog/2013/05/04/rename-file-and-buffer/
;  (defun rename-file-and-buffer ()
;    "Rename the current buffer and file it is visiting."
;    (interactive)
;    (let ((filename (buffer-file-name)))
;      (if (not (and filename (file-exists-p filename)))
;          (message "Buffer is not visiting a file!")
;        (let ((new-name (read-file-name "New name: " filename)))
;          (cond
;           ((vc-backend filename) (vc-rename-file filename new-name))
;           (t
;            (rename-file filename new-name t)
;            (set-visited-file-name new-name t t)))))))

 (general-create-definer tyrant-def
   :states '(normal visual insert motion emacs)
   :prefix "SPC"
   :non-normal-prefix "C-SPC")

;  (general-create-definer despot-def
;    :states '(normal insert)
;    :prefix "SPC"
;    :non-normal-prefix "C-SPC")

 (general-define-key
   :keymaps 'key-translation-map
   "ESC" (kbd "C-g"))

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
;    "qz"  'delete-frame

   ;; Buffer operations
   "b"   '(:ignore t :which-key "buffer")
   "bb"  'mode-line-other-buffer
   "bd"  'kill-this-buffer
;    "b]"  'next-buffer
;    "b["  'previous-buffer
   "bq"  'kill-buffer-and-window
;    "bR"  'rename-file-and-buffer
;    "br"  'revert-buffer

   ;; Window operations
   "w"   '(:ignore t :which-key "window")
   "wo"  'maximize-window
   "wx"  'split-window-horizontally
   "ws"  'split-window-vertically
;    "wu"  'winner-undo
;    "ww"  'other-window
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
;    "fR"  'rename-file-and-buffer
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
   "C-b" 'doc-view-scroll-down-or-previous-page)

;  (general-def '(normal visual) outline-minor-mode-map
;    "zn"  'outline-next-visible-heading
;    "zp"  'outline-previous-visible-heading
;    "zf"  'outline-forward-same-level
;    "zB"  'outline-backward-same-level)

 (general-def 'normal package-menu-mode-map
   ; "i"   'package-menu-mark-install
   ; "U"   'package-menu-mark-upgrades
   ; "d"   'package-menu-mark-delete
   ; "u"   'package-menu-mark-unmark
   ; "x"   'package-menu-execute
   "q"   'quit-window))

;  (general-def 'normal calendar-mode-map
;    "h"   'calendar-backward-day
;    "j"   'calendar-forward-week
;    "k"   'calendar-backward-week
;    "l"   'calendar-forward-day
;    "0"   'calendar-beginning-of-week
;    "^"   'calendar-beginning-of-week
;    "$"   'calendar-end-of-week
;    "["   'calendar-backward-year
;    "]"   'calendar-forward-year
;    "("   'calendar-beginning-of-month
;    ")"   'calendar-end-of-month
;    "SPC" 'scroll-other-window
;    "S-SPC" 'scroll-other-window-down
;    "<delete>" 'scroll-other-window-down
;    "<"   'calendar-scroll-right
;    ">"   'calendar-scroll-left
;    "C-b" 'calendar-scroll-right-three-months
;    "C-f" 'calendar-scroll-left-three-months
;    "{"   'calendar-backward-month
;    "}"   'calendar-forward-month
;    "C-k" 'calendar-backward-month
;    "C-j" 'calendar-forward-month
;    "gk"  'calendar-backward-month
;    "gj"  'calendar-forward-month
;    "v"   'calendar-set-mark
;    "."   'calendar-goto-today
;    "q"   'calendar-exit))

(use-package suggest
  :general (tyrant-def "as" 'suggest))

;; (use-package ranger
;;   :hook (after-init . ranger-override-dired-mode)
;;   :general (tyrant-def "ar" 'ranger))

(use-package evil
  :hook (after-init . evil-mode)
  :config
  (evil-set-initial-state 'shell-mode 'normal)
  (evil-set-initial-state 'doc-view-mode 'normal)
  (evil-set-initial-state 'package-menu-mode 'normal)
  (evil-set-initial-state 'biblio-selection-mode 'motion)
  (setq doc-view-continuous t)
  :general
  (tyrant-def
    "wh"  'evil-window-left
    "wl"  'evil-window-right
    "wj"  'evil-window-down
    "wk"  'evil-window-up))
    ; "bN"  'evil-buffer-new
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

(use-package evil-easymotion
  :after evil
  :config (evilem-default-keybindings "gs"))

(use-package evil-commentary
  :after evil
  :config (evil-commentary-mode 1))

(use-package evil-visualstar
  :after evil
  :config
  (setq evilmi-always-simple-jump t)
  (global-evil-visualstar-mode 1))

(use-package company
  :hook (after-init . global-company-mode)
  :config
  (define-key company-active-map (kbd "M-n") nil)
  (define-key company-active-map (kbd "M-p") nil)
  (define-key company-active-map (kbd "C-n") #'company-select-next-or-abort)
  (define-key company-active-map (kbd "C-p") #'company-select-previous-or-abort)
  (setq company-frontends '(company-echo-metadata-frontend
                            company-pseudo-tooltip-unless-just-one-frontend
                            company-preview-frontend))
  (setq company-backends '((company-capf
                            company-files)
                           (company-dabbrev-code company-keywords)
                            company-dabbrev company-yasnippet)))

(use-package company-quickhelp
  :defer 5
  :config (company-quickhelp-mode))

(use-package company-statistics
  :defer 5
  :config (company-statistics-mode))

(use-package lsp-mode
  :hook (javascript-mode . lsp)
  :commands lsp)

(use-package lsp-ui
  :hook (javascript-mode . lsp-ui-mode)
  :commands lsp-ui-mode)

(use-package company-lsp
  :after company
  :commands company-lsp
  :config (add-to-list 'company-backends 'company-lsp))

(use-package helm-lsp :commands helm-lsp-workspace-symbol)
;; (use-package lsp-treemacs :commands lsp-treemacs-errors-list)
;; optionally if you want to use debugger
;; (use-package dap-mode)
;; (use-package dap-LANGUAGE) to load the dap adapter for your language

(use-package helm
  :hook (after-init . helm-mode)
  :config (require 'helm-config)
  :commands (helm-mini
             helm-find-files
             helm-recentf
             helm-locate
             helm-M-x
             helm-flyspell-correct)
  :general
  (tyrant-def
   "SPC" 'helm-M-x
   "bm"  'helm-mini
   "ff"  'helm-find-files
   "fr"  'helm-recentf
   "fL"  'helm-locate))

(use-package helm-flyspell
  :commands (helm-flyspell-correct))
  ;; :general
  ;;  (:keymaps '(flyspell-mode-map)
  ;;   :states '(normal visual)
  ;;   "zs" 'helm-flyspell-correct
  ;;   "z=" 'flyspell-buffer))

(use-package helm-projectile
  :after (projectile helm)
  :general
  (tyrant-def
   "p"   '(:ignore t :which-key "projectile")
   "pd"  'helm-projectile-dired-find-dir
   "po"  'helm-projectile-find-other-file
   "pf"  'helm-projectile-find-file
   "pb"  'helm-projectile-switch-to-buffer))

(use-package flycheck
  :commands (flycheck-mode)
  :general
  (tyrant-def
   "e"   '(:ignore t :which-key "Errors")
   "en"  'flycheck-next-error
   "ep"  'flycheck-previous-error))

(use-package magit
  :commands (magit-status)
  :general
  (tyrant-def
   "g"   '(:ignore t :which-key "git")
   "gs"  'magit-status))

(use-package evil-magit
  :hook (magit-mode . evil-magit-init))

; (use-package yasnippet
;   :hook ((prog-mode org-mode) . yas-minor-mode)
;   :general
;   (tyrant-def
;    "y"   '(:ignore t :which-key "yasnippet")
;    "yi"  'yas-insert-snippet
;    "yv"  'yas-visit-snippet-file
;    "yn"  'yas-new-snippet))

; (use-package yasnippet-snippets
;   :after yasnippet)

; (use-package org
;   :defer t
;   :mode ("\\.org\\'" . org-mode)
;   :ensure org-plus-contrib
;   :init
;   (defun my-org-mode-hooks ()
;     (visual-line-mode)
;     (display-line-numbers-mode t)
;     (flyspell-mode)
;     (outline-minor-mode)
;     (electric-pair-mode))
;   (add-hook 'org-mode-hook 'my-org-mode-hooks)
;   :general
;   (despot-def org-mode-map
;     "me"   'org-export-dispatch
;     "mt"   'org-hide-block-toggle
;     "mx"   'org-babel-execute-src-block
;     "mX"   'org-babel-execute-and-next
;     "md"   'org-babel-remove-result)
;   :config
;   (if (not (featurep 'ox-bibtex))
;       (require 'ox-bibtex))
;   (defun org-babel-execute-and-next ()
;     (interactive)
;     (progn (org-babel-execute-src-block)
;            (org-babel-next-src-block)))
;   (setq org-highlight-latex-and-related '(entities script latex)
;         org-tags-column 90)
;   (add-to-list 'org-structure-template-alist
;                '("<ip" "#+BEGIN_SRC ipython :session ? :results raw
;   drawer\n\n#+END_SRC"
;                  "<src lang=\"?\">\n\n</src>")))

; (use-package org-bullets
;   :hook (org-mode . org-bullets-mode))

; (use-package org-pomodoro
;   :general
;   (despot-def org-mode-map
;    "mps"  'org-pomodoro))

; (use-package ox-reveal
;   :hook (org-mode . load-org-reveal)
;   :config
;   (defun load-org-reveal ()
;     (if (not (featurep 'ox-reveal))
;         (require 'ox-reveal))))

(use-package shell-pop
  :commands (shell-pop)
  :config (setq shell-pop-shell-type '("shell"
                                       "*shell*"
                                       (lambda nil (shell))))
  :general
  (tyrant-def "'" 'shell-pop))

(setq whitespace-style '(face trailing))

(defun my-prog-mode-hook ()
  (auto-fill-mode)
  (show-paren-mode)
  (whitespace-mode)
  (electric-pair-mode)
  (flycheck-mode)
  (display-line-numbers-mode))

(add-hook 'prog-mode-hook 'my-prog-mode-hook)
(setq before-save-hook 'nil)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(use-package telephone-line
  :config
  ;; (setq telephone-line-primary-left-separator 'telephone-line-cubed-left)
  ;;       ;; telephone-line-secondary-left-separator 'telephone-line-cubed-hollow-left
  ;;       ;; telephone-line-primary-right-separator 'telephone-line-cubed-right
  ;;       ;; telephone-line-secondary-right-separator 'telephone-line-cubed-hollow-right)
  ;; (setq telephone-line-primary-left-separator 'telephone-line-cubed-left
  ;;       telephone-line-secondary-left-separator 'telephone-line-cubed-hollow-left
  ;;       telephone-line-primary-right-separator 'telephone-line-cubed-right
  ;;       telephone-line-secondary-right-separator 'telephone-line-cubed-hollow-right)
  ;; (setq telephone-line-primary-left-separator 'telephone-line-gradient
  ;;       telephone-line-secondary-left-separator 'telephone-line-nil
  ;;       telephone-line-primary-right-separator 'telephone-line-gradient
  ;;       telephone-line-secondary-right-separator 'telephone-line-nil)
  ;; (setq telephone-line-height 24
  ;;       telephone-line-evil-use-short-tag t)

  ;; (telephone-line-defsegment my-vc-info ()
  ;; (when vc-mode
  ;; (cond
  ;; ((string-match "Git[:-]" vc-mode)
  ;; (let ((branch (mapconcat 'concat (cdr (split-string vc-mode "[:-]")) "-")))
  ;; (concat "" (format " %s" branch))))
  ;; ((string-match "SVN-" vc-mode)
  ;; (let ((revision (cadr (split-string vc-mode "-"))))
  ;; (concat "" (format "SVN-%s" revision))))
  ;; (t (format "%s" vc-mode)))))

  ;; (telephone-line-defsegment* my-airline-position-segment (&optional lines columns)
  ;;   (let* ((l (number-to-string (if lines lines 1)))
  ;;          (c (number-to-string (if columns columns 2))))
  ;;     (if (eq major-mode 'paradox-menu-mode)
  ;;         (telephone-line-raw mode-line-front-space t)
  ;;         (concat " " "%" l "l:%" c "c"))))

  ;; (setq telephone-line-lhs
  ;;       '((evil   . (telephone-line-evil-tag-segment))
  ;;         (accent . (my-vc-info
  ;;                    telephone-line-process-segment))
  ;;         (nil    . (telephone-line-buffer-segment
  ;;                    telephone-line-projectile-segment))))
  ;; (setq telephone-line-rhs
  ;;       '((nil    . (telephone-line-flycheck-segment
  ;;                    telephone-line-misc-info-segment))
  ;;         (accent . (telephone-line-major-mode-segment))
  ;;         (nil    . (telephone-line-hud-segment
  ;;                    my-airline-position-segment))))

  ;; (setq display-time-format "%b %d %a %R")
  ;; (setq display-time-default-load-average nil)
  ;; (setq display-time-use-mail-icon t)
  ;; (setq display-time-mail-file t)
  ;; (display-time-mode t)

  (telephone-line-mode 1))

; (use-package spacemacs-theme
;   :hook (after-init . load-spacemacs-dark)
;   :config
;   (defun load-spacemacs-dark ()
;     "Load the `spacemacs-dark' theme."
;     (interactive)
;     (load-theme 'spacemacs-dark)))

(load-theme 'spacemacs-dark t)

(eval-when-compile
  (setq-default custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file)))

; (eval-and-compile
; (add-hook 'emacs-startup-hook '(lambda ()
;                 (setq gc-cons-threshold 16777216
;                         gc-cons-percentage 0.1
;                         file-name-handler-alist temp--file-name-handler-alist))))
; (setq initial-scratch-message (concat "Startup time: " (emacs-init-time)))
(provide 'init)
;;; init ends here
