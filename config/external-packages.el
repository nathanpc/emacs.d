;; ~/.emacs.d/config/external-packages.el
;; Manages all of our external packages to improve our life.
;;
;; Author: Nathan Campos <nathan@innoveworkshop.com>

; Initialize Emacs's built-in package manager.
(require 'package)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

; Setup use-package.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-and-compile
  (setq use-package-always-ensure t
        use-package-expand-minimally t))
(eval-when-compile
  (require 'use-package))

;;
;; Theming and User Interface
;;

; Nord theme (font face)
(use-package nord-theme
  :config (load-theme 'nord t))

; All the Icons
(use-package all-the-icons
  :if (display-graphic-p))

; DOOM Modeline
(use-package doom-modeline
  :init (doom-modeline-mode t)
  :hook (text-mode-hook . show-word-count-doom-modeline)
  :config
  (setq doom-modeline-buffer-file-name-style
		'relative-from-project)                  ; Show path relative to project.
  (setq doom-modeline-indent-info t)             ; Show indentation information.
  (setq doom-modeline-minor-modes
		(featurep 'minions))                     ; Display minor modes.
  (defun show-word-count-doom-modeline ()
	(setq doom-modeline-enable-word-count t)))   ; Display word count text mode.

; Dashboard
(use-package dashboard
  :config
  (setq dashboard-startup-banner 'logo)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-set-navigator t)
  (setq dashboard-items '((recents  . 15)
                          (bookmarks . 8)
                          (registers . 8)))
  (dashboard-setup-startup-hook))

; Dimmer
(use-package dimmer
  :init (dimmer-mode)
  :config
  (setq dimmer-fraction 0.35))  ; Make other buffers dimmer.

;;
;; Indentation, Fringe, and other Editor Aesthetics
;;

; Highlight Escape Sequences
(use-package highlight-escape-sequences
  :hook (prog-mode-hook . hes-mode))

; Highlight Numbers
(use-package highlight-numbers
  :hook (prog-mode-hook . highlight-numbers-mode))

; Rainbow Mode
(use-package rainbow-mode
  :init
  (dolist (hook '(css-mode-hook html-mode-hook sass-mode-hook))
	(add-hook hook 'rainbow-mode)))

; Rainbow Delimiters
(use-package rainbow-delimiters
  :hook (prog-mode-hook . rainbow-delimiters-mode))

; Highlight Indent Guides
(use-package highlight-indent-guides
  :config
  (setq highlight-indent-guides-method 'character)           ; Thin line style.
  :init
  (add-hook 'prog-mode-hook 'highlight-indent-guides-mode)   ; Show guides in code.
  (add-hook 'text-mode-hook 'highlight-indent-guides-mode))  ; Show guides in text.

; Git Gutter Fringe
(use-package git-gutter-fringe
  :init (global-git-gutter-mode t))

;;
;; Auto-Completion and Snippets
;;

; Company Mode
(use-package company
  :init (global-company-mode)
  :config
  (setq company-idle-delay 0))  ; Auto-complete instantly.

;;
;; Programming Languages
;;

; Markdown
(use-package markdown-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode)))

; Improved JavaScript Mode
(use-package js2-mode
  :mode "\\.js$"
  :hook (js2-mode-hook . js2-imenu-extras-mode)
  :config
  (setq-default
   js2-auto-indent-p t
   js2-cleanup-whitespace t
   js2-enter-indents-newline t
   js2-indent-on-enter-key t
   js2-global-externs (list "window" "module" "require" "buster" "sinon" "assert"
							"refute" "setTimeout" "clearTimeout" "setInterval"
							"location" "clearInterval" "__dirname" "console"
							"JSON" "jQuery" "$")))

; Better JavaScript Code Jumps
(use-package xref-js2
  :init
  (add-hook 'js2-mode-hook
			(lambda () (add-hook 'xref-backend-functions
								 #'xref-js2-xref-backend nil t))))

;;
;; Miscellaneous
;;

;; Man completion.
(require 'man-completion)

(provide 'external-packages)
