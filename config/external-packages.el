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
  :init (doom-modeline-mode t))

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

;;
;; Miscellaneous
;;

;; Man completion.
(require 'man-completion)

(provide 'external-packages)
