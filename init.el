;; Add the NonGNU ELPA package archive
(require 'package)
(add-to-list 'package-archives  '("nongnu" . "https://elpa.nongnu.org/nongnu/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)
(unless package-archive-contents  (package-refresh-contents))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(vterm flycheck-languagetool multiple-cursors dape embark consult terraform-ts-mode quelpa-use-package quelpa lsp-ui lsp-mode terraform-mode avy markdown-mode diff-hl magit corfu company company-mode marginalia vertico use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


;; Load tango Dark theme
(load-theme 'tango-dark t)

;; disable menu bar, tool bar and splash screen
(menu-bar-mode -1)
(tool-bar-mode -1)
(setq inhibit-startup-screen t)

;; Always show lineNumbers
(global-display-line-numbers-mode t )

;; Autoclose parenthesis
(electric-pair-mode t)

;; Disable creating lock files
(setq create-lockfiles nil)

;; Temporary files directory
 (setq backup-directory-alist
        '(("." . "~/.saves/"))) ;; remove the star
 (setq auto-save-file-name-transforms
        `((".*" ,"~/.saves/" t)))

;; Vertico Completino Vertical in miniBuffer
(use-package vertico
  :ensure t
  :custom
  (vertico-cycle t) ;; go to the start when past the end
  :init
  (vertico-mode))
;; built-in Save select history of miniBuffer
(use-package savehist
  :init
  (savehist-mode))

;; Extra info in vertico options
(use-package marginalia
  :after vertico
  :ensure t
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :init
  (marginalia-mode))

;;Enabling syntax check
(add-hook 'prog-mode-hook #'flymake-mode)

;; Auto-completion with corfu
(use-package corfu
  :ensure t
  :custom
  (corfu-auto t)
  :init
  (global-corfu-mode)
  )

;; git integration
(use-package magit
  :ensure t
  :init
  (global-set-key (kbd "C-c g")#'magit-status)
  )

;;highlights non commited changes
(use-package diff-hl
  :ensure t
  )

(global-diff-hl-mode)


;;YAML support
(use-package yaml-mode
  :ensure t)

;;MD support
(use-package markdown-mode
  :ensure t)

;;Jump to different buffer point
(use-package avy
  :ensure t)
(global-set-key (kbd "C-c z") #'avy-goto-word-1)
(global-set-key (kbd "C-c <") #'avy-goto-line)


;;Recent open files
(recentf-mode t)
;: Open recent files with C-x C-r
(global-set-key (kbd "C-x C-r") #'recentf-open)
(use-package eglot

  )
;;Enable treesit syntax highlight
;; https://gist.github.com/habamax/290cda0e0cdc6118eb9a06121b9bc0d7
;; enable treesitter for hihgliht
(setq major-mode-remap-alist
      '((python-mode . python-ts-mode)))
;;Start eglot
(add-hook 'python-mode-hook 'eglot-ensure)

;;move cursor to windows using shift + arrows
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

;;Terraform. Install terraform-ls following https://www.hashicorp.com/official-packaging-guide
;; (use-package terraform-mode
;;   :ensure t
;;   :custom (terraform-indent-level 4)

;;   )

;;
(use-package quelpa
  :ensure t
  )


(quelpa
 '(terraform-ts-mode
   :fetcher git
   :url "https://github.com/kgrotel/terraform-ts-mode.git"))



;; Configure flymakes hotkeys
(require 'flymake)
(global-set-key (kbd "C-x n") #'flymake-goto-next-error)
(global-set-key (kbd "C-x p") #'flymake-goto-prev-error)

(use-package consult
  :ensure t
  :bind (
  ("M-y" . 'consult-yank-from-kill-ring)
  ("C-x b" . 'consult-buffer)))
;;(use-package embark
;;  :ensure t
;;  )
;;needed package python3-debugpy
(quelpa
 '(dape
   :fetcher git
   :url "https://github.com/svaante/dape.git"))
(use-package multiple-cursors
  :ensure t
  :bind (("C-S-c C-S-c" . 'mc/edit-lines)
	 ("C->" . 'mc/mark-next-like-this)
	 ("C-<" . 'mc/mark-previous-like-this)
	 ("C-c C->" . 'mc/mark-all-like-this)
	 ("C-S-n C-S-n" . 'mc/mark-next-lines)))


(use-package jinx
  :ensure t 
  :bind (("M-$" . jinx-correct)
         ("C-M-$" . jinx-languages)))
(dolist (hook '(org-mode-hook text-mode-hook))
  (add-hook hook #'jinx-mode))

(require 'terraform-ts-mode)

;;VTerm package. It needs libtool-bin and cmake installed in the system.

(use-package vterm
  :ensure t)

(use-package tide
  :ensure t
  :after (company flycheck)
  :hook ((typescript-ts-mode . tide-setup)
         (tsx-ts-mode . tide-setup)
         (typescript-ts-mode . tide-hl-identifier-mode)
         (before-save . tide-format-before-save)))


;;; init.el ends here
