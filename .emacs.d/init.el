(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
;;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)

(package-initialize)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-save-file-name-transforms '((".*" "~/.emacs.d/autosaves/\\1" t)))
 '(backup-directory-alist '((".*" . "~/.emacs.d/backups/")))
 '(custom-enabled-themes '(tango-dark))
 '(package-selected-packages
   '(company-shell multiple-cursors projectile use-package yasnippet flycheck)))

;; =================
;; ;; Put autosave files (ie #foo#) and backup files (ie foo~) in ~/.emacs.d/.


;; create the autosave dir if necessary, since emacs won't.
(make-directory "~/.emacs.d/autosaves/" t)


;; flycheck
;; ================

(use-package flycheck
  :ensure t)

 ;; ====================
 ;; CUSTOMIZACION BASICA
 ;; ====================
 (setq inhibit-startup-message t)
 (global-display-line-numbers-mode)

;; =================
;; Autocompletado Shell
;; =================
(use-package company-shell
  :ensure t)

(add-hook 'after-init-hook 'global-company-mode)

(add-to-list 'company-backends 'company-shell)

;; ====================
;; YASNIPPET
;; ===================
(use-package yasnippet
  :ensure t 
  )


;; =====================
;; PROJECTILE
;; ====================

(use-package projectile
  :ensure t
  :defer t)
;; ================================
;; MULTIPLE CURSORS
;; ================================
(use-package multiple-cursors
  :ensure t
  )

(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
