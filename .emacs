;; Remove cl package warning (for now)
(setq byte-compile-warnings '(cl-functions))

;; melpa
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl
    (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  ;;(add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  (add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))

(package-initialize)

;; refresh packages if no cache
(when (not package-archive-contents)
  (package-refresh-contents))

;; function for ensuring packages are installed
(defun ensure-package-installed (&rest packages)
  "Assure package installed, install if not."
  (mapcar
   (lambda (package)
     (unless (package-installed-p package)
       (package-install package)))
     packages)
  )

;; packages to install here:
(ensure-package-installed
 'evil
 'auto-complete
 'magit
 'dracula-theme
 'markdown-mode
 'terraform-mode
 'dockerfile-mode
 'go-mode
 'yaml-mode
 'anaconda-mode
 'csharp-mode
 'company
 'lsp-mode
 'lsp-ui
 'lsp-treemacs
)

;; default font
(set-frame-font "Monospace 14" nil t)

;; tab length
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)

;; line numbers
(global-linum-mode)

;; hide toolbar
(tool-bar-mode -1)

;; hide menubar
(menu-bar-mode -1)

;; allow ctrl-v/c/x in mark mode
;;(cua-mode t)

;; display current column
(setq column-number-mode t)

;; hide startup screen
(setq inhibit-startup-screen t)

;; flash screen for bell
(setq visible-bell t)

;; set term shell
(setq explicit-shell-file-name (getenv "SHELL"))

;; y-or-n vs yes-or-no
(defalias 'yes-or-no-p 'y-or-n-p)

;; always add newline
(setq require-final-newline t)

;; autocomplete always on
(global-auto-complete-mode t)

;; evil mode
(require 'evil)
(evil-mode 1)

;; Magit branch prompting DWIM
(setq magit-branch-read-upstream-first 'fallback)

;; auto-start omnisharp server
;(add-hook 'csharp-mode-hook #'omnisharp-mode)
;; auto-start omnisharp flycheck
;(add-hook 'csharp-mode-hook #'flycheck-mode)
;; auto-completion for omnisharp
;(eval-after-load
; 'company
; '(add-to-list 'company-backends #'company-omnisharp))

;(add-hook 'csharp-mode-hook #'company-mode)

(require 'lsp-mode)
(add-hook 'prog-mode-hook #'lsp)

;; make whitespaces chars a little more sensible
(setq whitespace-display-mappings
      '(
        (space-mark 32 [183] [46])
        (newline-mark 10 [182 10])
        (tab-mark 9 [9655 9] [92 9])
        ))

;; re-use dired buffer
(require 'dired)
(define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file)
;; originally dired-advertised-find-file
(define-key dired-mode-map (kbd "^") (lambda () (interactive) (find-alternate-file "..")))
;; originally dired-up-directory

;; ido-mode
(require 'ido)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

;; use ido ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; backups in dedicated directory
(setq backup-by-copying t
      backup-directory-alist `(("." . ,(concat user-emacs-directory "backups")))
      tramp-backup-directory-alist backup-directory-alist
      delete-old-versions t
      kept-new-versions 3
      kept-old-versions 2
      version-control t
      vs-cvs-stay-local nil)

;; function to bypass term prompt
;(defun t ()
;  "Open term directly."
;  (interactive)
;  (set-buffer (make-term "terminal" explicit-shell-file-name))
;  (term-mode)
;  (term-char-mode)
;  (switch-to-buffer "*terminal*"))

;; function to switch horizontal/vertical split
(defun window-split-toggle ()
  "Toggle between horizontal and vertical split with two windows."
  (interactive)
  (if (> (length (window-list)) 2)
      (error "Can't toggle with more than 2 windows!")
    (let ((func (if (window-full-height-p)
		    #'split-window-vertically
		  #'split-window-horizontally)))
      (delete-other-windows)
      (funcall func)
      (save-selected-window
	(other-window 1)
	(switch-to-buffer (other-buffer))))))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#212526" "#ff4b4b" "#b4fa70" "#fce94f" "#729fcf" "#e090d7" "#8cc4ff" "#eeeeec"])
 '(custom-enabled-themes '(dracula))
 '(custom-safe-themes
   '("fe1c13d75398b1c8fd7fdd1241a55c286b86c3e4ce513c4292d01383de152cb7" default))
 '(inhibit-startup-screen t)
 '(package-selected-packages
   '(auto-complete ergoemacs-mode markdown-mode evil terraform-mode dockerfile-mode dracula-theme)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'dired-find-alternate-file 'disabled nil)
