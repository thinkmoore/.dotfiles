(setq enable-local-variables :all)

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))

;; themes
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

(add-hook 'after-init-hook 'my-after-init-hook)
(defun my-after-init-hook ()
  (load-theme 'think-cyberpunk t)
  (set-default-font "Monospace-10"))

;; whitespace
(require 'whitespace)
(setq whitespace-style '(face empty tabs lines-tail trailing))

(setq-default TeX-master nil)
(setq TeX-parse-self t)
(setq TeX-auto-save t)
(setenv "PATH" (concat "/usr/texbin" ":" (getenv "PATH")))

;; Turn off the GUI
(when (boundp 'aquamacs-version)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (fringe-mode -1)
  (tabbar-mode 0))

(tool-bar-mode -1)
(scroll-bar-mode -1)
(fringe-mode -1)

;; Space bar auto-complete for file names
(define-key minibuffer-local-filename-completion-map
	      " " 'minibuffer-complete-word)

;; C-x C-<left> ;; C-x C-<right>
 (global-set-key (kbd "C-x C-<right>") 'tabbar-forward-group)
 (global-set-key (kbd "C-x C-<left>") 'tabbar-backward-group)

;; fast window switching
(defun select-next-window ()
  "Switch to the next window"
  (interactive)
  (select-window (next-window)))

(defun select-previous-window ()
  "Switch to the previous window"
  (interactive)
  (select-window (previous-window)))

(global-set-key (kbd "M-<right>") 'select-next-window)
(global-set-key (kbd "M-<left>")  'select-previous-window)

;; fast goto line
(global-set-key (kbd "M-l") 'goto-line)

;; backups in less annoying places
(setq backup-directory-alist `(("." . "~/.saves")))
(setq backup-by-copying-when-linked t)
(setq auto-save-file-name-transforms '((".*" "~/.saves/\\1" t)))

;; latex
(add-hook 'LaTeX-mode-hook
	  '(lambda()
	     (auto-fill-mode t)
	     (local-set-key (kbd "<f6>") (kbd "C-x C-s C-c C-c C-j"))))

(add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)
(setq TeX-source-correlate-method 'synctex)

;; (add-hook 'LaTeX-mode-hook
;;       (lambda()
;;         (add-to-list 'TeX-expand-list
;;              '("%q" skim-make-url))))

;; (defun skim-make-url () (concat
;;         (TeX-current-line)
;;         " "
;;         (expand-file-name (funcall file (TeX-output-extension) t)
;;             (file-name-directory (TeX-master-file)))
;;         " "
;;         (buffer-file-name)))

;; (setq TeX-view-program-list
;;       '(("Skim" "/Applications/Skim.app/Contents/SharedSupport/displayline %q")))

;; (setq TeX-view-program-selection '((output-pdf "Skim")))

;; jif files should open in java-mode
(add-to-list 'auto-mode-alist '("[.]jif$" . java-mode))
(add-to-list 'auto-mode-alist '("[.]jl5$" . java-mode))
(add-to-list 'auto-mode-alist '("[.]jl$" . java-mode))
(add-to-list 'auto-mode-alist '("[.]soup$" . java-mode))

;; Prolog files should open in prolog-mode
(add-to-list 'auto-mode-alist '("[.]pl$" . prolog-mode))

;; Coq
(load-file "~/.emacs.d/ProofGeneral/generic/proof-site.el")
(add-hook 'coq-mode-hook '(lambda()
			    (local-unset-key (kbd "C-c ."))
			    (local-set-key (kbd "C-c .") 'proof-goto-point)))

;; Paredit all the things
(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook           #'enable-paredit-mode)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(inhibit-startup-screen t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
