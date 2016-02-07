(setq enable-local-variables :all)

(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/")
	     t)

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

(defun set-exec-path-from-shell-PATH ()
  (let ((path-from-shell 
      (replace-regexp-in-string "[[:space:]\n]*$" "" 
        (shell-command-to-string "$SHELL -l -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))
(when (equal system-type 'darwin) (set-exec-path-from-shell-PATH))

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

(require 'cl)

(defun* get-closest-pathname (&optional (file "Makefile"))
  "Determine the pathname of the first instance of FILE starting from the current directory towards root.
This may not do the correct thing in presence of links. If it does not find FILE, then it shall return the name
of FILE in the current directory, suitable for creation"
  (let ((root (expand-file-name "/"))) ; the win32 builds should translate this correctly
    (loop 
     for d = default-directory then (expand-file-name ".." d)
     if (file-exists-p (expand-file-name file d))
     return d
     if (equal d root)
     return nil)))

(defun scribble-mode ()
  (text-mode)
  (defun do-compile ()
    (interactive nil)
    (set (make-local-variable 'compile-command) (format "cd %s && make" (get-closest-pathname))
;	 (format "make -f %s" (expand-file-name "Makefile" (get-closest-pathname)))
	 )
    (setq compilation-read-command nil)
    (let ((default-directory (get-closest-pathname)))
      (call-interactively 'compile)))
  (local-set-key (kbd "C-c C-c") 'do-compile))

(add-to-list 'auto-mode-alist '("[.]scrbl" . scribble-mode))

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

(add-to-list 'auto-mode-alist '("[.]rkt$" . racket-mode))

;; Paredit all the things
(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook           #'enable-paredit-mode)
(add-hook 'racket-mode-hook           #'enable-paredit-mode)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 ;; '(exec-path
 ;;   (quote
 ;;    ("/usr/bin" "/bin" "/usr/sbin" "/sbin" "/usr/local/Cellar/emacs/24.4/libexec/emacs/24.4/x86_64-apple-darwin14.0.0" "/usr/local/bin" "/Users/sdmoore/Documents/racket/racket/bin")))
 '(inhibit-startup-screen t)
 '(ispell-program-name "aspell")
 '(racket-racket-program "/Users/sdmoore/Documents/racket/racket/bin/racket")
 '(racket-raco-program "/Users/sdmoore/Documents/racket/racket/bin/raco"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

; Spelling
(add-hook 'tex-mode-hook 'flyspell-mode)
