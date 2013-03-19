(load-theme 'wombat t)

(setq-default TeX-master nil)
(setq TeX-parse-self t)
(setq TeX-auto-save t)

;; Turn off the GUI
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

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

;; quick compile latex
(add-hook 'LaTeX-mode-hook '(lambda()    (local-set-key (kbd "<f6>") (kbd "C-x C-s C-c C-c C-j"))))

;; Proof general
(load-file "~/.emacs.d/ProofGeneral/generic/proof-site.el")
(add-hook 'coq-mode-hook '(lambda()
			    (local-unset-key (kbd "C-c ."))
			    (local-set-key (kbd "C-c .") 'proof-goto-point)))


;; Prolog mode
(autoload 'run-prolog "prolog" "Start a Prolog sub-process." t)
(autoload 'prolog-mode "prolog" "Major mode for editing Prolog programs." t)
(setq-default prolog-indent-width 4)
(setq prolog-system 'swi)
(setq auto-mode-alist (append '(("\\.pl$" . prolog-mode)) auto-mode-alist))

;; jif files should open in java-mode
(add-to-list 'auto-mode-alist '("[.]jif$" . java-mode))

;; formatting for C/C++
(defun my-c-formatting ()
  (setq-default c-basic-offset 2)
  (c-set-offset 'innamespace 0))
(add-hook 'c-mode-hook 'my-c-formatting)
(add-hook 'c++-mode-hook 'my-c-formatting)

;; for auto-completion for C/C++
(add-to-list 'load-path "~/.emacs.d")
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(require 'yasnippet-bundle)
(require 'auto-complete-clang)
(setq ac-auto-start nil)
(setq ac-quick-help-delay 0.5)
(ac-set-trigger-key "TAB")
(defun my-ac-config ()
  (setq ac-clang-flags (split-string "
-I/home/cscollab-sdmoore/install/include
-I/home/cscollab-sdmoore/tools/include
-I/home/cscollab-sdmoore/miso/include
-I/home/cscollab-sdmoore/jitflow/include
-I/home/cscollab-sdmoore/analysis/include
-I/usr/include/c++/4.4.6
-I/usr/include/c++/4.4.6/x86_64-redhat-linux
-I/usr/include/c++/4.4.6/backward
-I/usr/local/include
-I/home/cscollab-sdmoore/tools/lib/clang/3.3/include
-I/usr/include
  "))
  (setq-default ac-sources '(ac-source-abbrev ac-source-dictionary ac-source-words-in-same-mode-buffers))
  (add-hook 'emacs-lisp-mode-hook 'ac-emacs-lisp-mode-setup)
  (add-hook 'c-mode-common-hook 'ac-cc-mode-setup)
  (add-hook 'ruby-mode-hook 'ac-ruby-mode-setup)
  (add-hook 'css-mode-hook 'ac-css-mode-setup)
  (add-hook 'auto-complete-mode-hook 'ac-common-setup)
  (global-auto-complete-mode t))
(defun my-ac-cc-mode-setup ()
  (setq ac-sources (append '(ac-source-clang ac-source-yasnippet) ac-sources)))
(add-hook 'c-mode-common-hook 'my-ac-cc-mode-setup)
(my-ac-config)
