(setenv "PATH" (concat (getenv "PATH") ":" (expand-file-name "~/go/bin")))
(setenv "PATH" (concat (getenv "PATH") ":" (expand-file-name "~/.local/bin")))
(setq exec-path (append exec-path (list (expand-file-name "~/go/bin"))))
(setq exec-path (append exec-path (list (expand-file-name "~/.local/bin"))))

(require 'package)
(add-to-list 'package-archives '("stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

;; Initializes the package infrastructure
(package-initialize)

;; If there are no archived package contents, refresh them
(when (not package-archive-contents)
  (package-refresh-contents))

(dolist (package '(use-package diminish bind-key quelpa))
  (unless (package-installed-p package)
    (package-install package))
  (require package))

(require 'use-package-ensure)
(setq use-package-always-ensure t)



;; ==================================================
;; Simple Configuration
;; ==================================================
(setq inhibit-startup-message t)    ;; Hide the startup message

(use-package material-theme)
(load-theme 'material t)            ;; Load material theme

(line-number-mode)
(tool-bar-mode -1)

(global-set-key (kbd "M-i") 'imenu)

;; (add-hook 'emacs-startup-hook 'toggle-frame-maximized)
;; (add-to-list 'default-frame-alist '(fullscreen . maximized))
;; (when window-system (set-frame-size (selected-frame) 100 50))
;; (add-to-list 'default-frame-alist '(width . 100))
;; (add-to-list 'default-frame-alist '(height . 50))
;;(setq default-frame-alist '((width . 100) (height . 50)))
(add-hook 'after-make-frame-functions
	  (lambda (frame)
	    (set-frame-width frame 100)
	    (set-frame-height frame 50)))


(setq column-number-mode t)

(setq dired-dwim-target t)

(setq ring-bell-function 'ignore)
;; (setq visible-bell 1)

;; ==================================================
;; Package Main Configuration
;; ==================================================
(use-package quelpa-use-package)

(require 'tramp)
(require 'tramp-container)

(use-package yaml-mode)
(use-package hcl-mode)
(use-package dockerfile-mode)
(use-package go-mode)
(use-package protobuf-mode)
(use-package markdown-mode)
(add-hook 'markdown-mode-hook (lambda () (setq-local require-final-newline nil)))
(advice-add 'markdown-preview :around (lambda (orig &rest args) "Use chromium as default browser" (let ((browse-url-browser-function #'browse-url-chrome)) (apply orig args))))

(use-package ace-window
  :defer t
  :init
  (global-set-key (kbd "M-o") 'ace-window)
  (setq aw-scope 'frame)
  )

(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status))

(setq auth-sources '("~/.authinfo"))
(use-package forge
  :after magit)

(use-package git-link)
(setq git-link-open-in-browser t)

(use-package helm
  :demand t
  :bind (("M-x" . helm-M-x)
	 ("C-x C-f" . helm-find-files))
  :config (helm-mode 1)
  )

(use-package flycheck)

(use-package which-key
    :config
    (which-key-mode))

(use-package google-this
  :config
  (google-this-mode 1))

(use-package pyvenv
  :ensure t
  :config
  (pyvenv-mode t)
  )

(use-package corfu
  ;; Optional customizations
  :custom
  (corfu-auto t)
  :init
  (global-corfu-mode))

;; For lsp with Python, install:
;; - "pip install "python-lsp-server[all]" # or
;; - sudo apt install python3-pylsp 

;; For go:
;; - go install golang.org/x/tools/gopls@latest

;; For js:
;; install ts-ls when queried after opening a js/ts buffer with lsp

(use-package lsp-mode
  :custom
  (lsp-completion-provider :none) ;; we use Corfu!
  :init
  (setq lsp-keymap-prefix "C-c l")
  (setq lsp-file-watch-threshold 3000)
  (defun my/lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(flex)))
  :hook (
         (java-mode . lsp)
         (js-mode . lsp)
	 (go-mode . lsp)
	 (python-mode . lsp)
	 (c-mode-hook . lsp)
	 (c++-mode-hook . lsp)
	 (ruby-mode-hook . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp
  )

(add-hook 'c-mode-hook 'lsp)
(add-hook 'c++-mode-hook 'lsp)
(add-hook 'ruby-mode-hook 'lsp)
(add-hook 'go-mode-hook #'lsp-deferred)
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)
(add-hook 'js-mode-hook 'lsp)
(with-eval-after-load 'js
  (define-key js-mode-map (kbd "M-.") nil))



(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))

;; this is generating problems. I do not want remote Tramp
(setq lsp-auto-register-remote-clients nil)

(use-package terraform-mode
  :custom (terraform-indent-level 4)
)

(use-package typescript-mode)
(use-package jinja2-mode)
(use-package apache-mode)

;; Install to the user bin with
;; pip install --user black-macchiato --break-system-packages
(use-package python-black)
  
(use-package tox
  :config
  (setq tox-runner 'py.test)
  (advice-add 'tox--get-runner-arguments :filter-return (lambda (args) (concat " -- " args)))
  )

(use-package ox-hugo
  :ensure t
  :pin melpa
  :after ox)


(use-package term
  :bind (:map term-raw-map ("C-c C-y" . term-paste)))

(use-package python-pytest)
;; https://gist.githubusercontent.com/grafov/10985431/raw/dcd12037308446179f26f7d2ab2c034a1e995d2e/go-template-mode.el
(use-package go-template-mode   :ensure nil  :load-path "~/.config/emacs/go-template-mode.el")


(use-package mermaid-mode
  :config
  (setq mermaid-mmdc-location "/usr/bin/npx")
    ;; FIXME Will not work with org mode, as mermaid-flags are the last arguments to the program
  (setq mermaid-flags (format "-p @mermaid-js/mermaid-cli mmdc -p %s/.config/emacs/puppeteer-config.json" (getenv "HOME")))
  )


;; https://emacs-lsp.github.io/dap-mode/page/configuration/#dap-mode-configuration
;; go install github.com/go-delve/delve/cmd/dlv@latest

(use-package dap-mode
  :config
  (require 'dap-dlv-go)
  (dap-auto-configure-features t)
  )


;; ==================================================
;; Some hardcoded configuration. Look for alternatives
;; ==================================================

(add-hook 'python-mode-hook
	  (lambda ()
	    (setq fill-column 99)
	    (display-fill-column-indicator-mode)
	    ))

;; ==================================================
;; TESTING/EXPERIMENTATION
;; ==================================================

;; If there is a pyvenv environment, activate it for the shell-mode
(add-hook 'shell-mode-hook
	  (lambda ()
	    (when (and (boundp 'pyvenv-virtual-env-name) pyvenv-virtual-env-name (not (file-remote-p default-directory)))
	      ;; (shell-eval-command (format "workon %s\n" pyvenv-virtual-env-name))
		(shell-eval-command (format ". %s/bin/activate" pyvenv-virtual-env))
		(comint-send-input))))


;; Playing with multipass and tramp. TODO Have a go with autocomplete
(add-to-list 'tramp-methods
	`("multipass"
	  (tramp-login-program      "multipass")
	  (tramp-login-args         (("shell") ("%h")))
	  (tramp-remote-shell "/bin/sh")
	  (tramp-remote-shell-login ("-l")))
	)

(defun tramp-multipass-parse-instance-names (_ignore)
  ;;   "Return a list of (nil host) tuples of multipass Running instances."
  (delq nil
	(mapcar
	 (lambda (line)
	   (when (string-match
		  (rx bol (group (+ (not ","))) "," (group (+ (not ","))) (* nonl) eol) line)
	     (let ((host (match-string 1 line))
		   (state (match-string 2 line))
		   )
	       (unless (or (string= host "Name") (not (string= state "Running")))
	       `(,state ,host)))
	     ))
	 (tramp-process-lines nil "multipass" "list" "--format=csv")
	 )
	))

(tramp-set-completion-function
 "multipass" '((tramp-multipass-parse-instance-names "")))


(defun remove-multipass-delete-default-host (orig-fun method)
  ;; FIXME remove the first element, that is tramp-parse-default-user-host.
  ;; It would be better to look for that fn
  (if (string= method "multipass")
      (progn
	(message "multipass")
	(let ((res (seq-drop (funcall orig-fun method) 1)))
	  res
	  ))
    (funcall orig-fun method)))

(advice-add 'tramp-get-completion-function :around #'remove-multipass-delete-default-host)


(setq auto-mode-alist (cons '("\\.csproj$" . nxml-mode) auto-mode-alist))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-src-preserve-indentation t)
 '(package-selected-packages
   '(dap-mod dap-mode dap-dlv-go python-pytest mermaid-mode python-black ox-hugo tox apache-mode jinja2-mode typescript-mode terraform-mode lsp-mode corfu pyvenv google-this which-key flycheck helm git-link forge magit ace-window markdown-mode protobuf-mode go-mode dockerfile-mode hcl-mode yaml-mode quelpa-use-package material-theme quelpa diminish))
 '(safe-local-variable-values
   '((eval set
	   (make-local-variable 'flycheck-go-build-tags)
	   '("libsqlite3"))
     (go-test-args . "-tags libsqlite3 -timeout 120s")
     (etags-regen-ignores "test/manual/etags/")
     (etags-regen-regexp-alist
      (("c" "objc")
       "/[ \11]*DEFVAR_[A-Z_ \11(]+\"\\([^\"]+\\)\"/\\1/" "/[ \11]*DEFVAR_[A-Z_ \11(]+\"[^\"]+\",[ \11]\\([A-Za-z0-9_]+\\)/\\1/"))))
 '(warning-suppress-types '((use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "DejaVu Sans Mono" :foundry "PfEd" :slant normal :weight regular :height 120 :width normal)))))

