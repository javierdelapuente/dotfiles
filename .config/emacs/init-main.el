(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(straight-use-package 'use-package)
(straight-use-package 'project)
(setq straight-use-package-by-default t)
(use-package quelpa-use-package)

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

(setq dired-listing-switches "-alhn")
;; default is "-al"
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
(use-package terraform-mode
  :custom (terraform-indent-level 4)
  )

(use-package typescript-mode)
(use-package jinja2-mode)
(use-package apache-mode)

(use-package project
  :init
  (setq project-vc-extra-root-markers '("pyproject.toml")))

(use-package eglot)
(use-package pyvenv
  :config
  (pyvenv-mode t)
  )
(use-package reformatter
  :config
  )
;; look for the reference for this:
;; tox --devenv .venv -e integration
;; echo "$(pwd)/lib" > $(pwd)/.venv/lib/python3.12/site-packages/custom_path.pth
(defun dd/python-init ()
  (let* ((project (project-current))
         (project-root (when project (project-root project)))
         (venv-path (when project-root
                      (expand-file-name ".venv" project-root))))
    (when (and venv-path (file-directory-p venv-path))
      (make-local-variable 'pyvenv-virtual-env)
      (pyvenv-activate venv-path))
    ))
(add-hook 'python-base-mode-hook #'dd/python-init)
(add-to-list 'eglot-server-programs
             `(python-base-mode
               . ,(eglot-alternatives '(("ty" "server")))))

(use-package magit
  :bind ("C-x g" . magit-status))

(use-package forge
  :init
  (setq auth-sources '("~/.authinfo"))
  :after magit)

(use-package git-link
  :config
  (setq git-link-open-in-browser t))

(use-package org
  :hook
  (org-mode . visual-line-mode)
  :config
  (setf (alist-get "s" org-structure-template-alist nil nil #'string=) "src emacs-lisp")
  )

(defconst llm-tools-dir
  (expand-file-name "llm-tool-collection/" user-emacs-directory)
  "Path to the llm-tool-collection repository.")

(require 'vc-git)
(unless (file-directory-p llm-tools-dir)
  (vc-git-clone "https://github.com/skissue/llm-tool-collection.git" llm-tools-dir "main"))
(when (file-exists-p (expand-file-name "llm-tool-collection.el" llm-tools-dir))
  (add-to-list 'load-path llm-tools-dir)
  (require 'llm-tool-collection))

(use-package gptel
  :ensure t
  :config
  (setq gptel-default-mode 'org-mode
    	gptel-expert-commands t
    	gptel-track-media t
    	gptel-include-reasoning 'ignore
    	gptel-model 'gpt-4.1 ;; "gpt-4.1"
    	gptel-log-level 'info
  	;; gptel-include-tool-results t
    	gptel-backend (gptel-make-gh-copilot "Copilot"))
  (require 'gptel)
  (require 'gptel-integrations)
  (global-set-key (kbd "C-c RET") #'gptel-send)
  (add-hook 'org-mode-hook
            (lambda ()
              (local-unset-key (kbd "C-c RET"))))
  )

;; We need this to use npx with nvm (for the mcp) 
(use-package nvm
  :straight (:host github :repo "rejeep/nvm.el")
  :config
  (nvm-use "v25.2.1"))

(use-package mcp
  :ensure t
  :after gptel
  :custom
  (mcp-hub-servers
   `(
     ("duckduckgo" . (:command "uvx" :args ("duckduckgo-mcp-server")))
     ("fetch" . (:command "uvx" :args ("mcp-server-fetch")))
     ;; ("github" . (:command "docker"
     ;;              :args ("run" "-i" "--rm"
     ;;                     "-e" "GITHUB_PERSONAL_ACCESS_TOKEN"
     ;;                     "ghcr.io/github/github-mcp-server")
     ;;              :env (:GITHUB_PERSONAL_ACCESS_TOKEN ,(get-sops-secret-value "gh_pat_mcp"))))
     ;; ("nixos" . (:command "uvx" :args ("mcp-nixos")))
     ("filesystem" . (:command "npx" 
  			       :args ("-y" "@modelcontextprotocol/server-filesystem")
  			       :roots ("/home/jpuente/github/")))
     ;;("sequential-thinking" . (:command "npx" :args ("-y" "@modelcontextprotocol/server-sequential-thinking")))
     ;; ("context7" . (:command "npx" :args ("-y" "@upstash/context7-mcp") :env (:DEFAULT_MINIMUM_TOKENS "6000")))
     ))
  (require 'mcp-hub)
  :hook (after-init . mcp-hub-start-all-server)
  )

(gptel-mcp-connect '("duckduckgo" "fetch"))

(defun org-babel-tangle-config ()
  (when (string-equal (buffer-file-name)
		      (expand-file-name "init-main.org" user-emacs-directory))
    (let ((org-config-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook
	  (lambda ()
	    (add-hook 'after-save-hook #'org-babel-tangle-config)))
