;; -*- lexical-binding: t -*-
(require 'early-init (concat user-emacs-directory "early-init.el"))
;; (setq debug-on-error t)

;;;+straight bootstrap
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
	(url-retrieve-synchronously
	 "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
	 'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(straight-use-package 'use-package)
;;-straight bootstrap

;;; one of 'lsp-bridge, 'eglot, 'lsp-mode
(setq lsp-package 'lsp-mode)

(setq leader-map (make-sparse-keymap)) ;; bind SPC-* keybindings here

(use-package use-package-ensure-system-package
  :straight t)

;; core emacs configuration
(use-package hideshow
  :hook ((prog-mode . hs-minor-mode)))

(use-package paren
  :config
  (show-paren-mode))

(use-package delsel
  :config
  (delete-selection-mode +1))

(use-package emacs
  :mode (("\\.js\\'" . js-ts-mode)
	 ("\\.ts\\'" . tsx-ts-mode) ;; .ts is not working for me, but tsx is
	 ("\\.json\\'" . json-ts-mode))
  :init
  (fset 'yes-or-no-p 'y-or-n-p)
  ;; https://emacsredux.com/blog/2013/05/18/instant-access-to-init-dot-el/
  (defun my/find-user-init-file ()
    "Edit the `user-init-file', in another window."
    (interactive)
    (find-file-other-window user-init-file))
  ;; https://github.com/bbatsov/crux/blob/master/crux.el#L427
  (defun my/delete-file-and-buffer ()
    "Kill the current buffer and deletes the file it is visiting."
    (interactive)
    (let ((filename (buffer-file-name)))
      (when filename
	;; (when (y-or-n-p (format "Are you sure you want to delete %s? " filename))
	(delete-file filename delete-by-moving-to-trash)
	(message "Deleted file %s" filename)
	(kill-buffer);; )
	)))
  ;; https://github.com/syl20bnr/spacemacs/blob/bd7ef98e4c35fd87538dd2a81356cc83f5fd02f3/layers/%2Bdistributions/spacemacs-base/funcs.el#L294
  (defun spacemacs/rename-current-buffer-file ()
    "Renames current buffer and file it is visiting."
    (interactive)
    (let* ((name (buffer-name))
	   (filename (buffer-file-name)))
      (if (not (and filename (file-exists-p filename)))
	  (error "Buffer '%s' is not visiting a file!" name)
	(let* ((dir (file-name-directory filename))
	       (new-name (read-file-name "New name: " dir)))
	  (cond ((get-buffer new-name)
		 (error "A buffer named '%s' already exists!" new-name))
		(t
		 (let ((dir (file-name-directory new-name)))
		   (when (and (not (file-exists-p dir)) (yes-or-no-p (format "Create directory '%s'?" dir)))
		     (make-directory dir t)))
		 (rename-file filename new-name 1)
		 (rename-buffer new-name)
		 (set-visited-file-name new-name)
		 (set-buffer-modified-p nil)
		 (when (fboundp 'recentf-add-file)
		   (recentf-add-file new-name)
		   (recentf-remove-if-non-kept filename))
		 (when (and (configuration-layer/package-usedp 'projectile)
			    (projectile-project-p))
		   (call-interactively #'projectile-invalidate-cache))
		 (message "File '%s' successfully renamed to '%s'" name (file-name-nondirectory new-name))))))))
  ;; switch-to-last-buffer
  ;; https://reddit.com/r/emacs/comments/2jzkz7/quickly_switch_to_previous_buffer/
  (defun my/switch-to-last-buffer ()
    (interactive)
    (switch-to-buffer nil))
  ;; reset text scale
  (defun my/text-scale-reset ()
    (interactive)
    (text-scale-set 0))
  ;; https://stackoverflow.com/a/24809045/6051261
  (defun my/text-scale-increase ()
    (interactive)
    (let ((old-face-attribute (face-attribute 'default :height)))
      (set-face-attribute 'default nil :height (+ old-face-attribute 10))))
  (defun my/text-scale-decrease ()
    (interactive)
    (let ((old-face-attribute (face-attribute 'default :height)))
      (set-face-attribute 'default nil :height (- old-face-attribute 10))))
  :custom
  (auto-save-default nil)
  (async-shell-command-buffer 'new-buffer)
  (create-lockfiles nil) ;; react issues
  (make-backup-files nil) ;; react issues
  (tab-always-indent 'complete)
  :bind
  ("<f6>" . load-theme)
  ("C--" . my/text-scale-decrease)
  ("C-+" . my/text-scale-increase)
  ("C-=" . my/text-scale-reset)
  (:map leader-map
	("SPC" . execute-extended-command)
	("TAB" . my/switch-to-last-buffer)

	("f f" . find-file)
	("f s" . save-buffer)
	("f r" . recentf-open-files)
	("f D" . my/delete-file-and-buffer)
	("f R" . spacemacs/rename-current-buffer-file)

	("d d" . my/find-user-init-file)

	("i e" . emoji-search)

	("b d" . kill-current-buffer)
	("b b" . switch-to-buffer)
	("b r" . revert-buffer))
  :bind-keymap
  ("C-c SPC" . leader-map)
  :config
  (setq ring-bell-function 'ignore)
  (global-display-line-numbers-mode)
  (unbind-key "C-k")
  (unbind-key "C-j"))

(use-package ansi-color
  ;; https://stackoverflow.com/a/71785402/6051261
  :hook ((compilation-filter . ansi-color-compilation-filter)
	 (shell-mode-hook . ansi-color-for-comint-mode-on)))

(electric-pair-mode)

(use-package dired
  :bind
  (:map leader-map
	("a d" . dired)))

(use-package help
  :config
  (define-key leader-map (kbd "h") help-map))

(use-package project
  :config
  (define-key leader-map (kbd "p") project-prefix-map))

;; (use-package format-buffer
;;   :load-path "modules"
;;   :bind
;;   (:map leader-map
;; 	("c f" . format-buffer)))

;;+base
(use-package no-littering
  :straight t
  :custom
  (auto-save-file-name-transforms
   `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
  (custom-file (no-littering-expand-etc-file-name "custom.el")))

(use-package editorconfig
  :straight t
  :config
  (editorconfig-mode))

;;+completion
(use-package orderless
  :straight t
  :custom
  (completion-styles '(orderless))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles . (partial-completion))))))

(use-package vertico
  :straight t
  :init
  (vertico-mode))

(use-package mct
  :disabled
  :straight t
  :config
  (mct-mode))

(use-package corfu
  :unless (equal lsp-package 'lsp-bridge)
  :straight t
  :custom
  (corfu-auto t)
  (corfu-auto-prefix 0)
  (corfu-auto-delay 1)
  (corfu-quit-no-match 'separator)
  :config
  (with-eval-after-load "eglot"
    (setq completion-category-overrides '((eglot (styles orderless)))))
  (global-corfu-mode)
  (with-eval-after-load 'evil
    (define-key evil-insert-state-map (kbd "C-SPC") 'indent-for-tab-command)))

(use-package kind-icon
  :straight t
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))


(use-package marginalia
  :straight t
  :config
  (marginalia-mode))
;;-completion

(use-package which-key
  :straight t
  :config
  (which-key-mode))

(use-package consult
  :straight t
  :init
  (defun my/project-root-function () (when-let (project (project-current))
				       (car (project-roots project))))
  :bind
  ([remap project-find-regexp] . consult-ripgrep)
  ([remap isearch-forward] . consult-line)
  ([remap switch-to-buffer] . consult-buffer)
  ([remap load-theme]. consult-theme)
  ([remap imenu] . consult-imenu)
  ([remap recentf-open-files] . consult-recent-file)
  (:map project-prefix-map
	("i" . consult-project-imenu))
  :custom
  (consult-project-root-function 'my/project-root-function)
  :init
  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
	xref-show-definitions-function #'consult-xref)
  :config
  (with-eval-after-load "flymake"
    (define-key leader-map (kbd "e c") 'consult-flymake)))

(use-package apheleia
  :straight t
  :demand
  :bind
  (:map leader-map
	("c f" . apheleia-format-buffer))
  :config
  ;; (defun apheleia-indent-region+ (orig scratch callback)
  ;;   (with-current-buffer scratch
  ;;     (setq-local indent-line-function
  ;;                 (buffer-local-value 'indent-line-function orig))
  ;;     (indent-region (point-min)
  ;;                    (point-max))
  ;;     (funcall callback scratch)))

  ;; (push '(indent-region . apheleia-indent-region+) apheleia-formatters)
  ;; (push '(elisp-mode . indent-region) apheleia-mode-alist)
  ;; (push '(lisp-interaction-mode . indent-region) apheleia-mode-alist)
  (apheleia-global-mode 1))
;;-base

;;+magit

(use-package magit
  :straight t
  :custom
  (magit-save-repository-buffers nil)
  (magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
  :bind
  (:map leader-map
	("g g" . magit-status)
	("g b" . magit-blame)
	("g s" . magit-stage-file)
	("g u" . magit-unstage-file)
	("g d" . magit-file-dispatch)
	("g l b" . magit-log-buffer-file)))

;;+languages
(use-package treesit)

(use-package lua-mode
  :straight t
  :mode "\\.lua\\'")

(use-package markdown-mode
  :straight t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown"))

(use-package markdown-toc
  :straight t
  :after markdown-mode)

(use-package yaml-mode
  :straight t)
;;-languages

(use-package flymake
  :disabled
  :hook ((prog-mode . flymake-mode))
  :init
  (defun my/flymake-goto-next-important-error ()
    (interactive)
    (let ((current-prefix-arg '(4))) (call-interactively 'flymake-goto-next-error)))
  (defun my/flymake-goto-prev-important-error ()
    (interactive)
    (let ((current-prefix-arg '(4))) (call-interactively 'flymake-goto-prev-error)))
  :bind
  (:map leader-map
	("e n" . my/flymake-goto-next-important-error)
	("e p" . my/flymake-goto-prev-important-error)
	("e l" . flymake-show-diagnostics-buffer)))

(use-package flymake-eslint
  :when (equal lsp-package 'lsp-bridge)
  :after flymake
  :straight t
  :init
  (defun my/flymake-eslint-enable ()
    (unless (string= (file-name-extension (buffer-file-name)) "json")
      (flymake-eslint-enable)
      ;; https://github.com/orzechowskid/flymake-eslint/issues/19#issuecomment-559833671
      (setq-local flymake-eslint-project-root (locate-dominating-file buffer-file-name ".eslintrc"))))
  :hook ((js-mode typescript-mode typescript-tsx-mode) . my/flymake-eslint-enable))

(use-package flycheck
  :straight t
  :custom
  (flycheck-navigation-minimum-level 'warning)
  :bind
  (:map leader-map
	("e n" . flycheck-next-error)
	("e p" . flycheck-previous-error)
	("e l" . flycheck-list-errors)))

(use-package consult-flycheck
  :straight t
  :after flycheck
  :bind
  (:map leader-map
	("e c" . consult-flycheck)))

;;+lsp
(use-package eglot
  :when (equal lsp-package 'eglot)
  ;; :straight t
  ;; :ensure-system-package ((typescript-language-server . "npm install --global typescript-language-server")
  ;; 			  ;; (eslint-lsp . "npm install --global danielpza/eslint-lsp")
  ;; 			  )
  :hook ((js-mode js-ts-mode typescript-ts-mode tsx-ts-mode lua-mode) . my/eglot-ensure)
  :init
  (defun my/eglot-ensure ()
    (eglot-ensure)
    (setq-local eglot-stay-out-of '(flymake))
    (add-hook 'flymake-diagnostic-functions 'eglot-flymake-backend nil t))
  :config
  (add-to-list 'eglot-server-programs '(lua-mode . ("lua-language-server")))
  :bind
  (:map leader-map
	("l r" . eglot-rename)
	("l a" . eglot-code-actions)
	("l i" . eglot-code-action-organize-imports)
	("l S" . eglot-shutdown-all))
  :custom
  (eglot-confirm-server-initiated-edits nil))

(use-package lsp-mode
  :when (equal lsp-package 'lsp-mode)
  :straight t
  :hook ((js-ts-mode tsx-ts-mode typescript-ts-mode lua-mode) . lsp-deferred)
  :custom
  (lsp-enable-snippet nil)
  (lsp-completion-provider :none)
  :config
  (setq lsp-log-io nil) ; if set to true can cause a performance hit
  (setq lsp-idle-delay 0.500)
  (setq read-process-output-max (* 1024 1024)) ;; 1mb
  (define-key leader-map (kbd "l") lsp-command-map))

(use-package lsp-tailwindcss
  :straight '(lsp-tailwindcss :type git :host github :repo "merrickluo/lsp-tailwindcss")
  :custom
  (lsp-tailwindcss-major-modes '(typescript-ts-mode tsx-ts-mode js-ts-mode))
  (lsp-tailwindcss-add-on-mode t))

(use-package lsp-ui
  :disabled
  :straight t
  :after lsp-mode
  ;; :custom
  ;; (lsp-ui-sideline-show-code-actions t)
  )

(use-package lsp-bridge
  :when (equal lsp-package 'lsp-bridge)
  :straight '(lsp-bridge :host github :repo "manateelazycat/lsp-bridge" :files (:defaults "*.py" "acm"))
  :ensure-system-package
  ((epc . "pip install --user epc")
   (orjson . "pip install --user orjson"))
  :custom
  (acm-enable-yas nil)
  :bind
  (:map leader-map
	("l g d" . lsp-bridge-find-def)
	("l g r" . lsp-bridge-find-references)
	("l g r" . lsp-bridge-find-references)
	("l r s" . lsp-bridge-restart-process)
	("l r r" . lsp-bridge-rename)
	("l e p" . lsp-bridge-diagnostic-jump-prev)
	("l e n" . lsp-bridge-diagnostic-jump-next)
	)
  :config
  (global-lsp-bridge-mode))
;;-lsp

(use-package consult-lsp
  :straight t
  :after lsp-mode
  :bind
  (:map lsp-command-map
	([remap xref-find-apropos] . consult-lsp-symbols)
	("f" . consult-lsp-diagnostics)))

(use-package evil
  :straight t
  :demand
  :custom
  (evil-want-keybinding nil) ;; needed for evil-collection
  (evil-want-C-u-scroll t)
  (evil-want-Y-yank-to-eol t)
  (evil-undo-system 'undo-redo)
  (evil-respect-visual-line-mode t)
  :bind
  ([remap evil-goto-definition] . xref-find-definitions)
  (:map evil-normal-state-map ("z l" . hs-hide-level))
  :init
  (defun my/setup-evil-leader-key ()
    ;; leader key https://github.com/noctuid/evil-guide#preventing-certain-keys-from-being-overridden
    (define-minor-mode leader-mode
      "Bind leader-map to SPC prefix in evil modes"
      :lighter " Leader"
      :init-value t
      :global t
      :keymap (make-sparse-keymap))

    (dolist (state '(normal visual insert))
      (evil-make-intercept-map
       (evil-get-auxiliary-keymap leader-mode-map state t t)
       state))
    (evil-define-key '(normal visual) leader-mode-map (kbd "SPC") leader-map))
  :config
  ;; https://emacs.stackexchange.com/a/20717/15986
  (defalias #'forward-evil-word #'forward-evil-symbol)
  (setq-default evil-symbol-word-search t)
  (define-key leader-map (kbd "w") evil-window-map)
  (unbind-key "C-f" evil-motion-state-map)
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :straight t
  :demand
  :custom
  (evil-collection-company-use-tng nil) ;; make enter work
  :config
  (evil-collection-init))

(use-package evil-indent-plus
  :straight t
  :after evil
  :bind (
	 :map evil-inner-text-objects-map
	 ("i" . evil-indent-plus-i-indent)
	 ("I" . evil-indent-plus-i-indent-up)
	 ("k" . evil-indent-plus-i-indent-up)
	 ("j" . evil-indent-plus-i-indent-up-down)
	 ("J" . evil-indent-plus-i-indent-up-down)
	 :map evil-outer-text-objects-map
	 ("i" . evil-indent-plus-a-indent)
	 ("I" . evil-indent-plus-a-indent-up)
	 ("J" . evil-indent-plus-a-indent-up-down)))

;; (use-package evil-lion
;;   :straight t
;;   :config
;;   (evil-lion-mode))
;;-evil

(with-eval-after-load 'evil
  (my/setup-evil-leader-key))

;;+visual
(use-package dashboard
  :straight t
  :custom
  (dashboard-projects-backend 'project-el)
  (dashboard-items '((recents  . 5)
		     (projects . 5)))
  (dashboard-set-heading-icons t)
  (dashboard-set-file-icons t)
  :config
  (dashboard-setup-startup-hook)
  (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*"))))

(use-package all-the-icons
  :straight t)

(use-package all-the-icons-completion
  :straight t
  :after all-the-icons
  :config
  (all-the-icons-completion-mode))

(use-package all-the-icons-dired
  :straight t
  :after all-the-icons
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package doom-modeline
  :straight t
  :config
  (doom-modeline-mode))

(use-package doom-themes
  :straight t
  ;; :config
  ;; (load-theme 'doom-one t)
  )

(use-package treemacs
  :straight t
  :custom
  (treemacs-pulse-on-success nil)
  (treemacs-width-is-initially-locked nil)
  :bind
  (:map project-prefix-map
	("t" . treemacs-display-current-project-exclusively))
  :config
  (treemacs-follow-mode))

(use-package treemacs-evil
  :straight t
  :after (treemacs evil))

(use-package treemacs-all-the-icons
  :straight t
  :after (treemacs all-the-icons)
  :config
  (treemacs-load-theme "all-the-icons"))

(use-package treemacs-magit
  :straight t
  :after (treemacs magit))

(use-package diff-hl
  :straight t
  :demand
  :bind
  (:map leader-map
	("g [" . diff-hl-previous-hunk)
	("g ]" . diff-hl-next-hunk))
  :config
  (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  (global-diff-hl-mode 1)
  (diff-hl-flydiff-mode 1))

(use-package ef-themes
  :straight t)
;;-visual

;; others

(use-package hydra
  :straight t
  :config
  (defhydra hydra-zoom (leader-map "B")
    "buffer"
    ("n" centaur-tabs-forward "next buffer")
    ("p" centaur-tabs-backward "previous buffer")
    ("d" kill-current-buffer "delete")))

(use-package centaur-tabs
  :straight t
  :demand
  :custom
  (centaur-tabs-set-icons t)
  (centaur-tabs-gray-out-icons 'buffer)
  (centaur-tabs-set-modified-marker t)
  (centaur-tabs-modified-marker "*")
  (centaur-tabs-cycle-scope 'tabs)
  :config
  (centaur-tabs-mode t)
  :bind
  ("C-<tab>" . centaur-tabs-forward)
  ("C-<iso-lefttab>" . centaur-tabs-backward))

(use-package helpful
  :straight t
  :bind
  ([remap describe-function] . helpful-callable)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-key] . helpful-key))

(use-package prodigy
  :disabled
  :straight t
  :bind
  (:map leader-map ("c P" . prodigy))
  :config

  (prodigy-define-tag
   :name 'webpack
   :stop-signal 'sigkill
   :ready-message  ".*Compiled successfully\\..*")

  (prodigy-define-tag
   :name 'python
   :stop-signal 'sigkill
   :ready-message ".* Debugger PIN:.*")

  (prodigy-define-tag
   :name 'node
   :stop-signal 'sigkill
   :ready-message ".*Environment:.*")

  (load (concat user-emacs-directory "var/prodigy.el") t))

(use-package git-link
  :straight t
  :custom
  (git-link-default-remote "origin")
  (git-link-use-commit t)
  :bind
  (:map leader-map
	("g l l" . git-link)))

(use-package copy-as-format
  :straight t
  :bind
  (:map leader-map
	("g c s" . copy-as-format-slack)
	("g c g" . copy-as-format-github)))

(use-package quickrun
  :disabled
  :straight t
  :config
  (quickrun-add-command "typescript"
			'((:command . "ts-node")
			  (:exec . ("%c -T %o %s"))
			  (:remove . ("true")))
			:override t)
  :custom
  (quickrun-focus-p nil)
  :bind
  ("<f5>" . quickrun))

(use-package autoinsert
  :init
  ;; https://emacs.stackexchange.com/a/55781/15986
  (defcustom auto-insert-init-form 'auto-insert-init-form
    "Symbol identifying init forms in template files."
    :group 'auto-insert
    :type 'symbol)

  (defun my-eval-auto-insert-init-form ()
    "Evaluate (AUTO-INSERT-INIT-FORM ...) in autoinsert templates.
Thereby, AUTO-INSERT-INIT-FORM stands for the symbol defined by
the customizable variable `auto-insert-init-form'.
\(auto-insert-init-form ...) works like `progn'.
Applied in the newly created file it should return the string
that replaces the form."
    (goto-char (point-min))
    (cl-letf (((symbol-function auto-insert-init-form) #'progn))
      (while (re-search-forward "(auto-insert-init-form[[:space:]]" nil t)
	(let* ((beg (goto-char (match-beginning 0)))
	       (end (with-syntax-table emacs-lisp-mode-syntax-table
		      (forward-sexp)
		      (point)))
	       (str (eval (read (buffer-substring beg end)))))
	  (delete-region beg end)
	  (insert str)))))
  :config
  (setq auto-insert-query nil)
  (setq auto-insert-alist '((".editorconfig" . "editorconfig")
			    (".tsx" . ["react-typescript" my-eval-auto-insert-init-form])
			    ))
  (auto-insert-mode))

(use-package string-inflection
  :straight t
  :init
  (defun my/string-inflection-safe-javascript-kebab-case()
    (interactive)
    (save-excursion
      (call-interactively 'string-inflection-kebab-case)
      (insert  "\""))
    (insert  "\""))
  :bind
  (:map leader-map
	("i c" . string-inflection-lower-camelcase)
	("i C" . string-inflection-camelcase)
	("i u" . string-inflection-underscore)
	("i k" . string-inflection-kebab-case)
	("i j" . my/string-inflection-safe-javascript-kebab-case)
	))

;; (put 'narrow-to-region 'disabled nil)


;; other functions 

(defun shell-command-dwim (command)
  (if (use-region-p)
      (shell-command-on-region (region-beginning) (region-end) command "*shell-command-dwim*")
    (shell-command-on-region (point-min) (point-max) command "*shell-command-dwim*"))
  (with-current-buffer  "*shell-command-dwim*" (buffer-string)))

(defun json-to-ts-dwim ()
  (interactive)
  (kill-new (shell-command-dwim "json-to-ts-cli")))

(defun typescript-to-clipboard ()
  "Converts current buffer from typescript to javascript and copies it to the clipboard."
  (interactive)
  (shell-command-on-region (point-min) (point-max) (format "esbuild %s" (buffer-file-name)) "*typescript-to-clipboard*")
  (kill-new (with-current-buffer "*typescript-to-clipboard*" (buffer-string)))
  ;; https://emacs.stackexchange.com/questions/55647/manually-close-a-compilation-window-that-was-most-recently-opened-by-running-e?rq=1
  ;; (quit-window nil (get-buffer-window "*typescript-to-clipboard*"))
  (delete-windows-on (get-buffer  "*typescript-to-clipboard*"))
  )

(defun typescript-to-browser-bookmark-to-clipboard ()
  "Converts current buffer from typescript to javascript and copies it to the clipboard in a format that can be run from a firefox bookmark."
  (interactive)
  (shell-command-on-region (point-min) (point-max) (format "esbuild %s" (buffer-file-name)) "*typescript-to-clipboard*")

  (with-current-buffer "*typescript-to-clipboard*"
    (while (re-search-forward "\n" nil t)
      (replace-match "" nil nil)))
  (kill-new (concat "javascript: (() => {" (with-current-buffer "*typescript-to-clipboard*" (buffer-string)) "})()"))
  (delete-windows-on (get-buffer  "*typescript-to-clipboard*")))

(define-key leader-map (kbd "c c") 'typescript-to-clipboard)

(define-key leader-map (kbd "c b") 'typescript-to-browser-bookmark-to-clipboard)

(define-key leader-map (kbd "c l") 'recenter)

;; others

(use-package dirvish
  :disabled
  :straight t
  :config
  (setq dired-kill-when-opening-new-dired-buffer t) ; added in emacs 28
  (setq dired-clean-confirm-killing-deleted-buffers nil)
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)
  (setq delete-by-moving-to-trash t)
  (setq dired-dwim-target t)
  (setq dired-listing-switches "-AGhlv --group-directories-first --time-style=long-iso"))

(use-package yasnippet
  :disabled
  :straight t
  :config
  (yas-global-mode))

(use-package transient
  :straight t
  :demand
  :config
  (defvar node-install-command "npm ci")
  (defvar node-add-command "npm install")
  (defvar node-add-dev-command "npm install --save-dev")
  (defvar node-remove-command "npm remove")
  (add-to-list 'display-buffer-alist '("*Async Shell Command*" display-buffer-no-window (nil)))
  (transient-define-suffix node-transient--remove-dependency-suffix (package)
    :description "Remove dependencies"
    (interactive "sDependencies:")
    (async-shell-command (concat node-remove-command " " package)))
  (transient-define-suffix node-transient--add-dev-dependency-suffix (package)
    :description "Add dev dependency"
    (interactive "sDev dependencies:")
    (async-shell-command (concat node-add-dev-command " " package)))
  (transient-define-suffix node-transient--add-dependency-suffix (package)
    :description "Add dependency"
    (interactive "sDependencies:")
    (async-shell-command (concat node-add-command " " package)))
  (transient-define-suffix node-transient--install-suffix ()
    :description "Install"
    (interactive)
    (async-shell-command node-install-command))
  (transient-define-prefix node-transient--add-prefix ()
    [:class transient-columns
	    ["Add Package"
	     ("a" "Add dependency" node-transient--add-dependency-suffix)
	     ("d" "Add dev dependency" node-transient--add-dev-dependency-suffix)]])
  (transient-define-prefix node-transient ()
    [:class transient-columns
	    ["Commands"
	     ("i" node-transient--install-suffix)
	     ("r" node-transient--remove-dependency-suffix)
	     ("a" "Add dependency" node-transient--add-prefix)]])
  (define-key leader-map (kbd "y") 'node-transient))

(use-package nimbus-theme
  :straight t)

(use-package dockerfile-mode
  :straight t)

(use-package find-file-rg
  :straight t
  :bind
  ([remap project-find-file] . find-file-rg))

;; Make gc pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 2 1000 1000))

(setq read-process-output-max (* 1024 1024)) ;; 1mb

(use-package git-modes
  :straight t
  :config
  (add-to-list 'auto-mode-alist (cons "/.*ignore\\'" 'gitignore-mode)) )

(use-package org
  :custom
  (org-confirm-babel-evaluate nil)
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((shell . t)))
  ;; (defun my/org-babel-insert-code-block ()
  ;;   (interactive))
  (evil-define-key 'normal org-mode-map (kbd "M-1") (lambda () (interactive) (org-cycle-global 100)))
  (evil-define-key 'normal org-mode-map (kbd "M-2") 'org-fold-show-all)
  (evil-define-key 'normal org-mode-map (kbd "M-3") 'org-fold-hide-block-all)
  (evil-define-key 'normal org-mode-map (kbd "H") 'org-previous-visible-heading)
  (evil-define-key 'normal org-mode-map (kbd "L") 'org-next-visible-heading)
  (evil-define-key 'normal org-mode-map (kbd "M-k") 'org-metaup)
  (evil-define-key 'normal org-mode-map (kbd "M-j") 'org-metadown)
  (evil-define-key 'normal org-mode-map (kbd "M-l") 'org-metaright)
  (evil-define-key 'normal org-mode-map (kbd "M-h") 'org-metaleft)
  (evil-define-key 'normal org-mode-map (kbd "M-K") 'org-shiftmetaup)
  (evil-define-key 'normal org-mode-map (kbd "M-J") 'org-shiftmetadown)
  (evil-define-key 'normal org-mode-map (kbd "M-L") 'org-shiftmetaright)
  (evil-define-key 'normal org-mode-map (kbd "M-H") 'org-shiftmetaleft))

(load-theme 'ef-dark t)
