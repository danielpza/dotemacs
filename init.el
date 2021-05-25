;; -*- lexical-binding: t -*-
(require 'early-init (concat user-emacs-directory "early-init.el"))

(global-display-line-numbers-mode)

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

(setq leader-map (make-sparse-keymap)) ;; bind SPC-* keybindings here

;; core emacs configuration
(use-package hideshow
  :hook (prog-mode . hs-minor-mode))

(use-package paren
  :config
  (show-paren-mode))

(use-package delsel
  :config
  (delete-selection-mode +1))

(use-package flymake
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

(use-package emacs
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
	(if (vc-backend filename)
	    (vc-delete-file filename)
	  (when (y-or-n-p (format "Are you sure you want to delete %s? " filename))
	    (delete-file filename delete-by-moving-to-trash)
	    (message "Deleted file %s" filename)
	    (kill-buffer))))))
  ;; https://github.com/bbatsov/crux/blob/master/crux.el#L409
  (defun my/rename-file-and-buffer ()
    "Rename current buffer and if the buffer is visiting a file, rename it too."
    (interactive)
    (let ((filename (buffer-file-name)))
      (if (not (and filename (file-exists-p filename)))
	  (rename-buffer (read-from-minibuffer "New name: " (buffer-name)))
	(let* ((new-name (read-file-name "New name: " (file-name-directory filename)))
	       (containing-dir (file-name-directory new-name)))
	  (make-directory containing-dir t)
	  (cond
	   ((vc-backend filename) (vc-rename-file filename new-name))
	   (t
	    (rename-file filename new-name t)
	    (set-visited-file-name new-name t t)))))))
  ;; switch-to-last-buffer
  ;; https://reddit.com/r/emacs/comments/2jzkz7/quickly_switch_to_previous_buffer/
  (defun my/switch-to-last-buffer ()
    (interactive)
    (switch-to-buffer nil))
  ;; reset text scale
  (defun my/text-scale-reset ()
    (interactive)
    (text-scale-set 0))
  :custom
  (auto-save-default nil)
  (create-lockfiles nil)
  (make-backup-files nil)
  (tab-always-indent 'complete)
  :bind
  ("<f5>" . load-theme)
  ("C--" . text-scale-decrease)
  ("C-+" . text-scale-increase)
  ("C-=" . my/text-scale-reset)
  (:map leader-map
	("SPC" . execute-extended-command)
	("TAB" . my/switch-to-last-buffer)

	("f f" . find-file)
	("f s" . save-buffer)
	("f r" . recentf-open-files)
	("f D" . my/delete-file-and-buffer)
	("f R" . my/rename-file-and-buffer)

	("d d" . my/find-user-init-file)

	("b d" . kill-current-buffer)
	("b b" . switch-to-buffer)
	("b r" . revert-buffer))
  :bind-keymap
  ("C-c SPC" . leader-map)
  :config
  (unbind-key "C-k")
  (unbind-key "C-j"))

(use-package help
  :config
  (define-key leader-map (kbd "h") help-map))

(use-package project
  :config
  (define-key leader-map (kbd "p") project-prefix-map))

(use-package format-buffer
  :load-path "modules"
  :bind
  (:map leader-map
	("c f" . format-buffer)))

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

(use-package corfu
  :disabled
  :straight t
  :config
  (corfu-global-mode)
  (with-eval-after-load 'evil
    (define-key evil-insert-state-map (kbd "C-SPC") 'indent-for-tab-command)))

(use-package company
  :straight t
  :config
  (global-company-mode)
  (with-eval-after-load 'evil
    (define-key evil-insert-state-map (kbd "C-SPC") 'company-complete)))

(use-package company-box
  :straight t
  :hook (company-mode . company-box-mode))

(use-package marginalia
  :straight t
  :config
  (marginalia-mode))

(use-package editorconfig
  :straight t
  :config
  (editorconfig-mode))

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
  ;; (xref-show-xrefs-function 'consult-xref)
  ;; (xref-show-definitions-function 'consult-xref)
  )
;;-base

;;+magit
(use-package magit
  :straight t
  :bind
  (:map leader-map
	("g g" . magit-status)
	("g b" . magit-blame)
	("g s" . magit-stage-file)
	("g u" . magit-unstage-file)
	("g d" . magit-file-dispatch)
	("g l b" . magit-log-buffer-file)))
;;-magit

;;+code
(use-package js
  :custom
  (js-indent-level 2))

(use-package typescript-mode
  :straight t
  :mode "\\.tsx?\\'")

(use-package flymake-eslint
  :straight t
  :init
  (defun my/flymake-eslint-enable ()
    (unless (string= (file-name-extension (buffer-file-name)) "json")
      (flymake-eslint-enable)))
  :hook ((js-mode typescript-mode typescript-react-mode) . my/flymake-eslint-enable))

(use-package eglot
  :disabled
  :straight t
  ;; :ensure-system-package ((typescript-language-server . "npm install --global typescript-language-server")
  ;;			  (eslint-lsp . "npm install --global danielpza/eslint-lsp"))
  :init
  (defun my/eglot-ensure ()
    (eglot-ensure)
    (setq-local eglot-stay-out-of '(flymake))
    (add-hook 'flymake-diagnostic-functions 'eglot-flymake-backend nil t))
  :hook ((js-mode typescript-mode typescript-react-mode) . my/eglot-ensure)
  :bind
  (:map leader-map
	("l r" . eglot-rename)
	("l a" . eglot-code-actions)
	("l i" . eglot-code-action-organize-imports)
	("l S" . eglot-shutdown-all))
  :custom
  (eglot-confirm-server-initiated-edits nil))

(use-package lsp-mode
  :straight t
  :hook ((js-mode typescript-mode lua-mode) . lsp-deferred)
  :config
  (setq read-process-output-max (* 1024 1024)) ;; 1mb
  (define-key leader-map (kbd "l") lsp-command-map))

(use-package lsp-ui
  :straight t
  :after lsp-mode)
;;-code

;;+evil
(use-package evil
  :straight t
  :demand
  :init
  :custom
  (evil-want-keybinding nil) ;; needed for evil-collection
  (evil-want-C-u-scroll t)
  (evil-want-Y-yank-to-eol t)
  (evil-undo-system 'undo-redo)
  (evil-respect-visual-line-mode t)
  :bind
  ([remap evil-goto-definition] . xref-find-definitions)
  (:map evil-normal-state-map ("z l" . hs-hide-level))
  :config
  (define-key leader-map (kbd "w") evil-window-map)
  (unbind-key "C-f" evil-motion-state-map)
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :straight t
  :demand
  :custom
  (evil-collection-company-use-tng nil)
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
;;-evil

;; leader key https://github.com/noctuid/evil-guide#preventing-certain-keys-from-being-overridden
(with-eval-after-load 'evil
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
  (dashboard-setup-startup-hook))

(use-package all-the-icons
  :straight t)

(use-package doom-modeline
  :straight t
  :config
  (doom-modeline-mode))

(use-package doom-themes
  :straight t
  :config
  (load-theme 'doom-one t))
;;-visual

;;+others
(use-package helpful
  :straight t
  :bind
  ([remap describe-function] . helpful-callable)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-key] . helpful-key))

(use-package treemacs
  :straight t
  :init
  (defun my/open-treemacs-hack ()
    (interactive)
    (call-interactively 'treemacs-display-current-project-exclusively)
    (treemacs-select-window))
  :custom
  (treemacs-pulse-on-success nil)
  :bind
  (:map treemacs-mode-map
	("Q" . quit-window)
	("q" . other-window))
  (:map project-prefix-map
	("t" . my/open-treemacs-hack))
  :config
  (treemacs-follow-mode))

(use-package treemacs-evil
  :straight t
  :after treemacs)

(use-package treemacs-all-the-icons
  :straight t
  :after (treemacs all-the-icons)
  :config
  (treemacs-load-theme 'all-the-icons))

(use-package treemacs-magit
  :straight t
  :after (treemacs magit))

(use-package all-the-icons-dired
  :straight t
  :after all-the-icons
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package prodigy
  :straight t
  :bind
  (:map leader-map ("c P" . prodigy))
  :config
  (load (concat user-emacs-directory "var/prodigy.el") t))

(use-package apheleia
  :straight '(apheleia :host github :repo "raxod502/apheleia")
  :init
  (defun setup-format-buffer-apheleia()
    (setq-local format-buffer-fn 'apheleia-format-buffer))
  :hook ((typescript-mode typescript-react-mode js-mode scss-mode) . setup-format-buffer-apheleia))

(use-package diff-hl
  :straight t
  :demand
  :hook ((magit-pre-refresh-hook . diff-hl-magit-pre-refresh)
	 (magit-post-refresh-hook . diff-hl-magit-post-refresh))
  :bind
  (:map leader-map
	("g [" . diff-hl-previous-hunk)
	("g ]" . diff-hl-next-hunk))
  :config
  (global-diff-hl-mode 1)
  (diff-hl-flydiff-mode 1))
;;-others

;; Make gc pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 2 1000 1000))
