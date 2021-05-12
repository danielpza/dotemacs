(setq comp-deferred-compilation t)
(setq comp-async-report-warnings-errors nil)
(setq gc-cons-threshold (* 50 1000 1000))
(setq package-enable-at-startup nil)
(setq site-run-file nil)

(add-to-list 'initial-frame-alist '(fullscreen . maximized))

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

(provide 'early-init)
