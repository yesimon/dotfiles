(defun coffee-custom ()
  "coffee-mode-hook"

  ;; CoffeeScript uses two spaces.
  (make-local-variable 'tab-width)
  (set 'tab-width 2)

  ;; If you don't have js2-mode
  (setq coffee-js-mode 'javascript-mode)

  ;; If you don't want your compiled files to be wrapped
  ;; (setq coffee-args-compile '("-c" "--bare"))

  ;; *Messages* spam
  (setq coffee-debug-mode t)

  ;; Emacs key binding
  (define-key coffee-mode-map [(meta r)] 'coffee-compile-buffer)

  ;; Riding edge.
  (setq coffee-command "coffee"))

;; Compile '.coffee' files on every save
(add-hook 'coffee-mode-hook '(lambda () (coffee-cos-mode t)))

(add-hook 'coffee-mode-hook 'coffee-custom)
(add-hook 'coffee-mode-hook 'flymake-coffee-load)

(add-to-list 'auto-mode-alist '("\\.coffee$" . coffee-mode))
(add-to-list 'auto-mode-alist '("Cakefile" . coffee-mode))
