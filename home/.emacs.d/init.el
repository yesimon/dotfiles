(require 'package)
(add-to-list 'package-archives
	     '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

;; Add in your own as you wish:
(defvar my-packages '(starter-kit starter-kit-lisp starter-kit-bindings
				  coffee-mode markdown-mode cmake-mode
                                  flymake flymake-shell pyflakes pymacs
                                  ipython ido-ubiquitous find-file-in-git-repo
                                  yaml-mode flymake-coffee)
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; Load user $PATH
(defun set-exec-path-from-shell-PATH ()
  (let ((path-from-shell
         (replace-regexp-in-string "[[:space:]\n]*$" ""
                                   (shell-command-to-string "$SHELL -l -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))
(when (equal system-type 'darwin) (set-exec-path-from-shell-PATH))

;; Find file in git repo
(require 'find-file-in-git-repo)
(global-set-key (kbd "C-x f") 'find-file-in-git-repo)

;; emacs-for-python setup.
(add-to-list 'load-path "~/.emacs.d/emacs-for-python/") ;; tell where to load the various files
(require 'epy-setup) ;; It will setup other loads, it is required!
(require 'epy-python) ;; If you want the python facilities [optional]
(require 'epy-completion) ;; If you want the autocompletion settings [optional]
(require 'epy-editing) ;; For configurations related to editing [optional]
(require 'epy-bindings) ;; For my suggested keybindings [optional]
(epy-setup-ipython)
(epy-setup-checker "pyflakes %f")

;; Disable auto-newline in html mode
(add-hook 'html-mode-hook 'turn-off-auto-fill)

;; Disable skeleton-pair parentheses
 (setq skeleton-pair nil)

;; Disable linenum
(global-linum-mode 0)

;; Show column number
(column-number-mode)

;; Yaml mode
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

;; Delete and show trailing whitespace
(setq-default show-trailing-whitespace t)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'before-save-hook 'delete-trailing-blank-lines)
(defun delete-trailing-blank-lines ()
      "Deletes all blank lines at the end of the file."
      (interactive)
      (save-excursion
        (save-restriction
          (widen)
          (goto-char (point-max))
          (delete-blank-lines))))

;; Auto revert to sync with VC
(global-auto-revert-mode t)
(setq auto-revert-check-vc-info t)
(setq auto-revert-interval 10)

;; Remove backup and auto-saves.
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Automatically save and restore sessions only in graphical emacs.
(setq desktop-dirname             "~/.emacs.d/desktop/"
      desktop-base-file-name      "emacs.desktop"
      desktop-base-lock-name      "lock"
      desktop-path                (list desktop-dirname)
      desktop-save                t
      desktop-files-not-to-save   "^$" ;reload tramp paths
      desktop-load-locked-desktop nil)
(when (display-graphic-p)
  (desktop-save-mode 1))

;; Remove visible-bell from starter-kit
(setq visible-bell nil)
;; Remove scroll bars
(require 'scroll-bar)
(set-scroll-bar-mode 'nil)

;; Coffee-mode customizations
(load "~/.emacs.d/coffee-custom")

;; Show trailing whitespace
(setq whitespace-style '(trailing tabs newline tab-mark newline-mark))

;; Remove ffap trying to guess url when opening files.
(setq ido-use-url-at-point nil)

;; Remove yasnippet
(yas/global-mode nil)

;; Remove auto-complete - Sometimes freezes
(global-auto-complete-mode nil)
