(require 'package)
(add-to-list 'package-archives
	     '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("gnu" . "http://elpa.gnu.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/"))
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

;; Add in your own as you wish:
(defvar my-packages '(starter-kit starter-kit-lisp starter-kit-bindings
				  coffee-mode markdown-mode cmake-mode
                                  flymake flymake-shell pyflakes
                                  flymake-python-pyflakes pymacs
                                  ipython yaml-mode flymake-coffee
                                  s dash projectile helm)
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

(require 'projectile)
(require 'helm-config)
(global-set-key (kbd "C-c h") 'helm-mini)
(helm-mode 1)
(projectile-global-mode)

;; Flymake for python
(require 'flymake-python-pyflakes)
(add-hook 'python-mode-hook 'flymake-python-pyflakes-load)

;; Disable auto-newline in html mode
(add-hook 'html-mode-hook 'turn-off-auto-fill)

;; Disable linenum
(global-linum-mode 0)

;; Show column number
(column-number-mode)

(set-default-font "DejaVu Sans Mono 12")

;; Yaml mode
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.sls$" . yaml-mode))

;; Markdown mode
(autoload 'markdown-mode "markdown-mode.el"
  "Major mode for editing Markdown files" t)
(setq auto-mode-alist (cons '("\\.md" . markdown-mode) auto-mode-alist))

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

;; Prevent ffap trying to open root paths when editing html.
(defadvice ffap-file-at-point (after ffap-file-at-point-after-advice ())
  (if (string= ad-return-value "/")
      (setq ad-return-value nil)))
(ad-activate 'ffap-file-at-point)
