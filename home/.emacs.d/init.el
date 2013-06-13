;; Load user $PATH
(defun set-exec-path-from-shell-PATH ()
  (let ((path-from-shell
         (replace-regexp-in-string "[[:space:]\n]*$" ""
                                   (shell-command-to-string "$SHELL -l -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))
(when (equal system-type 'darwin) (set-exec-path-from-shell-PATH))

(require 'package)
(add-to-list 'package-archives
	     '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("gnu" . "http://elpa.gnu.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/"))
(package-initialize)

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (let (el-get-master-branch)
      (goto-char (point-max))
      (eval-print-last-sexp))))

(setq el-get-sources
      '((:name flymake-python-pyflakes
               :type github
	       :pkgname "purcell/flymake-python-pyflakes"
	       :depends (flymake-easy)
               :post-init (progn
			    (add-hook 'python-mode-hook 'flymake-python-pyflakes-load))
               :features flymake-python-pyflakes)
        (:name helm
               :type github
               :pkgname "emacs-helm/helm"
               :compile nil
               :after (progn
                        (global-set-key (kbd "C-c h") 'helm-mini)
                        (helm-mode 1))
               :features helm-config)
        (:name projectile
               :type github
               :pkgname "bbatsov/projectile"
               :depends (dash s)
               :after (projectile-global-mode)
               :features projectile)
        (:name multi-term
               :type emacswiki
               :features multi-term
               :after (setq multi-term-program "/usr/local/bin/fish"))
        (:name coffee-mode
               :type elpa
               :after (load "~/.emacs.d/coffee-custom"))
        (:name yaml-mode
               :features yaml-mode
               :after (progn
                        (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
                        (add-to-list 'auto-mode-alist '("\\.sls$" . yaml-mode))))
        (:name revive-plus
               :type github
	       :depends (revive)
               :after (progn
                        (setq revive-plus:all-frames t)
                        (revive-plus:minimal-setup))
               :features revive+
               :pkgname "martialboniou/revive-plus")
        ))

(setq my-el-get-packages
      '(smex
	ido-ubiquitous
	idle-highlight-mode
	magit
	coffee-mode
        yaml-mode
        markdown-mode
        flymake-python-pyflakes
	helm
	projectile
        ipython
        flymake-coffee
        ack-and-a-half
        multi-term
        revive-plus
        frame-restore))

(el-get 'sync my-el-get-packages)

(add-hook 'term-mode-hook
          (lambda ()
            (setq show-trailing-whitespace nil)))

(add-hook 'shell-mode-hook
          (lambda ()
            (setq show-trailing-whitespace nil)))

;; Disable auto-newline in html mode
(add-hook 'html-mode-hook 'turn-off-auto-fill)

;; Disable linenum
(global-linum-mode 0)

;; Show column number
(column-number-mode)

(set-default-font "DejaVu Sans Mono 12")

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

(setq inhibit-startup-message t
      color-theme-is-global t
      sentence-end-double-space nil
      shift-select-mode nil
      mouse-yank-at-point t
      uniquify-buffer-name-style 'forward
      whitespace-style '(face trailing lines-tail tabs)
      whitespace-line-column 80
      ediff-window-setup-function 'ediff-setup-windows-plain
      oddmuse-directory (concat user-emacs-directory "oddmuse")
      save-place-file (concat user-emacs-directory "places")
      backup-directory-alist `(("." . ,(concat user-emacs-directory "backups")))
      diff-switches "-u")

(tool-bar-mode -1)
(add-hook 'before-make-frame-hook 'esk-turn-off-tool-bar)

;; Font size
(define-key global-map (kbd "C-+") 'text-scale-increase)
(define-key global-map (kbd "C--") 'text-scale-decrease)

;; Use regex searches by default.
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "\C-r") 'isearch-backward-regexp)
(global-set-key (kbd "M-%") 'query-replace-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)
(global-set-key (kbd "C-M-%") 'query-replace)

;; Jump to a definition in the current file. (Protip: this is awesome.)
(global-set-key (kbd "C-x C-i") 'imenu)

(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (tooltip-mode -1)
  (mouse-wheel-mode t)
  (blink-cursor-mode -1))
(show-paren-mode 1)

;; ido-mode is like magic pixie dust!
(ido-mode t)
(ido-ubiquitous t)
(setq ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-auto-merge-work-directories-length nil
      ido-create-new-buffer 'always
      ido-use-filename-at-point 'guess
      ido-use-virtual-buffers t
      ido-handle-duplicate-virtual-buffers 2
      ido-max-prospects 10)
(set-default 'indent-tabs-mode nil)

(defalias 'yes-or-no-p 'y-or-n-p)

(when (display-graphic-p)
  (desktop-save-mode 1))

;; Remove visible-bell from starter-kit
(setq visible-bell nil)
;; Remove scroll bars
(require 'scroll-bar)
(set-scroll-bar-mode 'nil)

;; Show trailing whitespace
(setq whitespace-style '(trailing tabs newline tab-mark newline-mark))

;; Remove ffap trying to guess url when opening files.
(setq ido-use-url-at-point nil)

(when (fboundp 'winner-mode)
  (winner-mode 1))

;; Prevent ffap trying to open root paths when editing html.
(defadvice ffap-file-at-point (after ffap-file-at-point-after-advice ())
  (if (string= ad-return-value "/")
      (setq ad-return-value nil)))
(ad-activate 'ffap-file-at-point)
