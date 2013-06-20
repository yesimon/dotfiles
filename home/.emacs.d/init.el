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
        (:name web-mode
               :description "emacs major mode for editing PHP/JSP/ASP HTML templates (with embedded CSS and JS blocks)"
               :type github
               :features web-mode
               :after (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
               :pkgname "fxbois/web-mode")
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
        web-mode
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

(when (display-graphic-p)
  (desktop-save-mode 1))

;; Remove ffap trying to guess url when opening files.
(setq ido-use-url-at-point nil)

(when (fboundp 'winner-mode)
  (winner-mode 1))

;; Prevent ffap trying to open root paths when editing html.
(defadvice ffap-file-at-point (after ffap-file-at-point-after-advice ())
  (if (string= ad-return-value "/")
      (setq ad-return-value nil)))
(ad-activate 'ffap-file-at-point)

(load "~/.emacs.d/misc.el" t)
(load "~/.emacs.d/local.el" t)
