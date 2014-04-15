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
         :post-init (add-hook 'python-mode-hook 'flymake-python-pyflakes-load)
         :features flymake-python-pyflakes)
        (:name thrift
               :type http
               :url "https://raw.github.com/apache/thrift/master/contrib/thrift.el"
               :features thrift)
	(:name auto-complete
               :after (global-auto-complete-mode t)
	       :features auto-complete)
        (:name flymake-cursor
               :features flymake-cursor)
        (:name projectile
               :after (projectile-global-mode)
               :features projectile)
        (:name anti-zenburn-theme
               :type github
               :pkgname "m00natic/anti-zenburn-theme"
               :prepare (add-to-list 'custom-theme-load-path default-directory))
        (:name color-theme-base16
               :type github
               :pkgname "milkypostman/base16-emacs"
               :prepare (add-to-list 'custom-theme-load-path default-directory))
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
        pkg-info
        ido-ubiquitous
        idle-highlight-mode
        magit
        coffee-mode
        yaml-mode
        markdown-mode
        flymake-python-pyflakes
        flymake-coffee
        flymake-cursor
        projectile
        jedi
        thrift
        ack-and-a-half
        color-theme-zenburn
        revive-plus
        frame-restore
        web-mode))

(el-get-cleanup my-el-get-packages)
(el-get 'sync my-el-get-packages)

(autoload 'octave-mode "octave-mode" nil t)
          (setq auto-mode-alist
                (cons '("\\.m$" . octave-mode) auto-mode-alist))

(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:setup-keys t)
(setq jedi:complete-on-dot t)

(add-hook 'web-mode-hook 'whitespace-turn-off)

;; Disable auto-newline in html mode
(add-hook 'html-mode-hook 'turn-off-auto-fill)

(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

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

(defun my-kill-emacs ()
  "save some buffers, then exit unconditionally"
  (interactive)
  (save-some-buffers nil t)
  (kill-emacs))
;; (global-set-key (kbd "C-x C-c") 'my-kill-emacs)
(setq vc-follow-symlinks t)

(load "~/.emacs.d/misc.el" t)
(load "~/.emacs.d/local.el" t)

(load-theme 'zenburn t)
