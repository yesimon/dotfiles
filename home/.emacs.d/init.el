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
      '((:name exec-path-from-shell
               :after (when (memq window-system '(mac ns))
                        (exec-path-from-shell-initialize)))
        (:name smex
               :after (progn (global-set-key (kbd "M-x") 'smex)
                             (global-set-key (kbd "M-X") 'smex-major-mode-commands)))
        (:name flycheck
               :after (add-hook 'after-init-hook #'global-flycheck-mode))
        (:name auto-complete
               :after (global-auto-complete-mode t)
               :features auto-complete)
        (:name julia-mode
               :type http
               :url "https://raw.githubusercontent.com/JuliaLang/julia/master/contrib/julia-mode.el"
               :features julia-mode)
        (:name rust-mode
               :type http
               :url "https://raw.github.com/mozilla/rust/master/src/etc/emacs/rust-mode.el"
               :description "Emacs mode for Rust"
               :features rust-mode
               :after (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode)))
        (:name projectile
               :after (projectile-global-mode)
               :features projectile)
        (:name anti-zenburn-theme
               :type github
               :pkgname "m00natic/anti-zenburn-theme"
               :prepare (add-to-list 'custom-theme-load-path default-directory))
        (:name color-theme-zenburn
               :after (add-to-list 'safe-local-eval-forms
                                   '(when (require 'rainbow-mode nil t) (rainbow-mode 1))))
        (:name color-theme-base16
               :type github
               :pkgname "milkypostman/base16-emacs"
               :prepare (add-to-list 'custom-theme-load-path default-directory))
        (:name coffee-mode
               :type elpa
               :after  (progn
                         (custom-set-variables '(coffee-tab-width 2))
                         (add-hook 'coffee-mode-hook 'coffee-cos-mode)))
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

(defvar my-el-get-packages
  '(exec-path-from-shell
    smex
    pkg-info
    ido-ubiquitous
    idle-highlight-mode
    magit
    coffee-mode
    yaml-mode
    markdown-mode
    flycheck
    julia-mode
    rust-mode
    ;; go-mode
    projectile
    jedi
    ack
    ag
    color-theme-zenburn
    rainbow-mode
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
(add-hook 'web-mode-hook 'turn-off-auto-fill)

(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

(eval-after-load "vc" '(remove-hook 'find-file-hooks 'vc-find-file-hook))
(setq vc-handled-backends ())

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

(setq-default tab-width 2)

(load-theme 'zenburn t)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flycheck-error ((t (:foreground "#BC8383" :weight bold :underline t))))
 '(flycheck-info ((t (:foreground "#93E0E3" :weight bold :underline t))))
 '(flycheck-warning ((t (:foreground "#F0DFAF" :weight bold :underline t)))))

(load "~/.emacs.d/misc.el" t)
(load "~/.emacs.d/local.el" t)
