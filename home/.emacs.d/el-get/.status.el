((ack-and-a-half status "installed" recipe
		 (:name ack-and-a-half :description "Yet another front-end for ack" :type github :pkgname "jhelwig/ack-and-a-half" :prepare
			(progn
			  (defalias 'ack 'ack-and-a-half)
			  (defalias 'ack-same 'ack-and-a-half-same)
			  (defalias 'ack-find-file 'ack-and-a-half-find-file)
			  (defalias 'ack-find-file-same 'ack-and-a-half-find-file-same))))
 (auto-complete status "installed" recipe
		(:name auto-complete :website "https://github.com/auto-complete/auto-complete" :description "The most intelligent auto-completion extension." :type github :pkgname "auto-complete/auto-complete" :depends
		       (popup fuzzy)))
 (cl-lib status "installed" recipe
	 (:name cl-lib :builtin "24.3" :type elpa :description "Properly prefixed CL functions and macros" :url "http://elpa.gnu.org/packages/cl-lib.html"))
 (coffee-mode status "installed" recipe
	      (:name coffee-mode :type elpa :after
		     (load "~/.emacs.d/coffee-custom")))
 (ctable status "installed" recipe
	 (:name ctable :description "Table Component for elisp" :type github :pkgname "kiwanami/emacs-ctable"))
 (dash status "installed" recipe
       (:name dash :description "A modern list api for Emacs. No 'cl required." :type github :pkgname "magnars/dash.el"))
 (deferred status "installed" recipe
   (:name deferred :description "Simple asynchronous functions for emacs lisp" :website "https://github.com/kiwanami/emacs-deferred" :type github :pkgname "kiwanami/emacs-deferred" :features "deferred"))
 (el-get status "installed" recipe
	 (:name el-get :website "https://github.com/dimitri/el-get#readme" :description "Manage the external elisp bits and pieces you depend upon." :type github :branch "4.stable" :pkgname "dimitri/el-get" :info "." :load "el-get.el"))
 (epc status "installed" recipe
      (:name epc :description "An RPC stack for Emacs Lisp" :type github :pkgname "kiwanami/emacs-epc" :depends
	     (deferred ctable)))
 (flymake-coffee status "installed" recipe
		 (:name flymake-coffee :type github :pkgname "purcell/flymake-coffee" :description "Flymake support for coffee script" :website "http://github.com/purcell/flymake-coffee" :depends
			(flymake-easy)
			:post-init
			(add-hook 'coffee-mode-hook 'flymake-coffee-load)))
 (flymake-easy status "installed" recipe
	       (:name flymake-easy :type github :description "Helpers for easily building flymake checkers" :pkgname "purcell/flymake-easy" :website "http://github.com/purcell/flymake-easy"))
 (flymake-python-pyflakes status "installed" recipe
			  (:name flymake-python-pyflakes :type github :pkgname "purcell/flymake-python-pyflakes" :depends
				 (flymake-easy)
				 :post-init
				 (progn
				   (add-hook 'python-mode-hook 'flymake-python-pyflakes-load))
				 :features flymake-python-pyflakes))
 (frame-restore status "installed" recipe
		(:name frame-restore :type emacswiki :description "Save/restore frame size & position with desktop-save" :website "https://raw.github.com/emacsmirror/emacswiki.org/master/frame-restore.el" :features frame-restore))
 (fuzzy status "installed" recipe
	(:name fuzzy :website "https://github.com/auto-complete/fuzzy-el" :description "Fuzzy matching utilities for GNU Emacs" :type github :pkgname "auto-complete/fuzzy-el"))
 (git-modes status "installed" recipe
	    (:name git-modes :description "GNU Emacs modes for various Git-related files" :type github :pkgname "magit/git-modes"))
 (helm status "installed" recipe
       (:name helm :type github :pkgname "emacs-helm/helm" :compile nil :after
	      (progn
		(global-set-key
		 (kbd "C-c h")
		 'helm-mini)
		(helm-mode 1))
	      :features helm-config))
 (idle-highlight-mode status "installed" recipe
		      (:name idle-highlight-mode :description "Idle Highlight Mode." :website "https://github.com/nonsequitur/idle-highlight-mode" :type github :pkgname "nonsequitur/idle-highlight-mode"))
 (ido-ubiquitous status "installed" recipe
		 (:name ido-ubiquitous :description "Use ido (nearly) everywhere" :type elpa))
 (ipython status "installed" recipe
	  (:name ipython :description "Adds support for IPython to python-mode.el" :type http :url "https://raw.github.com/ipython/ipython/master/docs/emacs/ipython.el" :depends python-mode :features ipython :post-init
		 (setq py-shell-name "ipython")))
 (jedi status "installed" recipe
       (:name jedi :description "An awesome Python auto-completion for Emacs" :type github :pkgname "tkf/emacs-jedi" :build
	      (("make" "requirements"))
	      :submodule nil :depends
	      (epc auto-complete)))
 (layout-restore status "required" recipe
		 (:name layout-restore :auto-generated t :type emacswiki :description "" :website "https://raw.github.com/emacsmirror/emacswiki.org/master/layout-restore.el"))
 (magit status "installed" recipe
	(:name magit :website "https://github.com/magit/magit#readme" :description "It's Magit! An Emacs mode for Git." :type github :pkgname "magit/magit" :depends
	       (cl-lib)
	       :info "." :build
	       (if
		   (version<= "24.3" emacs-version)
		   `(("make" ,(format "EMACS=%s" el-get-emacs)
		      "all"))
		 `(("make" ,(format "EMACS=%s" el-get-emacs)
		    "docs")))
	       :build/berkeley-unix
	       (("touch" "`find . -name Makefile`")
		("gmake"))))
 (markdown-mode status "installed" recipe
		(:name markdown-mode :description "Major mode to edit Markdown files in Emacs" :website "http://jblevins.org/projects/markdown-mode/" :type git :url "git://jblevins.org/git/markdown-mode.git" :before
		       (add-to-list 'auto-mode-alist
				    '("\\.\\(md\\|mdown\\|markdown\\)\\'" . markdown-mode))))
 (multi-term status "installed" recipe
	     (:name multi-term :type emacswiki :features multi-term :after
		    (setq multi-term-program "/usr/local/bin/fish")))
 (package status "installed" recipe
	  (:name package :description "ELPA implementation (\"package.el\") from Emacs 24" :builtin "24" :type http :url "http://repo.or.cz/w/emacs.git/blob_plain/1a0a666f941c99882093d7bd08ced15033bc3f0c:/lisp/emacs-lisp/package.el" :shallow nil :features package :post-init
		 (progn
		   (setq package-user-dir
			 (expand-file-name
			  (convert-standard-filename
			   (concat
			    (file-name-as-directory default-directory)
			    "elpa")))
			 package-directory-list
			 (list
			  (file-name-as-directory package-user-dir)
			  "/usr/share/emacs/site-lisp/elpa/"))
		   (make-directory package-user-dir t)
		   (unless
		       (boundp 'package-subdirectory-regexp)
		     (defconst package-subdirectory-regexp "^\\([^.].*\\)-\\([0-9]+\\(?:[.][0-9]+\\)*\\)$" "Regular expression matching the name of\n a package subdirectory. The first subexpression is the package\n name. The second subexpression is the version string."))
		   (setq package-archives
			 '(("ELPA" . "http://tromey.com/elpa/")
			   ("gnu" . "http://elpa.gnu.org/packages/")
			   ("marmalade" . "http://marmalade-repo.org/packages/")
			   ("SC" . "http://joseito.republika.pl/sunrise-commander/"))))))
 (popup status "installed" recipe
	(:name popup :website "https://github.com/auto-complete/popup-el" :description "Visual Popup Interface Library for Emacs" :type github :pkgname "auto-complete/popup-el"))
 (projectile status "installed" recipe
	     (:name projectile :type github :pkgname "bbatsov/projectile" :depends
		    (dash s)
		    :after
		    (projectile-global-mode)
		    :features projectile))
 (python-mode status "installed" recipe
	      (:type github :pkgname "emacsmirror/python-mode" :name python-mode :type emacsmirror :description "Major mode for editing Python programs" :features
		     (python-mode doctest-mode)
		     :compile nil :load "test/doctest-mode.el" :prepare
		     (progn
		       (autoload 'python-mode "python-mode" "Python editing mode." t)
		       (add-to-list 'auto-mode-alist
				    '("\\.py$" . python-mode))
		       (add-to-list 'interpreter-mode-alist
				    '("python" . python-mode)))))
 (revive status "installed" recipe
	 (:name revive :description "Revive.el saves current editing status including the window splitting configuration, which can't be recovered by `desktop.el' nor by `saveconf.el', into a file and reconstructs that status correctly." :type http :url "http://www.gentei.org/~yuuji/software/revive.el" :features "revive"))
 (revive-plus status "installed" recipe
	      (:name revive-plus :type github :after
		     (progn
		       (setq revive-plus:all-frames t)
		       (revive-plus:minimal-setup))
		     :features revive+ :pkgname "martialboniou/revive-plus"))
 (s status "installed" recipe
    (:name s :description "The long lost Emacs string manipulation library." :type github :pkgname "magnars/s.el" :features s))
 (smex status "installed" recipe
       (:name smex :description "M-x interface with Ido-style fuzzy matching." :type github :pkgname "nonsequitur/smex" :features smex :post-init
	      (smex-initialize)))
 (starter-kit status "required" recipe
	      (:name starter-kit :type elpa))
 (starter-kit-bindings status "required" recipe
		       (:name starter-kit-bindings :type elpa))
 (starter-kit-lisp status "required" recipe
		   (:name starter-kit-lisp :type elpa))
 (thrift status "installed" recipe
	 (:name thrift :type http :url "https://raw.github.com/apache/thrift/master/contrib/thrift.el" :features thrift))
 (web-mode status "installed" recipe
	   (:name web-mode :description "emacs major mode for editing PHP/JSP/ASP HTML templates (with embedded CSS and JS blocks)" :type github :features web-mode :after
		  (add-to-list 'auto-mode-alist
			       '("\\.html?\\'" . web-mode))
		  :pkgname "fxbois/web-mode"))
 (yaml-mode status "installed" recipe
	    (:name yaml-mode :description "Simple major mode to edit YAML file for emacs" :type github :pkgname "yoshiki/yaml-mode" :features yaml-mode :after
		   (progn
		     (add-to-list 'auto-mode-alist
				  '("\\.yml$" . yaml-mode))
		     (add-to-list 'auto-mode-alist
				  '("\\.sls$" . yaml-mode))))))
