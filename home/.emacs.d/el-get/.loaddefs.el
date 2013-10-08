;;; .loaddefs.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (ack-and-a-half-find-file-same ack-and-a-half-find-file
;;;;;;  ack-and-a-half-same ack-and-a-half) "ack-and-a-half/ack-and-a-half"
;;;;;;  "ack-and-a-half/ack-and-a-half.el" (20920 10038))
;;; Generated autoloads from ack-and-a-half/ack-and-a-half.el

(autoload 'ack-and-a-half "ack-and-a-half/ack-and-a-half" "\
Run ack.
PATTERN is interpreted as a regular expression, iff REGEXP is
non-nil.  If called interactively, the value of REGEXP is
determined by `ack-and-a-half-regexp-search'.  A prefix arg
toggles the behavior.  DIRECTORY is the root directory.  If
called interactively, it is determined by
`ack-and-a-half-project-root-file-patterns'.  The user is only
prompted, if `ack-and-a-half-prompt-for-directory' is set.

\(fn PATTERN &optional REGEXP DIRECTORY)" t nil)

(autoload 'ack-and-a-half-same "ack-and-a-half/ack-and-a-half" "\
Run ack with --type matching the current `major-mode'.
The types of files searched are determined by
`ack-and-a-half-mode-type-alist' and
`ack-and-a-half-mode-extension-alist'.  If no type is configured,
the buffer's file extension is used for the search.  PATTERN is
interpreted as a regular expression, iff REGEXP is non-nil.  If
called interactively, the value of REGEXP is determined by
`ack-and-a-half-regexp-search'.  A prefix arg toggles that value.
DIRECTORY is the directory in which to start searching.  If
called interactively, it is determined by
`ack-and-a-half-project-root-file-patterns`.  The user is only
prompted, if `ack-and-a-half-prompt-for-directory' is set.`

\(fn PATTERN &optional REGEXP DIRECTORY)" t nil)

(autoload 'ack-and-a-half-find-file "ack-and-a-half/ack-and-a-half" "\
Prompt to find a file found by ack in DIRECTORY.

\(fn &optional DIRECTORY)" t nil)

(autoload 'ack-and-a-half-find-file-same "ack-and-a-half/ack-and-a-half" "\
Prompt to find a file found by ack in DIRECTORY.

\(fn &optional DIRECTORY)" t nil)

;;;***

;;;### (autoloads (auto-complete) "auto-complete/auto-complete" "../../../../../../../.emacs.d/el-get/auto-complete/auto-complete.el"
;;;;;;  "901a3d1106ca99b08654979a9bc8ebd6")
;;; Generated autoloads from ../../../../../../../.emacs.d/el-get/auto-complete/auto-complete.el

(autoload 'auto-complete "auto-complete/auto-complete" "\
Start auto-completion at current point.

\(fn &optional SOURCES)" t nil)

;;;***

;;;### (autoloads (coffee-mode) "coffee-mode/coffee-mode" "coffee-mode/coffee-mode.el"
;;;;;;  (20920 9987))
;;; Generated autoloads from coffee-mode/coffee-mode.el

(autoload 'coffee-mode "coffee-mode/coffee-mode" "\
Major mode for editing CoffeeScript.

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.coffee$" . coffee-mode))

(add-to-list 'auto-mode-alist '("Cakefile" . coffee-mode))

;;;***

;;;### (autoloads (el-get-checksum el-get-make-recipes el-get-cd
;;;;;;  el-get-self-update el-get-update-all el-get-version) "el-get/el-get"
;;;;;;  "el-get/el-get.el" (20920 9813))
;;; Generated autoloads from el-get/el-get.el

(autoload 'el-get-version "el-get/el-get" "\
Message the current el-get version

\(fn)" t nil)

(autoload 'el-get-update-all "el-get/el-get" "\
Performs update of all installed packages.

\(fn &optional NO-PROMPT)" t nil)

(autoload 'el-get-self-update "el-get/el-get" "\
Update el-get itself.  The standard recipe takes care of reloading the code.

\(fn)" t nil)

(autoload 'el-get-cd "el-get/el-get" "\
Open dired in the package directory.

\(fn PACKAGE)" t nil)

(autoload 'el-get-make-recipes "el-get/el-get" "\
Loop over `el-get-sources' and write a recipe file for each
entry which is not a symbol and is not already a known recipe.

\(fn &optional DIR)" t nil)

(autoload 'el-get-checksum "el-get/el-get" "\
Compute the checksum of the given package, and put it in the kill-ring

\(fn PACKAGE &optional PACKAGE-STATUS-ALIST)" t nil)

;;;***

;;;### (autoloads (el-get-list-packages) "el-get/el-get-list-packages"
;;;;;;  "el-get/el-get-list-packages.el" (20920 9813))
;;; Generated autoloads from el-get/el-get-list-packages.el

(autoload 'el-get-list-packages "el-get/el-get-list-packages" "\
Display a list of packages.

\(fn)" t nil)

;;;***

;;;### (autoloads (flymake-coffee-load) "flymake-coffee/flymake-coffee"
;;;;;;  "flymake-coffee/flymake-coffee.el" (20920 9998))
;;; Generated autoloads from flymake-coffee/flymake-coffee.el

(autoload 'flymake-coffee-load "flymake-coffee/flymake-coffee" "\
Configure flymake mode to check the current buffer's coffeescript syntax.

\(fn)" t nil)

;;;***

;;;### (autoloads (flymake-python-pyflakes-load) "flymake-python-pyflakes/flymake-python-pyflakes"
;;;;;;  "flymake-python-pyflakes/flymake-python-pyflakes.el" (20920
;;;;;;  9997))
;;; Generated autoloads from flymake-python-pyflakes/flymake-python-pyflakes.el

(autoload 'flymake-python-pyflakes-load "flymake-python-pyflakes/flymake-python-pyflakes" "\
Configure flymake mode to check the current buffer's python syntax using pyflakes.

\(fn)" t nil)

;;;***

;;;### (autoloads (helm-follow-mode helm-kill-selection-and-quit
;;;;;;  helm-yank-selection helm-prev-visible-mark helm-next-visible-mark
;;;;;;  helm-display-all-visible-marks helm-toggle-all-marks helm-unmark-all
;;;;;;  helm-mark-all helm-toggle-visible-mark helm-reposition-window-other-window
;;;;;;  helm-recenter-top-bottom-other-window helm-scroll-other-window-down
;;;;;;  helm-scroll-other-window helm-execute-persistent-action helm-select-2nd-action-or-end-of-line
;;;;;;  helm-select-4th-action helm-select-3rd-action helm-select-2nd-action
;;;;;;  helm-swap-windows helm-enlarge-window helm-narrow-window
;;;;;;  helm-toggle-resplit-window helm-delete-minibuffer-contents
;;;;;;  helm-delete-current-selection helm-debug-output helm-keyboard-quit
;;;;;;  helm-exit-minibuffer helm-confirm-and-exit-minibuffer helm-next-source
;;;;;;  helm-previous-source helm-end-of-buffer helm-beginning-of-buffer
;;;;;;  helm-next-page helm-previous-page helm-next-line helm-previous-line
;;;;;;  helm-select-action helm-force-update helm-toggle-suspend-update
;;;;;;  helm-other-buffer helm-resume-previous-session-after-quit
;;;;;;  helm-resume helm-open-last-log helm-define-multi-key) "helm/helm"
;;;;;;  "helm/helm.el" (20920 10007))
;;; Generated autoloads from helm/helm.el

(autoload 'helm-define-multi-key "helm/helm" "\
In KEYMAP, define key sequence KEY for function list FUNCTIONS.
Each function run sequentialy each time the key KEY is pressed.
If DELAY is specified switch back to initial function of FUNCTIONS list
after DELAY seconds.
The functions in FUNCTIONS list are functions with no args.
e.g
  (defun foo ()
    (message \"Run foo\"))
  (defun bar ()
    (message \"Run bar\"))
  (defun baz ()
    (message \"Run baz\"))

\(helm-define-multi-key global-map \"<f5> q\" '(foo bar baz) 2)

Each time \"<f5> q\" is pressed the next function is executed, if you wait
More than 2 seconds, next hit will run again the first function and so on.

\(fn KEYMAP KEY FUNCTIONS &optional DELAY)" nil nil)

(autoload 'helm-open-last-log "helm/helm" "\
Open helm log file of last helm session.
If `helm-last-log-file' is nil, switch to `helm-debug-buffer' .

\(fn)" t nil)

(autoload 'helm-resume "helm/helm" "\
Resurrect previously invoked `helm'.
Called with a prefix arg, allow choosing among all existing
helm buffers.  i.e choose among various helm sessions.
Called from lisp, you can specify a buffer-name as a string with ARG.

\(fn ARG)" t nil)

(autoload 'helm-resume-previous-session-after-quit "helm/helm" "\
Resume previous helm session within running helm.

\(fn ARG)" t nil)

(autoload 'helm-other-buffer "helm/helm" "\
Simplified interface of `helm' with other `helm-buffer'.
Call `helm' with only ANY-SOURCES and ANY-BUFFER as args.

\(fn ANY-SOURCES ANY-BUFFER)" nil nil)

(autoload 'helm-toggle-suspend-update "helm/helm" "\
Enable or disable update of display in helm.
This can be useful for e.g writing quietly a complex regexp.

\(fn)" t nil)

(autoload 'helm-force-update "helm/helm" "\
Force recalculation and update of candidates.
The difference with `helm-update' is this function is reevaling
the `init' and `update' attributes functions when present
before updating candidates according to pattern i.e calling `helm-update'.
Selection is preserved to current candidate or moved to PRESELECT
if specified.

\(fn &optional PRESELECT)" t nil)

(autoload 'helm-select-action "helm/helm" "\
Select an action for the currently selected candidate.
If action buffer is selected, back to the helm buffer.

\(fn)" t nil)

(autoload 'helm-previous-line "helm/helm" "\
Move selection to the previous line.

\(fn)" t nil)

(autoload 'helm-next-line "helm/helm" "\
Move selection to the next line.

\(fn)" t nil)

(autoload 'helm-previous-page "helm/helm" "\
Move selection back with a pageful.

\(fn)" t nil)

(autoload 'helm-next-page "helm/helm" "\
Move selection forward with a pageful.

\(fn)" t nil)

(autoload 'helm-beginning-of-buffer "helm/helm" "\
Move selection at the top.

\(fn)" t nil)

(autoload 'helm-end-of-buffer "helm/helm" "\
Move selection at the bottom.

\(fn)" t nil)

(autoload 'helm-previous-source "helm/helm" "\
Move selection to the previous source.

\(fn)" t nil)

(autoload 'helm-next-source "helm/helm" "\
Move selection to the next source.

\(fn)" t nil)

(autoload 'helm-confirm-and-exit-minibuffer "helm/helm" "\
Maybe ask for confirmation when exiting helm.
It is similar to `minibuffer-complete-and-exit' adapted to helm.
If `minibuffer-completion-confirm' value is 'confirm,
send in minibuffer confirm message and exit on next hit.
If `minibuffer-completion-confirm' value is t,
don't exit and send message 'no match'.

\(fn)" t nil)

(autoload 'helm-exit-minibuffer "helm/helm" "\
Select the current candidate by exiting the minibuffer.

\(fn)" t nil)

(autoload 'helm-keyboard-quit "helm/helm" "\
Quit minibuffer in helm.
If action buffer is displayed, kill it.

\(fn)" t nil)

(autoload 'helm-debug-output "helm/helm" "\
Show all helm-related variables at this time.

\(fn)" t nil)

(autoload 'helm-delete-current-selection "helm/helm" "\
Delete the currently selected item.

\(fn)" t nil)

(autoload 'helm-delete-minibuffer-contents "helm/helm" "\
Delete minibuffer contents.
When called with a prefix arg or when
`helm-delete-minibuffer-contents-from-point' is non--nil,
delete minibuffer contents from point instead of deleting all.

\(fn &optional ARG)" t nil)

(autoload 'helm-toggle-resplit-window "helm/helm" "\
Toggle resplit helm window, vertically or horizontally.

\(fn)" t nil)

(autoload 'helm-narrow-window "helm/helm" "\
Narrow helm window.

\(fn)" t nil)

(autoload 'helm-enlarge-window "helm/helm" "\
Enlarge helm window.

\(fn)" t nil)

(autoload 'helm-swap-windows "helm/helm" "\
Swap window holding `helm-buffer' with other window.

\(fn)" t nil)

(autoload 'helm-select-2nd-action "helm/helm" "\
Select the 2nd action for the currently selected candidate.

\(fn)" t nil)

(autoload 'helm-select-3rd-action "helm/helm" "\
Select the 3rd action for the currently selected candidate.

\(fn)" t nil)

(autoload 'helm-select-4th-action "helm/helm" "\
Select the 4th action for the currently selected candidate.

\(fn)" t nil)

(autoload 'helm-select-2nd-action-or-end-of-line "helm/helm" "\
Select the 2nd action for the currently selected candidate.
This happen when point is at the end of minibuffer.
Otherwise goto the end of minibuffer.

\(fn)" t nil)

(autoload 'helm-execute-persistent-action "helm/helm" "\
Perform the associated action ATTR without quitting helm.
ATTR default is 'persistent-action', but it can be anything else.
In this case you have to add this new attribute to your source.

When `helm-full-frame' or SPLIT-ONEWINDOW are non--nil,
and `helm-buffer' is displayed in only one window,
the helm window is splitted to display
`helm-select-persistent-action-window' in other window 
and keep its visibility.

\(fn &optional (attr (quote persistent-action)) SPLIT-ONEWINDOW)" t nil)

(autoload 'helm-scroll-other-window "helm/helm" "\
Scroll other window (not *Helm* window) upward.

\(fn)" t nil)

(autoload 'helm-scroll-other-window-down "helm/helm" "\
Scroll other window (not *Helm* window) downward.

\(fn)" t nil)

(autoload 'helm-recenter-top-bottom-other-window "helm/helm" "\
`recenter-top-bottom' in other window (not *Helm* window).

\(fn)" t nil)

(autoload 'helm-reposition-window-other-window "helm/helm" "\
`helm-reposition-window' in other window (not *Helm* window).

\(fn)" t nil)

(autoload 'helm-toggle-visible-mark "helm/helm" "\
Toggle helm visible mark at point.

\(fn)" t nil)

(autoload 'helm-mark-all "helm/helm" "\
Mark all visible unmarked candidates in current source.

\(fn)" t nil)

(autoload 'helm-unmark-all "helm/helm" "\
Unmark all candidates in all sources of current helm session.

\(fn)" t nil)

(autoload 'helm-toggle-all-marks "helm/helm" "\
Toggle all marks.
Mark all visible candidates of current source or unmark all candidates
visible or invisible in all sources of current helm session

\(fn)" t nil)

(autoload 'helm-display-all-visible-marks "helm/helm" "\
Show all `helm' visible marks strings.
Only useful for debugging.

\(fn)" t nil)

(autoload 'helm-next-visible-mark "helm/helm" "\
Move next helm visible mark.
If PREV is non-nil move to precedent.

\(fn &optional PREV)" t nil)

(autoload 'helm-prev-visible-mark "helm/helm" "\
Move previous helm visible mark.

\(fn)" t nil)

(autoload 'helm-yank-selection "helm/helm" "\
Set minibuffer contents to current display selection.
With a prefix arg set to real value of current selection.

\(fn ARG)" t nil)

(autoload 'helm-kill-selection-and-quit "helm/helm" "\
Store current selection to kill ring.
With a prefix arg set to real value of current selection.

\(fn ARG)" t nil)

(autoload 'helm-follow-mode "helm/helm" "\
Execute persistent action everytime the cursor is moved when enabled.
The mode is enabled for the current source only, you will have to turn it
on again when you go to next source if you want it there also.
This mode can be enabled or disabled interactively at anytime during
helm session or enabled specifically by source by adding the `follow'
attribute to this source.
Even when the attribute `follow' exists in source, it is still possible
to disable/enable this mode interactively.
Note that when you disable it interactively and `follow' attribute exists,
`helm-follow-mode' will be disabled on next helm session even if `follow'
attribute is specified in source. To avoid this set your `follow' attribute
in source in `helm-before-initialize-hook'.

e.g:

\(add-hook 'helm-before-initialize-hook
          #'(lambda () (helm-attrset 'follow 1 helm-source-buffers-list)))

This will enable `helm-follow-mode' automatically in `helm-source-buffers-list'.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (helm-reset-adaptative-history) "helm/helm-adaptative"
;;;;;;  "helm/helm-adaptative.el" (20920 10007))
;;; Generated autoloads from helm/helm-adaptative.el

(autoload 'helm-reset-adaptative-history "helm/helm-adaptative" "\
Delete all `helm-adaptive-history' and his file.
Useful when you have a old or corrupted `helm-adaptive-history-file'.

\(fn)" t nil)

;;;***

;;;### (autoloads (helm-apt helm-apt-show-all helm-apt-show-only-deinstalled
;;;;;;  helm-apt-show-only-not-installed helm-apt-show-only-installed)
;;;;;;  "helm/helm-apt" "helm/helm-apt.el" (20920 10007))
;;; Generated autoloads from helm/helm-apt.el

(autoload 'helm-apt-show-only-installed "helm/helm-apt" "\


\(fn)" t nil)

(autoload 'helm-apt-show-only-not-installed "helm/helm-apt" "\


\(fn)" t nil)

(autoload 'helm-apt-show-only-deinstalled "helm/helm-apt" "\


\(fn)" t nil)

(autoload 'helm-apt-show-all "helm/helm-apt" "\


\(fn)" t nil)

(autoload 'helm-apt "helm/helm-apt" "\
Preconfigured `helm' : frontend of APT package manager.
With a prefix arg reload cache.

\(fn ARG)" t nil)

;;;***

;;;### (autoloads (helm-bbdb) "helm/helm-bbdb" "helm/helm-bbdb.el"
;;;;;;  (20920 10007))
;;; Generated autoloads from helm/helm-bbdb.el

(autoload 'helm-bbdb "helm/helm-bbdb" "\
Preconfigured `helm' for BBDB.

Needs BBDB.

http://bbdb.sourceforge.net/

\(fn)" t nil)

;;;***

;;;### (autoloads (helm-bookmark-ext helm-bmkext-run-edit) "helm/helm-bmkext"
;;;;;;  "helm/helm-bmkext.el" (20920 10007))
;;; Generated autoloads from helm/helm-bmkext.el

(autoload 'helm-bmkext-run-edit "helm/helm-bmkext" "\
Run `bmkext-edit-bookmark' from keyboard.

\(fn)" t nil)

(autoload 'helm-bookmark-ext "helm/helm-bmkext" "\
Preconfigured `helm' for bookmark-extensions sources.
Needs bookmark-ext.el:
<http://mercurial.intuxication.org/hg/emacs-bookmark-extension>.
Contain also `helm-source-google-suggest'.

\(fn)" t nil)

;;;***

;;;### (autoloads (helm-pp-bookmarks helm-bookmarks helm-bookmark-run-delete
;;;;;;  helm-bookmark-run-jump-other-window helm-bookmark-toggle-filename)
;;;;;;  "helm/helm-bookmark" "helm/helm-bookmark.el" (20920 10007))
;;; Generated autoloads from helm/helm-bookmark.el

(autoload 'helm-bookmark-toggle-filename "helm/helm-bookmark" "\
Toggle bookmark location visibility.

\(fn)" t nil)

(autoload 'helm-bookmark-run-jump-other-window "helm/helm-bookmark" "\
Jump to bookmark from keyboard.

\(fn)" t nil)

(autoload 'helm-bookmark-run-delete "helm/helm-bookmark" "\
Delete bookmark from keyboard.

\(fn)" t nil)

(autoload 'helm-bookmarks "helm/helm-bookmark" "\
Preconfigured `helm' for bookmarks.

\(fn)" t nil)

(autoload 'helm-pp-bookmarks "helm/helm-bookmark" "\
Preconfigured `helm' for bookmarks (pretty-printed).

\(fn)" t nil)

;;;***

;;;### (autoloads (helm-buffers-list helm-buffers-run-multi-occur
;;;;;;  helm-buffer-run-ediff helm-buffer-switch-to-elscreen helm-buffer-switch-other-frame
;;;;;;  helm-buffer-switch-other-window helm-buffer-run-query-replace
;;;;;;  helm-buffer-run-query-replace-regexp helm-buffer-run-zgrep
;;;;;;  helm-buffer-run-grep helm-buffer-run-kill-buffers helm-buffer-save-persistent
;;;;;;  helm-buffer-revert-persistent helm-buffer-diff-persistent)
;;;;;;  "helm/helm-buffers" "helm/helm-buffers.el" (20920 10007))
;;; Generated autoloads from helm/helm-buffers.el

(autoload 'helm-buffer-diff-persistent "helm/helm-buffers" "\
Toggle diff buffer without quitting helm.

\(fn)" t nil)

(autoload 'helm-buffer-revert-persistent "helm/helm-buffers" "\
Revert buffer without quitting helm.

\(fn)" t nil)

(autoload 'helm-buffer-save-persistent "helm/helm-buffers" "\
Save buffer without quitting helm.

\(fn)" t nil)

(autoload 'helm-buffer-run-kill-buffers "helm/helm-buffers" "\
Run kill buffer action from `helm-source-buffers-list'.

\(fn)" t nil)

(autoload 'helm-buffer-run-grep "helm/helm-buffers" "\
Run Grep action from `helm-source-buffers-list'.

\(fn)" t nil)

(autoload 'helm-buffer-run-zgrep "helm/helm-buffers" "\
Run Grep action from `helm-source-buffers-list'.

\(fn)" t nil)

(autoload 'helm-buffer-run-query-replace-regexp "helm/helm-buffers" "\
Run Query replace regexp action from `helm-source-buffers-list'.

\(fn)" t nil)

(autoload 'helm-buffer-run-query-replace "helm/helm-buffers" "\
Run Query replace action from `helm-source-buffers-list'.

\(fn)" t nil)

(autoload 'helm-buffer-switch-other-window "helm/helm-buffers" "\
Run switch to other window action from `helm-source-buffers-list'.

\(fn)" t nil)

(autoload 'helm-buffer-switch-other-frame "helm/helm-buffers" "\
Run switch to other frame action from `helm-source-buffers-list'.

\(fn)" t nil)

(autoload 'helm-buffer-switch-to-elscreen "helm/helm-buffers" "\
Run switch to elscreen  action from `helm-source-buffers-list'.

\(fn)" t nil)

(autoload 'helm-buffer-run-ediff "helm/helm-buffers" "\
Run ediff action from `helm-source-buffers-list'.

\(fn)" t nil)

(autoload 'helm-buffers-run-multi-occur "helm/helm-buffers" "\
Run `helm-multi-occur-as-action' by key.

\(fn)" t nil)

(autoload 'helm-buffers-list "helm/helm-buffers" "\
Preconfigured `helm' to list buffers.
It is an enhanced version of `helm-for-buffers'.

\(fn)" t nil)

;;;***

;;;### (autoloads (helm-simple-call-tree) "helm/helm-call-tree" "helm/helm-call-tree.el"
;;;;;;  (20920 10007))
;;; Generated autoloads from helm/helm-call-tree.el

(autoload 'helm-simple-call-tree "helm/helm-call-tree" "\
Preconfigured `helm' for simple-call-tree. List function relationships.

Needs simple-call-tree.el.
http://www.emacswiki.org/cgi-bin/wiki/download/simple-call-tree.el

\(fn)" t nil)

;;;***

;;;### (autoloads (helm-colors) "helm/helm-color" "helm/helm-color.el"
;;;;;;  (20920 10007))
;;; Generated autoloads from helm/helm-color.el

(autoload 'helm-colors "helm/helm-color" "\
Preconfigured `helm' for color.

\(fn)" t nil)

;;;***

;;;### (autoloads (helm-M-x) "helm/helm-command" "helm/helm-command.el"
;;;;;;  (20920 10007))
;;; Generated autoloads from helm/helm-command.el

(autoload 'helm-M-x "helm/helm-command" "\
Preconfigured `helm' for Emacs commands.
It is `helm' replacement of regular `M-x' `execute-extended-command'.

\(fn)" t nil)

;;;***

;;;### (autoloads (helm-configuration) "helm/helm-config" "helm/helm-config.el"
;;;;;;  (20920 10007))
;;; Generated autoloads from helm/helm-config.el

(autoload 'helm-configuration "helm/helm-config" "\
Customize `helm'.

\(fn)" t nil)

;;;***

;;;### (autoloads (helm-complex-command-history helm-timers helm-manage-advice
;;;;;;  helm-apropos helm-lisp-completion-or-file-name-at-point helm-lisp-indent
;;;;;;  helm-complete-file-name-at-point helm-lisp-completion-at-point)
;;;;;;  "helm/helm-elisp" "helm/helm-elisp.el" (20920 10007))
;;; Generated autoloads from helm/helm-elisp.el

(autoload 'helm-lisp-completion-at-point "helm/helm-elisp" "\
Helm lisp symbol completion at point.

\(fn)" t nil)

(autoload 'helm-complete-file-name-at-point "helm/helm-elisp" "\
Complete file name at point.

\(fn &optional FORCE)" t nil)

(autoload 'helm-lisp-indent "helm/helm-elisp" "\


\(fn)" t nil)

(autoload 'helm-lisp-completion-or-file-name-at-point "helm/helm-elisp" "\
Complete lisp symbol or filename at point.
Filename completion happen if string start after or between a double quote.

\(fn)" t nil)

(autoload 'helm-apropos "helm/helm-elisp" "\
Preconfigured helm to describe commands, functions, variables and faces.

\(fn)" t nil)

(autoload 'helm-manage-advice "helm/helm-elisp" "\
Preconfigured `helm' to disable/enable function advices.

\(fn)" t nil)

(autoload 'helm-timers "helm/helm-elisp" "\
Preconfigured `helm' for timers.

\(fn)" t nil)

(autoload 'helm-complex-command-history "helm/helm-elisp" "\


\(fn)" t nil)

;;;***

;;;### (autoloads (helm-elscreen) "helm/helm-elscreen" "helm/helm-elscreen.el"
;;;;;;  (20920 10007))
;;; Generated autoloads from helm/helm-elscreen.el

(autoload 'helm-elscreen "helm/helm-elscreen" "\
Preconfigured helm to list elscreen.

\(fn)" t nil)

;;;***

;;;### (autoloads (helm-emms) "helm/helm-emms" "helm/helm-emms.el"
;;;;;;  (20920 10007))
;;; Generated autoloads from helm/helm-emms.el

(autoload 'helm-emms "helm/helm-emms" "\
Preconfigured `helm' for emms sources.

\(fn)" t nil)

;;;***

;;;### (autoloads (helm-eshell-history helm-esh-pcomplete) "helm/helm-eshell"
;;;;;;  "helm/helm-eshell.el" (20920 10007))
;;; Generated autoloads from helm/helm-eshell.el

(autoload 'helm-esh-pcomplete "helm/helm-eshell" "\
Preconfigured helm to provide helm completion in eshell.

\(fn)" t nil)

(autoload 'helm-eshell-history "helm/helm-eshell" "\
Preconfigured helm for eshell history.

\(fn)" t nil)

;;;***

;;;### (autoloads (helm-calcul-expression helm-eval-expression-with-eldoc
;;;;;;  helm-eval-expression) "helm/helm-eval" "helm/helm-eval.el"
;;;;;;  (20920 10007))
;;; Generated autoloads from helm/helm-eval.el

(autoload 'helm-eval-expression "helm/helm-eval" "\
Preconfigured helm for `helm-source-evaluation-result'.

\(fn ARG)" t nil)

(autoload 'helm-eval-expression-with-eldoc "helm/helm-eval" "\
Preconfigured helm for `helm-source-evaluation-result' with `eldoc' support. 

\(fn)" t nil)

(autoload 'helm-calcul-expression "helm/helm-eval" "\
Preconfigured helm for `helm-source-calculation-result'.

\(fn)" t nil)

;;;***

;;;### (autoloads (helm-run-external-command) "helm/helm-external"
;;;;;;  "helm/helm-external.el" (20920 10007))
;;; Generated autoloads from helm/helm-external.el

(autoload 'helm-run-external-command "helm/helm-external" "\
Preconfigured `helm' to run External PROGRAM asyncronously from Emacs.
If program is already running exit with error.
You can set your own list of commands with
`helm-external-commands-list'.

\(fn PROGRAM)" t nil)

;;;***

;;;### (autoloads (helm-recentf helm-for-files helm-find-files helm-find
;;;;;;  helm-ff-run-find-sh-command helm-ff-run-browse-project helm-browse-project
;;;;;;  helm-ff-file-name-history helm-ff-rotate-right-persistent
;;;;;;  helm-ff-rotate-left-persistent helm-ff-run-kill-buffer-persistent
;;;;;;  helm-ff-persistent-delete helm-ff-properties-persistent helm-find-files-down-one-level
;;;;;;  helm-ff-run-toggle-basename helm-ff-run-print-file helm-ff-run-etags
;;;;;;  helm-ff-run-gnus-attach-files helm-ff-run-find-file-as-root
;;;;;;  helm-ff-run-locate helm-ff-run-open-file-with-default-tool
;;;;;;  helm-ff-run-open-file-externally helm-ff-run-switch-other-frame
;;;;;;  helm-ff-run-switch-other-window helm-ff-run-switch-to-eshell
;;;;;;  helm-ff-run-complete-fn-at-point helm-ff-run-delete-file
;;;;;;  helm-ff-run-hardlink-file helm-ff-run-symlink-file helm-ff-run-ediff-merge-file
;;;;;;  helm-ff-run-ediff-file helm-ff-run-eshell-command-on-file
;;;;;;  helm-ff-run-load-file helm-ff-run-byte-compile-file helm-ff-run-rename-file
;;;;;;  helm-ff-run-copy-file helm-ff-run-zgrep helm-ff-run-pdfgrep
;;;;;;  helm-ff-run-grep helm-ff-run-switch-to-history helm-ff-run-toggle-auto-update)
;;;;;;  "helm/helm-files" "helm/helm-files.el" (20920 10007))
;;; Generated autoloads from helm/helm-files.el

(autoload 'helm-ff-run-toggle-auto-update "helm/helm-files" "\


\(fn)" t nil)

(autoload 'helm-ff-run-switch-to-history "helm/helm-files" "\
Run Switch to history action from `helm-source-find-files'.

\(fn)" t nil)

(autoload 'helm-ff-run-grep "helm/helm-files" "\
Run Grep action from `helm-source-find-files'.

\(fn)" t nil)

(autoload 'helm-ff-run-pdfgrep "helm/helm-files" "\
Run Pdfgrep action from `helm-source-find-files'.

\(fn)" t nil)

(autoload 'helm-ff-run-zgrep "helm/helm-files" "\
Run Grep action from `helm-source-find-files'.

\(fn)" t nil)

(autoload 'helm-ff-run-copy-file "helm/helm-files" "\
Run Copy file action from `helm-source-find-files'.

\(fn)" t nil)

(autoload 'helm-ff-run-rename-file "helm/helm-files" "\
Run Rename file action from `helm-source-find-files'.

\(fn)" t nil)

(autoload 'helm-ff-run-byte-compile-file "helm/helm-files" "\
Run Byte compile file action from `helm-source-find-files'.

\(fn)" t nil)

(autoload 'helm-ff-run-load-file "helm/helm-files" "\
Run Load file action from `helm-source-find-files'.

\(fn)" t nil)

(autoload 'helm-ff-run-eshell-command-on-file "helm/helm-files" "\
Run eshell command on file action from `helm-source-find-files'.

\(fn)" t nil)

(autoload 'helm-ff-run-ediff-file "helm/helm-files" "\
Run Ediff file action from `helm-source-find-files'.

\(fn)" t nil)

(autoload 'helm-ff-run-ediff-merge-file "helm/helm-files" "\
Run Ediff merge file action from `helm-source-find-files'.

\(fn)" t nil)

(autoload 'helm-ff-run-symlink-file "helm/helm-files" "\
Run Symlink file action from `helm-source-find-files'.

\(fn)" t nil)

(autoload 'helm-ff-run-hardlink-file "helm/helm-files" "\
Run Hardlink file action from `helm-source-find-files'.

\(fn)" t nil)

(autoload 'helm-ff-run-delete-file "helm/helm-files" "\
Run Delete file action from `helm-source-find-files'.

\(fn)" t nil)

(autoload 'helm-ff-run-complete-fn-at-point "helm/helm-files" "\
Run complete file name action from `helm-source-find-files'.

\(fn)" t nil)

(autoload 'helm-ff-run-switch-to-eshell "helm/helm-files" "\
Run switch to eshell action from `helm-source-find-files'.

\(fn)" t nil)

(autoload 'helm-ff-run-switch-other-window "helm/helm-files" "\
Run switch to other window action from `helm-source-find-files'.

\(fn)" t nil)

(autoload 'helm-ff-run-switch-other-frame "helm/helm-files" "\
Run switch to other frame action from `helm-source-find-files'.

\(fn)" t nil)

(autoload 'helm-ff-run-open-file-externally "helm/helm-files" "\
Run open file externally command action from `helm-source-find-files'.

\(fn)" t nil)

(autoload 'helm-ff-run-open-file-with-default-tool "helm/helm-files" "\
Run open file externally command action from `helm-source-find-files'.

\(fn)" t nil)

(autoload 'helm-ff-run-locate "helm/helm-files" "\
Run locate action from `helm-source-find-files'.

\(fn)" t nil)

(autoload 'helm-ff-run-find-file-as-root "helm/helm-files" "\


\(fn)" t nil)

(autoload 'helm-ff-run-gnus-attach-files "helm/helm-files" "\
Run gnus attach files command action from `helm-source-find-files'.

\(fn)" t nil)

(autoload 'helm-ff-run-etags "helm/helm-files" "\
Run Etags command action from `helm-source-find-files'.

\(fn)" t nil)

(autoload 'helm-ff-run-print-file "helm/helm-files" "\
Run Print file action from `helm-source-find-files'.

\(fn)" t nil)

(autoload 'helm-ff-run-toggle-basename "helm/helm-files" "\


\(fn)" t nil)

(autoload 'helm-find-files-down-one-level "helm/helm-files" "\
Go down one level like unix command `cd ..'.
If prefix numeric arg is given go ARG level down.

\(fn ARG)" t nil)

(autoload 'helm-ff-properties-persistent "helm/helm-files" "\
Show properties without quitting helm.

\(fn)" t nil)

(autoload 'helm-ff-persistent-delete "helm/helm-files" "\
Delete current candidate without quitting.

\(fn)" t nil)

(autoload 'helm-ff-run-kill-buffer-persistent "helm/helm-files" "\
Execute `helm-ff-kill-buffer-fname' whitout quitting.

\(fn)" t nil)

(autoload 'helm-ff-rotate-left-persistent "helm/helm-files" "\
Rotate image left without quitting helm.

\(fn)" t nil)

(autoload 'helm-ff-rotate-right-persistent "helm/helm-files" "\
Rotate image right without quitting helm.

\(fn)" t nil)

(autoload 'helm-ff-file-name-history "helm/helm-files" "\
Switch to `file-name-history' without quitting `helm-find-files'.

\(fn)" t nil)

(autoload 'helm-browse-project "helm/helm-files" "\
Browse files and see status of project with its vcs.
Only hg and git are supported for now.
Fall back to `helm-find-files' if directory is not under
control of one of those vcs.
Need dependencies:
<https://github.com/emacs-helm/helm-ls-git.git>
and
<https://github.com/emacs-helm/helm-mercurial-queue/blob/master/helm-ls-hg.el>.

\(fn)" t nil)

(autoload 'helm-ff-run-browse-project "helm/helm-files" "\


\(fn)" t nil)

(autoload 'helm-ff-run-find-sh-command "helm/helm-files" "\
Run find shell command action with key from `helm-find-files'.

\(fn)" t nil)

(autoload 'helm-find "helm/helm-files" "\
Preconfigured `helm' for the find shell command.

\(fn ARG)" t nil)

(autoload 'helm-find-files "helm/helm-files" "\
Preconfigured `helm' for helm implementation of `find-file'.
Called with a prefix arg show history if some.
Don't call it from programs, use `helm-find-files-1' instead.
This is the starting point for nearly all actions you can do on files.

\(fn ARG)" t nil)

(autoload 'helm-for-files "helm/helm-files" "\
Preconfigured `helm' for opening files.
Run all sources defined in `helm-for-files-preferred-list'.

\(fn)" t nil)

(autoload 'helm-recentf "helm/helm-files" "\
Preconfigured `helm' for `recentf'.

\(fn)" t nil)

;;;***

;;;### (autoloads (helm-firefox-bookmarks) "helm/helm-firefox" "helm/helm-firefox.el"
;;;;;;  (20920 10007))
;;; Generated autoloads from helm/helm-firefox.el

(autoload 'helm-firefox-bookmarks "helm/helm-firefox" "\
Preconfigured `helm' for firefox bookmark.
You will have to enable html bookmarks in firefox:
open about:config in firefox and double click on this line to enable value to true:

user_pref(\"browser.bookmarks.autoExportHTML\", false);

You should have now:

user_pref(\"browser.bookmarks.autoExportHTML\", true);

After closing firefox, you will be able to browse you bookmarks.

\(fn)" t nil)

;;;***

;;;### (autoloads (helm-ucs helm-select-xfont) "helm/helm-font" "helm/helm-font.el"
;;;;;;  (20920 10007))
;;; Generated autoloads from helm/helm-font.el

(autoload 'helm-select-xfont "helm/helm-font" "\
Preconfigured `helm' to select Xfont.

\(fn)" t nil)

(autoload 'helm-ucs "helm/helm-font" "\
Preconfigured helm for `ucs-names' math symbols.

\(fn)" t nil)

;;;***

;;;### (autoloads (helm-gentoo) "helm/helm-gentoo" "helm/helm-gentoo.el"
;;;;;;  (20920 10007))
;;; Generated autoloads from helm/helm-gentoo.el

(autoload 'helm-gentoo "helm/helm-gentoo" "\
Preconfigured `helm' for gentoo linux.

\(fn)" t nil)

;;;***

;;;### (autoloads (helm-do-pdfgrep helm-do-zgrep helm-do-grep helm-grep-mode-jump-other-window
;;;;;;  helm-grep-mode-jump-other-window-backward helm-grep-mode-jump-other-window-forward
;;;;;;  helm-grep-mode-jump helm-grep-mode-quit helm-gm-precedent-file
;;;;;;  helm-gm-next-file helm-grep-mode helm-grep-run-save-buffer
;;;;;;  helm-grep-run-other-frame-action helm-grep-run-other-window-action
;;;;;;  helm-grep-run-default-action helm-grep-run-persistent-action
;;;;;;  helm-goto-next-file helm-goto-precedent-file) "helm/helm-grep"
;;;;;;  "helm/helm-grep.el" (20920 10007))
;;; Generated autoloads from helm/helm-grep.el

(autoload 'helm-goto-precedent-file "helm/helm-grep" "\
Go to precedent file in helm grep/etags buffers.

\(fn)" t nil)

(autoload 'helm-goto-next-file "helm/helm-grep" "\
Go to precedent file in helm grep/etags buffers.

\(fn)" t nil)

(autoload 'helm-grep-run-persistent-action "helm/helm-grep" "\
Run grep persistent action from `helm-do-grep-1'.

\(fn)" t nil)

(autoload 'helm-grep-run-default-action "helm/helm-grep" "\
Run grep default action from `helm-do-grep-1'.

\(fn)" t nil)

(autoload 'helm-grep-run-other-window-action "helm/helm-grep" "\
Run grep goto other window action from `helm-do-grep-1'.

\(fn)" t nil)

(autoload 'helm-grep-run-other-frame-action "helm/helm-grep" "\
Run grep goto other frame action from `helm-do-grep-1'.

\(fn)" t nil)

(autoload 'helm-grep-run-save-buffer "helm/helm-grep" "\
Run grep save results action from `helm-do-grep-1'.

\(fn)" t nil)

(autoload 'helm-grep-mode "helm/helm-grep" "\
Major mode to provide actions in helm grep saved buffer.

Special commands:
\\{helm-grep-mode-map}

\(fn)" t nil)

(autoload 'helm-gm-next-file "helm/helm-grep" "\


\(fn)" t nil)

(autoload 'helm-gm-precedent-file "helm/helm-grep" "\


\(fn)" t nil)

(autoload 'helm-grep-mode-quit "helm/helm-grep" "\


\(fn)" t nil)

(autoload 'helm-grep-mode-jump "helm/helm-grep" "\


\(fn)" t nil)

(autoload 'helm-grep-mode-jump-other-window-forward "helm/helm-grep" "\


\(fn)" t nil)

(autoload 'helm-grep-mode-jump-other-window-backward "helm/helm-grep" "\


\(fn)" t nil)

(autoload 'helm-grep-mode-jump-other-window "helm/helm-grep" "\


\(fn)" t nil)

(autoload 'helm-do-grep "helm/helm-grep" "\
Preconfigured helm for grep.
Contrarily to Emacs `grep', no default directory is given, but
the full path of candidates in ONLY.
That allow to grep different files not only in `default-directory' but anywhere
by marking them (C-<SPACE>). If one or more directory is selected
grep will search in all files of these directories.
You can also use wildcard in the base name of candidate.
If a prefix arg is given use the -r option of grep (recurse).
The prefix arg can be passed before or after start file selection.
See also `helm-do-grep-1'.

\(fn)" t nil)

(autoload 'helm-do-zgrep "helm/helm-grep" "\
Preconfigured helm for zgrep.

\(fn)" t nil)

(autoload 'helm-do-pdfgrep "helm/helm-grep" "\
Preconfigured helm for pdfgrep.

\(fn)" t nil)

;;;***

;;;### (autoloads (helm-describe-helm-attribute helm-apt-help helm-top-help
;;;;;;  helm-moccur-help helm-buffers-ido-virtual-help helm-esh-help
;;;;;;  helm-bookmark-help helm-ucs-help helm-etags-help helm-pdfgrep-help
;;;;;;  helm-grep-help helm-generic-file-help helm-read-file-name-help
;;;;;;  helm-ff-help helm-buffer-help helm-help) "helm/helm-help"
;;;;;;  "helm/helm-help.el" (20920 10007))
;;; Generated autoloads from helm/helm-help.el

(defvar helm-mode-line-string "\\<helm-map>\\[helm-help]:Help \\[helm-select-action]:Act \\[helm-exit-minibuffer]/\\[helm-select-2nd-action-or-end-of-line]/\\[helm-select-3rd-action]:NthAct" "\
Help string displayed in mode-line in `helm'.
It can be a string or a list of two args, in this case,
first arg is a string that will be used as name for candidates number,
second arg any string to display in mode line.
If nil, use default `mode-line-format'.")

(autoload 'helm-help "helm/helm-help" "\
Help of `helm'.

\(fn)" t nil)

(autoload 'helm-buffer-help "helm/helm-help" "\
Help command for helm buffers.

\(fn)" t nil)

(autoload 'helm-ff-help "helm/helm-help" "\
Help command for `helm-find-files'.

\(fn)" t nil)

(autoload 'helm-read-file-name-help "helm/helm-help" "\


\(fn)" t nil)

(autoload 'helm-generic-file-help "helm/helm-help" "\


\(fn)" t nil)

(autoload 'helm-grep-help "helm/helm-help" "\


\(fn)" t nil)

(autoload 'helm-pdfgrep-help "helm/helm-help" "\


\(fn)" t nil)

(autoload 'helm-etags-help "helm/helm-help" "\
The help function for etags.

\(fn)" t nil)

(autoload 'helm-ucs-help "helm/helm-help" "\
Help command for `helm-ucs'.

\(fn)" t nil)

(autoload 'helm-bookmark-help "helm/helm-help" "\
Help command for bookmarks.

\(fn)" t nil)

(autoload 'helm-esh-help "helm/helm-help" "\
Help command for `helm-find-files-eshell-command-on-file'.

\(fn)" t nil)

(autoload 'helm-buffers-ido-virtual-help "helm/helm-help" "\
Help command for ido virtual buffers.

\(fn)" t nil)

(autoload 'helm-moccur-help "helm/helm-help" "\


\(fn)" t nil)

(autoload 'helm-top-help "helm/helm-help" "\


\(fn)" t nil)

(autoload 'helm-apt-help "helm/helm-help" "\


\(fn)" t nil)

(defvar helm-buffer-mode-line-string '("Buffer(s)" "\\<helm-buffer-map>\\[helm-buffer-help]:Help \\<helm-map>\\[helm-select-action]:Act \\[helm-exit-minibuffer]/\\[helm-select-2nd-action-or-end-of-line]/\\[helm-select-3rd-action]:NthAct" "String displayed in mode-line in `helm-source-buffers-list'"))

(defvar helm-buffers-ido-virtual-mode-line-string '("Killed Buffer(s)" "\\<helm-buffers-ido-virtual-map>\\[helm-buffers-ido-virtual-help]:Help \\<helm-map>\\[helm-select-action]:Act \\[helm-exit-minibuffer]/\\[helm-select-2nd-action-or-end-of-line]/\\[helm-select-3rd-action]:NthAct" "String displayed in mode-line in `helm-source-buffers-list'"))

(defvar helm-ff-mode-line-string "\\<helm-find-files-map>\\[helm-ff-help]:Help \\<helm-map>\\[helm-select-action]:Act \\[helm-exit-minibuffer]/\\[helm-select-2nd-action-or-end-of-line]/\\[helm-select-3rd-action]:NthAct" "\
String displayed in mode-line in `helm-source-find-files'")

(defvar helm-read-file-name-mode-line-string "\\<helm-read-file-map>\\[helm-read-file-name-help]:Help \\[helm-cr-empty-string]:Empty \\<helm-map>\\[helm-select-action]:Act \\[helm-exit-minibuffer]/\\[helm-select-2nd-action-or-end-of-line]/\\[helm-select-3rd-action]:NthAct" "\
String displayed in mode-line in `helm-source-find-files'.")

(defvar helm-generic-file-mode-line-string "\\<helm-generic-files-map>\\[helm-generic-file-help]:Help \\<helm-map>\\[helm-select-action]:Act \\[helm-exit-minibuffer]/\\[helm-select-2nd-action-or-end-of-line]/\\[helm-select-3rd-action]:NthAct \\[helm-toggle-suspend-update]:Tog.suspend" "\
String displayed in mode-line in Locate.")

(defvar helm-grep-mode-line-string "\\<helm-grep-map>\\[helm-grep-help]:Help \\<helm-map>\\[helm-select-action]:Act \\[helm-exit-minibuffer]/\\[helm-select-2nd-action-or-end-of-line]/\\[helm-select-3rd-action]:NthAct \\[helm-toggle-suspend-update]:Tog.suspend" "\
String displayed in mode-line in `helm-do-grep'.")

(defvar helm-pdfgrep-mode-line-string "\\<helm-pdfgrep-map>\\[helm-pdfgrep-help]:Help \\<helm-map>\\[helm-select-action]:Act \\[helm-exit-minibuffer]/\\[helm-select-2nd-action-or-end-of-line]/\\[helm-select-3rd-action]:NthAct \\[helm-toggle-suspend-update]:Tog.suspend" "\
String displayed in mode-line in `helm-do-pdfgrep'.")

(defvar helm-etags-mode-line-string "\\<helm-etags-map>\\[helm-etags-help]:Help \\<helm-map>\\[helm-select-action]:Act \\[helm-exit-minibuffer]/\\[helm-select-2nd-action-or-end-of-line]/\\[helm-select-3rd-action]:NthAct" "\
String displayed in mode-line in `helm-etags-select'.")

(defvar helm-ucs-mode-line-string "\\<helm-ucs-map>\\[helm-ucs-help]:Help \\<helm-map>\\[helm-select-action]:Act \\[helm-exit-minibuffer]/\\[helm-select-2nd-action-or-end-of-line]/\\[helm-select-3rd-action]:NthAct" "\
String displayed in mode-line in `helm-ucs'.")

(defvar helm-bookmark-mode-line-string '("Bookmark(s)" "\\<helm-bookmark-map>\\[helm-bookmark-help]:Help \\<helm-map>\\[helm-select-action]:Act \\[helm-exit-minibuffer]/\\[helm-select-2nd-action-or-end-of-line]/\\[helm-select-3rd-action]:NthAct") "\
String displayed in mode-line in `helm-source-buffers-list'")

(defvar helm-occur-mode-line "\\<helm-map>\\[helm-help]:Help \\<helm-occur-map>\\[helm-occur-run-query-replace-regexp]:Query replace regexp \\<helm-map>\\[helm-select-action]:Act \\[helm-exit-minibuffer]/\\[helm-select-2nd-action-or-end-of-line]/\\[helm-select-3rd-action]:NthAct \\[helm-toggle-suspend-update]:Tog.suspend")

(defvar helm-moccur-mode-line "\\<helm-moccur-map>\\[helm-moccur-help]:Help \\<helm-map>\\[helm-select-action]:Act \\[helm-exit-minibuffer]/\\[helm-select-2nd-action-or-end-of-line]/\\[helm-select-3rd-action]:NthAct \\[helm-toggle-suspend-update]:Tog.suspend")

(defvar helm-comp-read-mode-line "\\<helm-comp-read-map>\\[helm-cr-empty-string]:Empty \\<helm-map>\\[helm-help]:Help \\[helm-select-action]:Act \\[helm-exit-minibuffer]/\\[helm-select-2nd-action-or-end-of-line]/\\[helm-select-3rd-action]:NthAct")

(defvar helm-top-mode-line "\\<helm-top-map>\\[helm-top-help]:Help \\<helm-map>\\[helm-select-action]:Act \\[helm-exit-minibuffer]/\\[helm-select-2nd-action-or-end-of-line]/\\[helm-select-3rd-action]:NthAct \\[helm-toggle-suspend-update]:Tog.suspend")

(defvar helm-apt-mode-line "\\<helm-apt-map>\\[helm-apt-help]:Help \\<helm-map>\\[helm-select-action]:Act \\[helm-exit-minibuffer]/\\[helm-select-2nd-action-or-end-of-line]/\\[helm-select-3rd-action]:NthAct \\[helm-toggle-suspend-update]:Tog.suspend")

(autoload 'helm-describe-helm-attribute "helm/helm-help" "\
Display the full documentation of HELM-ATTRIBUTE.
HELM-ATTRIBUTE should be a symbol.

\(fn HELM-ATTRIBUTE)" t nil)

;;;***

;;;### (autoloads (helm-imenu) "helm/helm-imenu" "helm/helm-imenu.el"
;;;;;;  (20920 10007))
;;; Generated autoloads from helm/helm-imenu.el

(autoload 'helm-imenu "helm/helm-imenu" "\
Preconfigured `helm' for `imenu'.

\(fn)" t nil)

;;;***

;;;### (autoloads (helm-info-at-point) "helm/helm-info" "helm/helm-info.el"
;;;;;;  (20920 10007))
;;; Generated autoloads from helm/helm-info.el

(autoload 'helm-info-at-point "helm/helm-info" "\
Preconfigured `helm' for searching info at point.
With a prefix-arg insert symbol at point.

\(fn)" t nil)

;;;***

;;;### (autoloads (helm-locate) "helm/helm-locate" "helm/helm-locate.el"
;;;;;;  (20920 10007))
;;; Generated autoloads from helm/helm-locate.el

(autoload 'helm-locate "helm/helm-locate" "\
Preconfigured `helm' for Locate.
Note: you can add locate options after entering pattern.
See 'man locate' for valid options and also `helm-locate-command'.

You can specify a local database with prefix argument ARG.
With two prefix arg, refresh the current local db or create it
if it doesn't exists.
Many databases can be used: navigate and mark them.
See also `helm-locate-with-db'.

To create a user specific db, use
\"updatedb -l 0 -o db_path -U directory\".
Where db_path is a filename matched by
`helm-locate-db-file-regexp'.

\(fn ARG)" t nil)

;;;***

;;;### (autoloads (helm-man-woman) "helm/helm-man" "helm/helm-man.el"
;;;;;;  (20920 10007))
;;; Generated autoloads from helm/helm-man.el

(autoload 'helm-man-woman "helm/helm-man" "\
Preconfigured `helm' for Man and Woman pages.
With a prefix arg reinitialize the cache.

\(fn ARG)" t nil)

;;;***

;;;### (autoloads (helm-match-plugin-mode) "helm/helm-match-plugin"
;;;;;;  "helm/helm-match-plugin.el" (20920 10007))
;;; Generated autoloads from helm/helm-match-plugin.el

(defvar helm-match-plugin-mode nil "\
Non-nil if Helm-Match-Plugin mode is enabled.
See the command `helm-match-plugin-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `helm-match-plugin-mode'.")

(custom-autoload 'helm-match-plugin-mode "helm/helm-match-plugin" nil)

(autoload 'helm-match-plugin-mode "helm/helm-match-plugin" "\
Add more flexible regexp matching for helm.
See `helm-mp-matching-method' for the behavior of each method.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (helm-minibuffer-history helm-mini helm-stumpwm-commands
;;;;;;  helm-ratpoison-commands helm-eev-anchors helm-insert-latex-math
;;;;;;  helm-world-time) "helm/helm-misc" "helm/helm-misc.el" (20920
;;;;;;  10007))
;;; Generated autoloads from helm/helm-misc.el

(autoload 'helm-world-time "helm/helm-misc" "\
Preconfigured `helm' to show world time.

\(fn)" t nil)

(autoload 'helm-insert-latex-math "helm/helm-misc" "\
Preconfigured helm for latex math symbols completion.

\(fn)" t nil)

(autoload 'helm-eev-anchors "helm/helm-misc" "\
Preconfigured `helm' for eev anchors.

\(fn)" t nil)

(autoload 'helm-ratpoison-commands "helm/helm-misc" "\
Preconfigured `helm' to execute ratpoison commands.

\(fn)" t nil)

(autoload 'helm-stumpwm-commands "helm/helm-misc" "\


\(fn)" t nil)

(autoload 'helm-mini "helm/helm-misc" "\
Preconfigured `helm' lightweight version (buffer -> recentf).

\(fn)" t nil)

(autoload 'helm-minibuffer-history "helm/helm-misc" "\
Preconfigured `helm' for `minibuffer-history'.

\(fn)" t nil)

;;;***

;;;### (autoloads (helm-mode helm-comp-read helm-cr-empty-string)
;;;;;;  "helm/helm-mode" "helm/helm-mode.el" (20920 10007))
;;; Generated autoloads from helm/helm-mode.el

(autoload 'helm-cr-empty-string "helm/helm-mode" "\
Return empty string.

\(fn)" t nil)

(autoload 'helm-comp-read "helm/helm-mode" "\
Read a string in the minibuffer, with helm completion.

It is helm `completing-read' equivalent.

- PROMPT is the prompt name to use.

- COLLECTION can be a list, vector, obarray or hash-table.
  It can be also a function that receives three arguments:
  the values string, predicate and t. See `all-completions' for more details.

Keys description:

- TEST: A predicate called with one arg i.e candidate.

- INITIAL-INPUT: Same as input arg in `helm'.

- PRESELECT: See preselect arg of `helm'.

- DEFAULT: This option is used only for compatibility with regular
  Emacs `completing-read' (Same as DEFAULT arg of `completing-read').

- BUFFER: Name of helm-buffer.

- MUST-MATCH: Candidate selected must be one of COLLECTION.

- REQUIRES-PATTERN: Same as helm attribute, default is 0.

- HISTORY: A list containing specific history, default is nil.
  When it is non--nil, all elements of HISTORY are displayed in
  a special source before COLLECTION.

- INPUT-HISTORY: A symbol. the minibuffer input history will be
  stored there, if nil or not provided, `minibuffer-history'
  will be used instead.

- CASE-FOLD: Same as `helm-case-fold-search'.

- DEL-INPUT: Boolean, when non--nil (default) remove the partial
  minibuffer input from HISTORY is present.

- PERSISTENT-ACTION: A function called with one arg i.e candidate.

- PERSISTENT-HELP: A string to document PERSISTENT-ACTION.

- MODE-LINE: A string or list to display in mode line.
  (See `helm-mode-line-string')

- KEYMAP: A keymap to use in this `helm-comp-read'.
  (The keymap will be shared with history source)

- NAME: The name related to this local source.

- EXEC-WHEN-ONLY-ONE: Bound `helm-execute-action-at-once-if-one'
  to non--nil. (possibles values are t or nil).

- VOLATILE: Use volatile attribute (enabled by default).

- SORT: A predicate to give to `sort' e.g `string-lessp'.

- FC-TRANSFORMER: A `filtered-candidate-transformer' function.

- MARKED-CANDIDATES: If non--nil return candidate or marked candidates as a list.

- ALISTP: (default is non--nil) See `helm-comp-read-get-candidates'.

- CANDIDATES-IN-BUFFER: when non--nil use a source build with
  `helm-candidates-in-buffer' which is much faster.
  Argument VOLATILE have no effect when CANDIDATES-IN-BUFFER is non--nil.

Any prefix args passed during `helm-comp-read' invocation will be recorded
in `helm-current-prefix-arg', otherwise if prefix args were given before
`helm-comp-read' invocation, the value of `current-prefix-arg' will be used.
That's mean you can pass prefix args before or after calling a command
that use `helm-comp-read' See `helm-M-x' for example.

\(fn PROMPT COLLECTION &key TEST INITIAL-INPUT DEFAULT PRESELECT (buffer \"*Helm Completions*\") MUST-MATCH (requires-pattern 0) (history nil) INPUT-HISTORY (case-fold helm-comp-read-case-fold-search) (del-input t) (persistent-action nil) (persistent-help \"DoNothing\") (mode-line helm-comp-read-mode-line) (keymap helm-comp-read-map) (name \"Helm Completions\") CANDIDATES-IN-BUFFER EXEC-WHEN-ONLY-ONE (volatile t) SORT (fc-transformer (quote helm-cr-default-transformer)) (marked-candidates nil) (alistp t))" nil nil)

(defvar helm-mode nil "\
Non-nil if Helm mode is enabled.
See the command `helm-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `helm-mode'.")

(custom-autoload 'helm-mode "helm/helm-mode" nil)

(autoload 'helm-mode "helm/helm-mode" "\
Toggle generic helm completion.

All functions in Emacs that use `completing-read'
or `read-file-name' and friends will use helm interface
when this mode is turned on.
However you can modify this behavior for functions of your choice
with `helm-completing-read-handlers-alist'.

Called with a positive arg, turn on unconditionally, with a
negative arg turn off.
You can turn it on with `helm-mode'.

Some crap emacs functions may not be supported,
e.g `ffap-alternate-file' and maybe others
You can add such functions to `helm-completing-read-handlers-alist'
with a nil value.

Note: This mode is incompatible with Emacs23.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (helm-yahoo-suggest helm-google-suggest helm-surfraw)
;;;;;;  "helm/helm-net" "helm/helm-net.el" (20920 10007))
;;; Generated autoloads from helm/helm-net.el

(autoload 'helm-surfraw "helm/helm-net" "\
Preconfigured `helm' to search PATTERN with search ENGINE.

\(fn PATTERN ENGINE)" t nil)

(autoload 'helm-google-suggest "helm/helm-net" "\
Preconfigured `helm' for google search with google suggest.

\(fn)" t nil)

(autoload 'helm-yahoo-suggest "helm/helm-net" "\
Preconfigured `helm' for Yahoo searching with Yahoo suggest.

\(fn)" t nil)

;;;***

;;;### (autoloads (helm-org-headlines helm-org-keywords) "helm/helm-org"
;;;;;;  "helm/helm-org.el" (20920 10007))
;;; Generated autoloads from helm/helm-org.el

(autoload 'helm-org-keywords "helm/helm-org" "\
Preconfigured `helm' for org keywords.

\(fn)" t nil)

(autoload 'helm-org-headlines "helm/helm-org" "\
Preconfigured helm to show org headlines.

\(fn)" t nil)

;;;***

;;;### (autoloads (helm-browse-code helm-multi-occur-from-isearch
;;;;;;  helm-multi-occur helm-occur-from-isearch helm-occur helm-regexp)
;;;;;;  "helm/helm-regexp" "helm/helm-regexp.el" (20920 10007))
;;; Generated autoloads from helm/helm-regexp.el

(autoload 'helm-regexp "helm/helm-regexp" "\
Preconfigured helm to build regexps.
`query-replace-regexp' can be run from there against found regexp.

\(fn)" t nil)

(autoload 'helm-occur "helm/helm-regexp" "\
Preconfigured helm for Occur.

\(fn)" t nil)

(autoload 'helm-occur-from-isearch "helm/helm-regexp" "\
Invoke `helm-occur' from isearch.

\(fn)" t nil)

(autoload 'helm-multi-occur "helm/helm-regexp" "\
Preconfigured helm for multi occur.

  BUFFERS is a list of buffers to search through.
With a prefix arg, reverse the behavior of
`helm-moccur-always-search-in-current'.
The prefix arg can be set before calling `helm-multi-occur'
or during the buffer selection.

\(fn BUFFERS)" t nil)

(autoload 'helm-multi-occur-from-isearch "helm/helm-regexp" "\
Invoke `helm-multi-occur' from isearch.

With a prefix arg, reverse the behavior of
`helm-moccur-always-search-in-current'.
The prefix arg can be set before calling
`helm-multi-occur-from-isearch' or during the buffer selection.

\(fn &optional ARG)" t nil)

(autoload 'helm-browse-code "helm/helm-regexp" "\
Preconfigured helm to browse code.

\(fn)" t nil)

;;;***

;;;### (autoloads (helm-show-kill-ring helm-register helm-all-mark-rings
;;;;;;  helm-global-mark-ring helm-mark-ring) "helm/helm-ring" "helm/helm-ring.el"
;;;;;;  (20920 10007))
;;; Generated autoloads from helm/helm-ring.el

(autoload 'helm-mark-ring "helm/helm-ring" "\
Preconfigured `helm' for `helm-source-mark-ring'.

\(fn)" t nil)

(autoload 'helm-global-mark-ring "helm/helm-ring" "\
Preconfigured `helm' for `helm-source-global-mark-ring'.

\(fn)" t nil)

(autoload 'helm-all-mark-rings "helm/helm-ring" "\
Preconfigured `helm' for `helm-source-global-mark-ring' and `helm-source-mark-ring'.

\(fn)" t nil)

(autoload 'helm-register "helm/helm-ring" "\
Preconfigured `helm' for Emacs registers.

\(fn)" t nil)

(autoload 'helm-show-kill-ring "helm/helm-ring" "\
Preconfigured `helm' for `kill-ring'.
It is drop-in replacement of `yank-pop'.

First call open the kill-ring browser, next calls move to next line.

\(fn)" t nil)

;;;***

;;;### (autoloads (helm-semantic-or-imenu helm-semantic) "helm/helm-semantic"
;;;;;;  "helm/helm-semantic.el" (20920 10007))
;;; Generated autoloads from helm/helm-semantic.el

(autoload 'helm-semantic "helm/helm-semantic" "\
Preconfigured `helm' for `semantic'.

\(fn)" t nil)

(autoload 'helm-semantic-or-imenu "helm/helm-semantic" "\
Run `helm' with `semantic' or `imenu'.

If `semantic-mode' is active in the current buffer, then use
semantic for generating tags, otherwise fall back to `imenu'.
Fill in the symbol at point by default.

\(fn)" t nil)

;;;***

;;;### (autoloads (helm-xrandr-set helm-list-emacs-process helm-top
;;;;;;  helm-top-run-sort-by-user helm-top-run-sort-by-mem helm-top-run-sort-by-cpu
;;;;;;  helm-top-run-sort-by-com) "helm/helm-sys" "helm/helm-sys.el"
;;;;;;  (20920 10007))
;;; Generated autoloads from helm/helm-sys.el

(autoload 'helm-top-run-sort-by-com "helm/helm-sys" "\


\(fn)" t nil)

(autoload 'helm-top-run-sort-by-cpu "helm/helm-sys" "\


\(fn)" t nil)

(autoload 'helm-top-run-sort-by-mem "helm/helm-sys" "\


\(fn)" t nil)

(autoload 'helm-top-run-sort-by-user "helm/helm-sys" "\


\(fn)" t nil)

(autoload 'helm-top "helm/helm-sys" "\
Preconfigured `helm' for top command.

\(fn)" t nil)

(autoload 'helm-list-emacs-process "helm/helm-sys" "\
Preconfigured `helm' for emacs process.

\(fn)" t nil)

(autoload 'helm-xrandr-set "helm/helm-sys" "\


\(fn)" t nil)

;;;***

;;;### (autoloads (helm-etags-select) "helm/helm-tags" "helm/helm-tags.el"
;;;;;;  (20920 10007))
;;; Generated autoloads from helm/helm-tags.el

(autoload 'helm-etags-select "helm/helm-tags" "\
Preconfigured helm for etags.
Called with one prefix arg use symbol at point as initial input.
Called with two prefix arg reinitialize cache.
If tag file have been modified reinitialize cache.

\(fn ARG)" t nil)

;;;***

;;;### (autoloads (helm-yank-text-at-point helm-w32-shell-execute-open-file
;;;;;;  helm-quit-and-find-file helm-display-all-sources helm-show-all-in-this-source-only)
;;;;;;  "helm/helm-utils" "helm/helm-utils.el" (20920 10007))
;;; Generated autoloads from helm/helm-utils.el

(autoload 'helm-show-all-in-this-source-only "helm/helm-utils" "\
Show only current source of this helm session with all its candidates.
With a numeric prefix arg show only the ARG number of candidates.

\(fn ARG)" t nil)

(autoload 'helm-display-all-sources "helm/helm-utils" "\
Display all sources previously hidden by `helm-set-source-filter'.

\(fn)" t nil)

(autoload 'helm-quit-and-find-file "helm/helm-utils" "\
Drop into `helm-find-files' from `helm'.
If current selection is a buffer or a file, `helm-find-files'
from its directory.

\(fn)" t nil)

(autoload 'helm-w32-shell-execute-open-file "helm/helm-utils" "\


\(fn FILE)" t nil)

(autoload 'helm-yank-text-at-point "helm/helm-utils" "\
Yank text at point in invocation buffer into minibuffer.

`helm-yank-symbol-first' controls whether the first yank grabs
the entire symbol.

\(fn)" t nil)

;;;***

;;;### (autoloads (helm-w3m-bookmarks) "helm/helm-w3m" "helm/helm-w3m.el"
;;;;;;  (20920 10007))
;;; Generated autoloads from helm/helm-w3m.el

(autoload 'helm-w3m-bookmarks "helm/helm-w3m" "\
Preconfigured `helm' for w3m bookmark.

Needs w3m and emacs-w3m.

http://w3m.sourceforge.net/
http://emacs-w3m.namazu.org/

\(fn)" t nil)

;;;***

;;;### (autoloads (helm-yaoddmuse-emacswiki-post-library helm-yaoddmuse-emacswiki-edit-or-view
;;;;;;  helm-yaoddmuse-cache-pages) "helm/helm-yaoddmuse" "helm/helm-yaoddmuse.el"
;;;;;;  (20920 10007))
;;; Generated autoloads from helm/helm-yaoddmuse.el

(autoload 'helm-yaoddmuse-cache-pages "helm/helm-yaoddmuse" "\
Fetch the list of files on emacswiki and create cache file.
If load is non--nil load the file and feed `yaoddmuse-pages-hash'.

\(fn &optional LOAD)" t nil)

(autoload 'helm-yaoddmuse-emacswiki-edit-or-view "helm/helm-yaoddmuse" "\
Preconfigured `helm' to edit or view EmacsWiki page.

Needs yaoddmuse.el.

http://www.emacswiki.org/emacs/download/yaoddmuse.el

\(fn)" t nil)

(autoload 'helm-yaoddmuse-emacswiki-post-library "helm/helm-yaoddmuse" "\
Preconfigured `helm' to post library to EmacsWiki.

Needs yaoddmuse.el.

http://www.emacswiki.org/emacs/download/yaoddmuse.el

\(fn)" t nil)

;;;***

;;;### (autoloads (idle-highlight-mode) "idle-highlight-mode/idle-highlight-mode"
;;;;;;  "idle-highlight-mode/idle-highlight-mode.el" (20920 9989))
;;; Generated autoloads from idle-highlight-mode/idle-highlight-mode.el

(autoload 'idle-highlight-mode "idle-highlight-mode/idle-highlight-mode" "\
Idle-Highlight Minor Mode

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (ido-ubiquitous-function-compatibility-exceptions
;;;;;;  ido-ubiquitous-command-compatibility-exceptions ido-ubiquitous-function-exceptions
;;;;;;  ido-ubiquitous-command-exceptions ido-ubiquitous-mode ido-ubiquitous)
;;;;;;  "ido-ubiquitous/ido-ubiquitous" "ido-ubiquitous/ido-ubiquitous.el"
;;;;;;  (20920 9943))
;;; Generated autoloads from ido-ubiquitous/ido-ubiquitous.el

(let ((loads (get 'ido-ubiquitous 'custom-loads))) (if (member '"ido-ubiquitous/ido-ubiquitous" loads) nil (put 'ido-ubiquitous 'custom-loads (cons '"ido-ubiquitous/ido-ubiquitous" loads))))

(defvar ido-ubiquitous-mode nil "\
Non-nil if Ido-Ubiquitous mode is enabled.
See the command `ido-ubiquitous-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `ido-ubiquitous-mode'.")

(custom-autoload 'ido-ubiquitous-mode "ido-ubiquitous/ido-ubiquitous" nil)

(autoload 'ido-ubiquitous-mode "ido-ubiquitous/ido-ubiquitous" "\
Use `ido-completing-read' instead of `completing-read' almost everywhere.

  This mode has no effect unles `ido-mode' is also enabled.

  If this mode causes problems for a function, you can force the
  function to use the original completing read by using the macro
  `ido-ubiquitous-disable-in'. For example, if a
  function `foo' cannot work with ido-style completion, evaluate
  the following (for example by putting it in your .emacs file):

    (ido-ubiquitous-disable-in foo)

\(fn &optional ARG)" t nil)

(define-obsolete-variable-alias 'ido-ubiquitous 'ido-ubiquitous-mode "0.8")

(define-obsolete-function-alias 'ido-ubiquitous 'ido-ubiquitous-mode "0.8")

(defvar ido-ubiquitous-command-exceptions 'nil "\
List of commands that should not be affected by `ido-ubiquitous'.

Even when `ido-ubiquitous' mode is enabled, these commands will
continue to use `completing-read' instead of
`ido-completing-read'.

Only *interactive* commands should go here. To disable
ido-ubiquitous in non-interactive functions, customize
`ido-ubiquitous-function-exceptions'.

Note: this feature depends on the variable `this-command' being
properly set to the name of the currently executing command.
Depending on how the command is onvoked, this may or may not
happen, so this feature may simply not work in some cases.")

(custom-autoload 'ido-ubiquitous-command-exceptions "ido-ubiquitous/ido-ubiquitous" t)

(define-obsolete-variable-alias 'ido-ubiquitous-exceptions 'ido-ubiquitous-command-exceptions "0.4")

(defvar ido-ubiquitous-function-exceptions '(grep-read-files) "\
List of functions in which to disable ido-ubiquitous.

If you need to add a function to this list, please also file a
bug report at
https://github.com/DarwinAwardWinner/ido-ubiquitous/issues

Note that certain functions, such as `read-file-name', must
always have ido-ubiquitous disabled, and cannot be added
here. (They are effectively a permanent part of this list
already.)")

(custom-autoload 'ido-ubiquitous-function-exceptions "ido-ubiquitous/ido-ubiquitous" nil)

(defvar ido-ubiquitous-command-compatibility-exceptions 'nil "\
List of commands in which to disable compatibility.

See `ido-ubiquitous-enable-compatibility' for a description of
the compatibility behavior. If this behavior causes a command to
break, add that command to this list to disable compatibility
mode for just that command.

Only *interactive* commands should go here. To disable
compatibility mode in non-interactive functions, customize
`ido-ubiquitous-function-compatibility-exceptions'.")

(custom-autoload 'ido-ubiquitous-command-compatibility-exceptions "ido-ubiquitous/ido-ubiquitous" t)

(defvar ido-ubiquitous-function-compatibility-exceptions 'nil "\
List of functions in which to disable ido-ubiquitous compatibility mode.

See `ido-ubiquitous-enable-compatibility' for a description of
the compatibility behavior. If this behavior causes a function to
break, add that function to this list to disable compatibility
mode for just that command.

If you need to add a function to this list, please also file a
bug report at
https://github.com/DarwinAwardWinner/ido-ubiquitous/issues")

(custom-autoload 'ido-ubiquitous-function-compatibility-exceptions "ido-ubiquitous/ido-ubiquitous" nil)

;;;***

;;;### (autoloads (magit-status) "magit/magit" "magit/magit.el" (20920
;;;;;;  9964))
;;; Generated autoloads from magit/magit.el

(autoload 'magit-status "magit/magit" "\
Open a Magit status buffer for the Git repository containing DIR.
If DIR is not within a Git repository, offer to create a Git
repository in DIR.

Interactively, a prefix argument means to ask the user which Git
repository to use even if `default-directory' is under Git
control.  Two prefix arguments means to ignore `magit-repo-dirs'
when asking for user input.

\(fn DIR)" t nil)

;;;***

;;;### (autoloads (magit-blame-mode) "magit/magit-blame" "magit/magit-blame.el"
;;;;;;  (20920 9964))
;;; Generated autoloads from magit/magit-blame.el

(autoload 'magit-blame-mode "magit/magit-blame" "\
Display blame information inline.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (turn-on-magit-flow magit-flow-mode) "magit/magit-flow"
;;;;;;  "magit/magit-flow.el" (20920 9964))
;;; Generated autoloads from magit/magit-flow.el

(autoload 'magit-flow-mode "magit/magit-flow" "\
FLOW support for Magit

\(fn &optional ARG)" t nil)

(autoload 'turn-on-magit-flow "magit/magit-flow" "\
Unconditionally turn on `magit-flow-mode'.

\(fn)" nil nil)

;;;***

;;;### (autoloads (turn-on-magit-stgit magit-stgit-mode) "magit/magit-stgit"
;;;;;;  "magit/magit-stgit.el" (20920 9964))
;;; Generated autoloads from magit/magit-stgit.el

(autoload 'magit-stgit-mode "magit/magit-stgit" "\
StGit support for Magit

\(fn &optional ARG)" t nil)

(autoload 'turn-on-magit-stgit "magit/magit-stgit" "\
Unconditionally turn on `magit-stgit-mode'.

\(fn)" nil nil)

;;;***

;;;### (autoloads (turn-on-magit-svn magit-svn-mode) "magit/magit-svn"
;;;;;;  "magit/magit-svn.el" (20920 9964))
;;; Generated autoloads from magit/magit-svn.el

(autoload 'magit-svn-mode "magit/magit-svn" "\
SVN support for Magit

\(fn &optional ARG)" t nil)

(autoload 'turn-on-magit-svn "magit/magit-svn" "\
Unconditionally turn on `magit-svn-mode'.

\(fn)" nil nil)

;;;***

;;;### (autoloads (turn-on-magit-topgit magit-topgit-mode) "magit/magit-topgit"
;;;;;;  "magit/magit-topgit.el" (20920 9964))
;;; Generated autoloads from magit/magit-topgit.el

(autoload 'magit-topgit-mode "magit/magit-topgit" "\
Topgit support for Magit

\(fn &optional ARG)" t nil)

(autoload 'turn-on-magit-topgit "magit/magit-topgit" "\
Unconditionally turn on `magit-topgit-mode'.

\(fn)" nil nil)

;;;***

;;;### (autoloads (global-magit-wip-save-mode magit-wip-save-mode
;;;;;;  magit-wip-mode) "magit/magit-wip" "magit/magit-wip.el" (20920
;;;;;;  9964))
;;; Generated autoloads from magit/magit-wip.el

(defvar magit-wip-mode nil "\
Non-nil if Magit-Wip mode is enabled.
See the command `magit-wip-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `magit-wip-mode'.")

(custom-autoload 'magit-wip-mode "magit/magit-wip" nil)

(autoload 'magit-wip-mode "magit/magit-wip" "\
In Magit log buffers; give wip refs a special appearance.

\(fn &optional ARG)" t nil)

(autoload 'magit-wip-save-mode "magit/magit-wip" "\
Magit support for committing to a work-in-progress ref.

When this minor mode is turned on and a file is saved inside a
writable git repository then it is also committed to a special
work-in-progress ref.

\(fn &optional ARG)" t nil)

(defvar global-magit-wip-save-mode nil "\
Non-nil if Global-Magit-Wip-Save mode is enabled.
See the command `global-magit-wip-save-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-magit-wip-save-mode'.")

(custom-autoload 'global-magit-wip-save-mode "magit/magit-wip" nil)

(autoload 'global-magit-wip-save-mode "magit/magit-wip" "\
Toggle Magit-Wip-Save mode in all buffers.
With prefix ARG, enable Global-Magit-Wip-Save mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Magit-Wip-Save mode is enabled in all buffers where
`turn-on-magit-wip-save' would do it.
See `magit-wip-save-mode' for more information on Magit-Wip-Save mode.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (rebase-mode) "magit/rebase-mode" "magit/rebase-mode.el"
;;;;;;  (20920 9964))
;;; Generated autoloads from magit/rebase-mode.el

(autoload 'rebase-mode "magit/rebase-mode" "\
Major mode for editing of a Git rebase file.

Rebase files are generated when you run 'git rebase -i' or run
`magit-interactive-rebase'.  They describe how Git should perform
the rebase.  See the documentation for git-rebase (e.g., by
running 'man git-rebase' at the command line) for details.

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("git-rebase-todo" . rebase-mode))

;;;***

;;;### (autoloads (gfm-mode markdown-mode) "markdown-mode/markdown-mode"
;;;;;;  "markdown-mode/markdown-mode.el" (20920 9993))
;;; Generated autoloads from markdown-mode/markdown-mode.el

(autoload 'markdown-mode "markdown-mode/markdown-mode" "\
Major mode for editing Markdown files.

\(fn)" t nil)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))

(autoload 'gfm-mode "markdown-mode/markdown-mode" "\
Major mode for editing GitHub Flavored Markdown files.

\(fn)" t nil)

;;;***

;;;### (autoloads (multi-term-dedicated-select multi-term-dedicated-toggle
;;;;;;  multi-term-dedicated-open multi-term-prev multi-term-next
;;;;;;  multi-term) "multi-term/multi-term" "multi-term/multi-term.el"
;;;;;;  (20920 10039))
;;; Generated autoloads from multi-term/multi-term.el

(autoload 'multi-term "multi-term/multi-term" "\
Create new term buffer.
Will prompt you shell name when you type `C-u' before this command.

\(fn)" t nil)

(autoload 'multi-term-next "multi-term/multi-term" "\
Go to the next term buffer.
If OFFSET is `non-nil', will goto next term buffer with OFFSET.

\(fn &optional OFFSET)" t nil)

(autoload 'multi-term-prev "multi-term/multi-term" "\
Go to the previous term buffer.
If OFFSET is `non-nil', will goto previous term buffer with OFFSET.

\(fn &optional OFFSET)" t nil)

(autoload 'multi-term-dedicated-open "multi-term/multi-term" "\
Open dedicated `multi-term' window.
Will prompt you shell name when you type `C-u' before this command.

\(fn)" t nil)

(autoload 'multi-term-dedicated-toggle "multi-term/multi-term" "\
Toggle dedicated `multi-term' window.

\(fn)" t nil)

(autoload 'multi-term-dedicated-select "multi-term/multi-term" "\
Select the `multi-term' dedicated window.

\(fn)" t nil)

;;;***

;;;### (autoloads (helm-projectile) "projectile/helm-projectile"
;;;;;;  "projectile/helm-projectile.el" (20920 10015))
;;; Generated autoloads from projectile/helm-projectile.el

(autoload 'helm-projectile "projectile/helm-projectile" "\
Use projectile with Helm instead of ido.

\(fn)" t nil)

(eval-after-load 'projectile '(define-key projectile-mode-map (kbd "C-c p h") 'helm-projectile))

;;;***

;;;### (autoloads (projectile-global-mode projectile-mode) "projectile/projectile"
;;;;;;  "projectile/projectile.el" (20920 10015))
;;; Generated autoloads from projectile/projectile.el

(autoload 'projectile-mode "projectile/projectile" "\
Minor mode to assist project management and navigation.

\\{projectile-mode-map}

\(fn &optional ARG)" t nil)

(defvar projectile-global-mode nil "\
Non-nil if Projectile-Global mode is enabled.
See the command `projectile-global-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `projectile-global-mode'.")

(custom-autoload 'projectile-global-mode "projectile/projectile" nil)

(autoload 'projectile-global-mode "projectile/projectile" "\
Toggle Projectile mode in all buffers.
With prefix ARG, enable Projectile-Global mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Projectile mode is enabled in all buffers where
`projectile-on' would do it.
See `projectile-mode' for more information on Projectile mode.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (revive-plus:minimal-setup revive-plus:demo revive-plus:start-last-wconf
;;;;;;  revive-plus:start-wconf-archive revive-plus:restore-window-configuration
;;;;;;  revive-plus:save-window-configuration revive-plus:wconf-archive-load
;;;;;;  revive-plus:wconf-archive-restore revive-plus:wconf-archive-clear
;;;;;;  revive-plus:toggle-single-window) "revive-plus/revive+" "revive-plus/revive+.el"
;;;;;;  (20920 10551))
;;; Generated autoloads from revive-plus/revive+.el

(autoload 'revive-plus:toggle-single-window "revive-plus/revive+" "\
Toggle to single window and back. It uses standard
window configuration vector instead of REVIVE one as we
don't want to register the mark.

\(fn)" t nil)

(autoload 'revive-plus:wconf-archive-clear "revive-plus/revive+" "\


\(fn)" t nil)

(autoload 'revive-plus:wconf-archive-restore "revive-plus/revive+" "\


\(fn &optional NUM)" t nil)

(autoload 'revive-plus:wconf-archive-load "revive-plus/revive+" "\


\(fn)" t nil)

(autoload 'revive-plus:save-window-configuration "revive-plus/revive+" "\


\(fn &optional SPECIAL-CASE)" nil nil)

(autoload 'revive-plus:restore-window-configuration "revive-plus/revive+" "\


\(fn)" nil nil)

(autoload 'revive-plus:start-wconf-archive "revive-plus/revive+" "\
Setup example for wconf-archive. Enable window configuration saving
and restoring for a single frame.

\(fn &optional WITH-KEYBINDINGS)" nil nil)

(autoload 'revive-plus:start-last-wconf "revive-plus/revive+" "\
Setup example for last-wconf. Enable the complete window configuration
reload. Frame and Escreen are maintained if REVIVE-PLUS:ALL-FRAMES is true.
Frames are merged to escreen when Emacs is started in NO-WINDOW-SYSTEM context.

\(fn &optional WITH-KEYBINDINGS)" nil nil)

(autoload 'revive-plus:demo "revive-plus/revive+" "\
Setup example for revive-plus including wconf-archive, complete window
configuration reload and single window switching key.

\(fn)" nil nil)

(autoload 'revive-plus:minimal-setup "revive-plus/revive+" "\
Setup example for revive-plus. Like `revive-plus:demo', w/o key bindings.

\(fn)" nil nil)

;;;***

;;;### (autoloads (resume save-current-configuration wipe restore-window-configuration
;;;;;;  current-window-configuration-printable) "revive/revive" "revive/revive.el"
;;;;;;  (20920 10053))
;;; Generated autoloads from revive/revive.el

(autoload 'current-window-configuration-printable "revive/revive" "\
Return the printable current-window-configuration.
This configuration will be stored by restore-window-configuration.
Returned configurations are list of:
'(Screen-Width Screen-Height Edge-List Buffer-List)

Edge-List is a return value of revive:all-window-edges, list of all
window-edges whose first member is always of north west window.

Buffer-List is a list of buffer property list of all windows.  This
property lists are stored in order corresponding to Edge-List.  Buffer
property list is formed as
'((buffer-file-name) (buffer-name) (point) (window-start)).

\(fn)" nil nil)

(autoload 'restore-window-configuration "revive/revive" "\
Restore the window configuration.
Configuration CONFIG should be created by
current-window-configuration-printable.

\(fn CONFIG)" nil nil)

(autoload 'wipe "revive/revive" "\
Wipe Emacs.

\(fn)" t nil)

(autoload 'save-current-configuration "revive/revive" "\
Save current window/buffer configuration into configuration file.

\(fn &optional NUM)" t nil)

(autoload 'resume "revive/revive" "\
Resume window/buffer configuration.
Configuration should be saved by save-current-configuration.

\(fn &optional NUM)" t nil)

;;;***

;;;### (autoloads (smex-initialize smex) "smex/smex" "smex/smex.el"
;;;;;;  (20920 9930))
;;; Generated autoloads from smex/smex.el

(autoload 'smex "smex/smex" "\


\(fn)" t nil)

(autoload 'smex-initialize "smex/smex" "\


\(fn)" t nil)

;;;***

;;;### (autoloads (web-mode) "web-mode/web-mode" "web-mode/web-mode.el"
;;;;;;  (20937 18547))
;;; Generated autoloads from web-mode/web-mode.el

(autoload 'web-mode "web-mode/web-mode" "\
Major mode for editing web templates.

\(fn)" t nil)

;;;***

;;;### (autoloads (yaml-mode yaml) "yaml-mode/yaml-mode" "yaml-mode/yaml-mode.el"
;;;;;;  (20920 9991))
;;; Generated autoloads from yaml-mode/yaml-mode.el

(let ((loads (get 'yaml 'custom-loads))) (if (member '"yaml-mode/yaml-mode" loads) nil (put 'yaml 'custom-loads (cons '"yaml-mode/yaml-mode" loads))))

(autoload 'yaml-mode "yaml-mode/yaml-mode" "\
Simple mode to edit YAML.

\\{yaml-mode-map}

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.ya?ml$" . yaml-mode))

;;;***

;;;### (autoloads nil nil ("../../../../../../../.emacs.d/el-get/auto-complete/auto-complete-config.el"
;;;;;;  "../../../../../../../.emacs.d/el-get/auto-complete/auto-complete-pkg.el"
;;;;;;  "../../../../../../../.emacs.d/el-get/auto-complete/auto-complete.el"
;;;;;;  "../../../../../../../.emacs.d/el-get/ctable/ctable.el" "../../../../../../../.emacs.d/el-get/ctable/test-ctable.el"
;;;;;;  "../../../../../../../.emacs.d/el-get/deferred/concurrent-sample.el"
;;;;;;  "../../../../../../../.emacs.d/el-get/deferred/concurrent.el"
;;;;;;  "../../../../../../../.emacs.d/el-get/deferred/deferred-samples.el"
;;;;;;  "../../../../../../../.emacs.d/el-get/deferred/deferred.el"
;;;;;;  "../../../../../../../.emacs.d/el-get/deferred/test-concurrent.el"
;;;;;;  "../../../../../../../.emacs.d/el-get/deferred/test-deferred.el"
;;;;;;  "../../../../../../../.emacs.d/el-get/epc/epc.el" "../../../../../../../.emacs.d/el-get/epc/epcs.el"
;;;;;;  "../../../../../../../.emacs.d/el-get/epc/test-epc.el" "../../../../../../../.emacs.d/el-get/fuzzy/fuzzy.el"
;;;;;;  "../../../../../../../.emacs.d/el-get/popup/popup.el" "cl-lib/cl-lib-autoloads.el"
;;;;;;  "cl-lib/cl-lib-pkg.el" "cl-lib/cl-lib.el" "coffee-mode/coffee-mode-autoloads.el"
;;;;;;  "coffee-mode/coffee-mode-pkg.el" "dash/dash.el" "el-get/el-get-autoloads.el"
;;;;;;  "el-get/el-get-build.el" "el-get/el-get-byte-compile.el"
;;;;;;  "el-get/el-get-core.el" "el-get/el-get-custom.el" "el-get/el-get-dependencies.el"
;;;;;;  "el-get/el-get-install.el" "el-get/el-get-methods.el" "el-get/el-get-notify.el"
;;;;;;  "el-get/el-get-recipes.el" "el-get/el-get-status.el" "flymake-easy/flymake-easy.el"
;;;;;;  "frame-restore/frame-restore.el" "helm/helm-aliases.el" "helm/helm-pkg.el"
;;;;;;  "helm/helm-plugin.el" "ido-ubiquitous/ido-ubiquitous-autoloads.el"
;;;;;;  "ido-ubiquitous/ido-ubiquitous-pkg.el" "ipython/ipython.el"
;;;;;;  "magit/magit-bisect.el" "magit/magit-key-mode.el" "python-mode/python-mode.el"
;;;;;;  "s/s.el") (20941 15680 120347))

;;;***

(provide '.loaddefs)
;; Local Variables:
;; version-control: never
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; .loaddefs.el ends here
