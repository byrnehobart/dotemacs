;; UI Stuff
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode -1)
(line-number-mode 1)
(column-number-mode 1)
(set-cursor-color     "red")

(add-to-list 'load-path "~/site-lisp/") ;; many useful things live here.
;(require 'idle-highlight-mode)

;; Set encoding to UTF-8
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; magit, cl
(require 'cl-lib)

;; Useful key strokes
(global-set-key "\M-g" 'goto-line)
(global-set-key "\M-d" 'delete-region)
(global-set-key "\C-cp" 'replace-string)
(defalias 'qrr 'query-replace-regexp)

;; Frame title : set to buffer name and uniquify
(setq frame-title-format "Emacs - %f ")
(setq icon-title-format  "Emacs - %b")
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

(setq-default truncate-lines t)
(setq scroll-step 1
      scroll-margin 3
      scroll-conservatively 10000) ;; Scrolling 
(show-paren-mode t) ;; Highlight parens 
(setq show-paren-style 'parentheses) ;; Shor parens
(fset 'yes-or-no-p 'y-or-n-p) ;; Alias Y and N
(follow-mode t) ;; Easier editing of longs files
(setq inhibit-startup-message t) ;; Disable start up message
(setq search-highlight t) ;; Highlight search results
(setq kill-whole-line t) ;; Kill whole line
(setq visible-bell t) ;; Visible bells
(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)

;; Kill without confirmation
(global-set-key "\C-xk" 'kill-current-buffer)
(defun kill-current-buffer ()
  "Kill the current buffer, without confirmation."
  (interactive)
  (kill-buffer (current-buffer)))

;; Google Search
(defun search-google ()
  "Prompt for a query in the minibuffer, launch the web browser and query google. Uses &pws=0 to suppress customization"
  (interactive)
  (let ((search (read-from-minibuffer "Google Search: ")))
    (browse-url (concat "http://www.google.com/search?q=" search "&pws=0"))))

;; Org

(setq org-log-done t)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(global-set-key (kbd "C-c a") 'org-agenda)                   
(setq org-todo-keywords '("TODO" "STARTED" "WAITING" "DONE"))
(setq org-agenda-include-diary t)
(setq org-agenda-include-all-todo t)     
(setq org-agenda-show-log t)
(setq org-clock-in-switch-to-state "STARTED")
(setq org-agenda-custom-commands
      (quote (("s" "Started Tasks" todo "STARTED" ((org-agenda-todo-ignore-with-date 0)))
              ("w" "Tasks waiting on something" tags "WAITING/!" ((org-use-tag-inheritance 0)))
              ("r" "Refile New Notes and Tasks" tags "LEVEL=1+REFILE" ((org-agenda-todo-ignore-with-date 0))))))
(add-hook 'org-mode-hook 'visual-line-mode)
(org-babel-do-load-languages
'org-babel-load-languages
'((python . t)))

;; markdown
					
(require 'markdown-mode)
(add-to-list 'auto-mode-alist '("\\.mdt\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-hook 'markdown-mode-hook 'visual-line-mode)
(setq markdown-command "/anaconda/bin/markdown_py")

;; packages
(require 'package) 
(setq package-archives '(("marmalade" . "http://marmalade-repo.org/packages/")
			 ("gnu" . "http://elpa.gnu.org/packages/")
			 ("melpa" . "http://melpa.milkbox.net/packages/")))
(package-initialize)



;; shell
;(add-to-list 'load-path "~/site-lisp/exec-path-from-shell/")
;(require 'exec-path-from-shell)
;(when (memq window-system '(mac ns))
;  (exec-path-from-shell-initialize))



;; python
(setq
 python-shell-interpreter "/Library/Frameworks/Python.framework/Versions/3.5/bin/ipython3"
 python-shell-interpreter-args ""
 python-shell-prompt-regexp "In \\[[0-9]+\\]: "
 python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
 python-shell-completion-setup-code
   "from IPython.core.completerlib import module_completion"
 python-shell-completion-module-string-code
   "';'.join(module_completion('''%s'''))\n"
 python-shell-completion-string-code
 "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")

(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)


;; Slime!

(setq inferior-lisp-program "/usr/local/bin/sbcl")
(setq slime-contribs '(slime-fancy))

;; Yaml
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

(global-set-key "\C-c\C-b" 'browse-url-at-point)

;; New stuff for .js, node

(ac-config-default)
(add-hook 'js-mode-hook 'js2-minor-mode)
(add-hook 'js2-mode-hook 'ac-js2-mode)
(setq js2-highlight-level 3)
(add-to-list 'auto-mode-alist '("\\.json$" . js-mode))
;; getting a "void variable" error for js-mode-map
;;(define-key js-mode-map "{" 'paredit-open-curly)
;;(define-key js-mode-map "}" 'paredit-close-curly-and-newline)
(eval-after-load 'js-mode
  '(define-key js-mode-map "{" 'paredit-open-curly))
(eval-after-load 'js-mode
  '(define-key js-mode-map "}" 'paredit-close-curly-and-newline))

;(add-to-list 'load-path "~/.emacs.d/elpa/js-comint-20080530.757/")
(require 'js-comint)
(setq inferior-js-program-command "node")
 
(setq inferior-js-mode-hook
      (lambda ()
        ;; We like nice colors
        (ansi-color-for-comint-mode-on)
        ;; Deal with some prompt nonsense
        (add-to-list
         'comint-preoutput-filter-functions
         (lambda (output)
	   (replace-regexp-in-string "\033\\[[0-9]+[A-Z]" "" output)))))
