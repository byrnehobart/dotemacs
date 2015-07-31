;; UI Stuff
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode -1)
(line-number-mode 1)
(column-number-mode 1)
(set-cursor-color     "red")

(add-to-list 'load-path "~/site-lisp") ;; many useful things live here.
(require 'idle-highlight-mode)

;; Set encoding to UTF-8
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Useful key strokes
(global-set-key "\M-g" 'goto-line)
(global-set-key "\M-d" 'delete-region)
(global-set-key "\C-cp" 'replace-string)

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
(add-to-list 'auto-mode-alist
	     '("\\.mdt\\'" . markdown-mode)
	     '("\\.md\\'" . markdown-mode))
(add-hook 'markdown-mode-hook 'visual-line-mode)
(setq markdown-command "/anaconda/bin/markdown_py")

;; shell
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;; packages
(require 'package) 
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(package-initialize) 

;; python
(setq
 python-shell-interpreter "ipython"
 python-shell-interpreter-args ""
 python-shell-prompt-regexp "In \\[[0-9]+\\]: "
 python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
 python-shell-completion-setup-code
   "from IPython.core.completerlib import module_completion"
 python-shell-completion-module-string-code
   "';'.join(module_completion('''%s'''))\n"
 python-shell-completion-string-code
 "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")
(setenv "LC_CTYPE" "UTF-8") ;; necesary for showing some strings in *shell* buffs
