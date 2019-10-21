(require 'package)

;; from https://stackoverflow.com/questions/5052088/what-is-custom-set-variables-and-faces-in-my-emacs
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("org" . "https://orgmode.org/elpa/") t)
(add-to-list 'package-pinned-packages
       '(cider . "melpa-stable") t)
(add-to-list 'package-archives
             '("melpa-stable" . "http://stable.melpa.org/packages/") t)

(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages
  '(
    ;;aggressive-indent-mode
    better-defaults
    browse-kill-ring
    cider
    ;;clj-refactor
    company
    counsel
    elpy
    exec-path-from-shell
    expand-region
    fill-column-indicator
    flycheck-joker
    git-gutter
    helm
    helm-ag
    helm-cider
    helm-clojuredocs
    helm-company
    helm-flycheck
    helm-flyspell
    helm-git
    helm-git-files
    helm-make
    helm-projectile
    helm-swoop
    highlight-symbol
    magit
    multiple-cursors
    neotree
    paredit
    projectile
    spacemacs-theme
    swiper
    undo-tree
    ))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))


(require 'browse-kill-ring)
(require 'cider)
(require 'company)
(require 'counsel)
(require 'exec-path-from-shell)
(require 'expand-region)
(require 'fill-column-indicator)
(require 'flycheck-joker)
(require 'helm)
(require 'helm-projectile)
(require 'highlight-symbol)
(require 'magit)
(require 'multiple-cursors)
(require 'neotree)
(require 'paredit)
(require 'projectile)
(require 'swiper)
(require 'undo-tree)

(exec-path-from-shell-initialize)

;; Basic settings
(setq inhibit-startup-message t)
(global-display-line-numbers-mode t)
(column-number-mode t)
(global-git-gutter-mode t)
(winner-mode t)
;; Remove dated looking toolbar
(tool-bar-mode -1)
;; Remove scrollbars
(scroll-bar-mode -1)
;; Enable replacing & deleting of highlighted text
;; https://www.gnu.org/software/emacs/manual/html_node/efaq/Replacing-highlighted-text.html
(delete-selection-mode 1)
;; Revert buffer automatically
(global-auto-revert-mode 1)
;; Remove trailing whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)
;; Make look nice - load theme
(load-theme 'spacemacs-dark t)
;; set fill column to 80 for fci-mode
(setq-default fill-column 80)
;; Add to exec-path (required for running lein)
(add-to-list 'exec-path "/usr/local/bin/")

;; Enable Ctrl-H for delete
;; suggested here: https://www.emacswiki.org/emacs/BackspaceKey
(global-set-key (kbd "C-?") 'help-command)
(global-set-key (kbd "M-?") 'mark-paragraph)
(global-set-key (kbd "C-h") 'delete-backward-char)
(global-set-key (kbd "M-h") 'backward-kill-word)

;; set key for imenu & other window (from Mastering Emacs)
(global-set-key (kbd "M-i") 'imenu)
(global-set-key (kbd "M-o") 'other-window)


;; Highlight Symbol setup - bind to F3
(global-set-key [(control f3)] 'highlight-symbol)
(global-set-key [f3] 'highlight-symbol-next)
(global-set-key [(shift f3)] 'highlight-symbol-prev)
(global-set-key [(meta f3)] 'highlight-symbol-query-replace)
(setq highlight-symbol-idle-delay 0.5)

;; Hide/Show setup
;; TODO: these conflict with winner mode
(global-set-key (kbd "C-c <up>")    'hs-hide-all)
(global-set-key (kbd "C-c <down>")  'hs-show-all)
(global-set-key (kbd "C-c <left>") 'hs-hide-block)
(global-set-key (kbd "C-c <right>")  'hs-show-block)

;; Expand Region setup
(global-set-key (kbd "C-=") 'er/expand-region)

;; Window Resize
;; Take from https://www.emacswiki.org/emacs/WindowResize
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)

;; Easily move between windows (S-↑, S-→, ...)
(windmove-default-keybindings)

(defun show-file-name ()
  "Show the full path file name in the minibuffer."
  (interactive)
  (message (buffer-file-name)))
(global-set-key (kbd "<f12>") 'show-file-name)

(defun reverse-transpose-sexps (arg)
  (interactive "*p")
  (transpose-sexps (- arg))
  (backward-sexp (1+ arg))
  (forward-sexp 2))
(global-set-key (kbd "C-M-y") 'reverse-transpose-sexps)

;; Projectile setup
(projectile-mode)
(global-set-key (kbd "C-.") 'helm-projectile-ag)
;; Projectile Mode
;;(projectile-mode +1)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

;; Multiple Cursors setup
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this-symbol)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this-symbol)
(global-set-key (kbd "C-c <") 'mc/mark-all-symbols-like-this-in-defun)

;; Browse kill ring setup
(global-set-key (kbd "C-c k") 'browse-kill-ring)

;; Company mode setup
(define-key company-active-map (kbd "\C-n") 'company-select-next)
(define-key company-active-map (kbd "\C-p") 'company-select-previous)

;; Undo Tree setup
(global-undo-tree-mode)

;; Magit keyboard shortcuts
(global-set-key (kbd "C-x g") 'magit-status)


;; Helm
(helm-mode t)
(helm-projectile-on)
(setq enable-recursive-minibuffers t)
(global-set-key (kbd "C-s") 'helm-swoop)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-c r") 'helm-resume)
(define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-c h o") 'helm-occur)

;; allow C-h to be used as delete in helm
;; from https://emacs.stackexchange.com/questions/5486/helm-override-c-h
;; (setq help-char nil)
(eval-after-load "helm-files"
  '(let ((helm-find-files-C-h-map (lookup-key helm-find-files-map (kbd "C-h"))))
     ;; make sure C-h is no longer a prefix key
     (define-key helm-find-files-map (kbd "C-h") nil)
     ;; rebind "C-h ..." to "M-m ..." to preserve functionality
     (define-key helm-find-files-map (kbd "M-m") helm-find-files-C-h-map)))

;; From https://github.com/emacs-helm/helm/issues/745
(define-key helm-map (kbd "C-h") nil)
(define-key helm-map (kbd "C-h") 'helm-ff-delete-char-backward)
(put 'upcase-region 'disabled nil)

;; from andrea - to get helm to size nicely - but it doesn't seem to worc
(helm-autoresize-mode t)

(setq helm-buffers-fuzzy-matching t
      helm-recentf-fuzzy-match t
      helm-locate-fuzzy-match t
      helm-use-frame-when-more-than-two-windows nil
      helm-M-x-fuzzy-match t)

(global-set-key (kbd "M-y") 'helm-show-kill-ring)


;; NeoTree
;; Make NeoTree resizeable
;; from - https://emacs.stackexchange.com/questions/37678/neotree-window-not-resizable
(setq neo-window-fixed-size nil)
;; NeoTree to work with Projectile - from https://www.emacswiki.org/emacs/NeoTree
(setq neo-smart-open t)
(defun neotree-project-dir ()
  "Open NeoTree using the git root."
  (interactive)
  (let ((project-dir (projectile-project-root))
        (file-name (buffer-file-name)))
    (neotree-toggle)
    (if project-dir
        (if (neo-global--window-exists-p)
            (progn
              (neotree-dir project-dir)
              (neotree-find file-name)))
      (message "Could not find git project root."))))
(global-set-key [f8] 'neotree-project-dir)

;; Clojure Mode setup
(add-hook 'clojure-mode-hook 'hs-minor-mode)
(add-hook 'clojure-mode-hook 'paredit-mode)
(add-hook 'clojure-mode-hook 'highlight-symbol-mode)
(add-hook 'clojure-mode-hook 'company-mode)
(add-hook 'clojure-mode-hook 'show-paren-mode)
(add-hook 'clojure-mode-hook 'aggressive-indent-mode)
(add-hook 'clojure-mode-hook 'fci-mode)
(add-hook 'clojure-mode-hook 'highlight-indentation-mode)
;; From https://stackoverflow.com/questions/5500035/set-custom-keybinding-for-specific-emacs-mode
(add-hook 'clojure-mode-hook (lambda () (local-set-key (kbd "C-S-k") #'paredit-copy-as-kill)))
;; From https://www.reddit.com/r/Clojure/comments/56e3hp/syntax_highlighting_for_function_calls_in_emacs/
(add-hook 'clojure-mode-hook
          '(lambda ()
             (font-lock-add-keywords
        nil
        '(("(\\(\\w+\\)\\s-+" 1 font-lock-keyword-face)))))
;; From http://emacsredux.com/blog/2014/08/25/a-peek-at-emacs-24-dot-4-prettify-symbols-mode/
(add-hook 'clojure-mode-hook 'prettify-symbols-mode)
;; From https://docs.cider.mx/cider/config/basic_config.html
(setq cider-save-file-on-load t)

;; CIDER Repl Mode setup
(add-hook 'cider-repl-mode-hook 'paredit-mode)
(add-hook 'cider-repl-mode-hook 'aggressive-indent-mode)
(add-hook 'cider-repl-mode-hook (lambda () (local-set-key (kbd "C-S-k") #'paredit-copy-as-kill)))
(add-hook 'cider-repl-mode-hook (lambda () (local-set-key (kbd "C-c C-q") #'cider-quit)))
(add-hook 'cider-repl-mode-hook 'highlight-symbol-mode)
(setq cider-repl-use-pretty-printing t)

;; Python Mode setup
;; https://realpython.com/emacs-the-best-python-editor/
(setenv "PATH"
  (concat
    (getenv "PATH") ":" "/Users/sluke/.local/bin"))
(elpy-enable)

;; Haskell Mode
(add-hook 'haskell-mode-hook 'intero-mode)

;; SCSS Mode
(add-hook 'scss-mode-hook
          (lambda ()
            (set (make-local-variable 'compile-command)
                 (concat "stylelint --fix " buffer-file-name))))

;; Org Mode
;; Some code from Andrea to be able to evaluate code in org mode
(org-babel-do-load-languages
 'org-babel-load-languages
 '((sql . t)
   (clojure . t)
   (lisp . t)
   (haskell . t)
   (dot . t)
   (ruby . t)
   (scheme . t)
   ;; (R . t)
   (ditaa . t)
   (lisp . t)
   (python . t)))
(put 'downcase-region 'disabled nil)


;; Go Mode
;; from http://tleyden.github.io/blog/2014/05/22/configure-emacs-as-a-go-editor-from-scratch
(defun set-exec-path-from-shell-PATH ()
  (let ((path-from-shell (replace-regexp-in-string
                          "[ \t\n]*$"
                          ""
                          (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq eshell-path-env path-from-shell) ; for eshell users
    (setq exec-path (split-string path-from-shell path-separator))))

(when window-system (set-exec-path-from-shell-PATH))

(setenv "GOPATH" "/Users/sluke/go")

(add-to-list 'exec-path "/Users/sluke/go/bin")
(defun my-go-mode-hook ()
  ;; Use goimports instead of go-fmt
  (setq gofmt-command "goimports")
  ;; Call Gofmt before saving
  (add-hook 'before-save-hook 'gofmt-before-save)
  ;; Customize compile command to run go build
  (if (not (string-match "go" compile-command))
      (set (make-local-variable 'compile-command)
           "go build -v && go test -v && go vet"))
  ;; Godef jump key binding
  (local-set-key (kbd "M-.") 'godef-jump)
  (local-set-key (kbd "M-,") 'pop-tag-mark))
(add-hook 'go-mode-hook 'my-go-mode-hook)
;; autocomplete
(defun auto-complete-for-go ()
(auto-complete-mode 1))
(add-hook 'go-mode-hook 'auto-complete-for-go)

(with-eval-after-load 'go-mode
  (require '
   go-autocomplete))
