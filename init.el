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


(require 'helm)
(require 'helm-projectile)
(require 'cider)
(require 'magit)
(require 'paredit)
(require 'undo-tree)
(require 'highlight-symbol)
(require 'expand-region)
(require 'swiper)
(require 'counsel)
(require 'company)
(require 'projectile)
(require 'multiple-cursors)
(require 'exec-path-from-shell)
(require 'browse-kill-ring)
(require 'fill-column-indicator)
(require 'flycheck-joker)

;; Basic settings
(setq inhibit-startup-message t)
(global-display-line-numbers-mode t)
(global-git-gutter-mode t)
;; Remove dated looking toolbar
(tool-bar-mode -1)
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

;; CIDER Repl Mode setup
(add-hook 'cider-repl-mode-hook 'paredit-mode)
(add-hook 'cider-repl-mode-hook 'aggressive-indent-mode)
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



