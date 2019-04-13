(require 'package)

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
    clj-refactor
    company
    counsel
    elpy
    exec-path-from-shell
    expand-region
    fill-column-indicator
    flycheck-joker
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

(setq inhibit-startup-message t)
(global-linum-mode t)

(require 'helm)
(require 'helm-projectile)

(helm-mode t)
(helm-projectile-on)

(tool-bar-mode -1)
(add-to-list 'exec-path "/usr/local/bin/")

(require 'cider)
(require 'magit)
(require 'paredit)
(require 'undo-tree)
(require 'highlight-symbol)
(require 'expand-region)
;; (require 'ivy)
(require 'swiper)
(require 'counsel)
(require 'company)
;; (require 'ivy-hydra)
(require 'projectile)
;; (require 'counsel-projectile)
(require 'multiple-cursors)
(require 'exec-path-from-shell)
(require 'browse-kill-ring)
(require 'fill-column-indicator)

;; Revert buffer automatically
(global-auto-revert-mode 1)

;; Remove trailing whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Make look nice - load theme
(load-theme 'spacemacs-dark t)

;; Make env vars the same in GUI as per shell
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;; Magit keyboard shortcuts
(global-set-key (kbd "C-x g") 'magit-status)

;; set fill column to 80 for fci-mode
(setq-default fill-column 80)

;; set key for imenu & other window (from Mastering Emacs)
(global-set-key (kbd "M-i") 'imenu)
(global-set-key (kbd "M-o") 'other-window)


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


;; Highlight Symbol setup - bind to F3
(global-set-key [(control f3)] 'highlight-symbol)
(global-set-key [f3] 'highlight-symbol-next)
(global-set-key [(shift f3)] 'highlight-symbol-prev)
(global-set-key [(meta f3)] 'highlight-symbol-query-replace)
(setq highlight-symbol-idle-delay 0.5)

;; Expand Region setup
(global-set-key (kbd "C-=") 'er/expand-region)

;; Undo Tree setup
(global-undo-tree-mode)

;; Hide/Show setup
(global-set-key (kbd "C-c <up>")    'hs-hide-all)
(global-set-key (kbd "C-c <down>")  'hs-show-all)
(global-set-key (kbd "C-c <left>") 'hs-hide-block)
(global-set-key (kbd "C-c <right>")  'hs-show-block)

;; Ivy & Counsel setup
;; TODO: remove ivy and counsel stuff and replace with helm
(setq enable-recursive-minibuffers t)
(global-set-key (kbd "C-s") 'helm-swoop)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-c r") 'ivy-resume)
(define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)

;; Clojure Mode setup
;; from https://github.com/clojure-emacs/clj-refactor.el
(require 'clj-refactor)
(defun my-clojure-mode-hook ()
    (clj-refactor-mode 1)
    (yas-minor-mode 1) ; for adding require/use/import statements
    ;; This choice of keybinding leaves cider-macroexpand-1 unbound
    (cljr-add-keybindings-with-prefix "C-c C-m"))
(add-hook 'clojure-mode-hook #'my-clojure-mode-hook)
;; from https://github.com/candid82/flycheck-joker
(require 'flycheck-joker)


(add-hook 'clojure-mode-hook 'hs-minor-mode)
(add-hook 'clojure-mode-hook 'paredit-mode)
(add-hook 'clojure-mode-hook 'highlight-symbol-mode)
(add-hook 'clojure-mode-hook 'company-mode)
;;(add-hook 'clojure-mode-hook 'linum-mode)
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


;; custom-set-variables was added by Custom.
;; If you edit it by hand, you could mess it up, so be careful.
;; Your init file should contain only one such instance.
;; If there is more than one, they won't work right.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cider-lein-parameters "repl :headless :host ::")
 '(custom-safe-themes
   (quote
    ("e11569fd7e31321a33358ee4b232c2d3cf05caccd90f896e1df6cab228191109" "ff7625ad8aa2615eae96d6b4469fcc7d3d20b2e1ebc63b761a349bebbb9d23cb" "43c1a8090ed19ab3c0b1490ce412f78f157d69a29828aa977dae941b994b4147" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "e3fc83cdb5f9db0d0df205f5da89af76feda8c56d79a653a5d092c82c7447e02" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" default)))
 '(package-selected-packages
   (quote
    (aggressive-indent markdown-mode feature-mode git-gutter-fringe git-gutter+ pretty-symbols elein clj-refactor highlight-indentation flycheck-joker zenburn-theme json-mode dracula-theme spacemacs-theme smart-mode-line leuven-theme helm-themes intellij-theme fill-column-indicator yaml-mode neotree solarized-theme browse-kill-ring exec-path-from-shell multiple-cursors counsel-projectile projectile ivy-hydra company counsel swiper ivy expand-region highlight-symbol undo-tree paredit magit cider)))
 '(python-shell-interpreter "python3")
 '(safe-local-variable-values
   (quote
    ((cider-figwheel-main-default-options . ":dev")
     (scss-mode
      (css-indent-offset . 2))
     (eval cider-register-cljs-repl-type
	   (quote figwheel+integrant)
	   "(do
              (require 'figwheel-sidecar.repl-api)
              (require 'integrant.repl)
              (integrant.repl/go)
              (figwheel-sidecar.repl-api/cljs-repl))")
     (eval cider-register-cljs-repl-type
	   (quote figwheel+integrant)
	   "(do (require 'figwheel-sidecar.repl-api)
               (require 'integrant.repl)
               (integrant.repl/go)
               (figwheel-sidecar.repl-api/cljs-repl))"))))
 '(scroll-bar-mode nil)
 '(sh-basic-offset 2)
 '(winner-mode t))

;; custom-set-faces was added by Custom.
;; If you edit it by hand, you could mess it up, so be careful.
;; Your init file should contain only one such instance.
;; If there is more than one, they won't work right.

(put 'scroll-left 'disabled nil)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Window Resize
;; Take from https://www.emacswiki.org/emacs/WindowResize
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)

;; Easily move between windows (S-↑, S-→, ...)
(windmove-default-keybindings)

;; Enable Ctrl-H for delete
;; suggested here: https://www.emacswiki.org/emacs/BackspaceKey
(global-set-key (kbd "C-?") 'help-command)
(global-set-key (kbd "M-?") 'mark-paragraph)
(global-set-key (kbd "C-h") 'delete-backward-char)
(global-set-key (kbd "M-h") 'backward-kill-word)

(require 'neotree)
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

(exec-path-from-shell-copy-env "ARTIFACTORY_USER")
(exec-path-from-shell-copy-env "ARTIFACTORY_PASSWORD")

;; Suggested by Andrea to auto-start finops-admin
;;(load-file "~/fc/finops-admin/docs/configs/finops_admin_auto_start.el")

;; Projectile Mode
(projectile-mode +1)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

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
