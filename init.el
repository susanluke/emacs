(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(tool-bar-mode -1)
(add-to-list 'exec-path "/usr/local/bin/")

(require 'cider)
(require 'magit)
(require 'paredit)
(require 'undo-tree)
(require 'highlight-symbol)
(require 'expand-region)
(require 'ivy)
(require 'swiper)
(require 'counsel)
(require 'company)
(require 'ivy-hydra)
(require 'projectile)
(require 'counsel-projectile)
(require 'multiple-cursors)
(require 'exec-path-from-shell)
(require 'browse-kill-ring)
(require 'fill-column-indicator)

;; Revert buffer automatically
(global-auto-revert-mode 1)

;; Remove trailing whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Make look nice - load theme
(load-theme 'solarized-light t)

;; Make env vars the same in GUI as per shell
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

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
(global-set-key (kbd "C-c <right>") 'hs-hide-block)
(global-set-key (kbd "C-c <left>")  'hs-show-block)

;; Ivy & Counsel setup
(ivy-mode 1)
(counsel-projectile-on)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)
(global-set-key (kbd "C-s") 'swiper)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "C-c r") 'ivy-resume)
(define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)

;; Clojure Mode setup
(add-hook 'clojure-mode-hook 'hs-minor-mode)
(add-hook 'clojure-mode-hook 'paredit-mode)
(add-hook 'clojure-mode-hook 'highlight-symbol-mode)
(add-hook 'clojure-mode-hook 'company-mode)
(add-hook 'clojure-mode-hook 'linum-mode)
(add-hook 'clojure-mode-hook 'show-paren-mode)
(add-hook 'clojure-mode-hook 'fci-mode)

;; CIDER Repl Mode setup
(add-hook 'cider-repl-mode-hook 'paredit-mode)
(setq cider-repl-use-pretty-printing t)

;; Projectile setup
(projectile-mode)
(global-set-key (kbd "C-.") 'counsel-projectile-ag)

;; Multiple Cursors setup
(global-set-key (kbd "C->") 'mc/mark-next-like-this-symbol)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this-symbol)
(global-set-key (kbd "C-c <") 'mc/mark-all-symbols-like-this-in-defun)

;; Browse kill ring aetup
(global-set-key (kbd "C-c k") 'browse-kill-ring)


;; custom-set-variables was added by Custom.
;; If you edit it by hand, you could mess it up, so be careful.
;; Your init file should contain only one such instance.
;; If there is more than one, they won't work right.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cider-lein-parameters "with-profile +e2e repl :headless :host ::")
 '(custom-safe-themes
   (quote
    ("a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" default)))
 '(package-selected-packages
   (quote
    (fill-column-indicator yaml-mode neotree solarized-theme browse-kill-ring exec-path-from-shell multiple-cursors counsel-projectile projectile ivy-hydra company counsel swiper ivy expand-region highlight-symbol undo-tree paredit magit cider))))

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

;; NeoTree to work with Projectile - from https://www.emacswiki.org/emacs/NeoTree
(setq projectile-switch-project-action 'neotree-projectile-action)
(global-set-key [f8] 'neotree-project-dir)
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

