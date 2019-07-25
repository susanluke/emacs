



(add-to-list 'exec-path "/usr/local/bin/")










;; Make env vars the same in GUI as per shell
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))









;;(setq custom-file (make-conf-path "custom.el"))

;; Clojure Mode setup
;; from https://github.com/clojure-emacs/clj-refactor.el
;;(require 'clj-refactor)
;;(defun my-clojure-mode-hook ()
;;    (clj-refactor-mode 1)
;;    (yas-minor-mode 1) ; for adding require/use/import statements
;;    ;; This choice of keybinding leaves cider-macroexpand-1 unbound
;;    (cljr-add-keybindings-with-prefix "C-c C-m"))
(add-hook 'clojure-mode-hook #'my-clojure-mode-hook)






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
    (git-gutter kibit-helper markdown-preview-eww markdown-mode+ flycheck-clojure aggressive-indent markdown-mode feature-mode git-gutter-fringe git-gutter+ pretty-symbols elein clj-refactor highlight-indentation flycheck-joker zenburn-theme json-mode dracula-theme spacemacs-theme smart-mode-line leuven-theme helm-themes intellij-theme fill-column-indicator yaml-mode neotree solarized-theme browse-kill-ring exec-path-from-shell multiple-cursors counsel-projectile projectile ivy-hydra company counsel swiper ivy expand-region highlight-symbol undo-tree paredit magit cider)))
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



(exec-path-from-shell-copy-env "ARTIFACTORY_USER")
(exec-path-from-shell-copy-env "ARTIFACTORY_PASSWORD")

;; Suggested by Andrea to auto-start finops-admin
;;(load-file "~/fc/finops-admin/docs/configs/finops_admin_auto_start.el")


;; from andrea - to get helm to size nicely - but it doesn't seem to worc
(helm-autoresize-mode t)

(setq helm-buffers-fuzzy-matching t
      helm-recentf-fuzzy-match t
      helm-locate-fuzzy-match t
      helm-use-frame-when-more-than-two-windows nil
      helm-M-x-fuzzy-match t)

(global-set-key (kbd "M-y") 'helm-show-kill-ring)



;; https://emacs.stackexchange.com/questions/15168/show-helm-candidates-at-left-immediately-above-minibuffer
;; So that helm does not use current window to display the helm window
(setq helm-split-window-in-side-p nil)


(defun my-helm-split-window (window)
  (if (one-window-p t)
      ;; With just window helm does the right thing
      (split-window
       (selected-window) nil (if (eq helm-split-window-default-side 'other)
                                 'below helm-split-window-default-side))

    ;; If there are multiple windows, select the bottom-left window
    (while (window-in-direction 'left)
      (select-window (window-in-direction 'left)))
    (while (window-in-direction 'below)
      (select-window (window-in-direction 'below)))

    (selected-window)))

(setq helm-split-window-preferred-function #'my-helm-split-window)
