(setq load-prefer-newer t)
(package-initialize)

(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))

;; maximise
(defun w32-maximize-frame ()
  "Maximize the current frame (windows only)"
  (interactive)
  (w32-send-sys-command 61488))
(add-hook 'window-setup-hook 'w32-maximize-frame t)
(set-frame-parameter nil 'fullscreen 'maximized)

;; backups
(setq auto-save-default nil)
(setq make-backup-files nil)
(setq auto-save-list-file-prefix nil)
(setq smex-save-file "C:/Users/Toby/AppData/Roaming/.smex")

(windmove-default-keybindings)
(delete-selection-mode 1)
(load-theme 'monokai t)

(setq frame-title-format "emacs")
(setq initial-scratch-message "")
(setq default-directory "~/")

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(show-paren-mode)
(column-number-mode)
(winner-mode t)
(global-nlinum-mode t)
(electric-pair-mode 1)
(global-undo-tree-mode)

(powerline-center-theme)
(setq powerline-default-separator 'wave)
(set-face-attribute 'default nil :height 110 :family "Consolas")

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)
(global-set-key (kbd "M-/") 'undo-tree-visualize)
(global-set-key (kbd "C->") 'ace-jump-mode)

(require 'multiple-cursors)
(global-set-key (kbd "C-}") 'mc/mark-next-like-this)
(global-set-key (kbd "C-{") 'mc/mark-previous-like-this)

(require 'switch-window)
(global-set-key (kbd "C-M-z") 'switch-window) 

(require 'auto-complete)
(defun auto-complete-mode-maybe ()
  (unless (minibufferp (current-buffer))
    (auto-complete-mode 1)))
(global-auto-complete-mode t)

(require 'ido)
(ido-mode t)
(setq ido-save-directory-list-file "C:/Users/Toby/AppData/Roaming/ido.last")

(require 'org)
(require 'ox)
(require 'ox-latex)
(setq org-indent-mode t)
(setq org-hide-leading-stars t)
(setq org-startup-indented t)

(setq org-todo-keywords '((sequence "TODO(t)" "WAITING(w)" "CANCELLED(c)" "|" "DONE(d)")))
(setq org-todo-keyword-faces '(("CANCELLED" . "yellow")))

(setq org-completion-use-ido t)
(setq org-return-follows-link t)
(setq org-log-done 'time)
(setq org-log-into-drawer t)
(setq org-image-actual-width nil)
(setq org-startup-with-latex-preview t)

(setq org-latex-create-formula-image-program 'dvipng)
(org-babel-do-load-languages 'org-babel-load-languages '((latex . t)))

;; Org following links
(setq org-link-frame-setup
   (quote
    ((vm . vm-visit-folder-other-frame)
     (vm-imap . vm-visit-imap-folder-other-frame)
     (gnus . org-gnus-no-new-news)
     (file . find-file)
     (wl . wl-other-frame))))

;; Make windmove work in org-mode
(add-hook 'org-shiftup-final-hook 'windmove-up)
(add-hook 'org-shiftleft-final-hook 'windmove-left)
(add-hook 'org-shiftdown-final-hook 'windmove-down)
(add-hook 'org-shiftright-final-hook 'windmove-right)

(setq org-outline-path-complete-in-steps nil) 
(setq org-refile-use-outline-path t)                 

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-iswitchb)

(require 'wrap-region)
(wrap-region-add-wrappers
   '(("*" "*" nil (org-mode))
     ("~" "~" nil (org-mode))
     ("/" "/" nil (org-mode))
     ("=" "=" nil (org-mode))
     ("+" "+" nil (org-mode))
     ("_" "_" nil (org-mode))
     ("$" "$" nil (org-mode latex-mode))))
(add-hook 'org-mode-hook 'wrap-region-mode)
(add-hook 'latex-mode-hook 'wrap-region-mode)

(require 'org-autolist)
(add-hook 'org-mode-hook (lambda () (org-autolist-mode)))

;; word-wrapping in org-mode
(add-hook 'org-mode-hook #'(lambda () (visual-line-mode)))
;; (defun t-word-wrap()
;;   (turn-on-auto-fill)
;;   (setq-default fill-column -1)
;;   (setq auto-hscroll-mode nil))
;; (add-hook 'text-mode-hook 't-word-wrap)

;; enable move-text
(move-text-default-bindings)

(custom-set-variables
 '(inhibit-startup-buffer-menu t)
 '(inhibit-startup-screen 1)
 '(org-tags-column 0))
