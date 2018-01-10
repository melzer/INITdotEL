(setq load-prefer-newer t)
(package-initialize)

(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))

;; maximise
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; backups
(setq version-control t     ;; Use version numbers for backups.
      kept-new-versions 10  ;; Number of newest versions to keep.
      kept-old-versions 0   ;; Number of oldest versions to keep.
      delete-old-versions t ;; Don't ask to delete excess backup versions.
      backup-by-copying t)  ;; Copy all files, don't rename them.
(setq vc-make-backup-files t)
(setq auto-save-list-file-prefix nil)
;; Default and per-save backups go here:
(setq backup-directory-alist '(("" . "/tmp/backups/per-save")))
(defun force-backup-of-buffer ()
  ;; Make a special "per session" backup at the first save of each
  ;; emacs session.
  (when (not buffer-backed-up)
    ;; Override the default parameters for per-session backups.
    (let ((backup-directory-alist '(("" . "/tmp/backups/per-session")))
          (kept-new-versions 3))
      (backup-buffer)))
  ;; Make a "per save" backup on each save.  The first save results in
  ;; both a per-session and a per-save backup, to keep the numbering
  ;; of per-save backups consistent.
  (let ((buffer-backed-up nil))
    (backup-buffer)))
(add-hook 'before-save-hook  'force-backup-of-buffer)
(setq smex-save-file "~/.smex")

(windmove-default-keybindings)
(delete-selection-mode 1)
(load-theme 'monokai t)

(setq frame-title-format "emacs")
(setq initial-scratch-message "")
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
(ac-flyspell-workaround)

(require 'ido)
(ido-mode t)
(setq ido-save-directory-list-file "/tmp/ido.last")

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

(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
(require 'org-autolist)
(add-hook 'org-mode-hook (lambda () (org-autolist-mode)))

;; spell check
(require 'flyspell)
(setq flyspell-issue-message-flag nil)
(setq ispell-extra-args '("--sug-mode=fast"))
(setq ispell-dictionary "british")
(add-to-list 'ispell-skip-region-alist '("^#+BEGIN_SRC" . "^#+END_SRC"))

(global-set-key (kbd "C-x o") 'flyspell-mode)
(global-set-key (kbd "C-x p") 'flyspell-buffer)
(global-set-key (kbd "C-x P") 'ispell-word)
(global-set-key (kbd "C-x C-M-p") 'ispell)
(global-set-key (kbd "C-x C-p") 'flyspell-check-previous-highlighted-word)
(defun flyspell-check-next-highlighted-word ()
  "Custom function to spell check next highlighted word"
  (interactive)
  (flyspell-goto-next-error)
  (ispell-word))
(global-set-key (kbd "C-x M-p") 'flyspell-check-next-highlighted-word)

;; enable move-text
(move-text-default-bindings)

;; word-wrapping in org-mode
(add-hook 'org-mode-hook #'(lambda () (visual-line-mode)))
;; (defun t-word-wrap()
;;   (turn-on-auto-fill)
;;   (setq-default fill-column -1)
;;   (setq auto-hscroll-mode nil))
;; (add-hook 'text-mode-hook 't-word-wrap)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(inhibit-startup-buffer-menu t)
 '(inhibit-startup-screen 1)
 '(org-startup-with-latex-preview nil)
 '(org-tags-column 0))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
