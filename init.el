;;; init.el --- My emacs setup

;;; Commentary:

;; TODO
;; learn smartparens
;; learn how to use reftex/zotero

;; magit is so slow on windows, better off using cmder

;;; Code:

;; Packages
(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

(defalias 'list-buffers 'ibuffer-other-window)

;; Bootstrap use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

(use-package try
  :ensure t)

(use-package which-key
  :ensure t
  :diminish which-key-mode
  :config (which-key-mode))

(use-package org
  :ensure t
  :config
  (setq org-hide-leading-stars t)
  (setq org-startup-indented t)
  (setq org-edit-src-content-indentation 0)
  (add-hook 'org-mode-hook #'visual-line-mode))

(use-package org-bullets
  :ensure t
  :config (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(use-package org-autolist
  :ensure t
  :config (add-hook 'org-mode-hook (lambda () (org-autolist-mode))))

(use-package switch-window
  :ensure t)

(use-package winner
  :ensure t
  :init (winner-mode))

(use-package counsel
  :ensure t
  :bind
  (("M-y" . counsel-yank-pop)
   :map ivy-minibuffer-map
   ("M-y" . ivy-next-line)))

(use-package ivy
  :ensure t
  :diminish ivy-mode
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-display-style 'fancy)
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-wrap t))

(use-package ido
  :ensure t
  :config
  (ido-mode t)
  (setq ido-save-directory-list-file "C:/Users/Toby/AppData/Roaming/ido.last"))

(use-package monokai-theme
  :ensure t
  :config (load-theme 'monokai t))

(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
 :init (global-undo-tree-mode t))

(use-package move-text
  :ensure t
  :init (move-text-default-bindings))

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode t))

(use-package powerline
  :ensure t
  :config
  (powerline-center-theme)
  (setq powerline-default-separator 'wave))

(use-package expand-region
  :ensure t)

(use-package iedit
  :ensure t)

(use-package ace-jump-mode
  :ensure t)

(use-package smartparens-config
  :ensure smartparens
  :config
  (show-smartparens-global-mode t)
  (smartparens-global-mode t))

(use-package company
  :ensure t
  :config
  (add-hook 'after-init-hook 'global-company-mode))

(use-package auto-yasnippet
  :ensure t)

(add-to-list 'exec-path "C:/Program Files/Git/bin")
(setenv "PATH" (mapconcat #'identity exec-path path-separator))

(use-package magit
  :ensure t
  :config
  (setq magit-completing-read-function 'ivy-completing-read))

;; DWIM
;; better commenting
(defun better-comment-dwim ()
  "Like 'comment-dwim', but toggle comment if cursor is not at end of line."
  (interactive)
  (if (region-active-p)
      (comment-dwim nil)
    (let (($lbp (line-beginning-position))
          ($lep (line-end-position)))
      (if (eq $lbp $lep)
          (progn
            (comment-dwim nil))
        (if (eq (point) $lep)
            (progn
              (comment-dwim nil))
          (progn
            (comment-or-uncomment-region $lbp $lep)
            (forward-line )))))))

;; better narrowing and widening
(defun narrow-or-widen-dwim (p)
  "Widen if buffer is narrowed, narrow-dwim otherwise.
Dwim means: region, org-src-block, org-subtree, or
defun, whichever applies first.  Narrowing to
org-src-block actually calls `org-edit-src-code'.
With prefix P, don't widen, just narrow even if buffer
is already narrowed."
  (interactive "P")
  (declare (interactive-only))
  (cond ((and (buffer-narrowed-p) (not p)) (widen))
        ((region-active-p)
         (narrow-to-region (region-beginning)
                           (region-end)))
        ((derived-mode-p 'org-mode)
         ;; `org-edit-src-code' is not a real narrowing
         ;; command. Remove this first conditional if
         ;; you don't want it.
         (cond ((ignore-errors (org-edit-src-code) t)
                (delete-other-windows))
               ((ignore-errors (org-narrow-to-block) t))
               (t (org-narrow-to-subtree))))
        ((derived-mode-p 'latex-mode)
         (LaTeX-narrow-to-environment))
        (t (narrow-to-defun))))

(define-key ctl-x-map "n" #'narrow-or-widen-dwim)

(defun find-corresponding-file ()
    "Find the file that corresponds to this one."
    (interactive)
    (setq corresponding-file-name nil)
    (setq base-file-name (file-name-sans-extension buffer-file-name))
    (if (string-match "\\.c" buffer-file-name)
       (setq corresponding-file-name (concat base-file-name ".h")))
    (if (string-match "\\.h" buffer-file-name)
       (if (file-exists-p (concat base-file-name ".c")) (setq corresponding-file-name (concat base-file-name ".c"))
	   (setq corresponding-file-name (concat base-file-name ".cpp"))))
    (if (string-match "\\.hin" buffer-file-name)
       (setq corresponding-file-name (concat base-file-name ".cin")))
    (if (string-match "\\.cin" buffer-file-name)
       (setq corresponding-file-name (concat base-file-name ".hin")))
    (if (string-match "\\.cpp" buffer-file-name)
       (setq corresponding-file-name (concat base-file-name ".h")))
    (if corresponding-file-name (find-file corresponding-file-name)
      (error "Unable to find a corresponding file")))

(defun find-corresponding-file-other-window ()
  "Find the file that corresponds to this one."
  (interactive)
  (find-file-other-window buffer-file-name)
  (find-corresponding-file)
  (other-window -1))

;; GUI
(setq inhibit-startup-message t)
(setq frame-title-format "emacs")
(setq initial-scratch-message "")
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(set-face-attribute 'default nil :family "Consolas" :height 110)
(setq split-width-threshold 80)
(setq split-height-threshold nil)

;; maximise
(defun w32-maximize-frame ()
  "Maximize the current frame (windows only)"
  (interactive)
  (w32-send-sys-command 61488))
(add-hook 'window-setup-hook 'w32-maximize-frame t)
(set-frame-parameter nil 'fullscreen 'maximized)

;; editing
(electric-pair-mode 1)
(delete-selection-mode 1)
(fset 'yes-or-no-p 'y-or-n-p)
(setq comment-style 'extra-line)

;; Keys
;; global
(bind-keys*
 ("<f5>" . revert-buffer)
 ("C-c r" . query-replace)
 ("M-g" . goto-line)
 ("C-." . ace-jump-mode)
 ("C-'" . better-comment-dwim)

 ;; auto-yasnippet
 ("C-c w" . aya-create)
 ("C-c e" . aya-expand)

 ;; counsel
 ("M-x" . counsel-M-x)
 ("C-x C-b" . list-buffers)
 ("C-x b" . ivy-switch-buffer)

 ;; misc
 ("C-x g" . magit-status)
 ("M-/" . undo-tree-visualize)
 ("C-=" . er/expand-region)
 ("C-x o" . switch-window)
 ("C-c l" . company-complete)
 )

;; c/c++
(use-package cc-mode
  :defer t
  :config (bind-keys :map c-mode-base-map
	   ("C-c j" . compile)
	   ("C-c k" . next-error)
	   ("C-c s" . find-corresponding-file)
	   ("C-c S" . find-corresponding-file-other-window)))

;; Saving
;; auto-save
(defvar user-temporary-file-directory
  (concat temporary-file-directory "auto-save" "/"))
(setq auto-save-list-file-prefix
      (concat user-temporary-file-directory ".auto-saves-"))
(setq auto-save-file-name-transforms
      `((".*" ,user-temporary-file-directory t)))

;; backups
(setq version-control t     ;; Use version numbers for backups.
      kept-new-versions 10  ;; Number of newest versions to keep.
      kept-old-versions 0   ;; Number of oldest versions to keep.
      delete-old-versions t ;; Don't ask to delete excess backup versions.
      backup-by-copying t)  ;; Copy all files, don't rename them.
(setq vc-make-backup-files t)

;; Default and per-save backups go here:
(setq backup-directory-alist
      `(("." . "C:/Users/Toby/AppData/Roaming/backups/per-save")
        (,tramp-file-name-regexp nil)))

;; Disable saving
;; (setq auto-save-default nil)

(defun force-backup-of-buffer ()
  ;; Make a special "per session" backup at the first save of each
  ;; emacs session.
  (when (not buffer-backed-up)
    ;; Override the default parameters for per-session backups.
    (let ((backup-directory-alist `(("." . "C:/Users/Toby/AppData/Roaming/backups/per-session/")))
          (kept-new-versions 3))
      (backup-buffer)))
  ;; Make a "per save" backup on each save.  The first save results in
  ;; both a per-session and a per-save backup, to keep the numbering
  ;; of per-save backups consistent.
  (let ((buffer-backed-up nil))
    (backup-buffer)))
(add-hook 'before-save-hook  'force-backup-of-buffer)

(provide 'init)
;;; init.el ends here
