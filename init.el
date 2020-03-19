;;; init.el --- My emacs set up

;;; Commentary:

;;; Code:

;; Packages
(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

;; Bootstrap use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'diminish)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

;; don't vomit in my init file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

(use-package try
  :ensure t)

(use-package which-key
  :ensure t
  :diminish which-key-mode
  :config (which-key-mode))

(use-package hydra
  :ensure t)

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
  (setq ivy-wrap t)

  ;; use enter on a directory to navigate into the directory, not open it with dired.
  (define-key ivy-minibuffer-map (kbd "C-m") 'ivy-alt-done)

  (use-package ivy-hydra
    :ensure t))

(use-package projectile
  :ensure t
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (setq projectile-completion-system 'ivy)
  (projectile-mode +1))

(use-package ag
  :ensure t)

(use-package org
  :ensure t
  :config
  (setq org-hide-leading-stars t)
  (setq org-startup-indented t)
  (add-hook 'org-mode-hook 'org-indent-mode)

  ;; org-refile stuff
  (setq org-refile-targets '((nil :maxlevel . 9)
			     (org-agenda-files :maxlevel . 9)))
  (setq org-outline-path-complete-in-steps nil)
  (setq org-refile-use-outline-path 'file)
  (setq org-refile-allow-creating-parent-nodes 'confirm)

  ;; org file apps
  (setq org-file-apps (butlast org-file-apps))
  (setq org-file-apps (append org-file-apps '(("\\.pdf::\\([0-9]+\\)\\'" . "zathura -P %1 %s")
					      ("\\.png\\'" . "sxiv %s")
					      ("\\.jpg\\'" . "sxiv %s")
					      ("\\.jpeg\\'" . "sxiv %s")
					      ("\\.gif\\'" . "sxiv %s"))))
  
  (add-to-list 'org-structure-template-alist
	       '("r" "\\begin{align*}\n?\n\\end{align*}"))

  ;; enable fuzzy search in org-refile
  (setq ivy-initial-inputs-alist (cdr (cdr ivy-initial-inputs-alist)))

  (global-set-key (kbd "C-c c") 'org-capture)

  (setq org-capture-templates
	'(("n" "Note" entry (file "~/gdrive/tech/org/notes.org") "* %^{Heading}\n%T\n\n%?\n\n%i" :empty-lines 1 :prepend t)
	  ("s" "Some day" entry (file "~/gdrive/tech/org/someday.org") "* %?\n%i" :empty-lines 1 :prepend t)
	  ("r" "Reading" entry (file+headline "~/gdrive/lit/lit list.org" "Reading List") "* %?\n%i" :empty-lines 1)
	  ("w" "Writing" entry (file+headline "~/gdrive/lit/lit list.org" "Writing List") "* %?\n%i" :empty-lines 1)
	))

  (setq org-blank-before-new-entry '((heading . t) (plain-list-item . nil)))
  
  ;; org-goto
  (setq org-goto-interface 'outline-path-completion)

  ;; org-latex preview size
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 1.5)))

(use-package org-bullets
  :ensure t
  :config (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(use-package org-autolist
  :ensure t
  :config (add-hook 'org-mode-hook (lambda () (org-autolist-mode))))

(use-package interleave
  :ensure t
  )

(use-package worf
  :ensure t
  :defer t
  :init (add-hook 'org-mode-hook (lambda () (progn
					      (worf-mode)
					      (define-key evil-normal-state-map "[ [" 'worf-forward)
					      (define-key evil-normal-state-map "] ]" 'worf-backward)
					      (define-key evil-normal-state-map (kbd "C-c C-h") 'worf-goto)
					      (define-key evil-insert-state-map (kbd "C-c C-h") 'worf-goto))))
  :config
  (define-key worf-mode-map (kbd "[") nil)
  (define-key worf-mode-map (kbd "]") nil)
  )

(use-package switch-window
  :ensure t
  :config
  (setq switch-window-shortcut-style 'qwerty)
  (setq switch-window-qwerty-shortcuts
    '("a" "s" "d" "f" "j" "k" "l" ";" "w" "e" "i" "o")))

(use-package magit
  :ensure t
  :config (setq magit-completing-read-function 'ivy-completing-read))

(use-package keyfreq
  :ensure t
  :config
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))

(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  :config
  ;; enable evil
  (evil-mode 1)

  ;; put all of this somewhere else lol
  
  ;; stop escape being a prefix
  (define-key key-translation-map (kbd "ESC") (kbd "C-g"))

  ;; make escape quit the minibuffer
  (define-key evil-normal-state-map [escape] 'keyboard-quit)
  (define-key evil-visual-state-map [escape] 'keyboard-quit)
  (define-key minibuffer-local-map [escape] 'abort-recursive-edit)
  (define-key minibuffer-local-ns-map [escape] 'abort-recursive-edit)
  (define-key minibuffer-local-completion-map [escape] 'abort-recursive-edit)
  (define-key minibuffer-local-must-match-map [escape] 'abort-recursive-edit)
  (define-key minibuffer-local-isearch-map [escape] 'abort-recursive-edit)

  ;; start modes in insert state
  (add-hook 'git-commit-mode-hook 'evil-insert-state)
  (add-hook 'org-capture-mode-hook 'evil-insert-state)
  (add-hook 'org-src-mode-hook 'evil-insert-state)

  ;; make pasting better
  (defun evil-paste-after-from-0 ()
    (interactive)
    (let ((evil-this-register ?0))
      (call-interactively 'evil-paste-after)))

  (define-key evil-visual-state-map "p" 'evil-paste-after-from-0)

  ;; make evil use switch-window
  (define-key evil-window-map (kbd "C-w") 'switch-window)

  ;; make keys work in normal mode
  (define-key evil-motion-state-map (kbd "RET") nil)
  (define-key evil-motion-state-map (kbd "TAB") nil)

  ;; make search case sensitive when there's a capital letter
  (setq evil-ex-search-case 'sensitive)
)

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init)
  )

(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))

(use-package evil-magit
  :after evil
  :ensure t)

(use-package evil-org
  :after org
  :ensure t
  :config
  (add-hook 'org-mode-hook 'evil-org-mode)
  (add-hook 'evil-org-mode-hook (lambda () (evil-org-set-key-theme))))

(use-package evil-args
  :ensure t
  :init
  (progn
    ;; bind evil-args text objects
    (define-key evil-inner-text-objects-map "a" 'evil-inner-arg)
    (define-key evil-outer-text-objects-map "a" 'evil-outer-arg)

    ;; bind evil-forward/backward-args
    (define-key evil-normal-state-map "L" 'evil-forward-arg)
    (define-key evil-normal-state-map "H" 'evil-backward-arg)
    (define-key evil-motion-state-map "L" 'evil-forward-arg)
    (define-key evil-motion-state-map "H" 'evil-backward-arg)

    ;; bind evil-jump-out-args
    (define-key evil-normal-state-map "K" 'evil-jump-out-args)))

(use-package web-mode
  :ensure t
  :mode ("\\.html$" . web-mode)
  :init
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-css-indent-offset 2)

  (setq web-mode-enable-auto-pairing t)
  (setq web-mode-enable-auto-expanding t)
  (setq web-mode-enable-css-colorization t))

(use-package web-beautify
  :ensure t
  :commands (web-beautify-css
             web-beautify-css-buffer
             web-beautify-html
             web-beautify-html-buffer
             web-beautify-js
             web-beautify-js-buffer))

(use-package winner
  :ensure t
  :init (winner-mode))

(use-package xresources-theme
  :ensure t)

(use-package powerline
  :ensure t
  :config
  (powerline-center-evil-theme))

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

(use-package expand-region
  :ensure t)

(use-package avy
  :ensure t
  :config (setq avy-background t))

(use-package smartparens-config
  :ensure smartparens
  :config
  (show-smartparens-global-mode t)
  (smartparens-global-mode t)

  (sp-local-pair 'org-mode "_" nil :actions :rem))

(use-package company
  :ensure t
  :config
  (add-hook 'after-init-hook 'global-company-mode))

(use-package flyspell
  :ensure t
  :config
  (setq ispell-program-name "aspell")
  (setq flyspell-issue-message-flag nil)
  (setq ispell-extra-args '("--sug-mode=fast"))
  (setq ispell-dictionary "british")
  (add-to-list 'ispell-skip-region-alist '("^#+BEGIN_SRC" . "^#+END_SRC")))

(use-package latex
  :ensure auctex
  :mode ("\\.tex\\'" . latex-mode)
  :init
  (add-hook 'LaTeX-mode-hook (lambda ()
			       (turn-on-reftex)
			       (setq reftex-plug-into-AUCTeX t)
			       (reftex-isearch-minor-mode)))

  :config
  (setq TeX-auto-save t)
  (setq Tex-parse-self t)
  (setq-default TeX-master nil)
  (setq TeX-PDF-mode t)
  (setq TeX-source-correlate-mode 'synctex)
  (setq TeX-source-correlate-start-server t)
  (setq TeX-source-correlate-mode t)

  ;; pdf-tools
  
  ;; Update PDF buffers after successful LaTeX runs
  (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)

  ;; Use pdfview with auctex
  (add-hook 'LaTeX-mode-hook 'pdf-tools-install)
  (setq TeX-view-program-selection '((output-pdf "pdf-tools"))
  	TeX-source-correlate-start-server t)
  (setq TeX-view-program-list '(("pdf-tools" "TeX-pdf-tools-sync-view")))

  ;; zathura
  (add-to-list 'TeX-view-program-selection '(output-pdf "Zathura")))

(use-package reftex
  :ensure t
  :defer t
  :config
  (setq reftex-cite-prompt-optional-args t))

(use-package company-bibtex
  :ensure t
  :after latex
  :config
  (add-to-list 'company-backends 'company-bibtex)
  (setq company-bibtex-bibliography '("~/gdrive/uni/bibliography/references.bib")))

(use-package pdf-tools
  :after latex
  :ensure t
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :config
  (pdf-tools-install))

(defun flyspell-check-next-highlighted-word ()
  "Custom function to spell check next highlighted word."
  (interactive)
  (flyspell-goto-next-error)
  (ispell-word))

(use-package auto-yasnippet
  :ensure t)

(use-package olivetti
  :ensure t
  :bind (("C-c b" . olivetti-toggle-hide-mode-line))
  :config
  (setq olivetti-body-width 80)
  (define-key olivetti-mode-map (kbd "C-c b") '(lambda () (interactive) (olivetti-toggle-hide-mode-line) (force-mode-line-update))))

(use-package focus
  :ensure t)

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

;; (define-key endless/toggle-map "n"
  ;; #'narrow-or-widen-dwim)
;; This line actually replaces Emacs' entire narrowing
;; keymap, that's how much I like this command. Only
;; copy it if that's what you want.
(define-key ctl-x-map "n" #'narrow-or-widen-dwim)
;; (add-hook 'LaTeX-mode-hook
;;           (lambda ()
;;             (define-key LaTeX-mode-map "\C-xn"
;;               nil)))

(defun find-corresponding-file ()
    "Find the file that corresponds to this one."
    (interactive)
    (setq CorrespondingFileName nil)
    (setq BaseFileName (file-name-sans-extension buffer-file-name))
    (if (string-match "\\.c" buffer-file-name)
       (setq CorrespondingFileName (concat BaseFileName ".h")))
    (if (string-match "\\.h" buffer-file-name)
       (if (file-exists-p (concat BaseFileName ".c")) (setq CorrespondingFileName (concat BaseFileName ".c"))
	   (setq CorrespondingFileName (concat BaseFileName ".cpp"))))
    (if (string-match "\\.hin" buffer-file-name)
       (setq CorrespondingFileName (concat BaseFileName ".cin")))
    (if (string-match "\\.cin" buffer-file-name)
       (setq CorrespondingFileName (concat BaseFileName ".hin")))
    (if (string-match "\\.cpp" buffer-file-name)
       (setq CorrespondingFileName (concat BaseFileName ".h")))
    (if CorrespondingFileName (find-file CorrespondingFileName)
       (error "Unable to find a corresponding file")))

(defun find-corresponding-file-other-window ()
  "Find the file that corresponds to this one."
  (interactive)
  (find-file-other-window buffer-file-name)
  (find-corresponding-file)
  (other-window -1))

(defun server-shutdown ()
  "Choose to shutdown frame or server."
  (interactive)
  (let* ((choices '("frame" "server"))
	 (fs-choice (completing-read "Close: " choices)))
    (if (string= fs-choice "frame")
	(delete-frame))
    (if (string= fs-choice "server")
	(progn
	  (save-some-buffers)
	  (kill-emacs)))))

(defun my/dwim-kill ()
  "Kill buffer and window if there are multiple windows open."
  (interactive)
  (ido-kill-buffer)
  (if (> (count-windows) 1)
      (progn
	(delete-window)
	(balance-windows))))

(defvar img-d "~/gdrive/misc/img")
(defun my/img-complete-link ()
  "Create an image link using completion."
  (interactive)
  (org-insert-link nil (concat "file:" (abbreviate-file-name (expand-file-name (read-file-name "Image: " img-d)))) nil))

(define-key org-mode-map (kbd "C-c C-d") 'my/img-complete-link)

;; need to check if before headline!
(defun my/org-render-latex-fragments ()
  "Render org latex fragments on save."
  (save-excursion
    (let (beg end)
    (setq beg (if (org-at-heading-p) (line-beginning-position)
		(outline-previous-heading)
		(point)))
    (setq end (progn (org-end-of-subtree t) (point)))
    (if (org--list-latex-overlays beg end)
	(progn (org-toggle-latex-fragment)
               (org-toggle-latex-fragment))
      (org-toggle-latex-fragment)))))

;; org inlines all images regardless of description??
(defun my/org-render-images ()
  "Render org inline images on save."
  (org-redisplay-inline-images))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook (lambda () (my/org-render-latex-fragments)) nil 'make-it-local)))

(defun my/org-toggle-headings ()
  "Toggle headings in 'org-mode'."
  (interactive)
  (org-toggle-heading (org-current-level)))

(define-key org-mode-map (kbd "C-c C-t") 'my/org-toggle-headings)

;; splits
(global-set-key (kbd "C-x 2") (lambda () (interactive) (split-window-vertically) (other-window 1)))
(global-set-key (kbd "C-x 3") (lambda () (interactive) (split-window-horizontally) (other-window 1)))

(defun split-window-sensibly-prefer-horizontal (&optional window)
"Based on split-window-sensibly, but designed to prefer a horizontal split,
i.e. windows tiled side-by-side."
  (let ((window (or window (selected-window))))
    (or (and (window-splittable-p window t)
         ;; Split window horizontally
         (with-selected-window window
           (split-window-right)))
    (and (window-splittable-p window)
         ;; Split window vertically
         (with-selected-window window
           (split-window-below)))
    (and
         ;; If WINDOW is the only usable window on its frame (it is
         ;; the only one or, not being the only one, all the other
         ;; ones are dedicated) and is not the minibuffer window, try
         ;; to split it horizontally disregarding the value of
         ;; `split-height-threshold'.
         (let ((frame (window-frame window)))
           (or
            (eq window (frame-root-window frame))
            (catch 'done
              (walk-window-tree (lambda (w)
                                  (unless (or (eq w window)
                                              (window-dedicated-p w))
                                    (throw 'done nil)))
                                frame)
              t)))
     (not (window-minibuffer-p window))
     (let ((split-width-threshold 0))
       (when (window-splittable-p window t)
         (with-selected-window window
               (split-window-right))))))))

(defun split-window-really-sensibly (&optional window)
  (let ((window (or window (selected-window))))
    (if (> (window-total-width window) (* 2 (window-total-height window)))
        (with-selected-window window (split-window-sensibly-prefer-horizontal window))
      (with-selected-window window (split-window-sensibly window)))))

(setq
   split-height-threshold 80
   split-width-threshold 160
   split-window-preferred-function 'split-window-really-sensibly)

;; GUI
(setq inhibit-startup-message t)
(setq frame-title-format "emacs")
(setq initial-scratch-message "")
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(global-visual-line-mode)

(add-to-list 'default-frame-alist '(fullscreen . maximized))
(add-to-list 'default-frame-alist '(font . "Deja Vu Sans Mono 12"))

(setq help-window-select t)

;(defalias 'list-buffers 'ibuffer-other-window)
(defalias 'list-buffers 'ibuffer)

(defun fix-xresources ()
  "Fix up xresouces theme."
  (let* ((background (xresources-theme-color "background"))
	 (yellow (xresources-theme-color "color1"))
	 (blue (xresources-theme-color "color4"))
	 (white (xresources-theme-color "color15")))
    (custom-theme-set-faces
     'xresources

     `(isearch ((t (:foreground ,background :weight bold :background ,blue))))
     `(lazy-highlight ((t (:foreground ,background :weight light :background ,yellow))))
     `(sp-show-pair-match-face ((t (:foreground ,white :background ,yellow :weight bold))))
     )))

;; load theme
(defvar my:theme 'xresources)
(defvar my:theme-window-loaded nil)
(defvar my:theme-terminal-loaded nil)

(if (daemonp)
    (add-hook 'after-make-frame-functions(lambda (frame)
					   (select-frame frame)
					   (if (window-system frame)
					       (unless my:theme-window-loaded
						 (if my:theme-terminal-loaded
						     (enable-theme my:theme)
						   (load-theme my:theme t)
						   (fix-xresources))
						 (setq my:theme-window-loaded t))
					     (unless my:theme-terminal-loaded
					       (if my:theme-window-loaded
						   (enable-theme my:theme)
						 (load-theme my:theme t)
						 (fix-xresources))
					       (setq my:theme-terminal-loaded t)))))

  (progn
    (load-theme my:theme t)
    (fix-xresources)
    (if (display-graphic-p)
        (setq my:theme-window-loaded t)
      (setq my:theme-terminal-loaded t))))

;; editing
(electric-pair-mode 1)
(delete-selection-mode 1)
(fset 'yes-or-no-p 'y-or-n-p)
;; (setq comment-style 'extra-line)
(global-auto-revert-mode)

;; Keys
;; global
(bind-keys*
 ("<f5>" . revert-buffer)
 ("C-c r" . query-replace)
 ("M-g" . goto-line)
 ("C-x C-c" . server-shutdown)
 ("C-x k" . my/dwim-kill)
 ("C-x K" . kill-buffer)
 ("C-'" . better-comment-dwim)
 ("C-," . avy-goto-word-or-subword-1)
 ("C-<" . avy-goto-char)

 ;; C-z freezes emacs in i3
 ("C-w C-w" . switch-window)

 ;; flyspell
 ("C-c u" . flyspell-mode)
 ("C-c i" . flyspell-buffer)
 ("C-c I" . ispell-word)
 ("C-c C-i" . flyspell-check-previous-highlighted-word)
 ("C-c M-i" . flyspell-check-next-highlighted-word)
 ("C-c C-M-i" . ispell)

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
 ("C-+" . er/contract-region)
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
      `(("." . "/tmp/backups/per-save")
        (,tramp-file-name-regexp nil)))

;; Disable saving
;; (setq auto-save-default nil)

(defun force-backup-of-buffer ()
  ;; Make a special "per session" backup at the first save of each
  ;; emacs session.
  (when (not buffer-backed-up)
    ;; Override the default parameters for per-session backups.
    (let ((backup-directory-alist `(("." . "/tmp/backups/per-session/")))
          (kept-new-versions 3))
      (backup-buffer)))
  ;; Make a "per save" backup on each save.  The first save results in
  ;; both a per-session and a per-save backup, to keep the numbering
  ;; of per-save backups consistent.
  (let ((buffer-backed-up nil))
    (backup-buffer)))
(add-hook 'before-save-hook  'force-backup-of-buffer)

;; global org-capture

(defadvice org-capture-finalize
    (after delete-capture-frame activate)
  "Advise capture-finalize to close the frame."
  (if (equal "capture" (frame-parameter nil 'name))
      (delete-frame)))

(defadvice org-capture-destroy
    (after delete-capture-frame activate)
  "Advise capture-destroy to close the frame."
  (if (equal "capture" (frame-parameter nil 'name))
      (delete-frame)))

(use-package noflet
  :ensure t)

;; (defun make-capture-frame ()
;;   "Create a new frame and run 'org-capture'."
;;   (interactive)
;;   (make-frame '((name . "capture")))
;;   (select-frame-by-name "capture")
;;   (delete-other-windows)
;;   (noflet ((switch-to-buffer-other-window (buf) (switch-to-buffer buf)))
;; 	  (org-capture)))
(defun activate-capture-frame ()
  "run org-capture in capture frame"
  (select-frame-by-name "capture")
  ;; (switch-to-buffer (get-buffer-create "*scratch*"))
  ;; (org-capture)
  (delete-other-windows)

  (noflet ((switch-to-buffer-other-window (buf) (switch-to-buffer buf)))
	  (org-capture)))

(provide 'init)
;;; init.el ends here
