;; disable annoying start up screen
(setq inhibit-startup-screen t)

;; disable superfluous gui
(menu-bar-mode -1) 
(toggle-scroll-bar -1) 
(tool-bar-mode -1)
(blink-cursor-mode 0)

;; cask
(require 'cask "/usr/local/share/emacs/site-lisp/cask.el")
(cask-initialize)

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;; sml config
(setq sml/theme 'automatic
      sml/mode-width 'full
      sml/name-width '(0 . 20)
      sml/replacer-regexp-list
      '(("^~/org/" ":O:")
        ("^~/\\.emacs\\.d/" ":ED:")))

;; disable warning for custom themes
(setq custom-safe-themes t)

;; theme directory
(add-to-list 'load-path "~/.emacs.d/settings")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
;; (require 'nonissue)

(load-theme 'darktooth t)
(load-theme 'nonissue t)
(global-linum-mode t)

;; desktop save mode
;; Automatically save and restore sessions


;; This fixes line num background missing on lines that wrap
;; not sure if it's worth it.
(defvar endless/margin-display
  `((margin left-margin) ,(propertize "     " 'face 'linum))
  "String used on the margin.")

(defvar-local endless/margin-overlays nil
  "List of overlays in current buffer.")

(defun endless/setup-margin-overlays ()
  "Put overlays on each line which is visually wrapped."
  (interactive)
  (let ((ww (- (window-width)
               (if (= 0 (or (cdr fringe-mode) 1)) 1 0)))
        ov)
    (mapc #'delete-overlay endless/margin-overlays)
    (save-excursion
      (goto-char (point-min))
      (while (null (eobp))
        ;; On each logical line
        (forward-line 1)
        (save-excursion
          (forward-char -1)
          ;; Check if it has multiple visual lines.
          (while (>= (current-column) ww)
            (endles/make-overlay-at (point))
            (forward-char (- ww))))))))

(defun endles/make-overlay-at (p)
  "Create a margin overlay at position P."
  (push (make-overlay p (1+ p)) endless/margin-overlays)
  (overlay-put
   (car endless/margin-overlays) 'before-string
   (propertize " "  'display endless/margin-display)))




(add-hook 'linum-before-numbering-hook #'endless/setup-margin-overlays)

;; fix scratch buffer
(setq initial-scratch-message "")
(setq initial-major-mode 'emacs-lisp-mode)

;; set initial buffer to notes
;; disabled until i learn org-mode
;; (setq remember-notes-initial-major-mode 'org-mode)
;; (setq initial-buffer-choice 'remember-notes)

;; shorten yes/no to y/n
(fset 'yes-or-no-p 'y-or-n-p)

;; fix scrolling
(setq scroll-conservatively 10000
      scroll-preserve-screen-position t)

;; indent with spaces
(setq-default indent-tabs-mode nil)

;; back up configuration
(setq backup-directory-alist '((".*" . "~/.emacs.d/backup")))
(setq version-control t)
(setq delete-old-versions t)

;; autosave configuration
(setq auto-save-list-file-prefix "~/.emacs.d/autosave/")
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/autosave/" t)))

;; display buffer name in frame title
(setq frame-title-format
      '("" invocation-name ": " (:eval (replace-regexp-in-string
                                        "^ +" "" (buffer-name)))))

;; display fringe in visual line mode
(setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))

;; save other app clipboard to kill ring if possible
(setq save-interprogram-paste-before-kill t)

;; move recentf save files
(setq recentf-save-file "~/.emacs.d/etc/recentf"
      recentf-max-saved-items 50)

;; move M-? save files
(setq savehist-file "~/.emacs.d/etc/savehist"
      history-length 150)

;; move save place in file
(setq-default save-place t)
(setq save-place-file "~/.emacs.d/etc/saveplace")

;; move bookmarks
(setq bookmark-default-file "~/.emacs.d/etc/bookmarks")

;; keybindings

;; Change M-x, original M-x rebound 
;; (global-set-key (kbd "M-x") 'helm-smex)
;; (global-set-key (kbd "C-c M-x") 'execute-extended-command)

;; Helm
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x b") 'helm-buffers-list)
(global-set-key (kbd "M-x") 'helm-M-x)
;; (global-set-key (kbd "<f10>") 'helm-resume)

;; helm-projectile set up
(projectile-global-mode)
(setq projectile-completion-system 'helm)
(setq projectile-switch-project-action 'helm-projectile)
(helm-projectile-on)

;; helm customization
(setq helm-ff-ido-style-backspace 'always
      helm-ff-auto-update-initial-value t
      helm-ff--auto-update-state t)

(with-eval-after-load 'helm-files
  (define-key helm-read-file-map (kbd "<backspace>") 'helm-find-files-up-one-level)
  (define-key helm-find-files-map (kbd "<backspace>") 'helm-find-files-up-one-level))

(setq helm-ff-newfile-prompt-p nil
      helm-ff-skip-boring-files t)

(setq multi-term-program "/bin/bash")

;; mode customizations
;; move?
;; markdown
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; Emmet mode hooks
;; move?
(add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
(add-hook 'css-mode-hook  'emmet-mode) ;; enable Emmet's css abbreviation.

;; web mode hooks
;; enabled for html since meteor uses erb in html
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

;; haskell mode hook
(add-hook 'haskell-mode-hook 'haskell-indentation-mode)

(defun my-after-init ()
  (sml/setup)
  (require 'dash)
  (require 's)
  (recentf-mode)
  (savehist-mode)
  (require 'saveplace)
  (require 'multi-term)
  (require 'magit)
  
  ;; (company-quickhelp-mode)
  (require 'evil)
  (require 'evil-leader)
  (evil-mode)
  (smex-initialize)
  (helm-mode)
  (require 'projectile)
  (require 'helm-projectile)
  (require 'elpy)
  (elpy-enable)
  (elpy-use-ipython)
  ;;(require 'helm-smex)
  (column-number-mode)
  
  (require 'emmet-mode)
  (require 'web-mode)
  (require 'haskell-mode)
  (require 'defuns)
  (require 'my-desktop)
  (require 'key-bindings)
  )


(add-hook 'after-init-hook 'my-after-init)
