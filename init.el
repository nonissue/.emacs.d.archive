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
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
;; (require 'nonissue)
(load-theme 'nonissue t)
(load-theme 'darktooth t)

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




(defun my-after-init ()
  (sml/setup)
  (require 'dash)
  (require 's)
  (recentf-mode)
  (savehist-mode)
  (require 'saveplace)
  ;; (company-quickhelp-mode)
  (smex-initialize)
  (helm-mode)
  ;;(require 'helm-smex)
  (line-number-mode)
  (column-number-mode))
  

(add-hook 'after-init-hook 'my-after-init)
