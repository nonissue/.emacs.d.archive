;; new favourite emacs config
;; because it's hella simple
;; https://github.com/Slava/emacs.d
;; references purcells sanityinc config as well

;;; disable annoying start up screen
(setq inhibit-startup-screen t)

;; disable superfluous gui
(menu-bar-mode -1) 
(toggle-scroll-bar -1) 
(tool-bar-mode -1)
(blink-cursor-mode 0)

;; cask
(require 'cask "/usr/local/share/emacs/site-lisp/cask/cask.el")
(cask-initialize)

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))


;; define is a mac
(defconst *is-a-mac* (eq system-type 'darwin))

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

;;----------------------------------------------------------------------------

;; Fonts -> init-fonts.el
;;----------------------------------------------------------------------------

(defun sanityinc/font-name-replace-size (font-name new-size)
  (let ((parts (split-string font-name "-")))
    (setcar (nthcdr 7 parts) (format "%d" new-size))
    (mapconcat 'identity parts "-")))

(defun sanityinc/set-frame-font-size (size)
    (set-frame-font (sanityinc/font-name-replace-size (face-font 'default) size) t t))


;;----------------------------------------------------------------------------
;; Move to init-mac
;;----------------------------------------------------------------------------


(when (memq window-system '(mac ns))
  (add-to-list 'default-frame-alist '(font . "Source Code Pro-13"))
  (set-face-attribute 'default t :font "Source Code Pro-1")
  (sanityinc/set-frame-font-size 14)
  (define-key global-map (kbd "<s-return>") 'toggle-frame-fullscreen))


;; tern.js
;; not working
;; (add-to-list 'load-path "~/.emacs.d/tern/emacs/")
;; (autoload 'tern-mode "tern.el" nil t)

;;----------------------------------------------------------------------------
;; Theme sht
;;----------------------------------------------------------------------------

;; (require 'nonissue)
;; (load-theme 'darktooth t)
;; (load-theme 'nonissue t)

(load-theme 'spacegray t)
(global-linum-mode t)


;;----------------------------------------------------------------------------
;; Window size and features
;;----------------------------------------------------------------------------

;; indicate frames not in buffer
(setq indicate-empty-lines t)

;; adjust border
(let ((no-border '(internal-border-width . 0)))
  (add-to-list 'default-frame-alist no-border)
  (add-to-list 'initial-frame-alist no-border))

(when (and *is-a-mac* (fboundp 'toggle-frame-fullscreen))
  ;; Command-Option-f to toggle fullscreen mode
  ;; Hint: Customize `ns-use-native-fullscreen'
  (global-set-key (kbd "M-ƒ") 'toggle-frame-fullscreen))

;; adjust opacity
(set-frame-parameter (selected-frame) 'alpha '(100 90))
(add-to-list 'default-frame-alist '(alpha 100 90))

;; highlight line
(global-hl-line-mode 1)

;; show line numbers dynamically with spaces
(defadvice linum-update-window (around linum-dynamic activate)
  (let* ((w (length (number-to-string
                     (count-lines (point-min) (point-max)))))
         (linum-format (concat " %" (number-to-string w) "d ")))
    ad-do-it))

;; desktop save mode
;; Automatically save and restore sessions

;;----------------------------------------------------------------------------
;; move this to init-gui under window?
;;----------------------------------------------------------------------------

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

;; end of gui shit

;;----------------------------------------------------------------------------
;; sane defaults?
;;----------------------------------------------------------------------------

;; fix scratch buffer
(setq initial-scratch-message "")
(setq initial-major-mode 'emacs-lisp-mode)

;; shorten yes/no to y/n
(fset 'yes-or-no-p 'y-or-n-p)

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

;;----------------------------------------------------------------------------
;; Helm Config
;;----------------------------------------------------------------------------

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
;; I don't really like this so I'm disabling
;; (setq helm-ff-ido-style-backspace 'always
;;       helm-ff-auto-update-initial-value t
;;       helm-ff--auto-update-state t)

(with-eval-after-load 'helm-files
  (define-key helm-read-file-map (kbd "<backspace>") 'helm-find-files-up-one-level)
  (define-key helm-find-files-map (kbd "<backspace>") 'helm-find-files-up-one-level))

(setq helm-ff-newfile-prompt-p nil
      helm-ff-skip-boring-files t)

;; Defualt term > sane defualts?
(setq multi-term-program "/bin/bash")

;; Auto-complete mode
(autoload 'auto-complete-mode "auto-complete" nil t)

;;----------------------------------------------------------------------------
;; Mode customizations -> mode specific files .el
;;----------------------------------------------------------------------------

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
(setq web-mode-enable-engine-detection t)

;; haskell mode hook
(add-hook 'haskell-mode-hook 'haskell-indentation-mode)

;; javascript (js2-mode and ac-js2 for autocomplete)

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-hook 'js-mode-hook 'js2-minor-mode)
;; (add-hook 'js2-mode-hook 'ac-js2-mode) 
(setq js2-highlight-level 3)

;; tern.js config
(add-hook 'js2-mode-hook (lambda () (tern-mode t)))

;; if use node.js, we need nice output
(setenv "NODE_NO_READLINE" "1")
(setq inferior-js-program-command "node --interactive")

;;----------------------------------------------------------------------------
;; Require packages
;;----------------------------------------------------------------------------

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
  (require 'avy)
  (require 'ace-window)
  
  (require 'emmet-mode)
  (require 'web-mode)
  (require 'haskell-mode)
  (require 'defuns)
  (require 'my-desktop)
  (require 'key-bindings)
  (require 'js2-mode)
  ;; (require 'ac-js2)
  (require 'js-comint)
  )


(add-hook 'after-init-hook 'my-after-init)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values (quote ((engine . blaze)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
