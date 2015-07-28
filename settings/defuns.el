
;; Most of this was stolen from Isaac Azuelos

;; Normal Em and En Dashes
(defun insert-em-dash ()
  "Insert a proper Unicode em-dash."
  (interactive)
  (insert "—"))

(defun insert-en-dash ()
  "Insert a proper Unicode en-dash."
  (interactive)
  (insert "–"))

;; Kill line 
(defun backward-kill-line (arg)
  "Kill ARG lines backward."
  (interactive "p")
  (kill-line (- 1 arg)))

;; Textmate-like commenting
;; From http://stackoverflow.com/questions/9688748/
(defun comment-or-uncomment-region-or-line ()
  "Like Textmate's Command-/"
  (interactive)
  (let (start end)
    (if (region-active-p)
        (setq start (region-beginning)
              end   (region-end))
      (setq start (line-beginning-position)
	    end   (line-end-position)))
    (comment-or-uncomment-region start end)))

;; New Empty Frame
(defun new-empty-frame ()
  "Create a new window with a blank untitled buffer."
  (interactive)
  (let ((buffer (generate-new-buffer "untitled")))
    (set-buffer-major-mode buffer)
    (display-buffer buffer '(display-buffer-pop-up-frame . nil))))


;; Rename current buffer
;; source: http://steve.yegge.googlepages.com/my-dot-emacs-file
(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn
          (rename-file name new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))

;; Compile and run Java buffer in ansi-term
(defun compile-and-run-java ()
  "Saves, compiles the current java buffer in ansi-term"
  (interactive)
  (let* ((proc (get-buffer-process "*ansi-term*")))
    (unless proc
      (error "no process found"))
    (save-buffer)
    (process-send-string proc
			 ;; Changing from && for bash to ; for fish
			 ;; (format "javac -cp %s %s ; java -cp %s %s \n"
			 ;; Changed it so it should be more consistent, as there were
			 ;; issues with the -cp command. This way it does not have to be
			 ;; used. Could even set a variable with current location first,
			 ;; and then return after this is run but not sure it's worth it.
			 (format "cd %s ; javac %s ; java %s \n"
				 (shell-quote-argument (file-name-directory buffer-file-name))
				 (shell-quote-argument (buffer-file-name))
				 ;;(shell-quote-argument (file-name-directory buffer-file-name))
				 (shell-quote-argument (file-name-sans-extension (buffer-name)))))
    (switch-to-buffer-other-frame "*ansi-term*")
    ))

;; Kill Buffer Volatile
(defun kill-this-buffer-volatile ()
  "kill the current buffer without confirmation, only if saved."
  (interactive)
  (when (not (buffer-modified-p))
    (kill-buffer (current-buffer))))

;; Edit host file as root
(defun hosts ()
  "Open /etc/hosts as root."
  (interactive)
  (find-file "/sudo::/etc/hosts"))


;; *****************************************
;; The following should be moved to GUI file or common?
;; I guess technically my-bell-function is a defun
;; *****************************************

;; Set alarm bell to visual warning
(setq visible-bell t)

;; Disable alarm bell when over-scrolling because it is annoying
(defun my-bell-function ()
  (unless (memq this-command
		'(isearch-abort abort-recursive-edit exit-minibuffer
				keyboard-quit mwheel-scroll down up next-line previous-line
				backward-char forward-char))
    (ding)))
(setq ring-bell-function 'my-bell-function)

;; *****************************************



;; Org mode hotkeys? Not sure why here
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)

;; Helm config
;; Hotkeys more than defuns?
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z



(provide 'defuns)
