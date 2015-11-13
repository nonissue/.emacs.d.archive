;; Textmate/Chocolat-inspired keys (from Ike)
;; (setq mac-command-modifier 'meta)
;; (setq mac-option-modifier 'super)

(global-set-key (kbd "s-/")           'comment-or-uncomment-region-or-line)
(global-set-key (kbd "C-x k")         'kill-this-buffer-volatile)

(defun smart-beginning-of-line ()
  "Move point to first non-whitespace character or beginning-of-line.

Move point to the first non-whitespace character on this line.
If point was already at that position, move point to beginning of line."
  (interactive)
  (let ((oldpos (point)))
    (back-to-indentation)
    (and (= oldpos (point))
         (beginning-of-line))))
(global-set-key [home] 'smart-beginning-of-line)
(global-set-key "\C-a" 'smart-beginning-of-line)


;; Avy (ace-jump equiv and ace-window keybindings)
(global-set-key (kbd "C-c j") 'avy-goto-word-or-subword-1)
(global-set-key (kbd "s-.") 'avy-goto-word-or-subword-1)
(global-set-key (kbd "s-w") 'ace-window)



(provide 'key-bindings)
