;; Textmate/Chocolat-inspired keys (from Ike)
;; (setq mac-command-modifier 'meta)
;; (setq mac-option-modifier 'super)

(global-set-key (kbd "s-/")           'comment-or-uncomment-region-or-line)
(global-set-key (kbd "C-x k")         'kill-this-buffer-volatile)


;; Avy (ace-jump equiv and ace-window keybindings)
(global-set-key (kbd "C-c j") 'avy-goto-word-or-subword-1)
(global-set-key (kbd "s-.") 'avy-goto-word-or-subword-1)
(global-set-key (kbd "s-w") 'ace-window)



(provide 'key-bindings)
