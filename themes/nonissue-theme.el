(deftheme nonissue "Nonissue theme")

(custom-theme-set-faces
 'nonissue
 `(visible-mark-face
   ((t (:foreground unspecified
                    :background unspecified
                    :inverse-video unspecified
                    :inherit 'hl-line))))
 `(linum
   ((t (:foreground unspecified
                    :inherit 'shadow
                    :slant normal))))
 `(fringe
   ((t (:background unspecified))))
 `(vertical-border
   ((t (:foreground unspecified
                    :background unspecified
                    :inherit file-name-shadow)))))

(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'nonissue)
