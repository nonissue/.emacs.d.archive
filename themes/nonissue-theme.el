(deftheme nonissue "Nonissue theme")

(defvar darktooth-dark4
  (if (display-graphic-p) "#7C6F64" "color-243"))

(defvar darktooth-dark0
  (if (display-graphic-p) "#282828" "color-235"))

(defvar darktooth-dark0_soft
  (if (display-graphic-p) "#32302F" "color-236"))

(defvar darktooth-dark1
  (if (display-graphic-p) "#3C3836" "color-237"))

(defvar darktooth-dark2           (if (display-graphic-p) "#504945" "color-239"))
(defvar darktooth-dark3           (if (display-graphic-p) "#665C54" "color-241"))



(custom-theme-set-faces
 'nonissue
 `(visible-mark-face
   ((t (:foreground unspecified
                    :background unspecified
                    :inverse-video unspecified
                    :inherit 'hl-line))))
 `(linum
   ((t (:background ,darktooth-dark0_soft
                    :foreground ,darktooth-dark2
                    :inherit 'shadow
                    :slant normal))))
 `(fringe
   ((t (:background ,darktooth-dark0))))
 `(vertical-border
   ((t (:foreground ,darktooth-dark1
                    :background unspecified
                    :inherit file-name-shadow)))))

(setq linum-format " %4d ")

(add-to-list 'default-frame-alist '(internal-border-width . 0))
(set-fringe-mode '(8 . 0))

(set-frame-parameter (selected-frame) 'alpha '(95 90))
(add-to-list 'default-frame-alist '(alpha 95 90))

(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'nonissue)
