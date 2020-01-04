(deftheme ob1
  "Created 2019-12-23.")

(custom-theme-set-variables
 'ob1
 '(ansi-color-faces-vector [default default default italic underline success warning error])
 '(ansi-color-names-vector ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(custom-safe-themes (quote ("2540689fd0bc5d74c4682764ff6c94057ba8061a98be5dd21116bf7bf301acfb" default)))
 '(org-confirm-babel-evaluate nil)
 '(org-default-notes-file "~/Sync/orgfiles/notes.org")
 '(org-directory "~/Sync/orgfiles")
 '(org-hide-leading-stars t)
 '(org-src-fontify-natively t)
 '(org-startup-folded (quote overview))
 '(org-startup-indented t)
 '(package-selected-packages (quote (tern-auto-complete tern prettier-js company indium emmet-mode web-mode htmlize noflet org-bullets which-key try use-package)))
 '(tool-bar-mode nil))

(custom-theme-set-faces
 'ob1
 '(default ((t (:inherit nil :stipple nil :background "#181a26" :foreground "gray80" :inverse-video nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 159 :width normal :foundry "MS  " :family "Consolas"))))
 '(cursor ((t (:background "white smoke"))))
 '(font-lock-function-name-face ((t (:foreground "goldenrod1"))))
 '(font-lock-variable-name-face ((t (:foreground "dark sea green"))))
 '(js2-external-variable ((t (:foreground "khaki1"))))
 '(js2-function-call ((t (:foreground "khaki1"))))
 '(js2-function-param ((t (:foreground "cyan"))))
 '(js2-instance-member ((t (:foreground "DeepSkyBlue4"))))
 '(rjsx-attr ((t (:foreground "light steel blue"))))
 '(rjsx-tag ((t (:foreground "steel blue"))))
 '(rjsx-text ((t (:foreground "white smoke"))))
 '(web-mode-html-attr-engine-face ((t (:inherit web-mode-block-delimiter-face))))
 '(web-mode-html-attr-value-face ((t (:inherit font-lock-string-face))))
 '(whitespace-tab ((t (:foreground "#636363")))))

(provide-theme 'ob1)
