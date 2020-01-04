;;; package --- Summary
;;; Commentary:

(require 'server)
(unless (server-running-p)
  (server-start))

(setq max-lisp-eval-depth 10000)
(setq max-specpdl-size 10000)
(require 'package)

(setq package-enable-at-startup nil)
(add-to-list 'package-archives
      '("melpa" . "https://melpa.org/packages/"))
 (add-to-list 'package-archives
       '("gnu" . "https://elpa.gnu.org/packages/"))
;; (add-to-list 'package-archives
;;      '("melpa3" . "http://www.mirrorservice.org/sites/stable.melpa.org/packages/"))
(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))


(org-babel-load-file (expand-file-name "~/.emacs.d/myinit.org"))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(company-idle-delay 0)
 '(custom-enabled-themes (quote (deeper-blue)))
 '(custom-safe-themes
   (quote
    ("2540689fd0bc5d74c4682764ff6c94057ba8061a98be5dd21116bf7bf301acfb" default)))
 '(display-line-numbers-grow-only nil)
 '(display-line-numbers-width nil)
 '(global-company-mode t)
 '(indicate-buffer-boundaries nil)
 '(js-indent-level 2)
 '(js2-bounce-indent-p t)
 '(js2-mode-indent-ignore-first-tab nil)
 '(line-number-mode nil)
 '(mode-line-format
   (quote
    ("%e"
     (:eval
      (let*
          ((active
            (powerline-selected-window-active))
           (mode-line-buffer-id
            (if active
                (quote mode-line-buffer-id)
              (quote mode-line-buffer-id-inactive)))
           (mode-line
            (if active
                (quote mode-line)
              (quote mode-line-inactive)))
           (face0
            (if active
                (quote powerline-active0)
              (quote powerline-inactive0)))
           (face1
            (if active
                (quote powerline-active1)
              (quote powerline-inactive1)))
           (face2
            (if active
                (quote powerline-active2)
              (quote powerline-inactive2)))
           (separator-left
            (intern
             (format "powerline-%s-%s"
                     (powerline-current-separator)
                     (car powerline-default-separator-dir))))
           (separator-right
            (intern
             (format "powerline-%s-%s"
                     (powerline-current-separator)
                     (cdr powerline-default-separator-dir))))
           (lhs
            (list
             (powerline-raw "%*" face0
                            (quote l))
             (when powerline-display-buffer-size
               (powerline-buffer-size face0
                                      (quote l)))
             (powerline-buffer-id
              (\`
               (mode-line-buffer-id
                (\, face0)))
              (quote l))
             (powerline-raw " " face0)
             (funcall separator-left face0 face1)
             (powerline-narrow face1
                               (quote l))
             (powerline-vc face1)))
           (rhs
            (list
             (powerline-raw global-mode-string face1
                            (quote r))
             (powerline-raw "%4l" face1
                            (quote r))
             (powerline-raw ":" face1)
             (powerline-raw "%3c" face1
                            (quote r))
             (funcall separator-right face1 face0)
             (powerline-raw " " face0)
             (powerline-raw "%6p" face0
                            (quote r))
             (when powerline-display-hud
               (powerline-hud face2 face1))
             (powerline-fill face0 0)))
           (center
            (list
             (powerline-raw " " face1)
             (funcall separator-left face1 face2)
             (when
                 (and
                  (boundp
                   (quote erc-track-minor-mode))
                  erc-track-minor-mode)
               (powerline-raw erc-modified-channels-object face2
                              (quote l)))
             (powerline-major-mode face2
                                   (quote l))
             (powerline-process face2)
             (powerline-raw " :" face2)
             (powerline-raw " " face2)
             (funcall separator-right face2 face1))))
        (concat
         (powerline-render lhs)
         (powerline-fill-center face1
                                (/
                                 (powerline-width center)
                                 2.0))
         (powerline-render center)
         (powerline-fill face1
                         (powerline-width rhs))
         (powerline-render rhs)))))))
 '(org-confirm-babel-evaluate nil)
 '(org-default-notes-file (concat org-directory "/notes.org"))
 '(org-directory "~/Sync/orgfiles")
 '(org-export-html-postamble nil)
 '(org-hide-leading-stars t)
 '(org-src-fontify-natively t)
 '(org-startup-folded (quote overview))
 '(org-startup-indented t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#181a26" :foreground "gray80" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight extra-light :height 167 :width condensed :foundry "MS  " :family "Consolas"))))
 '(border ((t (:background "#14151E"))))
 '(company-scrollbar-bg ((t (:background "gold"))))
 '(company-tooltip ((t (:background "DeepSkyBlue4" :foreground "black"))))
 '(company-tooltip-selection ((t (:background "DeepSkyBlue3"))))
 '(cursor ((t (:background "white smoke"))))
 '(font-lock-function-name-face ((t (:foreground "yellow"))))
 '(font-lock-variable-name-face ((t (:foreground "dark sea green"))))
 '(js2-external-variable ((t (:foreground "deep sky blue"))))
 '(js2-function-call ((t (:foreground "khaki1"))))
 '(js2-function-param ((t (:foreground "cyan"))))
 '(js2-instance-member ((t (:foreground "DeepSkyBlue4"))))
 '(line-number ((t (:inherit (shadow default) :slant italic :height 150))))
 '(line-number-current-line ((t (:inherit line-number :foreground "gold"))))
 '(region ((t (:background "rosy brown"))))
 '(rjsx-attr ((t (:foreground "light steel blue"))))
 '(rjsx-tag ((t (:foreground "steel blue"))))
 '(rjsx-text ((t (:foreground "white smoke"))))
 '(web-mode-html-attr-engine-face ((t (:inherit web-mode-block-delimiter-face))))
 '(web-mode-html-attr-value-face ((t (:inherit font-lock-string-face))))
 '(web-mode-html-tag-face ((t (:foreground "deep sky blue"))))
 '(whitespace-tab ((t (:foreground "#636363")))))
