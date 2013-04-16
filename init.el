;; ;;idle-require
;; (add-to-list 'load-path "~/.emacs.d/elisp/idle-mode/")
;; (require 'idle-require)
;; (idle-require-mode t)

;; save desktop
(desktop-save-mode 1) ;; save my files open

;; dead-keys
(require 'iso-transl)

;; show parent
(show-paren-mode t)

;; no transient mark
(transient-mark-mode -1)

;; unset keys
;; (global-unset-key "\C-.")
;; miscellaneous
(global-set-key "\C-xc" 'calendar)
(global-set-key "\C-xt" 'eshell)
(global-set-key (kbd "C-. C-c") 'comment-or-uncomment-region)
(global-set-key (kbd "C-. C-l") 'global-linum-mode) ;; number of line

;;column number
(column-number-mode t)

;;(setq-default tab-width 4)

;; set goal column enable and other defaults of emacs also
(put 'set-goal-column 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; default diccionary
(setq ispell-program-name "aspell")

;; ido mode
(add-to-list 'load-path "~/.emacs.d/elisp/ido-mode/")
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(require 'ido)
(ido-mode t)

;; easy spell check
(global-set-key (kbd "C-x <f8>") 'ispell-change-dictionary)
(global-set-key (kbd "<f8>") 'ispell-word)
(global-set-key (kbd "C-S-<f8>") 'flyspell-mode)
(global-set-key (kbd "M-S-<f8>") 'flyspell-buffer)
(global-set-key (kbd "C-<f8>") 'flyspell-check-previous-highlighted-word)
(defun flyspell-check-next-highlighted-word ()
  "Custom function to spell check next highlighted word"
  (interactive)
  (flyspell-goto-next-error)
  (ispell-word)
  )
(global-set-key (kbd "M-<f8>") 'flyspell-check-next-highlighted-word)


;; MUMAMO
(load "~/.emacs.d/nxhtml/autostart.el")
(setq mumamo-background-colors nil)

;; tabkey2 it seems it's really problematic, I deactivated
;;(tabkey2-mode t)


;; obsolete stuff in mumamo
(when (and (equal emacs-major-version 23)
           (equal emacs-minor-version 3))
  (eval-after-load "bytecomp"
    '(add-to-list 'byte-compile-not-obsolete-vars
                  'font-lock-beginning-of-syntax-function))
  ;; tramp-compat.el clobbers this variable!
  (eval-after-load "tramp-compat"
    '(add-to-list 'byte-compile-not-obsolete-vars
                  'font-lock-beginning-of-syntax-function)))

(when (and (equal emacs-major-version 24)
		   (or (equal emacs-minor-version 1) (equal emacs-minor-version 2)))
  ;;first
  (eval-after-load "bytecomp"
    '(add-to-list 'byte-compile-not-obsolete-vars
                  'font-lock-beginning-of-syntax-function))
  ;; tramp-compat.el clobbers this variable!
  (eval-after-load "tramp-compat"
    '(add-to-list 'byte-compile-not-obsolete-vars
                  'font-lock-beginning-of-syntax-function))
  ;;second
  (eval-after-load "bytecomp"
    '(add-to-list 'byte-compile-not-obsolete-vars
                  'font-lock-syntactic-keywords))
  ;; tramp-compat.el clobbers this variable!
  (eval-after-load "tramp-compat"
    '(add-to-list 'byte-compile-not-obsolete-vars
                  'font-lock-syntactic-keywords))
  ;; if emacs 24 then we can use the great org-babel mode!
  (org-babel-do-load-languages
   'org-babel-load-languages
   '(
	 (sh . t)
	 (python . t)
	 (R . t)
	 (ruby . t)
	 (ditaa . t)
	 (dot . t)
	 (octave . t)
	 (sqlite . t)
	 (sql . t)
	 (perl . t)
	 (plantuml . t)
	 ;;(graphviz . t)
	 ))
  ;; (load "~/.emacs.d/elisp/uml/ob-plantuml.el")
  (setq org-plantuml-jar-path (expand-file-name "~/.emacs.d/elisp/uml/plantuml.jar"))
  (setq org-ditaa-jar-path (expand-file-name "~/.emacs.d/elisp/uml/ditaa.jar"))

  )

;; ;; test yasnippet auto-complete
;; (add-to-list 'load-path "~/.emacs.d/yasnippet")
;; (add-to-list 'load-path "~/.emacs.d/autocomplete/")
;; (require 'yasnippet)
;; (require 'auto-complete)
;; ;; yasnippet
;; (yas-global-mode 1)
;; ;; autocomplete
;; (require 'auto-complete-config)
;; (add-to-list 'ac-dictionary-directories "~/.emacs.d/autocomplete/ac-dict")
;; (set-default 'ac-sources
;;              '(ac-source-abbrev
;;                ac-source-dictionary
;;                ac-source-yasnippet
;;                ac-source-words-in-buffer
;;                ac-source-words-in-same-mode-buffers
;;                ac-source-semantic))
;; (ac-config-default)
;; (dolist (m '(c-mode c++-mode java-mode))
;;   (add-to-list 'ac-modes m))

;; (global-auto-complete-mode t)


;;yasnippet
(add-to-list 'load-path "~/.emacs.d/yasnippet")
(require 'yasnippet)
;; (setq yas/trigger-key (kbd "C-c <tab>"))
;; (setq yas/trigger-key (kbd "C-c TAB"))
;; (yas--initialize)
(yas/global-mode t)
;; keybinding to deactivate yas mode (sometimes it's useful)
(global-set-key (kbd "C-x y") 'yas/minor-mode) 
(global-set-key (kbd "C-x C-y") 'yas-global-mode)
(setq ac-source-yasnippet nil)



;;autocomplete
(add-to-list 'load-path "~/.emacs.d/autocomplete/")
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/autocomplete/ac-dict")
(ac-config-default)

(setq ac-use-menu-map t)
;; Default settings
(define-key ac-menu-map "\C-n" 'ac-next)
(define-key ac-menu-map "\C-p" 'ac-previous)

;; snippets in the auto-completetion
;; (add-to-list 'ac-sources 'ac-source-yasnippet)


;; (add-hook 'org-mode-hook
;;           (let ((original-command (lookup-key org-mode-map [tab])))
;;             `(lambda ()
;;                (setq yas/fallback-behavior
;;                      '(apply ,original-command))
;;                (local-set-key [tab] 'yas/expand))))



;; (ac-set-trigger-key "TAB")
;; (ac-set-trigger-key "<tab>")

;; (add-hook 'c-mode-common-hook '(lambda ()
;;         (add-to-list 'ac-omni-completion-sources
;;                      (cons "\\." '(ac-source-semantic)))
;;         (add-to-list 'ac-omni-completion-sources
;;                      (cons "->" '(ac-source-semantic)))
;;         (setq ac-sources '(ac-source-semantic ac-source-yasnippet))
;; ))
;; (add-hook 'c++-mode-common-hook '(lambda ()
;;         (add-to-list 'ac-omni-completion-sources
;;                      (cons "\\." '(ac-source-semantic)))
;;         (add-to-list 'ac-omni-completion-sources
;;                      (cons "->" '(ac-source-semantic)))
;;         (setq ac-sources '(ac-source-semantic ac-source-yasnippet))
;; ))

;;show all in org-mode
(global-set-key (kbd "C-. C-a") 'show-all)
(global-set-key (kbd "C-. l") 'visual-line-mode)
(global-set-key (kbd "C-. C-b m") 'menu-bar-mode)
(global-set-key (kbd "C-. C-b t") 'tool-bar-mode)


;; if X11 or terminal
(add-to-list 'load-path "~/.emacs.d/themes/")

(case window-system
  (x 
   ;; color theme
   (require 'color-theme)
   (require 'color-theme-mycomidia)
   (color-theme-initialize)
   (color-theme-mycomidia)
   ;; fullscreen
   (defun toggle-fullscreen ()
     "Toggle full screen on X11"
     (interactive)
     (when (eq window-system 'x)
       (set-frame-parameter
		nil 'fullscreen
		(when (not (frame-parameter nil 'fullscreen)) 'fullboth))))
										;(toggle-fullscreen)
   (global-set-key [f11] 'toggle-fullscreen)
   (display-time-mode t)
   )
  (otherwise 
   (setq mumamo-background-colors nil)
   )
  )


;; don't show the scroll
(scroll-bar-mode -1)
;; (menu-bar-mode -1)
(tool-bar-mode -1)

;; open pdf in org with evince
(eval-after-load "org"
  '(progn
     ;; .txt files aren't in the list initially, but in case that changes
     ;; in a future version of org, use if to avoid errors
     ;; (if (assoc "\\.txt\\'" org-file-apps)
     ;;     (setcdr (assoc "\\.txt\\'" org-file-apps) "notepad.exe %s")
     ;;   (add-to-list 'org-file-apps '("\\.txt\\'" . "notepad.exe %s") t))
     ;; Change .pdf association directly within the alist
     (setcdr (assoc "\\.pdf\\'" org-file-apps) "evince %s")))
;; org to pdf with texi2dvi
(setq org-latex-to-pdf-process '("texi2dvi --pdf --clean --verbose --batch %s"))



;;(setq TeX-PDF-mode t)
(setq preview-image-type 'png)
;; org to latex
;; (add-to-list 'org-export-latex-classes
;;              '("article"
;;                "\\documentclass{article}"
;;                ("\\section{%s}" . "\\section*{%s}")
;;                ("\\subsection{%s}" . "\\subsection*{%s}")
;;                ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
;;                ("\\subsubsubsection{%s}" . "\\subsubsubsection*{%s}")
;;                ("\\paragraph{%s}" . "\\paragraph*{%s}")
;;                ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))


;; default files

;;ROS
(add-to-list 'auto-mode-alist '("\\.launch$" . nxml-mumamo-mode)) ;;cakephp

;;octave m files
(autoload 'octave-mode "octave-mod" nil t)
(setq auto-mode-alist
	  (cons '("\\.m$" . octave-mode) auto-mode-alist))

;;cakephp
(add-to-list 'auto-mode-alist '("\\.ctp$" . nxhtml-mumamo-mode)) 
