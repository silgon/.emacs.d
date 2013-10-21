;; emacs server
(server-start)

;; unset keys
(define-key (current-global-map) (kbd "C-.") nil)
(eval-after-load "flyspell"
	'(define-key flyspell-mode-map (kbd "C-.") nil))
(eval-after-load "php-mode"
	'(define-key php-mode-map (kbd "C-.") nil))

;; my default path for files
(add-to-list 'load-path "~/.emacs.d/elisp/")

;; save desktop
(desktop-save-mode 1) ;; save my files open

;; dead-keys
(require 'iso-transl)

;; auctex
;; for this to run, you'll need the installation in root (I'll try to do it later in the git repository)
(load "auctex.el" nil t t)
(load "preview-latex.el" nil t t)

;; show parent
(show-paren-mode t)

;; no transient mark
(transient-mark-mode -1)

;; miscellaneous
(global-set-key "\C-xc" 'calendar)
(global-set-key "\C-xt" 'eshell)
(global-set-key (kbd "C-. C-c") 'comment-or-uncomment-region)
(global-set-key (kbd "C-. C-l") 'global-linum-mode) ;; number of line
(global-set-key (kbd "C-. C-i") 'irc)

(global-set-key (kbd "s-C-r") 'shrink-window-horizontally)
(global-set-key (kbd "s-C-n") 'enlarge-window-horizontally)
(global-set-key (kbd "s-C-t") 'shrink-window)
(global-set-key (kbd "s-C-c") 'enlarge-window)

;; don't show the scroll and others
(scroll-bar-mode -1)
;; (menu-bar-mode -1)
(tool-bar-mode -1)
;;column number
(column-number-mode t)


;; toggle visual line and logical line
(defun toggle-line-move-visual ()
	"Toggle behavior of C-n and C-p, by visual line vs logical line."
	(interactive)
	(if line-move-visual
		(progn 
			(setq line-move-visual nil)
			(message "line-move-visual deactivated")
			)
		(progn 
			(setq line-move-visual t)
			(message "line-move-visual enabled")
			)
		)
	)
(global-set-key (kbd "C-. C-v") 'toggle-line-move-visual)

;; irc
(setq rcirc-server-alist
      '(("irc.freenode.net"
         :port 6665
         ;; :connect-function open-tls-stream
         :channels ("#emacs")
			))
	)

;; ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer) ;; Use Ibuffer for Buffer List

(setq ibuffer-saved-filter-groups
	'(("home"
		  ("emacs-config" (or (filename . ".emacs.d")
							  (filename . "emacs-config")))
		  ("martinowen.net" (filename . "martinowen.net"))
		  ("Org" (or (mode . org-mode)
					 (filename . "OrgMode")))
		  ("code" (filename . "code"))
		  ("pynoz" (filename . "www/pynoz"))
		  ("oislas" (filename . "www/oislas"))
		  ("android" (filename . "programming/android"))
		  ;; ("Web Dev" (or (mode . html-mode)
		  ;; 		(mode . css-mode)))
		  ;; ("Subversion" (name . "\*svn"))
		  ;; ("Magit" (name . "\*magit"))
		  ;; ("ERC" (mode . erc-mode))
		  ("Help" (or (name . "\*Help\*")
					  (name . "\*Apropos\*")
					  (name . "\*info\*"))))))
(add-hook 'ibuffer-mode-hook 
	'(lambda ()
	     (ibuffer-switch-to-saved-filter-groups "home")))

;; gpg encryption
(require 'epa-file)
(epa-file-enable)


;; indentiation stuff (maybe some variable is missing for other language
(setq-default indent-line-function 4)
(setq-default tab-width 4)
(setq-default c-basic-offset 4)
(setq-default lisp-indent-offset 4)
(setq-default sgml-basic-offset 4)
(setq-default nxml-child-indent 4)
(setq tab-stop-list (number-sequence 4 200 4))

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
(setq flyspell-use-meta-tab nil)
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

;; android-mode
(add-to-list 'load-path "~/.emacs.d/elisp/android")
(require 'android-mode)
(global-set-key (kbd "C-<f12>") 'android-ant-debug)

;; CEDET
;; (load-file "~/.emacs.d/cedet-1.1/common/cedet.el")

(setq android-mode-sdk-dir "~/programming/android/")
(add-hook 'gud-mode-hook
	(lambda ()
		(add-to-list 'gud-jdb-classpath "~/android-sdk/platforms/android-17/android.jar")
		))

;; etags
(defun find-file-upwards (file-to-find)
	"Recursively searches each parent directory starting from the default-directory.
looking for a file with name file-to-find.  Returns the path to it
or nil if not found."
	(labels
		((find-file-r (path)
			 (let* ((parent (file-name-directory path))
					   (possible-file (concat parent file-to-find)))
				 (cond
					 ((file-exists-p possible-file) possible-file) ; Found
					 ;; The parent of ~ is nil and the parent of / is itself.
					 ;; Thus the terminating condition for not finding the file
					 ;; accounts for both.
					 ((or (null parent) (equal parent (directory-file-name parent))) nil) ; Not found
					 (t (find-file-r (directory-file-name parent))))))) ; Continue
		(find-file-r default-directory)))
(let ((my-tags-file (find-file-upwards "TAGS")))
	(when my-tags-file
		(message "Loading tags file: %s" my-tags-file)
		(visit-tags-table my-tags-file)))

;; tabkey2 it seems it's really problematic, I deactivated
;;(tabkey2-mode t)

;; ;; MUMAMO
;; (load "~/.emacs.d/nxhtml/autostart.el")
;; (setq mumamo-background-colors nil)
;; ;;cakephp
;; (add-to-list 'auto-mode-alist '("\\.ctp$" . nxhtml-mumamo-mode)) 
;; ;; html
;; (add-to-list 'auto-mode-alist '("\\.html$" . nxml-mode)) 

(add-to-list 'load-path "~/.emacs.d/elisp/web-mode/")
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.ctp?\\'" . web-mode))

(defun web-mode-hook ()
  "Hooks for Web mode."
  ;; (setq web-mode-markup-indent-offset 2)
)
(add-hook 'web-mode-hook  'web-mode-hook)
(defun yas-web-mode-fix ()
  (web-mode-buffer-refresh)
  (indent-for-tab-command))
(setq yas/after-exit-snippet-hook 'yas-web-mode-fix)

;; ;; multi web mode
;; (add-to-list 'load-path "~/.emacs.d/elisp/multi-web-mode/")
;; (require 'multi-web-mode)
;; (setq mweb-default-major-mode 'html-mode)
;; (setq mweb-tags '((php-mode "<\\?php\\|<\\? \\|<\\?=" "\\?>")
;; 					 (js-mode "<script +\\(type=\"text/javascript\"\\|language=\"javascript\"\\)[^>]*>" "</script>")
;; 					 (css-mode "<style +type=\"text/css\"[^>]*>" "</style>")))
;; (setq mweb-filename-extensions '("php" "ctp" "htm" "html" "ctp" "phtml" "php4" "php5"))
;; (multi-web-global-mode 1)

;; obsolete stuff in mumamo
(when (and (equal emacs-major-version 23)
		  (equal emacs-minor-version 3))
	(eval-after-load "bytecomp"
		'(add-to-list 'byte-compile-not-obsolete-vars
			 'font-lock-beginning-of-syntax-function))
	;; tramp-compat.el clobbers this variable!
	(eval-after-load "tramp-compat"
		'(add-to-list 'byte-compile-not-obsolete-vars
			 'font-lock-beginning-of-syntax-function))
	;; org to pdf with texi2dvi

	;; using the dirty one, just for now, because of the gnuplot eps files
	(setq org-latex-to-pdf-process '("texi2dvi --pdf --clean --verbose --batch %s"))
	;; (setq org-latex-to-pdf-process '("texi2dvi --pdf --verbose --batch %s"))

	)

(when (equal emacs-major-version 24)
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
	(eval-after-load "mumamo"
		'(setq mumamo-per-buffer-local-vars
			 (delq 'buffer-file-name mumamo-per-buffer-local-vars)))
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
			 (latex . t)
			 (plantuml . t)
			 ;;(graphviz . t)
			 ))
	;; (load "~/.emacs.d/elisp/uml/ob-plantuml.el")
	(setq org-plantuml-jar-path (expand-file-name "~/.emacs.d/elisp/uml/plantuml.jar"))
	(setq org-ditaa-jar-path (expand-file-name "~/.emacs.d/elisp/uml/ditaa.jar"))

	;; org to pdf with texi2dvi
	(setq org-latex-to-pdf-process '("texi2dvi --pdf --clean --verbose --batch %f"))
	;; (setq org-latex-to-pdf-process '("texi2dvi --pdf --verbose --batch %f"))

	)

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
(add-hook 'eshell-mode-hook
	(lambda ()
		(setq yas/minor-mode nil)
		))
(add-hook 'inferior-octave-mode-hook
	(lambda ()
		(setq yas/minor-mode nil)
		))
(add-hook 'inferior-python-mode-hook
	(lambda ()
		(setq yas/minor-mode nil)
		))

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

;;show all in org-mode
(global-set-key (kbd "C-. C-a") 'show-all)
(global-set-key (kbd "C-. l") 'visual-line-mode)
(global-set-key (kbd "C-. t") 'toggle-truncate-lines)
(global-set-key (kbd "C-. C-b m") 'menu-bar-mode)
(global-set-key (kbd "C-. C-b t") 'tool-bar-mode)

;; reftex in org-mode
(defun org-mode-reftex-setup ()
  (load-library "reftex")
  (and (buffer-file-name) (file-exists-p (buffer-file-name))
       (progn
     ;enable auto-revert-mode to update reftex when bibtex file changes on disk
         (global-auto-revert-mode t)
         (reftex-parse-all)
     ))
  (define-key org-mode-map (kbd "C-c r") 'reftex-citation)
  )
(add-hook 'org-mode-hook 'org-mode-reftex-setup)

(require 'org-latex)
(unless (boundp 'org-export-latex-classes)
	(setq org-export-latex-classes nil))
;; types in org-mode
(add-to-list 'org-export-latex-classes
	'("report"
		 "\\documentclass{report}"
		 ;; ("\\part{%s}" . "\\part*{%s}")
		 ("\\chapter{%s}" . "\\chapter*{%s}")
		 ("\\section{%s}" . "\\section*{%s}")
		 ("\\subsection{%s}" . "\\subsection*{%s}")
		 ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
	)
(add-to-list 'org-export-latex-classes
	'("myreport"
		 "\\documentclass{scrreprt}
\\usepackage[utf8]{inputenc}
\\usepackage[T1]{fontenc}
\\usepackage{fixltx2e}
\\usepackage{graphicx}
\\usepackage{longtable}
\\usepackage{float}
\\usepackage{wrapfig}
\\usepackage{soul}
\\usepackage{textcomp}
\\usepackage{marvosym}
\\usepackage{amssymb}
\\usepackage{amsmath}
\\usepackage{hyperref}
[NO-DEFAULT-PACKAGES]
[PACKAGES]
[EXTRA]
"	
		 ;; ("\\part{%s}" . "\\part*{%s}")
		 ("\\chapter{%s}" . "\\chapter*{%s}")
		 ("\\section{%s}" . "\\section*{%s}")
		 ("\\subsection{%s}" . "\\subsection*{%s}")
		 ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
	)

;; if X11 or terminal
(add-to-list 'load-path "~/.emacs.d/themes/")
(add-to-list 'load-path "~/.emacs.d/color-theme/")

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

;; open pdf in org with evince
(eval-after-load "org"
	'(progn
		 ;; .txt files aren't in the list initially, but in case that changes
		 ;; in a future version of org, use if to avoid errors
		 ;; (if (assoc "\\.txt\\'" org-file-apps)
		 ;;     (setcdr (assoc "\\.txt\\'" org-file-apps) "notepad.exe %s")
		 ;;   (add-to-list 'org-file-apps '("\\.txt\\'" . "notepad.exe %s") t))
		 ;; Change .pdf association directly within the alist
		 ;; (setcdr (assoc "\\.pdf\\'" org-file-apps) "evince %s")))
		 (setcdr (assoc "\\.pdf\\'" org-file-apps) "okular %s")))

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


;; aliases
;; (defalias 'html-mode 'nxml-mode)
;; (defalias 'xml-mode 'nxml-mode)

;; major modes

;; (add-to-list 'load-path "~/.emacs.d/elisp/uml/")
;; (require 'plantuml-mode)

;; php mode
(require 'php-mode)

;; cmake mode
(require 'cmake-mode)

;; default files
(add-to-list 'auto-mode-alist '("CMakeLists.txt" . cmake-mode)) ;;cmake

;;ROS
(add-to-list 'auto-mode-alist '("\\.launch$" . nxml-mode)) ;;cakephp

;;octave m files
;; (autoload 'octave-mode "octave-mod" nil t) ;; ATTENION: octave-mod without the E is alright
(setq auto-mode-alist (cons '("\\.m$" . octave-mode) auto-mode-alist))
(setq-default octave-comment-start "% ")

