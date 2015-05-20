;;; Package --- Sumary:
;; emacs configuration

;;; Commentary:
;; Before running properly this installation you will need to install some packages
;;   sudo apt-get install clang libclang-dev # in the case of ubuntu
;;   sudo pip install pylint json-rpc rope jedi cpplint
;; and of course, don't forget to download the git submodules

;;; code:
;; emacs server
(server-start)

;; font
(add-to-list 'default-frame-alist
                       '(font . "DejaVu Sans Mono-11"))
;; no startup message
(setq inhibit-startup-message t)

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

;; my default path for plugins - adding all subfolders
(let ((base "~/.emacs.d/elisp/"))
  (add-to-list 'load-path base)
  (dolist (f (directory-files base))
    (let ((name (concat base "/" f)))
      (when (and (file-directory-p name) 
                 (not (equal f ".."))
                 (not (equal f ".")))
        (add-to-list 'load-path name)))))



;; unset keys
(define-key (current-global-map) (kbd "C-.") nil)
(define-key (current-global-map) (kbd "C-,") nil)
(eval-after-load "flyspell"
	'(define-key flyspell-mode-map (kbd "C-.") nil))
(eval-after-load "php-mode"
	'(define-key php-mode-map (kbd "C-.") nil))



;; save desktop
(load-file "~/.emacs.d/elisp/my-desktop.el")
(global-set-key (kbd "C-. d s") 'my-desktop-save) ;; save
(global-set-key (kbd "C-. d S") 'my-desktop-save-and-clear) ;; save and clear
(global-set-key (kbd "C-. d r") 'my-desktop-read) ;; read
(global-set-key (kbd "C-. d D") 'my-desktop-delete) ;; delete
(global-set-key (kbd "C-. d c") 'my-desktop-change) ;; change
(global-set-key (kbd "C-. d g") 'my-desktop-name) ;; get name
(global-set-key (kbd "C-. d 0") 'desktop-clear) ;; get name

;; navigate
(global-set-key (kbd "C-, C-r")  'windmove-left)
(global-set-key (kbd "C-, C-n") 'windmove-right)
(global-set-key (kbd "C-, C-c")    'windmove-up)
(global-set-key (kbd "C-, C-t")  'windmove-down)


(global-set-key (kbd "s-C-r") 'shrink-window-horizontally)
(global-set-key (kbd "s-C-n") 'enlarge-window-horizontally)
(global-set-key (kbd "s-C-t") 'shrink-window)
(global-set-key (kbd "s-C-c") 'enlarge-window)

;; dead-keys
(require 'iso-transl)

;; recent files
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

;; auctex
;; for this to run, you'll need the installation in root (I'll try to do it later in the git repository)
(load "auctex.el" nil t t)
(load "preview-latex.el" nil t t)
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)

;; org-mode and org-reveal
(add-to-list 'load-path "~/.emacs.d/elisp/org-mode/lisp")
(require 'org-compat)
(eval-after-load "org"
	'(progn
		 (require 'ox-md nil t)
		 (require 'ox-reveal nil t)
		 (require 'ox-beamer nil t)
		 ;; open with evince
		 (setcdr (assoc "\\.pdf\\'" org-file-apps) "evince %s")
		 ))
(setq org-highlight-latex-and-related '(latex))
;; (setq org-pretty-entities t)

;; fill column indicator
;; (require 'fill-column-indicator)
;; (setq fci-rule-width 1)
;; (setq fci-rule-color "DimGray")
;; (add-hook 'c-mode-hook 'fci-mode)
;; (add-hook 'c++-mode-hook 'fci-mode)

;; markdown-mode
(autoload 'markdown-mode "markdown-mode" "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))

;; show parent
(show-paren-mode t)

;; no transient mark
(transient-mark-mode -1)

;; miscellaneous
(global-set-key (kbd "C-x c") 'calendar)
(global-set-key (kbd "C-x t") 'eshell)
(global-set-key (kbd "C-. C-c") 'comment-or-uncomment-region)
(global-set-key (kbd "C-. C-.") 'global-linum-mode) ;; number of line
;; (global-set-key (kbd "C-. C-i") 'irc)

;; scroll by one
(global-set-key (kbd "M-n") (lambda () (interactive) (scroll-up 1)))
(global-set-key (kbd "M-p") (lambda () (interactive) (scroll-down 1)))

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

;; Toggle maximize buffer
(defun toggle-maximize-buffer () "Toggle Maximize buffer"
  (interactive)
  (if (= 1 (length (window-list)))
      (jump-to-register '_) 
    (progn
      (window-configuration-to-register '_)
      (delete-other-windows))))
(global-set-key (kbd "C-. C-t") 'toggle-maximize-buffer) ;; get name

(setq ibuffer-saved-filter-groups
	'(("home"
		  ("emacs-config" (or (filename . ".emacs.d")
							  (filename . "emacs-config")))
		  ("Org" (or (mode . org-mode)
					 (filename . "OrgMode")))
		  ("code" (filename . "code"))
		  ("android" (filename . "programming/android"))
		  ("Help" (or (name . "\*Help\*")
					  (name . "\*Apropos\*")
					  (name . "\*info\*"))))))
(add-hook 'ibuffer-mode-hook 
	'(lambda ()
	     (ibuffer-switch-to-saved-filter-groups "home")))

;; gpg encryption
(require 'epa-file)
(epa-file-enable)

;; navigation
(global-set-key (kbd "C-. C-p") 'beginning-of-defun) 
(global-set-key (kbd "C-. C-n") 'end-of-defun) 
;; hs for navigation
(defun hs-and-shortcuts ()
	"Custom function to spell check next highlighted word"
    (local-set-key (kbd "C-. C-S-s") 'hs-hide-block)
    (local-set-key (kbd "C-. C-s") 'hs-show-block)
    (local-set-key (kbd "C-. C-S-l") 'hs-hide-level)
    (local-set-key (kbd "C-. C-S-a") 'hs-hide-all)
    (local-set-key (kbd "C-. C-a") 'hs-show-all)
    (hs-minor-mode t)
)
(add-hook 'c-mode-common-hook 'hs-and-shortcuts)


;; sage mode
(when (file-exists-p "/usr/bin/sage")
	(add-to-list 'load-path (expand-file-name "~/.emacs.d/elisp/sage_mode/emacs"))
	(require 'sage "sage")
	(setq sage-command "/usr/bin/sage")
)

;;yasnippet
(require 'yasnippet)
(yas/global-mode t)
;; (setq yas/trigger-key (kbd "C-c <tab>"))
;; (setq yas/trigger-key (kbd "C-c TAB"))
;; (yas--initialize)

;; keybinding to deactivate yas mode (sometimes it's useful)
(global-set-key (kbd "C-x y") 'yas/minor-mode) 
(global-set-key (kbd "C-x C-y") 'yas-global-mode)

;; don't activate yasnippet in some script languages modes
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

;; auto-complete
(add-to-list 'load-path "~/.emacs.d/elisp/auto-complete/lib/popup")

;; iedit
(require 'iedit)
(global-set-key (kbd "C-. C-e") 'iedit-mode) 


;; flycheck
(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)
(global-set-key (kbd "C-. f") 'flycheck-mode) 
(eval-after-load 'flycheck
  '(progn
	   (require 'flycheck-google-cpplint)
	   ;; Add Google C++ Style checker.
	   ;; In default, syntax checked by Clang and Cppcheck.
	   (flycheck-add-next-checker 'c/c++-clang
		   '(warnings-only . c/c++-googlelint))))

(custom-set-variables
	'(flycheck-c/c++-googlelint-executable "cpplint")
)

;; ;; ccputils
;; (require 'cpputils-cmake)
;; (add-hook 'c-mode-common-hook
;;           (lambda ()
;;             (if (derived-mode-p 'c-mode 'c++-mode)
;;                 (cppcm-reload-all)
;;               )))
;; ;; OPTIONAL, somebody reported that they can use this package with Fortran
;; (add-hook 'c90-mode-hook (lambda () (cppcm-reload-all)))
;; ;; OPTIONAL, avoid typing full path when starting gdb
;; (global-set-key (kbd "C-c C-g")
;;  '(lambda ()(interactive) (gud-gdb (concat "gdb --fullname " (cppcm-get-exe-path-current-buffer)))))



(require 'irony)
(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'objc-mode-hook 'irony-mode)

(require 'company)
(add-hook 'after-init-hook 'global-company-mode)

(require 'company-irony)
(require 'company-c-headers)
;; (require 'company-inf-python)

(require 'company-anaconda)

(add-to-list 'company-backends 'company-anaconda)
(add-to-list 'company-backends 'company-c-headers)
(add-to-list 'company-backends 'company-irony)


;; (optional) adds CC special commands to `company-begin-commands' in order to
;; trigger completion at interesting places, such as after scope operator
;;     std::|
(add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)



;; ;; ecb
;; (add-to-list 'load-path "~/.emacs.d/elisp/ecb/")
;; (require 'ecb)
;; (require 'ecb-autoloads)
;; (setq stack-trace-on-error t) ;; this prevent some error (maybe it's not right to do it)
;; (setq ecb-layout-name "left-analyse")
;; (setq ecb-tip-of-the-day nil) 
;; ;;; activate and deactivate ecb
;; (global-set-key (kbd "C-. ,") 'ecb-activate)
;; (global-set-key (kbd "C-. C-,") 'ecb-deactivate)
;; ;;; show/hide ecb window
;; (global-set-key (kbd "C-. ;") 'ecb-show-ecb-windows)
;; (global-set-key (kbd "C-. C-;") 'ecb-hide-ecb-windows)
;; ;;; quick navigation between ecb windows
;; (global-set-key (kbd "C-. 1") 'ecb-goto-window-edit1)
;; (global-set-key (kbd "C-. 2") 'ecb-goto-window-directories)
;; (global-set-key (kbd "C-. 3") 'ecb-goto-window-sources)
;; (global-set-key (kbd "C-. 4") 'ecb-goto-window-methods)
;; (global-set-key (kbd "C-. 5") 'ecb-goto-window-compilation)

;; indentiation stuff (maybe some variable is missing for other language
(setq-default indent-tabs-mode nil)
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
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(require 'ido)
(ido-mode t)

;; tramp open files as sudoer inside emacs
(require 'tramp)
;; edit as sudo
(defun sudo-edit (&optional arg)
"Edit currently visited file as root.
With a prefix ARG prompt for a file to visit.
Will also prompt for a file to visit if current
buffer is not visiting a file."
  (interactive "P")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:"
                         (ido-read-file-name "Find file(as root): ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))
(global-set-key (kbd "C-x C-g") 'sudo-edit)
;; advices ido mode to use root with tramp
(defadvice ido-find-file (after find-file-sudo activate)
  "Find file as root if necessary."
  (unless (and buffer-file-name
               (file-writable-p buffer-file-name))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))


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


;; (add-to-list 'load-path "~/.emacs.d/elisp/web-mode/")
;; (require 'web-mode)
;; (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.ctp?\\'" . web-mode))

;; (defun web-mode-hook ()
;;   "Hooks for Web mode."
;;   ;; (setq web-mode-markup-indent-offset 2)
;; )
;; (add-hook 'web-mode-hook  'web-mode-hook)
;; (defun yas-web-mode-fix ()
;;   (web-mode-buffer-refresh)
;;   (indent-for-tab-command))
;; (setq yas/after-exit-snippet-hook 'yas-web-mode-fix)

(when (equal emacs-major-version 24)
	;; if emacs 24 then we can use the great org-babel mode
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

	;; for newest org-mode
	;; (setq org-latex-pdf-process '("texi2dvi --pdf --clean --verbose --batch %f"))
	;; (setq org-latex-pdf-process '("texi2dvi --pdf --verbose --batch %f"))
    ;; (setq org-latex-pdf-process
    ;;     '("pdflatex -interaction nonstopmode %f"
    ;;          "bibtex %f"
    ;;          "pdflatex -interaction nonstopmode %f"
    ;;          "pdflatex -interaction nonstopmode %f"))
    (setq org-latex-pdf-process
        '("latexmk -pdflatex='pdflatex -interaction nonstopmode' -pdf -bibtex -f %f"))
	)

;; others
(global-set-key (kbd "C-. l") 'visual-line-mode)
(global-set-key (kbd "C-. t") 'toggle-truncate-lines)
(global-set-key (kbd "C-. C-b m") 'menu-bar-mode)
(global-set-key (kbd "C-. C-b t") 'tool-bar-mode)

;; show all in org-mode
(global-set-key (kbd "C-. C-a") 'show-all)

;; reftex in org-mode
(defun org-mode-reftex-setup ()
    (load-library "reftex")
    (and (buffer-file-name) (file-exists-p (buffer-file-name))
        (progn
            ;; Reftex should use the org file as master file. See C-h v TeX-master for infos.
            (setq TeX-master t)
            ;; enable auto-revert-mode to update reftex when bibtex file changes on disk
            (global-auto-revert-mode t)
            (reftex-parse-all)
            ))
    (define-key org-mode-map (kbd "C-c r") 'reftex-citation)
    )
(add-hook 'org-mode-hook 'org-mode-reftex-setup)

(add-hook 'LaTeX-mode-hook 'turn-on-reftex)   ; with AUCTeX LaTeX mode
(add-hook 'latex-mode-hook 'turn-on-reftex)   ; with Emacs latex mode

;; zotelo (for zotero)
(require 'zotelo)
(add-hook 'TeX-mode-hook 'zotelo-minor-mode)
(add-hook 'org-mode-hook 'zotelo-minor-mode)

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
		 ("\\chapter{%s}" . "\\chapter*{%s}")
		 ("\\section{%s}" . "\\section*{%s}")
		 ("\\subsection{%s}" . "\\subsection*{%s}")
		 ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
	)

;; if X11 or terminal
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

;; opencl files with c-mode syntax
(setq auto-mode-alist (cons '("\\.cl$" . c-mode) auto-mode-alist))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ecb-options-version "2.40")
 '(safe-local-variable-values (quote ((zotero-collection . #("0" 0 1 (name "*ALL*")))))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; ergoemacs
;; (require 'ergoemacs-mode)
;; (setq ergoemacs-theme nil)
;; (setq ergoemacs-keyboard-layout "es-dv-1")
;; (ergoemacs-mode 1)
