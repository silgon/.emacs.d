;;; Package --- Sumary:
;; emacs configuration

;; WARNING: for Emacs 24.5 some tramp version, you have to add the following to your /etc/hosts file. Reference: https://stackoverflow.com/a/36380255/2237916
;;  127.0.0.1  host.does.not.exist

;;; Commentary:
;; Before running properly this installation you will need to install some packages
;;   sudo apt-get install clang libclang-dev # in the case of ubuntu
;;   sudo pip install pylint json-rpc rope jedi cpplint
;; and of course, don't forget to download the git submodules
;; for php:
;; sudo apt-get install cscope php7
;; as for golang, be sure to have the following variables in your environment
;; export PATH=$PATH:$(go env GOPATH)/bin
;; export GOPATH=$(go env GOPATH)


;; title of my emacs
(setq-default frame-title-format '("%f [%m]"))
;;; code:
;; emacs server
(require 'server)
(unless (server-running-p)
  (server-start))

;; font
(add-to-list 'default-frame-alist
                       '(font . "DejaVu Sans Mono-11"))
;; no startup message
(setq inhibit-startup-message t)
;; install the packages I require
(require 'package)
(setq package-list '(auctex flycheck company iedit auto-complete irony cpputils-cmake markdown-mode web-mode yasnippet org ctable flycheck-irony yaml-mode company-irony-c-headers idomenu outline-magic ess ido-vertical-mode find-file-in-project company-web  minizinc-mode multiple-cursors dockerfile-mode gitignore-mode protobuf-mode js2-mode js2-refactor helm-bibtex exec-path-from-shell vue-mode go-mode conda lsp-mode quelpa use-package))
;; lsp-mode lsp-ui
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;; (setenv "PATH"
;;   (concat
;;       (getenv "PATH") ":"
;;       (substring (shell-command-to-string "go env GOPATH") 0 -1)"/bin"
;;   )
;; )
;; (setenv "GOPATH" (substring (shell-command-to-string "go env GOPATH") 0 -1))

; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

(require 'rx)
; install the missing packages
(dolist (package package-list)
 (unless (package-installed-p package)
   (package-install package)))

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
(eval-after-load "flyspell"
	'(define-key flyspell-mode-map (kbd "C-,") nil))
(eval-after-load "org"
	'(define-key org-mode-map (kbd "C-,") nil))
(eval-after-load "php-mode"
	'(define-key php-mode-map (kbd "C-.") nil))


(setq flycheck-checker-error-threshold 1000)

(setq
    conda-anaconda-home (expand-file-name "~/anaconda/")
    conda-env-home-directory (expand-file-name "~/anaconda/") ;; as in previous example; not required
    conda-env-subdirectory "envs")

(global-set-key (kbd "C-. l") 'lsp) ;; lsp
;; (setq lsp-ui-doc-enable nil)
;; (setq lsp-ui-sideline-enable nil)
;; (require 'lsp-mode)
(add-hook 'web-mode-hook #'lsp)

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
;; (load "auctex.el" nil t t)
;; (load "preview-latex.el" nil t t)
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)


;; org-mode and org-reveal
(add-to-list 'load-path "~/.emacs.d/elisp/org-mode/lisp")
(require 'org-compat)
(eval-after-load "org"
	'(progn
		 (require 'ox-md nil t)
		 ;; (require 'ox-reveal nil t)
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
(global-set-key (kbd "C-. s") 'ido-occur)
(global-set-key (kbd "C-. C-c") 'comment-or-uncomment-region)
(global-set-key (kbd "C-. C-.") 'global-linum-mode) ;; number of line
;; (global-set-key (kbd "C-. C-i") 'irc)

;; scroll by one
(global-set-key (kbd "M-n") (lambda () (interactive) (scroll-up 1)))
(global-set-key (kbd "M-p") (lambda () (interactive) (scroll-down 1)))
;; respect scroll in markdown mode
(eval-after-load "markdown-mode"
	'(define-key markdown-mode-map (kbd "M-n") nil))
(eval-after-load "markdown-mode"
	'(define-key markdown-mode-map (kbd "M-p") nil))



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
;; (epa-file-enable)

(global-set-key (kbd "C-. C-m") 'idomenu)
;; navigation
(global-set-key (kbd "C-. C-p") 'beginning-of-defun)
(global-set-key (kbd "C-. C-n") 'end-of-defun)
;; hs for navigation
(defun hs-and-shortcuts ()
	"Custom function to hide and show blocks"
    ;; (local-set-key (kbd "C-. C-s") 'hs-toggle-hiding)
    (local-set-key (kbd "C-. C-h") 'hs-hide-block)
    (local-set-key (kbd "C-. C-s") 'hs-show-block)
    (local-set-key (kbd "C-. C-l") 'hs-hide-level)
    (local-set-key (kbd "C-. x C-h") 'hs-hide-all)
    (local-set-key (kbd "C-. x C-s") 'hs-show-all)
    (hs-minor-mode t)
    (hide/show-comments)
    (local-set-key (kbd "C-. C-g") 'hide/show-comments-toggle)
    )
(add-hook 'c-mode-common-hook 'hs-and-shortcuts)

(defun hide-body-recenter ()
    (interactive)
    (hide-body)
    (recenter))
(defun hs-python-and-shortcuts ()
    ;; (setq outline-regexp "[[:space:]]*\\_<\\(?:def\\|class\\|if\\|elif\\|else\\|try\\|except\\|finally\\|for\\|while\\|with\\|async[[:space:]]+\\(?:def\\|for\\|with\\)\\)\\_>")
    (setq outline-regexp "\\s-*\\_<\\(?:def\\|class\\)\\_>")
    (outline-minor-mode t)
    (require 'hide-python-docstrings)

    ;; (local-set-key (kbd "C-. h") 'outline-cycle) ;; not working well
    (local-set-key (kbd "C-. C-s") 'show-entry) ;; not working well
    (local-set-key (kbd "C-. C-h") 'hide-entry) ;; not working well
    (local-set-key (kbd "C-. C-a") 'show-all)  ;; not necessary because it's in global already
    (local-set-key (kbd "C-. C-l") 'hide-body-recenter)
    (local-set-key (kbd "C-. C-r") 'hide-python-docstrings) ;; not working well
    (local-set-key (kbd "C-. C-d") 'show-python-docstrings) ;; not working well
    ;; (require 'my-pythonic-activate)
    ;; (local-set-key (kbd "C-. .") 'my-pythonic-activate)
    (setq python-indent 4)
    (setq yas-indent-line 'auto)
    ;; (set (make-local-variable 'yas-indent-line) 'fixed)
    )
(add-hook 'python-mode-hook 'hs-python-and-shortcuts)

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
;; (add-to-list 'load-path "~/.emacs.d/elisp/auto-complete/lib/popup")

;; iedit
;; (require 'iedit)
(global-set-key (kbd "C-. C-e") 'iedit-mode)


;; flycheck
;; (require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)
(global-set-key (kbd "C-. f") 'flycheck-mode)

;; (custom-set-variables
;; 	'(flycheck-c/c++-googlelint-executable "cpplint")
;; )
(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))
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



;; (require 'irony)
(defun irony-hooks ()
  "Enable the hooks in the preferred order: 'yas -> auto-complete -> irony'."
  ;; if yas is not set before (auto-complete-mode 1), overlays may persist after
  ;; an expansion.
  ;; (yas/minor-mode-on)
  ;; (auto-complete-mode 1)
  (when (member major-mode irony-supported-major-modes)
    (irony-mode 1)))

(add-hook 'c++-mode-hook 'irony-hooks)
(add-hook 'c-mode-hook 'irony-hooks)
;; (add-hook 'objc-mode-hook 'irony-mode)

(add-hook 'after-init-hook 'global-company-mode)

(require 'company)

;; (require 'company-irony)
;; (require 'company-inf-python)
;; (require 'company-irony-c-headers)
;; (add-hook 'python-mode-hook 'anaconda-mode)
;; (add-hook 'python-mode-hook 'anaconda-eldoc-mode)
;; (eval-after-load 'company '(progn
;;            (add-to-list 'company-backends 'company-anaconda)
;;            ;; (add-to-list 'company-backends 'company-irony)
;;            ;; (add-to-list 'company-backends '(company-irony-c-headers company-irony))
;;                                ))
(setq-default c-basic-offset 2)
(setq irony-additional-clang-options '("-std=c++11"))



(setq company-tooltip-limit 20)                      ; bigger popup window
;; (setq company-idle-delay .3)                         ; decrease delay before autocompletion popup shows
(setq company-echo-delay 0)                          ; remove annoying blinking
(setq company-begin-commands '(self-insert-command)) ; start autocompletion only after typing
(setq company-minimum-prefix-length 1
      company-idle-delay 0.1)

;; (optional) adds CC special commands to `company-begin-commands' in order to
;; trigger completion at interesting places, such as after scope operator
;;     std::|
(add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)



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
(setq ido-default-buffer-method 'selected-window)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(require 'ido)
(ido-mode t)
(ido-vertical-mode 1)
(setq ido-vertical-define-keys 'C-n-and-C-p-only)
(setq ffip-prefer-ido-mode t)
(global-set-key (kbd "C-x C-g") 'find-file-in-project)

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
;; (global-set-key (kbd "C-x C-g") 'sudo-edit)
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
;; (require 'android-mode)
(global-set-key (kbd "C-<f12>") 'android-ant-debug)

(setq android-mode-sdk-dir "~/programming/android/")
(add-hook 'gud-mode-hook
	(lambda ()
		(add-to-list 'gud-jdb-classpath "~/android-sdk/platforms/android-17/android.jar")
		))


;; etags
;; (defun find-file-upwards (file-to-find)
;; 	"Recursively searches each parent directory starting from the default-directory.
;; looking for a file with name file-to-find.  Returns the path to it
;; or nil if not found."
;; 	(labels
;; 		((find-file-r (path)
;; 			 (let* ((parent (file-name-directory path))
;; 					   (possible-file (concat parent file-to-find)))
;; 				 (cond
;; 					 ((file-exists-p possible-file) possible-file) ; Found
;; 					 ;; The parent of ~ is nil and the parent of / is itself.
;; 					 ;; Thus the terminating condition for not finding the file
;; 					 ;; accounts for both.
;; 					 ((or (null parent) (equal parent (directory-file-name parent))) nil) ; Not found
;; 					 (t (find-file-r (directory-file-name parent))))))) ; Continue
;; 		(find-file-r default-directory)))
;; (let ((my-tags-file (find-file-upwards "TAGS")))
;; 	(when my-tags-file
;; 		(message "Loading tags file: %s" my-tags-file)
;; 		(visit-tags-table my-tags-file)))

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
(global-set-key (kbd "C-. v") 'visual-line-mode)
(add-hook 'org-mode-hook 'visual-line-mode)
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
(setq org-startup-folded t) 
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)   ; with AUCTeX LaTeX mode
(add-hook 'latex-mode-hook 'turn-on-reftex)   ; with Emacs latex mode


(setq org-latex-caption-above nil)  ; Table now has the caption below

;; (require 'org-latex)
(unless (boundp 'org-latex-classes)
	(setq org-latex-classes nil))

;; if X11 or terminal
(if (or (eq window-system 'x) (eq window-system 'w32))
    (progn
		;; color theme
		(require 'color-theme)
		(require 'color-theme-mycomidia)
        ;; (add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
        ;; (load-theme `wilmersdorf t)
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
	(progn
		(setq mumamo-background-colors nil)
		)
	)


;;(setq TeX-PDF-mode t)
(setq preview-image-type 'png)

;; default files
(add-to-list 'auto-mode-alist '("CMakeLists.txt" . cmake-mode)) ;;cmake
(add-to-list 'auto-mode-alist '(".env" . conf-mode)) ;;cmake

;;ROS
(add-to-list 'auto-mode-alist '("\\.launch$" . nxml-mode)) ;;cakephp

;;octave m files
;; (autoload 'octave-mode "octave-mod" nil t) ;; ATTENION: octave-mod without the E is alright
(setq auto-mode-alist (cons '("\\.m$" . octave-mode) auto-mode-alist))
(setq-default octave-comment-start "% ")

;; opencl files with c-mode syntax
(setq auto-mode-alist (cons '("\\.cl$" . c-mode) auto-mode-alist))

(require 'ess-site)


;; greek letters
(load "~/.emacs.d/julia_abbrevs.el")
(setq save-abbrevs nil)
(add-hook 'julia-mode-hook 'abbrev-mode)

(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.ctp?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.php?\\'" . php-mode))
(add-to-list 'auto-mode-alist '("\\.blade\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tmpl?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.vue?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.astro?\\'" . web-mode))

(setq web-mode-engines-alist
      '(("astro"    . "\\.astro\\'")
        )
)

(setq web-mode-enable-engine-detection t)
(setq web-mode-enable-auto-indentation nil)

(defun my-web-mode-hook ()
  "Hooks for Web mode."
    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-css-indent-offset 2)
    (setq web-mode-code-indent-offset 2)
    (setq web-mode-indent-style 2)
    )
(add-hook 'web-mode-hook  'my-web-mode-hook)
(defun yas-web-mode-fix ()
  (web-mode-buffer-refresh)
  (indent-for-tab-command))
(setq yas/after-exit-snippet-hook 'yas-web-mode-fix)

(add-to-list 'auto-mode-alist '("\\.mzn?\\'" . minizinc-mode))
(add-to-list 'auto-mode-alist '("\\.dzn?\\'" . minizinc-mode))


;; multicursor mode
(global-set-key (kbd "C-c C-,") 'mc/mark-next-like-this)
(global-set-key (kbd "C-c C-.") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c a") 'mc/mark-all-like-this)
(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
(insert (current-kill 0)))))

(global-set-key (kbd "C-c e") 'eval-and-replace)

(add-hook 'mmm-mode-hook
          (lambda ()
            (set-face-background 'mmm-default-submode-face nil)))

(push
 (cons
  "docker"
  '((tramp-login-program "docker")
    (tramp-login-args (("exec" "-it") ("%h") ("/bin/bash")))
    (tramp-remote-shell "/bin/sh")
    (tramp-remote-shell-args ("-i") ("-c"))))
 tramp-methods)

(defadvice tramp-completion-handle-file-name-all-completions
  (around dotemacs-completion-docker activate)
  "(tramp-completion-handle-file-name-all-completions \"\" \"/docker:\" returns
    a list of active Docker container names, followed by colons."
  (if (equal (ad-get-arg 1) "/docker:")
      (let* ((dockernames-raw (shell-command-to-string "docker ps | perl -we 'use strict; $_ = <>; m/^(.*)NAMES/ or die; my $offset = length($1); while(<>) {substr($_, 0, $offset, q()); chomp; for(split m/\\W+/) {print qq($_:\n)} }'"))
             (dockernames (cl-remove-if-not
                           #'(lambda (dockerline) (string-match ":$" dockerline))
                           (split-string dockernames-raw "\n"))))
        (setq ad-return-value dockernames))
    ad-do-it))

(when (featurep 'project)
  (when-let ((project (project-current)))
    (with-no-warnings
      (if (fboundp 'project-roots)
          (car (project-roots project))
        (project-root project)))))

(setq gofmt-command "goimports")
(add-hook 'before-save-hook 'gofmt-before-save)
(setq markdown-use-pandoc-style-yaml-metadata t)
;; (setq lsp-julia-package-dir nil)
;; (setq lsp-julia-flags `("-J/home/fogg/.julia/compiled/v1.9/LanguageServer/ite7n_spZi4.so"))
;; (setq lsp-julia-flags `("-J/home/fogg/.julia/compiled/v1.9/LanguageServer/languageserver.so"))
;; (quelpa '(lsp-julia :fetcher github
;;                     :repo "gdkrmr/lsp-julia"
;;                     :files (:defaults "languageserver")))
;; (require 'use-package)
;; (use-package lsp-julia
;;   :config
;;   (setq lsp-julia-default-environment "~/.julia/environments/v1.9"))
;; (require 'lsp-julia)
;; (add-hook 'ess-julia-mode-hook #'lsp-mode)
(use-package lsp-mode
    :commands lsp
    :ensure t
    :diminish lsp-mode
    :hook
    (elixir-mode . lsp)
    :init
    (add-to-list 'exec-path "~/.emacs.d/elixir-ls"))

(setq warning-minimum-level :emergency)

(require 'org-download)
(add-hook 'dired-mode-hook 'org-download-enable)
(setq org-startup-with-inline-images t)

(global-set-key (kbd "C-, g") 'gptel-send)
(global-set-key (kbd "C-, C-g") (lambda () (interactive) (gptel-send '(4 gptel-send))))
(setq gptel-model "gpt-4o-mini")
(setq gptel-directives
    '((default . "You are a large language and a helpful assistant. Respond concisely.")
         (programming . "You are a large language model and a careful programmer. Provide code and only code as output without any additional text, prompt, note or ```.")
         (writing . "You are a large language model and a writing assistant. Respond concisely.")
         (chat . "You are a large language model and a conversation partner. Respond concisely.")))


(setq epa-pinentry-mode 'loopback)


(if (>= emacs-major-version 27)
    (set-fontset-font t '(#x1f000 . #x1faff)
              (font-spec :family "Noto Color Emoji")))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
      '(org-download astro-ts-mode auctex company company-c-headers company-math company-reftex gnu-elpa-keyring-update eldoc elixir-mode use-package lsp-julia quelpa scss-mode cmake-mode zotelo yasnippet-snippets web-mode typescript-mode sass-mode protobuf-mode project outline-magic nginx-mode mutt-mode minizinc-mode lsp-mode lean-mode julia-mode json-mode js2-refactor ivy iedit ido-vertical-mode ido-occur helm-bibtex haskell-mode gitignore-mode find-file-in-project exec-path-from-shell ess elm-mode dockerfile-mode dash-functional cpputils-cmake conda company-anaconda)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
