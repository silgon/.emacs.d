(defgroup hide-python-docstrings nil
    "Commands for hiding docstrings")

(defvar-local hide-lines-invisible-areas ()
    "List of invisible overlays used by hidelines")

(defun hide-lines-invisible-p (start end)
    "Return non-nil if region between START and END is already hidden."
    (remove nil (mapcar (lambda (ov)
			                (and (<= (overlay-start ov) start)
			                    (>= (overlay-end ov) end)))
                    hide-lines-invisible-areas)))


(defun hide-lines-add-overlay (start end)
    "Add an overlay from `START' to `END' in the current buffer.
Push the overlay onto the `hide-lines-invisible-areas' list"
    (unless (hide-lines-invisible-p start end)
        (let ((overlay (make-overlay start end)))
            (setq hide-lines-invisible-areas (cons overlay hide-lines-invisible-areas))
            (unless (member 'hl buffer-invisibility-spec)
	            (add-to-invisibility-spec 'hl))
	            ;; (add-to-invisibility-spec '(hl . t)))
            (overlay-put overlay 'invisible 'hl))))


(defun hide-blocks-matching (start-text end-text)
    "Hide text that is between lines matching START-TEXT and END-TEXT."
    (set (make-local-variable 'line-move-ignore-invisible) t)
    (save-excursion
        (goto-char (point-min))
        (let ((pos (re-search-forward start-text nil t))
                 start-position)
            (while pos
                (beginning-of-line)
                (setq start-position (point))
	            (forward-line 1)
	            (if (re-search-forward end-text nil t)
	                (beginning-of-line)
	                (goto-char (point-max)))
                (hide-lines-add-overlay start-position (point))
                (if (eq (point) (point-max))
                    (setq pos nil)
                    (setq pos (re-search-forward start-text nil t)))))))

(defun hide-python-docstrings ()
    (interactive)
    (hide-blocks-matching
        "\\:[^-_A-Za-z0-9]*\\(\\\"\\\"\\\"\\)"
        "\\\"\\\"\\\""))

(defun show-python-docstrings ()
    "Unhide all hidden areas."
    (interactive)
    (mapc (lambda (overlay) (delete-overlay overlay)) 
        hide-lines-invisible-areas)
    (setq hide-lines-invisible-areas ())
    (remove-from-invisibility-spec 'hl))

(provide 'hide-python-docstrings)

;; (define-minor-mode hide-python-docstrings-mode
;;   "Get your foos in the right places."
;;   :lighter " hide-python-docstrings"
;;   :keymap (let ((map (make-sparse-keymap)))
;;             (define-key map (kbd "C-c f") 'insert-foo)
;;             map))

;;;###autoload
;; (add-hook 'text-mode-hook 'hide-python-docstrings-mode)

;; (provide 'hide-python-docstrings-mode)
