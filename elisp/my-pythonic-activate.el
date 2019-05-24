(defun my-pythonic-activate (virtualenv)
  "Activate python VIRTUALENV."
  (interactive "DEnv: ")
    (progn
        (setq flycheck-python-pycompile-executable (concat virtualenv "bin/python"))
        (setq flycheck-python-flake8-executable (concat virtualenv "bin/flake8"))
        (setq flycheck-python-pylint-executable (concat virtualenv "bin/pylint"))
        (setq python-shell-virtualenv-root (pythonic-python-readable-file-name virtualenv))
        (flycheck-mode -1)
        (flycheck-mode t)
        ))

(provide 'my-pythonic-activate)
;;; my-pythonic-activate.el ends here

