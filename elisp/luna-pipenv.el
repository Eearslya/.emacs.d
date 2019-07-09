;;; luna-pipenv.el --- Homemade support for Pipenv within Python projects

;; Author: Eearslya Sleiarion <eearslya@sleiarion.net>
;; Keywords: python pipenv pyvenv
;; Version: 1.0.0

;;; Commentary:
;; This file defines several functions designed to allow Emacs to automatically
;; detect and load a Python virtual environment created by Pipenv.
;; There are also additional hooks to allow compatibility with Flycheck.

;;; Code:

(require 'flycheck)
(require 'projectile)

(defun luna|pipenv-find-venv (dir)
  "Attempt to find the venv for a project in DIR."
  (when dir
    (let* ((process-environment initial-environment)
           (default-directory dir)
           (output-pipe (if (eq system-type 'windows-nt) "2> NUL" "2> /dev/null"))
           (candidate
            (replace-regexp-in-string "\n\\'" ""
                                      (shell-command-to-string
                                       (concat "pipenv --venv " output-pipe)))))
      (unless (string-empty-p candidate)
        candidate))))

(defun luna|pipenv-find-for-buffer (buffer-or-name)
  "Attempt to find the venv for a project in BUFFER-OR-NAME.
The function will search the directory of the given buffer,
as well as the root directory of the Projectile project, if
it exists.
If neither directory can be found to have a venv, it will
return a default venv in the user's home directory."
  (interactive "The buffer to find out a virtualenv for: ")
  (let* ((default-venv (concat (file-name-as-directory (getenv "HOME")) "venv"))
         (buffer (get-buffer buffer-or-name))
         (buf-file-name (buffer-file-name buffer))
         (pipenv-file-dir
          (let ((file-dir (when buf-file-name (file-name-directory buf-file-name))))
            (luna|pipenv-find-venv file-dir)))
         (pipenv-projectile-root (when (projectile-project-p)
                                   (luna|pipenv-find-venv (projectile-project-root))))
         (target-venv (or pipenv-file-dir pipenv-projectile-root default-venv)))
    target-venv))

(defun luna|pipenv-project-p ()
  "Return whether or not the directory we're in is a Pipenv project."
  (locate-dominating-file default-directory "Pipfile"))

(defun luna|pipenv-flycheck ()
  "Reset Python Flycheck checkers after venv is activated.
This is necessary if the checker depends on a python library
that is only installed after the venv is activated."
  (let ((python-checkers)
        (checkers (flycheck-defined-checkers 'modes)))
    ;; Build up a list of Flycheck checkers that apply to python-mode.
    (while checkers
      (let ((checker (car checkers)))
        (when (memq 'python-mode (flycheck-checker-get checker 'modes))
          (push checker python-checkers))
        (setq checkers (cdr checkers))))
    ;; For each open buffer in that project, reset the 'enabled' flag
    ;; in Flycheck, so it is forced to re-evaluate. This allows Flycheck
    ;; to re-check for any Python libraries that may have been installed
    ;; via the virtual environment.
    (dolist (buf (projectile-project-buffers))
      (with-current-buffer buf
        (when (eq major-mode 'python-mode)
          (dolist (checker python-checkers)
            (flycheck-reset-enabled-checker checker)))))))

(defun luna|pipenv-mode ()
  "Activate Pipenv upon Projectile project switch."
  (interactive)
  ;; Deactivate Pyvenv first to ensure we start with a clean slate.
  (pyvenv-deactivate)
  (when (luna|pipenv-project-p)
    (let ((venv (luna|pipenv-find-for-buffer (current-buffer))))
      (pyvenv-activate venv)
      (luna|pipenv-flycheck))))

(provide 'luna-pipenv)
;;; luna-pipenv.el ends here
