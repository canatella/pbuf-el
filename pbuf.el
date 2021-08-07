;;; pbuf.el --- Process buffer minor mode -*- lexical-binding: t; -*-

;; Author: Damien Merenne
;; URL: https://github.com/canatella/pbuf
;; Created: 2020-03-11
;; Keywords: convenience
;; Version: 0.1
;; Package-Requires: ((emacs "26.1"))

;; Copyright (C) 2020 Damien Merenne <dam@cosinux.org>

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides a minor mode for displaying live process output. It
;; allows processing output line by line and removing old lines.

;;; Code:
(require 'subr-x)

(defvar-local pbuf-start-process-function '() "A function that returns the command use to start the process.")
(defvar-local pbuf-process-name '() "The name of the process.")
(defvar-local pbuf-process '() "The process associated to this buffer.")
(defvar-local pbuf-line-count 0 "The line count in the current buffer.")
(defvar pbuf-max-line-count 2000 "The maximum number of lines in the buffer.")
(defvar pbuf-pre-insert-functions '() "Abnormal hook to run before inserting a line. Called with the line as argument, it should return a string.")

(defun pbuf-filter-output (line functions)
  "Filter LINE using FUNCTION.S"
  (if functions
      (let ((filtered (funcall (car functions) line)))
        (if (and filtered (not (string-empty-p filtered)))
          (pbuf-filter-output filtered (cdr functions))))
    line))

(defun pbuf-insert-filter-output (string)
  "Process output STRING."
  (let* ((lines (reverse (split-string string "\n")))
         (last (car lines)))
    (setq lines (reverse (cdr lines)))
    (dolist (line lines)
      (setq line (pbuf-filter-output line pbuf-pre-insert-functions))
      (when line
        (setq pbuf-line-count (+ 1 pbuf-line-count))
        (insert line "\n")))
    (insert last)))

(defun pbuf-truncate ()
  "Truncate buffer so that it contains the right number of lines."
  (save-excursion
    (when (> pbuf-line-count pbuf-max-line-count)
      (goto-char (point-min))
      (forward-line (- pbuf-line-count pbuf-max-line-count))
      (delete-region (point-min) (point))
      (setq pbuf-line-count pbuf-max-line-count))))

(defun pbuf-maybe-prepend-last (string)
  "If the last line is not complete, remove it and prepend it to STRING."
  (when (not (eq (char-before) ?\n))
    (beginning-of-line)
    (setq string (concat (buffer-substring (point) (point-max)) string))
    (delete-region (point) (point-max)))
  string)

(defun pbuf-insert (string)
  "Insert process buffer output string."
  (pbuf-insert-filter-output (pbuf-maybe-prepend-last string))
  (pbuf-truncate))

(defun pbuf-process-filter (proc string)
  "Remove old lines in PROC buffer, runs `pbuf-pre-insert-functions' with STRING and insert the result."
  (with-current-buffer (process-buffer proc)
    (let ((moving (= (point) (process-mark proc)))
          (inhibit-read-only t))
      (save-excursion
        (goto-char (process-mark proc))
        (pbuf-insert string)
        (set-marker (process-mark proc) (point)))
      (if moving (goto-char (process-mark proc))))))

(defun pbuf-do-start-process (&rest _)
  "Start process stored in `pbuf-process-start-command'"
  (let ((inhibit-read-only t))
    (delete-region (point-min) (point-max)))
  (when (and pbuf-process (process-live-p pbuf-process)) (kill-process pbuf-process))
  (setq pbuf-line-count 0
        pbuf-process (funcall pbuf-start-process-function pbuf-process-name (current-buffer)))
  (set-process-filter pbuf-process #'pbuf-process-filter)
  pbuf-process)

(defun pbuf-start-process (name function)
  "Run process NAME in a pbuf buffer using the process returning FUNCTION.

The function will be called with the name and the process buffer as arguments."
  (with-current-buffer (get-buffer-create name)
    (make-local-variable 'pbuf-max-line-count)
    (make-local-variable 'pbuf-pre-insert-functions)
    (make-local-variable 'revert-buffer-function)
    (setq pbuf-start-process-function function
          pbuf-process-name name
          revert-buffer-function #'pbuf-do-start-process
          buffer-read-only t)
    (pbuf-minor-mode t)
    (pbuf-do-start-process)))

(defun pbuf-start-process-shell-command (name command)
  "Run process NAME using shell COMMAND."
  (pbuf-start-process name (lambda (name buffer) (start-process-shell-command name buffer command))))

(defvar pbuf-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") #'revert-buffer)
    (define-key map (kbd "q") #'bury-buffer)
    (define-key map (kbd "<") #'beginning-of-buffer)
    (define-key map (kbd ">") #'end-of-buffer)
    map)
  "Keymap for command `pbuf-minor-mode'.")

(define-minor-mode pbuf-minor-mode
  "Minor mode for pbuf buffers."
  :group 'bloom
  :init-value nil
  :keymap pbuf-minor-mode-map)




(provide 'pbuf)

;;; pbuf.el ends here
