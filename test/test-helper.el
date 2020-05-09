;;; test-helper.el -- Test helpers for pbuf

;; Copyright (C) 2020 Damien Merenne <dam@cosinux.org>

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(when (> emacs-major-version 26)
  (require 'debug)
  (defun ert--print-backtrace (frames)
    (unless frames (setq frames (backtrace-get-frames 'backtrace-to-string)))
    (let ((backtrace-fontify nil))
    (with-temp-buffer
      (backtrace-mode)
      (setq backtrace-view '(:show-flags t)
            backtrace-frames frames
            backtrace-print-function #'cl-prin1)
      (backtrace-print)
      (ert-runner-message "%s" (substring-no-properties (filter-buffer-substring (point-min)
                                                                                 (point-max))))))))


(defconst pbuf-test-path (file-name-as-directory
                             (file-name-directory (or load-file-name buffer-file-name)))
  "The test directory.")

(defconst pbuf-test-data-path (file-name-as-directory
                                       (concat pbuf-test-path "data"))
  "The test data directory.")

(defconst pbuf-root-path (file-name-as-directory
                                         (file-name-directory
                                          (directory-file-name pbuf-test-path)))
  "The package root path.")

(add-to-list 'load-path pbuf-root-path)

;;; test-helper.el ends here
