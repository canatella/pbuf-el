;;; pbuf-test.el -- pbuf test suite -*- lexical-binding: t; -*-

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

;; Test suite for xwidget-webkit-plus feature

;;; Code:

(require 'pbuf)

(ert-deftest test-pbuf-insert-filter-output ()
    (let ((pbuf-max-line-count 2))
      (with-temp-buffer
        (pbuf-insert-filter-output "foo\nbar\n")
        (should (equal "foo\nbar\n" (buffer-string)))
        (should (equal 2 pbuf-line-count))
        (pbuf-truncate)
        (should (equal "foo\nbar\n" (buffer-string)))
        (should (equal 2 pbuf-line-count)))
      (with-temp-buffer
        (pbuf-insert-filter-output "foo\nb")
        (should (equal "foo\nb" (buffer-string)))
        (should (equal 1 pbuf-line-count))
        (pbuf-truncate)
        (should (equal "foo\nb" (buffer-string)))
        (should (equal 1 pbuf-line-count)))
      (with-temp-buffer
        (pbuf-insert-filter-output "1\n2\n3\n4\n")
        (should (equal "1\n2\n3\n4\n" (buffer-string)))
        (should (equal 4 pbuf-line-count))
        (pbuf-truncate)
        (should (equal "3\n4\n" (buffer-string)))
        (should (equal 2 pbuf-line-count)))
      (with-temp-buffer
        (pbuf-insert-filter-output "1\n2\n3\n4")
        (should (equal "1\n2\n3\n4" (buffer-string)))
        (should (equal 3 pbuf-line-count))
        (pbuf-truncate)
        (should (equal "2\n3\n4" (buffer-string)))
        (should (equal 2 pbuf-line-count)))
      ))

(ert-deftest test-pbuf-maybe-prepend-last ()
  (with-temp-buffer
    (insert "foo")
    (should (equal "foobar" (pbuf-maybe-prepend-last "bar")))
    (should (equal "" (buffer-string))))
  (with-temp-buffer
    (insert "foo\n")
    (should (equal "bar" (pbuf-maybe-prepend-last "bar")))
    (should (equal "foo\n" (buffer-string))))
  (with-temp-buffer
    (insert "previous\nfoo")
    (should (equal "foobar" (pbuf-maybe-prepend-last "bar")))
    (should (equal "previous\n" (buffer-string))))
  (with-temp-buffer
    (insert "previous\nfoo\n")
    (should (equal "bar" (pbuf-maybe-prepend-last "bar")))
    (should (equal "previous\nfoo\n" (buffer-string)))))

(ert-deftest test-pbuf-insert ()
      (let ((pbuf-max-line-count 2))
        (with-temp-buffer
          (pbuf-insert "foo\nbar\nbaz\nban\n")
          (should (equal "baz\nban\n" (buffer-string)))
          (pbuf-insert "not complete")
          (should (equal "baz\nban\nnot complete" (buffer-string)))
          (pbuf-insert "/still not")
          (should (equal "baz\nban\nnot complete/still not" (buffer-string)))
          (pbuf-insert "/completed\n")
          (should (equal "ban\nnot complete/still not/completed\n" (buffer-string)))
          )))

(ert-deftest test-pbuf-filter-output ()
  (let ((functions '((lambda (line) (should (equal "0" line)) "1")
                     (lambda (line) (should (equal "1" line)) "2")
                     (lambda (line) (should (equal "2" line)) "3")
                     (lambda (line) (should (equal "3" line)) "4"))))
    (should (equal "4" (pbuf-filter-output "0" functions))))
  (let ((functions '((lambda (line) (should (equal "0" line)) "1")
                     (lambda (line) (should (equal "1" line)) nil)
                     (lambda (line) (should nil) "3"))))
    (should (equal nil (pbuf-filter-output "0" functions)))))

(provide 'pbuf-test)
;;; pbuf-test.el ends here
