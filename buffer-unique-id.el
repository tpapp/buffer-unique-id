;;; buffer-unique-id.el --- Generate strings that are unique in an Emacs buffer.  -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Tamas Papp

;; Author: Tamas Papp <tkpapp@gmail.com>
;; Keywords: convenience, outlines
;; Homepage: https://github.com/tpapp/buffer-unique-id

;; This program is free software; you can redistribute it and/or modify
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

;; Generate strings that are unique in an Emacs buffer. The structure
;; of the string is the following: start with BUFFER-UNIQUE-ID-START,
;; then BUFFER-UNIQUE-ID-LENGTH characters chosen randomly from
;; the string BUFFER-UNIQUE-ID-CHARSET.
;;
;; The unique id is generated randomly, retrying if it is already
;; present in the buffer. This is not a sophisticated algorithm and
;; may fail or be inefficient in corner cases, but should suffice for
;; the most common use case.
;;
;; The recommended user interface is INSERT-BUFFER-UNIQUE-ID. When
;; BUFFER-UNIQUE-ID-REGISTER is not NIL, the generated unique id is
;; saved there (overwriting the previous content without asking).

;;; Code:

(defun buffer-unique-id (start length charset)
  "Generate string that starts with START, and is followed by LENGTH random characters from CHARSET. It is ensured that the resulting string is unique in the buffer."
  (interactive)
  (let* ((n (length charset))
         (max-tries (* 5 (min (expt n length)) (buffer-size)))) ; heuristic
    (cl-flet ((gen-id ()
                      (concat start
                              (cl-loop
                               repeat length
                               collect (aref charset (random n)))))
              (id-in-buffer? (id)
                             (save-excursion
                               (save-match-data
                                 (goto-char (point-min))
                                 (search-forward id nil t)))))
      (loop
       repeat max-tries
       for id = (gen-id)
       when (not (id-in-buffer? id))
       return id
       finally (error "Could not generate a unique ID after %d tries, giving up"
                      max-tries)))))

(defvar-local buffer-unique-id-start "x"
  "Buffer unique IDs start with this string.")

(defvar-local buffer-unique-id-length 4
  "Number of characters for buffer unique IDs following BUFFER-UNIQUE-ID-START.")

(defvar-local buffer-unique-id-charset "0123456789abcdef"
  "Character set for buffer unique IDs. Each character is chosen randomly.")

(defvar-local buffer-unique-id-register nil
  "Register for saving the last buffer unique ID. Only used when not NIL.")

(defun insert-buffer-unique-id ()
  "Insert a unique ID, and save it in the register named by BUFFER-UNIQUE-ID.

See BUFFER-UNIQUE-ID-START, BUFFER-UNIQUE-ID-LENGTH, and BUFFER-UNIQUE-ID-CHARSET."
  (interactive)
  (let ((id (buffer-unique-id buffer-unique-id-start buffer-unique-id-length
                              buffer-unique-id-charset)))
    (when buffer-unique-id-register
      (set-register buffer-unique-id-register id))
    (insert id)))

(provide 'buffer-unique-id)
;;; buffer-unique-id.el ends here
