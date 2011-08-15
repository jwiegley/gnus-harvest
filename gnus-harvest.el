;;; gnus-harvest --- Harvest e-mail address from read/written articles

;; Copyright (C) 2011 John Wiegley

;; Author: John Wiegley <johnw@gnu.org>
;; Created: 15 Aug 2011
;; Version: 1.0
;; Keywords: gnus email
;; X-URL: https://github.com/jwiegley/gnus-harvest

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This code requires that SQLite3 be installed.  Check to see if the command
;; "sqlite3" is already available on your system.
;;
;; Once you have that, add this to your .emacs:
;;
;;   (eval-after-load "gnus"
;;     '(progn (require 'gnus-harvest)
;;             (gnus-harvest-install)))
;;
;; If you use message-x and ido, you can get TAB completion of harvested
;; address in your To:, Cc: and From: fields by using this instead of the
;; above:
;;
;;   (eval-after-load "gnus"
;;     '(progn (require 'gnus-harvest)
;;             (gnus-harvest-install 'message-x)))
;;

(require 'gnus)

(defgroup gnus-harvest nil
  ""
  :group 'gnus)

(defcustom gnus-harvest-sqlite-program (executable-find "sqlite3")
  ""
  :type 'file
  :group 'gnus-harvest)

(defcustom gnus-harvest-db-path (expand-file-name ".addrs" gnus-home-directory)
  ""
  :type 'file
  :group 'gnus-harvest)

(defcustom gnus-harvest-query-limit 50
  ""
  :type 'integer
  :group 'gnus-harvest)

(defcustom gnus-harvest-ignore-email-regexp "@public.gmane.org"
  "A regexps which, if an email matches, that email is ignored."
  :type 'string
  :group 'gnus-harvest)

(defun gnus-harvest-sqlite-invoke (sql &optional ignore-output-p)
  (let ((tmp-buf (and (not ignore-output-p)
                      (generate-new-buffer "*sqlite*"))))
    (if sql
        (call-process gnus-harvest-sqlite-program
                      nil tmp-buf nil "-noheader" "-list"
                      gnus-harvest-db-path sql)
      (call-process-region (point-min) (point-max)
                           gnus-harvest-sqlite-program
                           nil tmp-buf nil "-noheader" "-list"
                           gnus-harvest-db-path))
    (unless ignore-output-p
      (with-current-buffer tmp-buf
        (prog1
            (buffer-string)
          (kill-buffer (current-buffer)))))))

(defun gnus-harvest-create-db ()
  (gnus-harvest-sqlite-invoke "
CREATE TABLE
    addrs
    (
        email TEXT(255) NOT NULL,
        fullname TEXT(255),
        last_seen INTEGER NOT NULL,
        PRIMARY KEY (email),
        UNIQUE (email)
    )
" t))

(defun gnus-harvest-complete-stub (stub &optional prefix-only-p)
  (read (concat "("
                (gnus-harvest-sqlite-invoke
                 (format "
SELECT
    '\"' ||
    CASE
        WHEN fullname IS NOT NULL
        THEN fullname || ' <' || email || '>'
        ELSE email
    END
    || '\"'
FROM
    (
        SELECT
            email, fullname, last_seen
        FROM
            addrs
        WHERE
            (email LIKE '%s%s%%' OR fullname LIKE '%s%s%%')
        ORDER BY
            last_seen DESC
        LIMIT
            %d
    )"
                         (if prefix-only-p "" "%") stub
                         (if prefix-only-p "" "%") stub
                         gnus-harvest-query-limit))
                ")")))

(defun gnus-harvest-insert-address (email fullname moment)
  (insert "INSERT OR REPLACE INTO addrs (email, ")
  (if fullname
      (insert "fullname, "))
  (insert "last_seen) VALUES (lower('" email "'), '")
  (if fullname
      (insert fullname "', '"))
  (insert moment "');\n"))

(defun gnus-harvest-addresses ()
  "Harvest and remember the addresses in the current article buffer."
  (let ((tmp-buf (generate-new-buffer "*gnus harvest*"))
        (moment (number-to-string (floor (float-time)))))
    (mapc (lambda (info)
            (with-current-buffer tmp-buf
              (gnus-harvest-insert-address (cadr info) (car info) moment)))
          (delete
           nil
           (mapcar (lambda (info)
                     (and (not (string-match gnus-harvest-ignore-email-regexp
                                             (cadr info)))
                          info))
                   (append
                    (mapcar (lambda (field)
                              (let ((value (message-field-value field t)))
                                (and value
                                     (mail-extract-address-components value))))
                            '("to" "reply-to" "from" "resent-from" "cc" "bcc"))))))
    (with-current-buffer tmp-buf
      (gnus-harvest-sqlite-invoke nil t)
      (kill-buffer (current-buffer)))))

(defun gnus-harvest-find-address ()
  (interactive)
  (backward-kill-word 1)
  (insert
   (let ((stub (word-at-point)))
     (ido-completing-read "Use address: "
                          (gnus-harvest-complete-stub stub)
                          nil t stub))))

(defun gnus-harvest-install (&rest features)
  (unless (file-readable-p gnus-harvest-db-path)
    (gnus-harvest-create-db))

  (add-hook 'gnus-article-prepare-hook 'gnus-harvest-addresses)
  (add-hook 'message-send-hook 'gnus-harvest-addresses)

  (dolist (feature features)
    (cond ((eq 'message-x feature)
           (eval-after-load "message"
             '(require 'message-x))
           (eval-after-load "message-x"
             '(add-to-list 'message-x-completion-alist
                           '("\\([rR]esent-\\|[rR]eply-\\)?[tT]o:\\|[bB]?[cC][cC]:" .
                             gnus-harvest-find-address)))))))

(provide 'gnus-harvest)

;;; gnus-harvest.el ends here
