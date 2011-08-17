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
        weight INTEGER NOT NULL,
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
            email, fullname, last_seen, weight
        FROM
            addrs
        WHERE
            (email LIKE '%s%s%%' OR fullname LIKE '%s%s%%')
        ORDER BY
            weight DESC,
            last_seen DESC
        LIMIT
            %d
    )"
                         (if prefix-only-p "" "%") stub
                         (if prefix-only-p "" "%") stub
                         gnus-harvest-query-limit))
                ")")))

(defun gnus-harvest-mailalias-complete-stub (stub)
  (require 'mailalias)
  (sendmail-sync-aliases)
  (if (eq mail-aliases t)
      (progn
	(setq mail-aliases nil)
	(if (file-exists-p mail-personal-alias-file)
	    (build-mail-aliases))))
  (let ((entry (assoc stub mail-aliases)))
    (if entry
        (cdr entry)
      (delete nil
              (mapcar (lambda (entry)
                        (if (string-prefix-p stub (car entry))
                            (cdr entry)))
                      mail-aliases)))))

(defun gnus-harvest-bbdb-complete-stub (stub)
  (require 'bbdb)
  (catch 'found
    (delete
     nil
     (apply
      'append
      (mapcar
       (lambda (record)
         (let* ((nets (bbdb-record-net record))
                (name (bbdb-record-name record))
                (aliases
                 (bbdb-split (bbdb-record-getprop
                              record bbdb-define-all-aliases-field) ","))
                (match (catch 'matches
                         (ignore
                          (mapc (lambda (alias)
                                  (if (string-match stub alias)
                                      (throw 'matches t)))
                                aliases)))))
           (when match
             (mapc
              (lambda (alias)
                (if (and (string= alias stub)
                         (= 1 (length nets)))
                    (throw 'found (format "%s <%s>" name (car nets)))))
              aliases)
             (mapcar (lambda (addr) (format "%s <%s>" name addr)) nets))))
       (let ((target (cons bbdb-define-all-aliases-field ".")))
         (bbdb-search (bbdb-records) nil nil nil target)))))))

(defun gnus-harvest-insert-address (email fullname moment weight)
  (insert "INSERT OR REPLACE INTO addrs (email, ")
  (if fullname
      (insert "fullname, "))
  (insert "last_seen, weight) VALUES (lower('" email "'), '")
  (if fullname
      (insert fullname "', '"))
  (insert moment "', '")
  (insert (number-to-string weight) "');\n"))

;;;###autoload
(defun gnus-harvest-addresses ()
  "Harvest and remember the addresses in the current article buffer."
  (let ((tmp-buf (generate-new-buffer "*gnus harvest*"))
        (moment (number-to-string (floor (float-time)))))
    (mapc
     (lambda (info)
       (if info
           (let ((field (car info)))
             (mapc (lambda (addr)
                     (unless (string-match gnus-harvest-ignore-email-regexp
                                           (cadr addr))
                       (with-current-buffer tmp-buf
                         (gnus-harvest-insert-address
                          (cadr addr) (car addr) moment
                          (if (string= "to" field)
                              10
                            1)))))
                   (cdr info)))))
     (mapcar (lambda (field)
               (let ((value (message-field-value field)))
                 (and value
                      (cons field
                            (mail-extract-address-components value t)))))
             '("to" "reply-to" "from" "resent-from" "cc" "bcc")))
    (with-current-buffer tmp-buf
      (gnus-harvest-sqlite-invoke nil t)
      (kill-buffer (current-buffer)))))

;;;###autoload
(defun gnus-harvest-find-address ()
  (interactive)
  (let* ((stub (word-at-point))
         (aliases (if (featurep 'bbdb)
                      (gnus-harvest-bbdb-complete-stub stub)
                    (gnus-harvest-mailalias-complete-stub stub))))
    (backward-kill-word 1)
    (insert
     (if (stringp aliases)
         aliases
       (ido-completing-read "Use address: "
                            (delete-dups
                             (append aliases
                                     (gnus-harvest-complete-stub stub)))
                            nil t stub)))))

;;;###autoload
(defun gnus-harvest-install (&rest features)
  (unless (file-readable-p gnus-harvest-db-path)
    (gnus-harvest-create-db))

  (add-hook 'gnus-article-prepare-hook 'gnus-harvest-addresses)
  (add-hook 'message-send-hook 'gnus-harvest-addresses)

  (dolist (feature features)
    (cond ((eq 'message-x feature)
           (require 'message-x)
           (add-to-list 'message-x-completion-alist
                        '("\\([rR]esent-\\|[rR]eply-\\)?[tT]o:\\|[bB]?[cC][cC]:" .
                          gnus-harvest-find-address))))))

(provide 'gnus-harvest)

;;; gnus-harvest.el ends here
