;;; org-linker-edna.el --- Link things in orgmode          -*- lexical-binding: t; -*-

;; Copyright (C) 2020  tosh

;; Author: tosh <tosh.lyons@gmail.com>
;; Version: 0.1
;; Package-Requires: (org org-linker org-edna)
;; URL: https://github.com/toshism/org-linker-edna
;; Keywords: convenience, hypermedia

;; This program is free software; you can redistribute it and/or modify
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

;; Link things in org
;;
;; I should describe how it actually works here.

;;; Code:

(require 'org-linker)

(setq org-linker-to-heading t)
(setq link-id-function 'org-id-get-create)


(defun org-linker-edna-ids (s)
  "Return a list of ids found in S.
S is a string formatted as org edna ids property value."
  (when s
    (when (string-match "ids(\\([^\\)]*\\)).*" s)
      (split-string (match-string 1 s)))))


(defun org-linker-edna-get-or-create-id-for-marker (m)
  (save-excursion
    (goto-char (marker-position m))
    (funcall link-id-function)))


(defun org-linker-edna-set-prop (source target property)
    (let* ((existing-blocker-ids (org-linker-edna-ids (org-entry-get (marker-position target) property)))
	 (source-id (org-linker-edna-get-or-create-id-for-marker source)))
      (if existing-blocker-ids
	  (if (not (member source-id existing-blocker-ids))
	      (cons source-id existing-blocker-ids)
	    existing-blocker-ids)
	(cons source-id '()))))


(defun org-linker-edna-set-blocker (source target)
  (org-entry-put target "BLOCKER" (format "ids%s" (org-linker-edna-set-prop source target "BLOCKER"))))


(defun org-linker-edna-set-trigger (source target)
  (org-entry-put target "TRIGGER" (concat (format "ids%s" (org-linker-edna-set-prop source target "TRIGGER")) " todo!(TODO) scheduled!(\".\")")))


(defun org-linker-edna-callback (source target)
  (let ((source-id (org-linker-edna-get-or-create-id-for-marker source))
	(target-id (org-linker-edna-get-or-create-id-for-marker target)))
    (org-linker-edna-set-blocker source target)
    (org-linker-edna-set-trigger target source)
    ))


(defun org-linker-edna ()
  (interactive)
  (org-linker 'org-linker-edna-callback))


(provide 'org-linker-edna)

;;; org-linker-edna.el ends here
