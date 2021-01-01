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
  (with-current-buffer (marker-buffer m)
    (save-excursion
      (goto-char (marker-position m))
      (funcall link-id-function))))


(defun org-linker-edna-set-prop (source target property)
  (let* ((existing-blocker-ids (org-linker-edna-ids (org-entry-get (marker-position target) property)))
	 (source-id (org-linker-edna-get-or-create-id-for-marker source)))
    (cons (concat "\"id:" source-id "\"") '())))


(defun org-linker-edna-set-blocker (source target)
  (org-entry-put target "BLOCKER" (format "ids%s" (org-linker-edna-set-prop source target "BLOCKER"))))


(defun org-linker-edna-todo-state ()
  (let ((selected-state (ido-completing-read "todo!: " (cons "nil" org-todo-keywords-1))))
    (if (not (or (string= selected-state "nil") (string= selected-state "")))
	(concat " todo!(" selected-state ")")
      "")))


(defun org-linker-edna-schedule ()
  (let ((scheduled-date (read-string "Schedule: ")))
    (if (not (string= scheduled-date ""))
	(concat " scheduled!(" scheduled-date ")")
      "")))


(defun org-linker-edna-set-trigger (source target)
  (let* ((todo-state (org-linker-edna-todo-state))
	 (scheduled-date (org-linker-edna-schedule)))
    (org-entry-put target "TRIGGER"
		   (concat
		    (format "ids%s"
			    (org-linker-edna-set-prop source target "TRIGGER"))
		    todo-state
		    scheduled-date))))


(defun org-linker-edna-set-trigger-helm (source target)
  (let* ((actions (helm :sources (helm-build-sync-source "Select Trigger Actions"
				   :candidates org-linker-edna-actions
				   :action 'org-linker-edna-actions-dispatcher)))
	 (todo (plist-get actions :todo))
	 (scheduled (plist-get actions :scheduled))
	 (deadline (plist-get actions :deadline))
	 (todo-state (when todo
		       (concat " todo!(" todo ")")))
	 (scheduled-date (when scheduled
			   (concat " scheduled!(\"" scheduled "\")")))
	 (deadline-date (when deadline
			  (concat " deadline!(\"" deadline "\")")))
	 (existing-trigger (org-entry-get (marker-position target) "TRIGGER"))
	 (existing-blocker-ids (org-linker-edna-ids (org-entry-get (marker-position target) "TRIGGER")))
	 (source-id (org-linker-edna-get-or-create-id-for-marker source)))
    (org-entry-put target "TRIGGER"
		   (concat
		    existing-trigger
		    (format " ids%s"
			    (org-linker-edna-set-prop source target "TRIGGER"))
		    todo-state
		    scheduled-date
		    deadline-date))))


(defvar org-linker-edna-actions '("scheduled" "deadline" "todo"))
(setq org-linker-edna-actions '("scheduled" "deadline" "todo"))


(defun org-linker-edna-date-selector (type)
  (read-string (format "%s: " (capitalize type))))


(defun org-linker-edna-todo-candidates ()
  (buffer-local-value 'org-todo-keywords-1 (current-buffer)))


(defun org-linker-edna-state-selector ()
  (helm :sources (helm-build-sync-source "Select TODO state"
		   :candidates org-todo-keywords-1)))


(defun org-linker-edna-action-dispatcher (candidate)
  (cond ((string= "scheduled" candidate) `(:scheduled ,(org-linker-edna-date-selector "scheduled")))
	((string= "deadline" candidate) `(:deadline ,(org-linker-edna-date-selector "deadline")))
	((string= "todo" candidate) `(:todo ,(org-linker-edna-state-selector)))))


(defun org-linker-edna-actions-dispatcher (candidate)
  (mapcan 'org-linker-edna-action-dispatcher (helm-marked-candidates)))


(defun org-linker-edna-callback (source target)
  (let ((source-id (org-linker-edna-get-or-create-id-for-marker source))
	(target-id (org-linker-edna-get-or-create-id-for-marker target)))
    (org-linker-edna-set-blocker source target)
    (org-linker-edna-set-trigger-helm target source)
    ))


(defun org-linker-edna ()
  (interactive)
  (org-linker 'org-linker-edna-callback))


(provide 'org-linker-edna)

;;; org-linker-edna.el ends here
