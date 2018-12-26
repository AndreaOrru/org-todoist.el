;;; org-todoist.el --- Org sync with Todoist -*- lexical-binding: t -*-

;; Version: 0.0.1
;; Author: Andrea Orru <andrea@orru.io>
;; Keywords: org, todoist
;; Package-Requires: ((emacs "24") (request-deferred "0.3.0"))
;; URL: https://github.com/AndreaOrru/org-todoist.el

;; This file is distributed under the terms of the BSD 2-Clause license.

;;; Commentary:
;;

;;; Code:

(require 'json)
(require 'org)
(require 'parse-time)
(require 'request-deferred)
(require 'seq)

(defgroup org-todoist nil
  "Org sync with Todoist."
  :tag "org todoist"
  :group 'org)

(defcustom org-todoist-api-token "a75389723a5846c2b1838261aaabc1c5cd099271"
  "API Token for authentication."
  :group 'org-todoist
  :type 'string)

(defcustom org-todoist-file "~/org/todo.org"
  "."
  :group 'org-todoist
  :type 'string)

(defconst org-todoist-url "https://beta.todoist.com/API/v8/%s")

(defvar org-todoist-projects nil)

(defun org-todoist-project-tasks (project tasks)
  "Given a list of TASKS, return only the ones in PROJECT."
  (seq-filter
   (lambda (task)
     (= (alist-get 'id project)
        (alist-get 'project_id task)))
   tasks))

(defun org-todoist-format-date (date-string)
  "Given a DATE-STRING, return it in Org format."
  (let* ((date  (parse-time-string date-string))
         (day   (nth 3 date))
         (month (nth 4 date))
         (year  (nth 5 date)))
    (format-time-string "%Y-%m-%d %a"
                        (encode-time 0 0 0 day month year))))

(defun org-todoist-format-project (project)
  "Given a PROJECT, return its Org representation."
  (concat "* "
          (alist-get 'name project)
          "\n   :PROPERTIES:"
          (format "\n   :ID: %s" (alist-get 'id project))
          "\n   :END:\n"))

(defun org-todoist-format-task (task)
  "Given a TASK, return its Org representation."
  (concat "** "
          (if (eq (alist-get 'completed task) :json-false)
              "TODO "
            "DONE ")
          (pcase (alist-get 'priority task)
            (4 "[#A] ")
            (3 "[#B] ")
            (2 "[#C] ")
            (_ ""))
          (alist-get 'content task)
          (when (alist-get 'due task)
            (format "\n   SCHEDULED: <%s>"
                    (org-todoist-format-date
                     (alist-get 'date (alist-get 'due task)))))
          "\n   :PROPERTIES:"
          (format "\n   :ID: %s" (alist-get 'id task))
          "\n   :END:\n"
          ))

(defun org-todoist-parse ()
  "Return tasks as defined in the Org file."
  (let ((ast (with-temp-buffer
               (insert-file-contents org-todoist-file)
               (org-mode)
               (org-element-parse-buffer))))
    (progn
      (defvar current-project nil)
      (defvar file-tasks nil)
      (org-element-map ast 'headline
        (lambda (hl)
          (if (= (org-element-property :level hl) 1)
              (setq current-project hl)
            (add-to-list
             'file-tasks
              `((id         . ,(org-element-property :ID hl))
                (project_id . ,(org-element-property :ID current-project))
                (content    . ,(org-element-property :raw-value hl))
                (completed  . ,(if (string= (org-element-property :todo-keyword hl) "TODO")
                                   :json-false
                                 t)))))))
      file-tasks)))

(defun org-todoist-sync ()
  "Sync Org file with Todoist."
  (interactive)
  (deferred:$
    (request-deferred
     (format org-todoist-url "projects")
     :headers `(("Authorization" . ,(format "Bearer %s" org-todoist-api-token)))
     :parser 'json-read)
    (deferred:nextc it
      (lambda (response)
        (setq org-todoist-projects (request-response-data response))))

    (request-deferred
     (format org-todoist-url "tasks")
     :headers `(("Authorization" . ,(format "Bearer %s" org-todoist-api-token)))
     :parser 'json-read)
    (deferred:nextc it
      (lambda (response)
        (request-response-data response)))

    (deferred:nextc it
      (lambda (tasks)
        (with-current-buffer (find-file-noselect org-todoist-file)
          (save-excursion
            (erase-buffer)
            (insert
             (mapconcat (lambda (project)
                          (concat (org-todoist-format-project project)
                                  (mapconcat (lambda (task)
                                               (org-todoist-format-task task))
                                             (org-todoist-project-tasks project tasks)
                                             "")))
                        org-todoist-projects
                        ""))
            (save-buffer)))))))

(provide 'org-todoist)
;;; org-todoist.el ends here
