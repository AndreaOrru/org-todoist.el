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
(require 'request-deferred)

(defgroup org-todoist nil
  "Org sync with Todoist."
  :tag "org todoist"
  :group 'org)

(defcustom org-todoist-api-token nil
  "API Token for authentication."
  :group 'org-todoist
  :type 'string)


(defcustom org-todoist-file "~/org/todo.org"
  "."
  :group 'org-todoist
  :type 'string)

(defconst org-todoist-url "https://beta.todoist.com/API/v8/%s")

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
        (request-response-data response)))

    (deferred:nextc it
      (lambda (projects)
        (with-current-buffer (find-file-noselect org-todoist-file)
          (save-excursion
            (goto-char (point-max))
            (insert projects)
            (save-buffer)))))))


(provide 'org-todoist)
;;; org-todoist.el ends here
