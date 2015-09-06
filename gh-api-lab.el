;;; gh-api-lab.el ---                                    -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Antoine R. Dumont

;; Author: Antoine R. Dumont <eniotna.t AT gmail.com>
;; Maintainer: Antoine R. Dumont <eniotna.t AT gmail.com>
;; Version: 0.0.1
;; Package-Requires: ((emacs "24") (dash-functional "2.11.0") (s "1.9.0") (deferred "0.3.2") (request-deferred "0.2.0"))
;; Keywords: github api convenience
;; URL: https://github.com/ardumont/gh-api-lab

;; This file is NOT part of GNU Emacs.

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

;; Personal tinkering with github's api to try and automate release from within
;; emacs

;;; Code:

(defun gh-api-lab--base-http (uri)
  "Compute the base http url from URI."
  (format "https://api.github.com%s" uri) )

(defvar gh-api-lab--access-token-file "~/.emacs.d/.github/token"
  "Access-token file containing only the token to access the api.
This is installed manually.")

(defun gh-api-lab--load-access-token (token-file)
  "Given TOKEN-FILE, return the token if file exists.
Returns nil otherwise."
  (when (file-exists-p token-file)
    (s-trim
     (with-temp-file token-file
       (insert-file-contents token-file)
       (buffer-substring-no-properties (point-min) (point-max))))))

(defvar gh-api-lab--access-token nil
  "The actual access token.")
(setq gh-api-lab--access-token (gh-api-lab--load-access-token gh-api-lab--access-token-file))

(require 's)

(require 'request)

(defconst gh-api-lab-log-no-log 0 "No log level except for error.")
(defconst gh-api-lab-log-error  1 "Error log level.")
(defconst gh-api-lab-log-warn   2 "Warn log level.")
(defconst gh-api-lab-log-info   3 "Info log level.")
(defconst gh-api-lab-log-debug  4 "Debug log level.")
(defconst gh-api-lab-log-trace  5 "Trace log level.")

(defcustom gh-api-lab-log-level gh-api-lab-log-info
  "Set log level.
Levels:
0 - no log   (`gh-api-lab-log-quiet')
1 - errors   (`gh-api-lab-log-error')
2 - warnings (`gh-api-lab-log-warn')
3 - info     (`gh-api-lab-log-info')
4 - debug    (`gh-api-lab-log-debug')
5 - trace    (`gh-api-lab-log-trace')
To change such level, add this to your init.el file:
\(custom-set-variables '\(gh-api-lab-log-level gh-api-lab-log-trace\)\)"
  :options (list gh-api-lab-log-no-log
                 gh-api-lab-log-error
                 gh-api-lab-log-warn
                 gh-api-lab-log-info
                 gh-api-lab-log-debug
                 gh-api-lab-log-trace)
  :type 'integer
  ;;:require 'gh-api
  ;;:group 'org-trello
  )

(defun gh-api-lab-log-msg (level &rest args)
  "Log message with LEVEL.
Depending on `gh-api-lab-log-level', this will be displayed or not.
All errors are displayed anyway.
ARGS constitutes the parameters to feed to message."
  (when (or (<= level gh-api-lab-log-level) (eq gh-api-lab-log-error level))
    (apply 'message (format "gh-api - %s" (car args)) (cdr args))))

(defun gh-api-lab--error-callback (&rest response)
  "Standard error callback which expects a RESPONSE.
Simply displays an error message in the minibuffer with the error code."
  (let ((resp (plist-get response :response)))
    (gh-api-lab-log-msg gh-api-lab-log-info "client - Problem during request - error-thrown: %s" (request-response-error-thrown resp))
    (gh-api-lab-log-msg gh-api-lab-log-debug "Detailed response: %S" resp)))

(defun gh-api-lab--success-callback (&rest response)
  "Standard success callback with expects a RESPONSE.
Simply displays a success message in the minibuffer."
  (let* ((resp (plist-get response :response))
         (data (request-response-data resp)))
    (gh-api-lab-log-msg gh-api-lab-log-debug "Response: %S" resp)
    (gh-api-lab-log-msg gh-api-lab-log-debug "Data: %S" data)))

(require 'json)

(defun gh-api-lab--get (uri params &optional token)
  "GET to URI with PARAMS and DATA with TOKEN."
  (let ((default-headers '(("Content-type" . "application/json"))))
    `(request ,uri
              :sync    t
              :type    "GET"
              :params  ,params
              :headers (quote ,(if token
                                   (cons `("Authorization" . ,(format "token %s" token)) default-headers)
                                 default-headers))
              :parser  'json-read
              :success 'gh-api-lab--success-callback
              :error   'gh-api-lab--error-callback)))

(defun gh-api-lab--post (uri params data &optional token)
  "Post to URI with PARAMS and DATA with TOKEN."
  (let ((default-headers '(("Content-type" . "application/json"))))
    `(request ,uri
              :sync    t
              :type    "POST"
              :params  ,params
              :headers (quote ,(if token
                                   (cons `("Authorization" . ,(format "token %s" token)) default-headers)
                                 default-headers))
              :data    ,(json-encode data)
              :parser  'json-read
              :success 'gh-api-lab--success-callback
              :error   'gh-api-lab--error-callback)))

(defun gh-api-lab-make-properties (properties)
  "Return a hash-table from PROPERTIES key/values."
  (--reduce-from (orgtrello-hash-puthash-data (car it) (cdr it) acc)
                 (orgtrello-hash-empty-hash)
                 properties))

(defun gh-api-lab-execute-query (query &optional token)
  "Execute the QUERY.
Optional TOKEN for authentication."
  (let* ((uri (gethash :uri query))
         (params (gethash :params query))
         (body (gethash :body query))
         (full-uri (gh-api-lab--base-http uri))
         (method (gethash :method query)))
    (eval
     (cond ((string= method "GET") (gh-api-lab--get full-uri params token))
           ((string= method "POST") (gh-api-lab--post full-uri params body token))
           (t nil)))))

(defun gh-api-lab-make-query (uri method &optional body params)
  "Make a generic query from URI, METHOD, BODY and PARAMS."
  (let ((h (make-hash-table :test 'equal)))
    (puthash :uri uri h)
    (puthash :method method h)
    (puthash :body body h)
    (puthash :params params h)
    h))

(defun gh-api-lab-api-release (owner repo tag branch desc body &optional draft prerelease)
  "Create the api release call.
OWNER owner of the repository REPO.
TAG is the tag to create on BRANCH
DESC is the summary on the release.
BODY is the long description of the release.
DRAFT represents the status of draft or not.
PRERELEASE represents the status of prerelease or not."
  (gh-api-lab-make-query (format "/repos/%s/%s/releases" owner repo)
                         "POST"
                         (gh-api-lab-create-release-json tag branch desc body)))


(defun gh-api-lab-get-releases (owner repo)
  "Create the api release query to list the releases of the OWNER/REPO."
  (gh-api-lab-make-query (format "/repos/%s/%s/releases" owner repo)
                         "GET"))

(require 'dash)

(defun gh-api-lab--deal-with-optional-value (optional-entry value entries)
  "Add the optional value in entries depending on optional-entry.
If OPTIONAL-ENTRY is non nil, cons the VALUE to ENTRIES and return it.
Otherwise,return ENTRIES."
  (if optional-entry (cons value entries) entries))

(defun gh-api-lab--deal-with-optional-values (optional-entries-values entries)
  "Add the optional entry/value OPTIONAL-ENTRIES-VALUES in ENTRIES.
Return entries updated with value if entry, entries untouched otherwise."
  (->> optional-entries-values
       (--reduce-from (gh-api-lab--deal-with-optional-value (car it) (cdr it) acc)
                      entries)
       nreverse))

(defun gh-api-lab-create-release-json (tag-name branch-name desc body &optional draft prerelease)
  "Create a release json from TAG-NAME, BRANCH-NAME, DESC, BODY.
OWNER owner of the repository REPO.
TAG-NAME is the tag to create on BRANCH-NAME.
DESC is the summary on the release.
BODY is the long description of the release.
DRAFT represents the status of draft or not.
PRERELEASE represents the status of prerelease or not."
  (let ((initial-values `(("tag_name" . ,tag-name)
                          ("target_commitish" . ,branch-name)
                          ("name" . ,desc)
                          ("body" . ,body))))
    (gh-api-lab--deal-with-optional-values
     `((,draft . ("draft" . t))
       (,prerelease . ("prerelease" . t)))
     initial-values)))

(defun gh-api-lab-list-releases (owner repo)
  "List the releases of OWNER/REPO."
  (-> (gh-api-lab-get-releases owner repo)
      (gh-api-lab-execute-query gh-api-lab--access-token)))

(defun gh-api-lab-create-release (owner repo tag branch desc body &optional draft prerelease)
  "Create the actual release for the repo OWNER/REPO.
OWNER owner of the repository REPO.
TAG is the tag to create on BRANCH
DESC is the summary on the release.
BODY is the long description of the release.
DRAFT represents the status of draft or not.
PRERELEASE represents the status of prerelease or not."
  (-> (gh-api-lab-api-release owner repo tag branch desc body draft prerelease)
      (gh-api-lab-execute-query gh-api-lab--access-token)))

;; list releases
;; (gh-api-lab-list-releases "ardumont" "gh-api-lab")

;; create new releases
;; (gh-api-lab-create-release "ardumont" "gh-api-lab" "0.0.0.2" "master" "second dummy release" "this is the first release from emacs' repl" t)

(provide 'gh-api-lab)
;;; gh-api-lab.el ends here
