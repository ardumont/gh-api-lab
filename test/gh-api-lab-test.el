;;; gh-api-lab-test.el ---                           -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Antoine R. Dumont

;; Author: Antoine R. Dumont <tony@dagobah>
;; Keywords:

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

;;

;;; Code:

(require 'ert)
(require 'cl)  ;; for avoiding void-function incf ... (in test dependency apparently)
(require 'el-mock)

(ert-deftest test-gh-api-lab--post ()
  (should (equal
           '(request "uri" :sync t
                     :type "POST"
                     :params (("a" . "b"))
                     :headers '(("Content-type" . "application/json"))
                     :data "{\"data\":\"1\"}"
                     :parser 'json-read
                     :success 'gh-api-lab--success-callback
                     :error 'gh-api-lab--error-callback)
           (gh-api-lab--post "uri" '(("a" . "b")) '(("data" . "1")))))
  (should (equal
           '(request "uri"
                     :sync t
                     :type "POST"
                     :params (("a" . "b"))
                     :headers '(("Authorization" . "token token")
                                ("Content-type" . "application/json"))
                     :data "\"data\""
                     :parser 'json-read
                     :success 'gh-api-lab--success-callback
                     :error 'gh-api-lab--error-callback)
           (gh-api-lab--post "uri" '(("a" . "b")) "data" "token"))))

(ert-deftest test-gh-api-lab--get ()
  (should (equal
           '(request "uri"
                     :sync t
                     :type "GET"
                     :params (("a" . "b"))
                     :headers '(("Content-type" . "application/json"))
                     :parser 'json-read
                     :success 'gh-api-lab--success-callback
                     :error 'gh-api-lab--error-callback)
           (gh-api-lab--get "uri" '(("a" . "b")))))
  (should (equal
           '(request "uri"
                     :sync t
                     :type "GET"
                     :params (("a" . "b"))
                     :headers '(("Authorization" . "token token")
                                ("Content-type" . "application/json"))
                     :parser 'json-read
                     :success 'gh-api-lab--success-callback
                     :error 'gh-api-lab--error-callback)
           (gh-api-lab--get "uri" '(("a" . "b")) 'token))))


(ert-deftest test-gh-api-lab--delete ()
  (should (equal
           '(request "uri"
                     :sync t
                     :type "DELETE"
                     :params (("a" . "b"))
                     :headers '(("Content-type" . "application/json"))
                     :parser 'json-read
                     :success 'gh-api-lab--success-callback
                     :error 'gh-api-lab--error-callback)
           (gh-api-lab--delete "uri" '(("a" . "b")))))
  (should (equal
           '(request "uri"
                     :sync t
                     :type "DELETE"
                     :params (("a" . "b"))
                     :headers '(("Authorization" . "token token")
                                ("Content-type" . "application/json"))
                     :parser 'json-read
                     :success 'gh-api-lab--success-callback
                     :error 'gh-api-lab--error-callback)
           (gh-api-lab--delete "uri" '(("a" . "b")) 'token))))

(require 'dash)
(defun gh-api-lab-hash-make-properties (properties)
  "Return a hash-table from PROPERTIES key/values."
  (defun gh-api-lab-hash-empty-hash ()
    "Empty hash table with test 'equal."
    (make-hash-table :test 'equal))
  (defun gh-api-lab-hash-puthash-data (key value entity)
    "Update at KEY the VALUE in the ENTITY map.
Return the entity updated or nil if the entity is nil."
    (when entity
      (puthash key value entity)
      entity))
  (--reduce-from (gh-api-lab-hash-puthash-data (car it) (cdr it) acc)
                 (gh-api-lab-hash-empty-hash)
                 properties))

(ert-deftest test-gh-api-lab-execute-query ()
  (should (eq 1
              (with-mock
                (mock (gh-api-lab--base-http :some-uri) => "uri")
                (mock (gh-api-lab--get "uri" :some-params :token) => '(+ 1 0))
                (gh-api-lab-execute-query
                 (gh-api-lab-hash-make-properties '((:uri . :some-uri)
                                                    (:params . :some-params)
                                                    (:method . "GET")))
                 :token))))
  (should (eq 2
              (with-mock
                (mock (gh-api-lab--base-http :some-uri) => "uri")
                (mock (gh-api-lab--post "uri" :some-params :some-body :token) => '(+ 1 1))
                (gh-api-lab-execute-query
                 (gh-api-lab-hash-make-properties '((:uri . :some-uri)
                                                    (:params . :some-params)
                                                    (:method . "POST")
                                                    (:body . :some-body)))
                 :token))))
  (should (eq 3
              (with-mock
                (mock (gh-api-lab--base-http :some-uri) => "uri")
                (mock (gh-api-lab--delete "uri" :some-params :token) => '(+ 1 2))
                (gh-api-lab-execute-query
                 (gh-api-lab-hash-make-properties '((:uri . :some-uri)
                                                    (:params . :some-params)
                                                    (:method . "DELETE")))
                 :token))))
  (should-error (gh-api-lab-execute-query
                 (gh-api-lab-hash-make-properties '((:uri . :some-uri)
                                                    (:params . :some-params)
                                                    (:method . "unknown")))
                 :token)
                :type 'error))

(ert-deftest test-gh-api-lab-make-query ()
  (should (equal "uri" (gethash :uri (gh-api-lab-make-query "uri" "method" "body" "params"))))
  (should (equal "method" (gethash :method (gh-api-lab-make-query "uri" "method" "body" "params"))))
  (should (equal "body" (gethash :body (gh-api-lab-make-query "uri" "method" "body" "params"))))
  (should (equal "params" (gethash :params (gh-api-lab-make-query "uri" "method" "body" "params"))))
  (should-not (gethash :params (gh-api-lab-make-query "uri" "method" "body"))))

(ert-deftest test-gh-api-lab-api-create-release-query ()
  (should (equal "/repos/:owner/:repo/releases"
                 (gethash :uri (with-mock
                                 (mock (gh-api-lab-create-release-json :tag :branch :desc :body) => "body")
                                 (gh-api-lab-api-create-release-query :owner :repo :tag :branch :desc :body)))))
  (should (equal "POST"
                 (gethash :method (with-mock
                                    (mock (gh-api-lab-create-release-json :tag :branch :desc :body) => "body")
                                    (gh-api-lab-api-create-release-query :owner :repo :tag :branch :desc :body)))))
  (should (equal "body"
                 (gethash :body (with-mock
                                  (mock (gh-api-lab-create-release-json :tag :branch :desc :body) => "body")
                                  (gh-api-lab-api-create-release-query :owner :repo :tag :branch :desc :body))))))

(ert-deftest test-gh-api-lab-create-release-json ()
  (should (equal '(("body" . "body") ("name" . "desc") ("target_commitish" . "branch") ("tag_name" . "tag"))
                 (gh-api-lab-create-release-json "tag" "branch" "desc" "body")))
  (should (equal '(("body" . "body") ("name" . "desc") ("target_commitish" . "branch") ("tag_name" . "tag") ("draft" . t) ("prerelease" . t))
                 (gh-api-lab-create-release-json "tag" "branch" "desc" "body" "draft" "prerelease"))))

(ert-deftest test-gh-api-lab-get-releases-query ()
  (should (equal "/repos/user/repo/releases" (gethash :uri (gh-api-lab-get-releases-query "user" "repo"))))
  (should (equal "GET" (gethash :method (gh-api-lab-get-releases-query "user" "repo"))))
  (should-not (gethash :body (gh-api-lab-get-releases-query "user" "repo")))
  (should-not (gethash :params (gh-api-lab-get-releases-query "user" "repo"))))


(ert-deftest test-gh-api-lab-delete-release-query ()
  (should (equal "/repos/:owner/:repo/releases/:id" (gethash :uri (gh-api-lab-delete-release-query :owner :repo :id))))
  (should (equal "DELETE" (gethash :method (gh-api-lab-delete-release-query :owner :repo :id))))
  (should-not (gethash :body (gh-api-lab-delete-release-query :owner :repo :id)))
  (should-not (gethash :params (gh-api-lab-delete-release-query :owner :repo :id))))

(ert-deftest test-gh-api-lab-delete-release ()
  (should (eq :delete-done
              (with-mock
                (mock (gh-api-lab-delete-release-query :owner :repo :id) => :delete-query)
                (mock (gh-api-lab-execute-query :delete-query gh-api-lab--access-token) => :delete-done)
                (gh-api-lab-delete-release :owner :repo :id)))))

(provide 'gh-api-lab-test)
;;; gh-api-lab-test.el ends here
