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

(ert-deftest test-gh-api-lab-execute-query ()
  (should (eq 2
              (with-mock
                (mock (gh-api-lab--post "https://api.github.com/repos/ardumont/gh-api-lab/releases"
                                        nil
                                        '(("body" . "release notes should be here")
                                          ("name" . "first tryout")
                                          ("target_commitish" . "master")
                                          ("tag_name" . "0.0.0.1"))
                                        'token) => '(+ 1 1))
                (gh-api-lab-execute-query
                 (gh-api-lab-api-release "ardumont" "gh-api-lab"
                                         "0.0.0.1"
                                         "master"
                                         "first tryout"
                                         "release notes should be here"
                                         t)

                 'token)))))

(ert-deftest test-gh-api-lab-make-query ()
  (should (equal "uri" (gethash :uri (gh-api-lab-make-query "uri" "method" "body" "params"))))
  (should (equal "method" (gethash :method (gh-api-lab-make-query "uri" "method" "body" "params"))))
  (should (equal "body" (gethash :body (gh-api-lab-make-query "uri" "method" "body" "params"))))
  (should (equal "params" (gethash :params (gh-api-lab-make-query "uri" "method" "body" "params"))))
  (should-not (gethash :params (gh-api-lab-make-query "uri" "method" "body"))))

(ert-deftest test-gh-api-lab-api-release ()
  (should (equal "/repos/:owner/:repo/releases"
                 (gethash :uri (with-mock
                                 (mock (gh-api-lab-create-release-json :tag :branch :desc :body) => "body")
                                 (gh-api-lab-api-release :owner :repo :tag :branch :desc :body)))))
  (should (equal "POST"
                 (gethash :method (with-mock
                                    (mock (gh-api-lab-create-release-json :tag :branch :desc :body) => "body")
                                    (gh-api-lab-api-release :owner :repo :tag :branch :desc :body)))))
  (should (equal "body"
                 (gethash :body (with-mock
                                  (mock (gh-api-lab-create-release-json :tag :branch :desc :body) => "body")
                                  (gh-api-lab-api-release :owner :repo :tag :branch :desc :body))))))

(ert-deftest test-gh-api-lab-create-release-json ()
  (should (equal '(("body" . "body") ("name" . "desc") ("target_commitish" . "branch") ("tag_name" . "tag"))
                 (gh-api-lab-create-release-json "tag" "branch" "desc" "body")))
  (should (equal '(("body" . "body") ("name" . "desc") ("target_commitish" . "branch") ("tag_name" . "tag") ("draft" . t) ("prerelease" . t))
                 (gh-api-lab-create-release-json "tag" "branch" "desc" "body" "draft" "prerelease"))))

(ert-deftest test-gh-api-lab-get-releases ()
  (should (equal "/repos/user/repo/releases" (gethash :uri (gh-api-lab-get-releases "user" "repo"))))
  (should (equal "GET" (gethash :method (gh-api-lab-get-releases "user" "repo"))))
  (should-not (gethash :body (gh-api-lab-get-releases "user" "repo")))
  (should-not (gethash :params (gh-api-lab-get-releases "user" "repo"))))


(provide 'gh-api-lab-test)
;;; gh-api-lab-test.el ends here
