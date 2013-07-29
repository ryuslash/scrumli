;; scrumli --- A simple scrum web application
;; Copyright (C) 2013  Tom Willemse

;; scrumli is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; scrumli is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Affero General Public License for more details.

;; You should have received a copy of the GNU Affero General Public License
;; along with scrumli.  If not, see <http://www.gnu.org/licenses/>.

(defpackage #:scrumli
  (:use :cl :ningle :drakma :clack.builder :clack.middleware.session
        :clack.response :clack.request :clack.middleware.static
        :postmodern :clack.middleware.postmodern :parenscript)
  (:import-from :clack :clackup)
  (:import-from :json :encode-json-to-string :decode-json)
  (:export #:start-scrumli))

(in-package #:scrumli)
