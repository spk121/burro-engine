;; -*- Mode: scheme -*-
;; Copyright 2018 Michael L. Gran

;; This file is part of burro-engine

;; Burro-Engine is free software: you can redistribute it and/or
;; modify it under the terms of the GNU Lesser General Public License
;; as published by the Free Software Foundation, either version 3 of
;; the License, or (at your option) any later version.

;; Burro-Engine is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Lesser General Public License for more details.

;; You should have received a copy of the GNU Lesser General Public
;; License along with Burro-Engine.  If not, see
;; <http://www.gnu.org/licenses/>.

(use-modules (z automake-test-lib))

(automake-test
 (let ((estimated 481)
       (measured (window-height)))
   (format #t "Window height: goal ~a, measured ~a~%" estimated measured)
   (equal? estimated measured)))
