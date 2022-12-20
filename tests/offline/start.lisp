;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt/tests)

(define-class dummy-renderer (renderer)
  ((name "Dummy renderer"))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:accessor-name-transformer (class*:make-name-transformer name)))

(defmethod install ((_ dummy-renderer)) t)

(defmethod uninstall ((_ dummy-renderer)) t)

(setf nyxt::*renderer* (make-instance 'dummy-renderer))

;; When attempting to read the slot's value (slot-value), the slot
;; NYXT::READY-P is missing from the object NIL.
;;    [Condition of type SB-PCL::MISSING-SLOT]

;; If there's no browser, then gracefully do nothing.
(define-test null-quit ()
  (nyxt:quit))

(define-test stateless-arguments ())
