;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(nyxt:define-package :nyxt/no-image-mode
    (:documentation "Mode to disable image display in buffer."))
(in-package :nyxt/no-image-mode)

(define-mode no-image-mode ()
  "Disable images in current buffer.")

(defmethod enable ((mode no-image-mode) &key)
  (setf (ffi-buffer-auto-load-image-enabled-p (buffer mode)) nil))

(defmethod disable ((mode no-image-mode) &key)
  (setf (ffi-buffer-auto-load-image-enabled-p (buffer mode)) t))
