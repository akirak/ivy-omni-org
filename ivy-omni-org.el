;;; ivy-omni-org.el --- One command to rule all Org buffers and files -*- lexical-binding: t -*-

;; Copyright (C) 2019 Akira Komamura

;; Author: Akira Komamura <akira.komamura@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "25.1") (ivy "0.10"))
;; Keywords: outlines
;; URL: https://github.com/akirak/ivy-omni-org

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This library provides `ivy-omni-org' command, which lets you access
;; buffers, files, and bookmarks via a single Ivy interface.

;;; Code:

(require 'ivy)

;;;; Custom variables
(defcustom ivy-omni-org-file-sources nil
  "List of sources producing a list of Org files."
  :type '(repeat (choice symbol function)))

(defcustom ivy-omni-org-buffer-display-transformer
  'ivy-omni-org-default-buffer-transformer
  "Display transformer for Org buffers."
  )
(defcustom ivy-omni-org-file-display-transformer
  'ivy-omni-org-default-file-transformer
  "Display transformer for Org files."
  )
(defcustom ivy-omni-org-bookmark-display-transformer
  'ivy-omni-org-default-bookmark-transformer
  "Display transformer for Org bookmarks."
  )

;;;; Faces
(defface ivy-omni-org-file-name
  '((default :inherit 'ivy-virtual))
  "Face for file names in the function.")

(defface ivy-omni-org-bookmark-name
  '((default :inherit 'font-lock-string-face))
  "Face for file names in the function.")

;;;; Commands
;;;###autoload
(defun ivy-omni-org ()
  "Ivy interface to buffers, files, and bookmarks in Org."
  (interactive)
  (ivy-read "Org: "
            #'ivy-omni-org--complete
            :caller #'ivy-omni-org
            :action #'ivy-omni-org--action))

(defun ivy-omni-org--prepend-entry-type (type entry)
  "Prepend TYPE indicator to an Ivy ENTRY."
  (declare (indent 1))
  (concat (format "%-8s "
                  (propertize type 'face 'font-lock-comment-face))
          entry))

(defun ivy-omni-org-default-buffer-transformer (buf)
  "Default display transformer for BUF."
  (format "%-18s  %s" buf (or (buffer-file-name buf) nil)))

(defun ivy-omni-org-default-file-transformer (file)
  "Default display transformer for FILE."
  (propertize (abbreviate-file-name file)
              'face 'ivy-omni-org-file-name))

(defun ivy-omni-org-default-bookmark-transformer (name)
  "Default display transformer for a bookmark with NAME."
  (format "%-30s  %s"
          (propertize name 'face 'ivy-omni-org-bookmark-name)
          (propertize (abbreviate-file-name (bookmark-get-filename name))
                      'face 'ivy-omni-org-file-name)))

(defun ivy-omni-org--display-transformer (inp)
  "The default display transformer for `ivy-omni-org'.

INP is an entry in the Ivy command."
  (condition-case _
      (cond
       ((get-buffer inp)
        (ivy-omni-org--prepend-entry-type "buffer"
          (funcall ivy-omni-org-buffer-display-transformer inp)))
       ((file-exists-p inp)
        (ivy-omni-org--prepend-entry-type "file"
          (funcall ivy-omni-org-file-display-transformer inp)))
       ;; Fallback
       ((bookmark-get-bookmark inp)
        (ivy-omni-org--prepend-entry-type "bookmark"
          (funcall ivy-omni-org-bookmark-display-transformer inp)))
       (t (error "Unexpected input to the display transformer: %s" inp)))
    (error inp)))

(ivy-set-display-transformer
 'ivy-omni-org
 #'ivy-omni-org--display-transformer)

(defun ivy-omni-org--bookmarks ()
  "Get a list of Org bookmarks."
  (bookmark-maybe-load-default-file)
  (cl-remove-if-not (lambda (info)
                      (let ((filename (alist-get 'filename (cdr info))))
                        (string-match-p "\\.org\\'" filename)))
                    bookmark-alist))

(defun ivy-omni-org--complete (&rest _args)
  "Generate completion candidates.

_ARGS is a list of arguments as passed to `all-completions'."
  (let* ((bufs (cl-remove-if-not
                (lambda (buf)
                  (with-current-buffer buf
                    (derived-mode-p 'org-mode)))
                (mapcar #'get-buffer (internal-complete-buffer "" nil t))))
         (bufnames (mapcar #'buffer-name bufs))
         (loaded-files (delq nil (mapcar #'buffer-file-name bufs)))
         (files (delete-duplicates
                 (-flatten (mapcar (lambda (source)
                                     (cl-etypecase nil
                                       (function (funcall source))
                                       (symbol (if (boundp source)
                                                   (symbol-value source)
                                                 (error "Unbound source: %s" source)))))
                                   ivy-omni-org-file-sources))
                 :test #'file-equal-p))
         (unloaded-files (seq-difference files loaded-files #'file-equal-p))
         (bookmarks (ivy-omni-org--bookmarks)))
    (append bufnames
            unloaded-files
            (mapcar #'car bookmarks))))

(defun ivy-omni-org--action (inp)
  "The default action of `ivy-omni-org' on INP."
  (pcase inp
    ((pred get-buffer) (switch-to-buffer inp))
    ((pred file-exists-p) (find-file inp))
    ((pred bookmark-get-bookmark) (bookmark-jump inp))))

(defun ivy-omni-org--other-window-action (inp)
  "Open INP (file/buffer/bookmark) in other window."
  (pcase inp
    ((pred get-buffer) (switch-to-buffer-other-window inp))
    ((pred file-exists-p) (find-file-other-window inp))
    ((pred bookmark-get-bookmark) (bookmark-jump-other-window inp))))

(ivy-add-actions 'ivy-omni-org
                 '(("j" ivy-omni-org--other-window-action "other window")))

(provide 'ivy-omni-org)
;;; ivy-omni-org.el ends here
