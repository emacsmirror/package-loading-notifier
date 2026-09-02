;;; package-loading-notifier.el --- Notify a package is being loaded -*- lexical-binding: t; -*-

;; Author: SeungKi Kim <tttuuu888@gmail.com>
;; URL: https://github.com/tttuuu888/package-loading-notifier
;; Version: 0.4.0
;; Keywords: convenience faces config startup
;; Package-Requires: ((emacs "25.1"))

;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License see
;; <http://www.gnu.org/licenses/>.

;;; Commentary:

;; To enable `package-loading-notifier' globally, add the following lines to
;; your .emacs:
;;
;;  (require 'package-loading-notifier)
;;  (package-loading-notifier-mode 1)
;;

;;; Code:

(require 'seq)

(defgroup package-loading-notifier nil
  "Notify a package is being loaded."
  :prefix "package-loading-notifier-"
  :group 'startup)

(defcustom package-loading-notifier-packages '(org)
  "List of packages to notify when the package is being loaded."
  :type '(repeat symbol)
  :group 'package-loading-notifier)

(defcustom package-loading-notifier-format "%s loading ..."
  "String format to notify a package is being loaded.
`%s' is replaced by the package name."
  :type 'string
  :group 'package-loading-notifier)

(defface package-loading-notifier-face
  '((t :inverse-video t :weight bold))
  "Face used to notify a package is being loaded."
  :group 'package-loading-notifier)

(defvar package-loading-notifier--pending nil
  "Packages still waiting to be notified.")

(defun package-loading-notifier--file-regexp (pkg)
  "Return a regexp matching the library file of PKG."
  (format "/%s\\.elc?\\(?:\\.gz\\)?\\'" (regexp-quote (symbol-name pkg))))

(defun package-loading-notifier--pending-package (file)
  "Return the pending package whose library file is FILE, or nil."
  (let ((case-fold-search nil))
    (seq-find (lambda (pkg) (string-match-p
                             (package-loading-notifier--file-regexp pkg) file))
              package-loading-notifier--pending)))

(defun package-loading-notifier--update ()
  "Make `file-name-handler-alist' watch exactly the pending packages."
  (setq file-name-handler-alist
        (rassq-delete-all #'package-loading-notifier--handler
                          file-name-handler-alist))
  (when package-loading-notifier--pending
    (push (cons (mapconcat #'package-loading-notifier--file-regexp
                           package-loading-notifier--pending "\\|")
                #'package-loading-notifier--handler)
          file-name-handler-alist)))

(defun package-loading-notifier--notify (pkg fn args)
  "Notify that PKG is being loaded while calling FN with ARGS."
  (let* ((msg (capitalize (format package-loading-notifier-format pkg)))
         (win (selected-window))
         (ovr (with-current-buffer (window-buffer win)
                (make-overlay (window-point win) (window-point win)))))
    (overlay-put ovr 'window win)
    (overlay-put ovr 'after-string
                 (propertize msg 'face 'package-loading-notifier-face))
    (message "%s" msg)
    (redisplay t)
    (unwind-protect
        (apply fn args)
      (delete-overlay ovr))))

(defun package-loading-notifier--handler (operation &rest args)
  "File name handler notifying that a pending package is being loaded.
Catches `require', `load', and autoloads via the `load' OPERATION.
ARGS are passed to OPERATION."
  (let ((pkg (and (eq operation 'load)
                  (package-loading-notifier--pending-package (car args)))))
    (if (not pkg)
        ;; Not ours: run the real OPERATION, skipping this handler.
        (let ((inhibit-file-name-handlers
               (cons #'package-loading-notifier--handler
                     (and (eq inhibit-file-name-operation operation)
                          inhibit-file-name-handlers)))
              (inhibit-file-name-operation operation))
          (apply operation args))
      ;; Ours: stop watching PKG, then load it with a notification.
      (setq package-loading-notifier--pending
            (remq pkg package-loading-notifier--pending))
      (package-loading-notifier--update)
      (package-loading-notifier--notify pkg #'load args))))

;;;###autoload
(define-minor-mode package-loading-notifier-mode
  "Notify a package is being loaded.
Set `package-loading-notifier-packages' before enabling the mode."
  :init-value nil
  :global t
  (if package-loading-notifier-mode
      (progn
        (setq package-loading-notifier--pending
              package-loading-notifier-packages)
        ;; Init files that reset `file-name-handler-alist' during startup would
        ;; drop the entry, so put it back once startup is over.
        (unless after-init-time
          (add-hook 'emacs-startup-hook #'package-loading-notifier--update t)))
    (setq package-loading-notifier--pending nil)
    (remove-hook 'emacs-startup-hook #'package-loading-notifier--update))
  (package-loading-notifier--update))

(provide 'package-loading-notifier)
;;; package-loading-notifier.el ends here
