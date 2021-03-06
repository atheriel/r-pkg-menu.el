;;; r-pkg-menu.el --- Manage R packages with Emacs and ESS -*- lexical-binding: t -*-

;; Copyright (C) 2017 Aaron Jacobs

;; Author: Aaron Jacobs <atheriel@gmail.com>
;; Version: 0.1
;; Keywords: ess
;; URL: https://github.com/atheriel/r-pkgdev.el

;; This file is not part of GNU Emacs.

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

;; https://github.com/atheriel/r-pkg-menu.el

;;; Code:

;;;; Front Matter

(require 'package)
(require 'ess-inf)

(eval-and-compile
  (require 'cl-lib))

(defcustom r-pkg-menu-title-as-desc nil
  "Use the R package's title field as its description, instead of
the description itself."
  :type 'boolean)

;;;; R Code

(defconst r-pkg-menu-dir
  (let ((libfile (locate-library "r-pkg-menu" nil
                                 (cons default-directory load-path)
                                 nil)))
    (when libfile
      (directory-file-name (file-name-directory libfile))))
  "Directory containing the `r-pkg-menu' lisp library.")

(defconst r-pkg-menu--pkg-list-code
  (when r-pkg-menu-dir
    (with-temp-buffer
      (insert "local({\n")
      (insert-file-contents (expand-file-name (concat r-pkg-menu-dir
                                                      "/list-packages.R")))
      (goto-char (point-max))
      (insert "invisible(NULL)\n})\n")
      (buffer-string)))
  "R code to load relevant package information from a running R session.")

(defsubst r-pkg-menu--cran-is-unset ()
  "Checks whether the 'CRAN' repository is unset in a given R
session."
  (string-equal
   "@CRAN@"
   (car (ess-get-words-from-vector "getOption('repos')[['CRAN']]\n"))))

;;;; Data Format and Package Cache

(cl-defstruct (r-pkg-menu-pkg
               (:constructor r-pkg-menu-pkg-create))
  "Docs."
  name
  version
  available
  status
  repo
  built-under
  title
  desc)

(defvar-local r-pkg-menu-pkgs nil
  "Buffer-local cache of R packages in the buffer's R session.")

(defun r-pkg-menu--parse-r-pkgs (buffer)
  "Parse BUFFER and return the list of packages it contains."
  (let (pkgs)
    (with-current-buffer buffer
      (goto-char (point-min))
      ;; Skip forward until we see the printed data.frame output.
      (if (not (re-search-forward "Description" nil t))
          ;; Error out if this doesn't look like we'd expect.
          (progn
            (user-error "Failed to find package updates.")
            (switch-to-buffer buff))
        ;; Populate the list of packages.
        (forward-line 1)
        (while (not (equal (point) (point-max)))
          (let* ((begin (point))
                 (end (progn (forward-line 1) (point)))
                 (raw (buffer-substring begin end))
                 ;; Header/order should be: Package Available Installed
                 ;; (Repository) Built Under Description
                 (parts (split-string raw "[\\ |\\\n]\\{2,\\}" t "[\\ |\\\n]"))
                 (pkg (r-pkg-menu-pkg-create
                       :name (car parts)
                       :version (nth 2 parts)
                       :available (when (not (string= "--" (nth 1 parts)))
                                    (nth 1 parts))
                       :status (nth 3 parts)
                       :repo (when (not (string= "--" (nth 4 parts)))
                               (nth 4 parts))
                       :title (nth 5 parts)
                       :desc (nth 6 parts))))
            (push pkg pkgs)))))
    pkgs))

;;;###autoload
(defun r-pkg-menu-refresh-contents ()
  "Update the cache of packages used in the current R session."
  (interactive)
  (inferior-ess-r-force)
  (message "Building list of installed R packages & available updates...")
  (let ((buff (get-buffer-create "*R Packages (Internal)*"))
        pkgs)
    (ess-command r-pkg-menu--pkg-list-code buff)
    (setq pkgs (r-pkg-menu--parse-r-pkgs buff))
    ;; Store a buffer-local cache in the R process buffer.
    (with-current-buffer (process-buffer (get-process ess-local-process-name))
      (setq r-pkg-menu-pkgs pkgs))))

;;;; Major Mode

;;;;; Marking Packages in the R Packages Buffer

(defsubst r-pkg-menu--pkg-at-point (&optional pos)
  (tabulated-list-get-id pos))

(defun r-pkg-menu--set-tag (mark &optional pos)
  "Tag the package under point (or at POS if non-nil) with MARK."
  (save-excursion
    (when pos (goto-char pos))
    (tabulated-list-put-tag mark)))

(defun r-pkg-menu--get-tag (&optional pos)
  "Retrieve the mark for the `tabulated-list-entry' under point,
or at POS if non-nil."
  (save-excursion
    (when pos (goto-char pos))
    (beginning-of-line)
    (buffer-substring-no-properties (point) (1+ (point)))))

(defun r-pkg-menu-mark-or-unmark-update (&optional pos)
  "Mark or unmark the package under point (or at POS if non-nil)
for updating or re-installation."
  (interactive)
  ;; TODO: Add support for marking multiple entries when region is active.
  ;; TODO: Prevent marking packages that don't have a source.
  ;; TODO: (Temporary) Prevent marking packages from GitHub.
  (if (string= (r-pkg-menu--get-tag pos) "U")
      (r-pkg-menu--set-tag " " pos)
    (r-pkg-menu--set-tag "U")))

(defun r-pkg-menu-mark-or-unmark-delete (&optional pos)
  "Mark or unmark the package under point (or at POS if non-nil)
for deletion."
  (interactive)
  ;; TODO: Add support for marking multiple entries when region is active.
  (if (string= (r-pkg-menu--get-tag pos) "D")
      (r-pkg-menu--set-tag " " pos)
    (let ((pkg (r-pkg-menu--pkg-at-point pos)))
      (when (pcase (r-pkg-menu-pkg-status pkg)
              ("base" (prog1 nil
                        (message "R's base packages cannot be removed")))
              ("dependency"
               (yes-or-no-p (format (concat "Package '%s' is depended upon by "
                                            "other packages. Remove anyway?")
                                    (r-pkg-menu-pkg-name pkg))))
              ("recommended"
               (yes-or-no-p (format (concat "Package '%s' is recommended. "
                                            "Remove anyway?")
                                    (r-pkg-menu-pkg-name pkg))))
              (_ t))
        (r-pkg-menu--set-tag "D")))))

(defun r-pkg-menu--get-pkgs-marked-update ()
  "Retrieve a list of all packages marked 'U' for updating or
re-installation."
  (let (queue)
    (save-excursion
      (goto-char (point-min))
      (while (not (equal (point) (point-max)))
        (when (string= (r-pkg-menu--get-tag) "U")
          (push (r-pkg-menu--pkg-at-point) queue))
        (forward-line 1)))
    queue))

(defun r-pkg-menu--get-pkgs-marked-delete ()
  "Retrieve a list of all packages marked 'D' for deletion."
  (let (queue)
    (save-excursion
      (goto-char (point-min))
      (while (not (equal (point) (point-max)))
        (when (string= (r-pkg-menu--get-tag) "U")
          (push (r-pkg-menu--pkg-at-point) queue))
        (forward-line 1)))
    queue))

;;;;; Sorting Packages in the R Packages Buffer

(defun r-pkg-menu--name-pred (pkg-a pkg-b)
  "Compare the names of PKG-A and PKG-B.

Following R's convention, this comparison is case-insensitive."
  (string< (downcase (r-pkg-menu-pkg-name (car pkg-a)))
           (downcase (r-pkg-menu-pkg-name (car pkg-b)))))

(defun r-pkg-menu--status-index (pkg)
  "Convert the status of PKG from a string to an index."
  (pcase (r-pkg-menu-pkg-status pkg)
    ("installed" 4)
    ("dependency" 3)
    ("recommended" 2)
    ("base" 1)
    (_ 0)))

(defun r-pkg-menu--status-pred (pkg-a pkg-b)
  "Compare the status of PKG-A and PKG-B.

The sort order is 'base', 'recommended', 'dependency',
'installed'."
  (< (r-pkg-menu--status-index (car pkg-a))
     (r-pkg-menu--status-index (car pkg-b))))

(defun r-pkg-menu--available-pred (pkg-a pkg-b)
  "Compare available versions of PKG-A and PKG-B.

The sort order is purely 'does an available version exist'. There
is no sorting on the actual version. Instead, secondary sorting
is on the package name."
  (let ((avail-a (r-pkg-menu-pkg-available (car pkg-a)))
        (avail-b (r-pkg-menu-pkg-available (car pkg-b))))
    (cond
     ((and (null avail-a) (null avail-b))
      (r-pkg-menu--name-pred pkg-a pkg-b))
     ((and (not (null avail-a)) (not (null avail-b)))
      (r-pkg-menu--name-pred pkg-a pkg-b))
     ((null avail-a) nil)
     (t t))))

;;;;; Formatting Entries for the R Package Buffer

(defun r-pkg-menu--format-pkg (pkg)
  "Formats PKG appropriately for `tabulated-list-entries' in the
`r-pkg-menu-mode' buffer."
  (list pkg
        `[,(propertize (r-pkg-menu-pkg-name pkg) 'font-lock-face 'package-name)
          ,(if (not (r-pkg-menu-pkg-available pkg)) "--"
             (propertize (r-pkg-menu-pkg-available pkg)
                         'font-lock-face 'package-status-available))
          ,(if (not (r-pkg-menu-pkg-version pkg)) "--"
             (propertize (r-pkg-menu-pkg-version pkg)
                         'font-lock-face 'package-status-available))
          ,(if (not (r-pkg-menu-pkg-status pkg)) "--"
             (propertize (r-pkg-menu-pkg-status pkg)
                         'font-lock-face 'package-status-available))
          ,(if (not (r-pkg-menu-pkg-repo pkg)) "--"
             (propertize (r-pkg-menu-pkg-repo pkg)
                         'font-lock-face 'package-status-available))
          ,(if r-pkg-menu-title-as-desc
               ;; Print the package's Title.
               (if (not (r-pkg-menu-pkg-title pkg)) "--"
                 (propertize (r-pkg-menu-pkg-title pkg)
                             'font-lock-face 'package-description))
             ;; Or its Description.
             (if (not (r-pkg-menu-pkg-desc pkg)) "--"
               (propertize (r-pkg-menu-pkg-desc pkg)
                           'font-lock-face 'package-description)))]))

(defun r-pkg-menu--init-header ()
  "Wrapper around `tabulated-list-init-header' that sets up the
Package Menu header format and initializes it."
  (interactive)
  (setq tabulated-list-format `[("Package" 14 r-pkg-menu--name-pred)
                                ("Available" 11 r-pkg-menu--available-pred)
                                ("Installed" 11 nil)
                                ("Status" 12 r-pkg-menu--status-pred)
                                ("Source" 7 t)
                                (,(if r-pkg-menu-title-as-desc "Title"
                                    "Description") 0 nil)])
  (tabulated-list-init-header))

(defun r-pkg-menu--refresh ()
  "Repopulate the `tabulated-list-entries' in the
`r-pkg-menu-mode' buffer."
  ;; Retrieve the package list from the buffer-local cache.
  (let ((pkgs (buffer-local-value
               'r-pkg-menu-pkgs
               (process-buffer (get-process ess-local-process-name)))))
    (with-current-buffer (get-buffer-create "*R Packages*")
      (setq tabulated-list-entries
            (mapcar #'r-pkg-menu--format-pkg pkgs)))))

;;;;; Major Mode Bindings and Definition

(defvar r-pkg-menu-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map [?d] #'r-pkg-menu-mark-or-unmark-delete)
    (define-key map [?u] #'r-pkg-menu-mark-or-unmark-update)
    map)
  "Local keymap for `r-pkg-menu-mode' buffers.")

(define-derived-mode r-pkg-menu-mode tabulated-list-mode "R Package Menu"
  "Docs."
  :keymap r-pkg-menu-mode-map
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key '("Available" . nil))
  (add-hook 'tabulated-list-revert-hook #'r-pkg-menu--refresh nil t))

;;;###autoload
(defun r-pkg-menu ()
  "Docs."
  (interactive)
  (inferior-ess-r-force)
  (when (r-pkg-menu--cran-is-unset)
    (error "CRAN mirror has not been set properly"))
  (let ((proc ess-local-process-name)
        (pkgs r-pkg-menu-pkgs))
    (with-current-buffer (get-buffer-create "*R Packages*")
      (r-pkg-menu-mode)
      (setq-local ess-local-process-name proc)
      (unless pkgs (r-pkg-menu-refresh-contents))
      (r-pkg-menu--refresh)
      (r-pkg-menu--init-header)
      (tabulated-list-print)
      (hl-line-mode 1)
      (switch-to-buffer (current-buffer)))))

;;;; Evil Support

(with-eval-after-load 'evil
  ;; Start in motion mode.
  (add-to-list 'evil-motion-state-modes 'r-pkg-menu-mode)
  (evil-define-key* 'motion r-pkg-menu-mode-map
                    "gr" #'revert-buffer)
  (evil-define-key* 'visual r-pkg-menu-mode-map
                    "gr" #'revert-buffer
                    "d" #'r-pkg-menu-mark-or-unmark-delete
                    "u" #'r-pkg-menu-mark-or-unmark-update))

;;;; End Matter

(provide 'r-pkg-menu)

;; Local Variables:
;; coding: utf-8
;; End:

;;; r-pkg-menu.el ends here
