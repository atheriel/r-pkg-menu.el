(defconst r-pkg-menu--pkg-list-code
  "local({
# Get all installed packages and relevant metadata.
raw_installed <- utils::installed.packages(fields = c('Title', 'Description',
                                                      'Repository'))
installed <- as.data.frame(raw_installed, stringsAsFactors = FALSE)[,
  c('Package', 'Title', 'Description', 'Version', 'Priority', 'Repository')]

# Compute package status: base, recommended, dependency, installed
installed$Status <- installed$Priority
installed$Status[is.na(installed$Status)] <-
  sapply(installed$Package[is.na(installed$Status)], function(pkg) {
    revdeps <- tools::dependsOnPkgs(pkg, recursive = FALSE,
                                    installed = raw_installed)
    if (length(revdeps) > 0) { 'dependency' } else { 'installed' }
  })
installed$Priority <- NULL

# Rename for merging
installed$Installed <- installed$Version
installed$Version <- NULL

# Style repositories in lowercase, to match Emacs's `package-menu'.
installed$Repository[is.na(installed$Repository)] <- '--'
installed$Repository <- tolower(installed$Repository)

# Trim whitespace in description and title.
installed$Title <- gsub('\\\\s+', ' ', installed$Title)
installed$Description <- gsub('\\\\s+', ' ', installed$Description)

# Take only the first sentence of the description.
installed$Description <- lapply(
  strsplit(installed$Description, '. ', fixed = TRUE),
  function(x) ifelse(length(x) > 1, paste0(x[1], '...'), x)
)

# Check which packages need to be updated.
updates <- utils::old.packages(instPkgs = raw_installed)
cat('\n') # For readability.
updates <- as.data.frame(updates, stringsAsFactors = FALSE)

# Format for merging.
updates$LibPath <- NULL
updates$Available <- updates$ReposVer
updates$Repository <- NULL

info <- merge(updates, installed, all.y = TRUE)[,
  c('Package', 'Available', 'Installed', 'Status', 'Repository',
    'Title', 'Description')]

# Replace NAs with '--'
info$Available[is.na(info$Available)] <- '--'

# Pad columns with an extra space, to make parsing easier.
for (col in names(info)[-1]) {
  info[[col]] <- paste0(' ', info[[col]], ' ')
}

# Print the data frame into the buffer.
old_width <- getOption('width')
options(width = 10000)
print(info, right = FALSE, row.names = FALSE)
options(width = old_width)
invisible(NULL)
})
")

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

(defun r-pkg-menu--format-pkg (pkg)
  "Docs."
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
          ,(if (not (r-pkg-menu-pkg-title pkg)) "--"
             (propertize (r-pkg-menu-pkg-title pkg)
                         'font-lock-face 'package-description))
          ,(if (not (r-pkg-menu-pkg-desc pkg)) "--"
             (propertize (r-pkg-menu-pkg-desc pkg)
                         'font-lock-face 'package-description))]))

(define-derived-mode r-pkg-menu-mode tabulated-list-mode "R Package Menu"
  "Docs."
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key '("Package" . nil))
  (add-hook 'tabulated-list-revert-hook #'r-pkg-menu--refresh nil t))

(defun r-pkg-menu ()
  "Docs."
  (interactive)
  (unless (and (boundp 'ess-dialect) (string= "R" ess-dialect))
    (user-error "Must be called from an *R* buffer."))
  (let ((proc ess-local-process-name))
    (with-current-buffer (get-buffer-create "*R Packages*")
      (r-pkg-menu-mode)
      (setq-local ess-local-process-name proc)
      (r-pkg-menu--refresh)
      (tabulated-list-print)
      (hl-line-mode 1)
      (switch-to-buffer (current-buffer)))))

(defun r-pkg-menu--refresh ()
  "Docs."
  (interactive)
  (let ((buff (get-buffer-create "*R Packages (Internal)*"))
        header has-repo pkgs)
    (ess-command r-pkg-menu--pkg-list-code buff)
    (with-current-buffer buff
      (goto-char (point-min))
      ;; Skip forward until we see the printed data.frame output.
      (if (not (re-search-forward "Package" nil t))
          ;; Error out if this doesn't look like we'd expect.
          (progn
            (user-error "Failed to find package updates.")
            (switch-to-buffer buff))
        ;; Parse the header.
        (let* ((begin (progn (backward-word 1) (point)))
               (end (progn (re-search-forward "Description") (point))))
          (setq header (split-string (buffer-substring begin end)
                                     "[\\ ]\\{1,\\}" t "\\ ")
                has-repo (not (null (member "Repository" header))))
          (message "header: %s" header)
          (forward-line 1))
        ;; Populate the list of packages.
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
    ;; Write the list of packages into `tabulated-list-entries'.
    (with-current-buffer (get-buffer-create "*R Packages*")
      (setq tabulated-list-entries
            (mapcar #'r-pkg-menu--format-pkg pkgs)
            tabulated-list-format `[("Package" 18 t)
                                    ("Available" 9 nil)
                                    ("Installed" 9 nil)
                                    ("Status" 11 t)
                                    ("Repository" 10 t)
                                    ("Title" 50 nil)
                                    ("Description" 0 nil)])
      ;; Refresh the header.
      (tabulated-list-init-header))
    ;; Clean up the intermediate buffer.
    ;; (kill-buffer buff)
    (message "R Packages: %d" (length pkgs))))

(provide 'r-pkg-menu)
