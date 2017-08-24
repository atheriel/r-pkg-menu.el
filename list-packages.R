# Get all installed packages and relevant metadata.
raw_installed <- utils::installed.packages(fields = c('Title', 'Description',
                                                      'Repository',
                                                      'RemoteType'))
installed <- as.data.frame(raw_installed, stringsAsFactors = FALSE)[,
  c('Package', 'Title', 'Description', 'Version', 'Priority', 'Repository',
    'RemoteType')]

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
installed$Title <- gsub('\\s+', ' ', installed$Title)
installed$Description <- gsub('\\s+', ' ', installed$Description)

# Take only the first sentence of the description.
installed$Description <- lapply(
  strsplit(installed$Description, '. ', fixed = TRUE),
  function(x) ifelse(length(x) > 1, paste0(x[1], '...'), x)
)

# Attempt to annotate local and GitHub-sourced repos using RemoteType.
installed$Source <- ifelse(is.na(installed$RemoteType),
                           installed$Repository,
                           installed$RemoteType)
installed$Repository <- NULL
installed$RemoteType <- NULL

# Check which packages need to be updated.
updates <- utils::old.packages(instPkgs = raw_installed)
cat('\n') # For readability.
updates <- as.data.frame(updates, stringsAsFactors = FALSE)

# Format for merging.
updates$LibPath <- NULL
updates$Available <- updates$ReposVer
updates$Repository <- NULL

info <- merge(updates, installed, all.y = TRUE)[,
  c('Package', 'Available', 'Installed', 'Status', 'Source', 'Title',
    'Description')]

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
