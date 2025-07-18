# # Function to get active Quarto file
# render_with_pause <- function(outputtype=NULL){
#
#   get_active_file <- function() {
#     if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
#       rstudioapi::getActiveDocumentContext()$path
#     } else {
#       readline("Enter the Quarto file to render: ")
#     }
#   }
#
#   qmd <- get_active_file()
#   render_with_pause_file(qmd,outputtype)
# }
#
# dropboxPause <- function()  {
#   system("C:/PStools/PsSuspend.exe -nobanner Dropbox.exe", wait = TRUE)
#   message("Dropbox syncing paused.")
# }
#
# dropboxResume <- function(){
#   system("C:/PStools/PsSuspend.exe -r -nobanner Dropbox.exe", wait = TRUE)
#   message("dropbox syncing resumed.")
# }
#
# render_with_pause_file <- function(qmd,outputtype=outputtype,...) {
#   # pssuspend_path <- "C:\\PStools\\PsSuspend.exe"
#
#   if (!file.exists(qmd)) {
#     stop("No valid Quarto file selected. Please open a .qmd file in RStudio or provide a valid path.")
#   }
#
#   # Pause Dropbox syncing
#   dropboxPause()
#
#   # Run Quarto in an RStudio background job
#   job_script <- tempfile(fileext = ".R")
#   writeLines(
#     paste0('
#     on.exit(dropboxResume())
#     try(quarto::quarto_render("', qmd, '", output_format = "', outputtype, '"))',
#     job_script
#   )
#
#   rstudioapi::jobRunScript(
#     path = job_script,
#     name = "Quarto Rendering",
#     workingDir = dirname(qmd)
#   )
#
#   message("🚀 Quarto rendering started in the background.")
# }






#' ---
#' Quarto rendering helpers that build *entirely* outside Dropbox:
#' 1. Clone the source directory to a scratch dir (Temp).
#' 2. Render the .qmd there (so Quarto never touches Dropbox).
#' 3. Copy the resulting artefacts (PDF/HTML + *_files) back to the source.
#' 4. Delete the scratch dir.  No Dropbox locking, no PsSuspend.
#' ---------------------------------------------------------------------------

# ----------------------------------------------------------------------------
# Utility: determine the Quarto file to build ---------------------------------
# ----------------------------------------------------------------------------

get_active_file <- function() {
  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    rstudioapi::getActiveDocumentContext()$path
  } else {
    readline("Enter the Quarto file to render: ")
  }
}

# ----------------------------------------------------------------------------
# Public helpers --------------------------------------------------------------
# ----------------------------------------------------------------------------

#' Render the currently active .qmd via scratch directory build.
render_with_temp <- function(outputtype = 'default') {
  qmd <- get_active_file()
  if(outputtype %in% 'default'){
    render_with_temp_file(qmd)
  } else {
  render_with_temp_file(qmd, outputtype = outputtype)
  }
}

#' Render a specific .qmd file via scratch directory build.
#' @param qmd        Path to the .qmd file.
#' @param outputtype Quarto output format, or NULL to let Quarto decide.
render_with_temp_file <- function(qmd, outputtype = NULL, ...) {

  if (!file.exists(qmd)) {
    stop("No valid Quarto file selected. Please open a .qmd file in RStudio or provide a valid path.")
  }

  src_dir <- dirname(qmd)
  stem    <- tools::file_path_sans_ext(basename(qmd))

  # ------------------------------------------------------------------------
  # 1. Make scratch dir & copy source tree ---------------------------------
  # ------------------------------------------------------------------------
  scratch <- tempfile("quarto_scratch_")
  dir.create(scratch)

  # Copy the whole source dir into scratch (fast and avoids figuring deps)
  file.copy(src_dir, scratch, recursive = TRUE)

  scratch_src <- file.path(scratch, basename(src_dir))
  scratch_qmd <- file.path(scratch_src, basename(qmd))

  # ------------------------------------------------------------------------
  # 2. Compose background job script ---------------------------------------
  # ------------------------------------------------------------------------
  job_script <- tempfile(fileext = ".R")

  norm <- function(x) gsub("\\\\", "/", normalizePath(x, winslash = "/", mustWork = TRUE))
  scratch_src <- norm(scratch_src)
  scratch_qmd <- norm(scratch_qmd)
  src_dir     <- norm(src_dir)

  lines <- c(
    "library(quarto)",
    sprintf("scratch_src <- '%s'", scratch_src),
    sprintf("scratch_qmd <- '%s'", scratch_qmd),
    sprintf("src_dir <- '%s'", src_dir),
    sprintf("stem <- '%s'", stem),
    sprintf("outputtype <- %s", if (is.null(outputtype)) "NULL" else sprintf("'%s'", outputtype)),
    "setwd(scratch_src)",
    "quarto_bin <- quarto::quarto_path()",
    "cmd <- c('render', shQuote(scratch_qmd))",
    "if (!is.null(outputtype)) cmd <- c(cmd, '--to', outputtype)",
    "system2(quarto_bin, cmd)",
    "# ---- identify artefacts to copy back (robust to special chars) ----",
    "all_items <- list.files('.', full.names = TRUE, recursive = FALSE)",
    "needs_copy <- vector('list')",
    "for (item in all_items) {",
    "  b <- basename(item)",
    "  # directory named '<stem>_files' EXACT match",
    "  if (dir.exists(item) && identical(b, paste0(stem, '_files'))) needs_copy[[length(needs_copy)+1]] <- item",
    "  # files whose base name starts with '<stem>.' (e.g. stem.html, stem.pdf, stem.tex)",
    "  if (!dir.exists(item) && startsWith(b, paste0(stem, '.'))) needs_copy[[length(needs_copy)+1]] <- item",
    "}",
    "if (length(needs_copy)) {",
    "  for (item in needs_copy) {",
    "    dest <- file.path(src_dir, basename(item))",
    "    if (dir.exists(item)) {",
    "      unlink(dest, recursive = TRUE, force = TRUE)",
    "      dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)",
    "      file.copy(item, src_dir, recursive = TRUE, overwrite = TRUE)",
    "    } else {",
    "      file.copy(item, dest, overwrite = TRUE)",
    "    }",
    "  }",
    "}",
    "# Clean up scratch",
    "setwd(tempdir())",
    "unlink(dirname(scratch_src), recursive = TRUE, force = TRUE)",
    "message(paste0('✅ Render finished; artefacts updated in ', src_dir))"
  )

  writeLines(lines, job_script)

  # ------------------------------------------------------------------------
  # 3. Launch via background job if possible -------------------------------
  # ------------------------------------------------------------------------
  if (requireNamespace('rstudioapi', quietly = TRUE) && rstudioapi::isAvailable()) {
    rstudioapi::jobRunScript(
      path       = job_script,
      name       = "Quarto Rendering (scratch build)",
      workingDir = dirname(qmd),
      importEnv  = FALSE
    )
    message("🚀 Quarto scratch build started in background (Dropbox untouched).")
  } else {
    source(job_script, echo = TRUE, local = new.env())
  }
}


render <- function() {
  render_with_temp('default')
}

render_html <- function() {
  render_with_temp("html")
}

render_revealjs <- function() {
  render_with_temp("revealjs")
}

render_pdf <- function() {
  render_with_temp("pdf")
}

render_beamer <- function() {
  render_with_temp("beamer")
}



