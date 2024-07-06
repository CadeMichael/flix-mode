;;; flix-mode.el --- Major mode for editing and interacting with flix source code -*- lexical-binding: t; -*-

;; Author: Cade Lueker
;; URL: https://github.com/CadeMichael/flix-mode
;; Version: 1.0.0
;; Keywords: languages

;;; Commentary:

;; Flix mode for syntax highlighting, interacting with `flix.jar', and flix repl
;; interopt.  There is a flix-mode that has been archived, but provided all but one
;; line of the code used for syntax highlighting in this package.  The repo cane be
;; found at https://github.com/jhckragh/flix-mode, and its author deserves props.

;; This package differs in that it allows users to interact with the `flix.jar' needed
;; in every flix project.  It provides functions for downloading, intializing, running,
;; building, and passing commands to `flix.jar'.

;; Additionally this package provides basic repl interopt for the flix repl.  Because
;; the flix repl reloads everytime a project file is saved and doesn't handle mutliline
;; input there is no send region function.  However, you can send lines, restart, and
;; navigate between the flix buffer and flix repl buffer seamlessly.

;;; Code:

;; +-------------------------------------+
;; |Flix mode definition and highlighting|
;; +-------------------------------------+

(defconst flix-mode-keywords
  '("alias" "and" "as" "case" "catch" "chan" "choose" "class" "def"
    "default" "deref" "else" "enum" "exists" "false" "forall" "force"
    "from" "get" "if" "import" "inline" "instance" "into" "lat" "law"
    "lawless" "lazy" "let" "match" "matchEff" "mut" "namespace" "new"
    "not" "opaque" "or" "override" "project" "pub" "query" "ref"
    "reifyBool" "reifyEff" "reifyType" "rel" "scoped" "sealed" "select"
    "set" "solve" "spawn" "true" "try" "type" "unlawful" "use" "where"
    "with")
  "Keywords recognized by `flix-mode'.")

(defvar flix-mode-font-lock-keywords
  `(("\\_<Impure\\|null\\_>\\|\\?\\?\\?\\|\\?[_[:lower:]][_[:alnum:]]*" (0 font-lock-warning-face))
    ("\\_<Pure\\_>" (0 font-lock-function-name-face))
    ("\\_<\\(true\\|false\\)\\_>" (0 font-lock-builtin-face))
    ("\\<\\(\\sw+\\)(" (1 font-lock-function-name-face))
    ("let\\*?[ \t]+\\([_[:lower:]][_[:alnum:]]*\\)" (1 font-lock-variable-name-face))
    ("\\_<\\([_[:lower:]][_[:alnum:]]*\\)[ \t]*:[ \t_[:upper:]]" (1 font-lock-variable-name-face))
    ("\\_<\\([_[:upper:]][_[:alnum:]]*\\)\\_>" (0 font-lock-type-face))
    (,(concat "\\_<" (regexp-opt flix-mode-keywords) "\\_>") (0 font-lock-keyword-face)))
  "Keyword highlighting for `flix-mode'.")

(defvar flix-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?\; "." st)
    (modify-syntax-entry ?: "." st)
    (modify-syntax-entry ?& "." st)
    (modify-syntax-entry ?# "." st)
    (modify-syntax-entry ?= "." st)
    (modify-syntax-entry ?\" "\"" st)
    (modify-syntax-entry ?\' "\"" st)
    (modify-syntax-entry ?_ "w" st)
    (modify-syntax-entry ?/ ". 124b" st)
    (modify-syntax-entry ?* ". 23" st)
    (modify-syntax-entry ?\n "> b" st)
    st)
  "Syntax table for `flix-mode'.")

(defcustom flix-mode-tab-width 4
  "The tab width to use for indentation.")

(defun flix-mode--indent-further ()
  (interactive)
  (let ((new-indent (+ (current-indentation) flix-mode-tab-width)))
    (if (> (current-column) (current-indentation))
        (save-excursion (indent-line-to new-indent))
      (indent-line-to new-indent))))

(defun flix-mode--indent-less ()
  (interactive)
  (save-excursion
    (indent-line-to (max 0 (- (current-indentation) flix-mode-tab-width)))))

(defun flix-mode--newline-and-maybe-indent ()
  (interactive)
  (let ((indent-further (and (eolp) (looking-back "[{(=]")))
        (prev-indent (current-indentation)))
    (newline)
    (if indent-further
        (indent-line-to (+ prev-indent flix-mode-tab-width))
      (indent-line-to prev-indent))))

(defvar flix-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-m" 'flix-mode--newline-and-maybe-indent)
    (define-key map [return] 'flix-mode--newline-and-maybe-indent)
    (define-key map "\C-j" 'flix-mode--newline-and-maybe-indent)
    (define-key map [tab] 'flix-mode--indent-further)
    (define-key map [backtab] 'flix-mode--indent-less)
    map)
  "Keymap for `flix-mode'.")

(require 'prog-mode)

;;;###autoload
(define-derived-mode flix-mode prog-mode "Flix"
  "A major mode for editing Flix files."
  :syntax-table flix-mode-syntax-table
  (setq-local indent-tabs-mode nil)
  (setq-local comment-start "//")
  (setq-local comment-start-skip "\\(//+\\|/\\*+\\) *")
  (setq-local comment-end "")
  (setq-local font-lock-defaults '(flix-mode-font-lock-keywords))
  (add-to-list 'electric-indent-chars ?\}))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.flix\\'" . flix-mode))

;; +-----------------------------+
;; |Flix repl based off of comint|
;; +-----------------------------+

(require 'comint)
(require 'ansi-color)

(defvar flix-jar-directory nil
  "The directory from which to start the Flix REPL.")

(defvar flix-last-buffer nil
  "The last flix buffer to call the repl.")

(defun +flix--repl-apply-ansi-color (string)
  "Apply ANSI color codes to STRING."
  (ansi-color-apply string))

(defun +flix--repl-stop-repl ()
  "Stop the current Flix REPL process, if any."
  (let ((buffer (get-buffer "*Flix REPL*")))
    (when (and buffer (get-buffer-process buffer))
      (kill-process (get-buffer-process buffer))
      (kill-buffer buffer))))

(defun +flix--repl-start-repl ()
  "Start a new Flix REPL process."
  (let* ((default-directory (or flix-jar-directory (read-directory-name "Select directory with flix.jar: ")))
         (buffer (get-buffer-create "*Flix REPL*")))
    (unless (comint-check-proc buffer)
      (with-current-buffer buffer
        (setq flix-jar-directory default-directory)
        (apply 'make-comint-in-buffer "Flix REPL" buffer "java" nil '("-jar" "flix.jar" "repl"))
        (flix-repl-mode)))
    (pop-to-buffer buffer)))

(defvar flix-repl-mode-map
  (let ((map (nconc (make-sparse-keymap) comint-mode-map)))
    (define-key map "\t" 'completion-at-point)
    map)
  "Basic mode map for `flix-repl-mode'.")

(defun +flix--repl-initialize ()
  "Helper function to initialize `flix-repl'."
  (setq comint-process-echoes t)
  (setq comint-use-prompt-regexp t)
  (add-hook 'comint-preoutput-filter-functions '+flix--repl-apply-ansi-color nil t))

(define-derived-mode flix-repl-mode comint-mode "Flix REPL"
  "Major mode for `flix-repl'."
  nil "Flix REPL"
  (setq comint-prompt-regexp "flix> ")
  (setq mode-line-process '(":%s"))
  (setq comint-get-old-input (lambda () ""))
  (setq comint-process-echoes t)
  (setq comint-use-prompt-regexp t)
  (set (make-local-variable 'paragraph-separate) "\\'")
  (set (make-local-variable 'paragraph-start) comint-prompt-regexp)
  (set (make-local-variable 'font-lock-defaults) '(nil t))
  (set (make-local-variable 'comint-input-filter)
       (lambda (str) (not (string-match "\\`\\s-*\\'" str))))
  (set (make-local-variable 'comint-output-filter-functions)
       (list 'comint-postoutput-scroll-to-bottom)))

(add-hook 'flix-repl-mode-hook '+flix--repl-initialize)

;; +-----------------------------------------+
;; |Functions for interacting with `flix.jar'|
;; +-----------------------------------------+

;;;###autoload
(defun +flix/install-jar ()
  "Install flix.jar in a selected directory if it is not already present."
  (interactive)
  (let* ((default-directory (or default-directory))
         (selected-dir (read-directory-name "Install into directory: " default-directory))
         (jar-path (concat (file-name-as-directory selected-dir) "flix.jar"))
         (download-url "https://github.com/flix/flix/releases/latest/download/flix.jar"))
    (if (file-exists-p jar-path)
        (message "flix.jar is already installed.")
      (url-copy-file download-url jar-path t)
      (message "flix.jar has been downloaded and installed.")
      (setq flix-jar-directory selected-dir))))

;;;###autoload
(defun +flix/flix-command ()
  "Run a command with the flix.jar in the specified directory."
  (interactive)
  (let* ((default-directory (or flix-jar-directory default-directory))
         (selected-dir (read-directory-name "init project in: " default-directory))
         (command (read-string "Command: "))
         (jar-path (concat (file-name-as-directory selected-dir) "flix.jar")))
    (if (file-exists-p jar-path)
        (let ((default-directory selected-dir))
          (message (concat "Running flix " command "..."))
          (setq flix-jar-directory selected-dir)
          (async-shell-command (concat "java -jar flix.jar " command)))
      (message "flix.jar not found in the selected directory."))))

;;;###autoload
(defun +flix/init-project ()
  "Initialize a flix project in the selected directory if flix.jar is present."
  (interactive)
  (let* ((default-directory (or flix-jar-directory default-directory))
         (selected-dir (read-directory-name "init project in: " default-directory))
         (jar-path (concat (file-name-as-directory selected-dir) "flix.jar")))
    (if (file-exists-p jar-path)
        (let ((default-directory selected-dir))
          (message "initializing flix project...")
          (setq flix-jar-directory selected-dir)
          (shell-command "java -jar flix.jar init"))
      (message "flix.jar not found in the selected directory."))))

;;;###autoload
(defun +flix/run-project ()
  "Initialize a Flix project in the selected directory if flix.jar is present."
  (interactive)
  (let* ((default-directory (or flix-jar-directory default-directory))
         (selected-dir (read-directory-name "Flix exe: " default-directory))
         (jar-path (concat (file-name-as-directory selected-dir) "flix.jar")))
    (if (file-exists-p jar-path)
        (let ((default-directory selected-dir))
          (message "Running Flix project...")
          (setq flix-jar-directory selected-dir)
          (async-shell-command "java -jar flix.jar run"))
      (message "flix.jar not found in the selected directory."))))

;;;###autoload
(defun +flix/build-project ()
  "Initialize a Flix project in the selected directory if flix.jar is present."
  (interactive)
  (let* ((default-directory (or flix-jar-directory default-directory))
         (selected-dir (read-directory-name "Flix exe: " default-directory))
         (jar-path (concat (file-name-as-directory selected-dir) "flix.jar")))
    (if (file-exists-p jar-path)
        (let ((default-directory selected-dir))
          (message "Building Flix project...")
          (setq flix-jar-directory selected-dir)
          (async-shell-command "java -jar flix.jar build"))
      (message "flix.jar not found in the selected directory."))))

;;;###autoload
(defun +flix/repl ()
  "Run an inferior instance of `flix' inside Emacs."
  (interactive)
  (setq flix-last-buffer (current-buffer))
  (+flix--repl-start-repl))

;;;###autoload
(defun +flix/goto-flix-buffer ()
  "Switch to buffer that called `flix-repl'."
  (interactive)
  (if (buffer-live-p (get-buffer flix-last-buffer))
      (switch-to-buffer-other-window flix-last-buffer)
    (message "Flix buffer deleted")))

;;;###autoload
(defun +flix/goto-repl ()
  "Switch to active Flix REPL."
  (interactive)
  (setq flix-last-buffer (current-buffer))
  (if (get-buffer "*Flix REPL*")
      (switch-to-buffer-other-window "*Flix REPL*")
    (+flix/repl)))

;;;###autoload
(defun +flix/repl-restart ()
  "Restart the Flix REPL process."
  (interactive)
  (+flix--repl-stop-repl)
  (+flix--repl-start-repl))

;;;###autoload
(defun +flix/set-jar-directory ()
  "Set the directory where flix.jar is located."
  (interactive)
  (let ((selected-dir (read-directory-name "Flix exe: " default-directory)))
    (setq flix-jar-directory selected-dir)))

;;;###autoload
(defun +flix/send-line-to-repl ()
  "Send the current line to the Flix REPL."
  (interactive)
  (let ((line (thing-at-point 'line t)))
    (comint-send-string (get-buffer-process "*Flix REPL*") line)
    (comint-send-string (get-buffer-process "*Flix REPL*") "\n")))

(provide 'flix-mode)

;;; flix-mode.el ends here
