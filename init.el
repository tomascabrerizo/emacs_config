;; ----------------------------
;; @Tomas: emacs configuration
;; ----------------------------
(setq inhibit-startup-message t)
(setq visible-bell t)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)

(setq make-backup-files nil)
(setq auto-save-default nil)

(setq scroll-step 1)
(set-default 'truncate-lines t)

;; -----------------------
;; ORG mode configuration
;; -----------------------

;; Enable org-indent mode by default
(org-indent mode 1)

(setq uni-directory "D:/Uni/")

(setq org-agenda-files (list (concat uni-directory "school.org")
			     (concat uni-directory "buffer.org")))
(setq org-agenda-span 30)
(setq org-deadline-warning-days 30)

;; -----------------
;; Bright-red TODOs
;; -----------------
(setq fixme-modes '(c++-mode c-mode emacs-lisp-mode))
(make-face 'font-lock-fixme-face)
(make-face 'font-lock-study-face)
(make-face 'font-lock-important-face)
(make-face 'font-lock-note-face)
(mapc (lambda (mode)
	(font-lock-add-keywords
	 mode
	 '(("\\<\\(TODO\\)" 1 'font-lock-fixme-face t)
	   ("\\<\\(STUDY\\)" 1 'font-lock-study-face t)
	   ("\\<\\(IMPORTANT\\)" 1 'font-lock-important-face t)
           ("\\<\\(NOTE\\)" 1 'font-lock-note-face t))))
      fixme-modes)
(modify-face 'font-lock-fixme-face "Red" nil nil t nil nil nil nil)
(modify-face 'font-lock-study-face "Yellow" nil nil t nil nil nil nil)
(modify-face 'font-lock-important-face "Yellow" nil nil t nil nil nil nil)
(modify-face 'font-lock-note-face "Yellow" nil nil t nil nil nil nil)


;; ---------------------------------
;; Run command in specify directory
;; ---------------------------------

(defun in-directory (dir)
  "Runs execute-extended-command with default-directory set to the given
directory."
  (Interactive "DIn directory: ")
  (let ((default-directory dir))
    (call-interactively 'execute-extended-command)))

;; -----------------------------------------------------
;; Search for compile.bat or compile.sh file and run it
;; -----------------------------------------------------

(setq compilation-file "compile.bat")
  
(defun find-project-dir ()
  "Search for the project root directory (where the build file is save)"
  (let ((path-stack (delete "" (split-string default-directory "/")))
	(project-dir-path nil)
	(dir nil))
    (while (and path-stack (not project-dir-path))
      (setq dir (mapconcat 'identity path-stack "/"))
      (if (file-exists-p (concat dir (concat "/" compilation-file))) 
	  (setq project-dir-path dir))
      (setq path-stack (butlast path-stack)))
    project-dir-path))

(defun  compile-in-directory ()
  "Search for a compile file and run it in the file current directory"
  (let ((default-directory (find-project-dir)))
    (compile (format compilation-file))))


;; -------------------------------------------
;; Enable text wrapping in compilation buffer
;; -------------------------------------------

(defun my-compilation-mode-hook ()
  (setq truncate-lines nil) ;; automatically becomes buffer local
  (set (make-local-variable 'truncate-partial-width-windows) nil))
(add-hook 'compilation-mode-hook 'my-compilation-mode-hook)

;; -------------------------------------------------
;; Create .h file with same name of current .c file
;; -------------------------------------------------

(defun swap-to-h-file()
  (let ((ext (file-name-extension buffer-file-name)))
    (if (or (string= ext "c") (string= ext "cpp"))
	(find-file (file-name-with-extension buffer-file-name "h")))))

(defun swap-to-c-file()
  (let ((ext (file-name-extension buffer-file-name)))
    (if (string= ext "h")
	(find-file (file-name-with-extension buffer-file-name "c")))))

(defun toggle-c-and-h-file ()
    (let ((ext (file-name-extension buffer-file-name)))
      (if (or (string= ext "c") (string= ext "cpp"))
	  (find-file (file-name-with-extension buffer-file-name "h")))
      (if (string= ext "h")
	  (find-file (file-name-with-extension buffer-file-name "c")))))
      

;; -------------------
;; Emacs Key bindings
;; -------------------

;; bind swap to h file to f1
(global-set-key [f1] (lambda () (interactive) (toggle-c-and-h-file)))
;; bind compiling to f5
(global-set-key [f5] (lambda () (interactive) (compile-in-directory)))

(global-set-key (kbd "C-n") 'dabbrev-expand)
(global-set-key (kbd "M-n") 'next-error)
(global-set-key (kbd "M-p") 'previous-error)

;; ------------------
;; Emacs color theme
;; ------------------

;; (custom-set-faces
;;  '(default ((t (:family "Liberation Mono" :foundry "outline" :slant normal :weight normal :height 105 :width normal)))))
;;(load-theme 'leuven t)

(add-to-list 'default-frame-alist '(font . "Liberation Mono-11.5"))
(set-face-attribute 'default t :font "Liberation Mono-11.5")
(set-face-attribute 'font-lock-builtin-face nil :foreground "#ccfccb")
(set-face-attribute 'font-lock-comment-face nil :foreground "#888888")
(set-face-attribute 'font-lock-constant-face nil :foreground "#00ff00")
(set-face-attribute 'font-lock-doc-face nil :foreground "#888888")

(set-face-attribute 'font-lock-function-name-face nil :foreground "#ccfccb")
(set-face-attribute 'font-lock-variable-name-face nil :foreground "#ccfccb")
(set-face-attribute 'font-lock-keyword-face nil :foreground "#f2a359")
(set-face-attribute 'font-lock-type-face nil :foreground "#f2a359")
(set-face-attribute 'font-lock-string-face nil :foreground "#00ff00")

(set-foreground-color "#ccfccb")
(set-background-color "#1c1d21")
(set-cursor-color "#00ff00")


;; ---------------------
;; C mode hook function
;; ---------------------

(defun big-fun-c-hook ()
  (setq c-basic-offset 2)
  ;;(c-set-offset 'case-label '+)

  (defun c-header-format ()
    "Format the given file as a header file."
    (interactive)
    (setq BaseFileName (file-name-sans-extension (file-name-nondirectory buffer-file-name)))
    (insert "#ifndef _")
    (push-mark)
    (insert BaseFileName)
    (upcase-region (mark) (point))
    (pop-mark)
    (insert "_H\n")
    (insert "#define _")
    (push-mark)
    (insert BaseFileName)
    (upcase-region (mark) (point))
    (pop-mark)
    (insert "_H\n")
    (insert "/* ========================================================================\n")
    (insert "   $File: $\n")
    (insert "   $Date: $\n")
    (insert "   $Revision: $\n")
    (insert "   $Creator: Tomas Cabrerizo $\n")
    (insert "   $Notice: (C) Copyright 2023. All Rights Reserved. $\n")
    (insert "   ======================================================================== */\n")
    (insert "\n")
    (insert "#endif")
    )

  (defun c-source-format ()
    "Format the given file as a source file."
    (interactive)
    (setq BaseFileName (file-name-sans-extension (file-name-nondirectory buffer-file-name)))
    (insert "/* ========================================================================\n")
    (insert "   $File: $\n")
    (insert "   $Date: $\n")
    (insert "   $Revision: $\n")
    (insert "   $Creator: Tomas Cabrerizo $\n")
    (insert "   $Notice: (C) Copyright 2023. All Rights Reserved. $\n")
    (insert "   ======================================================================== */\n")
    )

  (cond ((file-exists-p buffer-file-name) t)
        ((string-match "[.]h" buffer-file-name) (c-header-format))
	((string-match "[.]c" buffer-file-name) (c-source-format))
        ((string-match "[.]cpp" buffer-file-name) (c-source-format))))

(add-hook 'c-mode-common-hook 'big-fun-c-hook)


(custom-set-faces
 '(org-level-1 ((t (:weight bold :height 1.5))))
 '(org-level-2 ((t (:inherit outline-2 :extend nil :height 1.2)))))
