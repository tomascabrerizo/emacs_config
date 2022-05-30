;; ----------------------------
;; @Tomas: emacs configuration
;; ----------------------------

;; symbolic link

(setq inhibit-startup-message t)
(setq visible-bell t)

(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(hl-line-mode -1)

(setq make-backup-files nil)
(setq auto-save-default nil)

(setq-default indent-tabs-mode nil)

;; maximize emacs on start up
(add-hook 'window-setup-hook 'toggle-frame-maximized t)

;; set emasc theme
;; override modus operandi background color
(setq modus-themes-operandi-color-overrides
      '((bg-main . "#ffffff")))
(setq modus-themes-operandi-color-overrides
      '((bg-main . "#fcf1dc")))
(load-theme 'modus-operandi t)

;; set emacs font
(set-face-attribute 'default nil
                    :family "JetBrains Mono"
                    :height 110
                    :weight 'normal)

;; Temporal C configuration
(setq c-basic-offset 4)
(set-default 'truncate-lines t)
(c-set-offset 'case-label '+)
;; (add-hook 'c-mode-common-hook
;;           (lambda ()
;;             (c-set-offset 'case-label '+)))

;; ----------------------------
;; @Tomas: Custom lisp functions
;; ----------------------------

(defun t-select-line ()
  (interactive)
  (beginning-of-line)
  (set-mark-command nil)
  (end-of-line))

(defun t-select-line-newline ()
  (interactive)
  (beginning-of-line)
  (set-mark-command nil)
  (next-line))

(defun t-cut-line ()
  (interactive)
  (t-select-line-newline)
  (kill-region (point) (mark)))

;;------------------------------
;; @Tomas: Emacs modal mode (version 0.0)
;;------------------------------

;; Normal mode key binndings
(defvar t-normal-mode-keymap (copy-keymap global-map))
(suppress-keymap t-normal-mode-keymap) ;; Undefines all printables characters for normal mode keymap
(define-key t-normal-mode-keymap (kbd "C-,") 't-normal-mode) ;; Toggle
(define-key t-normal-mode-keymap (kbd "n") 'next-window-any-frame)

;; Insert mode key binndings
(defvar t-insert-mode-keymap (copy-keymap global-map))
(define-key t-insert-mode-keymap (kbd "C-,") 't-normal-mode) ;; Toggle

;; Create minor mode to use as normal mode
(define-minor-mode t-normal-mode
  "Toggles normal and insert mode"
  :global t
  :lighter " [Normal]"
  (if t-normal-mode
      (funcall (lambda ()
                 (message "normal mode")
                 (use-global-map t-normal-mode-keymap)))
    (funcall (lambda ()
               (message "insert mode")
               (use-global-map t-insert-mode-keymap)))))

;; Start Emacs with tomi normal mode
(t-normal-mode 1)
