;; ----------------------
;; Package configuration 
;; ----------------------
(add-to-list 'load-path  (concat default-directory "lib/evil"))
(require 'evil)
(evil-mode 1)

(load-theme 'modus-operandi nil)
(load-theme 'spacemacs-light t)

;; ----------------------------
 ;; @Tomas: emacs configuration
;; ----------------------------
(setq inhibit-startup-message t)
(setq visible-bell t)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)

(global-hl-line-mode 1)

(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

(setq make-backup-files nil)
(setq auto-save-default nil)

(setq-default indent-tabs-mode nil)
(add-hook 'window-setup-hook 'toggle-frame-maximized t)

;; set emacs font
(set-face-attribute 'default nil
                    :family "JetBrains Mono"
                    :height 110
                    :weight 'normal)

;; -------------------------
;; Temporal C configuration 
;; -------------------------
(set-default 'truncate-lines t)
(setq c-basic-offset 4)
(c-set-offset 'case-label '+)

;; -----------------------------
;; @Tomas: Custom lisp functions
;; -----------------------------

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

;;---------------------------------------
;; @Tomas: Emacs modal mode (version 0.0)
;;---------------------------------------

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
(t-normal-mode -1)
