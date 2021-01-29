;;; package -- summary
;;; Commentary:
(require 'bind-key)

(defun linux-copy (beg end)
  (interactive "r")
  ;;  (copy-region-as-kill beg end)
  (shell-command-on-region beg end  "pbcopy")
  ;;  (goto-char end)
  ;;  (insert-buffer "*Shell Command Output*")
  (with-current-buffer
      "*Shell Command Output*"
    (send-string-to-terminal
     (concat "\x1B]52;;"
             (buffer-string)
             "\x1B\x5C")

     ))
  )

;; not working because start-process is asynchronous
(defun linux-copy-non-inter-wrong (text &optional push)
  (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" "*pbcopy" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc)

      ))
  (with-current-buffer
      "*pbcopy"
    (send-string-to-terminal
     (concat "\x1B]52;;"
             (substring (buffer-substring-no-properties (line-beginning-position) (line-end-position) ) 0 -25)
             "\x1B\x5C")
     ))

  )

(defun linux-copy-non-inter (text &optional push)
  (with-temp-buffer
    (insert text)
    (shell-command-on-region (point-min) (point-max)  "pbcopy")
    (with-current-buffer
        "*Shell Command Output*"
      (send-string-to-terminal
       (concat "\x1B]52;;"
               (buffer-string)
               "\x1B\x5C")
       ))))

(setq interprogram-cut-function 'linux-copy-non-inter)
;;(setq interprogram-paste-function 'osx-copy)

(add-hook 'org-mode-hook
          (lambda ()
            (setq truncate-lines nil)))

(global-linum-mode 1) ; always show line numbers
(setq linum-format "%d| ")  ;set format

(bind-key* "M-n" (lambda ()
                   (interactive)
                   (setq this-command 'next-line)
                   (next-line 10)))
(bind-key* "M-p" (lambda ()
                   (interactive)
                   (setq this-command 'next-line)
                   (previous-line 10)))


(require 'use-package)

(use-package lsp-mode
  :ensure t
  ;; uncomment to enable gopls http debug server
  ;; :custom (lsp-gopls-server-args '("-debug" "127.0.0.1:0"))
  :commands (lsp lsp-deferred)
  :config (progn
            ;; use flycheck, not flymake
            (setq lsp-prefer-flymake nil)))

;; Set up before-save hooks to format buffer and add/delete imports.
;; Make sure you don't have other gofmt/goimports hooks enabled.
(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

;; Optional - provides fancier overlays.
(use-package lsp-ui
             :ensure t
             :commands lsp-ui-mode)

;; Company mode is a standard completion package that works well with lsp-mode.
(use-package company
  :ensure t
  :config
  ;; Optionally enable completion-as-you-type behavior.
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 1))

(use-package company-lsp
  :ensure t
  :commands company-lsp)


(use-package go-mode
  :ensure t
  :bind (
         ;; If you want to switch existing go-mode bindings to use lsp-mode/gopls instead
         ;; uncomment the following lines
         ;; ("C-c C-j" . lsp-find-definition)
         ;; ("C-c C-d" . lsp-describe-thing-at-point)
         )
  :hook ((go-mode . lsp-deferred)
         (before-save . lsp-format-buffer)
         (before-save . lsp-organize-imports)))

(use-package go-guru
  :demand t)

(provide 'init-local)
;;; init-local.el ends here
