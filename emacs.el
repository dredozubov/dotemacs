;;; disable toolbar
(tool-bar-mode -1)

;;; enables gemacs to start in front of terminal multiplexor
(x-focus-frame nil)

;;; package manager initialization
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "e16a771a13a202ee6e276d06098bc77f008b73bbac4d526f160faa2d76c1dd0e" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;; setting default color theme
(load-theme 'solarized-dark)

;;; enable line numbers
(global-linum-mode 1)

;;; comments toggler
;;; http://www.emacswiki.org/emacs/CommentingCode
(defun toggle-comment ()
      (interactive)
      (let ((start (line-beginning-position))
            (end (line-end-position)))
        (when (region-active-p)
          (setq start (save-excursion
                        (goto-char (region-beginning))
                        (beginning-of-line)
                        (point))
                end (save-excursion
                      (goto-char (region-end))
                      (end-of-line)
                      (point))))
        (comment-or-uncomment-region start end)))
(global-set-key (kbd "M-;") 'toggle-comment)

;;; set font size
(set-face-attribute 'default nil :height 150)

;;; super-tab
(global-set-key (kbd "TAB") 'hippie-expand)

;;; electric enabled by default
;;; provides indentation on RET
(electric-indent-mode +1)

;;; use only tabs for indentation
(setq-default indent-tabs-mode nil)

;;; C-SPC won't work on OS X, so use C-m instead
;;; but first, we need to unbind RET from C-m
(global-set-key (kbd "RET") 'newline)
(global-set-key (kbd "C-m") 'set-mark-command)

;;; erlang-mode

;;; disable electric commands
(setq erlang-electric-commands '())
