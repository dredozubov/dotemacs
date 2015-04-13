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

(defvar packages-to-install
             '(slime edts auto-highlight-symbol auto-complete eproject erlang f flycheck-haskell flycheck flymake ghci-completion grizzl haskell-mode helm async magit git-rebase-mode git-commit-mode pkg-info epl popup rust-mode s smart-tab smartparens solarized-theme dash))

; activate all the packages (in particular autoloads)
(package-initialize)

; fetch the list of packages available 
(unless package-archive-contents
  (package-refresh-contents))

; install the missing packages
(dolist (package packages-to-install)
  (unless (package-installed-p package)
    (package-install package)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("1297a022df4228b81bc0436230f211bad168a117282c20ddcba2db8c6a200743" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "e16a771a13a202ee6e276d06098bc77f008b73bbac4d526f160faa2d76c1dd0e" default)))
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-log t)
 '(haskell-process-suggest-remove-import-lines t)
 '(haskell-process-type (quote cabal-repl))
 '(haskell-tags-on-save t)
 '(scheme-program-name "mit-scheme"))
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

;;; enable show brackets mode
(show-paren-mode 1)

;;; show cursor's position
(column-number-mode 1)

;;; use ido mode
(require 'ido)
(ido-mode t)

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
;; (global-set-key (kbd "RET") 'newline)
;; (global-set-key (kbd "C-m") 'set-mark-command)

;;; company-mode visual completion
(add-hook 'haskell-mode-hook 'company-mode)

;;; use a dedicated swap directory
(setq backup-directory-alist
   `(("." . "~/.backup/emacs/")))
(setq backup-by-copying t) ;;; may be slow, but it's safe and ok with SSD

;;; use a dedicated temp files directory
(setq auto-save-file-name-transforms
   `((".*" ,(concat user-emacs-directory "auto-save/") t)))

;;; automatically reload buffers from files
(global-auto-revert-mode t)

;;; align around regex
(global-set-key (kbd "C-x a r") 'align-regexp)

;;; erlang-mode

;;; disable electric commands
(setq erlang-electric-commands '())

;;; haskell-mode
(require 'haskell-interactive-mode)
(require 'haskell-process)
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)

; Make Emacs look in Cabal directory for binaries
(let ((my-cabal-path (expand-file-name "~/.cabal/bin")))
  (setenv "PATH" (concat my-cabal-path ":" (getenv "PATH")))
  (add-to-list 'exec-path my-cabal-path))

;;; indentation
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

;;; auto tags generation: hasktags must be installed
(let ((my-cabal-path (expand-file-name "~/.cabal/bin")))
  (setenv "PATH" (concat my-cabal-path ":" (getenv "PATH")))
  (add-to-list 'exec-path my-cabal-path))

;;; haskell-mode hotkeys

(eval-after-load 'haskell-mode '(progn
  (define-key haskell-mode-map (kbd "C-'") 'haskell-interactive-bring)
  (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-or-reload)
  (define-key haskell-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
  (define-key haskell-mode-map (kbd "C-c C-t") 'haskell-process-do-type)
  (define-key haskell-mode-map (kbd "C-c C-i") 'haskell-process-do-info)
  ;; (define-key haskell-mode-map (kbd "SPC") 'haskell-mode-contextual-space)
  (define-key haskell-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
  (define-key haskell-mode-map (kbd "C-c c") 'haskell-process-cabal)))

(eval-after-load 'haskell-cabal '(progn
  (define-key haskell-cabal-mode-map (kbd "C-'") 'haskell-interactive-bring)
  (define-key haskell-cabal-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
  (define-key haskell-cabal-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
  (define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
  (define-key haskell-cabal-mode-map (kbd "C-c c") 'haskell-process-cabal)))
;;; use cabal repl(cabal 1.18+)

;;; hoogle import search
;;; requires hoogle binary
;; (custom-set-variables
;;   '(haskell-process-suggest-hoogle-imports t))

;;; contextual space -- sucks :(
;; (define-key haskell-mode-map (kbd "SPC") 'haskell-mode-contextual-space)

;;; nested block indenting
(eval-after-load "haskell-mode"
  '(progn
     (define-key haskell-mode-map (kbd "C-,") 'haskell-move-nested-left)
     (define-key haskell-mode-map (kbd "C-.") 'haskell-move-nested-right)))
;;; init ghc-mod
(autoload 'ghc-init "ghc" nil t)
(autoload 'ghc-debug "ghc" nil t)
(add-hook 'haskell-mode-hook (lambda () (ghc-init)))
;;; linting with hlint
(add-hook 'haskell-mode-hook 'flymake-hlint-load)
;;; jump to definition
(define-key haskell-mode-map (kbd "M-.") 'haskell-mode-jump-to-def)

;;; enabling disabled features
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;;; aligner rules
(add-hook 'align-load-hook (lambda ()
  ((add-to-list 'align-rules-list
               '(haskell-types
                 (regexp . "\\(\\s-+\\)\\(::\\|∷\\)\\s-+")
                 (modes quote (haskell-mode literate-haskell-mode))))
  (add-to-list 'align-rules-list
               '(haskell-assignment
                 (regexp . "\\(\\s-+\\)=\\s-+")
                 (modes quote (haskell-mode literate-haskell-mode))))
  (add-to-list 'align-rules-list
               '(haskell-arrows
                 (regexp . "\\(\\s-+\\)\\(->\\|→\\)\\s-+")
                 (modes quote (haskell-mode literate-haskell-mode))))
  (add-to-list 'align-rules-list
               '(haskell-left-arrows
                 (regexp . "\\(\\s-+\\)\\(<-\\|←\\)\\s-+")
                 (modes quote (haskell-mode literate-haskell-mode)))))))

;;; navigate imports
(define-key haskell-mode-map (kbd "C-c C-n C-i") 'haskell-navigate-imports)

;;; module templater
(add-hook 'haskell-mode-hook 'haskell-auto-insert-module-template)

;;; slime
;; Set your lisp system and, optionally, some contribs
(setq inferior-lisp-program "/usr/local/bin/sbcl")
(setq slime-contribs '(slime-fancy))
