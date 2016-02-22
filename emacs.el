;;; package manager initialization
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)

;;; activate all the packages (in particular autoloads)
(package-initialize)

(defvar packages-to-install
  '(evil evil-args evil-jumper evil-nerd-commenter slime edts auto-highlight-symbol auto-complete eproject erlang f flycheck-haskell flycheck flymake ghci-completion grizzl haskell-mode helm async magit pkg-info epl popup rust-mode s smart-tab smartparens solarized-theme dash org org-ac markdown-mode pandoc-mode ox-pandoc ox-reveal org-pandoc psci purescript-mode company shm paredit w3m))

;;; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

;;; install the missing packages
(dolist (package packages-to-install)
  (unless (package-installed-p package)
    (package-install package)))

;;; disable toolbar
(tool-bar-mode -1)

;;; enables gemacs to start in front of terminal multiplexor
(x-focus-frame nil)

;; line wrap everywhere
(global-visual-line-mode t)

;; automatic reverting
(global-auto-revert-mode)

;;; cyrillic layout
;; (setq set-input-method "cyrillic-yawerty")

;;; org mode
(require 'org)

;;; enable evil-mode by default
(evil-mode 1)

;;; enable line numbers
(global-linum-mode 1)

;;; enable show brackets mode
(show-paren-mode 1)

;;; show cursor's position
(column-number-mode 1)

;;; use ido mode
(require 'ido)
(ido-mode t)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(coq-compile-before-require t)
 '(custom-enabled-themes (quote (tango-dark)))
 '(custom-safe-themes
   (quote
    ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-log t)
 '(haskell-process-suggest-remove-import-lines t)
 '(haskell-process-type (quote stack-ghci))
 '(haskell-tags-on-save t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

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

;;; evil-nerd-commenter
(evilnc-default-hotkeys)

;;; ========================
;;; global keyboard bindings
;;; ========================

;;; requires 'toggle-comment
(global-set-key (kbd "M-;") 'toggle-comment)

;;; align by regexp
(global-set-key (kbd "C-x a r") 'align-regexp)

;;; switch windows by S-arrow
(windmove-default-keybindings)

;;; =====
;;; paths
;;; =====

;;; Nix bin
;; (add-to-list 'exec-path "/Users/dr/.nix-profile/bin")
;;; Opam
(add-to-list 'exec-path "/Users/dr/.opam/system/bin")
;;; Cabal
(add-to-list 'exec-path "/Users/dr/.cabal/bin")

;;; =======
;;; haskell
;;; =======

;;; -----------
;;; indentation
;;; -----------

;;; haskell indentation
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

;;; block indentation
(eval-after-load "haskell-mode"
  '(progn
     (define-key haskell-mode-map (kbd "C-,") 'haskell-move-nested-left)
     (define-key haskell-mode-map (kbd "C-.") 'haskell-move-nested-right)))

;;; ----------------
;;; interactive mode
;;; ----------------

;;; enable it
(require 'haskell-interactive-mode)
(require 'haskell-process)
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)

;;; logger and import remover


;;; handy keys
(define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-or-reload)
(define-key haskell-mode-map (kbd "C-'") 'haskell-interactive-bring)
(define-key haskell-mode-map (kbd "C-c C-t") 'haskell-process-do-type)
(define-key haskell-mode-map (kbd "C-c C-i") 'haskell-process-do-info)
(define-key haskell-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
(define-key haskell-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
(define-key haskell-mode-map (kbd "C-c c") 'haskell-process-cabal)
(define-key haskell-mode-map (kbd "C-c C-n C-i") 'haskell-navigate-imports-go)
(define-key haskell-mode-map (kbd "SPC") 'haskell-mode-contextual-space)

;;; cabal-mode keys
(define-key haskell-cabal-mode-map (kbd "C-'") 'haskell-interactive-bring)
(define-key haskell-cabal-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
(define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
(define-key haskell-cabal-mode-map (kbd "C-c c") 'haskell-process-cabal)

;;; make stack ghci default interactive shell

;;; make cabal-repl default interactive shell
;;; (custom-set-variables '(haskell-process-type 'cabal-repl))

;;; --------
;;; hasktags
;;; --------

;;; jump to definition
;;;
;;; to troubleshoot/reset use 'M-x tags-reset-tags-tables'
(define-key haskell-mode-map (kbd "C-M-.") 'haskell-mode-jump-to-def-or-tag)

;;; refresh tags on save


;;; ------------------
;;; haddock w3m viewer
;;; ------------------

;;; make w3m more passive
(setq w3m-mode-map (make-sparse-keymap))

(define-key w3m-mode-map (kbd "RET") 'w3m-view-this-url)
(define-key w3m-mode-map (kbd "q") 'bury-buffer)
(define-key w3m-mode-map (kbd "<mouse-1>") 'w3m-maybe-url)
(define-key w3m-mode-map [f5] 'w3m-reload-this-page)
(define-key w3m-mode-map (kbd "C-c C-d") 'haskell-w3m-open-haddock)
(define-key w3m-mode-map (kbd "M-<left>") 'w3m-view-previous-page)
(define-key w3m-mode-map (kbd "M-<right>") 'w3m-view-next-page)
(define-key w3m-mode-map (kbd "M-.") 'w3m-haddock-find-tag)

(defun w3m-maybe-url ()
  (interactive)
  (if (or (equal '(w3m-anchor) (get-text-property (point) 'face))
          (equal '(w3m-arrived-anchor) (get-text-property (point) 'face)))
      (w3m-view-this-url)))

;;; enable it
(require 'w3m-haddock)

;;; hook it
(add-hook 'w3m-display-hook 'w3m-haddock-display)

;;; map key
(define-key haskell-mode-map (kbd "C-c C-d") 'haskell-w3m-open-haddock)

;;; speedbar
;(speedbar-add-supported-extension ".hs")

;;; magit
(global-set-key (kbd "C-x g") 'magit-status)

;;;;;;;;;;;;;;;;;;
;;; postgresql ;;;
;;;;;;;;;;;;;;;;;;

(setq sql-connection-alist
  '((avia246-dev (sql-product 'postgres)
    (sql-port 5432)
    (sql-server "localhost")
    (sql-user "avia")
    (sql-password "password")
    (sql-database "avia24_6_development"))
  (avia246-test (sql-product 'postgres)
    (sql-port 5432)
    (sql-server "localhost")
    (sql-user "avia")
    (sql-password "password")
    (sql-database "avia24_6_test"))))

(defun sql-avia246-dev ()
  (interactive)
  (custom-sql-connect 'postgres 'avia246-dev))

(defun sql-avia246-test ()
  (interactive)
  (custom-sql-connect 'postgres 'avia246-test))

(defun custom-sql-connect (product connection)
  (interactive)
  ;; remember to set the sql-product, otherwise, it will fail for the first time
  ;; you call the function
  (setq sql-product product)
  (sql-connect connection))

(defun psql-scratch ()
  (interactive)
  (switch-to-buffer "*sql-scratch*")
  (sql-set-product "postgres")
  (sql-set-sqli-buffer)
  (sql-mode))

;;; truncate lines
(add-hook 'sql-interactive-mode-hook
  (lambda ()
    (toggle-truncate-lines t)))

;;; increase gc memory threshold to 20Mb
(setq gc-cons-threshold 20000000)

;;; enable proof general
(load-file "$HOME/.emacs.d/lib/ProofGeneral-4.2/generic/proof-site.el")

;;; proof general tag completion
(add-hook ’proof-mode-hook
     (lambda () (local-set-key ’(meta tab) ’tag-complete-symbol)))

;;; john wigley coq setup ripoff
(defcustom coq-use-Case t
  "Whether to use the Case hack for constructor alternatives."
  :type 'boolean
  :group 'coq-config)

(defcustom coq-use-bullets t
  "Whether to use bullets for scoping constructor alternatives."
  :type 'boolean
  :group 'coq-config)

(defun coq-insert-induction (name pos)
  "Given the name of a variable in scope, insert induction cases for it."
  (interactive "sInduction over term: ")
  (proof-shell-ready-prover)
  (let* ((leader
          (save-excursion
            (beginning-of-line)
            (let ((beg (point)))
              (skip-syntax-forward " ")
              (- (point) beg))))
         (thetype
          (ignore-errors
            (with-temp-buffer
              (insert (proof-shell-invisible-cmd-get-result
                       (concat "Check " name ".")))
              (goto-char (point-max))
              (skip-syntax-backward " ")
              (delete-region (point) (point-max))
              (search-backward " : ")
              (delete-region (point-min) (match-end 0))
              (goto-char (point-min))
              (forward-word 1)
              (buffer-substring (point-min) (point)))))
         (indstr
          (ignore-errors
            (with-temp-buffer
              (insert (proof-shell-invisible-cmd-get-result
                       (concat "Show Match " thetype ".")))
              (goto-char (point-min))
              (let (ctors)
                (while (re-search-forward "| \\(.+?\\) =>" nil t)
                  (push (match-string 1) ctors))
                (goto-char (point-min))
                (re-search-forward "| \\S-+ ")
                (delete-region (point-min) (point))
                (insert "[")
                (while (re-search-forward "=>\\(.\\|\n\\)+?| \\S-+ " nil t)
                  (replace-match "|"))
                (goto-char (point-max))
                (search-backward "=>" nil t)
                (delete-region (point) (point-max))
                (insert "].")
                (mapc #'(lambda (x)
                          (insert ?\n (make-string leader ? ))
                          (when coq-use-bullets
                            (insert "- "))
                          (when coq-use-Case
                            (insert (format "Case \"%s = %s\"." name x))))
                      (nreverse ctors))
                (buffer-string))))))
    indstr))
;;; remove trailing spaces
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;; winner(undo last window split/unsplit)
;;; C-c <left> and C-C <right> - undo/redo
(winner-mode 1)

;;; visual line length aid
(require 'whitespace)
(setq whitespace-style '(face empty tabs lines-tail trailing))
(global-whitespace-mode t)
