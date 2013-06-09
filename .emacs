;;------------------------------------------------------------------------------
(setq debug-on-error t)
(setq gc-cons-threshold 20000000)

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

;; server
(server-start)

;;------------------------------------------------------------------------------
(when (eq system-type 'windows-nt)
  (setenv "http_proxy" "http://proxy.zte.com.cn:80")
  (setenv "https_proxy" "http://proxy.zte.com.cn:80")
  ;; (setenv "no_proxy" "10.*,*.zte.com.cn")
  (setq url-proxy-services '(("no_proxy" . "10.*\\|zte.com.cn")
                             ("http" . "proxy.zte.com.cn:80")
                             ("https" . "proxy.zte.com.cn:80")))

  ;; (when window-system
  ;;   (set-frame-font "Dejavu Sans Mono-10")
  ;;   (set-fontset-font (frame-parameter nil 'font)
  ;;                     'symbol '("Arial Unicode MS" . "unicode-bmp")))

  ;; git
  (setenv "GIT_AUTHOR_NAME" "方涛10034491")
  (setenv "GIT_AUTHOR_EMAIL" "10034491@zte.com.cn")
  (setenv "GIT_COMMITTER_NAME" (getenv "GIT_AUTHOR_NAME"))
  (setenv "GIT_COMMITTER_EMAIL" (getenv "GIT_AUTHOR_EMAIL"))

  ;; git bug: https://github.com/magit/magit/issues/255
  (setenv "GIT_ASKPASS" "git-gui--askpass")

  ;; GIT_SSL_NO_VERIFY=true
  ;;(setenv "GIT_SSL_NO_VERIFY" "true")
  )

(when (eq system-type 'gnu/linux)
  ;; (when window-system
  ;;   (set-frame-font "Bitstream Vera Sans Mono-10")
  ;;   ;; see fontset.el
  ;;   (set-fontset-font (frame-parameter nil 'font)
  ;;                     'ascii '("Bitstream Vera Sans Mono" . "unicode-bmp"))
  ;;   (set-fontset-font (frame-parameter nil 'font)
  ;;                     'han '("WenQuanYi Bitmap Song" . "unicode-bmp"))
  ;;   (set-fontset-font (frame-parameter nil 'font)
  ;;                     'symbol '("WenQuanYi Bitmap Song" . "unicode-bmp"))
  ;;   ;; cjk
  ;;   (set-fontset-font (frame-parameter nil 'font)
  ;;                     'cjk-misc '("WenQuanYi Bitmap Song" . "unicode-bmp"))
  ;;   ;; japanese
  ;;   (set-fontset-font (frame-parameter nil 'font)
  ;;                     'kana '("IPAMonaPGothic" . "unicode-bmp"))
  ;;   ;; korea
  ;;   (set-fontset-font (frame-parameter nil 'font)
  ;;                     'hangul '("UnDotum" . "unicode-bmp")))

  ;; git
  (setenv "GIT_AUTHOR_NAME" "Tao Fang")
  (setenv "GIT_AUTHOR_EMAIL" "fangtao0901@gmail.com")
  (setenv "GIT_COMMITTER_NAME" (getenv "GIT_AUTHOR_NAME"))
  (setenv "GIT_COMMITTER_EMAIL" (getenv "GIT_AUTHOR_EMAIL"))
  ;; GIT_SSL_NO_VERIFY=true
  ;;(setenv "GIT_SSL_NO_VERIFY" "true")
  )

(setq
 ;; using trash bin
 delete-by-moving-to-trash t
 ;; don't show annoing startup msg
 inhibit-startup-message t
 ;; see what you type in echo area, 0 for nerver, -1 for immediately, other for seconds
 echo-keystrokes -1
 ;; text scrolling
 ;; scroll one line at a time (less "jumpy" than defaults)
 ;; scroll: 3 or 10(+ctrl)
 mouse-wheel-scroll-amount '(3 ((control) . 10)) ;; one line at a time
 mouse-wheel-progressive-speed nil ;; don't accelerate scrolling
 scroll-step 1 ;; keyboard scroll one line at a time
 auto-window-vscroll nil
 scroll-preserve-screen-position 'always
 ;; number of chars in line
 fill-column 80
 ;; delete line in one stage
 kill-whole-line t
 kill-ring-max 200
 ;; default mode
 major-mode 'text-mode
 ;; delete excess backup versions
 delete-old-versions t
 ;; this will take sure spaces will used instead of tabs
 indent-tabs-mode nil
 ;; paste at cursor NOT at mouse pointer position
 mouse-yank-at-point t
 ;; NO annoing backups
 make-backup-files nil
 cursor-in-non-selected-windows nil
 size-indication-mode t
 resize-mini-windows nil
 track-eol t
 auto-image-file-mode t
 ;; windows keyboard settings
 w32-pass-lwindow-to-system nil
 w32-pass-rwindow-to-system nil
 w32-pass-apps-to-system nil
 w32-lwindow-modifier 'super ;; Left Windows key
 w32-rwindow-modifier 'super ;; Right Windows key
 w32-apps-modifier 'hyper    ;; Menu key
 ;; Title formatting
 frame-title-format (list '(buffer-file-name "%f" "%b") " - GNU Emacs " emacs-version "@" system-name )
 icon-title-format frame-title-format
 ;; dired settings
 dired-recursive-copies t
 dired-recursive-deletes t
 )

;; menu toolbar scrollbar
(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)

;; desktop
(desktop-save-mode 1)
(setq desktop-save t)

;; maximize frame
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(toggle-frame-maximized)

;; 24.1 introduced
(electric-indent-mode +1)

(delete-selection-mode 1)

(mouse-avoidance-mode 'animate)

(which-function-mode 0)

;; case INsensitive search
(setq-default case-fold-search t)

;; setting the default tabulation
(setq-default tab-width 4)

;; this will take sure spaces will used instead of tabs
(setq-default indent-tabs-mode nil)

;; Make all "yes or no" prompts show "y or n" instead
(fset 'yes-or-no-p 'y-or-n-p)

;; M-: doc string show in mode-line
(add-hook 'eval-expression-minibuffer-setup-hook #'eldoc-mode)

;; udpate timestamp
(add-hook 'before-save-hook 'time-stamp)

;; highlight paren
(show-paren-mode t)

;; parenthesis, expression, mixed
(setq show-paren-style 'mixed)

;; show column number
(setq column-number-mode t)

;; syntax highlight
(cond ((fboundp 'global-font-lock-mode)
       ;; Turn on font-lock in all modes that support it
       (global-font-lock-mode t)
       (setq font-lock-maximum-decoration t)))

;; syntax
(add-hook 'c-mode-common-hook
          (lambda ()
            (when (derived-mode-p 'c-mode 'c++-mode)
              (modify-syntax-entry ?_ "w" c-mode-syntax-table)
              (modify-syntax-entry ?_ "w" c++-mode-syntax-table))
            ))
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (modify-syntax-entry ?_ "w" emacs-lisp-mode-syntax-table)
            (modify-syntax-entry ?- "w" emacs-lisp-mode-syntax-table)
            ))
;; bookmark C-x r m/C-x r b/list-bookmarks
(setq bookmark-save-flag 1)

;; time settings
(display-time)
(setq display-time-default-load-average nil)
(setq display-time-use-mail-icon t)
(setq display-time-interval 10)
(setq display-time-format "%m/%d/%A %H:%M")

;; uniquify for buffer names
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward)

;; keybindings
(global-set-key (kbd "<f1>") 'revert-buffer)
(global-set-key (kbd "<f10>") 'jump-to-register)
(global-set-key (kbd "<f11>") 'bookmark-set)
(global-set-key (kbd "<f12>") 'list-bookmarks)
(global-set-key (kbd "<C-return>") 'rectangle-mark-mode)

(global-set-key (kbd "C-z") 'eshell)

(global-set-key [(meta g)] 'goto-line)
(global-set-key (kbd "M-SPC") 'set-mark-command)
(global-set-key (kbd "C-M-'") 'comment-or-uncomment-region)
(global-set-key (kbd "C-x C-k") 'kill-this-buffer)

;; C-o switch windows
(global-set-key (kbd "C-o") (lambda () (interactive) (switch-to-buffer nil)))

;; ibuffer
(require 'ibuffer)
(global-set-key (kbd "<f5>") 'ibuffer)

;; for dired
(require 'dired)
(require 'dired-x)
(global-set-key (kbd "C-x C-j") 'dired-jump)
(global-set-key (kbd "C-x 4 C-j") 'dired-jump-other-window)

(defun ibuffer-ediff-marked-buffers ()
  (interactive)
  (let* ((marked-buffers (ibuffer-get-marked-buffers))
         (len (length marked-buffers)))
    (unless (= 2 len)
      (error (format "%s buffer%s been marked (needs to be 2)"
                     len (if (= len 1) " has" "s have"))))
    (ediff-buffers (car marked-buffers) (cadr marked-buffers))))
;; beyond comp
(defun ibuffer-bc-marked-buffers ()
  (interactive)
  (let* ((marked-buffers (ibuffer-get-marked-buffers))
         (len (length marked-buffers)))
    (unless (= 2 len)
      (error (format "%s buffer%s been marked (needs to be 2)"
                     len (if (= len 1) " has" "s have"))))
    (call-process "bcomp.exe" nil nil nil
                  (uniquify-buffer-file-name (car marked-buffers))
                  (uniquify-buffer-file-name (cadr marked-buffers)))
    ))
(setq ibuffer-saved-filter-groups
      '(("home"
         ("magit" (name . "\*magit"))
         ("twitter" (mode . twittering-mode))
         ("weibo" (name . "\*weibo"))
         ("ERC" (mode . erc-mode))
         ("WWW" (mode . w3-mode))
         ("eshell" (mode . eshell-mode))
         ("emacs-config" (or (filename . ".emacs")
                             (filename . "MyEmacs")
                             (mode . lisp-mode)
                             (mode . emacs-lisp-mode)))
         ("U31 svn" (filename . "U31_svn"))
         ("log" (or (filename . ".log")
                    (filename . ".val")))
         ("sql" (filename . ".sql"))
         ("xml files" (filename . "xml"))
         ("dired" (mode . dired-mode))
         ("Org" (or (mode . org-mode)
                    (filename . "orgfiles")))
         ("image" (mode . image-mode))
         ("Help" (or (name . "\*Help\*")
                     (name . "\*Apropos\*")
                     (name . "\*info\*")))
         ("Misc" (name . "\*.*\*")))))

(add-hook 'ibuffer-mode-hook
          (lambda ()
            (define-key ibuffer-mode-map (kbd "e") 'ibuffer-ediff-marked-buffers)
            (define-key ibuffer-mode-map (kbd "C-q") 'ibuffer-bc-marked-buffers)
            (define-key ibuffer-mode-map (kbd "C-o") (lambda () (interactive) (switch-to-buffer nil)))
            (setq ibuffer-expert t)
            (setq ibuffer-show-empty-filter-groups nil)
            (hl-line-mode t)
            (ibuffer-switch-to-saved-filter-groups "home")
            (ibuffer-auto-mode 1)
            ))
(defun dired-ediff-marked-buffers ()
  (interactive)
  (let* ((marked-buffers (dired-get-marked-files))
         (len (length marked-buffers)))
    (unless (= 2 len)
      (error (format "%s buffer%s been marked (needs to be 2)"
                     len (if (= len 1) " has" "s have"))))
    (ediff-files (car marked-buffers) (cadr marked-buffers))))
(add-hook 'dired-mode-hook
          (lambda ()
            (define-key dired-mode-map (kbd "=") 'dired-ediff-marked-buffers)
            (define-key dired-mode-map (kbd "C-o") (lambda () (interactive) (switch-to-buffer nil)))
            (define-key dired-mode-map [f2] 'wdired-change-to-wdired-mode)
            ;; absolute path copy
            (define-key dired-mode-map (kbd "W") (lambda () (interactive) (dired-copy-filename-as-kill 0)))
            ))
;; wdired
(require 'wdired)
(add-hook 'wdired-mode-hook
          (lambda ()
            (define-key wdired-mode-map [f2] 'wdired-abort-changes)))

;; C++
(add-to-list 'auto-mode-alist '("\\.h$" . c++-mode))
(defconst my-c-style
  '((c-tab-always-indent . t)
    (c-basic-offset . 4)
    (c-comment-only-line-offset . 0)
    (c-hanging-braces-alist . ((substatement-open after)
                               (brace-list-open)))
    (c-hanging-colons-alist . ((member-init-intro before)
                               (inher-intro)
                               (case-label after)
                               (label after)
                               (access-label after)))
    (c-cleanup-list . (scope-operator
                       empty-defun-braces
                       defun-close-semi))
    (c-offsets-alist . ((arglist-close . c-lineup-arglist)
                        (substatement-open . 0)
                        (case-label . 4)
                        (block-open . 0)
                        (comment-intro . 0)
                        (knr-argdecl-intro . -)
                        (inclass +)
                        (inline-open 0)
                        (friend -)
                        (substatement-open 0)
                        (arglist-intro +)
                        (member-init-intro . ++)
                        ))
    (c-echo-syntactic-information-p . t)
    )
  "My C programming style")
(add-hook 'c-mode-common-hook
          (lambda ()
            ;; add my personal style and set it for the current buffer
            (c-add-style "PERSONAL" my-c-style t)
            ;; we will like auto-newline and hungry-delete
            (c-toggle-auto-hungry-state 1)
            (define-key c-mode-base-map (kbd "C-m") 'c-context-line-break)
            ))
;; hideshow
(require 'hideshow)
(add-hook 'prog-mode-hook
          (lambda ()
            (hs-minor-mode)))

;; hilit-chg
(require 'hilit-chg)
(global-highlight-changes-mode)
(setq highlight-changes-visibility-initial-state nil)
(global-set-key (kbd "<f6>") 'highlight-changes-visible-mode)
(global-set-key (kbd "S-<f6>") 'highlight-changes-remove-highlight)
(global-set-key (kbd "M-<f4>") 'highlight-changes-previous-change)
(global-set-key (kbd "M-<f3>") 'highlight-changes-next-change)

;; IRC client
(require `erc)
(add-hook 'erc-mode-hook
          (lambda ()
            (when (eq system-type 'windows-nt)
              ;; proxy settings
              (setq socks-server '("Default Server" "localhost" 1080 5))
              (setq socks-noproxy '("localhost"))
              (require 'socks)
              (setq erc-server-connect-function 'socks-open-network-stream)
              ;; then start:
              ;;     desproxy-socks5server.exe proxy.zte.com.cn 80 1080
              )
            (define-key erc-mode-map (kbd "C-a")
              (lambda ()
                (interactive)
                (let ((p (point)))
                  (erc-bol)
                  (if (= p (point)) (beginning-of-line)))))
            ;; show channel name
            (setq erc-prompt (lambda ()
                               (if (and (boundp 'erc-default-recipients) (erc-default-target))
                                   (erc-propertize (concat (erc-default-target) ">") 'read-only t 'rear-nonsticky t 'front-nonsticky t)
                                 (erc-propertize (concat "ERC>") 'read-only t 'rear-nonsticky t 'front-nonsticky t))))
            ;; show url
            (setq erc-button-url-regexp
                  "\\([-a-zA-Z0-9_=!?#$@~`%&*+\\/:;,]+\\.\\)+[-a-zA-Z0-9_=!?#$@~`%&*+\\/:;,]*[-a-zA-Z0-9\\/]")
            ;; ErcAutoJoin
            ;;(erc :server "irc.freenode.net" :port 6667 :nick "ft")
            (setq erc-autojoin-channels-alist
                  '(("freenode.net" "#emacs" "#emacs-cn" "##linux" "##windows")))

            (eval-after-load 'erc-track
              '(progn
                 (defun erc-bar-move-back (n)
                   "Moves back n message lines. Ignores wrapping, and server messages."
                   (interactive "nHow many lines ? ")
                   (re-search-backward "^.*<.*>" nil t n))

                 (defun erc-bar-update-overlay ()
                   "Update the overlay for current buffer, based on the content of "
                   "erc-modified-channels-alist. Should be executed on window change."
                   (interactive)
                   (let* ((info (assq (current-buffer) erc-modified-channels-alist))
                          (count (cadr info)))
                     (if (and info (> count erc-bar-threshold))
                         (save-excursion
                           (end-of-buffer)
                           (when (erc-bar-move-back count)
                             (let ((inhibit-field-text-motion t))
                               (move-overlay erc-bar-overlay
                                             (line-beginning-position)
                                             (line-end-position)
                                             (current-buffer)))))
                       (delete-overlay erc-bar-overlay))))

                 (defvar erc-bar-threshold 1
                   "Display bar when there are more than erc-bar-threshold unread messages.")
                 (defvar erc-bar-overlay nil
                   "Overlay used to set bar")
                 (setq erc-bar-overlay (make-overlay 0 0))
                 (overlay-put erc-bar-overlay 'face '(:underline "black"))
                 ;;put the hook before erc-modified-channels-update
                 (defadvice erc-track-mode (after erc-bar-setup-hook
                                                  (&rest args) activate)
                   ;;remove and add, so we know it's in the first place
                   (remove-hook 'window-configuration-change-hook 'erc-bar-update-overlay)
                   (add-hook 'window-configuration-change-hook 'erc-bar-update-overlay))
                 (add-hook 'erc-send-completed-hook (lambda (str)
                                                      (erc-bar-update-overlay)))
                 ))))
;;------------------------------------------------------------------------------

;;------------------------------------------------------------------------------
;;
;; extensions
;;
(when (eq system-type 'windows-nt)
  ;; asn1
  (add-to-list 'load-path "~/MyEmacs/Extensions/asn1-mode")
  (autoload 'asn1-mode "asn1-mode" "Major mode for editing ASN.1 specifications." t)
  (add-to-list 'auto-mode-alist '("\\.[Aa][Ss][Nn]1?$" . asn1-mode))

  ;; No Gnus
  (add-to-list 'load-path "~/MyEmacs/Extensions/gnus/lisp")
  (add-to-list 'Info-default-directory-list "~/MyEmacs/Extensions/gnus/texi/")
  (require 'gnus-load)

  ;; ;; Sina weibo
  ;; (add-to-list 'load-path "~/MyEmacs/Extensions/weibo.emacs/")
  ;; (require 'weibo)
  ;; (add-hook 'weibo-timeline-mode-hook
  ;;           (lambda ()
  ;;             (define-key weibo-timeline-mode-map (kbd "<backtab>") 'backward-button)
  ;;             ))
  ;; (add-hook 'weibo-image-mode-hook
  ;;           (lambda ()
  ;;             (define-key weibo-image-mode-map (kbd "n") 'image-next-line)
  ;;             (define-key weibo-image-mode-map (kbd "p") 'image-previous-line)
  ;;             ))

  ;; emacs-db
  (add-to-list 'load-path "~/MyEmacs/Extensions/edb/lisp")
  (autoload 'edb-interact "database" "Emacs Database" t)
  (autoload 'db-find-file "database" "Emacs Database" t)

  (add-to-list 'load-path "~/MyEmacs/Extensions")
  ;; clearcase
  (require 'clearcase)
  ;; cdb-gud
  (require 'cdb-gud)
  (add-hook 'gud-mode-hook
            (lambda ()
              (define-key gud-mode-map (kbd "<f5>") 'gud-cont)
              (define-key gud-mode-map (kbd "<f10>") 'gud-next)
              (define-key gud-mode-map (kbd "<f11>") 'gud-step)
              (define-key gud-mode-map (kbd "S-<f11>") 'gud-finish)
              (define-key gud-mode-map (kbd "<f9>") 'gud-break)))
  ;; mpg123
  (require 'mpg123)

  )
;;------------------------------------------------------------------------------

;;------------------------------------------------------------------------------
;;
;; package
;;
(require 'package)
(setq package-user-dir "~/MyEmacs/elpa")
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("ELPA" . "http://tromey.com/elpa/"))
;;(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(package-initialize)

;; use-package
(require 'use-package)

(setq use-package-always-ensure t)

;; chinese-font-setup
(use-package chinese-fonts-setup
  :config (setq cfs-profiles
                '("program" "org-mode" "read-book")))

;; paradox
(use-package paradox
  :defer t
  :config (setq paradox-github-token (funcall (plist-get (nth 0 (auth-source-search :host "paradox-github")) :secret))
                paradox-automatically-star t))

;; paredit
(use-package paredit)

;; magit
(use-package magit)

;; git gutter fringer
(use-package git-gutter-fringe)

;; psvn
(use-package psvn)

;; avy
(use-package avy
  :bind ("C-'" . avy-goto-word-or-subword-1))

(use-package ace-window
  :config (custom-set-faces '(aw-leading-char-face ((t (:inherit ace-jump-face-foreground :height 3.0)))))
  :bind ("C-x o" . ace-window))

(use-package window-numbering
  :config (window-numbering-mode t))

;; easy-kill
(use-package easy-kill
  :config (global-set-key [remap kill-ring-save] 'easy-kill))
(use-package easy-kill-extras)

;; expand-region
(use-package expand-region
  :bind ("C-=" . er/expand-region))

;; Microsoft Visual Studio bookmark
(use-package bm
  :bind (("<C-f2>" . bm-toggle)
         ("<f2>" . bm-next)
         ("<S-f2>" . bm-previous)))

;; chinese calendar
(use-package cal-china-x
  :disabled t
  :config (setq mark-holidays-in-calendar t
                cal-china-x-important-holidays cal-china-x-chinese-holidays
                calendar-holidays cal-china-x-important-holidays))

;; popwin
(use-package popwin
  :config (popwin-mode 1))

;; which-key-mode
(use-package which-key
  :config (which-key-mode))

;; youdao
(use-package youdao-dictionary
  :config (progn (setq url-automatic-caching t)
                 (push "*Youdao Dictionary*" popwin:special-display-config))
  :bind (("<M-f1>" . youdao-dictionary-search-at-point)
         ("<M-f2>" . youdao-dictionary-search-from-input)))

;; smooth-scrolling
(use-package smooth-scrolling
  :config (setq smooth-scroll-margin 3))

;; yascroll
(use-package yascroll
  :config (global-yascroll-bar-mode))

;; back button
(use-package back-button
  :diminish back-button-mode
  :config (back-button-mode 1))

;; dired+.el
(use-package dired+)
(use-package dired-filter)

;; color-theme
(use-package material-theme
  :config (load-theme 'material t))

;; browse-kill-ring
(use-package browse-kill-ring
  :bind ("C-c k" . browse-kill-ring))

;; hl-line+
(use-package hl-line+
  :config (progn
            ;;(global-hl-line-mode t)
            ;;(toggle-hl-line-when-idle t)
            (make-face 'my-hl-line-face)
            (setq hl-line-face 'my-hl-line-face)
            (face-spec-set 'my-hl-line-face '((t (
                                                  ;; :background "snow4"
                                                  ;; :bold
                                                  ;; :weight nil
                                                  :inverse-video t
                                                                 ))))))

;; nyan-cat
(use-package nyan-prompt
  :config (add-hook 'eshell-mode-hook 'nyan-prompt-enable))

;; nyan-mode
(use-package nyan-mode)

;; rainbow
(use-package rainbow-mode)

;; eshell fringe status
(use-package eshell-fringe-status
  :config (add-hook 'eshell-mode-hook #'eshell-fringe-status-mode))

;; redo+
(use-package redo+
  :bind (("C-." . redo)
         ("C-/" . undo)))

;; auto-complete
(use-package auto-complete
  :diminish auto-complete-mode
  :config (progn (setq ac-use-menu-map t)
                 (setq-default ac-sources '(ac-source-words-in-same-mode-buffers))
                 ;; 设置不自动启动
                 (setq ac-auto-start nil)
                 ;; 设置响应时间 0.5
                 (setq ac-quick-help-delay 0.05)
                 (bind-key "M-/" 'auto-complete ac-mode-map)
                 (add-hook 'emacs-lisp-mode-hook 'ac-emacs-lisp-mode-setup)
                 (add-hook 'c-mode-common-hook 'ac-cc-mode-setup)
                 (add-hook 'auto-complete-mode-hook 'ac-common-setup)
                 (ac-config-default)
                 (global-auto-complete-mode t)))

;; tooltip
(use-package pos-tip)

;; fold-dwim
(use-package fold-dwim
  :commands (fold-dwim-toggle fold-dwim-hide-all fold-dwim-show-all)
  :bind (("<f7>" . fold-dwim-toggle)
         ("<M-f7>" . fold-dwim-hide-all)
         ("<S-M-f7>" . fold-dwim-show-all)))
(use-package fold-dwim-org)

;; vimrc
(use-package vimrc-mode
  :mode (".vim\\(rc\\)?$" . vimrc-mode))

;; cscope.exe
(use-package xcscope
  :config (cscope-setup))
(when (eq system-type 'windows-nt)
  ;; xscope+
  (require 'xcscope+)
  (setq cscope-database-file "d:/U31_svn/U31_E2E/trunk/cscope.out")
  (cscope-set-initial-directory "d:/U31_svn/U31_E2E/trunk")
  )

;; ggtags
(use-package ggtags
  :diminish ggtags-mode
  :config (add-hook 'c-mode-common-hook (lambda()
                                          (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
                                            (ggtags-mode 1)))))

;; eval-sexp-fu
(use-package eval-sexp-fu
  :config (progn (setq eval-sexp-fu-flash-duration 0.4)
                 (bind-keys :map lisp-interaction-mode-map
                            ("C-c C-c" 'eval-sexp-fu-eval-sexp-inner-list)
                            ("C-c C-e" 'eval-sexp-fu-eval-sexp-inner-sexp))
                 (bind-keys :map emacs-lisp-mode-map
                            ("C-c C-c" 'eval-sexp-fu-eval-sexp-inner-list)
                            ("C-c C-e" 'eval-sexp-fu-eval-sexp-inner-sexp))))

;; FIXME TODO BUG KLUDGE
(use-package fic-mode
  :diminish fic-mode
  :config (add-hook 'prog-mode-hook 'turn-on-fic-mode))

;; haskell
(use-package ghc
  :config (progn
            ;;(setq ghc-interactive-command "ghci")
            (add-hook 'haskell-mode-hook (lambda ()
                                           (ghc-init)
                                           (flycheck-mode)
                                           (setq haskell-interactive-popup-errors nil)
                                           (turn-on-haskell-doc-mode)
                                           (turn-on-haskell-indentation)
                                           (interactive-haskell-mode)
                                           ))))
(use-package ghci-completion)

;; ac
(use-package ac-haskell-process
  :config (add-hook 'interactive-haskell-mode-hook 'ac-haskell-process-setup))

;; shm
(use-package shm
  :config (add-hook 'haskell-mode-hook 'structured-haskell-mode))

;; flycheck-haskell
(use-package flycheck-haskell)

;; yasnippet
(use-package yasnippet
  :diminish yas-minor-mode
  :config (yas-global-mode 1)
  :config (progn
            (use-package dropdown-list)
            (setq yas-prompt-functions '(yas-dropdown-prompt))
            ))

;; header2
(use-package header2
  :config (progn (add-hook 'write-file-hooks 'auto-update-file-header)
                 (add-hook 'c-mode-common-hook   'auto-make-header)
                 (setq header-date-format "%x %A %T")
                 (setq make-header-hook '(header-mode-line
                                          header-copyright
                                          header-blank
                                          header-description
                                          header-blank
                                          header-author
                                          header-creation-date
                                          header-modification-date
                                          header-modification-author
                                          header-blank
                                          header-update-count
                                          header-blank
                                          header-history
                                          header-blank))))

;; modify mode-line
(use-package powerline
  :config (progn
            (setq powerline-default-separator 'arrow)
            (powerline-default-theme)
            ;; get coloring of arrows correct
            ;;(add-hook 'window-setup-hook 'powerline-reset)
            ))

;; highlight-symbol
(use-package highlight-symbol
  :bind (("<C-f3>" . highlight-symbol)
         ("<f3>" . highlight-symbol-next)
         ("<S-f3>" . highlight-symbol-prev)
         ("<f4>" . highlight-symbol-prev)
         ("<C-f4>" . highlight-symbol-query-replace)))

;; auto highlight-symbol
(use-package auto-highlight-symbol
  :diminish auto-highlight-symbol-mode
  :config (global-auto-highlight-symbol-mode))

;; volatile-highlights
(use-package volatile-highlights
  :config (volatile-highlights-mode t))

;; cursor-chg
;; win32 cause toggle-frame-fullscreen weird
;; (use-package cursor-chg
;;   :config (progn (change-cursor-mode 1)
;;                  (toggle-cursor-type-when-idle 1)))

(use-package multiple-cursors
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-!" . mc/mark-all-like-this))
  :config (progn (set-face-attribute 'mc/cursor-face nil :foreground "red" :background "forest green")
                 (set-face-attribute 'mc/region-face nil :background "red")))

(use-package phi-search
  :config (progn (global-set-key [remap isearch-forward] 'phi-search)
                 (global-set-key [remap isearch-backward] 'phi-search-backward)
                 (global-set-key [remap query-replace] 'phi-replace-query)
                 (set-face-attribute 'phi-search-selection-face nil :background "red")
                 (set-face-attribute 'phi-search-match-face nil :background "forest green")))

(use-package phi-search-mc
  :config (progn (phi-search-mc/setup-keys)
                 (add-hook 'isearch-mode-hook 'phi-search-from-isearch-mc/setup-keys)))

;; Iedit
(use-package iedit)

(use-package fuzzy)

(use-package ace-jump-zap
  :bind (("M-z" . ace-jump-zap-up-to-char-dwim)
         ("C-M-z" . ace-jump-zap-to-char-dwim)))

;; helm
(use-package helm
  :config (progn (setq helm-suspend-update-flag nil)
                 (helm-mode 1)
                 (setq helm-quick-update t
                       helm-idle-delay 0.01
                       helm-input-idle-delay 0.01)
                 (global-set-key [remap execute-extended-command] 'helm-M-x)
                 (global-set-key (kbd "M-y") 'helm-show-kill-ring)
                 (global-set-key (kbd "C-x m") 'helm-all-mark-rings)
                 (global-set-key (kbd "C-x b") 'helm-mini)
                 (setq helm-buffers-fuzzy-matching t
                       helm-recentf-fuzzy-match    t)
                 (global-set-key (kbd "C-x C-f") 'helm-find-files)
                 ))

(use-package helm-flx
  :config (helm-flx-mode +1))
(use-package helm-fuzzier
  :config (helm-fuzzier-mode 1))

(use-package helm-descbinds)

;; grep+
(use-package grep+)

;; wgrep
(use-package wgrep-helm)

;; ack
(use-package ack
  :config (setq ack-command "perl ~/tools/ack.pl "))
;; ack-menu
(use-package ack-menu)
;; helm-ack
(use-package helm-ack
  :config (setq helm-ack-base-command ack-command))

;; ag
(use-package ag)
;; helm-ag
(use-package helm-ag)
;; wgrep-ag
(use-package wgrep-ag)

;; pt
(use-package pt)
(use-package helm-pt)

;; helm-swoop
(use-package helm-swoop
  :config (progn
            (bind-key "M-o" 'helm-swoop)
            (bind-key "M-O" 'helm-swoop-back-to-last-point)
            (bind-key "C-c M-o" 'helm-multi-swoop)
            (bind-key "C-x M-o" 'helm-multi-swoop-all)
            (bind-key "M-o" 'helm-swoop dired-mode-map)
            (bind-key "M-o" 'helm-swoop-from-isearch isearch-mode-map)
            (bind-key "M-o" 'helm-multi-swoop-all-from-helm-swoop helm-swoop-map)))

;; swiper-helm
(use-package swiper-helm
  :bind ("M-i" . swiper-helm)
  )

(use-package org
  :bind (("C-c l" . org-store-link)
         ("C-c c" . org-capture)
         ("C-c a" . org-agenda)
         ("C-c b" . org-iswitchb))
  :config (progn
            (add-hook 'org-load-hook
                      (lambda ()
                        (define-key 'org-mode-map [f8] 'org-publish)))
            (setq org-agenda-custom-commands
                  '(("f" occur-tree "FIXME")))
            (setq org-log-done 'time)
            (setq org-enforce-todo-dependencies t)
            (setq org-enforce-todo-checkbox-dependencies t)
            (setq org-agenda-files (list "~/MyEmacs/orgfiles/work.org"
                                         "~/MyEmacs/orgfiles/home.org"))
            (setq org-publish-project-alist
                  '(("note-org"
                     :base-directory "~/MyEmacs/orgfiles"
                     :publishing-directory "~/MyEmacs/orgfiles_publish"
                     :base-extension "org"
                     :recursive t
                     :publishing-function org-publish-org-to-html
                     :auto-index t
                     :index-filename "index.org"
                     :index-title "index"
                     :link-home "index.html"
                     :section-numbers nil
                     :style "<link rel=\"stylesheet\"
                href=\"./emacs.css\"
                type=\"text/css\"/>"
                     )
                    ("note-static"
                     :base-directory "~/MyEmacs/orgfiles"
                     :publishing-directory "~/MyEmacs/orgfiles_publish"
                     :recursive t
                     :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|swf\\|zip\\|gz\\|txt\\|el"
                     :publishing-function org-publish-attachment
                     )
                    ("note"
                     :components ("note-org" "note-static")
                     :author "fang.tao@zte.com.cn"
                     )
                    ))

            (setq org-todo-keywords
                  (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
                          (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "PHONE" "MEETING"))))

            (setq org-todo-keyword-faces
                  (quote (("TODO" :foreground "red" :weight bold)
                          ("NEXT" :foreground "blue" :weight bold)
                          ("DONE" :foreground "forest green" :weight bold)
                          ("WAITING" :foreground "orange" :weight bold)
                          ("HOLD" :foreground "magenta" :weight bold)
                          ("CANCELLED" :foreground "forest green" :weight bold)
                          ("MEETING" :foreground "forest green" :weight bold)
                          ("PHONE" :foreground "forest green" :weight bold))))

            (setq org-todo-state-tags-triggers
                  (quote (("CANCELLED" ("CANCELLED" . t))
                          ("WAITING" ("WAITING" . t))
                          ("HOLD" ("WAITING") ("HOLD" . t))
                          (done ("WAITING") ("HOLD"))
                          ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
                          ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
                          ("DONE" ("WAITING") ("CANCELLED") ("HOLD")))))

            (setq org-use-fast-todo-selection t)
            (setq org-treat-S-cursor-todo-selection-as-state-change nil)

            (setq org-directory "~/MyEmacs/orgfiles")
            (setq org-default-notes-file "~/MyEmacs/orgfiles/refile.org")

            ;; Capture templates for: TODO tasks, Notes, appointments, phone calls, meetings, and org-protocol
            (setq org-capture-templates
                  (quote (("t" "todo" entry (file "~/MyEmacs/orgfiles/refile.org")
                           "* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t)
                          ("r" "respond" entry (file "~/MyEmacs/orgfiles/refile.org")
                           "* NEXT Respond to %:from on %:subject\nSCHEDULED: %t\n%U\n%a\n" :clock-in t :clock-resume t :immediate-finish t)
                          ("n" "note" entry (file "~/MyEmacs/orgfiles/refile.org")
                           "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)
                          ("j" "Journal" entry (file+datetree "~/MyEmacs/orgfiles/diary.org")
                           "* %?\n%U\n" :clock-in t :clock-resume t)
                          ("w" "org-protocol" entry (file "~/MyEmacs/orgfiles/refile.org")
                           "* TODO Review %c\n%U\n" :immediate-finish t)
                          ("m" "Meeting" entry (file "~/MyEmacs/orgfiles/refile.org")
                           "* MEETING with %? :MEETING:\n%U" :clock-in t :clock-resume t)
                          ("p" "Phone call" entry (file "~/MyEmacs/orgfiles/refile.org")
                           "* PHONE %? :PHONE:\n%U" :clock-in t :clock-resume t)
                          ("h" "Habit" entry (file "~/MyEmacs/orgfiles/refile.org")
                           "* NEXT %?\n%U\n%a\nSCHEDULED: %(format-time-string \"<%Y-%m-%d %a .+1d/3d>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n"))))
            ))

;; org2blog
(use-package org2blog
  :config (setq org2blog/wp-blog-alist '(
                                         ("wp-lo2net" :url "https://wp-lo2net.rhcloud.com/xmlrpc.php"
                                          :username "lo2net")
                                         )))

;; org-toc
(use-package org-plus-contrib
  :config (progn (require 'org-toc)
                 (if (eq system-type 'windows-nt)
                     (progn (require 'ox-s5)
                       (setq org-s5-ui-url "file://d:/home/fangtao/MyEmacs/Extensions/org-S5/ui"
                             org-s5-theme-file "default/slides.css"))
                   )
                 ;; google I/O HTML5 slide
                 (require 'ox-ioslide-helper)
  ))

;; markdown
(use-package markdown-mode+)

;; AutoHotKey
(use-package xahk-mode
  :mode ("\\.ahk\\'" . xahk-mode)
  :config (defalias 'ahk-mode 'xahk-mode))

;; graphviz-mode
(use-package graphviz-dot-mode)

;; bookmark+
(use-package bookmark+)

;; column-mark
(use-package column-marker)

;; bbdb-ext
(use-package bbdb-ext)

;; highligh nickname
(use-package erc-hl-nicks
  :config (progn (add-to-list 'erc-modules 'hl-nicks)
                 (erc-update-modules)))

;; twitter
(use-package twittering-mode
  :config (progn
            (add-hook 'twittering-mode-hook
                      (lambda ()
                        (define-key twittering-mode-map (kbd "M-n") 'twittering-goto-next-uri)
                        (define-key twittering-mode-map (kbd "M-p") 'twittering-goto-previous-uri)
                        ))
            ;;(setq twittering-debug-mode t)
            (setq twittering-use-master-password t)
            ;;(setq twittering-username "")
            ;;(setq twittering-password "")
            (when (eq system-type 'windows-nt)
              (setq twittering-proxy-use t)
              (setq twittering-proxy-server "proxy.zte.com.cn")
              (setq twittering-proxy-port 80)
              )
            (setq twittering-icon-mode t)
            ;;(setq twittering-convert-fix-size 48)
            (setq twittering-use-icon-storage t)
            (twittering-enable-unread-status-notifier)
            (setq twittering-timeline-spec-alias
                  '(("FRIENDS" . "my-account/friends-list")
                    ("related-to" .
                     (lambda (username)
                       (if username
                           (format ":search/to:%s OR from:%s OR @%s/"
                                   username username username)
                         ":home")))
                    ("related-to-twitter" . "$related-to(twitter)")))
            (setq twittering-initial-timeline-spec-string '(
                                                            "(:home+@)"
                                                            "(:search/twittering mode/+:search/twmode/)"
                                                            ))
            (setq twittering-status-format
                  "%FOLD{%RT{%FACE[bold]{RT}}%i%s>>%r @%C{%Y-%m-%d %H:%M:%S} %@{}\n%FOLD[ ]{%T%RT{\nretweeted by %s @%C{%Y-%m-%d %H:%M:%S}}}}")
            ))

;; weibo
(use-package weibo
  :defer t
  :config (setq weibo-consumer-key (plist-get (nth 0 (auth-source-search :host "weibo")) :user)
                weibo-consumer-secret (funcall (plist-get (nth 0 (auth-source-search :host "weibo")) :secret))))

;; elnode
;; default dispatcher: elnode-webserver
(use-package elnode
  :config (setq elnode-webserver-docroot "~/MyEmacs/elnode/public_html/"))

;; peek-mode ( impatient-mode for elnode )
(use-package peek-mode)

;; ix.io
(use-package ix
  :defer t
  :config (setq ix-user "lo2net"
                ix-token (funcall (plist-get (nth 0 (auth-source-search :host "ix.io")) :secret))))

;; refheap
(use-package refheap
  :defer t
  :config (setq refheap-user "lo2net"
                refheap-token (funcall (plist-get (nth 0 (auth-source-search :host "refheap")) :secret))))

;; openstack pastebin
(use-package lodgeit
  :config (setq lodgeit-pastebin-base "http://paste.openstack.org/"))

;; diff-hl
(use-package diff-hl
  :config (progn (global-diff-hl-mode)
                 (diff-hl-margin-mode)))

;; multi-web-mode
(use-package multi-web-mode
  :config (progn (setq mweb-default-major-mode 'html-mode)
                 (setq mweb-tags '((js-mode "<script +\\(type=\"text/javascript\"\\|language=\"javascript\"\\)[^>]*>" "</script>")
                                   (css-mode "<style +type=\"text/css\"[^>]*>" "</style>")))
                 (setq mweb-filename-extensions '("htm" "html"))
                 (multi-web-global-mode 1)))

;; projectile
(use-package projectile
  :config (progn (projectile-global-mode)
                 (setq projectile-enable-caching t)
                 (setq projectile-indexing-method 'alien)
                 (setq projectile-completion-system 'grizzl)))
;; ;; helm-projectile
;; (use-package helm-projectile
;;   :config (progn (helm-projectile-on)
;;                  (setq projectile-switch-project-action 'helm-projectile)))
;; perspective
(use-package perspective
  :config (progn (persp-mode)
                 ;; perspectives-hash can't save in desktop, so..
                 (set-frame-parameter nil 'desktop-dont-save t)))
;; persp-projectile
(use-package persp-projectile)

;; collaboration edit
(use-package rudel
  :disabled t
  :config (progn (global-rudel-minor-mode 1)
                 ;;(global-rudel-header-subscriptions-mode)
                 ;;(global-rudel-mode-line-publish-state-mode)
                 ))

;; refactoring
(use-package emr
  :config (progn (bind-key "M-RET" 'emr-show-refactor-menu prog-mode-map)
                 (add-hook 'prog-mode-hook 'emr-initialize)))

;; chinese-pyim
(use-package chinese-pyim
  :config (setq pyim-dicts '((:name "bigdict" :file "~/pyim-bigdict.txt" :coding utf-8-unix))
                default-input-method "chinese-pyim"
                pyim-use-tooltip nil))

;; debbugs
(use-package debbugs)

;; diminish
(use-package diminish
  :config (progn (diminish 'abbrev-mode)
                 (diminish 'hs-minor-mode)
                 (diminish 'helm-mode)
                 (diminish 'volatile-highlights-mode)
                 (diminish 'highlight-changes-mode)
                 (diminish 'which-key-mode)
                 (add-hook 'emacs-lisp-mode-hook
                           (lambda ()
                             (setq mode-name "ELISP")))
                 ))
;;------------------------------------------------------------------------------

;;------------------------------------------------------------------------------
;;
;; T3 development settings
;;
(when (eq system-type 'windows-nt)
  (when nil
    (setenv "PATH" (concat "C:/Program Files/Microsoft Visual Studio/VC98/bin" path-separator (getenv "PATH")))
    (setenv "PATH" (concat "c:/program files/microsoft visual studio/common/msdev98/bin" path-separator (getenv "PATH")))
    (setenv "PATH" (concat "C:/Program Files/Microsoft Visual Studio/Common/IDE/IDE98" path-separator (getenv "PATH")))
    (setenv "INCLUDE" (concat "c:/program files/microsoft visual studio/vc98/include" path-separator (getenv "INCLUDE")))
    (setenv "LIB" (concat "c:/program files/microsoft visual studio/vc98/lib" path-separator (getenv "LIB")))
    )
  (when t
    (setenv "PATH" (concat "D:/VS2010/bin" path-separator (getenv "PATH")))
    (setenv "INCLUDE" (concat "d:/VS2010/include" path-separator (getenv "INCLUDE")))
    (setenv "INCLUDE" (concat "d:/VS2010/platformsdk/include" path-separator (getenv "INCLUDE")))
    (setenv "LIB" (concat "d:/VS2010/lib" path-separator (getenv "LIB")))
    (setenv "LIB" (concat "d:/VS2010/platformsdk/lib" path-separator (getenv "LIB")))
    )

  (setenv "EDITOR" "emacsclient")
  ;; maven
  (setenv "PATH" (concat "D:/U31_devtools/maven/bin" path-separator (getenv "PATH")))
  (setenv "DEVTOOLS_ROOT" "D:/U31_devtools/devtools")
  (setenv "JAVA_HOME" "D:/U31_devtools/jdk")
  ;; ssh-askpass
  (setenv "SSH_ASKPASS" "d:/home/fangtao/tools/win_ssh_askpass/win-ssh-askpass.exe")
  ;; cygwin
  (setenv "CYGWIN" "nodosfilewarning")
  ;; eshell execute cmds
  (setq eshell-path-env (getenv "PATH"))
  ;; used by emacs etags
  (setq exec-path (split-string (getenv "PATH") path-separator))

  )

;;------------------------------------------------------------------------------
;; was set t at top of buffer
(setq debug-on-error nil)
