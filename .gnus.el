(setq user-full-name "Tao Fang")
(setq user-mail-address "fangtao0901@gmail.com")
(setq message-signature-file "~/.mygnus.sig")

;; 语言环境设定
(set-language-environment 'Chinese-GB)
(setq gnus-default-charset 'chinese-iso-8bit
      gnus-group-name-charset-group-alist '((".*" . cn-gb-2312))
      gnus-summary-show-article-charset-alist
      '((1 . cn-gb-2312)
        (2 . gb18030)
        (3 . chinese-iso-8bit)
        (4 . gbk)
        (5 . big5)
        (6 . utf-8))
      gnus-newsgroup-ignored-charsets
      '(unknown-8bit x-unknown iso-8859-1))

;;解决gb18030乱码
(setq gnus-newsgroup-ignored-charsets
      '(unknown-8bit x-unknown gb18030))

;; 发送设置
(setq gnus-posting-styles
      '((".*"
         (name "Tao Fang")
         (address "fangtao0901@gmail.com")
         (User-Agent "Emacs/gnus")
         (signature "Good good study, day day up!\n")
         )

        ;;邮件发送方式的发送配置
        ((message-mail-p)
         (name"Tao Fang")
         (address "fangtao0901@gmail.com")
         (User-Agent "Emacs/Gnus")
         (signature "Emacs/Gnus\n")
         )
        ))

;; 搜索
(require 'nnir)
(setq gnus-select-method '(nnimap "gmail"
                                  (nnimap-address "imap.gmail.com")
                                  (nnimap-server-port 993)
                                  (nnimap-stream ssl)
                                  (nnir-search-engine imap))) ;; E-mail search

;; ask encyption password once
(setq epa-file-cache-passphrase-for-symmetric-encryption t)

(setq smtpmail-auth-credentials "~/.authinfo.gpg")

(setq message-send-mail-function 'smtpmail-send-it
      ;;smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
      ;;smtpmail-auth-credentials '(("smtp.gmail.com" 587 "fangtao0901@gmail.com" nil))
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587
      smtpmail-local-domain "yourcompany.com")

;;tls-program

;; Make Gnus NOT ignore [Gmail] mailboxes
(setq gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]")

;; sort
(setq gnus-thread-sort-functions 'gnus-thread-sort-by-most-recent-date)

;; NO 'passive
(setq gnus-use-cache t)

;; bbdb
(require 'bbdb)
(bbdb-initialize 'message 'gnus 'mail)
(setq bbdb-file "~/.bbdb") ;; OPTIONAL, because I'm sharing my ~/.emacs.d
(setq bbdb/mail-auto-create-p t
      bbdb/news-auto-create-p t)

;; auto-complete emacs address using bbdb's own UI
(add-hook 'message-mode-hook
          '(lambda ()
             (flyspell-mode t)
             (local-set-key "<TAB>" 'bbdb-complete-name)))

;; Fetch only part of the article if we can.  I saw this in someone
;; else's .gnus
(setq gnus-read-active-file 'some)

;; Tree view for groups.  I like the organisational feel this has.
(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)

;; Threads!  I hate reading un-threaded email -- especially mailing
;; lists.  This helps a ton!
(setq gnus-summary-thread-gathering-function 'gnus-gather-threads-by-subject)

;; Also, I prefer to see only the top level message.  If a message has
;; several replies or is part of a thread, only show the first
;; message.  'gnus-thread-ignore-subject' will ignore the subject and
;; look at 'In-Reply-To:' and 'References:' headers.
(setq gnus-thread-hide-subtree t)
(setq gnus-thread-ignore-subject t)

;;设定要显示的头信息
;; (setq gnus-visible-headers
;;       "^\\(^From:\\|^Subject:\\|^Date:\\|^Followup-To:
;;             \\|^X-Newsreader:\\|^User-Agent:\\|^X-Mailer:
;;             \\|Line:\\|Lines:\\|Content-Type:\\|NNTP-Posting-Host\\)")

;;设定屏幕的分割比例
(gnus-add-configuration '(article (vertical 1.0
                                            (summary .40 point) (article 1.0))))

;; 自动显示图片
(auto-image-file-mode)
(setq mm-inline-large-images t)
(add-to-list 'mm-attachment-override-types "image/*")

;; 杂  项
(setq gnus-confirm-mail-reply-to-news t
      message-kill-buffer-on-exit t
      message-elide-ellipsis "[...]\n"
      )
