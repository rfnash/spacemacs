;;; packages.el --- gnus Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq gnus-packages '(gnus))

(defun gnus/init-gnus ()
  "Initialize my package"
  (use-package gnus
    :defer t
    :bind ("C-c g" . rfnash-visit-gnus-buffer)
    :commands (gnus rfnash-visit-gnus-buffer)
    :init
    (evil-leader/set-key
      "ag" 'gnus
      "og"  'rfnash-visit-gnus-buffer)
    :config
    (progn
    (setq gnus-select-method '(nntp "news.gwene.org"))
    (setq gnus-secondary-select-methods
          '((nnimap "rfnash@openmailbox.org"
                    (nnimap-address "imap.openmailbox.org")
                    (nnimap-server-port 143)
                    (nnimap-user "rfnash@openmailbox.org")
                    (nnimap-stream starttls))
            (nnimap "robertnash@openmailbox.org"
                    (nnimap-address "imap.openmailbox.org")
                    (nnimap-server-port 143)
                    (nnimap-user "robertnash@openmailbox.org")
                    (nnimap-stream starttls))
            (nnimap "musicmaker1118@gmail.com"
                    (nnimap-address "imap.gmail.com")
                    (nnimap-user "musicmaker1118@gmail.com"))
            (nnimap "localhost"
                    (nnimap-stream ssl))
            (nnimap "outlook"
                    (nnimap-address "imap-mail.outlook.com")
                    (nnimap-user "rfnash@outlook.com"))
            (nnimap "robertnash.net"
                    (nnimap-address "imap.gmail.com")
                    (nnimap-user "robert@nash.co.vu"))))

    (setq message-send-mail-function 'message-send-mail-with-sendmail
          message-sendmail-f-is-evil 't ; this is needed to allow msmtp to do its magic
          sendmail-program "msmtp"
          message-sendmail-extra-arguments '("--read-envelope-from" "-t"))

    ; Use topics per default:
    (add-hook 'gnus-group-mode-hook 'gnus-topic-mode)

    (setq gnus-visible-headers
        "^From:\\|^Reply-To\\|^Organization:\\|^To:\\|^Cc:\\|^Newsgroups:\\|^Subject:\\|^Date:\\|^Gnus")

    ;;; Show the article headers in this order.
    (setq gnus-sorted-header-list
      '("^From:" "^Reply-To" "^Organization:" "^To:" "^Cc:" "^Newsgroups:"
        "^Subject:" "^Date:" "^Gnus"))

    (setq-default
        gnus-summary-line-format "%U%R%z %(%&user-date;  %-15,15f  %B (%c) %s%)\n"
        gnus-user-date-format-alist '((t . "%Y-%m-%d %H:%M"))
        gnus-group-line-format "%M%S%p%P%5y:%B %G\n";;"%B%(%g%)"
        gnus-summary-thread-gathering-function 'gnus-gather-threads-by-references
        gnus-thread-sort-functions '(gnus-thread-sort-by-most-recent-date)
       ;; Make Gnus NOT ignore [Gmail] mailboxes
        gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\”]\”[#’()]"
       nnmail-expiry-wait 'immediate
       gnus-group-default-list-level 3
       gnus-save-duplicate-list t
       gnus-suppress-duplicates t
       gnus-view-pseudos 'not-confirm
       user-full-name "Robert F. Nash"
       user-mail-address "robert@robertnash.net"
       gnus-buttonized-mime-types '("multipart/alternative")
       ;; mm-discouraged-alternatives '("text/html" "text/richtext")
        gnus-sum-thread-tree-false-root ""
        gnus-sum-thread-tree-indent " "
        gnus-sum-thread-tree-leaf-with-other "├► "
        gnus-sum-thread-tree-root ""
        gnus-sum-thread-tree-single-leaf "╰► "
        gnus-sum-thread-tree-vertical "│"
        gnus-article-browse-delete-temp nil 
        gnus-fetch-old-headers t
        gnus-treat-strip-trailing-blank-lines 'last
        gnus-keep-backlog 'nil
        gnus-summary-display-arrow nil ; Don't show that annoying arrow:
        gnus-mime-display-multipart-related-as-mixed t ; Show more MIME-stuff:
        gnus-auto-select-first nil ; Don't get the first article automatically:
        smiley-style 'medium
        gnus-keep-backlog '0)

      (add-hook 'kill-emacs-hook 'gnus-group-exit)

      (defun rfnash-visit-gnus-buffer ()
        "Create or visit GNUS's group buffer."
        (interactive)
        (if (get-buffer "*Group*")
            (rfnash-switch-buffer "*Group*")
          (gnus)))

      (defun my-fmt-width ()
        "Find the width of the current window in the current font size."
        (interactive)
        (ceiling (/ (window-width) (expt text-scale-mode-step text-scale-mode-amount))))

      ;; TODO: make interative, and allow entering a width,
      ;; still defaulting to calculated one
      (defun my-fmt ()
        "Wash Gnus Articles by wrapping words to my liking."
        (interactive)
        (save-excursion
          (gnus-article-fill-cited-article (my-fmt-width))
          (gnus-article-strip-trailing-space)))

      (define-key gnus-article-mode-map (kbd "\\") 'my-fmt)

      (require 'wrap-to-fill)
      (defun rfnash-gnus-article-mode-hook ()
        "Increase font size and set wrap-to-fill-column-mode for Gnus Articles."
        (interactive)
        (text-scale-set 2)
        (set-fill-column (floor (* (frame-width) 0.72 )))
        (wrap-to-fill-column-mode t))

      (add-hook 'gnus-article-mode-hook 'rfnash-gnus-article-mode-hook)
      ;;(require 'mu4e-utils)
      ;;(add-hook 'gnus-get-top-new-news-hook (lambda () (mu4e-update-mail-and-index nil)))
    (require 'browse-url)
    (require 'nnrss)
    (defun spacemacs/browse-nnrss-url (arg)
      "Open RSS Article directy in the browser"
    (interactive "p")
    (let ((url (assq nnrss-url-field
                        (mail-header-extra
                        (gnus-data-header
                        (assq (gnus-summary-article-number)
                                gnus-newsgroup-data))))))
        (if url
            (progn
            (browse-url (cdr url))
            (gnus-summary-mark-as-read-forward 1))
        (gnus-summary-scroll-up arg))))
    (add-to-list 'nnmail-extra-headers nnrss-url-field)

    (evilify gnus-group-mode gnus-group-mode-map
               (kbd "l") #'gnus-group-list-groups)
    (evilify gnus-server-mode gnus-server-mode-map)
    (evilify gnus-browse-mode gnus-browse-mode-map)
;;  (evilify gnus-article-mode gnus-article-mode-map)
;;  (evilify gnus-summary-mode gnus-summary-mode-map
;;      (kbd "J") 'gnus-summary-next-article
;;      (kbd "K") 'gnus-summary-prev-article
;;      (kbd "<RET>") 'spacemacs/browse-nnrss-url)
))

  ;; org-mime is initialized here because otherwise spacemacs
  ;; complains that the org-mime package does not exist
  (use-package org-mime
    :defer t
    :commands (org-mime-htmlize
               org-mime-org-buffer-htmlize)
    :init
    (progn
      ;; setup org-mime
      (evil-leader/set-key-for-mode 'message-mode
        "mo" 'org-mime-htmlize)
      (evil-leader/set-key-for-mode 'org-mode
        "mH" 'org-mime-org-buffer-htmlize))))
