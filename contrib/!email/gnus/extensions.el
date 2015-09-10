;;; extensions.el --- Org Extensions File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq gnus-pre-extensions '(wrap-to-fill))

(defun gnus/init-wrap-to-fill ()
  (use-package wrap-to-fill))
