;;; image-link.el --- Easily insert org mode images -*- lexical-binding: t -*-
;;
;; Created: February 13, 2022
;; Modified: February 13, 2022
;; Author: Laurent Charignon <l.charignon@gmail.com>
;; Maintainer: Laurent Charignon <l.charignon@gmail.com>
;; Keywords: org, roam, image, tools
;; Homepage: https://github.com/charignon/image-link
;; Package-Version: 0.1
;; Package-Requires: ((emacs "27.1") (a "1.0.0"))

;;; Commentary:
;;
;;`image-link` lets you insert images into org mode buffer more easily

;;; Code:

(require 'a)
(require 's)
(require 'f)
(require 'image-dired)
(require 'org)

(defcustom image-link-subjects-settings '()
  "A-list of settings for picture subjects.
The keys are subject names and the values are a-list containing:
'album', a folder to put pictures into once inserted.
'file', an org file that will contain all the pictures for that subject.

Example:
'((\"armand\" . ((\"album\" . \"/Users/laurent/.roam/album_armand/\")
   (\"file\" . \"/Users/laurent/.roam/20220119215323-armand_s_photo_album_2022.org\")))
 (\"josephine\" . ((\"album\" . \"/Users/laurent/.roam/album_josephine/\")
   (\"file\" . \"/Users/laurent/.roam/20220119215313-josephine_s_photo_album_2022.org\"))))"
  :group 'image-link
  :type 'a-list)

(defcustom image-link-folder nil
  "Path to the folder containing images to insert."
  :group 'image-link
  :type 'string)

(setq image-link-destination-file nil
      image-link-destination-buffer nil)

(defun image-link-uuid-create ()
  "Return a newly generated UUIDm, use a simple hashing of variable data.
From http://xahlee.info/emacs/emacs/elisp_generate_uuid.html"
  (let ((s (md5 (format "%s%s%s%s%s%s%s%s%s%s"
                        (user-uid)
                        (emacs-pid)
                        (system-name)
                        (user-full-name)
                        user-mail-address
                        (current-time)
                        (emacs-uptime)
                        (garbage-collect)
                        (random)
                        (recent-keys)))))
    (format "%s-%s-3%s-%s-%s"
            (substring s 0 8)
            (substring s 8 12)
            (substring s 13 16)
            (substring s 16 20)
            (substring s 20 32))))

(defun image-link-attach-image-at-point ()
  "Attach image at point into the previously visited 'org-mode' buffer."
  (let* ((fname                 (image-dired-original-file-name))
         (kind                  (completing-read "Kind: " (a-keys image-link-subjects-settings)))
         (dest-fname            (f-filename image-link-destination-file))
         (dest-fname-format     (format "<%s>" (s-replace ".org" "" dest-fname)))
         (is-date-file          (s-match "20..-..-.." dest-fname))
         (dateraw               (if is-date-file
                                    dest-fname-format
                                  (completing-read "Date: " '("today" "other"))))
         (ts                     (cond
                                  ((string= dateraw "other") (with-temp-buffer
                                                               (org-time-stamp nil)
                                                               org-last-inserted-timestamp))
                                  ((string= dateraw "today") (format-time-string "<%Y-%m-%d %a>" (current-time)))
                                  (t dateraw)))
         (description           (read-from-minibuffer "Enter a description: "))
         (uuid                  (s-concat (image-link-uuid-create) ".png"))
         (subject               (a-get image-link-subjects-settings kind))
         (album                 (a-get subject "album"))
         (albumfile             (a-get subject "file"))
         (path                  (s-concat album uuid))
         (link                  (s-concat
                                 "#+DATE:" ts
                                 "\n#+CAPTION: " description
                                 "\n#+ATTR_ORG: :width 300"
                                 "\n#+ATTR_HTML: :width 600"
                                 "\n[[" path "]]\n\n\n"))
         (link-with-star        (s-concat "* " ts "\n\n" link)))
    (message "Attaching %s as %s in %s" fname path image-link-destination-file)
    ;; Quit the buffer
    (pop-to-buffer image-link-destination-buffer)
    (delete-other-windows)
    ;; Move the image where it should go
    (f-move fname path)
    ;; Insert a block here
    (if (string= albumfile image-link-destination-file)
        ;; Insert just here
        (insert link-with-star)
      (progn
        (insert link)
        (f-append-text link-with-star 'utf-8 albumfile)))))


(defun image-link-launch-image-picker (folder)
  "Open an image picked in FOLDER and remember the file we are coming from."
  (let ((bfn (buffer-file-name))
        (bf (current-buffer)))
    (image-dired folder)
    (message "Updating variable image-link-destination-file %s" bfn)
    (setq-local image-link-destination-file bfn
                image-link-destination-buffer bf)
    (delete-other-windows)))

;;;###autoload
(defun image-link-run ()
  "Entry point for image-link.
If in 'org-mode' pops to select a file from the landing zone.
In 'image-dired' mode inserts the file in the 'org-mode' buffer we came from."
  (interactive)
  (cond
   ;; User picked a file
   ((bound-and-true-p image-link-destination-file)
    (image-link-attach-image-at-point))

   ((< (length (f-glob "*.png" image-link-folder)) 1)
    (message "No pictures in the landing zone"))

   ((string-equal major-mode "org-mode")
    (image-link-launch-image-picker image-link-folder)
    (message "Please run the same command again while over a file you want to insert"))

   (t (message "Need to run from an org mode buffer"))))

(provide 'image-link)
;;; image-link.el ends here
