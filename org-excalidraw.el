;;; org-excalidraw.el -- org excalidraw link  -*- lexical-binding: t; -*-

;; Author: 4honor <binz.hust@gmail.com>
;; URL: https://github.com/4honor/org-excalidraw
;; Version: 0.1.0
;; Keywords: orgmode, excalidraw
;; Package-Requires: ((org "9.7") (emacs "30.0"))

;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Commentary:
;; org-excalidraw.el provides supports to open, create, export, and display excalidraw drawing in org mode.

;;; Code:

(require 'org)
(require 'org-element)

;;; Customizations

(defcustom org-excalidraw-url-protocol "excalidraw"
  "Protocol identified for excalidraw links"
  :group 'org-excalidraw
  :type 'string)


;;; Open, Create, Export, and Display
;; Open

(defun org-excalidraw-link-open (link)
  "Open excalidraw LINK with resource opener according to the desktop environment."

  (let ((path (expand-file-name link)))
    ;; Validate file extension
    (unless (string-suffix-p ".excalidraw" path)
      (error "Excalidraw diagrams must ends with .excaldiraw extension."))

    ;; Open file
    (pcase system-type
      ;; Linux
      ('gnu/linux (shell-command (concat "xdg-open " (shell-quote-argument path))))
      ;; MacOS
      ('darwin (shell-command (concat "open " (shell-quote-argument path))))
      ;; Others
      (_ (message "Unsupported system type, only Linux/MacOS supported")))))

;; Create

(defconst org-excalidraw-default-template
  "{
    \"type\": \"excalidraw\",
    \"version\": 2,
    \"source\": \"https://excalidraw.com\",
    \"elements\": [],
    \"appState\": {
      \"gridSize\": null,
      \"viewBackgroundColor\": \"#ffffff\"
    },
    \"files\": {}
  }
"
  "excalidraw template in JSON.")

(defun org-excalidraw-create-drawing ()
  "Create an excalidraw drawing and insert an org-mode link at point."

  (interactive)

  (let* ((path (read-file-name "Enter path: "))
         (link (format "[[excalidraw:%s]]" path)))

    (if (not (file-exists-p path))
        (with-temp-file path
          (insert org-excalidraw-default-template)))

    (insert link)
    (org-excalidraw-link-open path)))


;; Thumbnail

(defconst org-excalidraw-thumbnail-extension "svg"
  "Extension for excalidraw thumbnail export.")

(defun org-excalidraw-svg-thumbnail-path (path)
  "Return the thumbnail path of PATH in svg."

  (format "%s.%s" path org-excalidraw-thumbnail-extension))


(defun org-excalidraw-to-svg-thumbnail (file)
  "Export excalidraw FILE to svg thumbnail."

  (let* ((path (expand-file-name file))
         (svg-path (org-excalidraw-svg-thumbnail-path path)))

    ;; Validate
    (unless (string-suffix-p ".excalidraw" path)
      (error "Excalidraw diagrams must ends with .excaldiraw extension."))

    ;; Convert
    (cond
     ;; With `kroki'
     ((executable-find "kroki")
      (shell-command
       (format "kroki convert \"%s\" --type excalidraw --format svg --out-file \"%s\""
               path
               svg-path)))

     ;; With `excalidraw_export'
     ;; npm install -g excalidraw_export
     ((executable-find "excalidraw_export")
      (shell-command (format "excalidraw_export --rename_fonts \"%s\"" path)))

     ;; Default
     (t
      (error "Can't convert %s to svg." file)))))

;; Export

(defun org-excalidraw-link-export (link _description backend)
  "Export excalidraw LINK to BACKEND."

  (let* ((path (expand-file-name link))
         (svg-path (org-excalidraw-svg-thumbnail-path path)))

    (org-excalidraw-to-svg-thumbnail path)

    (org-export-string-as (format "[[%s]]" svg-path) backend t)))


;; Display

(defun org-excalidraw-data-fun (_protocol link _description)
  "Get thumbnail data corresponding to LINK."

  (let* ((path (expand-file-name link))
         (svg-path (org-excalidraw-svg-thumbnail-path path)))

    (org-excalidraw-to-svg-thumbnail path)

    (condition-case nil
        (with-temp-buffer
          (set-buffer-multibyte nil)
          (insert-file-contents-literally svg-path)
          (let ((image-data (buffer-string)))
            ;; Make sure get something
            (if (> (string-bytes image-data) 0)
                image-data
              nil)))
      (error nil))))


(defun org-image-update-overlay (file link &optional data-p refresh)
  "Create image overlay for FILE asscociated with org-element LINK.
If DATA-P is non-nil FILE is not a file name but a string with the image data.
If REFRESH is non-nil don't download the file but refresh the image."
  (when (or data-p (file-exists-p file))
    (let ((width
           ;; Apply `org-image-actual-width' specifications
           (cond
            ((eq org-image-actual-width t)
             nil)
            ((listp org-image-actual-width)
             (or
              ;; First try to find a width among attributes asscociated to
              ;; the paragraph containing link.
              (let ((paragraph
                     (let ((e link))
                       (while (and (setq e (org-element-property :parent e))
                                   (not (eq (org-element-type e) 'paragraph))))
                       e)))
                (when paragraph
                  (save-excursion
                    (goto-char (org-element-property :begin paragraph))
                    (when (re-search-forward "^[ \t]*#\\+attr_.*?: +.*?:width +\\(\\S-+\\)"
                                             (org-element-property :post-affiliated paragraph)
                                             t)
                      (string-to-number (match-string 1))))))
              ;; Otherwise, fall-back to provided number.
              (car org-image-actual-width)))
            ((numberp org-image-actual-width)
             org-image-actual-width)))
          (old
           (get-char-property-and-overlay (org-element-property :begin link) 'org-image-overlay)))
      (if (and (car-safe old) refresh)
          (image-flush (overlay-get (cdr old) 'display))
        (let ((image
               (create-image file
                             (and (image-type-available-p 'imagemagick) width 'imagemagick)
                             data-p
                             :width width)))
          (when image
            (let* ((link
                    ;; If inline image is the description of another link, be sure
                    ;; to consider the latter as the one to apply the overlay on
                    (let ((parent (org-element-property :parent link)))
                      (if (eq (org-element-type parent) 'link)
                          parent
                        link)))
                   (ov
                    (make-overlay
                     (org-element-property :begin link)
                     (progn
                       (goto-char (org-element-property :end link))
                       (skip-chars-backward " \t")
                       (point)))))
              (overlay-put ov 'display image)
              (overlay-put ov 'face 'default)
              (overlay-put ov 'org-image-overlay t)
              (overlay-put ov 'modification-hooks (list 'org-display-inline-remove-overlay))
              (push ov org-inline-image-overlays)
              ov)))))))


(defun org-display-user-inline-images (&optional _include-linked _refreshed beg end)
  "Like `org-display-inline-images' but for image data links.
_INCLUDE-LINKED and _REFRESHED are ignored.
Restrict to region between BEG and END if both are non-nil.
Image data links have a :image-data-fun parameter.
The value of the :image-data-fun parameter is a function taking the
PROTOCOL, the LINK, and the DESCRIPTION as arguments.
If that function returns nil the link is not interpreted as image.
Otherwise the return value is the image data string to be displayed.

Note that only bracket links are allowed as image data links with one
of the forms
[[PROTOCOL:LINK]]
or
[[PROTOCOL:LINK][DESCRIPTION]]
are recognized."
  (interactive)
  (when (and (called-interactively-p 'any) (use-region-p))
    (setq
     beg (region-beginning)
     end (region-end)))

  (when (display-graphic-p)
    (org-with-wide-buffer
     (goto-char (or beg (point-min)))
     (when-let ((image-data-link-parameters
                 (cl-loop
                  for
                  link-par-entry
                  in
                  org-link-parameters
                  with
                  fun
                  when
                  (setq fun (plist-get (cdr link-par-entry) :image-data-fun))
                  collect
                  (cons (car link-par-entry) fun)))
                (image-data-link-re (regexp-opt (mapcar 'car image-data-link-parameters)))
                (re
                 (format "\\[\\[\\(%s\\):\\([^]]+\\)\\]\\(?:\\[\\([^]]+\\)\\]\\)?\\]"
                         image-data-link-re)))
       (while (re-search-forward re end t)
         (let* ((protocol (match-string-no-properties 1))
                (link (match-string-no-properties 2))
                (description (match-string-no-properties 3))
                (image-data-link (assoc-string protocol image-data-link-parameters))
                (el
                 (save-excursion
                   (goto-char (match-beginning 1))
                   (org-element-context)))
                image-data)
           (when el
             (setq image-data
                   (or (let ((old
                              (get-char-property-and-overlay
                               (org-element-property :begin el) 'org-image-overlay)))
                         (and old (car-safe old) (overlay-get (cdr old) 'display)))
                       (funcall (cdr image-data-link) protocol link description)))
             (when image-data
               (let ((ol (org-image-update-overlay image-data el t t)))
                 (when (and ol description)
                   (overlay-put ol 'after-string description)))))))))))

(advice-add #'org-display-inline-images :after #'org-display-user-inline-images)

;; Complete

(defun org-excalidraw-link-complete (&optional arg)
  "Create a excalidraw file link using completion.
With optional ARG \\='(16), abbreviate the file name in the link.
It works the same way as `org-link-complete-file'"
  (let ((file (read-file-name "Excalidraw File: "))
        (pwd (file-name-as-directory (expand-file-name ".")))
        (pwd1 (file-name-as-directory (abbreviate-file-name (expand-file-name ".")))))
    (cond
     ((equal arg '(16))
      (concat "excalidraw:" (abbreviate-file-name (expand-file-name file))))
     ((string-match (concat "^" (regexp-quote pwd1) "\\(.+\\)") file)
      (concat "excalidraw:" (match-string 1 file)))
     ((string-match (concat "^" (regexp-quote pwd) "\\(.+\\)") (expand-file-name file))
      (concat "excalidraw:" (match-string 1 (expand-file-name file))))
     (t
      (concat "excalidraw:" file)))))

;; Link

(org-link-set-parameters
 org-excalidraw-url-protocol
 :follow #'org-excalidraw-link-open
 :export #'org-excalidraw-link-export
 :image-data-fun #'org-excalidraw-data-fun
 :complete #'org-excalidraw-link-complete)


(provide 'org-excalidraw)
;;; org-excalidraw.el ends here
