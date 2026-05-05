;;; cv-export.el --- Export org CV to silver-dev-cv typst  -*- lexical-binding: t -*-
;;;
;;; Commentary:
;;;
;;; Command: M-x cv-export-to-typst.
;;;
;;; Reads the current org buffer and writes a sibling .typ file that imports
;;; @preview/silver-dev-cv and calls its content functions.
;;;
;;; Expected org structure (see cv-silver.org):
;;;
;;;   #+CV_NAME:, #+CV_ADDRESS:, #+CV_DATE:, #+CV_FONT:, #+CV_EMAIL:
;;;   #+TITLE:, #+AUTHOR:
;;;
;;;   * Section                                :descript:   → #descript[body]
;;;   * Section                                             → iterate children
;;;   ** Entry                                 :job:        → #job(...)
;;;       :PROPERTIES: INSTITUTION LOCATION DATE :END:
;;;   ** Entry                                 :education:  → #education(...)
;;;       :PROPERTIES: MAJOR DATE LOCATION :END:
;;;   ** Entry                                 :oneline:    → #oneline-title-item
;;;
;;; Code:

(require 'org)
(require 'org-element)
(require 'seq)

(defun cv-export--convert (s)
  "Convert org body text S to typst: transform links, escape $."
  (if (or (null s) (string-empty-p s))
      ""
    (let ((tokens nil) (i 0))
      (with-temp-buffer
        (insert s)
        (goto-char (point-min))
        (while (re-search-forward "\\[\\[\\([^]]+\\)\\]\\[\\([^]]+\\)\\]\\]" nil t)
          (let* ((url (match-string 1))
                 (text (match-string 2))
                 (tok (format "\000%d\000" (cl-incf i))))
            (push (cons tok (format "#link(\"%s\")[%s]" url text)) tokens)
            (replace-match tok t t)))
        (goto-char (point-min))
        (while (search-forward "$" nil t)
          (replace-match "\\$" t t))
        (dolist (pair tokens)
          (goto-char (point-min))
          (when (search-forward (car pair) nil t)
            (replace-match (cdr pair) t t)))
        (buffer-string)))))

(defun cv-export--indent (s n)
  "Prefix each line of S with N spaces."
  (let ((prefix (make-string n ?\s)))
    (mapconcat (lambda (l) (concat prefix l))
               (split-string s "\n")
               "\n")))

(defun cv-export--prop (hl key)
  "Return property KEY from headline HL, or empty string."
  (or (org-element-property (intern (concat ":" (upcase key))) hl) ""))

(defun cv-export--entry-body (hl)
  "Raw body text of HL, excluding property drawer and sub-headlines."
  (let* ((section (car (seq-filter
                        (lambda (c) (eq (org-element-type c) 'section))
                        (org-element-contents hl))))
         (parts (and section
                     (seq-filter
                      (lambda (c)
                        (not (memq (org-element-type c)
                                   '(property-drawer planning))))
                      (org-element-contents section)))))
    (if parts
        (string-trim
         (buffer-substring-no-properties
          (org-element-property :begin (car parts))
          (org-element-property :end (car (last parts)))))
      "")))

(defun cv-export--children (hl)
  "Direct child headlines of HL."
  (let ((child-level (1+ (org-element-property :level hl))))
    (seq-filter
     (lambda (c) (and (eq (org-element-type c) 'headline)
                      (= child-level (org-element-property :level c))))
     (org-element-contents hl))))

(defun cv-export--job (hl)
  (format "#job(
  position: \"%s\",
  institution: [%s],
  location: \"%s\",
  date: \"%s\",
  description: [
%s
  ],
)
"
          (org-element-property :raw-value hl)
          (cv-export--convert (cv-export--prop hl "INSTITUTION"))
          (cv-export--prop hl "LOCATION")
          (cv-export--prop hl "DATE")
          (cv-export--indent (cv-export--convert (cv-export--entry-body hl)) 4)))

(defun cv-export--education (hl)
  (format "#education(
  institution: [%s],
  major: [%s],
  date: \"%s\",
  location: \"%s\",
)
"
          (org-element-property :raw-value hl)
          (cv-export--convert (cv-export--prop hl "MAJOR"))
          (cv-export--prop hl "DATE")
          (cv-export--prop hl "LOCATION")))

(defun cv-export--oneline (hl)
  (format "#oneline-title-item(
  title: \"%s\",
  content: [%s],
)
"
          (org-element-property :raw-value hl)
          (cv-export--convert (cv-export--entry-body hl))))

(defun cv-export--entry (hl)
  (let ((tags (org-element-property :tags hl)))
    (cond
     ((member "job" tags)       (cv-export--job hl))
     ((member "education" tags) (cv-export--education hl))
     ((member "oneline" tags)   (cv-export--oneline hl))
     (t (format "// skipped entry: %s\n" (org-element-property :raw-value hl))))))

(defun cv-export--section (hl)
  (let ((title (org-element-property :raw-value hl))
        (tags  (org-element-property :tags hl)))
    (cond
     ((member "descript" tags)
      (format "#section[%s]\n#descript[%s]\n#sectionsep\n\n"
              title
              (cv-export--convert (cv-export--entry-body hl))))
     (t
      (concat
       (format "#section(\"%s\")\n" title)
       (mapconcat #'cv-export--entry (cv-export--children hl) "")
       "#sectionsep\n\n")))))

(defun cv-export--preamble (name address date font email)
  (format "#import \"@preview/silver-dev-cv:1.0.2\": *
#show: cv.with(
  font-type: \"%s\",
  continue-header: \"false\",
  name: \"%s\",
  address: \"%s\",
  lastupdated: \"true\",
  pagecount: \"true\",
  date: \"%s\",
  contacts: (
    (text: \"%s\", link: \"mailto:%s\"),
  ),
)
#show link: it => text(
  fill: blue,
  it
)

"
          font name address date email email))

;;;###autoload
(defun cv-export-to-typst ()
  "Export the current org CV buffer to silver-dev-cv typst source."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in an org buffer"))
  (unless (buffer-file-name)
    (user-error "Buffer is not visiting a file"))
  (let* ((kw (org-collect-keywords
              '("CV_NAME" "CV_ADDRESS" "CV_DATE" "CV_FONT" "CV_EMAIL"
                "TITLE" "AUTHOR")))
         (getk (lambda (k) (or (cadr (assoc k kw)) "")))
         (tree (org-element-parse-buffer))
         (out  (concat (file-name-sans-extension (buffer-file-name)) ".typ"))
         (top  (seq-filter
                (lambda (c) (and (eq (org-element-type c) 'headline)
                                 (= 1 (org-element-property :level c))))
                (org-element-contents tree))))
    (let ((result (concat
                   (cv-export--preamble
                    (funcall getk "CV_NAME")
                    (funcall getk "CV_ADDRESS")
                    (funcall getk "CV_DATE")
                    (let ((f (funcall getk "CV_FONT")))
                      (if (string-empty-p f) "PT Serif" f))
                    (funcall getk "CV_EMAIL"))
                   (mapconcat #'cv-export--section top "")
                   (format "#set document(author: \"%s\", title: \"%s\")\n"
                           (funcall getk "AUTHOR")
                           (funcall getk "TITLE")))))
      (with-temp-file out (insert result)))
    (message "Exported %s" out)))

(provide 'cv-export)
;;; cv-export.el ends here
