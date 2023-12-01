;; Check if the current elfeed entry is an arxiv entry
(defun is-arxiv-entry? ()
  "Check if the current buffer is an Elfeed buffer showing an Arxiv entry."
  (and (eq major-mode 'elfeed-show-mode)
        (let ((url (elfeed-entry-link elfeed-show-entry)))
          (string-match-p "arxiv\\.org" url))))

(defun extract-bibtex-key (bibtex-content)
"Extract the citation key from a BibTeX entry."
(when (string-match "@[a-zA-Z]+{\\([^,]+\\)," bibtex-content)
  (match-string 1 bibtex-content)))

(defun download-and-open-arxiv ()
  "Fetch the PDF of the current Arxiv entry and open it, or prompt for a URL if not an Arxiv entry."
  (interactive)
  (let* ((url (if (is-arxiv-entry?)
                  (elfeed-entry-link elfeed-show-entry)
                (read-string "Enter the PDF URL: ")))
        (bibtex-url (replace-regexp-in-string "/\\(abs\\|pdf\\)/" "/bibtex/" url))
        ;; Fetch BibTeX content to extract key only if it's an Arxiv entry
        (bibtex-content (when bibtex-url
                          (with-current-buffer (url-retrieve-synchronously bibtex-url)
                            (goto-char url-http-end-of-headers)
                            (buffer-substring-no-properties (point) (point-max)))))
        ;; Extract citation key from BibTeX content if available, else use file name
        (bibtex-key (if bibtex-content
                        (extract-bibtex-key bibtex-content)
                      (file-name-nondirectory url)))
        ;; Update URL for PDF download if it's an Arxiv entry
        (pdf-url (if (is-arxiv-entry?) (concat (replace-regexp-in-string "/abs/" "/pdf/" url) ".pdf") url))
        (download-path (concat (if (boundp 'my-arxiv-library-path)
                                  my-arxiv-library-path
                                "~/arxiv-library/")
                              bibtex-key ".pdf")))
    (url-copy-file pdf-url download-path t)
    (find-file-other-window download-path)))


(with-eval-after-load 'elfeed
  (define-key elfeed-show-mode-map (kbd "d") 'download-and-open-arxiv))

(defun fetch-bibtex-from-arxiv ()
"Fetch the BibTeX citation for the current Arxiv entry and append it to bibliography.bib."
(interactive)
(when (is-arxiv-entry?)
  (let* ((url (elfeed-entry-link elfeed-show-entry))
          ;; Ensure we have a valid URL
          (arxiv-id (when (string-match "/abs/\\([^/]+\\)" url)
                      (match-string 1 url)))
          (bibtex-url (when arxiv-id
                        (concat "https://arxiv.org/bibtex/" arxiv-id)))
          ;; Define the path to bibliography.bib
          (bib-file (expand-file-name "bibliography.bib" (if (boundp 'my-arxiv-library-path)
                                                            my-arxiv-library-path
                                                          "~/arxiv-library/"))))
    (when bibtex-url
      (let ((bibtex-content (with-current-buffer (url-retrieve-synchronously bibtex-url)
                              (goto-char url-http-end-of-headers)
                              (buffer-substring-no-properties (point) (point-max)))))
        ;; Append BibTeX content to bibliography.bib
        (with-temp-buffer
          (when (file-exists-p bib-file)
            (insert-file-contents bib-file))
          (goto-char (point-max))
          (insert "\n\n" bibtex-content)
          (write-file bib-file)))))))


(with-eval-after-load 'elfeed
  (define-key elfeed-show-mode-map (kbd "c") 'fetch-bibtex-from-arxiv))
