;;; btc-price.el --- Display BTC price in the modeline -*- lexical-binding: t; -*-

;; Author: anevolbap
;; Description: Fetches Bitcoin price from CoinGecko and shows it in the modeline.
;; Usage: M-x btc-price-mode

;;; Code:

(require 'url)
(require 'json)

(defgroup btc-price nil
  "Display BTC price in the modeline."
  :group 'convenience
  :prefix "btc-price-")

(defcustom btc-price-refresh-interval 120
  "Refresh interval in seconds.
CoinGecko free tier allows ~10-30 req/min; 120s is conservative."
  :type 'integer
  :group 'btc-price)

(defcustom btc-price-currency "usd"
  "Fiat currency to quote BTC against (e.g. \"usd\", \"eur\", \"jpy\")."
  :type 'string
  :group 'btc-price)

(defvar btc-price--current nil
  "Current BTC price as a number, or nil if unknown.")

(defvar btc-price--timer nil
  "Timer object for periodic refresh.")

(defvar btc-price--modeline-string ""
  "String displayed in the modeline.")

(put 'btc-price--modeline-string 'risky-local-variable t)

(defun btc-price--api-url ()
  "Return the CoinGecko API URL."
  (format "https://api.coingecko.com/api/v3/simple/price?ids=bitcoin&vs_currencies=%s"
          btc-price-currency))

(defun btc-price--format (price)
  "Format PRICE for modeline display."
  (if price
      (let ((symbol (upcase btc-price-currency)))
        (if (>= price 1000)
            (format " ₿ %s%s " symbol (btc-price--group-thousands price))
          (format " ₿ %s%.2f " symbol price)))
    " ₿ --- "))

(defun btc-price--group-thousands (n)
  "Format number N with comma thousands separators."
  (let* ((int-part (truncate n))
         (s (number-to-string int-part))
         (len (length s))
         (result ""))
    (dotimes (i len)
      (when (and (> i 0) (zerop (% (- len i) 3)))
        (setq result (concat result ",")))
      (setq result (concat result (substring s i (1+ i)))))
    result))

(defun btc-price--handle-response (status)
  "Callback for `url-retrieve'.  STATUS contains error info if any."
  (if (plist-get status :error)
      (progn
        (message "btc-price: fetch error: %S" (plist-get status :error))
        (setq btc-price--current nil))
    (condition-case err
        (progn
          ;; Skip HTTP headers — body starts after the first blank line.
          (goto-char (point-min))
          (re-search-forward "\n\n" nil t)
          (let* ((json-object-type 'alist)
                 (json-key-type 'symbol)
                 (data (json-read))
                 (price (alist-get (intern btc-price-currency)
                                   (alist-get 'bitcoin data))))
            (setq btc-price--current price)))
      (error
       (message "btc-price: parse error: %S" err)
       (setq btc-price--current nil))))
  (setq btc-price--modeline-string (btc-price--format btc-price--current))
  (force-mode-line-update t)
  (kill-buffer (current-buffer)))

(defun btc-price-refresh ()
  "Fetch the current BTC price asynchronously."
  (interactive)
  (let ((url-request-extra-headers '(("Accept" . "application/json"))))
    (url-retrieve (btc-price--api-url) #'btc-price--handle-response nil t t)))

;;;###autoload
(define-minor-mode btc-price-mode
  "Toggle BTC price display in the modeline."
  :global t
  :lighter nil
  :group 'btc-price
  (if btc-price-mode
      (progn
        (unless (memq 'btc-price--modeline-string global-mode-string)
          (setq global-mode-string
                (append global-mode-string '(btc-price--modeline-string))))
        (btc-price-refresh)
        (setq btc-price--timer
              (run-with-timer btc-price-refresh-interval
                              btc-price-refresh-interval
                              #'btc-price-refresh))
        (message "btc-price-mode enabled (refresh every %ds)" btc-price-refresh-interval))
    (when btc-price--timer
      (cancel-timer btc-price--timer)
      (setq btc-price--timer nil))
    (setq global-mode-string
          (delq 'btc-price--modeline-string global-mode-string))
    (setq btc-price--modeline-string "")
    (force-mode-line-update t)
    (message "btc-price-mode disabled")))

(provide 'btc-price)
;;; btc-price.el ends here
