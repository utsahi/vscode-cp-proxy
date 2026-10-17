;; -*- lexical-binding: t -*-
;;; vscode-cp-proxy.el --- VS Code Copilot Proxy integration.

;; Copyright (C) 2025

;; Author: Kishor Datar kishordatar at gmail
;; Homepage: http://localhost
;; Keywords: Custom

;; Package-Version: 1.0.0
;; Package-Requires: ((emacs "30.0"))


;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

(require 'gptel)
(require 'url)

(defgroup vscode-cp-proxy nil
  "VS Code Copilot Proxy."
  :group 'vscode-cp-proxy)

(defvar vscode-cp-proxy-token nil
  "Token to use for authentication with vscode-cp-proxy server.

If nil ask for a token when activating.
If a string, this token is used.
If a function, it should produce the token value.

By default, the token can be found is vs code in the output panel (View > Output)
in the vscode-cp-proxy log (choose via drop down menu).

Alternatively, you can invoke command setToken in VS Code to permenantly
set a token in VS Code.

")

(defcustom vscode-cp-proxy-host
  "127.0.0.1"
  "Host to communicate with the vscode-cp-proxy server."
  :type 'string
  :group 'vscode-cp-proxy)

(defcustom vscode-cp-proxy-port
  5555
  "Port to communicate with the vscode-cp-proxy server."
  :type 'integer
  :group 'vscode-cp-proxy)

(defun vscode-cp-proxy-set-token ()
  (interactive)

  (if (functionp vscode-cp-proxy-token)
      (or (apply vscode-cp-proxy-token nil) (error "No token was produced by 'vscode-cp-proxy-token"))
      (when (or (called-interactively-p 'any) (not vscode-cp-proxy-token))
        (setq
         vscode-cp-proxy-token
         (string-replace
          "'"
          ""
          (string-trim (read-from-minibuffer "Enter the token. (I'll remove the quotes.) "))))
        vscode-cp-proxy-token)))

(defun vscode-cp-proxy-set-gptel-backend (prefix)
  (interactive "P")

  (let* ((token (call-interactively 'vscode-cp-proxy-set-token))
         (url-request-extra-headers
          `(("Authorization" .
             ,(format "Bearer %s" token))))
         (models-json
          (with-current-buffer
              (url-retrieve-synchronously (format "http://%s:%s/openai/v1/models" vscode-cp-proxy-host vscode-cp-proxy-port))
            (beginning-of-buffer)
            (search-forward-regexp "\r?\n\r?\n")
            (json-parse-buffer)))
         (models-data (gethash "data" models-json))
         (model-displays
          (seq-map
           (lambda (e)
             (let* ((id (gethash "id" e))
                    (name (or (gethash "name" e) ""))
                    (maxTokens (or (gethash "maxTokens" e) (gethash "max_input_tokens" e) 0)))
               (format "%s | name=%s | maxTokens=%s" id name maxTokens)))
           models-data))
         (model-ids
          (seq-map (lambda (e) (gethash "id" e)) models-data)))
    (let* ((selection (completing-read "Select model: " model-displays nil t))
           (selected-id (car (split-string selection " |"))))
      (setq gptel-model (intern selected-id)))
    (setq
     gptel-backend
     (gptel-make-openai "gptel-vscode-cp-proxy"
       :host (format "%s:%s" vscode-cp-proxy-host vscode-cp-proxy-port)
       :protocol "http"
       :endpoint "/openai/v1/chat/completions"
       :stream (not prefix)
       :key 'vscode-cp-proxy-set-token
       :models (seq-map
                (lambda (m)
                  `(,(intern m)
                    :description
                    ""
                    :capabilities (media tool json url)
                    :mime-types
                    ("image/jpeg" "image/png" "image/gif" "image/webp")))
                model-ids)
       ))))

(defcustom vscode-cp-proxy-deepinfra-models-url
  "https://api.deepinfra.com/models/list"
  "URL of the DeepInfra model list endpoint."
  :type 'string
  :group 'vscode-cp-proxy)

(defun vscode-cp-proxy--deepinfra-absent-p (value)
  "Return non-nil if VALUE is absent in a `json-parse-buffer' result.

`json-parse-buffer' represents JSON null as `:null' and JSON false as
`:false', so a plain `null' check is not sufficient."
  (or (null value) (eq value :null) (eq value :false)))

(defun vscode-cp-proxy--deepinfra-flex-p (model)
  "Return non-nil if MODEL is a flex capable text generation model."
  (and (equal (gethash "type" model) "text-generation")
       (seq-contains-p (gethash "tags" model) "flex")
       (vscode-cp-proxy--deepinfra-absent-p (gethash "replaced_by" model))
       (vscode-cp-proxy--deepinfra-absent-p (gethash "deprecated" model))))

(defun vscode-cp-proxy--deepinfra-flex-savings (pricing)
  "Return the flex savings percentage string from PRICING, or nil."
  (let ((priority (gethash "rate_per_service_tier_priority" pricing))
        (flex (gethash "rate_per_service_tier_flex" pricing)))
    (when (and (numberp priority) (numberp flex) (> priority 0))
      (format "%d%% flex savings" (round (* 100 (/ (- priority flex) priority)))))))

(defun vscode-cp-proxy--deepinfra-format-days-ago (value)
  "Return the age of the ISO 8601 timestamp VALUE as \"NNNN day(s) ago\".

Returns an empty string when VALUE is absent or not a timestamp."
  (if (and (stringp value) (string-match "\\`\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\)" value))
      (let ((days (- (time-to-days (current-time))
                     (time-to-days (date-to-time (match-string 1 value))))))
        (format "%04d day%s ago" days (if (= days 1) "" "s")))
      ""))

(defun vscode-cp-proxy--deepinfra-format-date (value)
  "Return the YYYY-MM-DD part of the ISO 8601 timestamp VALUE.

Returns an empty string when VALUE is absent or not a timestamp."
  (if (and (stringp value) (string-match "\\`\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\)" value))
      (match-string 1 value)
      ""))

(defun vscode-cp-proxy--deepinfra-format-price (value)
  "Format VALUE as a plain decimal price string.

Emacs prints small floats in scientific notation, for example 9e-06.
This function expands them to a plain decimal, for example 0.000009,
and removes trailing zeros."
  (let ((text (format "%0.8f" value)))
    (when (string-match "\\." text)
      (setq text (string-remove-suffix "." (string-remove-suffix "0" (replace-regexp-in-string "0+\\'" "" text)))))
    text))

(defun vscode-cp-proxy--deepinfra-model-display (model)
  "Return a display string for MODEL."
  (let* ((name (gethash "model_name" model))
         (tags (gethash "tags" model))
         (pricing (gethash "pricing" model))
         (discount (gethash "discount" pricing))
         (max-tokens (gethash "max_tokens" model))
         (quantization (gethash "quantization" model))
         (input (gethash "cents_per_input_token" pricing))
         (output (gethash "cents_per_output_token" pricing))
         (cached (gethash "rate_per_input_token_cached" pricing))
         (savings (vscode-cp-proxy--deepinfra-flex-savings pricing))
         (create-ts (gethash "create_ts" model))
         (parts (list (vscode-cp-proxy--deepinfra-format-days-ago create-ts)
                      (format "%s" name)
                      (format "(featured=%s)" (if (seq-contains-p tags "featured") "true" "false"))
                      (format "(partner=%s)" (if (eq (gethash "is_partner" model) t) "true" "false")))))
    (when (numberp discount)
      (setq parts (append parts (list (format "(%d%% discount)" (round (* 100 discount)))))))
    (when savings
      (setq parts (append parts (list (format "(%s)" savings)))))
    (when (numberp max-tokens)
      (setq parts (append parts (list (format "(max_tokens=%s)" max-tokens)))))
    (when (and quantization (not (vscode-cp-proxy--deepinfra-absent-p quantization)))
      (setq parts (append parts (list (format "(quant=%s)" quantization)))))
    (when (numberp input)
      (setq parts (append parts (list (format "input $%s" (vscode-cp-proxy--deepinfra-format-price input))))))
    (when (numberp output)
      (setq parts (append parts (list (format "output $%s" (vscode-cp-proxy--deepinfra-format-price output))))))
    (when (numberp cached)
      (setq parts (append parts (list (format "cache %s" (vscode-cp-proxy--deepinfra-format-price cached))))))
    (setq parts (append parts (list (vscode-cp-proxy--deepinfra-format-date create-ts))))
    (mapconcat #'identity parts " ")))

(defun vscode-cp-proxy-deepinfra-flex-models ()
  "Fetch and filter the DeepInfra model list for flex capable models.

Returns a list of (MODEL-NAME . DISPLAY) pairs."
  (let* ((json
          (with-current-buffer
              (url-retrieve-synchronously vscode-cp-proxy-deepinfra-models-url)
            (beginning-of-buffer)
            (search-forward-regexp "\r?\n\r?\n")
            (json-parse-buffer)))
         (models (seq-filter #'vscode-cp-proxy--deepinfra-flex-p (append json nil))))
    (seq-map
     (lambda (m)
       (cons (gethash "model_name" m)
             (vscode-cp-proxy--deepinfra-model-display m)))
     models)))

(defun vscode-cp-proxy-select-deepinfra-flex-model ()
  "Prompt for a DeepInfra flex capable model and return its id.

Models are discovered from `vscode-cp-proxy-deepinfra-models-url' and
filtered to text generation models that carry the \"flex\" tag and are
neither replaced nor deprecated.

The caller is responsible for building the backend object from the
returned model id."
  (let* ((models (vscode-cp-proxy-deepinfra-flex-models))
         (model-displays (sort (seq-map #'cdr models) #'string<))
         (display-width (apply #'max 0 (seq-map #'length model-displays)))
         (model-displays (seq-map
                          (lambda (d) (format (format "%%-%ds" display-width) d))
                          model-displays))
         (selection (completing-read "Select model: " model-displays nil t)))
    (car (rassoc (string-trim-right selection) models))))

(provide 'vscode-cp-proxy)

;;; vscode-cp-proxy.el ends here
