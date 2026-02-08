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

(defcustom vscode-cp-proxy-token nil
  "Token to use for authentication with vscode-cp-proxy server.

If nil ask for a token when activating.
If a string, this token is used.
If a function, it should produce the token value.

The token can be found is vs code in the output panel (View > Output)
in the vscode-cp-proxy log (choose via drop down menu)."
  :type '(choice  'string 'funtion)
  :group 'vscode-cp-proxy)

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
         (models (seq-map
                  (lambda (e) (gethash "id" e))
                  (gethash "data" models-json))))
    (setq gptel-model
          (intern (completing-read "Select model: " models nil t)))
    (setq
     gptel-backend
     (gptel-make-openai "gptel-vscode-cp-proxy"
       :host (format "%s:%s" vscode-cp-proxy-host vscode-cp-proxy-port)
       :protocol "http"
       :endpoint "/openai/v1/chat/completions"
       :stream (not prefix)
       :key 'vscode-cp-proxy-set-token
       :models (mapcar 'intern models)
       ))))

(provide 'vscode-cp-proxy)

;;; vscode-cp-proxy.el ends here
