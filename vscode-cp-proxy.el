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

(defvar vscode-cp-proxy-token nil)
(defvar vscode-cp-proxy-host-port "127.0.0.1:5555")

(defun vscode-cp-proxy-set-token ()
  (interactive)
  
  (when (or (called-interactively-p 'any) (not vscode-cp-proxy-token))
    (setq
     vscode-cp-proxy-token
     (string-replace
      "'"
      ""
      (string-trim (read-from-minibuffer "Enter the token. (I'll remove the quotes.) ")))))
  
  vscode-cp-proxy-token)

(defun vscode-cp-proxy-set-gptel-backend (prefix)
  (interactive "P")

  (call-interactively 'vscode-cp-proxy-set-token)

  (let* (
         (url-request-extra-headers
          `(("Authorization" .
             ,(format "Bearer %s" vscode-cp-proxy-token))))
         (models-json
          (with-current-buffer
              (url-retrieve-synchronously (format "http://%s/openai/v1/models" vscode-cp-proxy-host-port))
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
       :host vscode-cp-proxy-host-port
       :protocol "http"
       :endpoint "/openai/v1/chat/completions"
       :stream (not prefix)
       :key 'vscode-cp-proxy-set-token
       :models (mapcar 'intern models)
       ))))

(provide 'vscode-cp-proxy)

;;; vscode-cp-proxy.el ends here
