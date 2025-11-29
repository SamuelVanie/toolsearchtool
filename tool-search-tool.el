;;; tool-search-tool.el --- summary -*- lexical-binding: t -*-

;; Author: samuelvanie
;; Maintainer: samuelvanie
;; Version: 1.0
;; Package-Requires: (gptel)
;; Homepage: homepage
;; Keywords: keywords


;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
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

;; commentary

;;; Code:

(require 'gptel)
(eval-when-compile (require 'cl-lib))

;;; The embeddings values for the different tools
(defvar toolsearchtool--embeddings nil)

(defun toolsearchtool--get-embedding (tool)
  "Get the embedding vector of a tool.
The tool description is built using `toolsearchtool--build-message'"
  )

(defun toolsearchtool--build-string (tool)
  "Build the description of the tool.
The tool structure is gotten from the variable `gptel--known-tools'"
  (let ((tool-name (first tool))
	(tool-struct (rest tool)))
    (concat
     (format "Tool: %s\n" tool-name)
     (format "Category: %s\n" (gptel-tool-category tool-struct))
     (format "Description: %s\n" (gptel-tool-description tool-struct))
     (format "Parameters: ")
     (mapconcat (lambda (param)
		  (format "%s (%s): %s"
			  (plist-get param :name)
			  (plist-get param :type)
			  (plist-get param :description)))
		(gptel-tool-args tool-struct) ",")
     )
    )
  )
    

(defun toolsearchtool--get-tools-suggestion ()
  "Call the embedding model from the url defined by the user then calculate
the cosine similarity to get the appropriate
tools that will be returned to the model"
  
  )

(provide 'tool-search-tool)

;;; tool-search-tool.el ends here
