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
(require 'url)
(require 'json)


(defun dot-product (vec1 vec2)
  "Calculate dot product of VEC1 and VEC2."
  (apply #'+ (cl-mapcar #'* vec1 vec2)))

(defun magnitude (vec)
  "Calculate magnitude (length) of vector VEC."
  (sqrt (apply #'+ (mapcar (lambda (x) (* x x)) vec))))

(defun cosine-similarity (vec1 vec2)
  "Calculate cosine similarity between VEC1 and VEC2.
Returns a value between -1 and 1, where 1 means identical direction."
  (let ((dot (dot-product vec1 vec2))
        (mag1 (magnitude vec1))
        (mag2 (magnitude vec2)))
    (if (or (zerop mag1) (zerop mag2))
        0.0
      (/ dot (* mag1 mag2)))))


(defgroup toolsearchtool nil
  "Configuration for embedding requests."
  :group 'tools)

;;; The embedding values for the different tools
(defvar toolsearchtool--embedding-values nil)

;;; The calculated cosine similarities with the last query
(defvar toolsearchtool--cosine-similarities nil)

(defcustom toolsearchtool-embedding-cache-file
  (locate-user-emacs-file ".cache/toolsearchtool-embeddings.eld")
  "File to cache emdedding values."
  :type 'file
  :group 'toolsearchtool)

(defcustom toolsearchtool-number-of-results 4
  "The number of tools to return to the llm after the query"
  :type 'number
  :group 'toolsearchtool)

(defun toolsearchtool--save-embeddings ()
  "Save embedding values to cache file."
  (when toolsearchtool--embedding-values
    (with-temp-file toolsearchtool-embedding-cache-file
      (insert " ;;; -*- lisp-data -*-\n")
      (prin1 toolsearchtool--embedding-values (current-buffer))
      (insert "\n"))
    (message "Saved embeddings to %s" toolsearchtool-embedding-cache-file)))

;;;###autoload
(defun toolsearchtool--load-embeddings ()
  "Load the embeddings from cache file."
  (when (file-exists-p toolsearchtool-embedding-cache-file)
    (condition-case err
	(with-temp-buffer
	  (insert-file-contents toolsearchtool-embedding-cache-file)
	  (goto-char (point-min))

	  ;; skip the header comment if present
	  (when (looking-at ";;;")
	    (forward-line 1))
	  
	  (setq toolsearchtool--embedding-values (read (current-buffer)))
	  
	  (message "Loaded %d embeddings from cache"
		   (length toolsearchtool--embedding-values)) t)
      (error
       (message "Failed to load embeddings cache: %s" (error-message-string err))
       nil))))

(defcustom toolsearchtool-embedding-endpoint "http://localhost:1234/v1/embeddings"
  "The full URL for the embedding endpoint."
  :type 'string
  :group 'toolsearchtool)

(defcustom toolsearchtool-embedding-model "text-embedding-qwen3-embedding-4b"
  "The model name to send in the request body."
  :type 'string
  :group 'toolsearchtool)

(defcustom toolsearchtool--get-available-tools #'toolsearchtool--default-get-available-tools
  "A function that list all the tools that exists.
Check out my default implementation for gptel `toolsearchtool--default-get-available-tools'"
  :type 'function
  :group 'toolsearchtool)


(defun toolsearchtool--default-get-available-tools ()
  "For gptel all, the known tools are stored inside the `gptel--known-tools' variable.
Returns the tools list in the format :
(
(\"toolname\" . #s(gptel cl-X))
(\"secondtool\" ...)
)
This is the format that I use in the code base. You could reimplement all the functions
tied to gptel, check the customize group `toolsearchtool--embedding'"
  (cl-loop for (_category . tools) in gptel--known-tools
	   append tools
	   )
  )

(defun toolsearchtool--get-embedding (text)
  "Send TEXT to the configured embedding endpoint and return the vector.
This function is synchronous and blocks until the request completes."
  (let ((url-request-method "POST")
        (url-request-extra-headers
         '(("Content-Type" . "application/json")))
        (url-request-data
         (json-encode `((input . ,text)
                        (model . ,toolsearchtool-embedding-model)))))
    
    (let ((buffer (url-retrieve-synchronously toolsearchtool-embedding-endpoint)))
      (if (not buffer)
          (error "Failed to retrieve embedding: Connection failed")
        (with-current-buffer buffer
          (goto-char (point-min))
          (if (search-forward "\n\n" nil t)
              (let* ((json-object-type 'plist) ;; Ensure we get plists
                     (json-array-type 'list)
                     (response (json-read))
                     ;; Parse: response -> data -> [0] -> embedding
                     (data (plist-get response :data))
                     (first-item (car data))
                     (embedding (plist-get first-item :embedding)))
                (kill-buffer buffer)
                (unless embedding
                  (error "Response did not contain an embedding field: %S" response))
                embedding)
            (kill-buffer buffer)
            (error "Invalid response headers")))))))

(defun toolsearchtool-compute-embedding (toolname)
  "Calculate the embedding value for a tool.
The user just have to give the name of the tool.  This function is
intended to be called by the user after he register a new tool.  Could
also be called if for some reason the user wants to recalculate
embedding for one tool."
  (interactive)
  (if (not (map-contains-key toolsearchtool--embedding-values toolname))
      ;; vector not already registered
      ;; then add it
      (setq toolsearchtool--embedding-values
	    (acons toolname vector toolsearchtool--embedding-values))
    ;; else
    
    )
  )

;;;###autoload
(defun toolsearchtool-compute-all-embedding ()
  "Get all the tools available then compute their embedding values and save them."
  (interactive)
  (when (or (not toolsearchtool--embedding-values)
	    (and toolsearchtool--embedding-values
		 (y-or-n-p "Recompute all the embedding values ?")))
    (setq toolsearchtool--embedding-values
	  (cl-loop for (toolname . toolstruct) in (funcall toolsearchtool--get-available-tools)
	     collect (cons toolname
			  (toolsearchtool--get-embedding
			   (toolsearchtool--build-string toolstruct)
			   ))
	     )
	  )
    (toolsearchtool--save-embeddings)
    )
  )

(defun toolsearchtool--compute-all-similarities (queryvector)
  "Compute the cosine similarities between the query's vector and the tools'
ones. We will then be able to choose the most relevant ones. Check the
vectors' values in the variable `toolsearchtool--embedding-values'"
  (setq toolsearchtool--cosine-similarities
	(cl-loop for (toolname . toolvector) in toolsearchtool--embedding-values
		 collect (cons toolname
			       (cosine-similarity queryvector toolvector))
		 )
	)

  ;; sort in order
  (setq toolsearchtool--cosine-similarities
	(sort toolsearchtool--cosine-similarities
	      (lambda (a b) (> (rest a) (rest b)))
	      )
	)
  )

(defun toolsearchtool--build-string (toolstruct)
  "Build the description of the tool.
The tool structure is the one from `gptel--known-tools'"
    (concat
     (format "Tool: %s\n" (gptel-tool-name toolstruct))
     (format "Category: %s\n" (gptel-tool-category toolstruct))
     (format "Description: %s\n" (gptel-tool-description toolstruct))
     (format "Parameters: ")
     (mapconcat (lambda (param)
		  (format "%s (%s): %s"
			  (plist-get param :name)
			  (plist-get param :type)
			  (plist-get param :description)))
		(gptel-tool-args toolstruct) ",")
     )
    )


(defun toolsearchtool--get-tools-suggestion (query)
  "Call the embedding model from the url defined by the user then calculate
the cosine similarity to get the appropriate tools and return the
appropriate tools to be selected for the query."

  ;; Try to load cached embeddings first
  (unless toolsearchtool--embedding-values
    (unless (toolsearchtool--load-embeddings)
      ;; If loading failed, compute them
      (toolsearchtool-compute-all-embedding)))


  ;; get the embedding for the user's query
  ;; compute the cosine similarities with the tools' vectors
  (toolsearchtool--compute-all-similarities (toolsearchtool--get-embedding query))

  ;; get the first n tools
  ;; n defined by a custom variable
  (take toolsearchtool-number-of-results toolsearchtool--cosine-similarities)
  )

(provide 'tool-search-tool)

;;; tool-search-tool.el ends here
