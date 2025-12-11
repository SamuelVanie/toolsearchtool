;;; tool-search-tool.el --- summary -*- lexical-binding: t -*-

;; Author: samuelvanie
;; Maintainer: samuelvanie
;; Version: 1.0
;; Package-Requires: (gptel)
;; Homepage: homepage
;; Keywords: tools, ai


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


(defun toolsearchtool--parse-embedding-response (buffer)
  "Parse embedding response from BUFFER.
Returns the embedding vector or nil on error."
  (with-current-buffer buffer
    (goto-char (point-min))
    (if (search-forward "\n\n" nil t)
        (condition-case err
            (let* ((json-object-type 'plist)
                   (json-array-type 'list)
                   (response (json-read))
                   (data (plist-get response :data))
                   (first-item (car data))
                   (embedding (plist-get first-item :embedding)))
              (if embedding
                  embedding
                (progn
                  (message "Response did not contain an embedding field")
                  nil)))
          (error
           (message "Error parsing embedding response: %S" err)
           nil))
      (progn
        (message "Invalid response headers")
        nil))))

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

(defcustom toolsearchtool--select-tools #'toolsearchtool--default-select-tools
  "The tool that the llm will use to add a new tool to the list of tools."
  :type 'function
  :group 'toolsearchtool)


(defcustom toolsearchtool--remove-tools #'toolsearchtool--default-remove-tools
  "The tool that the llm will use to delete a tool from the list of tools."
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


(defun toolsearchtool--get-embedding (text callback)
  "Send TEXT to the configured embedding endpoint asynchronously.
CALLBACK is called with the embedding vector when the request completes,
or with nil if the request fails. Shows progress messages in minibuffer."
  (let ((url-request-method "POST")
        (url-request-extra-headers
         '(("Content-Type" . "application/json")))
        (url-request-data
         (json-encode `((input . ,text)
                        (model . ,toolsearchtool-embedding-model)))))
    (url-retrieve
     toolsearchtool-embedding-endpoint
     (lambda (status)
       (unwind-protect
           (if (plist-get status :error)
               ;; Handle error case
               (progn
                 (message "Embedding request failed: %S" (plist-get status :error))
                 (funcall callback nil))
             ;; Handle success case by using the parser function
             (let ((embedding (toolsearchtool--parse-embedding-response (current-buffer))))
               (funcall callback embedding)))
         (kill-buffer (current-buffer)))))))


(defun toolsearchtool-compute-embedding (toolname)
  "Calculate the embedding value for a tool.
The user just have to give the name of the tool. This function is
intended to be called by the user after he register a new tool. Could
also be called if for some reason the user wants to recalculate
embedding for one tool."
  (interactive
   (let ((tools (mapcan (lambda (cat) (mapcar #'car (cdr cat)))
                        gptel--known-tools)))
     (list (intern (completing-read "Tool name: " tools nil t)))))
  (let* ((tool-symbol (if (stringp toolname) (intern toolname) toolname))
         (tool-name-str (symbol-name tool-symbol))
         (tool-struct
          (catch 'found
            (dolist (cat gptel--known-tools)
              (let ((match (assoc tool-name-str (cdr cat))))
                (when match (throw 'found (cdr match))))))))
    (unless tool-struct
      (error "Tool structure not found for %s in gptel--known-tools" tool-symbol))
    (when (and (map-contains-key toolsearchtool--embedding-values tool-symbol)
               (not (y-or-n-p (format "Embedding for %s already exists. Recompute? " tool-symbol))))
      (user-error "Aborted recomputation of embedding"))
    (message "Computing embedding for %s..." tool-symbol)
    (toolsearchtool--get-embedding
     (toolsearchtool--build-string tool-struct)
     (lambda (vector)
       (if vector
           (progn
             (setf (alist-get tool-symbol toolsearchtool--embedding-values) vector)
             (message "Embedding updated for %s" tool-symbol))
         (message "Failed to compute embedding for %s" tool-symbol))))))



;;;###autoload
(defun toolsearchtool-compute-all-embedding ()
  "Get all the tools available then compute their embedding values and save them."
  (interactive)
  (when (or (not toolsearchtool--embedding-values)
            (and toolsearchtool--embedding-values
                 (y-or-n-p "Recompute all the embedding values ?")))
    (let* ((all-tools (funcall toolsearchtool--get-available-tools))
           (total (length all-tools))
           (completed 0)
           (results nil))
      (message "Computing embeddings for %d tools..." total)
      
      (dolist (tool-pair all-tools)
        (let ((toolname (first tool-pair))
              (toolstruct (rest tool-pair)))
          (toolsearchtool--get-embedding
           (toolsearchtool--build-string toolstruct)
           (lambda (embedding)
             (setq completed (1+ completed))
             (when embedding
               (push (cons toolname embedding) results))
             (message "Progress: %d/%d tools completed" completed total)
             
             ;; When all requests are done, save results
             (when (= completed total)
               (setq toolsearchtool--embedding-values (nreverse results))
               (toolsearchtool--save-embeddings)
               (message "All embeddings computed and saved!")))))))))


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

(defun toolsearchtool--default-select-tools (list)
  "Add tools to the list of tools to be used by the llm.
This is the default gptel implementation that could be customized through the
variable `toolsearchtool--select-tools'"
  (let ((tools (funcall toolsearchtool--get-available-tools)))
    (cl-loop for tool in list
             do (let ((toolstruct (assoc tool tools)))
                  (if toolstruct
                      (unless (member (rest toolstruct) gptel-tools)
                        (setq gptel-tools (cons (rest toolstruct) gptel-tools))))))
    )
  
  (let ((gptel-buffer (cl-find-if (lambda (buf)
                                    (and (ignore-errors (buffer-local-value 'gptel-mode buf))))
                                  (buffer-list))))
    (when gptel-buffer
      (with-current-buffer gptel-buffer
        (gptel-abort gptel-buffer)
        (end-of-buffer)
        (gptel-send))))
  (format "List of tools successfully addded : %s" (mapconcat 'identity list "\n")))

(defun toolsearchtool--default-remove-tools (names)
  "Remove a tool from the tool list.
This is the default gptel implementation. See the variable
`toolsearchtool--remove-tools'"
  (cl-loop for name in names
	   do (setq gptel-tools (seq-filter
				 (lambda (x) (not (string= (gptel-tool-name x) name)))
				 gptel-tools))
	   )
  (let ((gptel-buffer (cl-find-if (lambda (buf)
				    (and (ignore-errors (buffer-local-value 'gptel-mode buf))))
				  (buffer-list))))
    (when gptel-buffer
      (with-current-buffer gptel-buffer
	(gptel-abort gptel-buffer)
	(end-of-buffer)
	(gptel-send))))
  (format "List of tools successfully removed : %s" (mapconcat 'identity names "\n"))
  )



(defun toolsearchtool--get-tools-suggestion (query)
  "Call the embedding model from the url defined by the user then calculate
the cosine similarity to get the appropriate tools and return the
appropriate tools to be selected for the query.
This function uses url-retrieve-synchronously for a single query."
  
  ;; Try to load cached embeddings first
  (unless toolsearchtool--embedding-values
    (unless (toolsearchtool--load-embeddings)
      (error "The embeddings for the tools are not yet computed, run the command
toolsearchtool-compute-all-embedding first")))
  
  (let ((url-request-method "POST")
        (url-request-extra-headers
         '(("Content-Type" . "application/json")))
        (url-request-data
         (json-encode `((input . ,query)
                        (model . ,toolsearchtool-embedding-model)))))
    (let ((buffer (url-retrieve-synchronously toolsearchtool-embedding-endpoint)))
      (if (not buffer)
          (error "Failed to retrieve embedding: Connection failed")
        (unwind-protect
            (let ((query-vector (toolsearchtool--parse-embedding-response buffer)))
              (unless query-vector
                (error "Failed to parse embedding response"))
              
              ;; Compute the cosine similarities with the tools' vectors
              (toolsearchtool--compute-all-similarities query-vector)
              ;; Get the first n tools
              (take toolsearchtool-number-of-results toolsearchtool--cosine-similarities))
          (kill-buffer buffer))))))


(gptel-make-tool
 :name "tool_search"
 :function #'toolsearchtool--get-tools-suggestion
 :description "Search for available tools that can help with a task. Returns tool
definitions for matching tools. Use this when you need a tool but don't
have it available yet."
 :include t
 :args (list '(:name "query"
                     :type string
		     :description "Describe the task you're looking for"))
 :category "tool_management")

(gptel-make-tool
 :name "select_tools"
 :function (lambda (list)
             (funcall toolsearchtool--select-tools (append list nil)))
 :description "Add tools to your current workspace. Use this after you've searched for
the appropriate tools using tool_search."
 :include t
 :args (list '(:name "list"
		     :type array
		     :items (:type string)
		     :description "The list of tools to add"))
 :category "tool_management"
 )

(gptel-make-tool
 :name "remove_tools"
 :function (lambda (list)
	     (funcall toolsearchtool--remove-tools (append list nil)))
 :description "Remove tools from the current workspace. Use this when you no more needs
those tools for the next task."
 :include t
 :args (list '(:name "list"
		     :type array
		     :items (:type string)
		     :description "The list of tools to remove"))
 :category "tool_management"
 )


(provide 'tool-search-tool)

;;; tool-search-tool.el ends here
