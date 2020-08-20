;; -*- lexical-binding: t; -*-
(require 'seq)
(require 'cl-lib)
(require 'all-the-icons)

;;; Global Variables and Constants
;;
;;
;; Display
(defvaralias 'star-tabs-header-line-format
  (if (boundp 'tab-line-format)
      'tab-line-format 
    'header-line-format)
  "Header line format for display of the tab bar")
(defvar star-tabs-header-line 'header-line
  "Header line where tabs are displayed")


;; Tab bar dividers 
(defvar star-tabs-left-margin "  " 
  "Space used to the left of the tab bar.")
(defvar star-tabs-right-margin " "
  "Space used to the right of the tab bar.")
(defvar star-tabs-tab-separator " "
  "Tab bar divider that separates tabs.")
(defvar star-tabs-number-name-separator " "
  "Tab bar divider that separates the buffer number and buffer name in a tab.")
(defvar star-tabs-name-modified-symbol-separator " "
  "Tab bar divider that separates the buffer number and buffer name in a tab.")
(defvar star-tabs-modified-symbol-close-button-separator " " 
  "Tab bar divider that separates the buffer number and buffer name in a tab.")
(defvar star-tabs-filter-name-number-separator "   "
  "Tab bar divider that separates the name of the active filter group and the first tab.")

;; Tab symbols
(defvar star-tabs-modified-buffer-symbol "*"
  "Tab symbol for modified buffers.")
(defvar star-tabs-unmodified-buffer-symbol "+"
  "Tab symbol for unmodified buffers.")
(defvar star-tabs-close-buffer-symbol "x"
  "Tab symbol for the tab close button")

;; Keymaps
(defvar star-tabs-map-select-tab
  (let ((map (make-sparse-keymap)))
    (define-key map (vector star-tabs-header-line 'mouse-1) 'star-tabs-switch-to-buffer-on-click)
    map)
  "Mouse keymap for select tab button")
(defvar star-tabs-map-close-tab
  (let ((map (make-sparse-keymap)))
    (define-key map (vector star-tabs-header-line 'mouse-1) 'star-tabs-close-buffer-on-click)
    map)
  "Mouse keymap for close tab button")

;; Global state variables
(defvar star-tabs-current-buffer nil
  "Helper variable to for function (star-tabs-buffer-switched-p) to keep track of the current 'real' buffer.
A 'real' or 'active' buffer refers open buffers that are not ephemeral/temporary or otherwise deemed unimportant.")
(defvar star-tabs-current-filter nil
  "Helper variable for (star-tabs-filter-changed-p) to keep track of when a filter changes.")
(defvar star-tabs-modified-state-changed-buffer-table (make-hash-table :test #'equal)
  "Store whether a buffer has been modified.")
(defvar star-tabs-last-timer nil
  "The last used timer. Set automatically by star-tabs-set-temporarily.")
(defvar star-tabs-debug-messages nil)

;; Collections
(defvar star-tabs-filter-collections nil
  "List of all filter collections. car of list represents the currently active collection in the tab bar.")

;; Filters
(defvar star-tabs-global-inclusion-prefix-filter nil
  "List of buffer name prefixes to be included globally. Buffers filtered this way will be cached and ignored
for all future searches. As such, global filtering may increase performance, and
should (and should only!) be applied to buffers that you really don't care about.

Buffers with the space prefix (\" \") are automatically filtered before this filter is applied, and thus cannot  
be included.

This filter is applied before star-tabs-global-exclusion-prefix-filter.")
(defvar star-tabs-global-exclusion-prefix-filter '("magit-"
					    "magit:"
					    "*Help"
					    "*WoM")
  "List of buffer name prefixes to be excluded globally. Buffers filtered this way will be cached and ignored
for all future searches. As such, global filtering may increase performance, and
should (and should only!) be applied to buffers that you really don't care about.

Buffers with the space prefix (\" \") are automatically filtered before this filter is applied, and thus need not
be added to this list.

This filter is applied after star-tabs-global-inclusion-prefix-filter.")
(defvar star-tabs-tab-bar-filter-name nil
  "Filter name to be displayed in the tab bar. Automatically set by other functions.")
(defvar star-tabs-cached-filtered-buffers (make-hash-table :test #'equal)
  "Cache globally filtered buffers to improve performance.")

;; File extension filters
(defvar star-tabs-add-file-extension-filters nil
  "If non-nil, file extension filters will be added to the collection. This variable is set and
controlled by functions, and depends on collection-specific configuration.")
(defcustom star-tabs-file-ext-filter-buffer-threshold 15
  "When the total number of buffers after global filters have been applied reaches or exceeds 
the number set in this variable, STAR-TABS-SET-FILE-EXTENSION-FILTERS is automatically set to t,
and file extension filters are subsequently added. If the buffer count goes down below the threshold again,
STAR-TABS-SET-FILE-EXTENSION-FILTERS is then set to nil, and all automatically added file extension filters removed.
Deactivate this feature by setting this variable to 0."
  :type 'int)
(defvar star-tabs-file-extension-filter-names nil
  "Automatically added file-extension filters. This is a helper variable for the automatic file extension filters")

;; Buffers
(defvar star-tabs-active-filtered-buffers-enum nil
  "Enumerated list of buffers after all filters have been applied.")
(defvar star-tabs-active-buffers nil
  "List of all currently active/'real' buffers.
A 'real' or 'active' buffer refers open buffers that are not ephemeral/temporary or otherwise deemed unimportant.")
(defvar star-tabs-buffers-enum nil
  "Alist of enumerated buffers for all filters. 
Key is filter name, value is an enumerated list of buffers.")

;;; Visuals
(defvar star-tabs-tab-bar-height 220)
(defvar star-tabs-tab-bar-text-height 150
  "Text height for tabs.")
(defvar star-tabs-tab-bar-filter-name-foreground "#ef21b3"
  "Foreground color for tab bar filter name.")
(defvar star-tabs-tab-bar-selected-background "#202020"
  "Background color for selected tab.")
(defvar star-tabs-tab-bar-selected-foreground "#a3c9e7"
  "Foreground color for selected tab.")
(defvar star-tabs-tab-bar-non-selected-background "#262626"
  "Background color for non-selected tabs.")
(defvar star-tabs-tab-bar-non-selected-foreground "#e1e1e1"
  "Foreground color for non-selected tabs.")

;; Faces
(defface star-tabs-tab-bar-left-margin
  `(( t
      (
       :height ,star-tabs-tab-bar-height
       :background ,star-tabs-tab-bar-non-selected-background)))
  "Face for left margin of the header-line which acts as the height-setter for the entire header-line")
(defface star-tabs-filter-name
  `((t
     (
      :background ,star-tabs-tab-bar-non-selected-background
      :foreground ,star-tabs-tab-bar-filter-name-foreground
      :height ,star-tabs-tab-bar-text-height)))
  "Face for displaying filter-name in the tab bar")
(defface star-tabs-non-selected-tab
  `((t (
	:background ,star-tabs-tab-bar-non-selected-background
	:foreground ,star-tabs-tab-bar-non-selected-foreground
	:height ,star-tabs-tab-bar-text-height)))
  "Face for displaying filter-name in the tab bar")
(defface star-tabs-selected-tab
  `((t
     (
      :background ,star-tabs-tab-bar-selected-background
      :foreground ,star-tabs-tab-bar-selected-foreground
      :height ,star-tabs-tab-bar-text-height)))
  "Face for displaying filter-name in the tab bar")
(defface star-tabs-non-selected-icon
  `((t (
	:background ,star-tabs-tab-bar-non-selected-background
	:height ,star-tabs-tab-bar-text-height)))
  "Face for displaying non-selected icon in the tab bar")
(defface star-tabs-selected-icon
  `((t
     (
      :background ,star-tabs-tab-bar-selected-background
      :height ,star-tabs-tab-bar-text-height)))
  "Face for displaying selected iconin the tab bar")

;;; Functions
;;

;;; Utility
(defun star-tabs-cycle-list-car (list &optional reverse)
  "Cycle (move forward, or backward if REVERSE is non-nil) through list LIST. Return the modified list."
(or reverse (setq reverse nil))
  (let (cycled-list)
    (if reverse
	(setq cycled-list
	      (append (list (car (reverse list))) (reverse (cdr (reverse list)))))
      (setq cycled-list (append (cdr list) (list (car list)))))
    cycled-list))
(defun star-tabs-left-of-elt (list elt)
  "Return the element to the left of element ELT in list LIST. If ELT is car of LIST, return last element of LIST.
Return nil if ELT is not in LIST."
  (if (member elt list)
    (let ((left-elt (cadr (member elt (reverse list)))))
      (or left-elt (car (reverse list))))))
(defun star-tabs-insert-at-nth (list elt n)
  "Insert element ELT in list LIST at position N. Return the modified list."
  (let ((nth-from-end (- (length list) n)))
    (append (reverse (nthcdr nth-from-end (reverse list))) (list elt) (nthcdr n list))))
(defun star-tabs-flatten-alist (alist)
  "Flattens an alist by removing keys and keeping values"
  (let ((flattened-list nil))
    (dolist (item alist flattened-list)
      (dolist (value (cdr item) flattened-list)
	(push value flattened-list)))
    (delq nil (reverse flattened-list))))

;; TODO Display collection name in tab bar temporarily when switched.
;;; Filter Collections
(defun star-tabs-create-filter-collection (&rest collection-props)
  "Create a filter collection. Available properties are:
:name - the name of the collection.
:use - if non-nil, switch to collection directly after creation (default nil).
:hide-close-buttons - if non-nil, hide tab close buttons (default nil).
:display-filter-name - if non-nil, display the name of the currently active filter in the tab bar (default nil).
:enable-file-extension-filters - if non-nil, enable filter groups in the collection for all file extensions 
among currently open buffers (default nil).
:collection-name-prefix - the prefix of the name of the collection, used in variable names. Individual collections can be 
identified by the symbol name (intern(concat collection-name-prefix name)). (default \"star-tabs-filter-collection\")."
  (let* ((use (plist-get collection-props :use))
	 (enable-file-extension-filters (plist-get collection-props :enable-file-extension-filters))
	 (hide-close-buttons (plist-get collection-props :hide-close-buttons))
	 (display-filter-name (plist-get collection-props :display-filter-name))
	 (collection-name-prefix (or (plist-get collection-props :collection-name-prefix) "star-tabs-filter-collection-" ))
	 (name (intern (concat collection-name-prefix (plist-get collection-props :name))))
	 (collection `(,name :enable-file-extension-filters ,enable-file-extension-filters
			     :display-filter-name ,display-filter-name
			     :collection-name-prefix ,collection-name-prefix
			     :last-filter nil)))

    (if (not (member name (star-tabs-filter-collection-names)))
	(progn (set name nil) 
	       (setq star-tabs-filter-collections
		     (append star-tabs-filter-collections (list collection)))
	       ;; Switch to the new collection upon creation if :use is non-nil.
	       (when use
		 (while (not (eq (star-tabs-active-filter-collection) name))
		   (star-tabs-cycle-filter-collection t))))
      (message "Collection name already exists"))))
(defun star-tabs-filter-collection-names ()
  "Return the names of all filter collections."
  (mapcar 'car star-tabs-filter-collections))

;; FIXME currently creating collection while the list is rotated causes the collections to be in the wrong order
;; Solve this by rotating the list back to the beginning (store first added list, move to second if the first is deleted etc)
;; Or just keeping track of last added..?
(defun star-tabs-remove-filter-collection (collection)
  "Delete filter collection COLLECTION."
  (if (>= (length star-tabs-filter-collections) 2)
      (let ((prefix (star-tabs-get-filter-collection-prop-value :collection-name-prefix collection)))
	;; makunbound will cause problems if we're removing the currently active collection,
	;; so make another collection active first.
	;; BEWARE: If for some reason in the future, star-tabs-cycle-filter-collection has the ability to skip collections,
	;; we might inadvertently, despite cycling, end up deleting COLLECTION when it's currently active. 
	(when (eq collection (star-tabs-active-filter-collection))
	  (star-tabs-cycle-filter-collection))
	(setq star-tabs-filter-collections (remove
				     (nth (cl-position collection (star-tabs-filter-collection-names)) star-tabs-filter-collections)
				     star-tabs-filter-collections))
	(makunbound collection))
  (message "Cannot delete last collection. Make another collection before attempting to delete this one.")))
(defun star-tabs-cycle-filter-collection (&optional reverse inhibit-refresh)
  "Cycle (move forward, or backward if REVERSE is non-nil) through filter collections.
Also refresh tab bar if INHIBIT-REFRESH is non-nil."
  (setq star-tabs-filter-collections (star-tabs-cycle-list-car star-tabs-filter-collections reverse))
  (star-tabs-display-tab-bar))
(defun star-tabs-active-filter-collection ()
  "Return the name of the active filter collection."
  (car (car star-tabs-filter-collections)))
(defun star-tabs-get-filter-collection-prop-value (prop &optional collection)
"Return the value of property PROP in filter collection COLLECTION. 
COLLECTION defaults to the currently active filter collection."
  (or collection (setq collection (star-tabs-active-filter-collection)))
  (plist-get (star-tabs-filter-collection-props collection) prop))
(defun star-tabs-set-filter-collection-prop-value (prop value &optional collection)
  "Set property PROP in filter collection COLLECTION to value VALUE, then refresh the tab bar. 
COLLECTION defaults to the currently active filter collection."
  (or collection (setq collection (star-tabs-active-filter-collection)))
  (plist-put (star-tabs-filter-collection-props collection) prop value)
  (star-tabs-display-tab-bar))
(defun star-tabs-filter-collection-props (collection)
  "Return the properties of filter collection COLLECTION"
  (alist-get collection star-tabs-filter-collections))
(defun star-tabs-active-filter-collection-props ()
  "Return the properties of the currently active filter collection."
    (star-tabs-filter-collection-props (star-tabs-active-filter-collection)))

;;; Filters
(defun star-tabs-filter-changed-p () 
  "Return non-nil if the active filter (star-tabs-get-active-filter) has changed since last time this function was called."
  (if (eq star-tabs-current-filter (star-tabs-get-active-filter))
      nil
    (progn (setq star-tabs-current-filter (star-tabs-get-active-filter))
	   t)))

;; Add and remove filters
(defun star-tabs-add-filter (&rest filter-props)
  "Add a regular expression-based filter to include/exclude buffers from being displayed.

-:NAME is the name of the filter. 
-Either :INCLUDE or :EXCLUDE or both must be set. 
-If both :INCLUDE and :EXCLUDE are set, buffers matching the regexp set in :EXCLUDE 
will be excluded from those matching the regexp in :INCLUDE.
-If only :INCLUDE is set, only the buffers matching the regexp in :INCLUDE will be displayed.
-If only :EXCLUDE is set, all buffers except the ones matching the regexp in :EXCLUDE will be displayed.
-The filter will be added to filter collection COLLECTION, which defaults to the currently active filter collection."

  (let* ((name (plist-get filter-props :name))
	 (exclude (plist-get filter-props :exclude))
	 (include (plist-get filter-props :include))
	 (collection (or (plist-get filter-props :collection) (star-tabs-active-filter-collection)))
	 (filter `(,name :exclude ,exclude
			 :include ,include))
	 last-filter-pos)
    (if (not (member name (star-tabs-get-filter-names)))
	;; Add the filter to the "right" of the last added filter. 
	(progn (setq last-filter-pos
		     (cl-position (star-tabs-get-filter-collection-prop-value
				   :last-filter (star-tabs-active-filter-collection))
				  (star-tabs-get-filter-names)))
	       (if last-filter-pos
		   (set collection (star-tabs-insert-at-nth (eval collection) filter (1+ last-filter-pos)))
		 (set collection (append (eval collection) (list filter))))
	       (star-tabs-set-filter-collection-prop-value :last-filter name collection))
      (message "Filter name already exists")))
  (star-tabs-display-tab-bar))
(defun star-tabs-remove-filter (filter-name &optional collection)
  "Remove filter FILTER-NAME from filter collection COLLECTION.
COLLECTION defaults to the currently active filter collection."
  (setq collection (or collection (star-tabs-active-filter-collection)))
  ;; If we're removing the last added filter, set the :last-filter property of the collection to the next-to-last
  ;; filter instead.
  (when (eq (star-tabs-get-filter-collection-prop-value :last-filter collection)
	    filter-name)
    (star-tabs-set-filter-collection-prop-value :last-filter (star-tabs-left-of-elt
						       (star-tabs-get-filter-names collection)
						       filter-name)))
  (set collection (assq-delete-all filter-name (eval collection)))
  (star-tabs-display-tab-bar))
(defun star-tabs-remove-all-filters (&optional collection)
  "Delete all filters in filter collection COLLECTION.
COLLECTION defaults to the currently active filter collection."
  (setq collection (or collection (star-tabs-active-filter-collection)))
  (let ((filters (star-tabs-get-filter-names)))
    (dolist (filter filters)
      (star-tabs-remove-filter filter collection))
    (setq star-tabs-file-extension-filter-names nil)))
(defun star-tabs-init-filters ()
  "Initialize default collection and filters."
  (star-tabs-create-filter-collection
   :name "default-collection"
   :use t
   :enable-file-extension-filters t 
   :display-filter-name t)
  
  (star-tabs-add-filter
   :name 'default
   :exclude '("^[[:space:]]" "^*.*\\*$" "^magit-" "^magit:"))

  (star-tabs-add-filter
   :name 'system
   :include `("^*.*\\*$"))
  
  ;; Add file extension filters if customizable variable is set 
  (if (plist-get (star-tabs-active-filter-collection-props) :enable-file-extension-filters)
      (star-tabs-update-file-extension-filters)))

;; Filter Interactions 
(defun star-tabs-cycle-filters (&optional reverse inhibit-refresh)
  "Cycle (move forward, or backward if REVERSE is non-nil) through filters in the currently active filter collection. 
Ignore empty filters. 

Refresh tab bar if INHIBIT-REFRESH is nil."
  (interactive)
  (or reverse (setq reverse nil))
  ;; Move (cycle) forward once, or backward if REVERSE is non-nil.
  (set (star-tabs-active-filter-collection) (star-tabs-cycle-list-car
				      (eval (star-tabs-active-filter-collection))
				      reverse))
  ;; Skip to the next filter when no buffers are returned (i.e. avoid empty filter groups).
  (let ((filter-count (length (eval (star-tabs-active-filter-collection)))))
    ;; Go through the list of filters in the active filter collection once, or until a non-empty filter group is found.
    (while (and (not (star-tabs-filter-buffers (star-tabs-get-active-filter-name)star-tabs-active-buffers))
		(>= filter-count 0))
      (set (star-tabs-active-filter-collection) (star-tabs-cycle-list-car (eval (star-tabs-active-filter-collection)) reverse))
      (setq filter-count (1- filter-count)))) ;Prevent infinite loop in case all groups are empty
  (unless inhibit-refresh
    (star-tabs-display-filter-name-temporarily)
    (star-tabs-display-tab-bar)))
(defun star-tabs-find-active-filter () ; TODO fix this
  "Find and display a filter for the currently active buffer, if such filter exists"
  (let ((current-buffer (star-tabs-current-buffer))
	(filter-count (length (eval (star-tabs-active-filter-collection)))))
    ;; Loop through the list of registered filters once, or until a filter is found
    (while (and (not (member current-buffer (star-tabs-filter-buffers (star-tabs-get-active-filter-name)star-tabs-active-buffers)))
		(>= filter-count 0))
      (star-tabs-cycle-filters nil t)  ; If buffer is not in filter group, move cycle index once
      (setq filter-count (1- filter-count)))
    (star-tabs-display-tab-bar)
    (star-tabs-get-active-filter-name))) ; Prevent infinite loop in case there is no match
(defun star-tabs-get-first-buffer-in-filter (filter-name) ; TODO fix this
  "Return the first buffer found in filter filter-name. If buffer star-tabs-current-buffer
exists in filter, return buffer star-tabs-current-buffer instead."
  (let((buffers (star-tabs-filter-buffers filter-name star-tabs-active-buffers)))
    (if (member star-tabs-current-buffer buffers)
	;; If the buffer we were previously in exists in the filter group, return that buffer
	(unless (eq star-tabs-current-buffer (buffer-name (star-tabs-current-buffer)))
	  star-tabs-current-buffer)
      ;; Otherwise, return the first buffer of the filter group
      (car buffers))))
(defun star-tabs-add-to-always-include-in-filter (buffer &optional filter-name collection-name)
  "Always include buffer BUFFER in filter FILTER-NAME of collection COLLECTION-NAME."
  (or filter-name (setq filter-name (star-tabs-get-active-filter-name)))
  (or collection-name (setq collection-name (star-tabs-active-filter-collection)))
  (let* ((buffer-name (buffer-name buffer))
	 (regexp (regexp-quote buffer-name))
	 (always-include (plist-get (alist-get filter-name (eval collection-name)) :always-include))
	 (always-include (when (not (member regexp always-include))
			     (push regexp always-include))))
    (when always-include
      (plist-put (alist-get filter-name (eval collection-name))
		 :always-include always-include)))
  (star-tabs-display-tab-bar t))
(defun star-tabs-remove-from-always-include-in-filter (buffer &optional filter-name collection-name)
  "Remove buffer BUFFER from automatic inclusion in filter FILTER-NAME of collection COLLECTION-NAME."
  (or filter-name (setq filter-name (star-tabs-get-active-filter-name)))
  (or collection-name (setq collection-name (star-tabs-active-filter-collection)))
  (let* ((buffer-name (buffer-name buffer))
	 (regexp (regexp-quote buffer-name))
	 (always-include (plist-get (alist-get filter-name (eval collection-name)) :always-include))
	 (always-include (remove regexp always-include))
	 (exclude (plist-get (alist-get filter-name (eval collection-name)) :exclude))
	 (exclude (when (not (member regexp exclude))
			     (push regexp exclude))))
    (plist-put (alist-get filter-name (eval collection-name))
	       :always-include always-include)
    (when exclude
      (plist-put (alist-get filter-name (eval collection-name))
		 :exclude exclude))
    (star-tabs-display-tab-bar t)))
(defun star-tabs-include-current-buffer-in-current-filter ()
  (interactive)
  (star-tabs-add-to-always-include-in-filter (star-tabs-current-buffer)))
(defun star-tabs-exclude-current-buffer-from-current-filter ()
  (interactive)
  (star-tabs-remove-from-always-include-in-filter (star-tabs-current-buffer)))

;; Get filter data 
(defun star-tabs-get-filter (filter-name &optional collection)
  "Return the filter FILTER-NAME in filter collection COLLECTION.
COLLECTION defaults to the currently active filter collection."
  (setq collection (or collection (star-tabs-active-filter-collection)))
  (alist-get filter-name (eval collection)))
(defun star-tabs-get-filter-names (&optional collection)
  "Return all filter names in filter collection COLLECTION.
COLLECTION defaults to the currently active filter collection."
  (setq collection (or collection (star-tabs-active-filter-collection)))
  (mapcar 'car (eval collection)))
(defun star-tabs-get-active-filter ()
  "Return the active filter"
 (car (eval (star-tabs-active-filter-collection))))
(defun star-tabs-get-active-filter-name ()
  "Return the active filter's name as a symbol. If there is no active filter, return 'ALL" 
  (or (car(star-tabs-get-active-filter))
      'ALL))

;; Apply filters
(defun star-tabs-filter-buffers (filter-name buffer-list)
  "Filter buffer list BUFFER-LIST with filter FILTER and return the filtered list of buffers."
  (let* ((filter (star-tabs-get-filter filter-name))
	 (include (plist-get filter :include))
	 (exclude (plist-get filter :exclude))
	 (always-include (plist-get filter :always-include))
	 (buffers buffer-list))

    (if (and
	 (null include)
	 (null exclude))
	buffers ; Return all buffers if neither :include or :exclude is defined
      
      (let ()
	;; Apply an inclusive filter
	(when (and
	       (null include)
	       exclude)
	  (setq buffers (star-tabs-apply-filter-list buffers exclude nil always-include)))

	;; Apply an exclusive filter
	(when (and
	       include
	       (null exclude))
	  (setq buffers (star-tabs-apply-filter-list buffers include t always-include)))
	
	;; Apply an a filter that is both inclusive and exclusive (include first, then exclude from the ones included)
	(when (and
	       include
	       exclude)
	  (setq buffers (star-tabs-apply-filter-list buffers include t always-include))
	  (setq buffers (star-tabs-apply-filter-list (star-tabs-get-buffers buffers) exclude nil always-include)))
	))
    (star-tabs-get-buffers buffers)))
(defun star-tabs-apply-filter-list (buffer-list regexps include always-include)
  "Apply all regular expressions in list REGEXPS to all buffers in BUFFER-LIST. 
If INCLUDE is non-nil, include all matching buffers. 
If INCLUDE is nil, exclude all matching buffers.
Buffers matching regexp in ALWAYS-INCLUDE will be always be included."
  (let ((buffers (delq nil
		       ;; For every buffer in BUFFER-LIST, apply all regular expressions in REGEXPS,
		       ;; then make a list consisting of nil values (in case the buffer is filtered out)
		       ;; and buffer names (in case the buffer is not filtered out),
		       ;; then remove all nil values. 
		       (mapcar
			(lambda (buffer)
			  (let ((match nil)
				(stop nil))
			    (dolist (regexp regexps match)
			      (when always-include
				  (dolist (include always-include)
				    (when (string-match include buffer)
				      (setq match buffer)
				      (setq stop t))))
				(if include
				    ;; Filter out anything not matched by the regexp list
				    (if (string-match regexp buffer)
					(setq match buffer))

				  ;; Filter out everything matched by the regexp list
				  (cond (stop)
					((string-match regexp buffer)
					 (progn
					   (setq match nil)
					   (setq stop t)))
					((setq match buffer)))))))
			(star-tabs-get-buffer-names buffer-list)))))
    buffers))
(defun star-tabs-filter-by-prefix (buffer-list prefix-list &optional include)
  "Globally filter buffers BUFFER-LIST with the prefixes PREFIX-LIST. If INCLUDE (default nil) is non-nil, 
return a list of buffers that match any of the prefixes.
Otherwise, return a list of buffers that don't match any of the prefixes."
  (or include (setq include nil))
  ;; Apply string-prefix-p for each prefix in PREFIX-LIST to each buffer in BUFFER-LIST 
  ;; If a buffer contains any of the prefixes, return nil.
  ;; Otherwise, return the buffer (or vice versa in case include is non-nil),
  ;; then delete all nil elements of the list.
  (delq nil (mapcar (lambda (buffer)
		      (if (not (member nil (mapcar (lambda (prefix)
						     (funcall #'star-tabs-buffer-prefix-p prefix buffer))
						   prefix-list)))
			  (if include nil buffer)
			(if include buffer nil)))
		    buffer-list)))
(defun star-tabs-buffer-prefix-p (prefix buffer)
  "Return BUFFER if its name has the prefix PREFIX. Otherwise, return nil."
  (if (string-prefix-p prefix (buffer-name buffer))
      nil  
    buffer))

;; Cache filtered buffers
(defun star-tabs-clear-cached-buffers ()
  "Clears cache STAR-TABS-CACHED-FILTERED-BUFFERS"
  (clrhash star-tabs-cached-filtered-buffers))
(defun star-tabs-add-filtered-buffer-to-cache (buffer hash-table)
  "Add buffer BUFFER to cache HASH-TABLE"
  (puthash buffer t hash-table))
(defun star-tabs-add-filtered-buffers-to-cache (buffer-list hash-table)
  "Add list of buffers BUFFER-LIST to cache HASH-TABLE"
  (mapc (lambda (buffer) (star-tabs-add-filtered-buffer-to-cache buffer hash-table)) buffer-list))

;; File extension filters
(defun star-tabs-get-file-extensions ()
  "Return all file extensions from all active buffers."
  (let ((buffers (star-tabs-get-buffer-names star-tabs-active-buffers))
	(file-ext-regexp ".+\\.*?\\(\\..+$\\)")  ; Regexp matching file extensions
	(no-ext-regexp "^[a-z0-9A-Z]+$")	 ; Regexp matching extensionless files/buffers TODO: FIX THIS
	file-extensions)
      
    ;; For each buffer, push the file extension name of that buffer, if one exists, to a list of file extensions. 
    (dolist (buffer buffers file-extensions)
      (when (string-match file-ext-regexp buffer)
	(push (match-string 1 buffer) file-extensions))) 
    file-extensions))
(defun star-tabs-add-file-extension-filter (extension-name &optional collection)
  "Add an inclusive filter for file extension EXTENSION-NAME to filter collection COLLECTION.
COLLECTION defaults to the currently active filter collection."
  (setq collection (or collection (star-tabs-active-filter-collection)))
  ;; Only add a filter if doesn not already exist in COLLECTION.
  (if (or (not (member extension-name star-tabs-file-extension-filter-names))
	  (and (not (alist-get extension-name (eval collection)))
	       (member extension-name star-tabs-file-extension-filter-names)))
      ;; Add filter to the filter list, then add the file extension to the file extension list
      (progn (star-tabs-add-filter
	      :name extension-name
	      :include (list(concat (symbol-name extension-name) "$"))
	      :collection collection)

	     (when (not (member extension-name star-tabs-file-extension-filter-names))
	       (push extension-name star-tabs-file-extension-filter-names)))))
(defun star-tabs-add-file-extension-filters (&optional collection)
  "Automatically add filters for each file type among all open buffers to filter collection COLLECTION.
COLLECTION defaults to the currently active filter collection."
  ;; Get all file extensions and turn them into filters
  (setq collection (or collection (star-tabs-active-filter-collection)))
  (let ((file-extensions (mapcar 'intern (star-tabs-get-file-extensions))))
    (dolist (ext file-extensions)
      (star-tabs-add-file-extension-filter ext collection)))
  ;; Add a filter for extensionless files too.
  (star-tabs-add-filter
   :name 'extensionless
   :include '("^[a-z0-9A-Z]+$")
   :collection collection)
  nil)
(defun star-tabs-update-file-extension-filters (&optional collection)
  "Update automatically added file extension buffer filters in filter collection COLLECTION.
COLLECTION defaults to the currently active filter collection."
  (setq collection (or collection (star-tabs-active-filter-collection)))
  ;; Make sure there is a filter for extensionless files.
  (when (not (member 'extensionless (star-tabs-get-filter-names)))
      (star-tabs-add-filter
       :name 'extensionless
       :include '("^[a-z0-9A-Z]+$")
       :collection collection))
  (let ((file-extensions (mapcar 'intern (star-tabs-get-file-extensions)))
	(filter-names star-tabs-file-extension-filter-names))
    ;; Add new filters if there are new file extensions among open buffers.
    (dolist (ext file-extensions)
      (if (or (not (member ext filter-names))
	      (and (not (alist-get ext (eval collection)))
		   (member ext filter-names)))
	  (star-tabs-add-file-extension-filter ext collection)))
    ;; Remove automatically added filters if there no longer are buffers with the corresponding file extension.
    (dolist (filter filter-names)
      (if (not (member filter file-extensions))
	  (star-tabs-remove-file-extension-filter filter collection))))
    nil)
(defun star-tabs-remove-file-extension-filter (filter-name &optional collection)
  "Remove automatically added file extension buffer filter FILTER-NAME from filter collection COLLECTION.
COLLECTION defaults to the currently active filter collection."
  (setq collection (or collection (star-tabs-active-filter-collection)))
  (if (member filter-name star-tabs-file-extension-filter-names) ; Make sure the filter is one of the automatically added filters.
      ;; Keep track of the current filter so that we don't accidentally change it
      (let ((current-filter (star-tabs-get-active-filter-name))) 
	(progn (star-tabs-remove-filter filter-name collection)
	       (setq star-tabs-file-extension-filter-names (delete filter-name star-tabs-file-extension-filter-names))
	       (if (and (not (equal current-filter (star-tabs-get-active-filter-name)))
			(not (equal current-filter filter-name)))
		   (star-tabs-cycle-filters t)))))
  nil)
(defun star-tabs-remove-file-extension-filters (&optional collection)
  "Remove all automatically added file extension buffer filters from filter collection COLLECTION.
COLLECTION defaults to the currently active filter collection."
  (setq collection (or collection (star-tabs-active-filter-collection)))
  (let ((file-extensions star-tabs-file-extension-filter-names))
    (dolist (ext file-extensions)
      (star-tabs-remove-file-extension-filter ext collection)))
  ;; Remove the extensionless file filter.
  (star-tabs-remove-filter 'extensionless collection)
  nil)
(defun star-tabs-auto-activate-file-extension-filters-on-buffer-count (threshold)
 "When the total number of buffers after global filters have been applied reaches or exceeds 
THRESHOLD, STAR-TABS-ADD-FILE-EXTENSION-FILTERS is automatically set to t,
and file extension filters are subsequently added. If the buffer count goes down below the threshold again,
STAR-TABS-ADD-FILE-EXTENSION-FILTERS is then set to nil, and all automatically added file extension filters removed.
Deactivate this feature by setting this variable to 0."
 (unless (<= threshold 0)
   (if (and (not star-tabs-add-file-extension-filters)
	    (>= (length star-tabs-active-buffers) threshold))
       (setq star-tabs-add-file-extension-filters t)
     (when star-tabs-add-file-extension-filters
       (setq star-tabs-add-file-extension-filters nil)))))

;;; Buffers
(defun star-tabs-buffer-read-only-p (buffer-or-name)
  "Return t if buffer BUFFER-OR-NAME is read-only, otherwise nil"
  (not (with-current-buffer buffer-or-name (null buffer-read-only))))
(defun star-tabs-modified-state-changed-p (buffer)
  "Return t if the state of (buffer-modified-p) changed for buffer BUFFER since last time this function was called.
Use only inside function star-tabs-buffer-list"
  (if (equal (buffer-modified-p buffer) (gethash buffer star-tabs-modified-state-changed-buffer-table "not set"))
      nil
    (progn
      (message "Modified state changed!")
      (puthash buffer (buffer-modified-p buffer) star-tabs-modified-state-changed-buffer-table)
      t)))
(defun star-tabs-current-buffer ()
  "Return the current buffer if it's a 'real' buffer. Otherwise, return the last 'real' buffer.
This is used instead of (current-buffer) because (current-buffer) 
sometimes returns temporary buffers used by other extensions."
  (if (get-buffer-window (current-buffer))
      (current-buffer)
    star-tabs-current-buffer))
(defun star-tabs-get-buffer-names (buffer-list)
  "Return the names of buffers BUFFER-LIST"
  (let ((buffers (mapcar 'buffer-name buffer-list)))
    buffers))
(defun star-tabs-get-buffers (buffer-names)
  "Return a list of buffers BUFFER-NAMES"
  (mapcar 'get-buffer buffer-names))
(defun star-tabs-current-buffer-name ()
  "Return the name of the current buffer"
    (buffer-name (star-tabs-current-buffer)))

;; Buffer list
(defun star-tabs-update-buffer-list () 
  "Update the list of 'real' buffers star-tabs-active-buffers
 if (a) buffer(s) have/has been created or killed. 
 Return t if the buffer list was updated, otherwise nil."

  ;; Ignore all buffers starting with a space.
  (let* ((active-buffers (delq nil
			       (mapcar (lambda (buffer)
					 (unless (string-prefix-p " " (buffer-name buffer))
					   buffer))
				       (buffer-list))))
	 ;; Ignore buffers in cache of filtered buffers.
	 (active-buffers (delq nil (mapcar (lambda (buffer)
					     (unless (gethash buffer star-tabs-cached-filtered-buffers)
					       buffer))
					   active-buffers))))
    ;; Return nil if there are no new 'real' buffers. Otherwise, apply global filters to the new buffers.
    (if (seq-set-equal-p star-tabs-active-buffers active-buffers)
	nil

      ;; Apply global filter.
      ;; First include...
      (let*((buffer-list-inc (when star-tabs-global-inclusion-prefix-filter
			       (star-tabs-filter-by-prefix active-buffers star-tabs-global-inclusion-prefix-filter t)))
	    ;; ...then exclude.
	    (buffer-list (if star-tabs-global-exclusion-prefix-filter
			     (star-tabs-filter-by-prefix (if buffer-list-inc
						      buffer-list-inc
						    active-buffers)
						  star-tabs-global-exclusion-prefix-filter nil)
			   (if buffer-list-inc
			       buffer-list-inc
			     active-buffers))))

	;; Add globally filtered buffers to cache so we can ignore them next time.
	(star-tabs-add-filtered-buffers-to-cache (seq-difference active-buffers buffer-list) star-tabs-cached-filtered-buffers)

	;; See if the globally filtered buffer list changed. If it did, update the list of active/'real' buffers.
	;; Otherwise, return nil. 
	(if (not (seq-set-equal-p star-tabs-active-buffers buffer-list))
	    (progn (let* ((all-buffers buffer-list)
			  (new-buffers (seq-difference buffer-list star-tabs-active-buffers))
			  (old-buffers))

		     ;; Append new buffers to the end of the list of active buffers.
		     (when new-buffers
		       (setq star-tabs-active-buffers (append star-tabs-active-buffers new-buffers)))

		     ;; Remove killed buffers from the list of active buffers.
		     (setq old-buffers (seq-difference star-tabs-active-buffers buffer-list))
		     (when old-buffers
		       (setq star-tabs-active-buffers (seq-difference star-tabs-active-buffers old-buffers))))
		   t)
	  nil)))))
(defun star-tabs-buffer-list (&optional force-refresh)
   "Return a filtered list of buffers on one or more on the following conditions:
 1. star-tabs-active-buffers, the list of all buffers after global filters have been applied, has changed.
 2. The current buffer has changed, and the new current buffer is a 'real' buffer (i.e. not ephemeral or 
 considered'unimportant').
 3. The un/modified state of the current buffer has changed.
 4. The active filter has changed.

 This should only be used as an argument for star-tabs-set-header-line in order to
 make sure the buffer list is in sync with the tab bar. "

   ;; Make sure all of conditions are checked, regardless of whether previous conditions are true (which is why
   ;; the checks are not done within the (or) function).
   (or force-refresh (setq force-refresh nil))
   (let* ((buffer-list-updated-p (star-tabs-update-buffer-list))
 	 (buffer-switched-p (star-tabs-buffer-switched-p))
 	 (modified-state-changed-p (star-tabs-modified-state-changed-p star-tabs-current-buffer))
 	 (filter-changed-p (star-tabs-filter-changed-p))
 	 (buffers )
 	 (counter 1)
 	 enum-buffer-list)
     (if (or buffer-list-updated-p 
 	    modified-state-changed-p
 	    filter-changed-p
 	    buffer-switched-p
 	    force-refresh)
 	(progn 
	  (when star-tabs-debug-messages
	    (message "Blist updated: %s\nModStateChanged: %s\nFilterChanged: %s\nBufferSwtched: %s\nForce: %s"
		     buffer-list-updated-p
		     modified-state-changed-p
		     filter-changed-p
		     buffer-switched-p
		     force-refresh))

 	  ;; Decide whether file extension filters should be added.
 	  ;; One of two conditions should be met:
 	  ;; 1. The currently active filter collection has the property :enable-file-extension-filters set to non-nil
 	  ;; 2. The currently active filter collection has the property :enable-file-extension-filters
 	  ;; set to nil, and a threshold set to above 0 and the total number of buffers
 	  ;; (after global filters were applied) exceeds that number.
 	  (setq star-tabs-add-file-extension-filters
 		(or (plist-get (star-tabs-active-filter-collection-props) :enable-file-extension-filters) nil))

 	  ;; Activate the file extension filters if the buffer count exceeds a certain number
 	  (when (and (not (plist-get (star-tabs-active-filter-collection-props) :enable-file-extension-filters))
 		     (not (<= star-tabs-file-ext-filter-buffer-threshold 0)))
 	    (star-tabs-auto-activate-file-extension-filters-on-buffer-count star-tabs-file-ext-filter-buffer-threshold))
  
 	  ;; Add and remove file extension filters in the current collection, based on what buffers are currently open.
 	  (if star-tabs-add-file-extension-filters
 	      (star-tabs-update-file-extension-filters)
 	    ;; Remove all automatically set file extension filters in case none of the two conditions described
 	    ;; above are met.
 	    (when star-tabs-file-extension-filter-names
 	      (star-tabs-remove-file-extension-filters)))

 	  ;; Find and display a filter for the current buffer if we just switched buffer, and a filter exists for it.
 	  (when buffer-switched-p
 	    (star-tabs-find-active-filter))

 	  ;; Apply all filters
 	  (let ((filters (star-tabs-get-filter-names))
 		(buffer-lists nil)
 		(filtered-buffers)
 		(filtered-buffers-enum)
 		(counter 1))
 	    (dolist (filter filters buffer-lists)
 	      (setq filtered-buffers-enum nil)
 	      (setq filtered-buffers (star-tabs-filter-buffers filter star-tabs-active-buffers))
 	      (dolist (buffer filtered-buffers filtered-buffers-enum)
 		(add-to-list 'filtered-buffers-enum `(,counter . ,buffer))
 		(setq counter (1+ counter)))
 	      (setq buffer-lists (push `(,filter . ,(reverse filtered-buffers-enum)) buffer-lists)))
 	    (setq star-tabs-buffers-enum (reverse buffer-lists))
 	    (setq star-tabs-active-filtered-buffers-enum (alist-get (star-tabs-get-active-filter-name) buffer-lists))))
       nil)))

;; Buffer Switching
(defun star-tabs-switch-to-buffer (n)
  "Switch to the buffer associated with the number N."
  (interactive "p")
  (let ((buffer (cdr (assoc n (star-tabs-flatten-alist star-tabs-buffers-enum)))))
    (switch-to-buffer buffer)))
(defun star-tabs-switch-to-buffer-on-click (event)
  "Switch to buffer when clicked on."
  (interactive "e")
  (let* ((window (posn-string (event-start event)))
	 (buffer-name (get-text-property (cdr window) 'buffer-name ( car window ))))
    (switch-to-buffer buffer-name)))
(defun star-tabs-close-buffer-on-click (event)
  "Close buffer when its respective close button is clicked."
  (interactive "e")
  (let* ((window (posn-string (event-start event)))
	 (buffer-name (get-text-property (cdr window) 'buffer-name (car window ))))
    (kill-buffer buffer-name)))
(defun star-tabs-buffer-switched-p ()
  "Return t if buffer has been switched since last time this function was called. Should only be used in one place, inside
(star-tabs-buffer-list)"
  (when (and (get-buffer-window (current-buffer))
	     (not (string-prefix-p " " (buffer-name (current-buffer)))))
    (if (not (equal star-tabs-current-buffer (current-buffer)))
	(progn
	  (when star-tabs-tab-bar-filter-name
	    (star-tabs-display-filter-name-temporarily))
	  (setq star-tabs-current-buffer (current-buffer))
	  (message (format "Buffer SWITCHED: %s" star-tabs-current-buffer ))
	  t)
      nil)))
(defun star-tabs-switch-to-first-in-new-filter (filter-name)
 "Switch to the first buffer in filter FILTER, or the last buffer before switching filters, as long as the last
was also a filter switch command."

 ;; Make sure the last command was a filter switch command (TODO: cover all filter switch commands)
 (when (or (eq last-command this-command)
	   (eq this-command 'star-tabs-cycle-filters))
   (progn
     ;; When a filter switch command is not preceded by another filter switch command,
     ;; remember which buffer we are in.   TODO: FIX?
     (when (and (not (eq last-command this-command))
	      (eq this-command 'star-tabs-cycle-filters))
       (setq star-tabs-current-buffer (buffer-name (current-buffer))))
     (switch-to-buffer (star-tabs-get-first-buffer-in-filter filter-name)))))

;;; Display
(defun star-tabs-set-header-line (buffers)
  "Set the tab bar to list buffers as tabs"
  (if (and (not buffers)
	   (not star-tabs-active-filtered-buffers-enum))
      (setq star-tabs-header-line-format "   No buffers in any group in current collection.")
    
    (when (and buffers
	       (not (window-dedicated-p (get-buffer-window (current-buffer)))))
      (progn 
	(setq star-tabs-header-line-format
	      ;; It's all just one giant string...
	      (concat (propertize star-tabs-left-margin
				  'face 'star-tabs-tab-bar-left-margin)
		      ;; Display the name of the active filter
		      (concat (propertize (concat 
					   (when (and (plist-get (star-tabs-active-filter-collection-props) :display-filter-name)
						      star-tabs-tab-bar-filter-name)
					     (let ((filter-name star-tabs-tab-bar-filter-name))
					       (concat (upcase (symbol-name filter-name))
						       star-tabs-filter-name-number-separator))))
					  'face 'star-tabs-filter-name)
					;star-tabs-tab-bar-filter-name
			      ;; Display tabs
			      (let (tab-line  ; This will be returned from let function and concat'd with the rest of the string.
				    (counter 1)) ; Give each tab a unique, incrementing number
				(dolist (buffer buffers tab-line)
				  (let ((name (buffer-name (cdr buffer))))
				    (setq tab-line
					  (concat tab-line
						  (star-tabs-tab name counter)))
				    (setq counter (1+ counter)))))))))
      (setq star-tabs-header-line-format
	    (concat star-tabs-header-line-format (star-tabs-header-line-white-space)))
      ))
  (force-mode-line-update t)
  nil)
(defun star-tabs-display-tab-bar (&optional force-refresh)
  "Refresh and display the tab bar"
  (unless (window-dedicated-p) ; Only show the tab bar in non-dedicated windows
    (star-tabs-set-header-line (star-tabs-buffer-list force-refresh)))
    nil)

(defun star-tabs-display-filter-name-temporarily ()
  (when star-tabs-last-timer
    (cancel-timer star-tabs-last-timer)
    star-tabs-last-timer nil)
  (setq star-tabs-last-timer (star-tabs-set-temporarily 'star-tabs-tab-bar-filter-name
		      (star-tabs-get-active-filter-name)
		      "1 sec"
		      nil
		      #'star-tabs-display-tab-bar
		      t)))
(defun star-tabs-set-temporarily (symbol
			   value
			   duration
			   &optional value-after func-after
			   &rest args)
  "Set symbol SYMBOL to value VALUE for DURATION, then set SYMBOL to VALUE-AFTER (default nil). 
Optionally run FUNCTION after DURATION. Return timer."
  (set symbol value)
  (run-at-time duration
	       nil
	       (lambda (symbol value-after func-after args)
		 (set symbol value-after)
		 (if func-after 
		     (apply func-after args)))
	       symbol value-after func-after args))
(defun star-tabs-tab (buffer-name number)
  "Return a propertized string that represents a tab for buffer BUFFER."
  (let* ((name buffer-name)
	 (tab-separator (if (not (equal number 1))
			  (propertize star-tabs-tab-separator
				      'keymap star-tabs-map-select-tab
				      'face 
				      (if (equal name (star-tabs-current-buffer-name))
					  'star-tabs-selected-tab
					'star-tabs-non-selected-tab)
				      'buffer-name name
				      'buffer-number number)
			  ""))
	 ;; Number and name:
	 (number-and-name (propertize (concat
				       (number-to-string number)
				       star-tabs-number-name-separator
				       name
				       star-tabs-name-modified-symbol-separator)
				      'keymap star-tabs-map-select-tab
				      'face 
				      (if (equal name (star-tabs-current-buffer-name))
					  'star-tabs-selected-tab
					'star-tabs-non-selected-tab)
				      'buffer-name name
				      'mouse-face 'star-tabs-filter-name
				      'buffer-number number))
	 ;; Modified symbol:
	 ;; Don't show (un)modified symbol for system buffers or read-only buffers.
	 (modified-symbol (propertize (if (and(not (string-match "^[[:space:]]" name))
					      (not (string-match "^*.*\\*$" name))
					      (not (star-tabs-buffer-read-only-p name)))
					  ;; Display (un)modified symbol:
					  (concat  
						  (if (buffer-modified-p (get-buffer name))
						      star-tabs-modified-buffer-symbol
						    star-tabs-unmodified-buffer-symbol)
						  (when (not(star-tabs-get-filter-collection-prop-value
							     :hide-close-buttons))
						    star-tabs-modified-symbol-close-button-separator))
					;; Display nothing if it's a system or read-only buffer:
					"")
				      'keymap star-tabs-map-select-tab
				      'face 
				      (if (equal name (star-tabs-current-buffer-name))
					  'star-tabs-selected-tab
					'star-tabs-non-selected-tab)
				      'mouse-face 'star-tabs-filter-name
				      'buffer-name name
				      'buffer-number number))
	 ;; Close button:
	 ;; Conditionally display close button
	 (close-button (propertize (if (not(star-tabs-get-filter-collection-prop-value
					    :hide-close-buttons))
				       star-tabs-close-buffer-symbol
				     "")
				   'keymap star-tabs-map-close-tab
				   'face 
				   (if (equal name (star-tabs-current-buffer-name))
				       'star-tabs-selected-tab
				     'star-tabs-non-selected-tab)
				   'mouse-face 'star-tabs-filter-name
				   'buffer-name name
				   'buffer-number number))
	 ;; (icon (star-tabs-select-icon name (if (equal name (star-tabs-current-buffer-name))
	 ;; 				'star-tabs-selected-icon
	 ;; 			      'star-tabs-non-selected-icon)))
	 (icon (star-tabs-select-icon name))
	 (divider (propertize " " 
			      'keymap star-tabs-map-select-tab
			      'face 
			      (if (equal name (star-tabs-current-buffer-name))
				  'star-tabs-selected-tab
				'star-tabs-non-selected-tab)
			      'buffer-name name
			      'mouse-face 'star-tabs-filter-name
			      'buffer-number number)))

    (concat divider
	    (when (stringp icon)
		  icon)
	    divider
	    number-and-name
	    modified-symbol
	    close-button
	    divider)))
(defun star-tabs-header-line-remaining-space()
  (- (window-total-width) (length star-tabs-header-line-format)))
(defun star-tabs-header-line-white-space ()
  (let ((empty-space (star-tabs-header-line-remaining-space))
	(whitespace ""))
    (while (> empty-space 0)
      (setq whitespace (concat " " whitespace))
      (setq empty-space 
	    (1- empty-space)))
    (propertize whitespace
		'face 'star-tabs-non-selected-tab)))
(defun star-tabs-select-icon (buffer)
  (with-current-buffer buffer
    ;(all-the-icons-icon-for-mode major-mode :face face :v-adjust 0.03)
    (all-the-icons-icon-for-buffer)))

;;;General
(defun star-tabs-select-icon (buffer)
   (with-current-buffer buffer
     ;(all-the-icons-icon-for-mode major-mode :face face :v-adjust 0.03)
     (all-the-icons-icon-for-buffer)))
(defun star-tabs-when-buffer-first-modified ()
  "Run when a buffer goes from an unmodified state to a modified state"
  (if (member (current-buffer) star-tabs-active-buffers)
      (progn (set-buffer-modified-p t) ; HACK: Make sure that buffer-modified-p is set to t
	     (star-tabs-display-tab-bar  t))))
(defun star-tabs-when-buffer-first-saved ()
   "Run when a buffer goes from a modified state to a unmodified state"
   (set-buffer-modified-p nil) ; HACK: Make sure that buffer-modified-p is set to nil.
   (star-tabs-display-tab-bar t))


;; Modes
(define-minor-mode star-tabs-tab-bar-mode
  "...desc..."
  :lighter " ST"
  :global t

  (if star-tabs-tab-bar-mode
      (progn (star-tabs-init-filters)
	     ;; Update the tab bar when buffers are created or killed
	     (add-hook 'buffer-list-update-hook
		       #'star-tabs-display-tab-bar nil nil)
	     ;; Functions to run when a buffer goes from an unmodified to a modified state
	     (add-hook 'first-change-hook
		       #'star-tabs-when-buffer-first-modified nil nil)
	     ;; Update the tab bar when a buffer is saved 
	     (add-hook 'after-save-hook
		       #'star-tabs-when-buffer-first-saved nil nil))))

(star-tabs-tab-bar-mode t)

(provide 'star-tabs)
