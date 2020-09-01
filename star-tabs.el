;; -*- lexical-binding: t; -*-
(require 'seq)
(require 'cl-lib)
(require 'all-the-icons)

;;; Global Variables and Constants
;;
;;
;; Display

(defvaralias 'star-tabs-header-line-format 'header-line-format
  "Header line format for display of the tab bar.")

(defvar star-tabs-header-line 'header-line
  "Header line where tabs are displayed.")

;; Tab bar dividers 

(defvar star-tabs-left-margin "  " 
  "Space used to the left of the tab bar.")

(defvar star-tabs-right-margin ""
  "Space used to the right of the tab bar.")

(defvar star-tabs-tab-separator " "
  "Tab bar divider that separates tabs.")

(defvar star-tabs-number-name-separator " "
  "Tab bar divider that separates the buffer number and buffer name in a tab.")

(defvar star-tabs-name-modified-icon-separator " "
  "Tab bar divider that separates the buffer name and modified icon in a tab.")

(defvar star-tabs-modified-icon-close-button-separator " " 
  "Tab bar divider that separates the modified icon and close button in a tab.")

(defvar star-tabs-filter-name-number-separator "   "
  "Tab bar divider that separates the name of the active filter group and the first tab.")


;; Tab icons

(defvar star-tabs-modified-buffer-icon "*"
  "Tab 'icon' for modified buffers.")

(defvar star-tabs-unmodified-buffer-icon "+"
  "Tab 'icon' for unmodified buffers.")

(defvar star-tabs-close-buffer-icon "x"
  "Tab 'icon' for the tab close button.")


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
  "Helper variable for function (star-tabs--buffer-switched-p) to keep track of the current 'real' buffer.
A 'real' or 'active' buffer refers to an open buffer that is not ephemeral/temporary or otherwise deemed unimportant.")

(defvar star-tabs-current-filter nil
  "Helper variable for (star-tabs--filter-changed-p) to keep track of when a filter changes.")

(defvar star-tabs-modified-state-changed-buffer-table (make-hash-table :test #'equal)
  "Store whether a buffer has been modified.")

(defvar star-tabs-last-timer nil
  "The last used timer, set automatically by (star-tabs--display-filter-name-temporarily).")

(defvar star-tabs-debug-messages t
  "If set to non-nil, debug messages will be displayed."
  ;; TODO: Remove this, and all debug messages.
)

;; Collections

(defvar star-tabs-filter-collections nil
  "List of all filter collections. car of the list represents the currently active collection in the tab bar.")


;; Filters

(defvar star-tabs-global-inclusion-prefix-filter nil
  "List of buffer name prefixes to be included globally. Buffers filtered this way will be cached and ignored
for all future searches. As such, global filtering may increase performance, and
should (and should only!) be applied to buffers that you really don't care about.

Buffers with the space prefix (\" \") are automatically filtered before this filter is applied, and thus cannot  
be included.

This filter is applied before star-tabs-global-exclusion-prefix-filter.")

(defvar star-tabs-global-exclusion-prefix-filter '("magit-" "magit:" "*Help" "*WoM")
  "List of buffer name prefixes to be excluded globally. Buffers filtered this way will be cached and ignored
for all future searches. As such, global filtering may increase performance, and
should (and should only!) be applied to buffers that you really don't care about.

Buffers with the space prefix (\" \") are automatically filtered before this filter is applied, and thus need not
be added to this list.

This filter is applied after star-tabs-global-inclusion-prefix-filter.")

(defvar star-tabs-tab-bar-filter-name nil
  "Filter name to be displayed in the tab bar; automatically set by other functions.")

(defvar star-tabs-cached-filtered-buffers (make-hash-table :test #'equal)
  "Cache globally filtered buffers to improve performance.")


;; File extension filters

(defvar star-tabs-add-file-extension-filters nil
  "If non-nil, file extension filters will be added to the collection. This variable is set and
controlled by functions, and depends on collection-specific configuration."
  ;; FIXME: Add as a collection property instead.
  )

(defcustom star-tabs-file-ext-filter-buffer-threshold 15
  "When the total number of buffers after global filters have been applied reaches or exceeds 
the number set in this variable, star-tabs-set-file-extension-filters is automatically set to t,
and file extension filters are subsequently added. If the buffer count goes down below the threshold again,
star-tabs-set-file-extension-filters is then set to nil, and all automatically added file extension filters removed.
Deactivate this feature by setting this variable to 0."
  ;; FIXME: Add as a collection property instead.
  :type 'int)

(defvar star-tabs-file-extension-filter-names nil
  "Automatically added file-extension filters. 
This is a helper variable for the automatic file extension filters")


;; Buffers

(defvar star-tabs-active-filtered-buffers-enum nil
  "Enumerated list of buffers after all filters have been applied.")

(defvar star-tabs-active-buffers nil
  "List of all currently active/'real' buffers.
A 'real' or 'active' buffer refers to an open buffer that is not ephemeral/temporary or otherwise deemed unimportant.")

(defvar star-tabs-buffers-enum nil
  "Alist of enumerated buffers for all filters in the active collection. 
Key is filter name, value is an enumerated list of buffers.")


;;; Visuals

(defvar star-tabs-tab-bar-height 210
  "Height of the tab bar.")

(defvar star-tabs-tab-bar-text-height 155
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
  "Face for left margin of the header-line which is used for determining the height of the header-line.")

(defface star-tabs-filter-name
  `((t
     (
      :background ,star-tabs-tab-bar-non-selected-background
      :foreground ,star-tabs-tab-bar-filter-name-foreground
      :height ,star-tabs-tab-bar-text-height)))
  "Face for displaying filter-name in the tab bar.")

(defface star-tabs-tab-divider-mouse-selected
  `((t
     (
      :background ,star-tabs-tab-bar-selected-background
      :foreground ,star-tabs-tab-bar-filter-name-foreground
      :height ,star-tabs-tab-bar-text-height)))
  "Face to be displayed when hovering over a selected tab with the mouse in the tab bar.")

(defface star-tabs-tab-divider-mouse-non-selected
  `((t
     (
      :background ,star-tabs-tab-bar-non-selected-background
      :foreground ,star-tabs-tab-bar-filter-name-foreground
      :height ,star-tabs-tab-bar-text-height)))
  "Face to be displayed when hovering over a selected tab with the mouse in the tab bar.")

(defface star-tabs-mouse-selected
  `((t
     (
      :background ,star-tabs-tab-bar-selected-background
      :foreground ,star-tabs-tab-bar-filter-name-foreground
      :height ,star-tabs-tab-bar-text-height)))
  "Face to be displayed when hovering over a selected tab with the mouse in the tab bar.")

(defface star-tabs-mouse-non-selected
  `((t
     (
      :background ,star-tabs-tab-bar-non-selected-background
      :foreground ,star-tabs-tab-bar-filter-name-foreground
      :height ,star-tabs-tab-bar-text-height)))
  "Face to be displayed when hovering over a selected tab with the mouse in the tab bar.")

(defface star-tabs-non-selected-tab
  `((t (
	:background ,star-tabs-tab-bar-non-selected-background
	:foreground ,star-tabs-tab-bar-non-selected-foreground
	:height ,star-tabs-tab-bar-text-height)))
  "Face for displaying the non-selected tab in the tab bar.")

(defface star-tabs-selected-tab
  `((t
     (
      :background ,star-tabs-tab-bar-selected-background
      :foreground ,star-tabs-tab-bar-selected-foreground
      :height ,star-tabs-tab-bar-text-height)))
  "Face for displaying the selected tab in the tab bar.")

(defface star-tabs-non-selected-icon
  `((t (
	:background ,star-tabs-tab-bar-non-selected-background
	:foreground nil	    
	:height ,star-tabs-tab-bar-text-height)))
  "Face for displaying the non-selected icon in the tab bar")

(defface star-tabs-selected-icon
  `((t
     (:background ,star-tabs-tab-bar-selected-background
      :foreground nil
      :height ,star-tabs-tab-bar-text-height)))
  "Face for displaying the selected icon in the tab bar.")


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
  "Return the element to the left of element ELT in list LIST. If ELT is the car of LIST, return the last element of LIST.
Return nil if ELT is not in LIST."
  (if (member elt list)
    (let ((left-elt (cadr (member elt (reverse list)))))
      (or left-elt (car (reverse list))))))

(defun star-tabs-insert-at-nth (list elt n)
  "Insert element ELT in list LIST at position N. Return the modified list."
  (let ((nth-from-end (- (length list) n)))
    (append (reverse (nthcdr nth-from-end (reverse list))) (list elt) (nthcdr n list))))

(defun star-tabs-flatten-alist (alist)
  "Flatten an alist by removing keys and keeping values."
  (let ((flattened-list nil))
    (dolist (item alist flattened-list)
      (dolist (value (cdr item) flattened-list)
	(push value flattened-list)))
    (delq nil (reverse flattened-list))))

(defun star-tabs-set-temporarily (symbol value duration &optional value-after func-after &rest args)
  "Set symbol SYMBOL to value VALUE for DURATION. After DURATION, set SYMBOL to VALUE-AFTER (default nil). 
Optionally run function FUNCTION with arguments ARGS after DURATION. Return timer."
  ;; Set to new value when the timer starts.
  (set symbol value)
  (run-at-time duration
	       nil
	       (lambda (symbol value-after func-after args)
		 ;; Set to nil when the timer ends.
		 (set symbol value-after)
		 ;; If set, run FUNC-AFTER when the timer ends.
		 (when func-after 
		     (apply func-after args)))
	       symbol value-after func-after args))

(defun star-tabs-string-pixel-width (string)
  "Return the width in pixels of string STRING."
  ;; REVIEW: Make just one dedicated window instead of creating new windows all the time?
  (save-window-excursion
    (with-temp-buffer
      (let ((window (display-buffer (current-buffer))))
	(erase-buffer)
	(insert string)
	(car (window-text-pixel-size window nil nil 20000 20000))))))


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
	 (file-extension-filter-threshold (or (plist-get collection-props :file-extension-filter-threshold) 0))
	 (collection-name-prefix (or (plist-get collection-props :collection-name-prefix) "star-tabs-filter-collection-"))
	 (name (intern (concat collection-name-prefix (plist-get collection-props :name))))
	 (collection `(,name :enable-file-extension-filters ,enable-file-extension-filters
			     :display-filter-name ,display-filter-name
			     :file-extension-filter-threshold ,file-extension-filter-threshold
			     :collection-name-prefix ,collection-name-prefix
			     :last-filter nil)))

    (if (not (member name (star-tabs-filter-collection-names)))
	(progn (set name nil) 
	       (setq star-tabs-filter-collections
		     (append star-tabs-filter-collections (list collection)))
	       ;; Switch to the new collection upon creation if :use is non-nil.
	       (when use
		 (while (not (eq (star-tabs-active-filter-collection-name) name))
		   (star-tabs-cycle-filter-collections t))))
      (message "Collection name already exists"))))

(defun star-tabs-filter-collection-names ()
  "Return the names of all filter collections."
  (mapcar 'car star-tabs-filter-collections))

;; FIXME currently creating collection while the list is rotated causes the collections to be in the wrong order
;; Solve this by rotating the list back to the beginning (store first added list, move to second if the first is deleted etc)
;; Or just keeping track of last added..?

(defun star-tabs-remove-filter-collection (collection-name)
  "Delete filter collection COLLECTION-NAME."
  (if (>= (length star-tabs-filter-collections) 2)
      (let ((prefix (star-tabs-get-filter-collection-prop-value :collection-name-prefix collection-name)))
	;; makunbound will cause problems if we're removing the currently active collection, so first make another collection active.
	;; BEWARE: If for some reason in the future, star-tabs-cycle-filter-collections has the ability to skip collections,
	;; we might inadvertently, despite cycling, end up deleting COLLECTION-NAME when it's currently active. 
	(when (eq collection-name (star-tabs-active-filter-collection-name))
	  (star-tabs-cycle-filter-collections))
	(setq star-tabs-filter-collections (remove
				     (nth (cl-position collection-name (star-tabs-filter-collection-names)) star-tabs-filter-collections)
				     star-tabs-filter-collections))
	(makunbound collection-name))
  (message "Cannot delete last collection. Make another collection before attempting to delete this one.")))

(defun star-tabs-cycle-filter-collections (&optional reverse inhibit-refresh)
  "Cycle (move forward, or backward if REVERSE is non-nil) through filter collections.
Also refresh tab bar if INHIBIT-REFRESH is non-nil."
  (interactive)
  (setq star-tabs-filter-collections (star-tabs-cycle-list-car star-tabs-filter-collections reverse))
  (star-tabs-display-tab-bar))

(defun star-tabs-active-filter-collection-name ()
  "Return the name of the active filter collection."
  (car (car star-tabs-filter-collections)))

(defun star-tabs-get-filter-collection-prop-value (prop &optional collection-name)
"Return the value of property PROP in filter collection COLLECTION-NAME. 
COLLECTION-NAME defaults to the currently active filter collection."
  (or collection-name (setq collection-name (star-tabs-active-filter-collection-name)))
  (plist-get (star-tabs-filter-collection-props collection-name) prop))

(defun star-tabs-set-filter-collection-prop-value (prop value &optional collection-name)
  "Set property PROP in filter collection COLLECTION-NAME to value VALUE, then refresh the tab bar. 
COLLECTION-NAME defaults to the currently active filter collection."
  (or collection-name (setq collection-name (star-tabs-active-filter-collection-name)))
  (plist-put (star-tabs-filter-collection-props collection-name) prop value)
  (star-tabs-display-tab-bar))

(defun star-tabs-filter-collection-props (collection-name)
  "Return the properties of filter collection COLLECTION-NAME."
  (alist-get collection-name star-tabs-filter-collections))

(defun star-tabs-active-filter-collection-props ()
  "Return the properties of the currently active filter collection."
    (star-tabs-filter-collection-props (star-tabs-active-filter-collection-name)))


;;; Filters

(defun star-tabs--filter-changed-p () 
  "Return non-nil if the active filter (as returned by (star-tabs-get-active-filter)) has changed since last time this function was called.
This function should only be used once, inside (bn-buffer-list). The last (current) active filter is stored 
in the global variable star-tabs-current-filter."
  (if (eq star-tabs-current-filter (star-tabs-get-active-filter))
      nil
    (progn (setq star-tabs-current-filter (star-tabs-get-active-filter))
	   t)))


;; Add and remove filters

(defun star-tabs-add-filter (&rest filter-props)
  "Add a regular expression-based filter to include/exclude buffers from being displayed.\n
-:name is the name of the filter. 
-:include and :exclude are lists of regular expressions.
-Either :include or :exclude or both must be set. 
-If both :include and :exclude are set, buffers matching the regexp set in :exclude 
will be excluded from those matching the regexp in :include.
-if only :include is set, only the buffers matching the regexp in :include will be displayed.
-if only :exclude is set, all buffers except the ones matching the regexp in :exclude will be displayed.
-The filter will be added to filter collection :collection-name, which defaults to the currently active filter collection."
  (let* ((name (plist-get filter-props :name))
	 (exclude (plist-get filter-props :exclude))
	 (include (plist-get filter-props :include))
	 (collection-name (or (plist-get filter-props :collection) (star-tabs-active-filter-collection-name)))
	 (filter `(,name :exclude ,exclude
			 :include ,include))
	 last-filter-pos)
    (if (not (member name (star-tabs-get-filter-names)))
	;; Add the filter to the "right" of the last added filter, in order to maintain order.
	(progn (setq last-filter-pos
		     (cl-position (star-tabs-get-filter-collection-prop-value
				   :last-filter (star-tabs-active-filter-collection-name))
				  (star-tabs-get-filter-names)))
	       (if last-filter-pos
		   (set collection-name (star-tabs-insert-at-nth (eval collection-name) filter (1+ last-filter-pos)))
		 (set collection-name (append (eval collection-name) (list filter))))
	       (star-tabs-set-filter-collection-prop-value :last-filter name collection-name))
      (message "Filter name already exists")))
  (star-tabs-display-tab-bar))

(defun star-tabs-remove-filter (filter-name &optional collection-name)
  "Remove filter FILTER-NAME from filter collection COLLECTION-NAME.
COLLECTION-NAME defaults to the currently active filter collection."
  (setq collection-name (or collection-name (star-tabs-active-filter-collection-name)))
  ;; When removing the last added filter, set the :last-filter property of the collection to the next-to-last
  ;; filter instead.
  (when (eq (star-tabs-get-filter-collection-prop-value :last-filter collection-name)
	    filter-name)
    (star-tabs-set-filter-collection-prop-value :last-filter (star-tabs-left-of-elt
						       (star-tabs-get-filter-names collection-name)
						       filter-name)))
  (set collection-name (assq-delete-all filter-name (eval collection-name)))
  (star-tabs-display-tab-bar))

(defun star-tabs-remove-all-filters (&optional collection-name)
  "Delete all filters in filter collection COLLECTION-NAME.
COLLECTION-NAME defaults to the currently active filter collection."
  (setq collection-name (or collection-name (star-tabs-active-filter-collection-name)))
  (let ((filters (star-tabs-get-filter-names)))
    (dolist (filter filters)
      (star-tabs-remove-filter filter collection-name))
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
      (star-tabs--update-file-extension-filters)))


;; Filter Interactions 

(defun star-tabs-cycle-filters (&optional reverse inhibit-refresh)
  "Cycle (move forward, or backward if REVERSE is non-nil) through filters in the currently active filter collection. 
Ignore empty filters.\n
Refresh tab bar if INHIBIT-REFRESH is nil."
  (interactive)
  (or reverse (setq reverse nil))
  ;; Move (cycle) forward once, or backward if REVERSE is non-nil.
  (set (star-tabs-active-filter-collection-name) (star-tabs-cycle-list-car
					     (eval (star-tabs-active-filter-collection-name))
					     reverse))
  (let ((filter-count (length (eval (star-tabs-active-filter-collection-name)))))
    ;; Skip to the next filter when no buffers are returned (i.e. avoid empty filter groups).
    ;; Go through the list of filters in the active filter collection once, or until a non-empty filter group is found.
    (while (and (not (star-tabs-filter-buffers (star-tabs-get-active-filter-name)star-tabs-active-buffers))
		(>= filter-count 0))
      (set (star-tabs-active-filter-collection-name) (star-tabs-cycle-list-car (eval (star-tabs-active-filter-collection-name)) reverse))
      (setq filter-count (1- filter-count)))) ;Prevent infinite loop in case all groups are empty
  ;; Refresh tab bar unless explicitly told not to.
  (unless inhibit-refresh
    (star-tabs--display-filter-name-temporarily)
    (star-tabs-display-tab-bar)))

(defun star-tabs-find-active-filter () 
  "Find and display a filter for the currently active buffer, if such filter exists in the current collection."
  (interactive)
  (let ((current-buffer (star-tabs-current-buffer))
	(filter-count (length (eval (star-tabs-active-filter-collection-name)))))
    ;; Loop through the list of registered filters once, or until a filter is found.
    (while (and (not (member current-buffer (star-tabs-filter-buffers (star-tabs-get-active-filter-name) star-tabs-active-buffers)))
		(>= filter-count 0))
      (star-tabs-cycle-filters nil t)  ; If buffer is not in filter group, move cycle index once.
      (setq filter-count (1- filter-count))) ; Prevent infinite loop in case there is no match.
    (star-tabs-display-tab-bar)
    (star-tabs-get-active-filter-name))) 

(defun star-tabs-add-to-always-include-in-filter (buffer &optional filter-name collection-name)
  "Always include buffer BUFFER in filter FILTER-NAME of collection COLLECTION-NAME."
  (or filter-name (setq filter-name (star-tabs-get-active-filter-name)))
  (or collection-name (setq collection-name (star-tabs-active-filter-collection-name)))
  (let* ((buffer-name (buffer-name buffer))
	 (regexp (regexp-quote buffer-name))
	 (always-include (plist-get (alist-get filter-name (eval collection-name)) :always-include))
	 (always-include (when (not (member regexp always-include))
			     (push regexp always-include))))
    (when always-include
      (plist-put (alist-get filter-name (eval collection-name))
		 :always-include always-include)))
  (star-tabs-display-tab-bar t))

(defun star-tabs-exclude-from-filter (buffer &optional filter-name collection-name)
  "Exclude buffer BUFFER from filter FILTER-NAME of collection COLLECTION-NAME.
Also remove it from automatic inclusion, if applicable."
  (or filter-name (setq filter-name (star-tabs-get-active-filter-name)))
  (or collection-name (setq collection-name (star-tabs-active-filter-collection-name)))
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
  "Include current buffer in the currently active filter."
  (interactive)
  (star-tabs-add-to-always-include-in-filter (star-tabs-current-buffer)))

(defun star-tabs-exclude-current-buffer-from-current-filter ()
  "Exclude current buffer from the currently active filter."
  (interactive)
  (star-tabs-exclude-from-filter (star-tabs-current-buffer)))

(defun star-tabs-print-active-filter-name ()
  "Output the active filter name."
  (interactive)
  (message "Active filter name: %s" (star-tabs-get-active-filter-name)))


;; Get filter data 

(defun star-tabs-get-filter-name (filter-name &optional collection-name)
  "Return the filter FILTER-NAME in filter collection COLLECTION-NAME.
COLLECTION-NAME defaults to the currently active filter collection."
  (setq collection-name (or collection-name (star-tabs-active-filter-collection-name)))
  (alist-get filter-name (eval collection-name)))

(defun star-tabs-get-filter-names (&optional collection-name)
  "Return all filter names in filter collection COLLECTION-NAME.
COLLECTION-NAME defaults to the currently active filter collection."
  (setq collection-name (or collection-name (star-tabs-active-filter-collection-name)))
  (mapcar 'car (eval collection-name)))

(defun star-tabs-get-active-filter ()
  "Return the active filter."
 (car (eval (star-tabs-active-filter-collection-name))))

(defun star-tabs-get-active-filter-name ()
  "Return the active filter's name as a symbol. If there is no active filter, return 'ALL" 
  (interactive)
  (or (car(star-tabs-get-active-filter))
      'ALL))


;; Apply filters

(defun star-tabs-filter-buffers (filter-name buffer-list)
  "Filter buffers BUFFER-LIST with filter FILTER and return the filtered list of buffers."
  (let* ((filter (star-tabs-get-filter-name filter-name))
	 (include (plist-get filter :include))
	 (exclude (plist-get filter :exclude))
	 (always-include (plist-get filter :always-include))
	 (buffers buffer-list))
    ;; Return all buffers if neither :include nor :exclude are defined.
    (if (and
	 (null include)
	 (null exclude))
	buffers 
      (let ()
	;; Apply an inclusive filter.
	(when (and
	       (null include)
	       exclude)
	  (setq buffers (star-tabs--apply-filter-list buffers exclude nil always-include)))
	;; Apply an exclusive filter.
	(when (and
	       include
	       (null exclude))
	  (setq buffers (star-tabs--apply-filter-list buffers include t always-include)))
	;; Apply a filter that is both inclusive and exclusive (include first, then exclude from the ones included).
	(when (and
	       include
	       exclude)
	  (setq buffers (star-tabs--apply-filter-list buffers include t always-include))
	  (setq buffers (star-tabs--apply-filter-list (star-tabs-get-buffers buffers) exclude nil always-include)))))
    (star-tabs-get-buffers buffers)))

(defun star-tabs--apply-filter-list (buffer-list regexps include always-include)
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
  "Return globally filtered buffers BUFFER-LIST with/without the prefixes PREFIX-LIST. 
If INCLUDE (default nil) is non-nil, return a list of buffers that match any of the prefixes.
Otherwise, return a list of buffers that don't match any of the prefixes."
  (or include (setq include nil))
  ;; Apply string-prefix-p for each prefix in PREFIX-LIST to each buffer in BUFFER-LIST.
  ;; If a buffer contains any of the prefixes, return nil;
  ;; otherwise, return the buffer (or vice versa in case include is non-nil),
  ;; then delete all nil elements of the list.
  (delq nil (mapcar (lambda (buffer)
		      (if (not (member nil (mapcar (lambda (prefix)
						     (funcall #'star-tabs-buffer-prefix-p prefix buffer))
						   prefix-list)))
			  (if include nil buffer)
			(if include buffer nil)))
		    buffer-list)))

(defun star-tabs-buffer-prefix-p (prefix buffer)
  "Return buffer BUFFER if its name has the prefix PREFIX. Otherwise, return nil."
  (if (string-prefix-p prefix (buffer-name buffer))
      nil  
    buffer))


;; Cache filtered buffers

(defun star-tabs-clear-cached-buffers ()
  "Clear cache star-tabs-cached-filtered-buffers."
  (interactive)
  (clrhash star-tabs-cached-filtered-buffers))

(defun star-tabs-add-filtered-buffer-to-cache (buffer hash-table)
  "Add unwanted buffer BUFFER to cache HASH-TABLE."
  (puthash buffer t hash-table))

(defun star-tabs-add-filtered-buffers-to-cache (buffer-list hash-table)
  "Add unwanted buffers BUFFER-LIST to cache HASH-TABLE."
  (mapc (lambda (buffer) (star-tabs-add-filtered-buffer-to-cache buffer hash-table)) buffer-list))


;; File extension filters

(defun star-tabs-get-file-extensions ()
  "Return all file extension names for all active buffers."
  (let ((buffers (star-tabs-get-buffer-names star-tabs-active-buffers))
	(file-ext-regexp ".+\\.*?\\(\\..+$\\)")  ; Regexp matching file extensions.
	(no-ext-regexp "^[a-z0-9A-Z]+$")	 ; Regexp matching extensionless files/buffers FIXME: Use this or throw it away.
	file-extensions)
    ;; For each buffer, push the file extension name of that buffer, if one exists, to a list of file extensions names. 
    (dolist (buffer buffers file-extensions)
      (when (string-match file-ext-regexp buffer)
	(push (match-string 1 buffer) file-extensions))) 
    file-extensions))

(defun star-tabs--add-file-extension-filter (extension-name &optional collection-name)
  "Add an inclusive filter for file extension EXTENSION-NAME to filter collection COLLECTION-NAME.
COLLECTION-NAME defaults to the currently active filter collection."
  (setq collection-name (or collection-name (star-tabs-active-filter-collection-name)))
  ;; Only add a filter if doesn not already exist in COLLECTION-NAME.
  (if (or (not (member extension-name star-tabs-file-extension-filter-names))
	  (and (not (alist-get extension-name (eval collection-name)))
	       (member extension-name star-tabs-file-extension-filter-names)))
      ;; Add the filter to the filter list, then add the file extension name to the file extension list.
      (progn (star-tabs-add-filter
	      :name extension-name
	      :include (list(concat (symbol-name extension-name) "$"))
	      :collection collection-name)
	     (when (not (member extension-name star-tabs-file-extension-filter-names))
	       (push extension-name star-tabs-file-extension-filter-names)))))

(defun star-tabs--update-file-extension-filters (&optional collection-name)
  "Update automatically added file extension buffer filters in filter collection COLLECTION-NAME.
COLLECTION-NAME defaults to the currently active filter collection."
  (setq collection-name (or collection-name (star-tabs-active-filter-collection-name)))
  ;; Make sure there is a filter for extensionless files.
  (when (not (member 'extensionless (star-tabs-get-filter-names)))
    (star-tabs-add-filter
     :name 'extensionless
     :include '("^[a-z0-9A-Z]+$") ;; FIXME: Make better regexp.
     :collection collection-name))
  (let ((file-extensions (mapcar 'intern (star-tabs-get-file-extensions)))
	(filter-names star-tabs-file-extension-filter-names))
    ;; Add new filters if there are new file extensions among open buffers.
    (dolist (ext file-extensions)
      (if (or (not (member ext filter-names))
	      (and (not (alist-get ext (eval collection-name)))
		   (member ext filter-names)))
	  (star-tabs--add-file-extension-filter ext collection-name)))
    ;; Remove automatically added filters if there no longer are buffers with the corresponding file extension.
    (dolist (filter filter-names)
      (if (not (member filter file-extensions))
	  (star-tabs--remove-file-extension-filter filter collection-name))))
  nil)

(defun star-tabs--remove-file-extension-filter (filter-name &optional collection-name)
  "Remove automatically added file extension filter FILTER-NAME from filter collection COLLECTION-NAME.
COLLECTION-NAME defaults to the currently active filter collection."
  (setq collection-name (or collection-name (star-tabs-active-filter-collection-name)))
  (if (member filter-name star-tabs-file-extension-filter-names) ; Make sure the filter is one of the automatically added filters.
	;; First remove the filter from the collection...
	(progn (star-tabs-remove-filter filter-name collection-name)
	       ;; Then remove the file extension from the list of file extensions.
	       (setq star-tabs-file-extension-filter-names (delete filter-name star-tabs-file-extension-filter-names))
	       ;; Make sure we're still in a non-empty filter
	       (if (not (star-tabs-filter-buffers (star-tabs-get-active-filter-name) star-tabs-active-buffers))
		   (star-tabs-cycle-filters t t))))
  nil)

(defun star-tabs--remove-file-extension-filters (&optional collection-name)
  "Remove all automatically added file extension buffer filters from filter collection COLLECTION-NAME.
COLLECTION-NAME defaults to the currently active filter collection."
  (setq collection-name (or collection-name (star-tabs-active-filter-collection-name)))
  (let ((file-extensions star-tabs-file-extension-filter-names))
    (dolist (ext file-extensions)
      (star-tabs--remove-file-extension-filter ext collection-name)))
  ;; Remove the extensionless file filter.
  (star-tabs-remove-filter 'extensionless collection-name)
  nil)

(defun star-tabs--auto-activate-file-extension-filters-on-buffer-count (threshold)
 "When the total number of buffers after global filters have been applied reaches or exceeds 
THRESHOLD, star-tabs-add-file-extension-filters is automatically set to t,
and file extension filters are subsequently added. If the buffer count goes down below the threshold again,
star-tabs-add-file-extension-filters is then set to nil, and all automatically added file extension filters are removed.
Deactivate this feature by setting this variable to 0."
 ;; FIXME: Fix documentation ("this variable" ??)
 (unless (<= threshold 0)
   (if (and (not star-tabs-add-file-extension-filters)
	    (>= (length star-tabs-active-buffers) threshold))
       (setq star-tabs-add-file-extension-filters t)
     (when star-tabs-add-file-extension-filters
       (setq star-tabs-add-file-extension-filters nil)))))


;;; Buffers

;; Buffer killing

(defun star-tabs-kill-all-buffers-in-filter (&optional filter-name)
  "Kill all buffers in the filter group FILTER-NAME (defaults to the currently active filter)."
  (interactive)
  (or filter-name (setq filter-name (star-tabs-get-active-filter-name)))
  (let ((buffers (star-tabs-filter-buffers filter-name star-tabs-active-buffers)))
    (star-tabs--kill-buffers buffers)))

(defun star-tabs-kill-all-unmodified-buffers-in-filter (&optional filter-name)
  "Kill all unmodified buffers in the filter group FILTER-NAME (defaults to the currently active filter)."
  (interactive)
  (or filter-name (setq filter-name (star-tabs-get-active-filter-name)))
  (let* ((buffers (star-tabs-filter-buffers filter-name star-tabs-active-buffers))
	 (buffers (delq nil (mapcar (lambda (buffer)
				      (unless (buffer-modified-p buffer)
					buffer))
				    buffers))))
    (star-tabs--kill-buffers buffers)))

(defun star-tabs-kill-all-inactive-buffers-in-filter (&optional filter-name)
  "Kill all buffers that are not shown in a window in the filter group FILTER-NAME.
FILTER-NAME defaults to the currently active filter."
  (interactive)
  (or filter-name (setq filter-name (star-tabs-get-active-filter-name)))
  (let* ((buffers (star-tabs-filter-buffers filter-name star-tabs-active-buffers))
	 (buffers (delq nil (mapcar (lambda (buffer)
				      (unless (get-buffer-window buffer)
					buffer))
				    buffers))))
   (star-tabs--kill-buffers buffers)))

(defun star-tabs-kill-all-unmodified-inactive-buffers-in-filter (&optional filter-name)
  "Kill all unmodified buffers that are not shown in a window in the filter group FILTER-NAME.
FILTER-NAME defaults to the currently active filter."
  (interactive)
  (or filter-name (setq filter-name (star-tabs-get-active-filter-name)))
  (let* ((buffers (star-tabs-filter-buffers filter-name star-tabs-active-buffers))
	 (buffers (delq nil (mapcar (lambda (buffer)
				      (unless (or (get-buffer-window buffer)
						   (buffer-modified-p buffer))
					buffer))
				    buffers))))
   (star-tabs--kill-buffers buffers)))

(defun star-tabs--kill-buffers (buffers)
  "Kill buffers BUFFERS."
  (dolist (buffer buffers)
    (kill-buffer buffer)))

;; Helper functions

(defun star-tabs-buffer-read-only-p (buffer-or-name)
  "Return t if buffer BUFFER-OR-NAME is read-only; otherwise return nil."
  (not (with-current-buffer buffer-or-name (null buffer-read-only))))

(defun star-tabs--modified-state-changed-p (buffer)
  "Return t if the state of (buffer-modified-p) changed for buffer BUFFER since the last time this function was called.
Otherwise, return nil. This should only be used inside function (star-tabs--buffer-list)."
  (if (equal (buffer-modified-p buffer) (gethash buffer star-tabs-modified-state-changed-buffer-table "not set"))
      nil
    (progn
      (puthash buffer (buffer-modified-p buffer) star-tabs-modified-state-changed-buffer-table)
      t)))

(defun star-tabs-current-buffer ()
  "Return the current buffer if it's being displayed in a window. Otherwise, return the last current buffer displayed in a window.
This is used instead of (current-buffer) because (current-buffer) 
sometimes returns temporary/unreal buffers."
  (if (get-buffer-window (current-buffer))
      (current-buffer)
    star-tabs-current-buffer))

(defun star-tabs-get-buffer-names (buffer-list)
  "Return the names (as strings) of buffers BUFFER-LIST."
  (let ((buffers (mapcar 'buffer-name buffer-list)))
    buffers))

(defun star-tabs-get-buffers (buffer-names)
  "Return a list of buffer BUFFER-NAMES."
  (mapcar 'get-buffer buffer-names))

(defun star-tabs-current-buffer-name ()
  "Return the name of the current buffer."
    (buffer-name (star-tabs-current-buffer)))


;; Buffer list

(defun star-tabs--update-buffer-list () 
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

(defun star-tabs--buffer-list (&optional force-refresh)
   "Return a filtered list of buffers on one or more on the following conditions:
 1. star-tabs-active-buffers, the list of all buffers after global filters have been applied, has changed.
 2. The current buffer has changed, and the new current buffer is a 'real' buffer (i.e. not ephemeral or 
 considered'unimportant').
 3. The un/modified state of the current buffer has changed.
 4. The active filter has changed.

 This should only be used as an argument for star-tabs--set-header-line in order to
 make sure the buffer list is in sync with the tab bar. "

   ;; Make sure all of conditions are checked, regardless of whether previous conditions are true (which is why
   ;; the checks are not done within the (or) function).
   (or force-refresh (setq force-refresh nil))
   (let* ((buffer-list-updated-p (star-tabs--update-buffer-list))
	  (buffer-switched-p (star-tabs--buffer-switched-p))
	  (modified-state-changed-p (star-tabs--modified-state-changed-p star-tabs-current-buffer))
	  (filter-changed-p (star-tabs--filter-changed-p))
	  (buffers)
	  (counter 1)
	  enum-buffer-list)
     (if (or buffer-list-updated-p 
	     modified-state-changed-p
	     filter-changed-p
	     buffer-switched-p
	     force-refresh)
	 (progn 
	   (when star-tabs-debug-messages
	     (message "Blist updated: %s\nModStateChanged: %s\nFilterChanged: %s\nBufferSwtched: %s\nForce: %s\nBuffer: %s"
		      buffer-list-updated-p
		      modified-state-changed-p
		      filter-changed-p
		      buffer-switched-p
		      force-refresh
		      (current-buffer)))
	   ;; Add file extension filters on one of the two conditions:
	   ;; 1. The currently active filter collection has the property :enable-file-extension-filters set to non-nil
	   ;; 2. The currently active filter collection has the property :enable-file-extension-filters set to nil,
	   ;; and a threshold set above 0 and the total number of buffers (after global filters were applied) exceeds that number.
	   (setq star-tabs-add-file-extension-filters
		 (or (plist-get (star-tabs-active-filter-collection-props) :enable-file-extension-filters) nil))
	   ;; Activate the file extension filters if the buffer count exceeds a certain number
	   (when (and (not (plist-get (star-tabs-active-filter-collection-props) :enable-file-extension-filters))
		      (not (<= star-tabs-file-ext-filter-buffer-threshold 0)))
	     (star-tabs--auto-activate-file-extension-filters-on-buffer-count (star-tabs-get-filter-collection-prop-value
									       :file-extension-filter-threshold)))
	   ;; Add and remove file extension filters in the current collection, based on what buffers are currently open.
	   (if star-tabs-add-file-extension-filters
	       (star-tabs--update-file-extension-filters)
	     ;; Remove all automatically set file extension filters in case none of the two conditions described
	     ;; above are met.
	     (when star-tabs-file-extension-filter-names
	       (star-tabs--remove-file-extension-filters)))
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
  "Switch to buffer when its respective tab is clicked on."
  (interactive "e")
  (let* ((window (posn-string (event-start event)))
	 (buffer-name (get-text-property (cdr window) 'buffer-name (car window))))
    (switch-to-buffer buffer-name)))

(defun star-tabs-close-buffer-on-click (event)
  "Close buffer when its respective close button is clicked."
  (interactive "e")
  (let* ((window (posn-string (event-start event)))
	 (buffer-name (get-text-property (cdr window) 'buffer-name (car window))))
    (kill-buffer buffer-name)))

(defun star-tabs--buffer-switched-p ()
  "Return t if the current buffer has been switched since last time this function was called. Otherwise, return nil.
This function should only be used in one place, inside (star-tabs--buffer-list)."
  ;; Make sure it's a real buffer.
  (when (and (get-buffer-window (current-buffer))
	     (not (string-prefix-p " " (buffer-name (current-buffer))))
	     (member (current-buffer) star-tabs-active-buffers))
    (if (not (equal star-tabs-current-buffer (current-buffer)))
	(progn
	  (when star-tabs-tab-bar-filter-name
	    (star-tabs--display-filter-name-temporarily))
	  (setq star-tabs-current-buffer (current-buffer))
	  t)
      nil)))


;;; Display


;; Scrolling

(defun star-tabs-scroll-tab-bar (&optional backward count)
  "Horizontally scroll the tab bar to the right (left if BACKWARD is non-nil), COUNT (default 3) times."
  (interactive)
  ;; FIXME: scrolling before filter name has disappeared will reset scrolling
  ;; FIXME: Decide when to maintain scrolling and when to reset. (get prop of first tab bar to get value of scrolling)
  ;; TODO: Cache header-line-format (maybe not necessary)
  (or count (setq count 3))
  (let* ((first-tab-number (star-tabs--first-number-in-tab-bar))
	 (count (if backward
		    (- (- first-tab-number 1) count)
		  (+ (- first-tab-number 1) count)))) 
  ;; Only scroll forward (right) if the tab bar is truncated, otherwise there's really no need to scroll forward. 
  (when (star-tabs--string-truncated-p star-tabs-header-line-format)
    ;; Make sure we don't scroll past the last buffer.
    (setq count (min
		 (1- (length star-tabs-active-filtered-buffers-enum))
		 count))
    (length star-tabs-active-filtered-buffers-enum)
    (star-tabs--set-header-line star-tabs-active-filtered-buffers-enum count t))
  ;; When going backward:
  (when (and (>= first-tab-number 2)
	     backward)
    (star-tabs--set-header-line star-tabs-active-filtered-buffers-enum count t))))

(defun star-tabs-scroll-tab-bar-forward (&optional count)
  "Scroll tab bar forward COUNT (prefix argument, default 2) tabs."
  (interactive "P") 
  (or count (setq count 2))
  (star-tabs-scroll-tab-bar nil count))

(defun star-tabs-scroll-tab-bar-backward (&optional count)
  "Scroll tab bar forward COUNT (prefix argument, default 2) tabs."
  (interactive "P") 
  (or count (setq count 2))
  (star-tabs-scroll-tab-bar t count))


;; Reordering 

(defun star-tabs-move-tab (&optional backward)
  "Move the currently active tab one step to the right (or left, if BACKWARD is non-nil).
Note that this might also change the tab's position in other filter groups."
  (when (> (length star-tabs-active-filtered-buffers-enum) 1) ; No need to move the tab if there is just 1 or less tabs.
    (let* ((active-tab-buffer (star-tabs-current-buffer))
	   (adjacent-tab-buffer (star-tabs-left-of-elt
				 (if backward
				     (star-tabs-filter-buffers (star-tabs-get-active-filter-name) star-tabs-active-buffers)
				   (reverse (star-tabs-filter-buffers (star-tabs-get-active-filter-name) star-tabs-active-buffers)))
				 active-tab-buffer))
	   (adjacent-tab-buffer-pos (cl-position adjacent-tab-buffer star-tabs-active-buffers)))
      (when adjacent-tab-buffer
	(setq star-tabs-active-buffers(star-tabs-insert-at-nth (remove active-tab-buffer star-tabs-active-buffers)
							       active-tab-buffer
							       adjacent-tab-buffer-pos)))))
  (star-tabs-display-tab-bar t))

(defun star-tabs-move-tab-right ()
  "Move the currently active tab one step to the right in the tab bar.
This only works if the active buffer is part of the active filter group."
  (interactive)
  (star-tabs-move-tab))

(defun star-tabs-move-tab-left ()
  "Move the currently active tab to the left in the tab bar.
This only works if the active buffer is part of the active filter group."
  (interactive)
  (star-tabs-move-tab t))


;; Set display

(defun star-tabs--set-header-line (buffers &optional scroll truncatedp)
  "Set the tab bar to list buffers BUFFERS as tabs.
If SCROLL is set to an integer higher than 0, skip that many tabs if TRUNCATEDP is non-nil."
  ;; If there are no buffers in any group in the current collection, display a message. 
  (or scroll (setq scroll 0))
  (if (and (not buffers)
	   (not star-tabs-active-filtered-buffers-enum))
      (setq star-tabs-header-line-format "   No buffers in any group in current collection.")
    ;; Build the tab bar using propertized strings.
    (when (and buffers
	       (not (window-dedicated-p (get-buffer-window (current-buffer)))))
      (let ((tab-bar-left-margin ""))
	(setq tab-bar-left-margin
	      ;; It's all just one giant string...start with the margin:
	      (concat (propertize star-tabs-left-margin
				  'face 'star-tabs-tab-bar-left-margin)
		      ;; Display the name of the active filter:
		      (concat (propertize (concat 
					   (when (and (plist-get (star-tabs-active-filter-collection-props) :display-filter-name)
						      star-tabs-tab-bar-filter-name)
					     (let ((filter-name star-tabs-tab-bar-filter-name))
					       (concat (upcase (symbol-name filter-name))
						       star-tabs-filter-name-number-separator))))
					  'face 'star-tabs-filter-name))))
	(setq star-tabs-header-line-format
	      (concat tab-bar-left-margin
		      ;; Display tabs:
		      (let ((tab-line "")  ; This will be returned from the let function and concat'd with the rest of the string.
			    (counter 1)) ; Give each tab a unique, incrementing number.
			(dolist (buffer buffers tab-line)
			  (let ((name (buffer-name (cdr buffer))))
			    (unless (< (1- counter) scroll) 
			      (setq tab-line
				    (concat tab-line (star-tabs--tab name counter))))
			    (setq counter (1+ counter))))))))
      ;; Add a fill to the unused area of the tab bar.
      (setq star-tabs-header-line-format (concat star-tabs-header-line-format (star-tabs--header-line-white-space)))))
  (force-mode-line-update t)
  nil)

(defun star-tabs--tab (buffer-name number)
  "Return a propertized string that represents a tab for buffer BUFFER-NAME (string)."
  (let* ((name buffer-name)
	 ;; Number and name:
	 (number-and-name (propertize (concat
				       (number-to-string number)
				       star-tabs-number-name-separator
				       name
				       star-tabs-name-modified-icon-separator)
				      'keymap star-tabs-map-select-tab
				      'face 
				      (if (equal name (star-tabs-current-buffer-name))
					  'star-tabs-selected-tab
					'star-tabs-non-selected-tab)
				      'buffer-name name
				      'mouse-face 
				      (if (equal name (star-tabs-current-buffer-name))
					  'star-tabs-mouse-selected
					'star-tabs-mouse-non-selected)
				      'buffer-number number))
	 ;; Modified symbol:
	 ;; Don't show (un)modified symbol for system buffers or read-only buffers.
	 (modified-icon (propertize (if (and(not (string-match "^[[:space:]]" name))
					    (not (string-match "^*.*\\*$" name))
					    (not (star-tabs-buffer-read-only-p name)))
					;; Display (un)modified symbol:
					(concat  
					 (if (buffer-modified-p (get-buffer name))
					     star-tabs-modified-buffer-icon
					   star-tabs-unmodified-buffer-icon)
					 (when (not(star-tabs-get-filter-collection-prop-value
						    :hide-close-buttons))
					   star-tabs-modified-icon-close-button-separator))
				      ;; Display nothing if it's a system or read-only buffer:
				      "")
				    'keymap star-tabs-map-select-tab
				    'face 
				    (if (equal name (star-tabs-current-buffer-name))
					'star-tabs-selected-tab
				      'star-tabs-non-selected-tab)
				    'mouse-face 
				    (if (equal name (star-tabs-current-buffer-name))
					'star-tabs-mouse-selected
				      'star-tabs-mouse-non-selected)
				    'buffer-name name
				    'buffer-number number))
	 ;; Close button:
	 ;; Conditionally display close button
	 (close-button (propertize (if (not(star-tabs-get-filter-collection-prop-value
					    :hide-close-buttons))
				       star-tabs-close-buffer-icon
				     "")
				   'keymap star-tabs-map-close-tab
				   'face 
				   (if (equal name (star-tabs-current-buffer-name))
				       'star-tabs-selected-tab
				     'star-tabs-non-selected-tab)
				   'mouse-face 
				   (if (equal name (star-tabs-current-buffer-name))
				       'star-tabs-mouse-selected
				     'star-tabs-mouse-non-selected)
				   'buffer-name name
				   'buffer-number number))
	 ;; Icon:
	 (icon-background (if (equal name (star-tabs-current-buffer-name))
			      (face-background 'star-tabs-selected-tab)
			    (face-background 'star-tabs-non-selected-tab)))
	 (icon (star-tabs--select-icon name))
	 (icon (when (stringp icon)
		 (propertize icon
			     'face `(:inherit ,(get-text-property 0 'face icon)
					      :background ,icon-background)
			     
			      'mouse-face 
			      (if (equal name (star-tabs-current-buffer-name))
				  'star-tabs-mouse-selected
				'star-tabs-mouse-non-selected))))
	 ;; Space between elements of tab:
	 (divider (propertize " " 
			      'keymap star-tabs-map-select-tab
			      'face 
			      (if (equal name (star-tabs-current-buffer-name))
				  'star-tabs-selected-tab
				'star-tabs-non-selected-tab)
			      'buffer-name name
			      'mouse-face 
			      (if (equal name (star-tabs-current-buffer-name))
				  'star-tabs-mouse-selected
				'star-tabs-mouse-non-selected)
			      'buffer-number number))
	 ;; Space between tabs:
	 (tab-divider (propertize " " 
				  'keymap star-tabs-map-select-tab
				  'face 
				  (if (equal name (star-tabs-current-buffer-name))
				      'star-tabs-selected-tab
				    'star-tabs-non-selected-tab)
				  'mouse-face 
				  (if (equal name (star-tabs-current-buffer-name))
				      'star-tabs-tab-divider-mouse-selected
				    'star-tabs-tab-divider-mouse-non-selected)
				  'buffer-name name
				  'buffer-number number)))
    ;; Final tab:
    (concat divider
	    (when (stringp icon)
	      icon)
	    divider
	    number-and-name
	    modified-icon
	    close-button
	    tab-divider)))

(defun star-tabs--select-icon (buffer)
  (with-current-buffer buffer
   (all-the-icons-icon-for-mode major-mode :v-adjust 0.001 :height 0.8)))

(defun star-tabs--header-line-white-space ()
  "Return white space to fill out the unoccupied part, if any, of tab bar."
  (let ((empty-space (/ (star-tabs--header-line-remaining-space)
			(window-font-width nil 'star-tabs-non-selected-tab)))
	(white-space ""))
    (while (> empty-space 0)
      (setq white-space (concat " " white-space))
      (setq empty-space (1- empty-space)))
    (propertize white-space
		'face 'star-tabs-non-selected-tab))) 

(defun star-tabs--display-filter-name-temporarily (&optional filter-name)
  "Return filter name FILTER-NAME for temporary display in tab bar. 
Unless set, FILTER-NAME defaults to the currently active filter name.
This function uses global helper variable star-tabs-last-timer to keep track of the timer."
  ;; Force cancel on any other active timers set with this function.
  (when star-tabs-last-timer
    (cancel-timer star-tabs-last-timer)
    star-tabs-last-timer nil)
  (setq star-tabs-last-timer (star-tabs-set-temporarily 'star-tabs-tab-bar-filter-name
							(star-tabs-get-active-filter-name)
							"1 sec"
							nil
							#'star-tabs-display-tab-bar
							t)))


;; Display helper functions

(defun star-tabs--string-truncated-p (string)
  "Return t if the width of the tab bar is greater than the width of the current window.
Otherwise, return the number of truncated pixels."
  (let ((tab-bar-width (star-tabs-string-pixel-width string))
	(window-width (window-pixel-width)))
    (if (>
	 tab-bar-width
	 (1+ window-width))
	(- tab-bar-width window-width)
      nil)))

(defun star-tabs--header-line-remaining-space()
  "Return the width in pixels between the end of the last tab and the right edge of the window."
  ;; FIXME: Make more accurate calculation of empty space in tab bar.
  (- (window-pixel-width) (star-tabs-string-pixel-width star-tabs-header-line-format)))

(defun star-tabs--tab-bar-left-margin-width ()
  "Return the column width of the left margin of the tab bar.
If set, also include the column width of the filter name. In other words,
return the column width of all characters left of the beginning of the first tab."
  (+ 
   (length star-tabs-left-margin)
   (if star-tabs-tab-bar-filter-name
       (+ (length star-tabs-filter-name-number-separator)
	  (length (symbol-name star-tabs-tab-bar-filter-name)))
     0)))

(defun star-tabs--first-number-in-tab-bar ()
  "Return the tab number of the first visible tab in the tab bar.
Or, return 0 if there are no tabs."
  (or (get-text-property (star-tabs--tab-bar-left-margin-width)
			 'buffer-number header-line-format)
      0))

(defun star-tabs--current-buffer-number ()
  "Return the tab number of the current buffer.
If the current buffer is not in the active filter group, return 0."
  (1+ (or (cl-position (star-tabs-current-buffer)
	       (star-tabs-filter-buffers
		(star-tabs-get-active-filter-name) star-tabs-active-buffers))
      -1)))



;;; Functions to run with hooks

(defun star-tabs-when-buffer-first-modified ()
  "Run when a buffer goes from an unmodified state to a modified state."
  (if (member (current-buffer) star-tabs-active-buffers)
      (progn (set-buffer-modified-p t) ; HACK: Make sure that buffer-modified-p is set to t even though it should be.
	     (star-tabs-display-tab-bar  t))))

(defun star-tabs-when-buffer-first-saved ()
   "Run when a buffer goes from a modified state to an unmodified state."
   (when (member (current-buffer) star-tabs-active-buffers)
     (set-buffer-modified-p nil) ; HACK: Make sure that buffer-modified-p is set to nil even though it should be.
     (star-tabs-display-tab-bar t)))

(defun star-tabs-display-tab-bar (&optional force-refresh)
  "Display the tab bar. Refresh when either 1) FORCE-REFRESH is non-nil, 2) any of the conditions in (star-tabs--buffer-list) are met."
  (unless (window-dedicated-p) ; Only show the tab bar in non-dedicated windows
    (star-tabs--set-header-line (star-tabs--buffer-list force-refresh)))
    nil)


;;; Modes

(define-minor-mode star-tabs-tab-bar-mode
  "...desc..."
  ;;:lighter " ST"
  :global t

  (if star-tabs-tab-bar-mode
      (progn (star-tabs-init-filters)
	     ;; Refresh the tab bar when buffers are created or killed.
	     (add-hook 'buffer-list-update-hook #'star-tabs-display-tab-bar nil nil)
	     ;; Functions to run when a buffer goes from an unmodified to a modified state.
	     (add-hook 'first-change-hook #'star-tabs-when-buffer-first-modified nil nil)
	     ;; Update the tab bar when a buffer is saved.
	     (add-hook 'after-save-hook #'star-tabs-when-buffer-first-saved nil nil))))



;;; TODO: Unused functions; remove or fix.

(defun star-tabs--switch-to-first-in-new-filter (filter-name)
 "Switch to the first buffer in filter FILTER, or the last buffer before switching filters, as long as the last
was also a filter switch command."

 ;; TODO: Currently not used. Remove or implement.
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

(defun star-tabs--add-file-extension-filters (&optional collection-name)
  "DEPRECATED: (?) Automatically add filters for each file type among all open buffers to filter collection COLLECTION-NAME.
COLLECTION-NAME defaults to the currently active filter collection."
  (setq collection-name (or collection-name (star-tabs-active-filter-collection-name)))
  ;; Get all file extensions and turn them into filters.
  (let ((file-extensions (mapcar 'intern (star-tabs-get-file-extensions))))
    (dolist (ext file-extensions)
      (star-tabs--add-file-extension-filter ext collection-name)))
  ;; Add a filter for extensionless files too.
  (star-tabs-add-filter
   :name 'extensionless
   :include '("^[a-z0-9A-Z]+$")
   :collection collection-name)
  nil)

(defun star-tabs-get-first-buffer-in-filter (filter-name) ; FIXME: can cause infinite loops probably
  "Return the first buffer found in filter FILTER-NAME. If buffer star-tabs-current-buffer
exists in filter, return buffer star-tabs-current-buffer instead."
  (let((buffers (star-tabs-filter-buffers filter-name star-tabs-active-buffers)))
    (if (member star-tabs-current-buffer buffers)
	;; If the buffer we were previously in exists in the filter group, return that buffer.
	(unless (eq star-tabs-current-buffer (buffer-name (star-tabs-current-buffer)))
	  star-tabs-current-buffer)
      ;; Otherwise, return the first buffer of the filter group.
      (car buffers))))


(provide 'star-tabs)

