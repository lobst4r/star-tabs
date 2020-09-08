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

(defvar star-tabs-left-margin "   " 
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


;; Hooks

(defvar star-tabs-buffer-switch-hook nil
  "Functions to run when the current (real) buffer switches.")

(defvar star-tabs-buffer-list-update-hook nil
  "Functions to run when the Star Tabs-curated list of real buffers is updated.")

(defvar star-tabs-filter-change-hook nil
  "Functions to run when the active filter group changes.")

(defvar star-tabs-collection-change-hook nil
  "Functions to run when the active filter collection changes.")

(defvar star-tabs-timer-start-hook nil
  "Functions to run when a Star Tabs timer starts.")

(defvar star-tabs-timer-end-hook nil
  "Functions to run when a Star Tabs timer ends.")

(defvar star-tabs-init-hook nil
  "Functions to run when Star Tabs first loads.")

(defvar star-tabs-disable-tab-bar-hook nil
  "Functions to run when Star Tabs goes from enabled to disabled.")

(defvar star-tabs-move-tab-hook nil
  "Functions to run when a tab changes position in the tab bar.")

(defvar star-tabs-collection-property-change-hook nil
  "Functions to run when the property of a collection changes.
Note that only Star Tabs functions will trigger this hook.")


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

(defvar star-tabs-filter-name-timer nil
  "The last used timer set automatically by (star-tabs--display-filter-name-temporarily).")

(defvar star-tabs-collection-name-timer nil
  "The last used timer set automatically by (star-tabs--display-collection-name-temporarily).")

(defvar star-tabs-debug-messages t
  "If set to non-nil, debug messages will be displayed.")

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

(defvar star-tabs-global-exclusion-prefix-filter '("*" "magit-" "magit:")
  "List of buffer name prefixes to be excluded globally. Buffers filtered this way will be cached and ignored
for all future searches. As such, global filtering may increase performance, and
should (and should only!) be applied to buffers that you really don't care about.

Buffers with the space prefix (\" \") are automatically filtered before this filter is applied, and thus need not
be added to this list.

This filter is applied after star-tabs-global-inclusion-prefix-filter.")

(defvar star-tabs-tab-bar-filter-name nil
  "Filter name to be displayed in the tab bar; automatically set by other functions.")

(defvar star-tabs-tab-bar-collection-name nil
  "Collection name to be displayed in the tab bar; automatically set by other functions.")

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

(defvar star-tabs-active-buffers nil
  "List of all currently active/'real' buffers.
A 'real' or 'active' buffer refers to an open buffer that is not ephemeral/temporary or otherwise deemed unimportant.")

(defvar star-tabs-active-group-buffers nil
  "List of all buffers in the active group")


;;; Visuals

(defvar star-tabs-tab-bar-height 210
  "Height of the tab bar.")

(defvar star-tabs-tab-bar-text-height 155
  "Text height for tabs.")

(defvar star-tabs-tab-bar-filter-name-foreground "#ef21b3"
  "Foreground color for tab bar filter name.")

(defvar star-tabs-tab-bar-collection-name-foreground "#7cd164"
  "Foreground color for tab bar collection name.")

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

(defface star-tabs-collection-name
  `((t
     (
      :background ,star-tabs-tab-bar-non-selected-background
      :foreground ,star-tabs-tab-bar-collection-name-foreground
      :height ,star-tabs-tab-bar-text-height)))
  "Face for displaying collection name in the tab bar.")

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
  (run-hooks 'star-tabs-timer-start-hook)
  (run-at-time duration
	       nil
	       (lambda (symbol value-after func-after args)
		 ;; Set to VALUE-AFTER (default nil) when the timer ends.
		 (set symbol value-after)
		 ;; If set, run FUNC-AFTER when the timer ends.
		 (when func-after 
		   (apply func-after args)
		   (run-hooks 'star-tabs-timer-end-hook)))
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

(defun star-tabs-update-list (old-list new-list)
  "Return list NEW-LIST, using the order of OLD-LIST to the extent possible.
More specifically, the order of OLD-LIST is kept, and any new elements from NEW-LIST will be appended at the end.
Any elements in OLD-LIST not in NEW-LIST will be removed."
  (append (seq-intersection old-list new-list)
	  (seq-difference new-list old-list)))


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
	 (disable-scroll-to-filter (plist-get collection-props :disable-scroll-to-filter))
	 (collection-name-prefix (or (plist-get collection-props :collection-name-prefix) "star-tabs-filter-collection-"))
	 (name-no-prefix (plist-get collection-props :name))
	 (name (intern (concat collection-name-prefix (plist-get collection-props :name))))
	 (collection `(,name :enable-file-extension-filters ,enable-file-extension-filters
			     :display-filter-name ,display-filter-name
			     :file-extension-filter-threshold ,file-extension-filter-threshold
			     :disable-scroll-to-filter ,disable-scroll-to-filter
			     :collection-name-prefix ,collection-name-prefix
			     :collection-name-no-prefix ,name-no-prefix
			     :last-filter nil)))
    (if (not (member name (star-tabs-filter-collection-names)))
	(progn (set name nil) 
	       (setq star-tabs-filter-collections
		     (append star-tabs-filter-collections (list collection)))
	       ;; Switch to the new collection upon creation if :use is non-nil.
	       (when use
		 (while (not (eq (star-tabs-active-filter-collection-name) name))
		   (star-tabs-cycle-filter-collections t t))
		 (run-hooks 'star-tabs-collection-change-hook)))
      (message "Collection name already exists"))))

(defun star-tabs-filter-collection-names ()
  "Return the names of all filter collections."
  (mapcar 'car star-tabs-filter-collections))

;; FIXME currently creating collection while the list is rotated causes the collections to be in the wrong order
;; Solve this by rotating the list back to the beginning (store first added list, move to second if the first is deleted etc)
;; Or just keeping track of last added..?
;; Or just ignore ??

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
  (unless inhibit-refresh
    (run-hooks 'star-tabs-collection-change-hook)))

(defun star-tabs-active-filter-collection-name (&optional no-prefix)
  "Return the name of the active filter collection."
  (if no-prefix
      (intern (star-tabs-get-filter-collection-prop-value :collection-name-no-prefix))
    (car (car star-tabs-filter-collections))))

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
  (run-hooks 'star-tabs-collection-property-change-hook))

(defun star-tabs-filter-collection-props (&optional collection-name)
  "Return the properties of filter collection COLLECTION-NAME."
  (or collection-name (setq collection-name (star-tabs-active-filter-collection-name)))
  (alist-get collection-name star-tabs-filter-collections))

(defun star-tabs-active-filter-collection-props ()
  "Return the properties of the currently active filter collection."
    (star-tabs-filter-collection-props (star-tabs-active-filter-collection-name)))


;;; Filters

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

  ;; TODO add auto-sort expl. to readme
  ;; TODO add expl of :inhibit-refresh
  (let* ((name (plist-get filter-props :name))
	 (exclude (plist-get filter-props :exclude))
	 (include (plist-get filter-props :include))
	 (collection-name (or (plist-get filter-props :collection) (star-tabs-active-filter-collection-name)))
	 (auto-sort (or (plist-get filter-props :auto-sort) nil))
	 (inhibit-refresh (or (plist-get filter-props :inhibit-refresh)))
	 (filter `(,name :exclude ,exclude
			 :include ,include
			 :auto-sort ,auto-sort))
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
      (message "Filter name already exists"))
    (unless inhibit-refresh
      (run-hooks 'star-tabs-collection-property-change-hook))))

(defun star-tabs-remove-filter (filter-name &optional inhibit-refresh collection-name)
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
  (unless inhibit-refresh
    (run-hooks 'star-tabs-collection-property-change-hook)))

(defun star-tabs-remove-all-filters (&optional inhibit-refresh collection-name)
  "Delete all filters in filter collection COLLECTION-NAME.
COLLECTION-NAME defaults to the currently active filter collection."
  (setq collection-name (or collection-name (star-tabs-active-filter-collection-name)))
  (let ((filters (star-tabs-get-filter-names)))
    (dolist (filter filters)
      (star-tabs-remove-filter filter inhibit-refresh collection-name))
    (setq star-tabs-file-extension-filter-names nil)))

(defun star-tabs-init-filters ()
  "Initialize default collection and filters."
  (star-tabs-create-filter-collection
   :name "default-collection9"
   :use t
   :enable-file-extension-filters t 
   :display-filter-name t)
  
  (star-tabs-add-filter
   :name 'default
   :exclude '("^[[:space:]]" "^*.*\\*$" "^magit-" "^magit:")
   :inhibit-refresh t)

  (star-tabs-add-filter
   :name 'system
   :include `("^*.*\\*$")
   :inhibit-refresh t)
  
  ;; Add file extension filters if customizable variable is set 
  (if (plist-get (star-tabs-active-filter-collection-props) :enable-file-extension-filters)
      (star-tabs--update-file-extension-filters t)))


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
    (run-hooks 'star-tabs-filter-change-hook)))

(defun star-tabs-find-active-filter (&optional inhibit-refresh) 
  "Find a filter for the current buffer, if such filter exists in the current collection.
If INHIBIT-REFRESH is nil (default), refresh the tab bar as well."
  (interactive)
  (let ((current-buffer (star-tabs-current-buffer))
	(filter-count (length (eval (star-tabs-active-filter-collection-name))))
	(prev-filter-name (star-tabs-get-active-filter-name)))
    ;; Loop through the list of registered filters once, or until a filter is found.
    ;; But there is no need to change filter group if the current buffer is already in the active filter group.
    (while (and (not (member current-buffer (star-tabs-filter-buffers (star-tabs-get-active-filter-name) star-tabs-active-buffers)))
		(>= filter-count 0))
      (star-tabs-cycle-filters nil t)  ; If buffer is not in filter group, move cycle index once.
      (setq filter-count (1- filter-count))) ; Prevent infinite loop in case there is no match.
    ;; Determine if the filter changed.
    (let ((filter-changed-p (not (eq prev-filter-name
				     (star-tabs-get-active-filter-name)))))
      (when (and (not inhibit-refresh)
		 filter-changed-p)
	(run-hooks 'star-tabs-filter-change-hook)) ; REVIEW: Will this trigger even if we don't actually change filter?
      (if filter-changed-p
	  (star-tabs-get-active-filter-name)
	nil))))

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
      (star-tabs-set-filter-prop-value :always-include always-include nil filter-name collection-name)))
  ;; (plist-put (alist-get filter-name (eval collection-name))
  ;; 		 :always-include always-include)))
  )

(defun star-tabs-exclude-from-filter (buffer &optional filter-name collection-name)
  "Exclude buffer BUFFER from filter FILTER-NAME of collection COLLECTION-NAME.
Also remove it from automatic inclusion, if applicable."
  ;; REVIEW: can regexp match other buffers as well?
  (or filter-name (setq filter-name (star-tabs-get-active-filter-name)))
  (or collection-name (setq collection-name (star-tabs-active-filter-collection-name)))
  (let* ((buffer-name (buffer-name buffer))
	 (regexp (regexp-quote buffer-name))
	 (always-include (plist-get (alist-get filter-name (eval collection-name)) :always-include))
	 (always-include (remove regexp always-include))
	 (exclude (plist-get (alist-get filter-name (eval collection-name)) :exclude))
	 (exclude (when (not (member regexp exclude))
			     (push regexp exclude))))
    (star-tabs-set-filter-prop-value :always-include always-include nil filter-name collection-name)
    (when exclude
      (star-tabs-set-filter-prop-value :always-include always-include nil filter-name collection-name))
    ))

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

(defun star-tabs-auto-sort (&optional filter-name collection-name sort-method)
  "Automatically sort filter group FILTER-NAME in collection COLLECTION-NAME.
FILTER-NAME defaults to the active filter group.
COLLECTION-NAME defaults to the active filter collection.
SORT-METHOD defaults to the filter property :auto-sort of FILTER-NAME.
If filter property :auto-sort is non-nil and SORT-METHOD is not specified, nothing will be sorted.\n
The following will explain the different options for SORT-METHOD:
- 'recent-first - The most recent (current) buffer will be the first (left-most) tab. The second most recent will be the second tab, and so on. 
The last (right-most) tab will thus be the buffer to last be revisited/reopened." 
;; TODO: Add the following sorting methods:
;; - 'last-first (to be implemented) - The opposite of 'recent-first. That is, the most recent buffer will be last in the tab bar. 
;; - 'alpha-desc (to be implemented) - Tabs will be sorted alphabetically, in a descending order; buffers with names starting with 'a' will appear 
;; left of those starting with 'z', for example.
;; - 'alpha-asc (to be implemented) - The same as 'alpha-desc, except that tabs will be sorted in an ascending alphabetical order.
;; - 'extension-desc (to be implemented) - Sort tabs by their file extensions alphabetically in a descending order. 
;; - 'extension-asc (to be implemented) - The same as 'extension-desc, except that the ordering will be in an ascending order.
;; TODO Add :auto-sort as a collection property as well. If it's not set on filter-level, it will default to the collection property (if set), 
;; otherwise nil
  (or filter-name (setq filter-name (star-tabs-get-active-filter-name)))
  (or collection-name (setq collection-name (star-tabs-active-filter-collection-name)))
  (or sort-method (setq sort-method (star-tabs-get-filter-prop-value :auto-sort filter-name collection-name)))
  (when sort-method
    (cond ((equal sort-method 'recent-first)
	   (star-tabs-move-current-tab-to-first)))))

(defun star-tabs-set-filter-prop-value (prop value &optional inhibit-hook filter-name collection-name)
  "Set property PROP of filter FILTER-NAME in collection COLLECTION-NAME to value VALUE."
  (or filter-name (setq filter-name (star-tabs-get-active-filter-name)))
  (or collection-name (setq collection-name (star-tabs-active-filter-collection-name)))
  (plist-put (star-tabs-get-filter-props filter-name collection-name) prop value)
  (unless inhibit-hook
    (run-hooks 'star-tabs-collection-property-change-hook)))


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

(defun star-tabs-get-filter-prop-value (prop &optional filter-name collection-name)
  "Return the value of the property PROP of filter FILTER-NAME in collection COLLECTION-NAME."
  (or filter-name (setq filter-name (star-tabs-get-active-filter-name)))
  (or collection-name (setq collection-name (star-tabs-active-filter-collection-name)))
  (let ((props (star-tabs-get-filter-props filter-name collection-name)))
    (plist-get props prop)))

(defun star-tabs-get-filter-props (&optional filter-name collection-name)
  "Return the properties of filter FILTER-NAME in collection COLLECTION-NAME."
  (or filter-name (setq filter-name (star-tabs-get-active-filter-name)))
  (or collection-name (setq collection-name (star-tabs-active-filter-collection-name)))
  (alist-get filter-name (eval collection-name)))


;; Apply filters

(defun star-tabs-filter-buffers (filter-name buffer-list)
  "Filter buffers BUFFER-LIST with filter FILTER and return the filtered list of buffers."
  ;; REVIEW: No longer necessary to maintain the order when filtering since that is handled elsewhere. Refactor?
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

(defun star-tabs--add-file-extension-filter (extension-name &optional inhibit-refresh collection-name)
  "Add an inclusive filter for file extension EXTENSION-NAME to filter collection COLLECTION-NAME.
COLLECTION-NAME defaults to the currently active filter collection.
Return non-nil if a filter was added, otherwise return nil."
  (setq collection-name (or collection-name (star-tabs-active-filter-collection-name)))
  ;; Only add a filter if does not already exist in COLLECTION-NAME.
  (if (or (not (member extension-name star-tabs-file-extension-filter-names))
	  (and (not (alist-get extension-name (eval collection-name)))
	       (member extension-name star-tabs-file-extension-filter-names)))
      ;; Add the filter to the filter list, then add the file extension name to the file extension list.
      (progn (star-tabs-add-filter
	      :name extension-name
	      :include (list(concat (symbol-name extension-name) "$"))
	      :collection collection-name
	      :inhibit-refresh inhibit-refresh)
	     (when (not (member extension-name star-tabs-file-extension-filter-names))
	       (push extension-name star-tabs-file-extension-filter-names))
	     t)
    nil))

(defun star-tabs--update-file-extension-filters (&optional inhibit-refresh collection-name)
  "Update automatically added file extension buffer filters in filter collection COLLECTION-NAME.
COLLECTION-NAME defaults to the currently active filter collection."
  (setq collection-name (or collection-name (star-tabs-active-filter-collection-name)))
  ;; Make sure there is a filter for extensionless files.
  (let ((extensions-updated-p nil)) ; Keep track of whether we actually add or remove a filter.
    (when (not (member 'extensionless (star-tabs-get-filter-names)))
      (star-tabs-add-filter
       :name 'extensionless
       :include '("^[a-z0-9A-Z]+$") ;; FIXME: Make better regexp.
       :collection collection-name
       :inhibit-refresh t)
      (setq extensions-updated-p t))
    (let ((file-extensions (mapcar 'intern (star-tabs-get-file-extensions)))
	  (filter-names star-tabs-file-extension-filter-names))
      ;; Add new filters if there are new file extensions among open buffers.
      (dolist (ext file-extensions)
	(when (or (not (member ext filter-names))
		(and (not (alist-get ext (eval collection-name)))
		     (member ext filter-names)))
	  (setq extensions-updated-p (or (star-tabs--add-file-extension-filter ext t collection-name)
					 extensions-updated-p))))
      ;; Remove automatically added filters if there no longer are buffers with the corresponding file extension.
      (dolist (filter filter-names)
	(when (not (member filter file-extensions))
	  (setq extensions-updated-p (or (star-tabs--remove-file-extension-filter filter t collection-name)
					 extensions-updated-p)))))
    (unless inhibit-refresh
       (star-tabs--set-header-line (star-tabs-get-active-group-buffers) 'keep-scroll))
    extensions-updated-p))

(defun star-tabs--remove-file-extension-filter (filter-name &optional inhibit-refresh collection-name)
  "Remove automatically added file extension filter FILTER-NAME from filter collection COLLECTION-NAME.
COLLECTION-NAME defaults to the currently active filter collection.
Return non-nil if a filter was removed, otherwise nil."
  (setq collection-name (or collection-name (star-tabs-active-filter-collection-name)))
  (if (member filter-name star-tabs-file-extension-filter-names) ; Make sure the filter is one of the automatically added filters.
	;; First remove the filter from the collection...
	(progn (star-tabs-remove-filter filter-name inhibit-refresh collection-name)
	       ;; Then remove the file extension from the global list of file extensions.
	       (setq star-tabs-file-extension-filter-names (delete filter-name star-tabs-file-extension-filter-names))
	       ;; Make sure we're still in a non-empty filter group
	       (if (not (star-tabs-filter-buffers (star-tabs-get-active-filter-name) star-tabs-active-buffers))
		   (star-tabs-cycle-filters t t))
	       t)
    nil))

(defun star-tabs--remove-file-extension-filters (&optional inhibit-refresh collection-name)
  "Remove all automatically added file extension buffer filters from filter collection COLLECTION-NAME.
COLLECTION-NAME defaults to the currently active filter collection."
  (setq collection-name (or collection-name (star-tabs-active-filter-collection-name)))
  (let ((file-extensions star-tabs-file-extension-filter-names)
	(extensions-updated-p nil))
    (dolist (ext file-extensions)
      (setq extensions-updated-p (or (star-tabs--remove-file-extension-filter ext inhibit-refresh collection-name)
				     extensions-updated-p)))
    ;; Remove the extensionless file filter.
    (setq extensions-updated-p (or (star-tabs-remove-filter 'extensionless inhibit-refresh collection-name)
				   extensions-updated-p))
    extensions-updated-p))

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

;; Buffer killing :*(

(defun star-tabs-kill-all-buffers-in-filter (&optional filter-name)
  "Kill all buffers in the filter group FILTER-NAME (defaults to the currently active filter)."
  (interactive)
  (or filter-name (setq filter-name (star-tabs-get-active-filter-name)))
  (let ((buffers (star-tabs-get-group-buffers filter-name)))
    (star-tabs--kill-buffers buffers)))

(defun star-tabs-kill-all-unmodified-buffers-in-filter (&optional filter-name)
  "Kill all unmodified buffers in the filter group FILTER-NAME (defaults to the currently active filter)."
  (interactive)
  (or filter-name (setq filter-name (star-tabs-get-active-filter-name)))
  (let* ((buffers (star-tabs-get-group-buffers filter-name))
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
  (let* ((buffers (star-tabs-get-group-buffers filter-name))
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
  (let* ((buffers (star-tabs-get-group-buffers filter-name))
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
  "Return non-nil if buffer BUFFER-OR-NAME is read-only; otherwise return nil."
  (not (with-current-buffer buffer-or-name (null buffer-read-only))))

(defun star-tabs-current-buffer ()
  "Return the current buffer if it's being displayed in a window. Otherwise, return the last current buffer displayed in a window.
This is used instead of (current-buffer) because (current-buffer) 
sometimes returns temporary/unreal buffers.
;; REVIEW: Is the window-check sufficient?"
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

(defun star-tabs-get-buffer-tab-number (&optional buffer)
  "Return the tab number of buffer BUFFER.
Return 0 if BUFFER is not in the active filter group."
  (1+ (or (cl-position (current-buffer) (star-tabs-get-active-group-buffers))
      -1)))


;; Buffer list

(defun star-tabs--update-buffer-list () 
  "Update the list of 'real' buffers star-tabs-active-buffers
 if (a) buffer(s) have/has been created or killed. 
 Return t if the buffer list was updated, otherwise nil."
  ;; REVIEW: Refactor possible?
  ;; --------------------------
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
    ;; Return nil if there are no new real buffers. Otherwise, apply global filters to the new buffers.
    (if (seq-set-equal-p star-tabs-active-buffers active-buffers)
	nil
      ;; Apply the global filters.
      ;; First include...
      ;; REVIEW: Remove the option to include, and instead just exclude.
      ;;         (although there should be some sort of always-include capability.)
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
	;; Add globally filtered buffers to the cache so we can ignore them next time.
	(star-tabs-add-filtered-buffers-to-cache (seq-difference active-buffers buffer-list) star-tabs-cached-filtered-buffers)
	;; See if the buffer list changed. If it did, update the global list of real buffers.
	;; Otherwise, return nil. 
	(if (not (seq-set-equal-p star-tabs-active-buffers buffer-list))
	    (progn
	      (setq star-tabs-active-buffers (star-tabs-update-list star-tabs-active-buffers buffer-list))
	      (run-hooks 'star-tabs-buffer-list-update-hook)
	      t)
	  nil)))))

(defun star-tabs--filter-all-buffers ()
  "Apply filters to all real buffers in, and set create separate buffer lists for, all filter groups in the active collection."
  ;; TODO: Only apply filters to new buffers, and remove killed buffers, instead of doing it all
  ;;       for all buffers in all filter groups. Make sure all buffers are filtered for any new groups created though.
  (let ((filters (star-tabs-get-filter-names))
	(filtered-buffers))
    (dolist (filter filters)
      (setq filtered-buffers (star-tabs-filter-buffers filter
						       star-tabs-active-buffers))
      (star-tabs-set-filter-prop-value :buffer-list
				       (star-tabs-update-list (star-tabs-get-filter-prop-value :buffer-list
											       filter)
							      filtered-buffers)
				       t
				       filter))))

(defun star-tabs-get-group-buffers (&optional filter-name collection-name)
  "Return all buffers in the filter group FILTER-NAME of collection COLLECTION-NAME as a list."
  (or filter-name (setq filter-name (star-tabs-get-active-filter-name)))
  (or collection-name (setq collection-name (star-tabs-active-filter-collection-name)))
  (star-tabs-get-filter-prop-value :buffer-list filter-name collection-name))

(defun star-tabs-get-active-group-buffers ()
  "Return all buffers in the active filter group as a list."
  (star-tabs-get-group-buffers (star-tabs-get-active-filter-name) (star-tabs-active-filter-collection-name)))


;; Buffer Switching

(defun star-tabs-switch-to-buffer (n)
  "Switch to the buffer associated with tab number N."
  (interactive "p")
  (let ((buffer (nth (1- n) (star-tabs-get-active-group-buffers))))
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
  "Return non-nil if the current buffer has been switched since last time this function was called. Otherwise, return nil.
This function should only be used in one place, inside (star-tabs-on-raw-buffer-list-update).
If the buffer was switched, also run hook star-tabs-buffer-switch-hook."
  (when (and (get-buffer-window (current-buffer))
	     (not (string-prefix-p " " (buffer-name (current-buffer))))
	     (member (current-buffer) star-tabs-active-buffers))
    (if (not (equal star-tabs-current-buffer (current-buffer)))
	(progn
	  ;; Refresh the filter-name timer if it was running when the buffer switched.
	  ;; REVIEW: Do this for collection-name timer as well?
	  (when star-tabs-tab-bar-filter-name
	    (star-tabs--display-filter-name-temporarily))
	  (setq star-tabs-current-buffer (current-buffer))
	  (run-hooks 'star-tabs-buffer-switch-hook)
	  t)
      nil)))


;;; Tab bar

(defun star-tabs--tab (buffer-name number)
  "Create/Update the tab, and return a propertized string that represents the tab, for buffer BUFFER-NAME with tab number NUMBER.
Properties of the tab can be accessed using (star-tabs-get-tab-prop-value TAB-OR-BUFFER PROP FILTER-NAME COLLECTION-NAME).\n
Properties related to the tab are:
:tab-name - The name of the tab.
:tab-number - The number of the tab (potentially deprecated soon).
:tab-string - Propertized string representation of the tab.
:tab-column-width - The width in columns of the string representation of the tab.
:tab-pixel-width - The width in pixels of the string representation of the tab.
:tab-modified-p - Whether the tab is modified (non-nil) or not (nil). "
  ;; TODO: buffer-name -> buffer
  ;; TODO: Rename, update separators/dividers (and make sure they are customizable)
  ;; TODO: Reduce if-statements that check for current buffer.
  ;; FIXME: Icon shouldn't be highlighted on mouse-over.
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
	 ;; TODO: move regexp into variables
	 (modified-icon (propertize (if (and (not (string-match "^[[:space:]]" name))
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
				      ;; Display nothing if it's an unreal buffer, system buffer, or read-only buffer:
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
    (let* ((tab-buffer (get-buffer name))
	   (tab-string (concat divider
			       (when (stringp icon)
				 icon)
			       divider
			       number-and-name
			       modified-icon
			       close-button
			       tab-divider))
	  (tab-number number)
	  (tab-name name)
	  (tab-column-width (length tab-string))
	  (tab-pixel-width (star-tabs-string-pixel-width tab-string))
	  (tab-modified-p (buffer-modified-p (get-buffer name)))
	  (tab `(,tab-buffer :tab-number ,tab-number ; REVIEW: Maybe no need to store this here?
			     :tab-name ,tab-name
			     :tab-string ,tab-string
			     :tab-column-width ,tab-column-width
			     :tab-pixel-width ,tab-pixel-width
			     :tab-modified-p ,tab-modified-p)))
      ;; Add tab to the filter group property :tabs as an alist,
      ;; where the key is the buffer and the value is a plist of tab properties.
      (if (alist-get tab-buffer (star-tabs-get-filter-prop-value :tabs))
		 (setf (alist-get tab-buffer (star-tabs-get-filter-prop-value :tabs)) (cdr tab))
	(star-tabs-set-filter-prop-value :tabs
      					 (append (star-tabs-get-filter-prop-value :tabs)
      						 (list tab))
      					 t))
      tab-string)))

(defun star-tabs-get-tab (buffer &optional filter-name collection-name)
  "Return the tab corresponding to buffer BUFFER in filter group FILTER-NAME of collection COLLECTION-NAME.
If no tab is found, return nil."
  (or filter-name (setq filter-name (star-tabs-get-active-filter-name)))
  (or collection-name (setq collection-name (star-tabs-active-filter-collection-name)))
  (alist-get buffer (star-tabs-get-filter-prop-value :tabs filter-name collection-name)))

(defun star-tabs-get-tab-prop-value (tab-or-buffer prop &optional filter-name collection-name)
  "Return value of property PROP of tab/buffer TAB-OR-BUFFER in filter group FILTER-name of collection COLLECTION-NAME.
Return nil if either the property is not found, or if the tab doesn't exists."
  (or filter-name (setq filter-name (star-tabs-get-active-filter-name)))
  (or collection-name (setq collection-name (star-tabs-active-filter-collection-name)))
  (plist-get (if (bufferp tab-or-buffer)
		 (star-tabs-get-tab tab-or-buffer filter-name collection-name)
	       tab-or-buffer)
	     prop))

(defun star-tabs--select-icon (buffer)
  "Return the all-theicons-icon for buffer BUFFER."
  ;; TODO: add checks for whether all-the-icons is installed (also make sure graphical elements are supported).
  ;; TODO: make height user-customizable.
  (with-current-buffer buffer
   (all-the-icons-icon-for-mode major-mode :v-adjust 0.001 :height 0.8)))

(defun star-tabs--set-tab-bar (&optional filter-name collection-name)
  "Set and return the tab bar of filter group FILTER-NAME of collection COLLECTION-NAME.
This function also sets the left margin (see (star-tabs--set-left-margin)).\n
Properties of the tab bar can be accessed using (star-tabs-get-filter-prop-value PROP FILTER-NAME COLLECTION-NAME).
Properties related to the tab bar are:
:tabs - An unordered list of all tabs in the tab bar (REVIEW: currently includes killed buffers as well, it probably shouldn't?)
:tab-bar - A list of all the tabs in the tab bar.
:tab-bar-format - Set using (star-tabs--set-tab-bar-format), this hold the string representation of the tab bar, which might be truncated.
:tab-bar-cumulative-pixel-width - List of the pixel width of all tabs in the tab bar."
  (or filter-name (setq filter-name (star-tabs-get-active-filter-name)))
  (or collection-name (setq collection-name (star-tabs-active-filter-collection-name)))
  (let ((buffers (star-tabs-get-group-buffers filter-name collection-name)) ; This is the order in which the tabs should appear.
	(tab-bar-tabs nil)
	(cumulative-pixel-width '(0)))
    ;; For each buffer in the filter group, get the corresponding tab and add it to a list (the tab bar).
    (dolist (buffer buffers tab-bar-tabs)
      (when (alist-get buffer (star-tabs-get-filter-prop-value :tabs))
	(setq tab-bar-tabs (append tab-bar-tabs
				   (list (star-tabs-get-tab buffer filter-name collection-name))))
	(let* ((pixel-width (star-tabs-get-tab-prop-value buffer :tab-pixel-width filter-name collection-name)))
	  (setq cumulative-pixel-width (append cumulative-pixel-width
					       (list (+ pixel-width
							(or (car (reverse cumulative-pixel-width))
							    0))))))))
    (star-tabs--set-left-margin)
    (star-tabs-set-filter-prop-value :tab-bar-cumulative-pixel-width cumulative-pixel-width t filter-name collection-name)
    (star-tabs-set-filter-prop-value :tab-bar tab-bar-tabs t filter-name collection-name)
    tab-bar-tabs))

(defun star-tabs--set-tab-bar-format (&optional start-number filter-name collection-name)
  "Set and return the string representation of the tab bar for filter FILTER-NAME of collection COLLECTION-NAME.
Scroll START-NUMBER - 1 (default 0) tabs to the right.
The tab bar format can be accessed using (star-tabs-get-filter-prop-value :tab-bar-format FILTER-NAME COLLECTION-NAME)."
  (or filter-name (setq filter-name (star-tabs-get-active-filter-name)))
  (or collection-name (setq collection-name (star-tabs-active-filter-collection-name)))
  (setq start-number (if start-number
			 (1- start-number)
		       0))
  (let* ((tabs (nthcdr start-number (star-tabs-get-filter-prop-value :tab-bar filter-name collection-name)))
	 (tab-bar-left-margin (star-tabs-get-filter-prop-value :tab-bar-left-margin filter-name collection-name))
	 (tab-bar-format (or tab-bar-left-margin "")))
    (dolist (tab tabs)
      (setq tab-bar-format
	    (concat tab-bar-format
		    (star-tabs-get-tab-prop-value tab :tab-string filter-name collection-name))))
    ;; REVIEW: Add visible tabs prop? (maybe not a good idea since window width can change all the time, meaning we would still need to recalculate.)
    (star-tabs-set-filter-prop-value :tab-bar-format tab-bar-format t filter-name collection-name)
    tab-bar-format))

(defun star-tabs--set-left-margin (&optional filter-name collection-name)
  "Set and return the left margin of the tab bar in filter group FILTER-NAME of collection COLLECTION-NAME.
Properties of the left margin can be accessed using (star-tabs-get-filter-prop-value PROP FILTER-NAME COLLECTION-NAME).
Properties related to the left margin are:
:tab-bar-left-margin - Propertized string representation of the left margin.
:tab-bar-left-margin-width - Width in pixels of the left margin.
:tab-bar-left-margin-column-width - Width in columns of the left margin."
  (let* ((left-margin-fill (propertize star-tabs-left-margin
				      'face 'star-tabs-tab-bar-left-margin))
	(left-margin-collection-name (when star-tabs-tab-bar-collection-name 
				       (propertize
					(let ((collection-name star-tabs-tab-bar-collection-name))
					  (concat (upcase (symbol-name collection-name))
						  star-tabs-filter-name-number-separator))
				       'face 'star-tabs-collection-name)))
	(left-margin-filter-name (when (and (plist-get (star-tabs-active-filter-collection-props) :display-filter-name)
					    star-tabs-tab-bar-filter-name)
				   (propertize 
				    (let ((filter-name star-tabs-tab-bar-filter-name))
				      (concat (upcase (symbol-name filter-name))
					      star-tabs-filter-name-number-separator))
				    'face 'star-tabs-filter-name)))
	(tab-bar-left-margin (concat left-margin-fill
				     (or left-margin-collection-name "")
				     (or left-margin-filter-name "")))
	(tab-bar-left-margin-width (star-tabs-string-pixel-width tab-bar-left-margin)))
    (star-tabs-set-filter-prop-value :tab-bar-left-margin-width tab-bar-left-margin-width t filter-name collection-name)
    (star-tabs-set-filter-prop-value :tab-bar-left-margin-column-width (length tab-bar-left-margin) t filter-name collection-name)
    (star-tabs-set-filter-prop-value :tab-bar-left-margin tab-bar-left-margin t filter-name collection-name)
    tab-bar-left-margin))

(defun star-tabs--update-tabs (buffers)
  "Update tabs BUFFERS in the active filter group."
  ;; TODO: revamp how tabs are updated. 
  (let ((counter 1))
    (dolist (buffer buffers)
      (star-tabs--tab (buffer-name buffer) counter)
      (setq counter (1+ counter)))))


;; (nth 0 (star-tabs-get-filter-prop-value :tab-bar-cumulative-pixel-width))
;; (star-tabs-scroll-to-active (current-buffer))

;; (star-tabs--visible-tabs)
;; (star-tabs--set-tab-bar)
;; (star-tabs--set-tab-bar-format 2)

;; (star-tabs-get-filter-prop-value :tab-bar-cumulative-pixel-width)
;; (insert (star-tabs-get-filter-prop-value :tab-bar-format))
;; (star-tabs-get-filter-prop-value :tab-bar-visible-tabs)
;; (nth 0 (length (star-tabs-get-filter-prop-value :tab-bar)))
;; (nth 0 (length (star-tabs-get-filter-prop-value :tabs)))
;; (star-tabs-get-filter-prop-value :tabs)

;; (message "%s" (star-tabs-get-filter-prop-value :tabs))
;; (nthcdr 1 '(a b c))

;; Scrolling

(defun star-tabs-scroll-tab-bar (&optional backward count)
  "Horizontally scroll the tab bar to the right (left if BACKWARD is non-nil), COUNT (default 2) times."
  ;; FIXME: scrolling before filter name has disappeared will reset scrolling
  ;; TODO: Cache header-line-format (maybe not necessary)
  (or count (setq count 3))
  (let* ((first-tab-number (star-tabs--first-number-in-tab-bar))
	 (count (if backward
		    (- (- first-tab-number 1) count)
		  (+ (- first-tab-number 1) count)))) 
    ;; Only scroll forward (right) if the right of the tab bar is truncated, otherwise there's really no need to scroll forward. 
    (when (star-tabs--string-truncated-p star-tabs-header-line-format)
      ;; Make sure we don't scroll past the last buffer.
      (setq count (min
		   (1- (length (star-tabs-get-active-group-buffers)))
		   count))
      (length (star-tabs-get-active-group-buffers))
      (star-tabs--set-header-line (star-tabs-get-active-group-buffers) count))
    ;; When going backward:
    (when (and (>= first-tab-number 2)
	       backward)
      (star-tabs--set-header-line (star-tabs-get-active-group-buffers) count))))

(defun star-tabs-scroll-tab-bar-forward (&optional count)
  "Scroll tab bar forward prefix argument COUNT (default 2) number of tabs.
Switch to the next filter group if we're already scrolled all the way to the right."
  ;; REVIEW: Require two consecutive scrolls before switching filters?
  ;; TODO: Fix docstring wording
  (interactive "P") 
  (or count (setq count 2))
  (if (and (not (star-tabs--string-truncated-p star-tabs-header-line-format))
	   (not (star-tabs-get-filter-collection-prop-value :disable-scroll-to-filter)))
      ;; Move to next filter group if scrolled all the way to the right in the current group.
      (progn (star-tabs-cycle-filters)
	     (star-tabs-scroll-tab-bar nil 0))
    (star-tabs-scroll-tab-bar nil count)))

(defun star-tabs-scroll-tab-bar-backward (&optional count)
  "Scroll tab bar backward prefix argument COUNT (default 2) number of tabs.
Switch to the previous filter group if we're already scrolled all the way to the left."
  ;; REVIEW: Require two consecutive scrolls before switching filters?
  ;; TODO: Fix docstring wording
  (interactive "P") 
  (or count (setq count 2))
  (if (and (equal (star-tabs--first-number-in-tab-bar) 1)
	   (not (star-tabs-get-filter-collection-prop-value :disable-scroll-to-filter)))
      ;; Move to the end of the previous filter group if scrolled all the way to the left in the current group.
      (progn (star-tabs-cycle-filters t)
	     (star-tabs-scroll-tab-bar nil 1000000))
    (star-tabs-scroll-tab-bar t count)))

(defun star-tabs-scroll-to-buffer (&optional buffer filter-name collection-name)
  "Return the number of tabs required to scroll to tab BUFFER in filter group FILTER-NAME in collection COLLECTION-NAME."
  ;; TODO: change function name.
  ;; TODO: change return value from list to int.
  (or filter-name (setq filter-name (star-tabs-get-active-filter-name)))
  (or collection-name (setq collection-name (star-tabs-active-filter-collection-name)))
  (or buffer (setq buffer (star-tabs-current-buffer)))
  (let* ((tab-number (star-tabs-get-buffer-tab-number buffer))
	 (tabs-pixel-width (star-tabs-get-filter-prop-value :tab-bar-cumulative-pixel-width filter-name collection-name))
	 (current-tab-accumulative-pixel-width (nth tab-number tabs-pixel-width))
	 (tab-bar-left-margin-width (star-tabs-get-filter-prop-value :tab-bar-left-margin-width filter-name collection-name))
	 (window-width (window-pixel-width))
	 (start-tab 1))
    ;; Using the pixel width of each tab, calculate how many tabs we can display while also displaying the tab corresponding to BUFFER.
    (while (and (< start-tab tab-number)
		(< window-width
		    (- (+ current-tab-accumulative-pixel-width
		        tab-bar-left-margin-width)
			  (nth (1- start-tab) tabs-pixel-width))))
      (setq start-tab (1+ start-tab)))
    `(,start-tab ,tab-number)))

(defun star-tabs-scroll-to-active-buffer ()
  "Scroll to the tab of the active buffer, if it exists in in the tab bar."
  (interactive)
  (let ((count (car (star-tabs-scroll-to-buffer (star-tabs-current-buffer)))))
    (star-tabs--set-header-line (star-tabs-get-active-group-buffers) count)))


;; Reordering 

(defun star-tabs-move-tab (&optional backward inhibit-refresh)
  "Move the active tab one step to the right (or left, if BACKWARD is non-nil).
If INHIBIT-REFRESH is non-nil, don't force a redisplay of the tab bar."
  (when (> (length (star-tabs-get-active-group-buffers)) 1) ; No need to move the tab if there is just 1 or 0 tabs in the tab bar.
    (let* ((active-tab-buffer (star-tabs-current-buffer))
	   (buffers (star-tabs-get-active-group-buffers))
	   (adjacent-tab-buffer (star-tabs-left-of-elt
				 (if backward
				     buffers
				   (reverse buffers))
				 active-tab-buffer))
	   (adjacent-tab-buffer-pos (cl-position adjacent-tab-buffer buffers)))
      (when adjacent-tab-buffer
	(star-tabs-set-filter-prop-value :buffer-list
					 (star-tabs-insert-at-nth (remove active-tab-buffer buffers)
								  active-tab-buffer
								  adjacent-tab-buffer-pos)
					 t))))
  (unless inhibit-refresh
    (run-hooks 'star-tabs-move-tab-hook)))

(defun star-tabs-move-tab-right ()
  "Move the active tab one step to the right in the tab bar.
This only works if the active buffer is part of the active filter group."
  (interactive)
  (star-tabs-move-tab))

(defun star-tabs-move-tab-left ()
  "Move the active tab to the left in the tab bar.
This only works if the active buffer is part of the active filter group."
  (interactive)
  (star-tabs-move-tab t))

(defun star-tabs-move-current-tab-to-first ()
  "Move the current buffer so that it becomes the first in the tab bar."
  ;; REVIEW: only run hook when tab ACTUALLY moves?
  (interactive)
  (let ((buffer (star-tabs-current-buffer))
	(buffers (star-tabs-get-active-group-buffers)))
    (star-tabs-set-filter-prop-value :buffer-list
				     (star-tabs-insert-at-nth (remove buffer buffers)
							      buffer
							      0)
				     t))
  (run-hooks 'star-tabs-move-tab-hook))


;;; Display

;; Set display

(defun star-tabs--set-header-line (buffer-list &optional scroll)
  "Set the tab bar to list buffers BUFFER-LIST as tabs.
If SCROLL is set to an integer higher than 0, skip that many tabs if TRUNCATEDP is non-nil."
  ;; TODO: Remove buffer-list argument as it's deprecated.
  ;; TODO: Update doc string.
  (if (and (not buffer-list)
	   (not (star-tabs-get-active-group-buffers)))
      ;;If there are no buffers in any group in the current collection, display a message. 
      (setq star-tabs-header-line-format "   No buffers in any group in current collection.")
    ;; Build the tab bar using propertized strings.
    (when (and buffer-list
	       (not (window-dedicated-p (get-buffer-window (current-buffer)))))
      ;; Set Tab Bar
      (star-tabs--set-tab-bar)
      ;; Determine how much to, and if we should scroll.
      (if star-tabs-debug-messages
	  (message "SCROLL: %s" scroll))
      ;; REVIEW: Make sure scroll max (and min?) values are always enforced.
      (or scroll (setq scroll 0))
      (unless (integerp scroll)
	(setq scroll (cond ((equal scroll 'keep-scroll) (star-tabs--first-number-in-tab-bar))
			   ((equal scroll 'scroll-to-current-buffer) (car (star-tabs-scroll-to-buffer))))))
      ;; Set header-line-format
      (setq star-tabs-header-line-format (star-tabs--set-tab-bar-format scroll))
      ;; Fill the right of the tab bar with propertized empty space.
      (setq star-tabs-header-line-format (concat star-tabs-header-line-format (star-tabs--header-line-white-space)))))
  (force-mode-line-update t)
  nil)

(defun star-tabs--header-line-white-space ()
  "Return white space as a string to fill out the unoccupied part, if any, of the tab bar."
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
This function uses global helper variable star-tabs-filter-name-timer to keep track of the timer."
  ;; Force cancel on any other active timers set with this function.
  (or filter-name (setq filter-name (star-tabs-get-active-filter-name)))
  (when star-tabs-filter-name-timer
    (cancel-timer star-tabs-filter-name-timer)
    star-tabs-filter-name-timer nil)
  (setq star-tabs-filter-name-timer (star-tabs-set-temporarily 'star-tabs-tab-bar-filter-name
							filter-name
							"1 sec"
							nil
							#'star-tabs--set-header-line
						        (star-tabs-get-active-group-buffers)	
							'keep-scroll)))

(defun star-tabs--display-collection-name-temporarily (&optional collection-name)
  "Return collection name COLLECTION-NAME for temporary display in tab bar. 
Unless set, COLLECTION-NAME defaults to the active collection name.
This function uses global helper variable star-tabs-collection-name-timer to keep track of the timer."
  ;; Force cancel on any other active timers set with this function.
  (or collection-name (setq collection-name (star-tabs-active-filter-collection-name t)))
  (when star-tabs-collection-name-timer
    (cancel-timer star-tabs-collection-name-timer)
    star-tabs-collection-name-timer nil)
  (setq star-tabs-collection-name-timer (star-tabs-set-temporarily 'star-tabs-tab-bar-collection-name
							collection-name
							"1 sec"
							nil
							#'star-tabs--set-header-line
							(star-tabs-get-active-group-buffers)
							'keep-scroll)))


;; Display helper functions

(defun star-tabs--string-truncated-p (string)
  "Return t if the width of string STRING is greater than the width of the current window.
Otherwise, return the number of truncated pixels."
  (let ((tab-bar-width (star-tabs-string-pixel-width string)) 
	(window-width (window-pixel-width)))
    (if (>
	 tab-bar-width
	 (1+ window-width))
	(- tab-bar-width window-width)
      nil)))

(defun star-tabs--header-line-remaining-space()
  "Return the width in pixels between the end of the last tab and the right edge of the current window."
  (- (window-pixel-width)
     (star-tabs-string-pixel-width star-tabs-header-line-format)))

(defun star-tabs--first-number-in-tab-bar ()
  "Return the tab number of the first visible tab in the tab bar.
Or, return 0 if there are no tabs."
  (if (> (length star-tabs-header-line-format) 0)
      (or (get-text-property (star-tabs-get-filter-prop-value :tab-bar-left-margin-column-width)
			     'buffer-number star-tabs-header-line-format)
	  0)
    0))

(defun star-tabs--current-buffer-number ()
  "Return the tab number of the current buffer.
If the current buffer is not in the active filter group, return 0."
  (star-tabs--get-buffer-number))

(defun star-tabs--get-buffer-number (&optional buffer filter-name collection-name)
  "Return the tab number of buffer BUFFER in filter group FILTER-NAME of collection COLLECTION-NAME.
If the buffer is not in the filter group, return 0."
  (or filter-name (setq filter-name (star-tabs-get-active-filter-name)))
  (or collection-name (setq collection-name (star-tabs-active-filter-collection-name)))
  (or buffer (setq buffer (star-tabs-current-buffer)))
  (1+ (or (cl-position buffer (star-tabs-get-group-buffers filter-name collection-name))
	  -1)))

(defun star-tabs--tab-visible-p (&optional buffer)
  "Return non-nil if the tab corresponding to buffer BUFFER is visible in the tab bar."
  (or buffer (star-tabs-current-buffer))
  (let ((tab-number (star-tabs--current-buffer-number))
	(visible-tab-numbers (star-tabs--visible-tabs)))
    (when (and (>= tab-number (car visible-tab-numbers))
	       (<= tab-number (nth 1 visible-tab-numbers)))
      t)))

(defun star-tabs--visible-tabs ()
  "Return, as a list, the min and max tab number currently displayed in the tab bar.
Exclude the last tab if it's truncated.
If there are no tabs in the tab bar, return (0 0) indicating that there is neither a start nor an end tab."
  (if (star-tabs-get-filter-prop-value :tab-bar)
      (let* ((tabs-pixel-width (star-tabs-get-filter-prop-value :tab-bar-cumulative-pixel-width))
	     (tab-bar-left-margin-width (star-tabs-get-filter-prop-value :tab-bar-left-margin-width))
	     (start-tab (star-tabs--first-number-in-tab-bar))
	     (start-tab-start-pixel (nth (1- start-tab) tabs-pixel-width))
	     (num-tabs (1- (length tabs-pixel-width)))
	     (end-tab start-tab)
	     (end-tab-end-pixel (nth end-tab tabs-pixel-width))
	     (window-width (window-pixel-width)))
	(while (and (<= end-tab num-tabs)
		    (> window-width (- (+ tab-bar-left-margin-width end-tab-end-pixel)
				       start-tab-start-pixel)))

	  (setq end-tab (1+ end-tab))
	  (setq end-tab-end-pixel (nth end-tab tabs-pixel-width)))
	`(,start-tab ,(1- end-tab)))
    '(0 0)))


;;; Functions to run with hooks

;; Functions to run when the buffer list updates, or when switching buffers .

(defun star-tabs-on-raw-buffer-list-update ()
  "Run with buffer-list-update-hook."
  (star-tabs--update-buffer-list)
  (star-tabs--buffer-switched-p)) 

(defun star-tabs-on-buffer-list-update ()
  "Run when the list of real buffers updates."
  ;; TODO: Add parameter for new/killed buffers
  (when star-tabs-debug-messages
    (message "Real buffer list updated"))
  (star-tabs--add-and-remove-file-extension-filters t t)
  (star-tabs--filter-all-buffers) 
  (star-tabs--update-tabs (star-tabs-get-active-group-buffers))
  (star-tabs--set-header-line (star-tabs-get-active-group-buffers) 'keep-scroll))

(defun star-tabs-on-buffer-switch ()
  "Run when the current real buffer is switched."
  (when star-tabs-debug-messages
    (message "Buffer Switched: %s" (buffer-name (current-buffer))))
  ;; Find a filter for the new buffer.
  (when (star-tabs-find-active-filter t)
    (star-tabs-on-filter-change t))
  ;; Auto Sort.
  (when (equal (star-tabs-get-filter-prop-value :auto-sort) 'recent-first)
    (star-tabs-auto-sort))
  ;; Update and display tab bar.
  (star-tabs--update-tabs (star-tabs-get-active-group-buffers))
  (if (star-tabs--tab-visible-p (star-tabs-current-buffer))
      (star-tabs--set-header-line (star-tabs-get-active-group-buffers) 'keep-scroll)
    (star-tabs--set-header-line (star-tabs-get-active-group-buffers) 'scroll-to-current-buffer)))
;; TODO: scroll-to-current-buffer should be 'keep-scroll if the tab is already visible.


;; Functions to run when modified state changes.

(defun star-tabs-when-buffer-first-modified ()
  "Run when a buffer goes from an unmodified state to a modified state."
  (when (member (current-buffer) star-tabs-active-buffers)
    (set-buffer-modified-p t) ; HACK: Make sure that buffer-modified-p is set to t even though it should automatically be set to t.
    (when star-tabs-debug-messages
      (message "Buffer Modified"))
    (star-tabs--update-tabs (star-tabs-get-active-group-buffers))
    (star-tabs--set-header-line (star-tabs-get-active-group-buffers) 'keep-scroll)))


(defun star-tabs-when-buffer-first-saved ()
  "Run when a buffer goes from a modified state to an unmodified state."
  (when star-tabs-debug-messages
    (message "Buffer Saved"))
  (when (member (current-buffer) star-tabs-active-buffers)
    (set-buffer-modified-p nil) ; HACK: Make sure that buffer-modified-p is set to nil even though it should automatically be set to nil.
    (star-tabs--update-tabs (star-tabs-get-active-group-buffers))
    (star-tabs--set-header-line (star-tabs-get-active-group-buffers) 'keep-scroll)))


;; Functions to run when the active filter group or collection changes.

(defun star-tabs-on-filter-change (&optional inhibit-refresh)
  "Run when the active filter group changes."
  ;; Review: Probably not triggered when changing collections (which subsequently will change the active filter)
  (when star-tabs-debug-messages
    (message "Filter Changed"))
  (star-tabs--display-filter-name-temporarily)
  (star-tabs--update-tabs (star-tabs-get-active-group-buffers))
  (unless inhibit-refresh
    (star-tabs--set-header-line (star-tabs-get-active-group-buffers) 'scroll-to-current-buffer)))

(defun star-tabs-on-collection-change ()
  "Run when the active filter collection changes."
  ;; TODO: (star-tabs--display-collection-name-temporarily) (and filter-name)
  ;; REVIEW: Make sure this works!
  (when star-tabs-debug-messages
    (message "Collection Changed"))
  (star-tabs--add-and-remove-file-extension-filters t t)
  (star-tabs--filter-all-buffers)
  (star-tabs--update-tabs (star-tabs-get-active-group-buffers))
  (star-tabs--set-header-line (star-tabs-get-active-group-buffers) 'scroll-to-current-buffer))


;; Functions to run when collection properties change.

(defun star-tabs--add-and-remove-file-extension-filters (&optional inhibit-hook inhibit-refresh)
  "Add or remove file extension filters, based on the collection settings.\n
File extension filters will be added on one of two conditions:
1. The active filter collection has the property :enable-file-extension-filters set to non-nil
2. The active filter collection has the property :enable-file-extension-filters set to nil,
and :file-extension-filter-threshold set above 0, and the total number of buffers (after applying global filters) exceeds that number."
  (setq star-tabs-add-file-extension-filters
	(or (plist-get (star-tabs-active-filter-collection-props) :enable-file-extension-filters) nil))
  ;; Activate file extension filters if the buffer count exceeds the threshold (if set).
  (when (and (not (plist-get (star-tabs-active-filter-collection-props) :enable-file-extension-filters))
	     (not (<= star-tabs-file-ext-filter-buffer-threshold 0)))
    (star-tabs--auto-activate-file-extension-filters-on-buffer-count (star-tabs-get-filter-collection-prop-value
								      :file-extension-filter-threshold)))
  ;; Add and remove file extension filters in the current collection, based on the file extension of currently open buffers.
  (let ((extensions-updated-p nil))
    (if star-tabs-add-file-extension-filters
	(setq extensions-updated-p (or (star-tabs--update-file-extension-filters inhibit-hook)
				       extensions-updated-p))
      ;; Remove all automatically set file extension filters in case none of the two conditions 
      ;; above are met.
      (when star-tabs-file-extension-filter-names
	(setq extensions-updated-p (or (star-tabs--remove-file-extension-filters) extensions-updated-p))))
    (when extensions-updated-p
      (when star-tabs-debug-messages
	(message "File Extension List/Filters Updated"))
      (unless inhibit-hook
	(run-hooks 'star-tabs-collection-property-change-hook))
      (unless inhibit-refresh
	(star-tabs--set-header-line (star-tabs-get-active-group-buffers) 'keep-scroll)))
    extensions-updated-p))

(defun star-tabs-on-collection-property-change ()
  "Run when a collection property changes."
  (when star-tabs-debug-messages
    (message "Collection Property Changed"))
  (star-tabs--add-and-remove-file-extension-filters t t) ; File extension filter groups will only be added if set to do so.
  (star-tabs--filter-all-buffers)
  (star-tabs--update-tabs (star-tabs-get-active-group-buffers))
  (star-tabs--set-header-line (star-tabs-get-active-group-buffers) 'keep-scroll))


;; Functions to run when enabling/disabling Star Tabs.

(defun star-tabs-init ()
  "Run when Star Tabs first loads"
  (when star-tabs-debug-messages
    (message "Star Tabs initializing...")))

(defun star-tabs-on-disable-tab-bar ()
  "Run when Star Tabs Mode goes from enabled to disabled."
  (when star-tabs-debug-messages
    (message "Star Tabs disabled")))


;; Misc. functions to run with hooks.

(defun star-tabs-on-tab-move ()
  "Run when a tab changes position in the tab bar."
  (when star-tabs-debug-messages
    (message "Tab moved"))
  (star-tabs--update-tabs (star-tabs-get-active-group-buffers))
  (star-tabs--set-header-line (star-tabs-get-active-group-buffers) 'scroll-to-current-buffer))

(defun star-tabs-on-timer-start ()
  "Run when a Star Tabs timer starts."
  (when star-tabs-debug-messages
    (message "Star Tabs Timer Started")))

(defun star-tabs-on-timer-end ()
  "Run when a Star Tabs timer ends."
  (when star-tabs-debug-messages
    (message "Star Tabs Timer Started")))


;;; Modes

(define-minor-mode star-tabs-tab-bar-mode
  "...desc..."
  ;;:lighter " ST"
  :global t

  (if star-tabs-tab-bar-mode
      (progn (star-tabs-init-filters)
	     ;; Refresh the tab bar when buffers are created or killed.
	     (add-hook 'buffer-list-update-hook #'star-tabs-on-raw-buffer-list-update nil nil)
	     ;; Functions to run when a buffer goes from an unmodified to a modified state.
	     (add-hook 'first-change-hook #'star-tabs-when-buffer-first-modified nil nil)
	     ;; Update the tab bar when a buffer is saved.
	     (add-hook 'after-save-hook #'star-tabs-when-buffer-first-saved nil nil)
	     ;; Make sure that emacs finds a filter group with tabs (if there is one) when activating star-tabs-tab-bar-mode.
	     (unless (star-tabs-get-active-group-buffers)
	       (star-tabs-cycle-filters)))))


;; Hooks
(add-hook 'star-tabs-move-tab-hook #'star-tabs-on-tab-move)
(add-hook 'star-tabs-collection-property-change-hook #'star-tabs-on-collection-property-change)
;; (add-hook 'star-tabs-disable-tab-bar-hook #'star-tabs-on-disable-tab-bar)
;; (add-hook 'star-tabs-init-hook #'star-tabs-init)
(add-hook 'star-tabs-buffer-list-update-hook #'star-tabs-on-buffer-list-update)
;; (add-hook 'star-tabs-timer-end-hook #'star-tabs-on-timer-end)
;; (add-hook 'star-tabs-timer-start-hook #'star-tabs-on-timer-start)
(add-hook 'star-tabs-collection-change-hook #'star-tabs-on-collection-change)
(add-hook 'star-tabs-filter-change-hook #'star-tabs-on-filter-change) 
(add-hook 'star-tabs-buffer-switch-hook #'star-tabs-on-buffer-switch)

;;; DEPRECATED

(defun star-tabs--tab-bar-left-margin-width ()
  "Return the column width of all characters left of the beginning of the first tab.
If there are no tabs, return 0."
  (let ((tab-bar-left-margin-width 0)
	(tab-bar-length (length star-tabs-header-line-format)))
    (while (and (< tab-bar-left-margin-width tab-bar-length)
		(eq 0 (or (get-text-property tab-bar-left-margin-width 'buffer-number star-tabs-header-line-format)
			  0)))
      (setq tab-bar-left-margin-width (1+ tab-bar-left-margin-width)))
    (if (>= tab-bar-left-margin-width tab-bar-length)
	0
      tab-bar-left-margin-width)))

(provide 'star-tabs)

;;; star-tabs.el ends here


