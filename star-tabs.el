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

;; TODO: Add variables for each divider and use them.
(defvar star-tabs-left-margin "   " 
  "Space used to the left of the tab bar.")

(defvar star-tabs-right-margin ""
  "Space used to the right of the tab bar.
This variable is currently not used.")

(defvar star-tabs-tab-separator " "
  "Tab bar divider that separates tabs.")

(defvar star-tabs-number-name-separator " "
  "Tab bar divider that separates the tab number and tab name in a tab.")

(defvar star-tabs-name-modified-icon-separator " "
  "Tab bar divider that separates the buffer name and modified icon in a tab.")

(defvar star-tabs-modified-icon-close-button-separator " " 
  "Tab bar divider that separates the modified icon and close button in a tab.")

(defvar star-tabs-filter-name-number-separator "   "
  "Tab bar divider that separates the name of the active filter group and the first tab.")


;; Tab icons

(defvar star-tabs-modified-buffer-icon "\u229a"
  "Tab 'icon' for modified buffers.")

(defvar star-tabs-unmodified-buffer-icon "+"
  "Tab 'icon' for unmodified buffers.")

(defvar star-tabs-close-buffer-icon "\u2a2f"
  "Tab 'icon' for the tab close button.")


;; Hooks

(defvar star-tabs-buffer-switch-hook nil
  "Functions to run when the current (real) buffer switches.")

(defvar star-tabs-buffer-list-update-hook nil
  "Functions to run when the Star Tabs-curated list of real buffers is updated.")

(defvar star-tabs-filter-switch-hook nil
  "Functions to run when the active filter group switches.")

(defvar star-tabs-collection-switch-hook nil
  "Functions to run when the active collection switches.")

(defvar star-tabs-timer-start-hook nil
  "Functions to run when a Star Tabs timer starts.")

(defvar star-tabs-timer-end-hook nil
  "Functions to run when a Star Tabs timer ends.")

(defvar star-tabs-init-hook nil
  "Functions to run when Star Tabs first loads.")

;; TODO: add enable-tab-bar-hook as well?

(defvar star-tabs-disable-tab-bar-hook nil
  "Functions to run when Star Tabs becomes disabled.")

(defvar star-tabs-move-tab-hook nil
  "Functions to run when a tab changes position in the tab bar.")

(defvar star-tabs-collection-property-change-hook nil
  "Functions to run when the property of a collection changes.
Note that only Star Tabs functions used to set or change collection properties will trigger this hook.")


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
  "Helper variable for function `star-tabs--buffer-switched-p' to keep track of the current real buffer.")

(defvar star-tabs-filter-name-timer nil
  "The last used timer set automatically by `star-tabs--display-filter-name-temporarily'.")

(defvar star-tabs-collection-name-timer nil
  "The last used timer set automatically by `star-tabs--display-collection-name-temporarily'.")

(defvar star-tabs-debug-messages t
  "If set to non-nil, debug messages will be displayed.")

(defvar star-tabs-tab-bar-filter-name nil
  "Filter name to be displayed in the tab bar; automatically set by other functions.")

(defvar star-tabs-tab-bar-collection-name nil
  "Collection name to be displayed in the tab bar; automatically set by other functions.")


;; Collections

(defvar star-tabs-collections nil
  "List of all collections. car of the list represents the active collection in the tab bar.")


;; Filters

(defvar star-tabs-global-inclusion-prefix-filter nil
  "List of buffer name prefixes to be included globally. Buffers filtered this way will be cached and ignored
for all future searches. As such, global filtering may increase performance, and
should (and should only!) be applied to buffers that you really don't care about.

Buffers with the space prefix (\" \") are automatically filtered before this filter is applied, and thus cannot  
be included.

This filter is applied before star-tabs-global-exclusion-prefix-filter."
  ;; REVIEW: DEPRECATED: Not really needed maybe?
  )

(defvar star-tabs-global-exclusion-prefix-filter '("*"
						   "magit-"
						   "magit:")
  "List of buffer name prefixes to be excluded globally. Buffers filtered this way will be cached and ignored
for all future buffer list updates. As such, global filtering may increase performance, and
should (and should only!) be applied to buffers that you really don't care about.

Buffers with the space prefix (\" \") are automatically filtered before this filter is applied, and thus need not
be added to this list.

REVIEW: DEPRECATED: This filter is applied after star-tabs-global-inclusion-prefix-filter, which will probably be removed soon?")

(defvar star-tabs-cached-filtered-buffers (make-hash-table :test #'equal)
  "Cache globally filtered buffers to improve performance.")


;; File extension filters

(defvar star-tabs-add-file-extension-filters nil
  "If non-nil, file extension filters will be added to the collection. This variable is set and
controlled by functions, and depends on collection-specific configuration."
  ;; TODO: Add as a collection property instead.
  )

(defvar star-tabs-file-extension-filter-names nil
  "Automatically added file-extension filter group names. 
This is a helper variable for the automatic file extension filter groups."
  ;; TODO: Add as a collection property instead
  )


;; Buffers

;; TODO: Rename star-tabs-active-buffers -> ??
(defvar star-tabs-active-buffers nil
  "List of all real buffers.")


;;; Visuals

;; TODO: Make these defcustom

(defvar star-tabs-tab-bar-height 210
  "Height of the tab bar.")

(defvar star-tabs-tab-bar-text-height 155 
  "Text height for tabs.")

(defvar star-tabs-tab-bar-filter-name-foreground "#ef21b3"
  "Foreground color for the tab bar filter name.")

(defvar star-tabs-tab-bar-collection-name-foreground "#7cd164"
  "Foreground color for the tab bar collection name.")

(defvar star-tabs-tab-bar-selected-background "#202020"
  "Background color for the selected tab.")

(defvar star-tabs-tab-bar-selected-foreground "#a3c9e7"
  "Foreground color for the selected tab.")

(defvar star-tabs-tab-bar-non-selected-background "#464646"
  "Background color for non-selected tabs.")

;; (defvar star-tabs-tab-bar-non-selected-background "#FF7F00"
;;   "Background color for non-selected tabs.")

(defvar star-tabs-tab-bar-non-selected-foreground "#b1b1b1"
  "Foreground color for non-selected tabs.")

(defvar star-tabs-modified-icon-non-selected-foreground "#2614e1"
  "Foreground color for non-selected tabs.")

(defvar star-tabs-modified-icon-selected-foreground "#e12614"
  "Foreground color for the selected tab.")

(defvar star-tabs-tab-bar-background "#363636"
  "Background color for the tab bar.")
;; Faces

(defface star-tabs-tab-bar-left-margin
  `(( t
      (
 ;      :height ,star-tabs-tab-bar-height
       :background ,star-tabs-tab-bar-background)))
  "Face for the left margin of the header-line which is used for determining the height of the header-line.")

(defface star-tabs-tab-bar-empty-space
  `(( t
      (
 ;      :height ,star-tabs-tab-bar-height
       :background ,star-tabs-tab-bar-background)))
  "Face for the unused space of the right of the header-line.")

(defface star-tabs-filter-name
  `((t
     (
      :background ,star-tabs-tab-bar-background
      :foreground ,star-tabs-tab-bar-filter-name-foreground
      :height ,star-tabs-tab-bar-text-height)))
  "Face for displaying the filter name in the tab bar.")

(defface star-tabs-collection-name
  `((t
     (
      :background ,star-tabs-tab-bar-background
      :foreground ,star-tabs-tab-bar-collection-name-foreground
      :height ,star-tabs-tab-bar-text-height)))
  "Face for displaying the collection name in the tab bar.")

(defface star-tabs-tab-divider-mouse-selected
  `((t
     (
      :background ,star-tabs-tab-bar-selected-background
      :foreground ,star-tabs-tab-bar-filter-name-foreground
      :height ,star-tabs-tab-bar-text-height)))
  "Face to prevent tabs from 'melting' together when hovering over a selected tab with the mouse in the tab bar.")

(defface star-tabs-tab-divider-mouse-non-selected
  `((t
     (
      :background ,star-tabs-tab-bar-non-selected-background
      :foreground ,star-tabs-tab-bar-filter-name-foreground
      :height ,star-tabs-tab-bar-text-height)))
  "Face to prevent tabs from 'melting' together when hovering over a non-selected tab with the mouse in the tab bar.")

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
  "Face to be displayed when hovering over a non-selected tab with the mouse in the tab bar.")

(defface star-tabs-non-selected-modified-icon
  `((t (
	:background ,star-tabs-tab-bar-non-selected-background
	:foreground ,star-tabs-modified-icon-non-selected-foreground
	:height ,star-tabs-tab-bar-text-height)))
  "Face for displaying the non-selected tab in the tab bar.")

(defface star-tabs-selected-modified-icon
  `((t
     (
      :background ,star-tabs-tab-bar-selected-background
      :foreground ,star-tabs-modified-icon-selected-foreground
      :height ,star-tabs-tab-bar-text-height)))
  "Face for displaying the selected tab in the tab bar.")

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
  "Cycle (move forward, or backward if REVERSE is non-nil) through list LIST. 
Return a copy of the modified list."
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
  "Insert element ELT in list LIST at position N. Return a copy of the modified list."
  (let ((nth-from-end (- (length list) n)))
    (append (reverse (nthcdr nth-from-end (reverse list))) (list elt) (nthcdr n list))))

(defun star-tabs-set-temporarily (symbol value duration &optional value-after func-after &rest args)
  "Set symbol SYMBOL to value VALUE for DURATION. After DURATION, set SYMBOL to VALUE-AFTER (default nil). 
Optionally run function FUNCTION with arguments ARGS after DURATION. Return timer.
Also run hook star-tabs-timer-start-hook before the timer starts, and
star-tabs-timer-end-hook when the timer ends."
  ;; TODO: Add inhibit-hook parameter.
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


;;; Collections

(defun star-tabs-create-collection (&rest collection-props)
  "Create a collection. Available properties are:
:name - the name of the collection.
:use - if non-nil, switch to collection directly after creation (default nil).
:hide-close-buttons - if non-nil, hide tab close buttons (default nil).
:display-filter-name - if non-nil, display the name of the active filter in the tab bar (default nil).
:enable-file-extension-filters - if non-nil, enable filter groups in the collection for all file extensions 
among currently open buffers (default nil).
:collection-name-prefix - the prefix of the name of the collection, used in variable names. Individual collections can be 
identified by the symbol name (intern (concat collection-name-prefix name)). (default \"star-tabs-collection-\").
:hide-extension-names - If non-nil, file extension names in buffer names will be hidden in tabs. Note that this won't change the name
of the buffers; just how the names are displayed in the tabs. "
  ;; TODO: Update documentation for :disable-scroll-to-filter
  ;; TODO: Update documentation for :border-style
  (let* ((use (plist-get collection-props :use))
	 (enable-file-extension-filters (plist-get collection-props :enable-file-extension-filters))
	 (hide-close-buttons (plist-get collection-props :hide-close-buttons))
	 (display-filter-name (plist-get collection-props :display-filter-name))
	 (file-extension-filter-threshold (or (plist-get collection-props :file-extension-filter-threshold) 0))
	 (disable-scroll-to-filter (plist-get collection-props :disable-scroll-to-filter))
	 (hide-extension-names (plist-get collection-props :hide-extension-names))
	 (collection-name-prefix (or (plist-get collection-props :collection-name-prefix) "star-tabs-collection-"))
	 (border-style (or (plist-get collection-props :border-style) 'rounded))
	 (name-no-prefix (plist-get collection-props :name))
	 (name (intern (concat collection-name-prefix (plist-get collection-props :name))))
	 (collection `(,name :enable-file-extension-filters ,enable-file-extension-filters
			     :display-filter-name ,display-filter-name
			     :file-extension-filter-threshold ,file-extension-filter-threshold
			     :disable-scroll-to-filter ,disable-scroll-to-filter
			     :hide-extension-names ,hide-extension-names
			     :collection-name-prefix ,collection-name-prefix
			     :collection-name-no-prefix ,name-no-prefix
			     :border-style ,border-style
			     :last-filter nil)))
    (if (not (member name (star-tabs-collection-names)))
	(progn (set name nil) 
	       (setq star-tabs-collections
		     (append star-tabs-collections (list collection)))
	       ;; Switch to the new collection upon creation if :use is non-nil.
	       (when use
		 (while (not (eq (star-tabs-active-collection-name) name))
		   (star-tabs-cycle-collections t t))
		 (run-hooks 'star-tabs-collection-switch-hook)))
      (message "STAR-TABS: Collection name already exists"))))

(defun star-tabs-collection-names ()
  "Return a list of names of all collections."
  (mapcar 'car star-tabs-collections))

;; FIXME currently creating collection while the list is rotated causes the collections to be in the wrong order
;; Solve this by rotating the list back to the beginning (store first added list, move to second if the first is deleted etc)
;; Or just keeping track of last added..?
;; Or just ignore ??

(defun star-tabs-remove-collection (collection-name)
  "Delete collection COLLECTION-NAME."
  (if (>= (length star-tabs-collections) 2)
      (let ((prefix (star-tabs-get-collection-prop-value :collection-name-prefix collection-name)))
	;; makunbound will cause problems if we're removing the active collection, so first make another collection active.
	;; BEWARE: If for some reason in the future, star-tabs-cycle-collections has the ability to skip collections,
	;; we might inadvertently, despite cycling, end up deleting COLLECTION-NAME when it's active. 
	(when (eq collection-name (star-tabs-active-collection-name))
	  (star-tabs-cycle-collections))
	(setq star-tabs-collections (remove
				     (nth (cl-position collection-name (star-tabs-collection-names)) star-tabs-collections)
				     star-tabs-collections))
	(makunbound collection-name))
  (message "STAR-TABS: Cannot delete last collection. Make another collection before attempting to delete this one.")))

(defun star-tabs-cycle-collections (&optional reverse inhibit-hook)
  "Cycle (move forward, or backward if REVERSE is non-nil) through collections.
Also run hook star-tabs-collection-switch-hook if INHIBIT-HOOK is nil (default)."
  (interactive)
  (setq star-tabs-collections (star-tabs-cycle-list-car star-tabs-collections reverse))
  (unless inhibit-hook
    (run-hooks 'star-tabs-collection-switch-hook)))

(defun star-tabs-active-collection-name (&optional no-prefix)
  "Return the name of the active collection."
  (if no-prefix
      (intern (star-tabs-get-collection-prop-value :collection-name-no-prefix))
    (car (car star-tabs-collections))))

(defun star-tabs-get-collection-prop-value (prop &optional collection-name)
"Return the value of property PROP in collection COLLECTION-NAME. 
COLLECTION-NAME defaults to the active collection."
  (or collection-name (setq collection-name (star-tabs-active-collection-name)))
  (plist-get (star-tabs-collection-props collection-name) prop))

(defun star-tabs-set-collection-prop-value (prop value &optional inhibit-hook collection-name)
  "Set property PROP in collection COLLECTION-NAME to value VALUE. 
Also run hook star-tabs-collection-property-change-hook if inhibit-hook is nil (default). 
COLLECTION-NAME defaults to the active collection."
  (or collection-name (setq collection-name (star-tabs-active-collection-name)))
  (plist-put (star-tabs-collection-props collection-name) prop value)
  (unless inhibit-hook
    (run-hook-with-args 'star-tabs-collection-property-change-hook collection-name)))

(defun star-tabs-collection-props (&optional collection-name)
  "Return the properties of collection COLLECTION-NAME.
COLLECTION-NAME defaults to the active collection."
  (or collection-name (setq collection-name (star-tabs-active-collection-name)))
  (alist-get collection-name star-tabs-collections))

(defun star-tabs-active-collection-props ()
  "Return the properties of the active collection."
    (star-tabs-collection-props (star-tabs-active-collection-name)))


;;; Filters

;; Add and remove filters

(defun star-tabs-add-filter (&rest filter-props)
  "Add a regular expression-based filter group to include and/or exclude buffers from being displayed.
The filter group can have the following properties set manually: \n
-:name is the name of the filter. 
-:include and :exclude are lists of regular expressions.
-Either :include or :exclude or both must be set. 
-If both :include and :exclude are set, buffers matching the regexp set in :exclude 
will be excluded from those matching the regexp in :include.
-if only :include is set, only the buffers matching the regexp in :include will be displayed.
-if only :exclude is set, all buffers except the ones matching the regexp in :exclude will be displayed.
-if :auto-sort is set to 'recent-first (default nil), tabs in the filter will be arranged in the order in which they became current/active.
-if :inhibit-refresh is nil (default), run hook star-tabs-collection-property-change-hook
-The filter will be added to collection :collection-name, which defaults to the active collection."
  ;; TODO add auto-sort expl. to readme
  ;; TODO add always inlclude prop?
  ;; TODO add doc for only-modified-buffers prop
  ;; TODO add doc for use prop
  (let* ((name (plist-get filter-props :name))
	 (exclude (plist-get filter-props :exclude))
	 (include (plist-get filter-props :include))
	 (collection-name (or (plist-get filter-props :collection) (star-tabs-active-collection-name)))
	 (auto-sort (or (plist-get filter-props :auto-sort) nil))
	 (only-modified-buffers (or (plist-get filter-props :only-modified-buffers) nil))
	 (inhibit-refresh (or (plist-get filter-props :inhibit-refresh)))
	 (use (or (plist-get filter-props :use) nil))
	 (filter `(,name :exclude ,exclude
			 :include ,include
			 :only-modified-buffers ,only-modified-buffers
			 :auto-sort ,auto-sort))
	 last-filter-pos)
    (if (not (member name (star-tabs-get-filter-names)))
	;; Add the filter group to the "right" of the last added filter group, in order to maintain order.
	(progn (setq last-filter-pos
		     (cl-position (star-tabs-get-collection-prop-value
				   :last-filter (star-tabs-active-collection-name))
				  (star-tabs-get-filter-names)))
	       ;; Make the filter group the last added.
	       (if last-filter-pos
		   (set collection-name (star-tabs-insert-at-nth (eval collection-name) filter (1+ last-filter-pos)))
		 (set collection-name (append (eval collection-name) (list filter))))
	       (star-tabs-set-collection-prop-value :last-filter name t collection-name))
      (message "STAR-TABS: Filter name already exists"))
    (when use
      (star-tabs--filter-all-buffers)
      (star-tabs-switch-to-filter name nil collection-name))
    (unless inhibit-refresh
      (run-hook-with-args 'star-tabs-collection-property-change-hook collection-name))))

(defun star-tabs-remove-filter (filter-name &optional inhibit-refresh collection-name)
  "Remove filter group FILTER-NAME from collection COLLECTION-NAME.
Also run hook star-tabs-collection-property-change-hook unless INHIBIT-REFRESH is non-nil.
COLLECTION-NAME defaults to the active collection."
  (setq collection-name (or collection-name (star-tabs-active-collection-name)))
  ;; When removing the last added filter, set the :last-filter property of the collection to the next-to-last
  ;; filter instead. This is to maintain the order in which the filter groups were added.
  (when (eq (star-tabs-get-collection-prop-value :last-filter collection-name)
	    filter-name)
    (star-tabs-set-collection-prop-value :last-filter (star-tabs-left-of-elt
							      (star-tabs-get-filter-names collection-name)

							      filter-name)
						t
						collection-name))
  (set collection-name (assq-delete-all filter-name (eval collection-name)))
  (unless inhibit-refresh
    (run-hook-with-args 'star-tabs-collection-property-change-hook collection-name)))

(defun star-tabs-remove-all-filters (&optional inhibit-refresh collection-name)
  "Delete all filters in collection COLLECTION-NAME.
COLLECTION-NAME defaults to the active collection.
Note that file extensions will be readded if activated."
  (setq collection-name (or collection-name (star-tabs-active-collection-name)))
  (let ((filters (star-tabs-get-filter-names)))
    (dolist (filter filters)
      (star-tabs-remove-filter filter inhibit-refresh collection-name))
    (setq star-tabs-file-extension-filter-names nil)))

(defun star-tabs-init-filters ()
  "Initialize default collection and filter groups."
  ;; TODO: DEPRECATED: Modify body of function and run with hook instead.
  (star-tabs-create-collection
   :name "default-collection"
   :use t
   :enable-file-extension-filters t
   :hide-extension-names t
   :display-filter-name t)
  (star-tabs-add-filter
   :name 'default
   :exclude '("^[[:space:]]" "^*.*\\*$" "^magit-" "^magit:")
   :inhibit-refresh t)
  ;; Optionally add file extension filters.
  (when (star-tabs-get-collection-prop-value  :enable-file-extension-filters)
      (star-tabs--add-and-remove-file-extension-filters t)))


;; filter Interactions 

(defun star-tabs-cycle-filters (&optional reverse inhibit-refresh include-empty)
  "Cycle (move forward, or backward if REVERSE is non-nil) through filter groups in the active collection. 
Ignore empty filter groups, unless include-empty is set to non-nil.
Also run hook star-tabs-filter-switch-hook if INHIBIT-REFRESH is nil."
  (interactive)
  (or reverse (setq reverse nil))
  (or include-empty (setq include-empty nil))
  ;; Move (cycle) forward once, or backward if REVERSE is non-nil.
  (set (star-tabs-active-collection-name) (star-tabs-cycle-list-car
					     (eval (star-tabs-active-collection-name))
					     reverse))
  (let ((filter-count (length (eval (star-tabs-active-collection-name)))))
    ;; Go through the list of filter groups in the active collection once, or until a non-empty filter group is found,
    ;; skipping to the next filter group if when the filter group is empty.
    (while (and (and (not include-empty)
                     (not (star-tabs-get-active-group-buffers)))
		(>= filter-count 0))
      ;; Cycle by rotating the list of filters. The active filter is the car of the list.
      (set (star-tabs-active-collection-name)
	   (star-tabs-cycle-list-car (eval (star-tabs-active-collection-name))
				     reverse))
      (setq filter-count (1- filter-count)))) ;Prevent infinite loop in case all groups are empty
  (unless inhibit-refresh
    (run-hooks 'star-tabs-filter-switch-hook)))

(defun star-tabs-find-active-filter (&optional inhibit-refresh) 
  "Find a filter group for the current buffer.
If no filter group exists in the current collection, do nothing.
If the current buffer is in the active filter, do nothing.
If INHIBIT-REFRESH is nil (default), refresh the tab bar as well.
Return the name of the new filter if the filter switches."
  (interactive)
  (let ((current-buffer (star-tabs-current-buffer))
	(filter-count (length (eval (star-tabs-active-collection-name)))) ; TODO: Create function to retrieve number of filters instead. 
	(prev-filter-name (star-tabs-get-active-filter-name)))
    ;; Loop through the list of registered filters once, or until a filter is found.
    ;; But there is no need to change filter group if the current buffer is already in the active filter group.
    (while (and (not (member current-buffer
			     (star-tabs-get-active-group-buffers)))
		(>= filter-count 0))
      (star-tabs-cycle-filters nil t)  ; If buffer is not in filter group, move cycle index once.
      (setq filter-count (1- filter-count))) ; Prevent infinite loop in case there is no match.
    ;; Determine if the filter changed.
    (let ((filter-changed-p (not (eq prev-filter-name
				     (star-tabs-get-active-filter-name)))))
      (when (and (not inhibit-refresh)
		 filter-changed-p)
	(run-hooks 'star-tabs-filter-switch-hook)) ; REVIEW: Will this trigger even if we don't actually change filter?
      (if filter-changed-p
	  (star-tabs-get-active-filter-name)
	nil))))

(defun star-tabs-add-to-always-include-in-filter (buffer &optional filter-name collection-name)
  "Always include buffer BUFFER in filter FILTER-NAME of collection COLLECTION-NAME."
  ;; REVIEW: inhibit-hooks maybe?
  (or filter-name (setq filter-name (star-tabs-get-active-filter-name)))
  (or collection-name (setq collection-name (star-tabs-active-collection-name)))
  (let* ((buffer-name (buffer-name buffer))
	 (regexp (concat "^" (regexp-quote (buffer-name (current-buffer))) "$"))
	 (always-include (star-tabs-get-filter-prop-value :always-include filter-name collection-name))
	 (always-include (when (not (member regexp always-include))
			   (push regexp always-include))))
    (when always-include
      (star-tabs-set-filter-prop-value :always-include always-include nil filter-name collection-name))))

(defun star-tabs-exclude-from-filter (buffer &optional filter-name collection-name)
  "Exclude buffer BUFFER from filter FILTER-NAME of collection COLLECTION-NAME.
Also remove it from automatic inclusion, if applicable."
  ;; REVIEW: inhibit-hooks maybe?
  (or filter-name (setq filter-name (star-tabs-get-active-filter-name)))
  (or collection-name (setq collection-name (star-tabs-active-collection-name)))
  (let* ((buffer-name (buffer-name buffer))
	 (regexp (concat "^" (regexp-quote (buffer-name (current-buffer))) "$"))
	 (always-include (remove regexp
				 (star-tabs-get-filter-prop-value :always-include filter-name collection-name)))
	 (exclude (star-tabs-get-filter-prop-value :exclude filter-name collection-name))
	 (exclude (when (not (member regexp exclude))
			     (push regexp exclude))))
    (star-tabs-set-filter-prop-value :always-include always-include nil filter-name collection-name)
    (when exclude
      (star-tabs-set-filter-prop-value :exclude exclude nil filter-name collection-name))))

(defun star-tabs-include-current-buffer-in-current-filter ()
  "Include current buffer in the active filter."
  (interactive)
  (star-tabs-add-to-always-include-in-filter (star-tabs-current-buffer)))

(defun star-tabs-exclude-current-buffer-from-current-filter ()
  "Exclude current buffer from the active filter group."
  (interactive)
  (star-tabs-exclude-from-filter (star-tabs-current-buffer)))

(defun star-tabs-print-active-filter-name ()
  "Output the active filter name as a message."
  ;; REVIEW: Probably not needed.
  (interactive)
  (message "STAR-TABS: Active filter name: %s" (star-tabs-get-active-filter-name)))

(defun star-tabs-auto-sort (&optional filter-name collection-name sort-method)
  "Automatically sort filter group FILTER-NAME in collection COLLECTION-NAME.
FILTER-NAME defaults to the active filter group.
COLLECTION-NAME defaults to the active collection.
SORT-METHOD defaults to the filter property :auto-sort of FILTER-NAME, which by default is nil.
If filter property :auto-sort is non-nil and SORT-METHOD is not specified, nothing will be sorted.\n
The following will explain the different options for SORT-METHOD:
- 'recent-first - The most recent (current) buffer will always be the first (left-most) tab. The second most recent will be the second tab, and so on. 
The last (right-most) tab will thus be the buffer to last be revisited/reopened." 
;; TODO: Add the following sorting methods:
;; - 'last-first (to be implemented) - The opposite of 'recent-first. That is, the most recent buffer will be last in the tab bar. 
;; - 'alpha-desc (to be implemented) - Tabs will be sorted alphabetically, in a descending order; buffers with names starting with 'a' will appear 
;; left of those starting with 'z', for example.
;; - 'alpha-asc (to be implemented) - The same as 'alpha-desc, except that tabs will be sorted in an ascending alphabetical order.
;; - 'extension-desc (to be implemented) - Sort tabs by their file extensions alphabetically in a descending order. 
;; - 'extension-asc (to be implemented) - The same as 'extension-desc, except that the ordering will be in an ascending order.
;; TODO: Add :auto-sort as a collection property as well. If it's not set on filter-level, it will default to the collection property (if set), 
;; otherwise nil
;; TODO: Allow for multiple sorting methods at the same time.
  (or filter-name (setq filter-name (star-tabs-get-active-filter-name)))
  (or collection-name (setq collection-name (star-tabs-active-collection-name)))
  (or sort-method (setq sort-method (star-tabs-get-filter-prop-value :auto-sort filter-name collection-name)))
  (when sort-method
    (cond ((equal sort-method 'recent-first)
	   (star-tabs-move-current-tab-to-first)))))

(defun star-tabs-set-filter-prop-value (prop value &optional inhibit-hook filter-name collection-name)
  "Set property PROP of filter FILTER-NAME in collection COLLECTION-NAME to value VALUE.
Also run hook star-tabs-collection-property-change-hook unless inhibit-hook is non-nil."
  ;; REVIEW: Make a filter-prop-change-hook as well?
  (or filter-name (setq filter-name (star-tabs-get-active-filter-name)))
  (or collection-name (setq collection-name (star-tabs-active-collection-name)))
  (plist-put (star-tabs-get-filter-props filter-name collection-name)
	     prop
	     value)
  (unless inhibit-hook
    (run-hook-with-args 'star-tabs-collection-property-change-hook collection-name)))

(defun star-tabs-switch-to-filter (filter-name &optional inhibit-hook collection-name recursive)
  ;; TODO: Add ability to switch collections as well.
  (or collection-name (setq collection-name (star-tabs-active-collection-name)))
  (when (member filter-name (star-tabs-get-filter-names collection-name))
    (let ((filter-count (length (star-tabs-get-filter-names collection-name)))
	  (current-filter (star-tabs-get-active-filter-name)))
      (while (and (not (eq (star-tabs-get-active-filter-name)
			   filter-name))
		  (>= filter-count 0))
	(star-tabs-cycle-filters nil t t)
	(setq filter-count (1- filter-count)))
      (when (and (not (eq filter-name (star-tabs-get-active-filter-name)))
		 (not (eq current-filter (star-tabs-get-active-filter-name)))
		 (not recursive))
	;; REVIEW: can this cause inf. loops?
	(star-tabs-switch-to-filter current-filter t collection-name t)))
    ;; FIXME: this will trigger even if we remain in the same filter group (not really a big problem though).
    (unless inhibit-hook
      (run-hooks 'star-tabs-filter-switch-hook))))

(defun star-tabs-all-modified-buffers ()
  "Create a filter group with all modified buffers, then switch to it."
  (interactive)
  (star-tabs-add-filter
   :name 'modified-buffers
   :use t
   :only-modified-buffers t))

(defun star-tabs-remove-all-modified-buffers-group ()
  "Remove the filter created with `star-tabs-all-modified-buffers'."
  (interactive)
  (star-tabs-remove-filter 'modified-buffers))



;; Get filter data 

(defun star-tabs-get-filter-name (filter-name &optional collection-name)
  "Return the filter group FILTER-NAME of collection COLLECTION-NAME.
COLLECTION-NAME defaults to the active collection."
  (setq collection-name (or collection-name (star-tabs-active-collection-name)))
  (alist-get filter-name (eval collection-name)))

(defun star-tabs-get-filter-names (&optional collection-name)
  "Return all filter group names in collection COLLECTION-NAME.
COLLECTION-NAME defaults to the active collection."
  (setq collection-name (or collection-name (star-tabs-active-collection-name)))
  (mapcar 'car (eval collection-name)))

(defun star-tabs-get-active-filter ()
  "Return the active filter."
 (car (eval (star-tabs-active-collection-name))))

(defun star-tabs-get-active-filter-name ()
  "Return the active filter's name as a symbol. If there is no active filter, return 'ALL" 
  (interactive)
  (or (car(star-tabs-get-active-filter))
      'ALL))

(defun star-tabs-get-filter-prop-value (prop &optional filter-name collection-name)
  "Return the value of the property PROP of filter FILTER-NAME in collection COLLECTION-NAME."
  (or filter-name (setq filter-name (star-tabs-get-active-filter-name)))
  (or collection-name (setq collection-name (star-tabs-active-collection-name)))
  (let ((props (star-tabs-get-filter-props filter-name collection-name)))
    (plist-get props prop)))

(defun star-tabs-get-filter-props (&optional filter-name collection-name)
  "Return the properties of filter FILTER-NAME in collection COLLECTION-NAME."
  (or filter-name (setq filter-name (star-tabs-get-active-filter-name)))
  (or collection-name (setq collection-name (star-tabs-active-collection-name)))
  (alist-get filter-name (eval collection-name)))


;; Apply filters

(defun star-tabs-filter-buffers (filter-name buffer-list)
  "Filter buffers BUFFER-LIST with filter FILTER-NAME and return the filtered list of buffers."
  ;; REVIEW: No longer necessary to maintain the order when filtering since that is handled elsewhere. Refactor?
  ;; REVIEW: Exluce first, then include??
  (let* ((filter (star-tabs-get-filter-name filter-name))
	 (include (plist-get filter :include))
	 (exclude (plist-get filter :exclude))
	 (always-include (plist-get filter :always-include))
	 (only-modified-buffers (plist-get filter :only-modified-buffers))
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
	(when only-modified-buffers
	  (setq buffers (delq nil (mapcar (lambda (buffer)
					    (when (buffer-modified-p buffer)
					      buffer))
					  buffers))))
    buffers))

(defun star-tabs--apply-filter-list (buffer-list regexps include always-include)
  "Apply all regular expressions in list REGEXPS to all buffers in BUFFER-LIST. 
Return the filtered list of buffers.
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
    (star-tabs-get-buffers buffers)))

(defun star-tabs-filter-by-prefix (buffer-list prefix-list &optional include)
  "Return globally filtered buffers BUFFER-LIST with/without the prefixes PREFIX-LIST. 
If INCLUDE (default nil) is non-nil, return a list of buffers that match any of the prefixes.
Otherwise, if INCLUDE is nil, return a list of buffers that don't match any of the prefixes."
  (or include (setq include nil))
  (delq nil (mapcar (lambda (buffer)
		      (if (not (member nil (mapcar (lambda (prefix)
						     (funcall #'star-tabs-buffer-name-prefix-p prefix buffer))
						   prefix-list)))
			  (if include nil buffer)
			(if include buffer nil)))
		    buffer-list)))

(defun star-tabs-buffer-name-prefix-p (prefix buffer)
  "Return buffer BUFFER if its name has the prefix PREFIX. Otherwise, return nil."
  (if (string-prefix-p prefix (buffer-name buffer))
      nil  
    buffer))


;; Cache filtered buffers

(defun star-tabs-clear-cached-buffers ()
  "Clear cache of unwanted/unreal buffers star-tabs-cached-filtered-buffers."
  (interactive)
  (clrhash star-tabs-cached-filtered-buffers))

(defun star-tabs-add-filtered-buffer-to-cache (buffer hash-table)
  "Add unwanted/unreal buffer BUFFER to cache HASH-TABLE."
  (puthash buffer t hash-table))

(defun star-tabs-add-filtered-buffers-to-cache (buffer-list hash-table)
  "Add unwanted/unreal buffers BUFFER-LIST to cache HASH-TABLE."
  (mapc (lambda (buffer)
	  (star-tabs-add-filtered-buffer-to-cache buffer hash-table))
	buffer-list))


;; File extension filters

(defun star-tabs-get-file-extensions ()
  "Return all file extension names for all active buffers."
  ;; TODO: Move regexp to global vars
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
  "Add an inclusive filter for file extension EXTENSION-NAME to collection COLLECTION-NAME.
COLLECTION-NAME defaults to the active collection.
Return non-nil if a filter was added, otherwise return nil."
  ;; REVIEW: Remove inhibit-refresh parameter?
  (setq collection-name (or collection-name (star-tabs-active-collection-name)))
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
  "Update automatically added file extension filters in collection COLLECTION-NAME.
COLLECTION-NAME defaults to the active collection.
Return non-nil if a filter was added or removed, otherwise nil."
  ;; REVIEW: Remove inhibit-refresh parameter?
  (setq collection-name (or collection-name (star-tabs-active-collection-name)))
  ;; Make sure there is a filter for extensionless files.
  (let ((extensions-updated-p nil)) ; Keep track of whether we actually add or remove a filter.
    ;; TODO: Only add this if there are extensionless files
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
       (star-tabs--set-header-line (star-tabs-get-active-group-buffers) 'save-scroll))
    extensions-updated-p))

(defun star-tabs--remove-file-extension-filter (filter-name &optional inhibit-refresh collection-name)
  "Remove automatically added file extension filter FILTER-NAME from collection COLLECTION-NAME.
COLLECTION-NAME defaults to the active collection.
Return non-nil if a filter was removed, otherwise nil."
  ;; REVIEW: Remove inhibit-refresh parameter?
  (setq collection-name (or collection-name (star-tabs-active-collection-name)))
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
  "Remove all automatically added file extension filters from collection COLLECTION-NAME.
Return non-nil if a filter was removed, otherwise nil.
COLLECTION-NAME defaults to the active collection."
  ;; REVIEW: Remove inhibit-refresh paremater (these filters should only be removed by setting collection prop)?
  (setq collection-name (or collection-name (star-tabs-active-collection-name)))
  (let ((file-extensions star-tabs-file-extension-filter-names)
	(extensions-updated-p nil))
    ;; Remove file extension filters, if any.
    (dolist (ext file-extensions)
      (setq extensions-updated-p (or (star-tabs--remove-file-extension-filter ext inhibit-refresh collection-name)
				     extensions-updated-p)))
    ;; Remove the extensionless file filter.
    (setq extensions-updated-p (or (star-tabs-remove-filter 'extensionless inhibit-refresh collection-name)
				   extensions-updated-p))
    extensions-updated-p))

(defun star-tabs--auto-activate-file-extension-filters-on-buffer-count (threshold)
 "Add file extension filters when the total number of real buffers reaches or exceeds THRESHOLD.
More specifically, when the threshold is met, star-tabs-add-file-extension-filters is automatically set to t,
and file extension filters are subsequently added. If the buffer count goes down below the threshold again,
star-tabs-add-file-extension-filters is then set to nil, and all automatically added file extension filters are removed.
Deactivate this feature by setting collection property :file-extension-filter-threshold to 0."
 (unless (<= threshold 0)
   (if (and (not star-tabs-add-file-extension-filters)
	    (>= (length star-tabs-active-buffers) threshold))
       (setq star-tabs-add-file-extension-filters t)
     (when star-tabs-add-file-extension-filters
       (setq star-tabs-add-file-extension-filters nil)))))


;;; Buffers

;; Buffer killing :*(

(defun star-tabs-kill-all-buffers-in-filter (&optional filter-name)
  "Kill all buffers in the filter group FILTER-NAME (defaults to the active filter)."
  (interactive)
  (or filter-name (setq filter-name (star-tabs-get-active-filter-name)))
  (let ((buffers (star-tabs-get-group-buffers filter-name)))
    (star-tabs--kill-buffers buffers)))

(defun star-tabs-kill-all-unmodified-buffers-in-filter (&optional filter-name)
  "Kill all unmodified buffers in the filter group FILTER-NAME (defaults to the active filter)."
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
FILTER-NAME defaults to the active filter."
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
FILTER-NAME defaults to the active filter."
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
	      (let ((killed-buffers (seq-difference star-tabs-active-buffers buffer-list))
		    (new-buffers (seq-difference buffer-list star-tabs-active-buffers)))
		(setq star-tabs-active-buffers (star-tabs-update-list star-tabs-active-buffers buffer-list))
		(run-hook-with-args 'star-tabs-buffer-list-update-hook new-buffers killed-buffers)
		t))
	  nil)))))

(defun star-tabs--filter-all-buffers ()
  "Apply filters to all real buffers in, and set create separate buffer lists for, all filter groups in the active collection."
  ;; TODO: Only apply filters to new buffers, and remove killed buffers, instead of doing it all
  ;;       for all buffers in all filter groups. Make sure all buffers are filtered for any new groups created though.
  (let ((filters (star-tabs-get-filter-names))
	(filtered-buffers)
	(new-buffers))
    (dolist (filter filters)
      (setq filtered-buffers (star-tabs-filter-buffers filter
						       star-tabs-active-buffers))
      (dolist (buffer filtered-buffers)
      	(if (not (member buffer (star-tabs-get-group-buffers filter)))
      	    (setq new-buffers (append new-buffers (list buffer)))))
      (star-tabs-set-filter-prop-value :buffer-list
				       (star-tabs-update-list (star-tabs-get-filter-prop-value :buffer-list
											       filter)
							      filtered-buffers)
				       t
				       filter)

      (dolist (buffer new-buffers)
	(star-tabs--init-tab buffer filter))
      (setq new-buffers nil))))

(defun star-tabs-get-group-buffers (&optional filter-name collection-name)
  "Return all buffers in the filter group FILTER-NAME of collection COLLECTION-NAME as a list."
  (or filter-name (setq filter-name (star-tabs-get-active-filter-name)))
  (or collection-name (setq collection-name (star-tabs-active-collection-name)))
  (star-tabs-get-filter-prop-value :buffer-list filter-name collection-name))

(defun star-tabs-get-active-group-buffers ()
  "Return all buffers in the active filter group as a list."
  (star-tabs-get-group-buffers (star-tabs-get-active-filter-name) (star-tabs-active-collection-name)))


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
	(let ((last-active-buffer star-tabs-current-buffer)
	      (new-active-buffer (current-buffer)))
	(progn
	  ;; Refresh the filter-name timer if it was running when the buffer switched.
	  ;; REVIEW: Do this for collection-name timer as well?
	  (when star-tabs-tab-bar-filter-name
	    (star-tabs--display-filter-name-temporarily))
	  (setq star-tabs-current-buffer (current-buffer))
	  (run-hook-with-args 'star-tabs-buffer-switch-hook last-active-buffer new-active-buffer)
	  t))
      nil)))


;;; Tab bar

;; Format string ("%s %d")

(defun star-tabs--eval-tab (buffer &optional filter-name collection-name)
  "Return a string representation of tab BUFFER in filter group FILTER-NAME of collection COLLECTION-NAME." 
  (or filter-name (setq filter-name (star-tabs-get-active-filter-name)))
  (or collection-name (setq collection-name (star-tabs-active-collection-name)))
  (let ((tab-number (1+ (cl-position buffer
				     (star-tabs-get-group-buffers filter-name
								  collection-name)))))

    
    ))

(defun star-tabs--init-tab (buffer &optional filter-name collection-name)
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
  ;; TODO: Update docstring
  (or filter-name (setq filter-name (star-tabs-get-active-filter-name)))
  (or collection-name (setq collection-name (star-tabs-active-collection-name)))
  (let* ((tab-buffer buffer)
	 (tab-name `(buffer-name ,tab-buffer))
	 (tab-icon (star-tabs--select-icon tab-buffer))
	 (tab-icon-background `(if (eq ,tab-buffer (star-tabs-current-buffer))
				   (face-background 'star-tabs-selected-tab)
				 (face-background 'star-tabs-non-selected-tab)))
	 (tab-icon-face `(list :inherit (get-text-property 0 'face ,tab-icon)
			       :background ,tab-icon-background))
	 (tab-face `(if (equal ,tab-buffer (star-tabs-current-buffer))
			(quote star-tabs-selected-tab)
		      (quote star-tabs-non-selected-tab)))
	 (tab-border-style (star-tabs-get-collection-prop-value :border-style))
	 (tab-border-xpm-selected (cond
				   ((eq tab-border-style 'rounded)
				    star-tabs-tab-border-round-selected)
				   ((eq tab-border-style 'slanted)
				    star-tabs-tab-border-slant-selected)))
	 (tab-border-xpm-non-selected (cond
				       ((eq tab-border-style 'rounded)
					star-tabs-tab-border-round-non-selected)
				       ((eq tab-border-style 'slanted)
					star-tabs-tab-border-slant-non-selected)))
	 (tab-separator-left `(star-tabs--create-image (star-tabs--mirror-xpm (star-tabs--fill-xpm
									      (if (eq ,tab-buffer (star-tabs-current-buffer))
										  ,tab-border-xpm-selected
										,tab-border-xpm-non-selected)
									      40))))
	 (tab-separator-right `(star-tabs--create-image (star-tabs--fill-xpm (if (eq ,tab-buffer (star-tabs-current-buffer))
										 ,tab-border-xpm-selected
									      ,tab-border-xpm-non-selected)
									    40)))
	 (modified-icon-face `(if (equal ,tab-buffer (star-tabs-current-buffer))
				  (quote star-tabs-selected-modified-icon)
				(quote star-tabs-non-selected-modified-icon)))
	 (tab-mouse-face `(if (equal ,tab-buffer (star-tabs-current-buffer))
			      'star-tabs-mouse-selected
			    'star-tabs-mouse-non-selected))
	 (tab-divider-mouse-face `(if (equal ,tab-buffer (star-tabs-current-buffer))
				      'star-tabs-tab-divider-mouse-selected
				    'star-tabs-tab-divider-mouse-non-selected))
	 (tab-number `(star-tabs--get-buffer-number ,tab-buffer
						    (quote ,filter-name)
						    (quote ,collection-name)))
	 (tab-number-string `(propertize (number-to-string ,tab-number)
					 'keymap star-tabs-map-select-tab
					 'face ,tab-face
					 'mouse-face ,tab-mouse-face
					 'buffer-name ,tab-name
					 'buffer-number ,tab-number))
	 (tab-name-string `(propertize (concat
					star-tabs-number-name-separator
					(if (star-tabs-get-collection-prop-value :hide-extension-names (quote ,collection-name))
					    (progn (string-match "\\(^.+\\)\\(\\..*$\\)" ,tab-name)
						   (or (match-string 1 ,tab-name)
						       ,tab-name))
					  ,tab-name)
					star-tabs-name-modified-icon-separator)
				       'keymap star-tabs-map-select-tab
				       'face ,tab-face
				       'mouse-face ,tab-mouse-face
				       'buffer-name ,tab-name
				       'buffer-number ,tab-number))
	 (tab-icon-string (if (stringp tab-icon)
			      `(propertize ,tab-icon 
					   'face ,tab-icon-face
					   'mouse-face ,tab-mouse-face)
			    ""))
	 (close-button `(propertize (if (not (star-tabs-get-collection-prop-value
					      :hide-close-buttons (quote ,collection-name)))
					star-tabs-close-buffer-icon
				      "")
				    'keymap star-tabs-map-close-tab
				    'face ,tab-face
				    'mouse-face ,tab-mouse-face
				    'buffer-name ,tab-name
				    'buffer-number ,tab-number))
	 (divider `(propertize " " 
			       'keymap star-tabs-map-select-tab
			       'face ,tab-face
			       'mouse-face ,tab-mouse-face
			       'buffer-name ,tab-name
			       'buffer-number ,tab-number))
	 (tab-divider `(propertize " " 
				   'keymap star-tabs-map-select-tab
				   'face ,tab-face
				   'mouse-face ,tab-divider-mouse-face
				   'buffer-name ,tab-name
				   'buffer-number ,tab-number))
	 (modified-icon `(propertize (if (and (not (string-match "^[[:space:]]" ,tab-name))
					      (not (string-match "^*.*\\*$" ,tab-name))
					      (not (star-tabs-buffer-read-only-p ,tab-name)))
					 ;; Display (un)modified symbol:
					 (concat  
					  (if (buffer-modified-p ,tab-buffer)
					      star-tabs-modified-buffer-icon
					    star-tabs-unmodified-buffer-icon))
				       ;; Display nothing if it's an unreal buffer, system buffer, or read-only buffer:
				       "")
				     'keymap star-tabs-map-select-tab
				     'face ,modified-icon-face
				     'mouse-face ,tab-mouse-face
				     'buffer-name ,tab-name
				     'buffer-number ,tab-number))
	 (tab-string `(concat ,tab-separator-left
			      ,divider
			      ,tab-icon-string
			      ,divider
			      ,tab-number-string
			      ,tab-name-string
			      (if (buffer-modified-p ,tab-buffer)
				  ,modified-icon
				,close-button)
			      ,tab-divider
			      ,tab-separator-right))
	 (tab-string-cached (eval tab-string))
	 (tab-digit-width (star-tabs-string-pixel-width (substring (eval tab-number-string) 0 1)))
	 (tab-number-width `(* ,tab-digit-width (eval (length (int-to-string ,tab-number)))))
	 (tab-column-width `(length (eval ,tab-string)))
	 (tab-pixel-width-without-number (- (star-tabs-string-pixel-width tab-string-cached)
					    (eval tab-number-width)))
	 (tab-pixel-width `(+ ,tab-pixel-width-without-number
			      ,tab-number-width))
	 (tab `(,tab-buffer :tab-string-divider ,divider
			    :tab-string-icon ,tab-icon-string
			    :tab-string-name ,tab-name-string
			    :tab-string-number ,tab-number-string
			    :tab-string-modified-icon ,modified-icon
			    :tab-string-close-button ,close-button
			    :tab-string-tab-divider ,tab-divider
			    :tab-number ,tab-number ; REVIEW: Maybe no need to store this here?
			    :tab-name ,tab-name
			    :tab-string ,tab-string
			    :tab-string-cached ,tab-string-cached
			    :tab-column-width ,tab-column-width
			    :tab-pixel-width ,tab-pixel-width)))
      ;; Add tab to the filter group property :tabs as an alist,
      ;; where the key is the buffer and the value is a plist of tab properties.
      (if (alist-get tab-buffer (star-tabs-get-filter-prop-value :tabs filter-name collection-name))
		 (setf (alist-get tab-buffer (star-tabs-get-filter-prop-value :tabs filter-name collection-name)) (cdr tab))
	(star-tabs-set-filter-prop-value :tabs
      					 (append (star-tabs-get-filter-prop-value :tabs filter-name collection-name)
      						 (list tab))
      					 t
					 filter-name
					 collection-name))
      tab-string-cached))

(defun star-tabs-set-tab-prop-value (tab-or-buffer prop value &optional filter-name collection-name)
  (or filter-name (setq filter-name (star-tabs-get-active-filter-name)))
  (or collection-name (setq collection-name (star-tabs-active-collection-name)))
  (let ((tab-props (or (when (not (bufferp tab-or-buffer))
			 tab-or-buffer)
		       (star-tabs-get-tab tab-or-buffer filter-name collection-name))))
    (plist-put tab-props
	       prop
	       value)
  (star-tabs-get-tab tab-or-buffer filter-name collection-name)))

(defun star-tabs-get-tab (buffer &optional filter-name collection-name)
  "Return the tab corresponding to buffer BUFFER in filter group FILTER-NAME of collection COLLECTION-NAME.
If no tab is found, return nil."
  (or filter-name (setq filter-name (star-tabs-get-active-filter-name)))
  (or collection-name (setq collection-name (star-tabs-active-collection-name)))
  (alist-get buffer (star-tabs-get-filter-prop-value :tabs filter-name collection-name)))

(defun star-tabs-get-tab-prop-value (tab-or-buffer prop &optional filter-name collection-name)
  "Return the value of property PROP of tab/buffer TAB-OR-BUFFER in filter group FILTER-name of collection COLLECTION-NAME.
Return nil if either the property is not found, or if the tab doesn't exists."
  (or filter-name (setq filter-name (star-tabs-get-active-filter-name)))
  (or collection-name (setq collection-name (star-tabs-active-collection-name)))
  (plist-get (star-tabs-get-tab-props tab-or-buffer filter-name collection-name) prop))

(defun star-tabs-get-tab-props (tab-or-buffer &optional filter-name collection-name)
  "Return the properties of tab TAB-OR-BUFFER in filter group FILTER-NAME of collection COLLECTION-NAME.
If TAB-OR-BUFFER is a tab, return itself.
If TAB-OR-BUFFER is a buffer, return the tab (including its properties) corresponding to TAB-OR-BUFFER."
  (or filter-name (setq filter-name (star-tabs-get-active-filter-name)))
  (or collection-name (setq collection-name (star-tabs-active-collection-name)))
  (if (bufferp tab-or-buffer)
      (star-tabs-get-tab tab-or-buffer filter-name collection-name)
    tab-or-buffer))

(defun star-tabs--select-icon (buffer)
  "Return the all-theicons-icon for buffer BUFFER."
  ;; TODO: add checks for whether all-the-icons is installed (also make sure graphical elements are supported).
  ;; TODO: make height user-customizable.
  (with-current-buffer buffer
   (if (equal major-mode 'fundamental-mode)
     (all-the-icons-icon-for-file (buffer-name buffer) :v-adjust 0.001 :height 0.8)
   (all-the-icons-icon-for-mode major-mode :v-adjust 0.001 :height 0.8))))

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
  (or collection-name (setq collection-name (star-tabs-active-collection-name)))
  (let ((buffers (star-tabs-get-group-buffers filter-name collection-name)) ; This is the order in which the tabs should appear.
	(tab-bar-tabs nil)
	(cumulative-pixel-width '(0)))
    ;; For each buffer in the filter group, get the corresponding tab and add it to a list (the tab bar).
    (dolist (buffer buffers tab-bar-tabs)
      (when (alist-get buffer (star-tabs-get-filter-prop-value :tabs))
	(setq tab-bar-tabs (append tab-bar-tabs
				   (list (star-tabs-get-tab buffer filter-name collection-name))))
	(let* ((pixel-width (eval (star-tabs-get-tab-prop-value buffer :tab-pixel-width filter-name collection-name))))
	  (setq cumulative-pixel-width (append cumulative-pixel-width
					       (list (+ pixel-width
							(or (car (reverse cumulative-pixel-width))
							    0))))))))
    (star-tabs--set-left-margin)
    (star-tabs-set-filter-prop-value :tab-bar-tab-count (length buffers) t filter-name collection-name) ; TODO: Move this to earlier in cycle?
    (star-tabs-set-filter-prop-value :tab-bar-cumulative-pixel-width cumulative-pixel-width t filter-name collection-name)
    (star-tabs-set-filter-prop-value :tab-bar tab-bar-tabs t filter-name collection-name)
    tab-bar-tabs))

(defun star-tabs--set-tab-bar-format (&optional start-number filter-name collection-name)
  "Set and return the string representation of the tab bar for filter FILTER-NAME of collection COLLECTION-NAME.
Scroll START-NUMBER - 1 (default 0) tabs to the right.
The tab bar format can be accessed using (star-tabs-get-filter-prop-value :tab-bar-format FILTER-NAME COLLECTION-NAME)."
  (or filter-name (setq filter-name (star-tabs-get-active-filter-name)))
  (or collection-name (setq collection-name (star-tabs-active-collection-name)))
  (setq start-number (if start-number
			 (min (1- (or (star-tabs-get-filter-prop-value :tab-bar-tab-count filter-name collection-name) 1))
			      (1- (max start-number 1)))
		       0))
  (let* ((tabs (nthcdr start-number (star-tabs-get-filter-prop-value :tab-bar filter-name collection-name)))
	 (tab-bar-left-margin (star-tabs-get-filter-prop-value :tab-bar-left-margin filter-name collection-name))
	 (tab-bar-format (or tab-bar-left-margin "")))
    (dolist (tab tabs)
      (setq tab-bar-format
	    (concat tab-bar-format
		    (eval (star-tabs-get-tab-prop-value tab :tab-string-cached filter-name collection-name)))))
    ;; REVIEW: Add visible tabs prop? (maybe not a good idea since window width can change all the time, meaning we would still need to recalculate.)
    (star-tabs-set-filter-prop-value :tab-bar-format-first-tab-number (1+ start-number) t filter-name collection-name)
    (star-tabs-set-filter-prop-value :tab-bar-format tab-bar-format t filter-name collection-name)
    tab-bar-format))

(defun star-tabs--set-left-margin (&optional filter-name collection-name)
  "Set and return the left margin of the tab bar in filter group FILTER-NAME of collection COLLECTION-NAME.
Properties of the left margin can be accessed using (star-tabs-get-filter-prop-value PROP FILTER-NAME COLLECTION-NAME).
Properties related to the left margin are:
:tab-bar-left-margin - Propertized string representation of the left margin.
:tab-bar-left-margin-width - Width in pixels of the left margin.
:tab-bar-left-margin-column-width - Width in columns of the left margin."
  (or filter-name (setq filter-name (star-tabs-get-active-filter-name)))
  (or collection-name (setq collection-name (star-tabs-active-collection-name)))
  ;; The vertical bar controls the tab bar height.
  (let* ((left-margin-vertical-bar (star-tabs--vertical-bar 40 star-tabs-tab-bar-background))
	 (left-margin-fill (propertize star-tabs-left-margin
				       'face 'star-tabs-tab-bar-left-margin))
	 (left-margin-collection-name (when star-tabs-tab-bar-collection-name 
					(propertize
					 (let ((collection-name star-tabs-tab-bar-collection-name))
					   (concat (upcase (symbol-name collection-name))
						   star-tabs-filter-name-number-separator))
					 'face 'star-tabs-collection-name)))
	 (left-margin-filter-name (when (and (plist-get (star-tabs-active-collection-props) :display-filter-name)
					     star-tabs-tab-bar-filter-name)
				    (propertize 
				     (let ((filter-name star-tabs-tab-bar-filter-name))
				       (concat (upcase (symbol-name filter-name))
					       star-tabs-filter-name-number-separator))
				     'face 'star-tabs-filter-name)))
	 (tab-bar-left-margin (concat left-margin-fill
				      left-margin-vertical-bar
				      (or left-margin-collection-name "")
				      (or left-margin-filter-name "")))
	 (tab-bar-left-margin-width (star-tabs-string-pixel-width tab-bar-left-margin)))
    (star-tabs-set-filter-prop-value :tab-bar-left-margin-width tab-bar-left-margin-width t filter-name collection-name)
    (star-tabs-set-filter-prop-value :tab-bar-left-margin-column-width (length tab-bar-left-margin) t filter-name collection-name)
    (star-tabs-set-filter-prop-value :tab-bar-left-margin tab-bar-left-margin t filter-name collection-name)
    tab-bar-left-margin))

(defun star-tabs-recache-tab (buffer &optional all-groups collection-name filter-name)
  "Update the cached string representation of tab BUFFER in filter group FILTER-NAME of collection COLLECTION-NAME.
If ALL-GROUPS is non-nil, update all tabs BUFFER in all filter groups of collection COLLECTION-NAME."
  (or filter-name (setq filter-name (star-tabs-get-active-filter-name)))
  (or collection-name (setq collection-name (star-tabs-active-collection-name)))
  (let ((filters (star-tabs-get-filter-names)))
    (if all-groups
	(dolist (filter filters)
	  (let ((tab-string (eval (star-tabs-get-tab-prop-value buffer :tab-string filter collection-name))))
	    (star-tabs-set-tab-prop-value buffer :tab-string-cached tab-string filter collection-name)
	    tab-string))
      (let ((tab-string (eval (star-tabs-get-tab-prop-value buffer :tab-string filter-name collection-name))))
	(star-tabs-set-tab-prop-value buffer :tab-string-cached tab-string filter-name collection-name)))))

(defun star-tabs--recache-tabs (buffers &optional all-groups collection-name filter-name)
  "Update the cached string representation of tabs BUFFERS in filter group FILTER-NAME of collection COLLECTION-NAME.
If ALL-GROUPS is non-nil, update all tabs BUFFERS in all filter groups of collection COLLECTION-NAME."
  (or filter-name (setq filter-name (star-tabs-get-active-filter-name)))
  (or collection-name (setq collection-name (star-tabs-active-collection-name)))
  (dolist (buffer buffers)
    (star-tabs-recache-tab buffer all-groups collection-name filter-name)))

(defun star-tabs--update-tabs (buffers &optional filter-name collection-name)
  "Update tabs BUFFERS in the active filter group."
  ;; TODO: revamp how tabs are updated. 
  (or filter-name (setq filter-name (star-tabs-get-active-filter-name)))
  (or collection-name (setq collection-name (star-tabs-active-collection-name)))
  (dolist (buffer buffers)
    (star-tabs--init-tab buffer filter-name collection-name)))

(defun star-tabs-update-tab (buffer &optional filter-name collection-name)
  "Create or update tab BUFFER in filter group FILTER-NAME of collection COLLECTION-NAME."
  (or filter-name (setq filter-name (star-tabs-get-active-filter-name)))
  (or collection-name (setq collection-name (star-tabs-active-collection-name)))
  (star-tabs--init-tab buffer filter-name collection-name))

(defun star-tabs-update-current-tab ()
  "Update the tab for the current buffer in the active group."
  (interactive)
  (star-tabs--init-tab (current-buffer)))


;; Scrolling

(defun star-tabs-scroll-tab-bar (&optional backward count)
  "Horizontally scroll the tab bar to the right (left if BACKWARD is non-nil), COUNT (default 2) times."
  (or count (setq count 2))
  ;; Keep timers going to display filter name and collection name in the tab bar, if they are running.
  (when star-tabs-tab-bar-filter-name
    (star-tabs--display-filter-name-temporarily))
  (when star-tabs-tab-bar-collection-name
    (star-tabs--display-collection-name-temporarily))
  (let* ((first-tab-number (star-tabs--first-number-in-tab-bar))
	 (count (if backward
		    (- first-tab-number (max count 1))
		  (+ first-tab-number (max count 0))))) 
    ;; Don't scroll past the last tab.
    (setq count (min
		 (length (star-tabs-get-active-group-buffers))
		 count))
    (star-tabs--set-header-line (star-tabs-get-active-group-buffers) count)))

(defun star-tabs-scroll-tab-bar-forward (&optional count)
  "Scroll tab bar forward prefix argument COUNT (default 2) number of tabs.
Switch to the next filter group if we're already scrolled all the way to the right."
  ;; TODO: Fix docstring wording
  (interactive "P") 
  (or count (setq count 2))
  (if (and (>  (+ count (star-tabs-get-filter-prop-value :tab-bar-format-first-tab-number))
	       (star-tabs-get-filter-prop-value :tab-bar-tab-count))
	   (not (star-tabs-get-collection-prop-value :disable-scroll-to-filter)))
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
	   (not (star-tabs-get-collection-prop-value :disable-scroll-to-filter)))
      ;; Move to the end of the previous filter group if scrolled all the way to the left in the current group.
      (progn (star-tabs-cycle-filters t)
	     (star-tabs-scroll-tab-bar nil 1000000))
    (star-tabs-scroll-tab-bar t count)))

(defun star-tabs-scroll-to-buffer (&optional buffer filter-name collection-name)
  "Return the number of tabs required to scroll to tab BUFFER in filter group FILTER-NAME in collection COLLECTION-NAME."
  ;; TODO: change function name.
  ;; TODO: change return value from list to int.
  (or filter-name (setq filter-name (star-tabs-get-active-filter-name)))
  (or collection-name (setq collection-name (star-tabs-active-collection-name)))
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
  "Scroll to the tab of the active buffer. Also switch filter if the buffer is not in the current group."
  (interactive)
  (star-tabs-find-active-filter)
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
	  (message "STAR-TABS: SCROLL: %s" scroll))
      ;; REVIEW: Make sure scroll max (and min?) values are always enforced.
      (or scroll (setq scroll 0))
      (unless (integerp scroll)
	(setq scroll (cond ((equal scroll 'save-scroll) (star-tabs--first-number-in-tab-bar))
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
			(window-font-width nil 'star-tabs-tab-bar-empty-space)))
	(white-space ""))
    (while (>= empty-space 0)
      (setq white-space (concat " " white-space))
      (setq empty-space (1- empty-space)))
    (propertize white-space
		'face 'star-tabs-tab-bar-empty-space))) 

(defun star-tabs--display-filter-name-temporarily (&optional filter-name)
  "Return filter name FILTER-NAME for temporary display in tab bar. 
Unless set, FILTER-NAME defaults to the active filter name.
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
							'save-scroll)))

(defun star-tabs--display-collection-name-temporarily (&optional collection-name)
  "Return collection name COLLECTION-NAME for temporary display in tab bar. 
Unless set, COLLECTION-NAME defaults to the active collection name.
This function uses global helper variable star-tabs-collection-name-timer to keep track of the timer."
  ;; Force cancel on any other active timers set with this function.
  (or collection-name (setq collection-name (star-tabs-active-collection-name t)))
  (when star-tabs-collection-name-timer
    (cancel-timer star-tabs-collection-name-timer)
    star-tabs-collection-name-timer nil)
  (setq star-tabs-collection-name-timer (star-tabs-set-temporarily 'star-tabs-tab-bar-collection-name
							collection-name
							"1 sec"
							nil
							#'star-tabs--set-header-line
							(star-tabs-get-active-group-buffers)
							'save-scroll)))


;; Display helper functions

(defun star-tabs--string-truncated-p (string)
  "Return t if the width of string STRING is greater than the width of the current window.
Otherwise, return the number of truncated pixels."
  ;; REVIEW: Deprecated ?
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
  (or (star-tabs-get-filter-prop-value :tab-bar-format-first-tab-number) 0))

(defun star-tabs--current-buffer-number ()
  "Return the tab number of the current buffer.
If the current buffer is not in the active filter group, return 0."
  (star-tabs--get-buffer-number))

(defun star-tabs--get-buffer-number (&optional buffer filter-name collection-name)
  "Return the tab number of buffer BUFFER in filter group FILTER-NAME of collection COLLECTION-NAME.
If the buffer is not in the filter group, return 0."
  (or filter-name (setq filter-name (star-tabs-get-active-filter-name)))
  (or collection-name (setq collection-name (star-tabs-active-collection-name)))
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

(defun star-tabs-on-buffer-list-update (new-buffers killed-buffers)
  "Run when the list of real buffers updates."
  (when star-tabs-debug-messages
    (message "STAR-TABS: Real buffer list updated"))
  (star-tabs--add-and-remove-file-extension-filters t t)
  (let ((filters-to-be-updated nil))
    ;; If there are killed buffers, find which groups they belonged to so that we can refresh the other tabs
    ;; in those groups (mainly their numbers).
    (when killed-buffers
      (let ((filters (star-tabs-get-filter-names)))
	(dolist (buffer killed-buffers)
	  (dolist (filter filters)
	    (when (member buffer (star-tabs-get-group-buffers filter))
	      (setq filters-to-be-updated (append filters-to-be-updated (list filter))))))))
    (star-tabs--filter-all-buffers)
    ;; Switch filter if the active filter group is empty
    (when (not (star-tabs-get-active-group-buffers))
      (star-tabs-cycle-filters t))
    (dolist (filter filters-to-be-updated)
      (star-tabs--recache-tabs (star-tabs-get-group-buffers filter) nil (star-tabs-active-collection-name) filter)))
  (star-tabs--set-header-line (star-tabs-get-active-group-buffers) 'save-scroll))

(defun star-tabs-on-buffer-switch (last-active-buffer new-active-buffer)
  "Run when the current real buffer is switched."
  (when star-tabs-debug-messages
    (message "STAR-TABS: Buffer Switched: %s" (buffer-name (current-buffer))))
  ;; Find a filter for the new buffer.
  (when (star-tabs-find-active-filter t)
    (star-tabs-on-filter-switch t))
  ;; Auto Sort.
  (when (equal (star-tabs-get-filter-prop-value :auto-sort) 'recent-first)
    (star-tabs-auto-sort))
  ;; Update and display tab bar.
  ;;(star-tabs--update-tabs (star-tabs-get-active-group-buffers))
  (when (member last-active-buffer star-tabs-active-buffers)
    ;; Don't recache last active tab if it was killed.
    (star-tabs-recache-tab last-active-buffer t)) 
  (star-tabs-recache-tab new-active-buffer t)
  (if (star-tabs--tab-visible-p (star-tabs-current-buffer))
      (star-tabs--set-header-line (star-tabs-get-active-group-buffers) 'save-scroll)
    (star-tabs--set-header-line (star-tabs-get-active-group-buffers) 'scroll-to-current-buffer)))

;; TODO: scroll-to-current-buffer should be 'save-scroll if the tab is already visible.


;; Functions to run when modified state changes.

(defun star-tabs-when-buffer-first-modified ()
  "Run when a buffer goes from an unmodified state to a modified state."
  (when (member (current-buffer) star-tabs-active-buffers) ;; REVIEW: Will this trigger if another buffer is modified?
    (set-buffer-modified-p t) ; HACK: Make sure that buffer-modified-p is set to t even though it should automatically be set to t.
    (when star-tabs-debug-messages
      (message "STAR-TABS: Buffer Modified: %s" (buffer-name (current-buffer))))
    (star-tabs--filter-all-buffers) ;; TODO: only update specific filter group
    (when (not (star-tabs-get-active-group-buffers))
      (star-tabs-cycle-filters t))
    (star-tabs-recache-tab (current-buffer) t)
    (star-tabs--set-header-line (star-tabs-get-active-group-buffers) 'save-scroll)))

(defun star-tabs-when-buffer-first-saved ()
  "Run when a buffer goes from a modified state to an unmodified state."
  (when star-tabs-debug-messages
    (message "STAR-TABS: Buffer Saved"))
  (when (member (current-buffer) star-tabs-active-buffers)
    (set-buffer-modified-p nil) ; HACK: Make sure that buffer-modified-p is set to nil even though it should automatically be set to nil.
    ;;(star-tabs--update-tabs (star-tabs-get-active-group-buffers))
    (star-tabs--filter-all-buffers) ;; TODO only update specific filter group
    (when (not (star-tabs-get-active-group-buffers))
      (star-tabs-cycle-filters t))
    (star-tabs-recache-tab (current-buffer) t)
    (star-tabs--set-header-line (star-tabs-get-active-group-buffers) 'save-scroll)))


;; Functions to run when the active filter group or collection changes.

(defun star-tabs-on-filter-switch (&optional inhibit-refresh)
  "Run when the active filter group changes."
  ;; Review: Probably not triggered when changing collections (which subsequently will change the active filter)
  (when star-tabs-debug-messages
    (message "STAR-TABS: Filter Changed"))
  (star-tabs--display-filter-name-temporarily)
  ;;(star-tabs--update-tabs (star-tabs-get-active-group-buffers))
  (unless inhibit-refresh
    (star-tabs--set-header-line (star-tabs-get-active-group-buffers) 'scroll-to-current-buffer)))

(defun star-tabs-on-collection-switch ()
  "Run when the active collection changes."
  ;; TODO: (star-tabs--display-collection-name-temporarily) (and filter-name)
  ;; REVIEW: Make sure this works!
  (when star-tabs-debug-messages
    (message "STAR-TABS: Collection Changed"))
  (star-tabs--add-and-remove-file-extension-filters t t)
  (star-tabs--filter-all-buffers)
  ;;(star-tabs--update-tabs (star-tabs-get-active-group-buffers))
  (star-tabs--set-header-line (star-tabs-get-active-group-buffers) 'scroll-to-current-buffer))


;; Functions to run when collection properties change.

(defun star-tabs--add-and-remove-file-extension-filters (&optional inhibit-hook inhibit-refresh)
  "Add or remove file extension filters, based on the collection settings.\n
File extension filters will be added on one of two conditions:
1. The active collection has the property :enable-file-extension-filters set to non-nil
2. The active collection has the property :enable-file-extension-filters set to nil,
and :file-extension-filter-threshold set above 0, and the total number of buffers (after applying global filters) exceeds that number."
  (setq star-tabs-add-file-extension-filters
	(or (star-tabs-get-collection-prop-value :enable-file-extension-filters) nil))
  ;; Activate file extension filters if the buffer count exceeds the threshold (if set).
  (when (and (not (star-tabs-get-collection-prop-value :enable-file-extension-filters))
	     (not (<= (star-tabs-get-collection-prop-value :file-extension-filter-threshold) 0)))
    (star-tabs--auto-activate-file-extension-filters-on-buffer-count (star-tabs-get-collection-prop-value
								      :file-extension-filter-threshold)))
  ;; Add and remove file extension filters in the current collection, based on the file extension of currently open buffers.
  (let ((extensions-updated-p nil))
    (if star-tabs-add-file-extension-filters
	(setq extensions-updated-p (or (star-tabs--update-file-extension-filters t)
				       extensions-updated-p))
      ;; Remove all automatically set file extension filters in case none of the two conditions 
      ;; above are met.
      (when star-tabs-file-extension-filter-names
	(setq extensions-updated-p (or (star-tabs--remove-file-extension-filters) extensions-updated-p))))
    (when extensions-updated-p
      (when star-tabs-debug-messages
	(message "STAR-TABS: File Extension List/Filters Updated"))
      (unless inhibit-hook
	(run-hooks 'star-tabs-collection-property-change-hook))
      (unless inhibit-refresh
	(star-tabs--set-header-line (star-tabs-get-active-group-buffers) 'save-scroll)))
    extensions-updated-p))

(defun star-tabs-on-collection-property-change (collection-name)
  "Run when a collection property changes."
  ;; TODO: Add collection-name param 
  (when star-tabs-debug-messages
    (message "STAR-TABS: Collection Property Changed"))
  (star-tabs--add-and-remove-file-extension-filters t t) ; File extension filter groups will only be added if set to do so.
  (star-tabs--filter-all-buffers)
  (let ((filters (star-tabs-get-filter-names collection-name)))
    (dolist (filter filters)
    (star-tabs--update-tabs (star-tabs-get-group-buffers filter collection-name) filter collection-name)))
  ;;(star-tabs--recache-tabs (star-tabs-get-active-group-buffers) t)
  (star-tabs--set-header-line (star-tabs-get-active-group-buffers) 'save-scroll))


;; Functions to run when enabling/disabling Star Tabs.

(defun star-tabs-init ()
  "Run when Star Tabs first loads"
  (when star-tabs-debug-messages
    (message "STAR-TABS: Star Tabs initializing...")))

(defun star-tabs-on-disable-tab-bar ()
  "Run when Star Tabs Mode goes from enabled to disabled."
  (when star-tabs-debug-messages
    (message "STAR-TABS: Star Tabs disabled")))


;; Misc. functions to run with hooks.

(defun star-tabs-on-tab-move ()
  "Run when a tab changes position in the tab bar."
  (when star-tabs-debug-messages
    (message "STAR-TABS: Tab moved"))
  ;;(star-tabs--update-tabs (star-tabs-get-active-group-buffers))
  (star-tabs--recache-tabs (star-tabs-get-active-group-buffers) nil)
  (star-tabs--set-header-line (star-tabs-get-active-group-buffers) 'scroll-to-current-buffer))

(defun star-tabs-on-timer-start ()
  "Run when a Star Tabs timer starts."
  (when star-tabs-debug-messages
    (message "STAR-TABS: Star Tabs Timer Started")))

(defun star-tabs-on-timer-end ()
  "Run when a Star Tabs timer ends."
  (when star-tabs-debug-messages
    (message "STAR-TABS: Star Tabs Timer Started")))


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
	       (star-tabs-cycle-filters))
       ;; Make 'default' the current group when there are no buffers, if it exists,
       ;; since it is a good starting point.
       (unless star-tabs-active-buffers
         (star-tabs-switch-to-filter 'default (star-tabs-active-collection-name))))))


;; Hooks
(add-hook 'star-tabs-move-tab-hook #'star-tabs-on-tab-move)
(add-hook 'star-tabs-collection-property-change-hook #'star-tabs-on-collection-property-change)
;; (add-hook 'star-tabs-disable-tab-bar-hook #'star-tabs-on-disable-tab-bar)
;; (add-hook 'star-tabs-init-hook #'star-tabs-init)
(add-hook 'star-tabs-buffer-list-update-hook #'star-tabs-on-buffer-list-update t)
;; (add-hook 'star-tabs-timer-end-hook #'star-tabs-on-timer-end)
;; (add-hook 'star-tabs-timer-start-hook #'star-tabs-on-timer-start)
(add-hook 'star-tabs-collection-switch-hook #'star-tabs-on-collection-switch)
(add-hook 'star-tabs-filter-switch-hook #'star-tabs-on-filter-switch) 
(add-hook 'star-tabs-buffer-switch-hook #'star-tabs-on-buffer-switch)


(defun star-tabs--parse-xpm-values (xpm-data)
  "Return a list of values for width, height, ncolors and cpp of xpm image XPM-DATA."
  (let* ((xpm-values (nth 2 (split-string xpm-data "\n"))))
    (save-match-data
      (string-match "\\([0-9]+\\).*?\\([0-9]+\\).*?\\([0-9]+\\).*?\\([0-9]+\\)" xpm-values)
      (mapcar 'string-to-number (split-string (match-string 0 xpm-values))))))

(defun star-tabs--mirror-xpm (xpm-data)
  "Mirror xpm image XPM-DATA. 
Note: This function require that the xpm image data is formatted in a specific way:\n
The 3 first rows are:
1: /* XPM */ 
2: static char* <variable_name>[]= {
3: <Values>
The rows 4 to (+ 3 ncolors) are:
4 to (+ 3 ncolors): <Colors>
The remaining lines are: <Pixels>
except for the last line, which should be \"};\""
  (let* ((img-string (split-string xpm-data "\n"))
	 (xpm-values (star-tabs--parse-xpm-values xpm-data))
	 (height (nth 1 xpm-values))
	 (ncolors (nth 2 xpm-values))
	 (start-line 0) 
	 (end-line (+ (+ 3 ncolors) height))
	 (reverse-img ""))
    ;; XPM header, values, and color descriptions: 
    (while (> (+ 3 ncolors) start-line)
      (setq reverse-img (concat reverse-img
				(nth start-line img-string)
				"\n"))
      (setq start-line (1+ start-line)))
    ;; Pixels
    (while (> end-line start-line)
      (if (not (equal (- end-line start-line) 1))
	  (setq reverse-img (concat reverse-img
				    (substring (reverse (nth start-line img-string)) 1)
				    ",\n" ))
	(setq reverse-img (concat reverse-img
				  (reverse (nth start-line img-string))
				  "\n" )))
      (setq start-line (1+ start-line)))
    ;; Close the variable declaration
    (setq reverse-img (concat reverse-img"};"))
    reverse-img))

;; If bottom: Replicate the last line of pixels at the bottom until height is target-height.
;; If top: Replicate the first line of pixels at the top until height is target-height. 
;; If middle: Replicate the middle line in the middle until height is target-height
;; If Top and Bottom: Replicate top and bottom uniformly.
;; If Top Middle Bottom: Replicate top, middle and bottom uniformly.
(defun star-tabs--fill-xpm (xpm-data target-height &optional fill-direction)
  "Fill the bottom of xpm image XPM-DATA with rows of \".\" characters to make it height TARGET-HEIGHT."
  (or fill-direction (setq fill-direction 'bottom))
  (let* ((xpm-values (star-tabs--parse-xpm-values xpm-data))
	 (width (nth 0 xpm-values))
	 (height (nth 1 xpm-values))
	 (ncolors (nth 2 xpm-values))
	 (cpp (nth 3 xpm-values))
	 (new-values (format "\"%d %d %d %d\"," width (max height target-height) ncolors cpp))
	 (xpm-split (split-string xpm-data "\n"))
	 (xpm-header-values-colors "")
	 (xpm-header-values-colors (dotimes (iter (+ 3 ncolors) xpm-header-values-colors)
				     (setq xpm-header-values-colors (concat xpm-header-values-colors
									    (if (not (equal iter 2))
										(nth iter xpm-split)
									      new-values)
									    "\n"
									    ))))
	 (xpm-pixels-top-half "")
	 (xpm-pixels-top-half (dotimes (iter (/ height 2) xpm-pixels-top-half)
		       (setq xpm-pixels-top-half (concat xpm-pixels-top-half
						(nth (+ iter 3 ncolors) xpm-split)
						  "\n"))))
	 (xpm-pixels-bottom-half "")
	 (xpm-pixels-bottom-half (dotimes (iter (if (equal (mod height 2) 0)
						    (/ height 2)
						  (1+ (/ height 2)))
						xpm-pixels-bottom-half)
		       (setq xpm-pixels-bottom-half (concat xpm-pixels-bottom-half
						(nth (+ iter 3 ncolors (/ height 2)) xpm-split)
						(when (not (= (+ 1 iter 3 ncolors (/ height 2)) (+ 3 ncolors height)))
						  "\n")))))
	 (xpm-pixels "")
	 (xpm-pixels (dotimes (iter height xpm-pixels)
		       (setq xpm-pixels (concat xpm-pixels
						(nth (+ iter 3 ncolors) xpm-split)
						(when (not (= (+ 1 iter 3 ncolors ) (+ 3 ncolors height)))
						  "\n")))))
	 (fill-pixel ".")
	 (fill-pixels "")
	 (fill-pixels-top-row (nth (+ 3 ncolors) xpm-split))
	 (fill-pixels-top-full "")
	 (fill-pixels-top-full (format "%s"
					  (dotimes (num (- target-height height) fill-pixels-top-full)
					    (setq fill-pixels-top-full (concat fill-pixels-top-full
										  fill-pixels-top-row
										  "\n")))))
	 (fill-pixels-middle-row (nth (+ 3 ncolors (/ height 2)) xpm-split))
	 (fill-pixels-middle-full "")
	 (fill-pixels-middle-full (format "%s"
				       (dotimes (num (- target-height height) fill-pixels-middle-full)
					 (setq fill-pixels-middle-full (concat fill-pixels-middle-full
									    fill-pixels-middle-row
									    "\n")))))
	 (fill-pixels-bottom-row (nth (+ 2 ncolors height) xpm-split))
	 (fill-pixels-bottom-full "")
	 (fill-pixels-bottom-full (format "%s"
				   (dotimes (num (- target-height height) fill-pixels-bottom-full)
				     (setq fill-pixels-bottom-full (concat fill-pixels-bottom-full
								    fill-pixels-bottom-row
								    (unless (= (1+ num) (- target-height height))
								      ",")
								    "\n")))))
	 (fill-pixels-bottom-full (concat fill-pixels-bottom-full
				   "};")))
    ;; (message "Top half:\n%s" xpm-pixels-top-half)
    ;; (message "Bottom half:\n%s" (concat xpm-pixels-bottom-half "\n"))
    ;; ;; (message (format "Full: \n%s" xpm-pixels))
    ;; ;; (message "HVC: \n%s" xpm-header-values-colors)
    ;; (message "FULLPLUSNEWLINE: \n%s" (concat xpm-pixels "\n"))
    ;; ;; (message "FULL again: \n%s" (concat xpm-pixels "\n"))
    ;; ;; (message "Origin: \n%s" xpm-data)
    ;; ;; (message "Test bottom full: \n%s" (concat xpm-header-values-colors
    ;; ;; 	      (if (> target-height height)
    ;; ;; 		  (concat xpm-pixels ",\n")
    ;; ;; 		(concat xpm-pixels "\n"))
    ;; ;; 	      fill-pixels-bottom-full))
    ;; ;; (message "Test-Top: %s" fill-pixels-top-row)
    ;; ;; (message "Bottom: %s" fill-pixels-bottom-row)
    ;;  (message "Middle: %s" fill-pixels-middle-row)
    ;;  (message "Middle Full: \n%s"
    ;; 	      (concat xpm-header-values-colors
    ;; 		      xpm-pixels-top-half
    ;; 		      fill-pixels-middle-full
    ;; 		      xpm-pixels-bottom-half
    ;; 		      "\n};")
    ;; 	      )
    ;; (message "Row : %s" fill-pixels-row)
    (cond
     ((equal fill-direction 'top)
      (concat xpm-header-values-colors
	      fill-pixels-top-full
	      xpm-pixels
	      "\n};"))
     ((equal fill-direction 'middle)
      (concat xpm-header-values-colors
	      xpm-pixels-top-half
	      fill-pixels-middle-full
	      xpm-pixels-bottom-half
	      "\n};"))
     ((equal fill-direction 'bottom)
      (concat xpm-header-values-colors
	      (if (> target-height height)
		  (concat xpm-pixels ",\n")
		(concat xpm-pixels "\n"))
	      fill-pixels-bottom-full))
     (t
      (concat xpm-header-values-colors
	      (if (> target-height height)
		  (concat xpm-pixels ",\n")
		(concat xpm-pixels "\n"))
	      fill-pixels-bottom-full)))))

(defun star-tabs--vertical-bar (height color)
  (let* ((xpm-img-header (format "/* XPM */\nstatic char * test_xpm[]= {\n\"5 %s 1 1\",\n\"* c %s\",\n" height color))
	(xpm-img-pixels "\"*****\",\n")
	(xpm-img-pixels-last "\"*****\"\n};")
	(xpm-img xpm-img-header))
    (dotimes (_num (1- height))
      (setq xpm-img (concat xpm-img xpm-img-pixels)))
    (setq xpm-img (concat xpm-img xpm-img-pixels-last))
    (propertize " " 'display (create-image xpm-img 'xpm t :ascent 'center ))))

(defun star-tabs--create-image (xpm-data &optional image-face)
  "Create an image using XPM-DATA for use in the header line. 
If set, apply face IMAGE-FACE to the image."
  (propertize " "
	      'display (create-image xpm-data 'xpm t :ascent 'center)
	      'face image-face))

(defvar star-tabs-tab-border-round-selected ""
  "XPM data for rounded selected tab borders.")
(setq star-tabs-tab-border-round-selected (format "/* XPM */
static char * test_xpm[]=  {
\"10 10 2 1\",
\". c %s\",
\"* c %s\",
\"**********\",
\".*********\",
\"...*******\",
\"....******\",
\"......****\",
\".......***\",
\"........**\",
\"........**\",
\".........*\",
\"..........\"
};"  star-tabs-tab-bar-selected-background star-tabs-tab-bar-background))

(defvar star-tabs-tab-border-round-selected ""
  "XPM data for rounded non-selected tab borders.")
(setq star-tabs-tab-border-round-non-selected (format "/* XPM */
static char * test_xpm[]=  {
\"10 10 2 1\",
\". c %s\",
\"* c %s\",
\"**********\",
\".*********\",
\"...*******\",
\"....******\",
\"......****\",
\".......***\",
\"........**\",
\"........**\",
\".........*\",
\"..........\"
};"  star-tabs-tab-bar-non-selected-background star-tabs-tab-bar-background))

(defvar star-tabs-tab-border-slant-selected ""
  "XPM data for slanted selected tab borders.")
(setq star-tabs-tab-border-slant-selected (format "/* XPM */
static char * test_xpm[]=  {
\"10 10 2 1\",
\". c %s\",
\"* c %s\",
\"**********\",
\".*********\",
\"..********\",
\"...*******\",
\"....******\",
\".....*****\",
\"......****\",
\".......***\",
\"........**\",
\".........*\"
};"  star-tabs-tab-bar-selected-background star-tabs-tab-bar-background))

(defvar star-tabs-tab-border-slant-non-selected ""
  "XPM data for slanted non-selected tab borders.")
(setq star-tabs-tab-border-slant-non-selected (format "/* XPM */
static char * test_xpm[]=  {
\"10 10 2 1\",
\". c %s\",
\"* c %s\",
\"**********\",
\".*********\",
\"..********\",
\"...*******\",
\"....******\",
\".....*****\",
\"......****\",
\".......***\",
\"........**\",
\".........*\"
};"  star-tabs-tab-bar-non-selected-background star-tabs-tab-bar-background))


(provide 'star-tabs)

;;; star-tabs.el ends here




