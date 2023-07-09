# IMPORTANT
**This project is currently not maintained and might not support your version of Emacs. If you wish to make changes or contribute, feel free to fork this project, but use at your own risk.**
I originally created this for Emacs 26.1 but have since patched it to make it compatible with newer versions. I have not, however, tested it to make sure that all features work as originally intended.


# Table of Contents

1.  [Installation](#org246a96c)
    1.  [Prerequisites](#org3c77193)
    2.  [Example Configuration](#org1426fa4)
2.  [Introduction](#orgc610f5f)
3.  [Features](#orga326efe)
4.  [Usage](#org0a3e05e)
    1.  [Usage Example](#org22b5cd3)
    2.  [Commands](#org25b29a9)
5.  [CUSTOMIZATION](#orgeccfc21)
    1.  [Global Settings](#org19ee1ee)
        1.  [Colors](#org6e2d7ec)
        2.  [Size](#orgcabdb48)
        3.  [Dividers](#org0a9f138)
        4.  [Tab ASCII Icons](#org94fb238)
        5.  [Global Filter Settings](#org634b229)
    2.  [Collections](#org704f8fa)
        1.  [Collection Properties](#orgcbddc67)
    3.  [Filters](#org143318c)
6.  [Questions and Answers](#org175b144)
    1.  [There are unwanted/strange tabs in my tab bar. How do I hide them?](#org7c28bfe)
    2.  [How do I enable/disable groups for file extensions?](#orgb8ad31c)
    3.  [Is this a fork of an existing project?](#orgad82fa4)
    4.  [Is Star Tabs useful for someone with hundreds of open buffers?](#org3f689d5)
    5.  [What's the difference between a filter and a group?](#orgd8c98ec)
    6.  [How can I contribute to Star Tabs?](#orga810743)
7.  [Contribute](#org8f579eb)



<a id="org246a96c"></a>

# Installation

-   Clone repository:

`git clone --single-branch --branch master https://github.com/lobst4r/star-tabs.git`

-   Add star-tabs.el to your Emacs load path.
-   In your .emacs configuration file, add:

    (require 'star-tabs)
    
    (star-tabs-tab-bar-mode t)


<a id="org3c77193"></a>

## Prerequisites

-   Emacs 28+
-   OPTIONAL: [all-the-icons](https://github.com/domtronn/all-the-icons.el)


<a id="org1426fa4"></a>

## Example Configuration

    (require 'star-tabs)
    (star-tabs-tab-bar-mode t)
    
    ;; Create a collection
    (star-tabs-create-filter-collection
      :name "my-collection"
      :use t
      :enable-file-extension-filters nil 
      :file-extension-filter-threshold 15
      :hide-close-buttons t
      :display-filter-name nil)
    
    ;; Add filters
    (star-tabs-add-filter
      :name 'default
      :exclude '("^[[:space:]]" "^*.*\\*$" "^magit-" "^magit:")
      :collection-name 'my-collection)
    
    ;; Keybinds
    (define-key evil-normal-state-map (kbd "RET") 'star-tabs-switch-to-buffer)
    (define-key evil-normal-state-map (kbd "`") 'star-tabs-cycle-filters)
    (define-key evil-normal-state-map (kbd ", r") 'star-tabs-include-current-buffer-in-current-filter)
    (define-key evil-normal-state-map (kbd ", t") 'star-tabs-exclude-current-buffer-from-current-filter)


<a id="orgc610f5f"></a>

# Introduction

Star Tabs aims to be a highly customizable and functional tab bar solution for Emacs, with focus on efficient and fast buffer navigation using numbers to
identify tabs. In the normal case, switching to any buffer is only 2-4 keystrokes away (its true power is unleashed in evil-mode!), and tabs can easily be arranged and displayed to your needs. 


<a id="orga326efe"></a>

# Features

-   Group and filter buffers however you want using regular expressions (or just use the default configuration).
-   Identify tabs by numbers, and switch buffer to any buffer with ease.
-   Switch between groups/filters easily.
-   Customize the look and feel of the tab bar.
-   Optional icon support using [all-the-icons](https://github.com/domtronn/all-the-icons.el).
-   Full mouse support (if you really need it).
-   Add and exclude any buffer to/from any group/filter.


<a id="org0a3e05e"></a>

# Usage

Most navigation in Star Tabs can be done with only two commands: `star-tabs-cycle-filters` and `star-tabs-switch-to-buffer`.
Thus, it's probably a good idea to bind these two functions to easy-to-access keys, and as few keys as possible in case you use keychords.
I personally use `` ` `` for `star-tabs-cycle-filters` and `RET` for `star-tabs-switch-to-buffer` in evil normal mode.


<a id="org22b5cd3"></a>

## Usage Example

To switch buffer: 

1.  Find the tab that corresponds to the buffer you want to switch to.
2.  If the buffer is not in the current group, cycle groups/filters using `star-tabs-cycle-filters` until you find a group with the buffer.
3.  Switch buffer: 
    -   For non-evil-mode users, press `C-u` followed by the number of the tab, and then the key to which `star-tabs-switch-to-buffer` is bound to.
    -   For evil-mode users, type the number of the tab, and then the key to which `star-tabs-switch-to-buffer` is bound to.
4.  You have now switched buffer!


<a id="org25b29a9"></a>

## Commands

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="org-left">Command</th>
<th scope="col" class="org-left">Description</th>
</tr>
</thead>

<tbody>
<tr>
<td class="org-left">`star-tabs-cycle-filters`</td>
<td class="org-left">Cycle through groups/filters</td>
</tr>


<tr>
<td class="org-left">`star-tabs-switch-to-buffer`</td>
<td class="org-left">Switch to buffer associated with tab N (N is the prefix argument that follows command `C-u`)</td>
</tr>


<tr>
<td class="org-left">`star-tabs-find-active-filter`</td>
<td class="org-left">Find and display a filter for the currently active buffer, if such filter exists in the current collection.</td>
</tr>


<tr>
<td class="org-left">`star-tabs-include-current-buffer-in-current-filter`</td>
<td class="org-left">Always display current buffer in the currently active group/filter</td>
</tr>


<tr>
<td class="org-left">`star-tabs-exclude-current-buffer-from-current-filter`</td>
<td class="org-left">Hide current buffer in the currently active group/filter</td>
</tr>


<tr>
<td class="org-left">`star-tabs-active-filter-name`</td>
<td class="org-left">Output the active group/filter name as a message</td>
</tr>


<tr>
<td class="org-left">`star-tabs-clear-cached-buffers`</td>
<td class="org-left">Clear the cached filtered buffers. Use this command after making changes to the global filter.</td>
</tr>


<tr>
<td class="org-left">`star-tabs-cycle-filter-collections`</td>
<td class="org-left">Cycle through group/filter collections.</td>
</tr>
</tbody>
</table>


<a id="orgeccfc21"></a>

# CUSTOMIZATION


<a id="org19ee1ee"></a>

## Global Settings

You can change the visual aspects of Star Tabs by altering these global settings.
Most functional settings are done on a collection-level scope, but there are still some functional settings that either must or can be set globally.
Most values shown in the examples below are default values.

**IMPORTANT: These commands should be added to your .emacs configuration file, since you will need to restart Emacs for most of the visual changes to take effect.**


<a id="org6e2d7ec"></a>

### Colors

    ;; Foreground color for tab bar filter name.
    (setq star-tabs-tab-bar-filter-name-foreground "#ef21b3")
    
    ;; Background color for selected tab.
    (setq star-tabs-tab-bar-selected-background "#202020")
    
    ;; Foreground color for selected tab.
    (setq star-tabs-tab-bar-selected-foreground "#a3c9e7")
    
    ;; Background color for non-selected tabs.
    (setq star-tabs-tab-bar-non-selected-background "#262626")
    
    ;; Foreground color for non-selected tabs.
    (setq star-tabs-tab-bar-non-selected-foreground "#e1e1e1")


<a id="orgcabdb48"></a>

### Size

    ;; Height of the tab bar.
    (setq star-tabs-tab-bar-height 220)
    
    ;; Text height for tabs.
    (setq star-tabs-tab-bar-text-height 150)


<a id="org0a9f138"></a>

### Dividers

    ;; Space used to the left of the tab bar.
    (setq star-tabs-left-margin "  ")
    
    ;; Space used to the right of the tab bar. Deprecated?
    (setq star-tabs-right-margin " ")
    
    ;; Tab bar divider that separates tabs.
    (setq star-tabs-tab-separator " ")
    
    ;; Tab bar divider that separates the buffer number and buffer name in a tab.
    (setq star-tabs-number-name-separator " ")
    
    ;; Tab bar divider that separates the buffer name and modified icon in a tab.
    (setq star-tabs-name-modified-icon-separator " ")
    
    ;; Tab bar divider that separates the modified icon and close button in a tab.
    (setq star-tabs-modified-icon-close-button-separator " ")
    
    ;; Tab bar divider that separates the name of the active filter group and the first tab.
    (setq  star-tabs-filter-name-number-separator "   ")


<a id="org94fb238"></a>

### Tab ASCII Icons

    ;; Tab 'icon' for modified buffers.
    (setq star-tabs-modified-buffer-icon "*")
    
    ;; Tab 'icon' for unmodified buffers.
    (setq star-tabs-unmodified-buffer-icon "+")
    
    ;; Tab 'icon' for the tab close button.
    (setq star-tabs-close-buffer-icon "x") 


<a id="org634b229"></a>

### Global Filter Settings

    ;; List of buffer name prefixes to be included globally. Buffers filtered this way will be cached and ignored
    ;; for all future searches. As such, global filtering may increase performance, and
    ;; should (and should only!) be applied to buffers that you really don't care about.
    ;; Buffers with the space prefix (\" \") are automatically filtered before this filter is applied, and thus cannot  
    ;; be included.
    ;; This filter is applied before star-tabs-global-exclusion-prefix-filter.
    (setq star-tabs-global-inclusion-prefix-filter nil)
    
    ;; List of buffer name prefixes to be excluded globally. Buffers filtered this way will be cached and ignored
    ;; for all future searches. As such, global filtering may increase performance, and
    ;; should (and should only!) be applied to buffers that you really don't care about.
    ;; Buffers with the space prefix (\" \") are automatically filtered before this filter is applied, and thus need not
    ;; be added to this list.
    ;; This filter is applied after star-tabs-global-inclusion-prefix-filter.
    (setq star-tabs-global-exclusion-prefix-filter '("magit-" "magit:" "*Help" "*WoM")


<a id="org704f8fa"></a>

## Collections

A collection is a bunch (a group, if you will) of groups/filters. Most customization in Star Tabs is done by setting the properties of a collection.
There is no hard limit on how many collections you can create, but realistically you probably won't be using more than one or two for a project. 

In order to create a filter, run this code, or add it to your .emacs configuration file:

    (star-tabs-create-filter-collection
      :name "my-collection"
      :use t
      :enable-file-extension-filters t 
      :file-extension-filter-threshold 0
      :hide-close-buttons t
      :display-filter-name t)


<a id="orgcbddc67"></a>

### Collection Properties

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="org-left">Property</th>
<th scope="col" class="org-left">Description</th>
</tr>
</thead>

<tbody>
<tr>
<td class="org-left">:name (string)</td>
<td class="org-left">The name of the collection</td>
</tr>


<tr>
<td class="org-left">:enable-file-extension-filters (bool)</td>
<td class="org-left">If `t`, add file extension filters to the collection</td>
</tr>


<tr>
<td class="org-left">:file-extension-filter-threshold (int)</td>
<td class="org-left">If greater than `0`, and if `:enable-file-extension-filters` is `nil`, add file extension filters to the collection if the total number of real buffers reaches or exceeds the value of the property.</td>
</tr>


<tr>
<td class="org-left">:hide-close-buttons (bool)</td>
<td class="org-left">If `non-nil`, hide the tab close button icons.</td>
</tr>


<tr>
<td class="org-left">:display-filter-name (bool)</td>
<td class="org-left">If `non-nil`, always display the name of the filter/group left of the tabs in the tab bar. Otherwise, only display the filter/group name temporarily when switching filters/groups</td>
</tr>


<tr>
<td class="org-left">:use (bool)</td>
<td class="org-left">If `non-nil`, switch to the collection upon creation.</td>
</tr>
</tbody>
</table>


<a id="org143318c"></a>

## Filters

A filter is a list, or multiple lists, of regular expressions used to include or exclude (or both include and exclude) buffers with names that match the regular expressions.
In case both `:include` and `:exclude` are set, first include buffers using the regular expressions from `:include`, then from those buffers, exclude buffers using the list from `:exclude`

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="org-left">Property</th>
<th scope="col" class="org-left">Description</th>
</tr>
</thead>

<tbody>
<tr>
<td class="org-left">:name (symbol&#x2026;TODO: change to string?)</td>
<td class="org-left">Name of the filter.</td>
</tr>


<tr>
<td class="org-left">:exclude  (list of regexps)</td>
<td class="org-left">List of regular expressions. Any buffer with a name matched by a regexp in this list will be excluded from the group.</td>
</tr>


<tr>
<td class="org-left">:include (list of regexps)</td>
<td class="org-left">List of regular expressions. Any buffer with a name matched by a regexp in this list will be included in the group.</td>
</tr>


<tr>
<td class="org-left">:always-include (regexp)</td>
<td class="org-left">Buffers matching this regular expression will always be included in the group, even if they were excluded by the list specified in `:exclude`</td>
</tr>


<tr>
<td class="org-left">:collection-name (symbol&#x2026;TODO: change to string?)</td>
<td class="org-left">The name of the collection the filter should be added to. If not set, it defaults to `(star-tabs-active-filter-collection-name)`</td>
</tr>
</tbody>
</table>


<a id="org175b144"></a>

# Questions and Answers


<a id="org7c28bfe"></a>

## There are unwanted/strange tabs in my tab bar. How do I hide them?

First, make sure you are in the correct tab group/filter. You can see the name of the currently active filter using command:

    M-x star-tabs-active-filter-name

If you're in the wrong group/filter, cycle filters using the following command until you find the correct filter:

    M-x star-tabs-cycle-filters

If you're in the correct group/filter and you want to hide a tab, open the buffer of the tab you want to hide and run the command:

    M-x star-tabs-exclude-current-buffer-from-current-filter

This will hide the buffer from the current group/filter.

Alternatively you can run the following elisp command, specifying the buffer name and filter name yourself:

    (star-tabs-exclude-from-filter (get-buffer buffer-name) filter-name)


<a id="orgb8ad31c"></a>

## How do I enable/disable groups for file extensions?

To add groups for file extensions for the current collection, run the following code, or add it to your emacs configuration file:

    (star-tabs-set-filter-collection-prop-value :enable-file-extension-filters t)

To remove groups for file extensions for the current collection, run the following code, or add it to your emacs configuration file:

    (star-tabs-set-filter-collection-prop-value :enable-file-extension-filters nil)

Alternatively, you can enable file extension filters only when the total number of real buffers reaches or exceeds a certain threshold.
This can be useful if you want as few groups as possible when you don't have a lot of active buffers, but want to mitigate some of the disorganization
that might follow a growing number of buffers. To do this, run the following code or add it to your emacs configuration file:

    (star-tabs-set-filter-collection-prop-value :enable-file-extension-filters nil) ; This must be nil when using threshold.
    (star-tabs-set-filter-collection-prop-value :file-extension-filter-threshold 15)

If you have disabled file extension filters by setting the property `:enable-file-extension-filters` to `nil`, but they are still showing,
make sure `:file-extension-filter-threshold` is set to `0` as well.


<a id="orgad82fa4"></a>

## Is this a fork of an existing project?

No. Although there are other good projects that accomplish similar things, I chose to start from scratch because this is a relatively small project and
I needed something to familiarize myself more with elisp.


<a id="org3f689d5"></a>

## Is Star Tabs useful for someone with hundreds of open buffers?

Potentially. Tabs become less efficient and less useful the more there are, so you probably want to minimize the number of tabs and groups.
Even though you have hundreds of open buffers, you can customize Star Tabs to only show the ones you want, and in which groups you want using filters,
so you might only end up with one or two groups with just a handful of tabs in each. The possibilities are endless!

That being said, Star Tabs is not a complete solution that is going work efficiently in all cases for everybody.


<a id="orgd8c98ec"></a>

## What's the difference between a filter and a group?

I use the terms filters and groups somewhat interchangeably when talking about a group of tabs. At the current state of development, all groups
of tabs are created and defined using filters, and that's why I refer to them both as filters and groups, or groups/filters. In the future,
the distinction will be more clear (and there should then be no need for this question).


<a id="orga810743"></a>

## How can I contribute to Star Tabs?

We all customize Emacs to our own needs and preferences. Star Tabs was created mainly for my own personal use in mind and, although care has been taken to ensure compatability with other people's configurations and styles, there are undoubtedly things you would like done differently.
As such, I'd love to know about any bugs and compatability issues you might find, as well as things - big and small - that could be improved.
To learn about how you can help improve Star Tabs, please refer to the [Contribute](#org8f579eb) section. 


<a id="org8f579eb"></a>

# Contribute

-   If you have any ideas or suggestions on how to improve Star Tabs, don't hesitate to let me know (either through email or by raising an issue on Github).
-   If you find a bug, file a report by raising an issue on Github.
-   In case you want to contribute with code, please fork the develop branch and create a pull request.

