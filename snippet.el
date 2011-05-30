;;; snippet.el -- insert snippets of text into a buffer

;; Copyright (C) 2005 Pete Kazmier

;; Version: 0.1
;; Author: Pete Kazmier

;; This file is not part of GNU Emacs, but it is distributed under
;; the same terms as GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 2, or (at your
;; option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING. If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Description:

;; A quick stab at providing a simple template facility like the one
;; present in TextMate (an OSX editor). The general idea is that a
;; snippet of text (called a template) is inserted into a buffer
;; (perhaps triggered by an abbrev), and while the point is within the
;; snippet, a special keymap is active to permit the user to cycle the
;; point to any of the defined fields (placeholders) within the
;; template via `snippet-next-field' and `snippet-prev-field'.

;; For example, the following template might be a useful to use while
;; editing HTML:

;; <a href="$$">$$</a>

;; This template might be useful for python developers. In this
;; example, reasonable defaults have been supplied:

;; for $${element} in $${sequence}:

;; When a template is inserted into a buffer (could be triggered by an
;; abbrev expansion, or simply bound to some key), point is moved to
;; the first field denoted by the "$$" characters (configurable via
;; `snippet-field-identifier'). The optional default for a field is
;; specified by the "{default}" (the delimiters are configurable via
;; `snippet-field-default-beg-char' and `snippet-field-defaul-end-char'.

;; If present, the default will be inserted and highlighted. The user
;; then has the option of accepting the default by simply tabbing over
;; to the next field (any other key bound to `snippet-next-field' in
;; `snippet-map' can be used). Alternatively, the user can start
;; typing their own value for the field which will cause the default
;; to be immediately replaced with the user's own input.

;; `snippet-next-field' (bound to <tab> by default) moves the point to
;; the next field. `snippet-prev-field' (bound to <S-tab> by default)
;; moves the point to the previous field. When the snippet has been
;; completed, the user simply tabs past the last field which causes
;; the snippet to revert to plain text in a buffer. The idea is that
;; snippets should get out of a user's way as soon as they have been
;; filled and completed.

;;; Usage:

;; Snippets are inserted with the `snippet-insert' function. This
;; function inserts the snippet into the current buffer. It expects a
;; single argument which is the template that is to be inserted. For
;; example:

;; (snippet-insert "for $${element} in $${sequence}:")

;; `snippet-insert' can be called interactively in which case the user
;; is prompted for the template to insert. This is hardly useful at
;; all unless you are testing the functionality of this code.

;; Snippets are much more effective when they are bound to expansions
;; for abbreviations. When binding a snippet to an abbreviation, it
;; is important that you disable the insertion of the character that
;; triggered the expansion (typically some form of whitespace). For
;; example, this is what you should NOT do:

;; (define-abbrev python-mode-abbrev-table ; abbrev table
;; "for" ; name
;; "" ; expansion
;; '(lambda () ; expansion hook
;; (snippet-insert
;; "for $${element} in $${sequence}:")))

;; The above example does not work as expected because after the
;; expansion hook is called, the snippet is inserted, and the point is
;; moved to the first field. The problem occurs because when the user
;; typed "f o r <Spc>", the "<Spc>" character is inserted after the
;; snippet has been inserted. The point happens to be located at the
;; first field and thus the "<Spc>" will delete any field default that
;; was present.

;; Fortunately, this is easy to fix. According to the documentation
;; for `define-abbrev', if the hook function is a symbol whose
;; `no-self-insert' property is non-nil, then hook can control whether
;; or not the insertion of the character that triggered the abbrev
;; expansion is inserted. `insert-snippet' returns non-nil and thus
;; the proper way of defining the abbrev is as follows:

;; (defun python-foo-expansion ()
;; (snippet-insert "for $${element} in $${sequence}:"))

;; (put 'python-foo-expansion 'no-self-insert t)

;; (define-abbrev python-mode-abbrev-table ; abbrev table
;; "for" ; name
;; "" ; expansion
;; 'python-foo-expansion) ; expansion hook

;; Unfortunately, this is a lot of work to define what should be a
;; simple abbrev. For convenience, this package provides a macro
;; `snippet-abbrev' that can be used with much less effort:

;; (snippet-abbrev python-mode-abbrev-table ; table
;; "for" ; name
;; "for $${element} in $${sequence}:") ; template

;; For even more convevience, when defining a lot of abbrevs in a
;; particular abbrev table, the package provides another macro
;; `snippet-with-abbrev-table':

;; (snippet-with-abbrev-table python-mode-abbrev-table
;; ("for" . "for $${element} in $${sequence}:")
;; ("im" . "import $$")
;; ("if" . "if $${True}:")
;; ("wh" . "while $${True}:"))

;; Be sure that the appropriate abbrev-table is loaded before using
;; the above otherwise you'll get an error. I use the above in my
;; python-mode-hook.

;;; Implementation Notes:

;; This is my first significant chunk of elisp code. I have very
;; little experience coding with elisp; however, I have tried to
;; document the code for anyone trying to follow along. Here are some
;; brief notes on the implementation.

;; When a snippet is inserted, the entire template of text has an
;; overlay applied. This overlay is referred to as the "bound"
;; overlay in the code. It is used to bold-face the snippet as well
;; as provide the keymap that is used while the point is located
;; within the snippet (so users can tab between fields). This overlay
;; is actually one character longer than the template. The reason is
;; to make sure that our local keymap is still in effect when a user
;; is typing in a field that happens to be at the end of the
;; template.

;; In addition, for each field (denoted by snippet-field-identifier),
;; an overlay is created. These overlays are used to provide the
;; highlighting of the default values, the location of where the point
;; should move when tab is pressed (the start of the overlay is used
;; for this purpose), as well as the hooks to delete the default value
;; if a user starts to type their own value (the modification hooks of
;; the overlay are used for this purpose).

;; Once the user has tabbed out of the snippet, all overlays are
;; deleted and the snippet then becomes normal text. Moving the
;; cursor back into the snippet has no affect (the snippet is not
;; activated again). The idea is that the snippet concept should get
;; out of the users way as quickly as possible.

;;; Code:

(require 'cl)

(defgroup snippet nil
"Insert a template with fields that con contain optional defaults."
:prefix "snippet-"
:group 'abbrev
:group 'convenience)

(defcustom snippet-bound-face 'bold
"*Face used for the body of the snippet."
:type 'face
:group 'snippet)

(defcustom snippet-field-face 'highlight
"*Face used for the fields' default values."
:type 'face
:group 'snippet)

(defcustom snippet-field-identifier "$$"
"*String used to identify field placeholders."
:type 'string
:group 'snippet)

(defcustom snippet-field-default-beg-char ?{
"*Character used to identify the start of a field's default value."
:type 'character
:group 'snippet)

(defcustom snippet-field-default-end-char ?}
"*Character used to identify the stop of a field's default value."
:type 'character
:group 'snippet)

(defvar snippet-map (make-sparse-keymap)
"Keymap used while the point is located within a snippet.")

;; Default key bindings
(define-key snippet-map (kbd "TAB") 'snippet-next-field)
(define-key snippet-map (kbd "<S-iso-lefttab>") 'snippet-prev-field)

(defstruct snippet
"Structure containing the overlays used to display a snippet.

The BOUND slot contains an overlay to bound the entire text of the
template. This overlay is used to provide a different face
configurable via `snippet-bound-face' as well as the keymap that
enables tabbing between fields.

The FIELDS slot contains a list of overlays used to indicate the
position of each field. In addition, if a field has a default, the
field overlay is used to provide a different face configurable via
`snippet-field-face'.

The FIELD-INDEX slot contains the current field where point has been
tabbed to. This can get out of sync if a user uses some other
mechanism to move between fields but I'm not in a rush to fix that at
the moment."
bound fields (field-index -1))

(defvar snippet nil
"Snippet in the current buffer.
There is no more than one snippet per buffer. This variable is buffer
local.")

(make-variable-buffer-local 'snippet)

(defun snippet-make-bound-overlay ()
"Create an overlay to bound a snippet.
Add the appropriate properties for the overlay to provide: a face used
to display the snippet, the keymap to use while within the snippet,
and the modification hooks to clean up the overlay in the event it is
deleted."
(let ((bound (make-overlay (point) (point) (current-buffer) nil nil)))
(overlay-put bound 'keymap snippet-map)
(overlay-put bound 'face snippet-bound-face)
(overlay-put bound 'modification-hooks '(snippet-bound-modified))
(overlay-put bound 'help-echo "TAB to move to next field")
bound))

(defun snippet-make-field-overlay ()
"Create an overlay for a field in a snippet.
Add the appropriate properties for the overlay to provide: a face used
to display a field's default value, and modification hooks to remove
the default text if the user starts typing."
(let ((field (make-overlay (point) (point) (current-buffer) nil nil)))
(overlay-put field 'face snippet-field-face)
(overlay-put field 'insert-in-front-hooks '(snippet-field-insert))
(overlay-put field 'insert-behind-hooks '(snippet-field-modified))
(overlay-put field 'modification-hooks '(snippet-field-modified))
field))

(defun snippet-bound-modified (bound after beg end &optional change)
"Ensure the overlay that bounds a snippet is cleaned up.
This modification hook is triggered when the overlay that bounds the
snippet is modified. It runs after the change has been made and
ensures that if the snippet has been deleted by the user, the
appropriate cleanup occurs."
;; This hook is not strictly required anymore as once the snippet
;; is deleted by the user, it can never expand again because of the
;; marker insertion policy specified when the overlays were
;; created. Earlier versions of this program had the bounded
;; overlay created with the REAR-ADVANCE arg of \\{make-overlay}}
;; set to `t'. In that case, it was necessary to delete the overlay
;; when it was shrunk to 0 is size.
(when (and after (= 0 (- (overlay-end bound) (overlay-start bound))))
(snippet-cleanup)))

(defun snippet-field-insert (field after beg end &optional change)
"Delete the field default value and remove modification hooks.
This insertion hook is triggered when a user starts to type when the
point is positioned at the beginning of a field (this occurs when the
user chooses to replace the field default). In this case, the hook
deletes the field default and then unregisters all hooks for this
field as they are not needed anymore (the highlighting should go
away). As a consequence, the field overlay is now of length 0 and it
is only used as a marker for cycling between fields."
(when (not after)
(delete-region (overlay-start field) (overlay-end field))
(dolist (hook '(modification-hooks
insert-in-front-hooks
insert-behind-hooks))
(overlay-put field hook nil))))

(defun snippet-field-modified (field after beg end &optional change)
"Shrink the field overlay and remove modifications hooks.
This modification hook is triggered when a user starts to type when
the point is positioned in the middle or at the end of a field (this
occurs when the user chooses to edit the field default). In this
case, the hook shrinks the field overlay to 0, and then unregisters
all hooks for this field as they are not needed anymore (the
highlighting should go away). After this hook is called, the field
overlay is only used as a marker for cycling between fields."
(when (not after)
(move-overlay field (overlay-start field) (overlay-start field))
(dolist (hook '(modification-hooks
insert-in-front-hooks
insert-behind-hooks))
(overlay-put field hook nil))))

(defun snippet-next-field ()
"Move point forward to the next field in the `snippet'.
If there are no more fields in the snippet, then point is moved to the
end of the snippet and the snippet reverts to normal text."
(interactive)
(incf (snippet-field-index snippet))
(let ((index (snippet-field-index snippet))
(bound (snippet-bound snippet))
(fields (snippet-fields snippet)))
(if (and fields (< index (length fields)))
(goto-char (overlay-start (elt fields index)))
(if (= (overlay-end bound) (point-max))
;; Special case in the event that the end of the snippet is
;; the very last thing in the buffer. We move point to the
;; end of the buffer.
(goto-char (point-max))
;; Normally, we move to the end of the overlay (which is
;; one character beyond the end of the template), then back
;; one character so the point lands immediately following the
;; last character of the template.
(goto-char (1- (overlay-end bound))))
(snippet-cleanup))))

(defun snippet-prev-field ()
"Move point backward to the previous field in the `snippet'.
If there are no more fields in the snippet, then point is moved to the
beginning of the snippet and the snippet reverts to normal text."
(interactive)
(decf (snippet-field-index snippet))
(let ((index (snippet-field-index snippet))
(bound (snippet-bound snippet))
(fields (snippet-fields snippet)))
(if (and fields (>= index 0))
(goto-char (overlay-start (elt fields index)))
(goto-char (overlay-start bound))
(snippet-cleanup))))

(defun snippet-cleanup ()
"Delete all overlays associated with `snippet'.
This effectively reverts the snippet to normal text in the buffer."
(when snippet
(when (snippet-bound snippet)
(delete-overlay (snippet-bound snippet)))
(dolist (field (snippet-fields snippet))
(delete-overlay field))
(setq snippet nil)))

(defun snippet-field-regexp ()
"Return a regexp that is used to search for fields within a template."
(let ((beg (char-to-string snippet-field-default-beg-char))
(end (char-to-string snippet-field-default-end-char)))
(concat (regexp-quote snippet-field-identifier)
"\\("
(regexp-quote beg)
"\\([^"
(regexp-quote end)
"]+\\)"
(regexp-quote end)
"\\)?")))

(defun snippet-insert (template)
"Insert a snippet into the current buffer at point.
TEMPLATE is a string that may optionally contain fields which are
specified by `snippet-field-identifier'. Fields may optionally also
include default values delimited by `snippet-field-default-beg-char'
and `snippet-field-default-end-char'.

For example, the following template specifies two fields which have
the default values of \"element\" and \"sequence\":

\"for $${element} in $${sequence}:\"

In the next example, only one field is specified and no default has
been provided:

\"import $$\"

This function may be called interactively, in which case, the TEMPLATE
is prompted for. However, users do not typically invoke this function
interactively, rather it is most often called as part of an abbrev
expansion. See `snippet-abbrev' and `snippet-with-abbrev-table' for
more information."
(interactive "sSnippet template: ")

;; Step 1: Ensure only one snippet exists at a time
(snippet-cleanup)

;; Step 2: Create a new snippet and add the overlay to bound the
;; template body. It should be noted that the bounded overlay is
;; sized to be one character larger than the template body text.
;; This enables our keymap to be active when a field happens to be
;; the last item in a template. We disable abbrev mode to prevent
;; our template from triggering another abbrev expansion (I do not
;; know if the use of `insert' will actually trigger abbrevs).
(let ((abbrev-mode nil))
(setq snippet (make-snippet :bound (snippet-make-bound-overlay)))
(let ((start (point)))
(insert template)
;; Not sure what to do about indentation. Perhaps if the
;; template string begins with ">", this might signal that we
;; should run `indent-region'. For now, leave disabled.
;; (indent-region start (point) nil)
(move-overlay (snippet-bound snippet) start (1+ (point))))

;; Step 3: Create field overlays for each field and insert any
;; default values for the field.
(goto-char (overlay-start (snippet-bound snippet)))
(while (re-search-forward (snippet-field-regexp)
(overlay-end (snippet-bound snippet))
t)
(let ((overlay (snippet-make-field-overlay))
(start (match-beginning 0)))
(push overlay (snippet-fields snippet))
(replace-match (if (match-beginning 2) "\\2" ""))
(move-overlay overlay start (point))))

;; These are reversed so they are in order of how they appeared in
;; the template as we index into this list when cycling field to
;; field.
(setf (snippet-fields snippet) (reverse (snippet-fields snippet))))

;; Step 4: Position the point at the first field or the end of the
;; template body if no fields are present.
(snippet-next-field))

(defvar snippet-count 0
"Count used to create snippet abbrev hook expansions.")

(defmacro snippet-abbrev (abbrev-table abbrev-name template)
"Establish an abbrev for a snippet template.
Set up an abbreviation called ABBREV-NAME in the ABBREV-TABLE that
expands into a snippet using the specified TEMPLATE string.

This macro facilitates the creation of a function for the expansion
hook to be used in `define-abbrev'. In addition, it also sets the
`no-self-insert' property on the function to prevent `abbrev-mode'
from inserting the character that triggered the expansion (typically
whitespace) which would otherwise interfere with the first field of a
snippet."
(let ((hook (intern (format "snippet-abbrev-%s" (incf snippet-count))))
(name (gensym)))
`(let ((,name ,abbrev-name))
(progn
(defun ,hook ()
(snippet-insert ,template))
(put ',hook 'no-self-insert t)
(put ',hook 'function-documentation
(format "Abbrev expansion hook for \"%s\"." ,name))
(define-abbrev ,abbrev-table ,name "" ',hook)))))

(defmacro snippet-with-abbrev-table (abbrev-table &rest snippet-alist)
"Establish a set of abbrevs for snippet templates.
Set up a series of snippet abbreviations in the ABBREV-TABLE. The
abbrevs are specified in SNIPPET-ALIST. For example:

(snippet-with-abbrev-table python-mode-abbrev-table
(\"for\" . \"for $${element} in $${sequence}:\")
(\"im\" . \"import $$\"))

See also `snippet-abbrev."
(let ((table (gensym)))
`(let ((,table ,abbrev-table))
(progn
,@(loop for (name . template) in snippet-alist
collect (list 'snippet-abbrev table name template))))))

(provide 'snippet)