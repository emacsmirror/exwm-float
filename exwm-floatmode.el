;;; exwm-floatmode.el --- Convenient modes and bindings for floating EXWM frames -*- lexical-binding: t  -*-

;; Copyright (C) 2020 Mehmet Tekman <mtekman89@gmail.com>

;; Author: Mehmet Tekman
;; URL: https://gitlab.com/mtekman/exwm-floatmode.el
;; Keywords: outlines
;; Package-Requires: ((emacs "25.1") (xelb "0.18") (exwm "0.24") (popwin "1.0.2"))
;; Version: 0.3

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;;; Commentary:

;; This package adds a minor-mode and some convenience functions to
;; easily move picture-in-picture frames around the screen and to
;; toggle playing and pausing video windows.

;; TODO:
;;
;; Window Configs:
;; * Setting window position saves (restoring now works)
;; * Refreshing the minor-mode map
;;
;; Multiple floating windows
;; * Currently, all the --get functions return one floating window when multiple could be possible.

;;; Code:
(require 'xcb)
(require 'exwm-core)
(require 'exwm-input)
(require 'exwm-floating)
(require 'exwm-workspace)
(require 'exwm-manage)
(require 'popwin)

(defgroup exwm-floatmode nil
  "Customization group for picture-in-picture."
  :group 'exwm-floating)

(defcustom exwm-floatmode-modify-amount
  '(:move-slow 20 :move-fast 100 :resize 50)
  "Incremental pixel amounts to MOVE-SLOW, MOVE-FAST or RESIZE the floating frame for the minor mode."
  :type 'plist
  :group 'exwm-floatmode)

(defcustom exwm-floatmode-frame-defaults
  '((:title nil ;;"Picture-in-Picture"
            :geometry '(x 0.6 y 0.05 width 600 height 500)
            :decoration  '(floating-mode-line nil
                                              tiling-mode-line nil
                                              floating-header-line nil
                                              tiling-header-line nil
                                              char-mode nil)))
  "Default window geometries and decorations for the spawned floating window.
Completely overrides the ``exwm-manage-configurations'' custom variable.

Each config will be applied to the matching floating window with TITLE.
If TITLE is nil, then it matches all.  Note that specifying nil for certain
options is not the same as leaving them out.  See ``exwm-manage-configurations''
for more information."
  :type 'list ;; TODO: Specify the type format better, see the above variable for more info.
  :group 'exwm-floatmode)

(defcustom exwm-floatmode-border
  '(:stationary ("navy" . 1) :moving ("maroon" . 3))
  "Floating border and width when stationary and during move-mode."
  :type 'plist
  :group 'exwm-floatmode)

(defcustom exwm-floatmode-custom-modes
  '(;; Move window slow
    (:title nil :common-fn exwm-floatmode-move-direction
            :keyargs ((\S-left . ('left))
                      (\S-right . ('right))
                      (\S-up .  ('up))
                      (\S-down . ('down))))
    ;; Move window fast
    (:title nil :common-fn exwm-floatmode-move-direction
            :keyargs ((\S-\M-left . ('left t))
                      (\S-\M-right . ('right t))
                      (\S-\M-up . ('up t))
                      (\S-\M-down . ('down t))))
    ;; Resize window
    (:title nil :common-fn exwm-floatmode-resize-delta
            :keyargs ((\M-left . ('dec nil))
                      (\M-right . ('inc nil))
                      (\M-up . (nil 'dec))
                      (\M-down . (nil 'inc))))
    ;; Common controls
    (:title nil :keyargs (;; (?s . (call-interactively #'exwm-floatmode-position-save))
                          (?q . (exwm-floatmode--inner-mode-exit))
                          (return . (exwm-floatmode--inner-mode-exit))))
    ;; Video-specific controls
    (:title "Picture-in-Picture" :common-fn exwm-floatmode--send-key
            :keyargs (left right up down ? ))
    (:title "Picture-in-Picture" :keyargs ((\S-? . (exwm-floatmode-forcetoggle-video)))))
  "A keymap list, loaded when TITLE matches ``exwm-title'' for the float window.

If TITLE is nil, then the mode is enabled on all floating windows.
The bindings are set by binding the car of each KEYARGS to the COMMON-FN plus the cdr of each KEYARGS.

e.g.1 (:title \"Foo\" :common-fn #'fun1 :keyargs '((a 9)))
         becomes (([a] . (fun1 a))
                  ([9] . (fun1 9)))

e.g.2 (:title \"Bar\" :common-fn #'fun2 :keyargs '((a . (b 22)) (9 . (123 m)) (left . (1 1))))
         becomes (([a] . (fun2 b 22))
                  ([9] . (fun2 123 m))
                  ([left] . (fun2 1 1)))

e.g.3 (:title \"Baz\" :common-fn nil :keyargs '((a . (fun1 1 22)) (b . (fun2 f g))))
         becomes (([a] . (fun1 1 22))
                  ([b] . (fun2 f g)))

Keys are either literal characters (e.g. ? for Space, ?f for 'f', etc) or keysyms found in ``xcb-keysyms.el''."
  :type 'list
  :group 'exwm-floatmode)

(defcustom exwm-floatmode-position-configs
  '((:name "NW" :key "1" :title nil :x 0 :y 0 :width 0.25 :height 0.25)
    (:name "NE" :key "2" :title nil :x -0.25 :y 0 :width 0.25 :height 0.25)
    (:name "SW" :key "3" :title nil :x 0 :y -0.25 :width 0.25 :height 0.25)
    (:name "SE" :key "4" :title nil :x -0.25 :y -0.25 :width 0.25 :height 0.25)
    (:name "Center" :key "5" :title nil :x 0.25 :y 0.25 :width 0.5 :height 0.5)
    (:name "Hide" :key "h" :title nil :x 0.5 :y -1 :width 1 :height 1))
  "List of ``(:name N :key K :title T :x X :y Y :width W :height H))'' elements, where K denotes the keyboard sequence used to place the floating window matching TITLE to position X and Y and resizing it to W and H.  All positions can be fractional (denoting proportion of screen space) or integers (denoting absolute pixels), and if negative are treated as offsets from the screen boundary.  If TITLE is nil, then apply to the first floating window.  The name N is ignored."
  :type 'list
  :group 'exwm-floatmode)

(defun exwm-floatmode--frame-geometry ()
  "Get geometry of current frame."
  (with-slots (x y width height)
      (xcb:+request-unchecked+reply exwm--connection
          (make-instance
           'xcb:GetGeometry :drawable
           (frame-parameter exwm--floating-frame 'exwm-container)))
    (list :x x :y y :width width :height height)))

;; --- BEGIN: Position Saving Functions ---
;;
;; These are disabled for now due to the issues with refreshing the keymap
;;
;; (defvar exwm-floatmode-position-file
;;   (concat (file-name-as-directory user-emacs-directory) "float_positions.el")
;;   "File to store floating video positions.")
;;
;; (defun exwm-floatmode-position-save (hotkey)
;;   "Save the current floating window position to a HOTKEY."
;;   (interactive (list (read-key-sequence "Hotkey: ")))
;;   (let* ((geometry (exwm-floatmode--do-floatfunc-and-restore
;;                   (lambda (cwin fwin)
;;                     (exwm-floatmode--frame-geometry))))
;;          (posinfo (append (list :key hotkey :title exwm-title) geometry)))
;;     (message "Binding: %s" posinfo)
;;     (exwm-floatmode--position-store posinfo)))
;;     ;;(exwm-floatmode--refresh-minor-mode)))

;; (defun exwm-floatmode--position-store (posinfo)
;;   "Store the following POSINFO into the
;; ``exwm-floatmode-position-configs'' variable, and sync with the
;; ``exwm-floatmode-position-file''."
;;   (pushnew posinfo exwm-floatmode-position-configs)
;;   (if exwm-floatmode-position-configs
;;       (with-temp-buffer
;;         (insert "(")
;;         (dolist (elem exwm-floatmode-position-configs)
;;           (let ((name (plist-get elem :name))
;;                 (key (plist-get elem :key)) (title (plist-get elem :title))
;;                 (x (plist-get elem :x)) (y (plist-get elem :y))
;;                 (w (plist-get elem :width)) (h (plist-get elem :height)))
;;             (if (eq 'string (type-of name)) (setq name (concat "\"" name "\"")))
;;             (if (eq 'string (type-of key)) (setq key (concat "\"" key "\"")))
;;             (if (eq 'string (type-of title)) (setq title (concat "\"" title "\"")))
;;             ;; Probably an easier way to ensure that values which have quotes are quoted, but eh.
;;             (insert (format "(:name %s :key %s :title %s :x %s :y %s :width %s :height %s)\n "
;;                             name key title x y w h))))
;;         (search-backward ")")
;;         (insert ")")
;;         (write-file exwm-floatmode-position-file))))

;; (defun exwm-floatmode--position-restore ()
;;   "Set the ``exwm-floatmode-position-configs'' variable from the contents of the ``exwm-floatmode-position-file'' and return it (to be fed directly into ``exwm-floatmode--position-expandbindings'')."
;;   (if exwm-floatmode-position-file
;;       (with-temp-buffer
;;         (unless (file-exists-p exwm-floatmode-position-file)
;;           (with-temp-buffer
;;             (insert "")
;;             (write-file exwm-floatmode-position-file)))
;;         (insert-file-contents exwm-floatmode-position-file)
;;         (let ((tmpvar (read-from-string (let ((it (buffer-string))) (if (equal it "") "nil" it)))))
;;           (if (car tmpvar)
;;               (setq exwm-floatmode-position-configs (car tmpvar))
;;             (user-error "Cannot parse"))))
;;     (user-error "``exwm-floatmode-position-file'' not set")))
;;
;; --- END: Position Saving Functions ---

(defun exwm-floatmode--position-expandbindings (position-config map)
  "Expand the bindings from POSITION-CONFIG (read from ``exwm-floatmode--position-restore'' into the keymap MAP, and check for conflicts."
  (dolist (binding position-config)
    (let* ((key (plist-get binding :key))
           (existing-binding (describe-key-briefly (kbd key)))
           (not-bound (or (string-match " undefined"  existing-binding)
                              (string-match " self-insert-command"  existing-binding))))
      (if not-bound
          (let* ((title (plist-get binding :title))
                 (x (plist-get binding :x)) (y (plist-get binding :y))
                 (w (plist-get binding :width)) (h (plist-get binding :height))
                 (func `(lambda () (interactive)
                          (exwm-floatmode-move ,x ,y ,w ,h ,title)
                          (message "%s" binding))))
            (define-key map key func))
        (user-error "Key: %s is already set" key)))))

(defun exwm-floatmode--convert2keymap (entry &optional map)
  "Convert an ENTRY in ``exwm-floatmode-custom-modes'' and to keymap MAP."
  (let ((title (plist-get entry :title))
        (commonfn (plist-get entry :common-fn))
        (keyargs (plist-get entry :keyargs))
        (map (or map (make-sparse-keymap))))
    (dolist (it keyargs)
      (let* ((key (if (eq (type-of it) 'cons) (car it) it))
             (args (if (eq (type-of it) 'cons) (cdr it)))
             (func (if commonfn
                       (if (not args) ;; only key
                           `(,commonfn ',key)
                         `(,commonfn ,@args))
                     ;; no initial function
                     (if (not args)
                         `(',key)
                       `(,@args))))
             ;; TODO: Find a cleaner way to write the above
             (titlefunc `(lambda () (interactive)
                           (if (or (not ,title) ;; no title, or a direct match
                                   (string= (with-current-buffer
                                                (exwm-floatmode--get-floating-buffer)
                                              exwm-title)
                                            ,title))
                               ,func)))
             (press (cond ((eq key 'SPC) `[? ])
                          (t `[,key]))))
        (define-key map press titlefunc)))
    map))

;; C-M-x to redefine this
;;(unintern 'exwm-floatmode-innermode)
(define-minor-mode exwm-floatmode-innermode
  "The minor mode for manipulating the exwm floating frame. Works only when called from non-floating frame, and it should therefore only be called from the ``exwm-floatmode-minor-mode'' function, which ensures this. NEVER CALL THIS MODE DIRECTLY"
  :init-value nil
  :lighter " FloatMode"
  :keymap (let ((map (make-sparse-keymap)))
            ;; Load user-mode bindings
            (dolist (entry exwm-floatmode-custom-modes)
              (exwm-floatmode--convert2keymap entry map))
            ;; Load user-position bindings
            (exwm-floatmode--position-expandbindings
             exwm-floatmode-position-configs map)
            ;; (exwm-floatmode--position-restore) map)
            map)
  ;; init
  (let ((colwid (plist-get exwm-floatmode-border
                           (if exwm-floatmode-innermode :moving :stationary))))
    (customize-set-variable 'exwm-floating-border-color (car colwid))
    (customize-set-variable 'exwm-floating-border-width (cdr colwid))))

(defun exwm-floatmode--refresh-minor-mode ()
  "Refresh the minor mode so that the new bindings from ``exwm-floatmode-position-configs'' take effect."
  ;;(exwm-floatmode-minor-mode -1)
  ;;(exwm-floatmode-minor-mode 1)
  (exwm-floatmode--inner-mode-exit)
  (exwm-floatmode-minor-mode))


(defun exwm-floatmode--send-key (keyseq)
  "Send KEYSEQ to floating window."
  (exwm-floatmode--do-floatfunc-and-restore
   (lambda (c f) (ignore c f)(exwm-input--fake-key keyseq))))

(defun exwm-floatmode--inner-mode-exit ()
  "Functions to run on move-mode exit.  Hooked to ``exwm-floating-exit-hook''."
  (remove-hook 'quit-window-hook #'exwm-floatmode--inner-mode-exit)
  (remove-hook 'next-error-hook #'exwm-floatmode--inner-mode-exit)
  (when (get-buffer "EXWM FloatMode")
    (exwm-floatmode-innermode -1)
    (kill-buffer "EXWM FloatMode")))

(defvar exwm-floatmode--prebuffer nil
  "Window before minor-mode was called, to be restored on exit.")

;;;###autoload
(defun exwm-floatmode-minor-mode (&optional junk)
  "Parent caller for `function `exwm-floatmode-innermode''.
Selects the floating window and sets the minor mode to STATE, 1
for on, anything else for off.  Event JUNK is discarded."
  (interactive)
  (ignore junk)
  (when (exwm-floatmode--get-floating-frame)
    (select-frame exwm-workspace--current) ;; grab workspace, not floating win
    (let* ((floater (get-buffer-create "EXWM FloatMode"))
           (newwin (popwin:popup-buffer floater
                                        :height 2 :position 'bottom
                                        :dedicated t :stick t :tail t)))
      (ignore newwin)
      (with-current-buffer "EXWM FloatMode"
        (add-hook 'quit-window-hook #'exwm-floatmode--inner-mode-exit)
        (add-hook 'next-error-hook #'exwm-floatmode--inner-mode-exit)
        (setq-local buffer-read-only t)
        (exwm-floatmode-innermode 1)))))

(defvar exwm-floatmode--float-buffer nil
  "Buffer currently showed in the floating frame.")

(defun exwm-floatmode--get-floating-buffer ()
  "Get the floating frame buffer.
If not found or currently active, search for it and update it."
  (if (and (buffer-live-p exwm-floatmode--float-buffer)
           (with-current-buffer exwm-floatmode--float-buffer
             exwm--floating-frame))
      exwm-floatmode--float-buffer
    ;; Attempt to find new buffer
    (dolist (buff (buffer-list)
                  (if (buffer-live-p exwm-floatmode--float-buffer)
                      exwm-floatmode--float-buffer))
      (if (with-current-buffer buff exwm--floating-frame)
          (setq exwm-floatmode--float-buffer buff)))))

(defun exwm-floatmode--get-floating-window ()
  "Get the window of the floating buffer."
  (let ((it (exwm-floatmode--get-floating-buffer)))
    (if it (get-buffer-window it t))))

(defun exwm-floatmode--get-floating-frame ()
  "Get the frame of the floating buffer."
  (let ((it (exwm-floatmode--get-floating-window)))
    (if it (window-frame it))))

(defun exwm-floatmode--do-floatfunc-and-restore (func)
  "Select the floating window, perform FUNC and then restore the current window.
If the floating window is already selected, then just run FUNC."
  (let ((curr-win (get-buffer-window (current-buffer) t))
        (float-win (exwm-floatmode--get-floating-window)))
    (unless float-win
      ;; Disable mode-mode if it's enabled
      (if exwm-floatmode-innermode (exwm-floatmode--inner-mode-exit))
      (user-error "No floating window available"))
    (if (eq curr-win float-win)
        (funcall func curr-win float-win)
      (select-window float-win)
      (let ((res (funcall func curr-win float-win)))
        (select-window curr-win)
        res)))) ;; return result of func

;;;###autoload
(defun exwm-floatmode-setup ()
  "Setup the floating window properties and associate it with the floating window.  Call this function upon loading the package."
  (interactive)
  (dolist (config exwm-floatmode-frame-defaults)
    (let* ((title (plist-get config :title))
           (geom (plist-get config :geometry))
           (decor (plist-get config :decoration))
           (newentry `((equal exwm-title ,title) ,@geom ,@decor)))
      (add-to-list 'exwm-manage-configurations newentry)))
  ;; onsetup, disable the tab-bar in this frame
  (add-hook 'exwm-floating-setup-hook
            (lambda () (interactive)
              (set-frame-parameter (exwm-floatmode--get-floating-frame)
                                   'tab-bar-lines 0)))
  ;; onexit, close the minor-mode
  (add-hook 'exwm-floating-exit-hook #'exwm-floatmode--inner-mode-exit))

(defun exwm-floatmode--mouse-click (&optional pos button-num window-id)
  "Perform a mouse click at position POS.

POS can be an (x . y) cons pair, nil to click at the current
location, or 'center to click in the center of the window.  By
default BUTTON-NUM is ``1'' (i.e. main click) and the WINDOW-ID
is the currently selected window."
  (let* ((button-index (intern (format "xcb:ButtonIndex:%d" (or button-num 1))))
         (button-mask (intern (format "xcb:ButtonMask:%d" (or button-num 1))))
         (window-id (or window-id (exwm--buffer->id
                                   (window-buffer (selected-window)))
                        (user-error "No window selected")))
         (xy-pos (cond ((eq pos 'center) (cons (floor (/ (window-pixel-width) 2))
                                               (floor (/ (window-pixel-height) 2))))
                       ((not pos) (mouse-absolute-pixel-position))
                       (t pos))) ;; assume X . Y
         (button-actions `((xcb:ButtonPress . ,button-mask)
                           (xcb:ButtonRelease . 0))))
    (dolist (b-action button-actions)
      (xcb:+request exwm--connection
          (make-instance 'xcb:SendEvent
                         :propagate 0
                         :destination window-id
                         :event-mask xcb:EventMask:NoEvent
                         :event (xcb:marshal
                                 (make-instance (car b-action)
                                                :detail button-index
                                                :time xcb:Time:CurrentTime
                                                :root exwm--root
                                                :event window-id
                                                :child 0
                                                :root-x (car xy-pos)
                                                :root-y (cdr xy-pos)
                                                :event-x (car xy-pos)
                                                :event-y (cdr xy-pos)
                                                :state (cdr b-action)
                                                :same-screen 0)
                                 exwm--connection))))
    (xcb:flush exwm--connection)
    xy-pos))

(defun exwm-floatmode--get-nonfloating-window ()
  "Get the first window in a non-floating buffer."
  (car (window-list exwm-workspace--current)))

;; (defvar exwm-floatmode--video-toggle nil
;;   "The video play/pause state.")

(defun exwm-floatmode-forcetoggle-video ()
  "Toggle the play/pause state of the video.
It assumes the first tabbed position would yield the play/pause button."
  (interactive)
  (exwm-floatmode--do-floatfunc-and-restore
   (lambda (curr-win float-win)
     (ignore curr-win float-win)
     ;; -- The below commented section does not work reliably
     ;; (setq exwm-floatmode--video-toggle (not exwm-floatmode--video-toggle))
     ;; (exwm-input--fake-key
     ;;  (if exwm-floatmode--video-toggle 'XF86AudioPause 'XF86AudioPlay))
     (exwm-input--fake-key 'escape)
     (exwm-floatmode--mouse-click 'center 2) ;; middle click
     (exwm-input--fake-key 'tab)
     (exwm-input--fake-key 'return)
     (exwm-input--fake-key 'S-tab)))) ;; reset button position

(defun exwm-floatmode-move-direction (direction &optional amount)
  "Move floating frame by AMOUNT in DIRECTION symbol: left, right, up, down.

If AMOUNT is nil then use the :move-slow amount, if t then use
the :move-fast amount, otherwise assume a pixel increment."
  (interactive (list (read-key "Direction ")))
  (let ((amount (cond ((not amount) (plist-get exwm-floatmode-modify-amount :move-slow))
                      ((booleanp amount) (plist-get exwm-floatmode-modify-amount :move-fast))
                      (t amount))))
    (exwm-floatmode--do-floatfunc-and-restore
     (lambda (a b)
       (ignore a b)
       (cond ((eq direction 'left) (exwm-floating-move (- amount) 0))
             ((eq direction 'right) (exwm-floating-move amount 0))
             ((eq direction 'up) (exwm-floating-move 0 (- amount)))
             ((eq direction 'down) (exwm-floating-move 0 amount))
             (t (user-error "No such direction")))
       (with-slots (x y width height)
           (xcb:+request-unchecked+reply
               exwm--connection (make-instance
                                 'xcb:GetGeometry
                                 :drawable (frame-parameter exwm--floating-frame
                                                            'exwm-container)))
         (message "%s" (list :x x :y y :width: width :height height)))))))

(defun exwm-floatmode--tolength (val dimension)
  "Scale a VAL to DIMENSION where appropriate.

If VAL is positive integer or nil return it, if a positive float
scale it by DIMENSION.  If VAL is a negative integer then
subtract it from DIMENSION.  If VAL is a negative float, then
scale it by DIMENSION and subtract from DIMENSION.

Used exclusively by ``exwm-floatmode-move''."
  (cond ((not val) val)
        ((integerp val) (if (>= val 0) val
                          (+ dimension val)))
        (t (floor (if (>= val 0) (* val dimension)
                    (+ dimension (* val dimension)))))))

(defun exwm-floatmode-move (x y width height &optional title)
  "Move and resize floating window to position X and Y and size WIDTH and HEIGHT, optionally only if the window TITLE.  If any of the required arguments are fractional, then it scales itself to the size of the screen as determined by ``(x-display-pixel-height/width)''."
  (interactive "nx: \nny: \nnwidth: \nnheight: ")
  (exwm-floatmode--do-floatfunc-and-restore
   (lambda (a b)
     (ignore a b)
     (let ((floating-container (frame-parameter exwm--floating-frame
                                                'exwm-container)))
       ;; if no title → continue
       ;; if title and match → continue
       (if (not (or (not title) (string= title exwm-title)))
           (user-error "'%s' not a set config")
         (let* ((pwt (x-display-pixel-width))
                (pht (x-display-pixel-height))
                (x (exwm-floatmode--tolength x pwt))
                (y (exwm-floatmode--tolength y pht))
                (width (exwm-floatmode--tolength width pwt))
                (height (exwm-floatmode--tolength height pht)))
           (exwm--set-geometry floating-container x y width height)
           (exwm--set-geometry exwm--id x y width height)
           (xcb:flush exwm--connection)))))))

(defun exwm-floatmode-resize-delta (&optional deltax deltay)
  "Resize the floating window by DELTAX pixels right and DELTAY pixels down.

Both DELTAX and DELTAY default to 0 if nil.  Both values can also
take the value 'inc or 'dec which reference the :resize keyword
amount from ``exwm-floatmode-modify-amount''.  This command should
be bound locally."
  (exwm-floatmode--do-floatfunc-and-restore
   (lambda (cwin fwin)
     (ignore cwin fwin)
     (exwm--log "DeltaX: %s, DeltaY: %s" deltax deltay)
     (unless (and (derived-mode-p 'exwm-mode) exwm--floating-frame)
       (user-error "[EXWM] `exwm-floating-move' is only for floating X windows"))
     (setq deltax (cond ((eq deltax 'inc) (plist-get exwm-floatmode-modify-amount :resize))
                        ((eq deltax 'dec) (- (plist-get exwm-floatmode-modify-amount :resize)))
                        ((not deltax) 0)
                        (t deltax)))
     (setq deltay (cond ((eq deltay 'inc) (plist-get exwm-floatmode-modify-amount :resize))
                        ((eq deltay 'dec) (- (plist-get exwm-floatmode-modify-amount :resize)))
                        ((not deltay) 0)
                        (t deltay)))
     (unless (and (= 0 deltax) (= 0 deltay))
       (let* ((floating-container (frame-parameter exwm--floating-frame
                                                   'exwm-container))
              (geometry (xcb:+request-unchecked+reply exwm--connection
                            (make-instance 'xcb:GetGeometry
                                           :drawable floating-container)))
              (_edges (window-inside-absolute-pixel-edges)))
         (with-slots (x y width height) geometry
           (let ((width-new (+ width deltax))
                 (height-new (+ height deltay)))
             (exwm--set-geometry floating-container nil nil width-new height-new)
             (exwm--set-geometry exwm--id nil nil width-new height-new)
             (xcb:flush exwm--connection)
             (message "%s" (list :x x :y y :width: width-new :height height-new)))))))))

(defun exwm-floatmode-resize (width height)
  "Resize the floating window to WIDTH pixels and to HEIGHT pixels."
  (interactive "nWidth: \nnHeight: ")
  (exwm--log "width: %s, height: %s" width height)
  (unless (and (derived-mode-p 'exwm-mode) exwm--floating-frame)
    (user-error "[EXWM] `exwm-floating-move' is only for floating X windows"))
  (unless width (setq width 1))
  (unless height (setq height 1))
  (unless (and (= 0 width) (= 0 height))
    (let ((floating-container (frame-parameter exwm--floating-frame
                                               'exwm-container)))
      (exwm--set-geometry floating-container nil nil width height)
      (exwm--set-geometry exwm--id nil nil width height)
      (xcb:flush exwm--connection))))

(defun exwm-floatmode-pause-media-windows (&optional matchstr num)
  "Pause all media windows matching regex MATCHSTR, and limit to the first NUM.

If MATCHSTR is nil, default to ``.*[Ww]atch.*''.  If NUM is nil, limit to none."
  (interactive)
  (let* ((matchstr (or matchstr (rx (seq (* any)
                                         (or "YouTube" (any ?W ?w) "atch")
                                         (* any)))))
         (blist (buffer-list))
         (nblist (length blist))
         (num (or num nblist)) ;; if nil consider all entries
         (ind -1)
         (toggled-windows ""))
    ;;(walk-windows (lambda (win)((
    (while (and (< (setq ind (1+ ind)) nblist) (> num 0))
      (let* ((buff (nth ind blist))
             (wid (exwm--buffer->id buff)))
        (when wid
          (with-current-buffer buff
            (and exwm-title
                 (when (string-match matchstr exwm-title)
                   (setq toggled-windows (concat toggled-windows
                                                 (format "'%s %s "
                                                         exwm-title
                                                         (exwm-floatmode--mouse-click 'center 1 wid))))
                   (setq num (1- num))))))))
    (exwm-floatmode-forcetoggle-video)
    (message "Toggled: %s" toggled-windows)))

(provide 'exwm-floatmode)
;;; exwm-floatmode.el ends here
