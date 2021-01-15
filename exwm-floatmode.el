;;; exwm-floatmode.el --- Convenient modes and bindings for
;;; floating picture-in-picture EXWM frames -*- lexical-binding: t;
;;; -*-

;; Copyright (C) 2020 Mehmet Tekman <mtekman89@gmail.com>

;; Author: Mehmet Tekman
;; URL: https://gitlab.com/mtekman/exwm-floatmode.el
;; Keywords: outlines
;; Package-Requires: ((emacs "25.1") (xelb "0.18") (exwm "0.24") (dash "2.17.0"))
;; Version: 0.2

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
;; * Setting and restoring window position saves
;;
;; Multiple floating windows
;; * Currently, all the --get functions return one floating window when multiple could be possible.
;;
;; README
;; * Write up with live demo

;;; Code:
(require 'xcb)
(require 'cl) ;; pushnew
(require 'exwm-core)
(require 'exwm-input)
(require 'exwm-floating)
(require 'dash)

(defgroup exwm-floatmode nil
  "Customization group for picture-in-picture."
  :group 'exwm-floating)

(defcustom exwm-floatmode-title "Picture-in-Picture"
  "Name of floating EXWM title to modify.
This can be found by invoking ``exwm-title'' on a window."
  :type 'string
  :group 'exwm-floatmode)

(defcustom exwm-floatmode-modify-amount
  '(:move 20 :resize 50)
  "Incremental amounts to :MOVE or :RESIZE the floating frame for the minor mode."
  :type 'plist
  :group 'exwm-floatmode)

(defcustom exwm-floatmode-geometry
  '(x 0.6 y 0.05 width 600 height 500)
  "Geometry for the floating window when spawned. Used by the
``exwm-floatmode-setup'' function. Units can be a fraction of the
visible workspace, or integer pixel values."
  :type 'alist
  :group 'exwm-floatmode)

(defcustom exwm-floatmode-decorations
  '(floating-mode-line nil floating-header-line nil
                       tiling-header-line nil tiling-mode-line nil
                       char-mode nil)
  "Decorations for the floating window when spawned."
  :type 'alist
  :group 'exwm-floatmode)

(defcustom exwm-floatmode-border
  '(:stationary ("navy" . 1) :moving ("maroon" . 3))
  "Floating border and width when stationary and during move-mode."
  :type 'plist
  :group 'exwm-floatmode)

(defcustom exwm-floatmode-custom-modes
  '(;; Move window default
    (:title nil :common-fn exwm-floatmode-move-direction
            :keyargs ((\S-left . 'left)
                      (\S-right . 'right)
                      (\S-up .  'up)
                      (\S-down . 'down)))
    ;; Move window 100
    (:title nil :common-fn exwm-floatmode-move-direction
            :keyargs ((\S-\M-left . ('left 100))
                      (\S-\M-right . ('right 100))
                      (\S-\M-up . ('up 100))
                      (\S-\M-down . ('down 100))))
    ;; Resize window
    (:title nil :common-fn exwm-floatmode-resize-delta
            :keyargs ((\M-left . (-20 nil))
                      (\M-right . (20 nil))
                      (\M-up . (nil -20))
                      (\M-down . (nil 20))))
    ;; Common controls
    (:title nil :keyargs ((?s . (call-interactively #'exwm-floatmode-position-save))
                          (?q . (exwm-floatmode--move-mode-exit))
                          (\S-? . (exwm-floatmode-forcetoggle-video))
                          (return . (exwm-floatmode--move-mode-exit))))
    ;; Video-specific controls
    (:title "Picture-in-Picture" :common-fn exwm-floatmode--send-key
            :keyargs (left right up down ? )))
  "A list of sparse keymaps to be loaded when TITLE matches the ``exwm-title'' for the floating window.

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



(defvar exwm-floatmode-position-configs
  '((:key [1] :title nil :x 0 :y 0 :width 400 :height 300)
    (:key [2] :title nil :x 100 :y 100 :width 400 :height 300)
    (:key [3] :title nil :x 200 :y 200 :width 300 :height 400)
    (:key [4] :title nil :x 300 :y 300 :width 300 :height 400))
    "List of ``(:key K :title T :x X :y Y :width W :height H))'' elements, where K denotes the keyboard sequence used to place the floating window matching TITLE to position X and Y and resizing it to W and H.  If TITLE is nil, then apply to the first floating window.")

(defvar exwm-floatmode-position-file
  (concat (file-name-as-directory user-emacs-directory) "float_positions.el")
  "File to store floating video positions.")

(defun exwm-floatmode--frame-geometry ()
  "Get geometry of current frame."
  (with-slots (x y width height)
      (xcb:+request-unchecked+reply exwm--connection
          (make-instance
           'xcb:GetGeometry :drawable
           (frame-parameter exwm--floating-frame 'exwm-container)))
    (list :x x :y y :width width :height height)))

(defun exwm-floatmode-position-save (hotkey)
  "Save the current floating window position to a HOTKEY."
  (interactive (list (read-key-sequence "Hotkey: ")))
  (let* ((geometry (exwm-floatmode--do-floatfunc-and-restore
                  (lambda (cwin fwin)
                    (exwm-floatmode--frame-geometry))))
         (posinfo (append (list :key hotkey :title exwm-title) geometry)))
    (message "Binding: %s" posinfo)
    (exwm-floatmode--position-store posinfo)))
    ;;(exwm-floatmode--refresh-minor-mode)))

(defun exwm-floatmode--position-store (posinfo)
  "Store the following POSINFO into the
``exwm-floatmode-position-configs'' variable, and sync with the
``exwm-floatmode-position-file''."
  (when exwm-floatmode-position-configs
    (pushnew posinfo exwm-floatmode-position-configs)
    (if exwm-floatmode-position-configs
        (with-temp-buffer
          (insert (format "%s" exwm-floatmode-position-configs))
          (write-file exwm-floatmode-position-file)))))

(defun exwm-floatmode--position-restore ()
  "Set the ``exwm-floatmode-position-configs'' variable from the contents of the ``exwm-floatmode-position-file''."
  (if exwm-floatmode-position-file
      (with-temp-buffer
        (unless (file-exists-p exwm-floatmode-position-file)
          (with-temp-buffer
            (insert "")
            (write-file exwm-floatmode-position-file)))
        (insert-file-contents exwm-floatmode-position-file)
        (let ((tmpvar (read-from-string (buffer-string))))
          (if tmpvar
              (setq exwm-floatmode-position-configs (car tmpvar))
            (user-error "Cannot parse"))))
    (user-error "``exwm-floatmode-position-file'' not set")))

(defun exwm-floatmode--convert2keymap (entry &optional map)
  "Convert an ENTRY in ``exwm-floatmode-custom-modes'' and to keymap MAP."
  (let ((title (plist-get entry :title))
        (commonfn (plist-get entry :common-fn))
        (keyargs (plist-get entry :keyargs))
        (map (or map (make-sparse-keymap))))
    (--map (let* ((_key (if (eq (type-of it) 'cons) (car it) it))
                  (_args (if (eq (type-of it) 'cons) (cdr it)))
                  (func (if commonfn
                            (if (not _args) ;; only key
                                `(,commonfn ',_key)
                              `(,commonfn ,@_args))
                          ;; no initial function
                          (if (not _args)
                              `(',_key)
                            `(,@_args))))
                  ;; TODO: Find a cleaner way to write the above
                  (titlefunc `(lambda () (interactive)
                                (if (or (not ,title) ;; no title, or a direct match
                                        (string= (with-current-buffer
                                                     (exwm-floatmode--get-floating-buffer)
                                                   exwm-title)
                                                 ,title))
                                    ,func)))
                  (press (cond ((eq _key 'SPC) `[? ])
                               (t `[,_key]))))
             (define-key map press titlefunc))
           keyargs)
    map))

;; C-M-x to redefine this
(define-minor-mode exwm-floatmode-move-mode
  "The minor mode for manipulating the exwm floating frame. Works only when called from non-floating frame, and it should therefore only be called from the ``exwm-floatmode-minor-mode'' function, which ensures this. NEVER CALL THIS MODE DIRECTLY"
  :init-value nil
  :lighter " FloatMode"
  :keymap
  (let ((map (make-sparse-keymap)))
    (dolist (entry exwm-floatmode-custom-modes map)
      (exwm-floatmode--convert2keymap entry map)))
  ;; init
  (let ((colwid (plist-get exwm-floatmode-border
                           (if exwm-floatmode-move-mode :moving :stationary))))
    (customize-set-variable 'exwm-floating-border-color (car colwid))
    (customize-set-variable 'exwm-floating-border-width (cdr colwid))))

(defun exwm-floatmode--refresh-minor-mode ()
  "Refresh the minor mode so that the new bindings from ``exwm-floatmode-position-configs'' take effect."
  ;;(exwm-floatmode-minor-mode -1)
  ;;(exwm-floatmode-minor-mode 1)
  (exwm-floatmode--move-mode-exit)
  (exwm-floatmode-minor-mode))


(defun exwm-floatmode--send-key (keyseq)
  "Send KEYSEQ to floating window."
  (exwm-floatmode--do-floatfunc-and-restore
   (lambda (c f) (exwm-input--fake-key keyseq))))

(defun exwm-floatmode--move-mode-exit ()
  "Functions to run on move-mode exit.  Hooked to ``exwm-floating-exit-hook''."
  (remove-hook 'quit-window-hook #'exwm-floatmode--move-mode-exit)
  (when (get-buffer "EXWM FloatMode")
    (exwm-floatmode-move-mode -1)
    (kill-buffer "EXWM FloatMode")))


(defun exwm-floatmode--position-expandbindings ()
  "Expand the bindings from ``exwm-floatmode-position-configs'' into the current keymap, and check for conflicts."
  (dolist (binding exwm-floatmode-position-configs)
    (if (string-match " undefined" (describe-key-briefly [a]))
        (let ((key (plist-get binding :key))
              (title (plist-get binding :title))
              (x (plist-get binding :x)) (y (plist-get binding :y))
              (w (plist-get binding :width)) (h (plist-get binding :height)))
          (define-key exwm-floatmode-move-mode key
            (lambda () (interactive)
              (exwm-floatmode--move x y w h title))))
      (user-error "%s is already set" binding))))

(defvar exwm-floatmode--prebuffer nil
  "Window before minor-mode was called, to be restored on exit.")


(defun exwm-floatmode-minor-mode (&optional junk)
  "Parent caller for ``exwm-floatmode-move-mode''.
Selects the floating window and sets the minor mode to STATE, 1 for on, anything else for off.
Event JUNK is discarded."
  (interactive)
  (ignore junk)
  (when (exwm-floatmode--get-floating-frame)
    (select-frame exwm-workspace--current) ;; grab workspace, not floating win
    (let* ((floater (get-buffer-create "EXWM FloatMode"))
           (newwin (popwin:popup-buffer floater
                                        :height 2 :position 'bottom
                                        :dedicated t :stick t :tail t)))
      (with-current-buffer "EXWM FloatMode"
        (add-hook 'quit-window-hook #'exwm-floatmode--move-mode-exit)
        (setq-local buffer-read-only t)
        (exwm-floatmode-move-mode 1)))))


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
  (--> (exwm-floatmode--get-floating-buffer)
       (if it (get-buffer-window it t))))

(defun exwm-floatmode--get-floating-frame ()
  "Get the frame of the floating buffer."
  (--> (exwm-floatmode--get-floating-window)
       (if it (window-frame it))))

(defun exwm-floatmode--hide-tab-bar-in-floating-frame ()
  "Hide the tab-bar mode in the floating frame.
Added to the ``after-make-frame-functions'' hook."
  (set-frame-parameter (exwm-floatmode--get-floating-frame)
                       'tab-bar-lines 0))

(defun exwm-floatmode--do-floatfunc-and-restore (func)
  "Select the floating window, perform FUNC and then restore the current window.
If the floating window is already selected, then just run FUNC."
  (let ((curr-win (get-buffer-window (current-buffer) t))
        (float-win (exwm-floatmode--get-floating-window)))
    (unless float-win
      ;; Disable mode-mode if it's enabled
      (if exwm-floatmode-move-mode (exwm-floatmode--move-mode-exit))
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
  ;; we use customize-set-variable because it triggers the :set function
  (setq exwm-manage-configurations nil)
  (customize-set-variable exwm-manage-configurations
                          (push
                           `((equal exwm-title exwm-floatmode-title)
                             floating t
                             ,@exwm-floatmode-decorations
                             ,@exwm-floatmode-geometry)
                           exwm-manage-configurations))
  ;; Setup: disable the tab-bar in this frame, launch move-mode
  ;; Exit: close the minor-mode
  (add-hook 'exwm-floating-setup-hook
            #'exwm-floatmode--hide-tab-bar-in-floating-frame)
  ;;(add-hook 'exwm-floating-setup-hook #'exwm-floatmode-minor-mode)
  (add-hook 'exwm-floating-exit-hook #'exwm-floatmode--move-mode-exit))

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
                       ((eq pos nil) (mouse-absolute-pixel-position))
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

;;;###autoload
(defun exwm-floatmode-forcetoggle-video ()
  "Toggle the play/pause state of the video.
It assumes the first tabbed position would yield the play/pause button."
  (interactive)
  (exwm-floatmode--do-floatfunc-and-restore
   (lambda (curr-win float-win)
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
  "Move floating frame by AMOUNT in DIRECTION symbol: left, right, up, down."
  (interactive (list (read-key "Direction ")))
  (let ((amount (or amount (plist-get exwm-floatmode-modify-amount :move))))
    (exwm-floatmode--do-floatfunc-and-restore
     (lambda (a b)
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

(defun exwm-floatmode-move (x y width height &optional title)
  "Move and resize floating window to position X and Y and size WIDTH and HEIGHT, optionally only if the window TITLE."
  (interactive)
  (exwm-floatmode--do-floatfunc-and-restore
   (lambda (a b)
     (let ((floating-container (frame-parameter exwm--floating-frame
                                                'exwm-container)))
       ;; if no title → continue
       ;; if title and match → continue
       (if (not (or (not title) (string= title exwm-title)))
           (user-error "'%s' not a set config")
         (exwm--set-geometry floating-container x y width height)
         (exwm--set-geometry exwm--id x y width height)
         (xcb:flush exwm--connection))))))

(defun exwm-floatmode-resize-delta (&optional deltax deltay)
  "Resize the floating window by DELTAX pixels right and DELTAY pixels down.

Both DELTAX and DELTAY default to 1.  This command should be bound locally."
  (exwm-floatmode--do-floatfunc-and-restore
   (lambda (cwin fwin)
     (exwm--log "DeltaX: %s, DeltaY: %s" deltax deltay)
     (unless (and (derived-mode-p 'exwm-mode) exwm--floating-frame)
       (user-error "[EXWM] `exwm-floating-move' is only for floating X windows"))
     (unless deltax (setq deltax 1))
     (unless deltay (setq deltay 1))
     (unless (and (= 0 deltax) (= 0 deltay))
       (let* ((floating-container (frame-parameter exwm--floating-frame
                                                   'exwm-container))
              (geometry (xcb:+request-unchecked+reply exwm--connection
                            (make-instance 'xcb:GetGeometry
                                           :drawable floating-container)))
              (edges (window-inside-absolute-pixel-edges)))
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


;;;###autoload
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
