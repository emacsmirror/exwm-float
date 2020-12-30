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

;;; Code:
(require 'xcb)
(require 'exwm-core)
(require 'exwm-input)
(require 'exwm-floating)

(defgroup exwm-floatmode nil
  "Customization group for picture-in-picture."
  :group 'exwm-floating)

(defvar exwm-floatmode--float-buffer nil
  "Buffer currently showed in the floating frame.")

(defcustom exwm-floatmode-title "Picture-in-Picture"
  "Name of floating exwm title to modify."
  :type 'string
  :group 'exwm-floatmode)

(defcustom exwm-floatmode-modify-amount
  '(:move 20 :resize 50)
  "Incremental amounts to :MOVE or :RESIZE the floating frame for the minor mode."
  :type 'plist
  :group 'exwm-floatmode)

(defcustom exwm-floatmode-geometry
  '(x 0.6 y 0.05 width 600 height 500)
  "Geometry for the floating window when spawned.
Units can be a fraction of the visible workspace, or integer pixel values."
  :type 'alist
  :group 'exwm-floatmode)

(defcustom exwm-floatmode-decorations
  '(floating-mode-line nil floating-header-line nil
                       tiling-header-line nil tiling-mode-line nil char-mode nil)
  "Decorations for the floating window when spawned."
  :type 'alist
  :group 'exwm-floatmode)

(defcustom exwm-floatmode-border
  '(:stationary ("navy" . 1) :moving ("maroon" . 2))
  "Floating border and width when stationary and during move-mode."
  :type 'plist
  :group 'exwm-floatmode)

(defcustom exwm-floatmode

;;(unintern exwm-floatmode-move-mode)
(define-minor-mode exwm-floatmode-move-mode
  "The minor mode for manipulating the exwm floating frame. Works only when called on the floating frame, and it should therefore only be called from the ``exwm-floatmode-minor-mode'' function, which ensures this."
  :init-value nil
  :lighter " FloatMode"
  :keymap
  '(([left] . (lambda () (interactive) (exwm-floatmode-move 'left)))
    ([right] . (lambda () (interactive) (exwm-floatmode-move 'right)))
    ([up] . (lambda () (interactive) (exwm-floatmode-move 'up)))
    ([down] . (lambda () (interactive) (exwm-floatmode-move 'down)))
    ([1] . (lambda () (interactive) (exwm-floatmode-move 'down)))
    ([? ] . exwm-floatmode-toggle-video)
    ([\M-left] . (lambda () (interactive) (exwm-floatmode-resize-delta -20 nil)))
    ([\M-right] . (lambda () (interactive) (exwm-floatmode-resize-delta 20 nil)))
    ([\M-up] . (lambda () (interactive) (exwm-floatmode-resize-delta nil -20)))
    ([\M-down] . (lambda () (interactive) (exwm-floatmode-resize-delta nil 20)))
    ([return] . exwm-floatmode-move-mode))
  (when exwm--floating-frame
    (exwm-floatmode-move-mode -1)
    ;; For some reason the floating window does not respond to mode
    ;; commands, and when enabled on the buffer it shifts the buffer
    ;; out of the floating frame.
    (user-error "This mode should NOT be enabled on the floating frame!"))
  (let* ((mode (if exwm-floatmode-move-mode :moving :stationary))
         (vald (plist-get exwm-floatmode-border mode)))
    (customize-set-variable 'exwm-floating-border-color (car vald))
    (customize-set-variable 'exwm-floating-border-width (cdr vald))))

(defvar exwm-floatmode--prewindow nil
  "Window before minor-mode was called, to be restored on exit.")

(defun exwm-floatmode-minor-mode-disable ()
  "Disable minor mode."
  (interactive)
  (exwm-floatmode-move-mode -1)
  ;;(remove-hook 'exwm-floating-exit-hook #'exwm-floatmode-minor-mode-disable)
  (message "What UP!"))

(defun exwm-floatmode-minor-mode (&optional state)
  "Parent caller for ``exwm-floatmode-move-mode''.
Selects the floating window and sets the minor mode to STATE, 1 for on, anything else for off."
  (interactive)
  (let ((state (or state 1))
        (fwin (exwm-floatmode--get-floating-window)))
    (unless fwin (user-error "No floating frame detected"))
    (if (eq state 1) ;; mode start
        (progn (setq exwm-floatmode--prewindow
                     (get-buffer-window (current-buffer)))
               (select-window fwin)
               (exwm-floatmode-move-mode t)
               (add-hook 'exwm-floating-exit-hook
                         #'exwm-floatmode-minor-mode-disable))
      ;; mode exit
      (select-window fwin)
      (exwm-floatmode-move-mode -1)
      ;; restore previous window
      (remove-hook 'exwm-floating-exit-hook #'exwm-floatmode-minor-mode-disable)
      (select-window exwm-floatmode--prewindow))))

(defun exwm-floatmode--get-floating-buffer ()
  "Get the floating frame buffer.
If not found or currently active, search for it and update it."
  (if (buffer-live-p exwm-floatmode--float-buffer)
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
      (if exwm-floatmode-move-mode (exwm-floatmode-move-mode -1))
      (user-error "No floating window available"))
    (if (eq curr-win float-win)
        (funcall func curr-win float-win)
      (select-window float-win)
      (funcall func curr-win float-win)
      (select-window curr-win))))

;;;###autoload
(defun exwm-floatmode-setup ()
  "Setup the floating window properties and associate it with the floating window.  Call this function upon loading the package."
  (interactive)
  (customize-set-variable 'exwm-manage-configurations
                          (pushnew
                           `((equal exwm-title exwm-floatmode-title)
                             floating t
                             ,@exwm-floatmode-decorations
                             ,@exwm-floatmode-geometry)
                           exwm-manage-configurations))
  ;; Disable the tab-bar in this frame
  (add-hook 'exwm-floating-setup-hook
            #'exwm-floatmode--hide-tab-bar-in-floating-frame))

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
(defun exwm-floatmode-toggle-video ()
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

(defun exwm-floatmode-move (direction &optional amount)
  "Move floating frame by AMOUNT in DIRECTION symbol: left, right, up, down."
  (interactive (list (read-key "Direction ")))
  (let ((amount (or amount (plist-get exwm-floatmode-modify-amount :move))))
    (exwm-floatmode--do-floatfunc-and-restore
     (lambda (a b)
       (cond ((eq direction 'left) (exwm-floating-move (- amount) 0))
             ((eq direction 'right) (exwm-floating-move amount 0))
             ((eq direction 'up) (exwm-floating-move 0 (- amount)))
             ((eq direction 'down) (exwm-floating-move 0 amount))
             (t (user-error "No such direction")))))))

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
         (with-slots (width height) geometry
           (exwm--set-geometry floating-container nil nil
                               (+ width deltax) (+ height deltay))
           (exwm--set-geometry exwm--id nil nil
                               (+ width deltax) (+ height deltay))
           (xcb:flush exwm--connection)))))))

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
    (exwm-floatmode-toggle-video)
    (message "Toggled: %s" toggled-windows)))

(provide 'exwm-floatmode)
;;; exwm-floatmode.el ends here
