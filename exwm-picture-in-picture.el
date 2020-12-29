;;; exwm-picture-in-picture.el --- Convenient modes and bindings for
;;; floating picture-in-picture EXWM frames -*- lexical-binding: t;
;;; -*-

;; Copyright (C) 2020 Mehmet Tekman <mtekman89@gmail.com>

;; Author: Mehmet Tekman
;; URL: https://gitlab.com/mtekman/exwm-picture-in-picture.el
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

(defgroup exwm-picture-in-picture nil
  "Customization group for picture-in-picture."
  :group 'exwm-floating)

(defvar exwm-picture-in-picture---float-buffer nil
  "Buffer currently showed in the floating frame.")

(defcustom exwm-picture-in-picture-move-amount 20
  "Amount to move the floating frame for the minor mode."
  :type 'integer
  :group 'exwm-picture-in-picture)

(defcustom exwm-picture-in-picture-geometry
  '(x 0.6 y 0.05 width 600 height 500)
  "Geometry for the floating window when spawned.
Units can be a fraction of the visible workspace, or integer pixel values."
  :type 'alist
  :group 'exwm-picture-in-picture)

(defcustom exwm-picture-in-picture-decorations
  '(floating-mode-line nil floating-header-line nil
                       tiling-header-line nil tiling-mode-line nil char-mode nil)
  "Decorations for the floating window when spawned."
  :type 'alist
  :group 'exwm-picture-in-picture)

(defcustom exwm-picture-in-picture-border
  '(:stationary ("navy" . 1) :moving ("maroon" . 2))
  "Floating border and width when stationary and during move-mode."
  :type 'plist
  :group 'exwm-picture-in-picture)

(define-minor-mode exwm-picture-in-picture-move-mode
  "The minor mode for manipulating the exwm Picture-in-Picture frame"
  :init-value nil
  :lighter " Picture-in-Picture"
  :keymap
  '(([left] . (lambda () (interactive) (exwm-picture-in-picture--move 'left)))
    ([right] . (lambda () (interactive) (exwm-picture-in-picture--move 'right)))
    ([up] . (lambda () (interactive) (exwm-picture-in-picture--move 'up)))
    ([down] . (lambda () (interactive) (exwm-picture-in-picture--move 'down)))
    ([? ] . exwm-picture-in-picture-toggle-video)
    ([return] . exwm-picture-in-picture-move-mode))
  (let* ((mode (if exwm-picture-in-picture-move-mode :moving :stationary))
         (vald (plist-get exwm-picture-in-picture-border mode)))
    (customize-set-variable 'exwm-floating-border-color (car vald))
    (customize-set-variable 'exwm-floating-border-width (cdr vald))))

(defun exwm-picture-in-picture--get-floating-buffer ()
  "Get the floating frame buffer.
If not found or currently active, search for it and update it."
  (if (buffer-live-p exwm-picture-in-picture---float-buffer)
      exwm-picture-in-picture---float-buffer
    ;; Attempt to find new buffer
    (dolist (buff (buffer-list)
                  (if (buffer-live-p exwm-picture-in-picture---float-buffer)
                      exwm-picture-in-picture---float-buffer))
      (if (with-current-buffer buff exwm--floating-frame)
          (setq exwm-picture-in-picture---float-buffer buff)))))

(defun exwm-picture-in-picture--get-floating-window ()
  "Get the window of the floating buffer."
  (--> (exwm-picture-in-picture--get-floating-buffer)
       (if it (get-buffer-window it t))))

(defun exwm-picture-in-picture--get-floating-frame ()
  "Get the frame of the floating buffer."
  (--> (exwm-picture-in-picture--get-floating-window)
       (if it (window-frame it))))

(defun exwm-picture-in-picture--hide-tab-bar-in-floating-frame ()
  "Hide the tab-bar mode in the floating frame.
Added to the ``after-make-frame-functions'' hook."
  (set-frame-parameter (exwm-picture-in-picture--get-floating-frame)
                       'tab-bar-lines 0))

(defun exwm-picture-in-picture-setup ()
  "Setup the floating window properties and associate it with the ``Picture-in-Picture'' window.  Call this function upon loading the package."
  (pushnew `((equal exwm-title "Picture-in-Picture")
             floating t
             ,@exwm-picture-in-picture-decorations
             ,@exwm-picture-in-picture-geometry)
           exwm-manage-configurations)
  (add-hook 'exwm-floating-setup-hook
            #'exwm-picture-in-picture--hide-tab-bar-in-floating-frame))

(defun exwm-picture-in-picture--mouse-click (&optional pos button-num window-id)
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
                                                :root-x 0 ;;(car xy-pos)
                                                :root-y 0 ;;(cdr xy-pos)
                                                :event-x (car xy-pos)
                                                :event-y (cdr xy-pos)
                                                :state (cdr b-action)
                                                :same-screen 0)
                                 exwm--connection))))
    (xcb:flush exwm--connection)
    xy-pos))

(defun exwm-picture-in-picture--get-nonfloating-window ()
  "Get the first window in a non-floating buffer."
  (car (window-list exwm-workspace--current)))

;; (defvar exwm-picture-in-picture--video-toggle nil
;;   "The video play/pause state.")

;;;###autoload
(defun exwm-picture-in-picture-toggle-video ()
  "Toggle the play/pause state of the video.
It assumes the first tabbed position would yield the play/pause button."
  (interactive)
  (let ((curr-win (get-buffer-window (current-buffer) t))
        (float-win (exwm-picture-in-picture--get-floating-window)))
    (if (eq curr-win float-win)
        ;; Select the first window in the current frame
        (setq curr-win (exwm-picture-in-picture--get-nonfloating-window)))
    (if (not float-win)
        ;; if no window, disable the move mode
        (if exwm-picture-in-picture-move-mode (exwm-picture-in-picture-move-mode -1))
      (select-window float-win)
      ;; -- The below commented section does not work reliably
      ;; (setq exwm-picture-in-picture--video-toggle (not exwm-picture-in-picture--video-toggle))
      ;; (exwm-input--fake-key
      ;;  (if exwm-picture-in-picture--video-toggle 'XF86AudioPause 'XF86AudioPlay))
      (exwm-input--fake-key 'escape)
      (exwm-picture-in-picture--mouse-click 'center 2) ;; middle click
      (exwm-input--fake-key 'tab)
      (exwm-input--fake-key 'return)
      (exwm-input--fake-key 'S-tab) ;; reset button position
      (select-window curr-win))))

(defun exwm-picture-in-picture--move (direction)
  "Move floating frame in DIRECTION symbol: left, right, up, down."
  (if (not (exwm-picture-in-picture--get-floating-buffer))
      (if exwm-picture-in-picture-move-mode (exwm-picture-in-picture-move-mode -1))
    ;; The mode is active in the calling buffer window, but the
    ;; floating window needs to be selected to work.
    (let ((curr-win (get-buffer-window (current-buffer)))
          (float-win (get-buffer-window exwm-picture-in-picture---float-buffer t)))
      (if (eq curr-win float-win)
          ;; Select the first window in the current frame
          (setq curr-win (exwm-picture-in-picture--get-nonfloating-window))
        (select-window float-win)
        ;; (with-current-buffer exwm-picture-in-picture---float-buffer
        (cond ((eq direction 'left) (exwm-floating-move (- exwm-picture-in-picture-move-amount) 0))
              ((eq direction 'right) (exwm-floating-move exwm-picture-in-picture-move-amount 0))
              ((eq direction 'up) (exwm-floating-move 0 (- exwm-picture-in-picture-move-amount)))
              ((eq direction 'down) (exwm-floating-move 0 exwm-picture-in-picture-move-amount))
              (t (user-error "No such direction"))))
      (select-window curr-win))))



;; (defun center-click-current-window ()
;;   (exwm-picture-in-picture--mouse-click 'center 2))

(defun exwm-picture-in-picture-pause-media-windows (&optional matchstr num)
  "Pause all media windows matching regex MATCHSTR, and limit to the first NUM.

If MATCHSTR is nil, default to ``.*[Ww]atch.*''.  If NUM is nil, limit to none."
  (interactive)
  (let* ((matchstr (or matchstr ".*[Ww]atch.*"))
         (blist (buffer-list))
         (nblist (length blist))
         (num (or num nblist)) ;; if nil consider all entries
         (ind -1)
         (toggled-windows ""))
    ;;(walk-windows (lambda (wid)
    (while (and (< (setq ind (1+ ind)) nblist) (> num 0))
      (let* ((buff (nth ind blist))
             (wid (exwm--buffer->id buff)))
        (when wid
          (with-current-buffer buff
            (and exwm-title
                 (when (string-match matchstr exwm-title)
                   (setq toggled-windows (concat toggled-windows (format "'%s' " exwm-title)))
                   (exwm-picture-in-picture--mouse-click 'center 1 wid)
                   (setq num (1- num))))))))
    (message "Toggled: %s" toggled-windows)))


(provide 'exwm-picture-in-picture)
;;; exwm-picture-in-picture.el ends here
