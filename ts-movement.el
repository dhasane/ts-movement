;;; ts-movement.el --- Movement commands using treesit syntax tree -*- lexical-binding:t -*-

;; Copyright (C) 2022 Free Software Foundation, Inc.

;; Author: Harit Kapadia <haritkapadia@outlook.com>
;; Version: 0.1
;; Package-Requires: ((treesit))
;; Keywords:
;; URL:

;;; Commentary:
;; Movement commands using treesit syntax tree.
;; Optionally depends on `hydra'.

;;; Code:
(require 'treesit)

(defvar-local tsm/-overlays (make-hash-table :test #'eq))

(defun tsm/-find-overlay-at-point (point)
  "Find any overlay in tsm/-overlays containing POINT."
  (seq-find (lambda (o) (gethash o tsm/-overlays)) (overlays-at point)))

(defun tsm/-overlay-at-point (point)
  "Get overlay at POINT, or make one and add to `tsm/-overlays' if it does not exist."
  (or (tsm/-find-overlay-at-point point)
      (let* ((node (treesit-node-on point point))
             (overlay (make-overlay (treesit-node-start node) (treesit-node-end node))))
        (overlay-put overlay 'face 'highlight)
        (overlay-put overlay 'evaporate t)
        (overlay-put overlay 'node node)
        (puthash overlay overlay tsm/-overlays)
        overlay)))

(defun tsm/delete-overlay-at-point (point)
  "Delete node indicator at POINT."
  (interactive "d")
  (let ((overlay (tsm/-find-overlay-at-point point)))
    (when overlay
      (delete-overlay overlay)
      (remhash overlay tsm/-overlays))))

(defun tsm/go-to-node (point next-func)
  "Go to previous sibling of node at POINT and go to start of node."
  (let* ((overlay (tsm/-overlay-at-point point))
         (node (overlay-get overlay 'node))
         (next (funcall next-func node)))
    (when next
      (overlay-put overlay 'node next)
      (move-overlay overlay (treesit-node-start next) (treesit-node-end next))
      (goto-char (treesit-node-start next)))))

(defun tsm/node-prev (point)
  "Go to previous sibling of node at POINT and go to start of node."
  (interactive "d")
  (tsm/go-to-node point 'treesit-node-prev-sibling))

(defun tsm/node-next (point)
  "Select next sibling of node at POINT and go to start of node."
  (interactive "d")
  (tsm/go-to-node point 'treesit-node-next-sibling))

(defun tsm/node-parent (point)
  "Select parent of indicated node at POINT."
  (interactive "d")
  (tsm/go-to-node point 'treesit-node-parent))

(defun tsm/node-child (point)
  "Select child containing POINT of indicated node."
  (interactive "d")
  (tsm/go-to-node point (lambda (node) (treesit-node-first-child-for-pos node point))))

(defun tsm/node-start (point)
  "Go to start of node at POINT."
  (interactive "d")
  (goto-char (treesit-node-start (overlay-get (tsm/-overlay-at-point point) 'node))))

(defun tsm/node-end (point)
  "Go to end of node at POINT."
  (interactive "d")
  (goto-char (1- (treesit-node-end (overlay-get (tsm/-overlay-at-point point) 'node)))))

(defun tsm/node-mark (point)
  "Mark node at POINT."
  (interactive "d")
  (let ((node (overlay-get (tsm/-overlay-at-point point) 'node)))
    (push-mark (treesit-node-end node) nil t)
    (goto-char (treesit-node-start node))))

(defun tsm/clear-overlays (&optional beg end)
  "Remove all overlays. BEG and END are unused."
  (interactive)
  (ignore beg end)
  (dolist (overlay (hash-table-keys tsm/-overlays))
    (delete-overlay overlay))
  (clrhash tsm/-overlays))

(if (featurep 'hydra)
    (defhydra tsm/hydra ()
      "TS Movement"
      ("d" #'tsm/delete-overlay-at-point "delete overlay" :column "overlay")
      ("b" #'tsm/node-prev "prev node" :column "movement")
      ("f" #'tsm/node-next "next node" :column "movement")
      ("p" #'tsm/node-parent "parent node" :column "movement")
      ("n" #'tsm/node-child "child node" :column "movement")
      ("a" #'tsm/node-start "node start" :column "movement")
      ("e" #'tsm/node-end "node end" :column "movement")
      ("m" #'tsm/node-mark "node mark" :column "overlay")
      )
  )


;; (defvar-keymap ts-movement-map
;;   :repeat t
;;   "b" #'tsm/node-prev
;;   "f" #'tsm/node-next
;;   "p" #'tsm/node-parent
;;   "n" #'tsm/node-child
;;   "a" #'tsm/node-start
;;   "e" #'tsm/node-end
;;   "d" #'tsm/delete-overlay-at-point
;;   "m" #'tsm/node-mark
;;   )

(defvar-keymap ts-movement-map
  :repeat t
  "M-k" #'tsm/node-prev
  "M-j" #'tsm/node-next
  "M-h" #'tsm/node-parent
  "M-l" #'tsm/node-child
  "M-i" #'tsm/node-start
  "M-e" #'tsm/node-end
  "M-p" #'tsm/delete-overlay-at-point
  "M-m" #'tsm/node-mark
  )
(fset 'ts-movement-map ts-movement-map)
;;;###autoload
(define-minor-mode ts-movement-mode
  "Movement and editing commands using treesit syntax tree."
  :keymap ts-movement-map
  (setq-local before-change-functions (cons #'tsm/clear-overlays before-change-functions)))

(provide 'ts-movement)
;;; ts-movement.el ends here
