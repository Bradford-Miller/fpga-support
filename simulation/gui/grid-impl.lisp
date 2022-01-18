;; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/33/LISPcapi-examples/RCS/elements:grid-impl.lisp,v 1.24.1.1 2017/01/19 11:50:04 martin Exp $" -*-

(in-package :fpga-gui-support)

;; minor mods for s79 BWM
(fpga-support-version-reporter "LispWorks CAPI Grid Screen" 0 1 0
                               "Time-stamp: <2022-01-11 13:51:10 gorbag>"
                               "0.1 release")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 0.1.0   1/11/22 snapping a line: 0.1 release of library supports scheme-79 test-0 and test-1. ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 0.0.6  12/ 8/21 renumber to fit new location under support (still minor number 0)

;; 0.1.1   9/ 8/21 move to support/simulation/gui/

;; 0.1.0   8/20/21 "official" 0.2 release: test-0 passed!

;; 0.0.3   8- 5-21      reduce calls to check-box-function by using let

;; 0.0.x   4-21-21      Add TODO to remember to add font spec for columns (so
;;                          we can shrink some verbose ones to fit)

;; 0.0.2   1-20-2021    Add grid-band-p and check it when looking for a
;;                          focus-function

;; 0.0.1   1- 2-2021    Add :disabled flag for checkbox-function (i.e. it
;;                          is not relevant / read-only) end mods

;; 0.0.0   from <lispworks installation>/Library/lib/7-1-0-0/examples/capi/elements/grid-impl.lisp

;; TODO (for s79, etc):
;;    Add font specification for individual columns 

;; Copyright (c) 1987--2017 LispWorks Ltd. All rights reserved.

;;;; There is an example using this implementation in grid.lisp

;;;; Classes:
;;;; GRID-SECTION-ANY  -  top of the hierarchy of all grid objects
;;;; GRID-WIDGET       - the whole widget. Subbclass of CAPI:PINBOARD-LAYOUT
;;;; GRID-SECTION  (abstract) - a section of the grid. GRID-SECTIONs has band, which
;;;                         can be either GRID-SECTIONs or GRID-SUBSETIONs. 
;;;;    subclasses     GRID-ROW-SECTION, GRID-COLUMN-SECTION
;;;; GRID-BAND (abstract)  - band of the grid, which define a group of bands. 
;;;;    subclasses GRID-ROW, GRID-COLUMN. 
;;;;
;;;; A pair of GRID-ROW and GRID-COLUMN define a sub-grid of "cells" where their 
;;;; bands intersect. 
;;;; User drawing is done by the method DRAW-CELL, which is called 

;; minor mods for s79 BWM
;;(in-package "CL-USER") ; use our own package (see start of file) BWM

;; end minor mods BWM

(toggle-source-debugging t)

(proclaim '(optimize (fixnum-safety 0)))


(defclass widget-row-mixin () ())
(defclass vertical-section-mixin () ())
(defclass vertical-band-mixin (vertical-section-mixin) ())

(defclass horizontal-section-mixin () ())
(defclass horizontal-band-mixin (horizontal-section-mixin) ())



;;; ******************************************************************
;;;    Drawing lines using line-specs
;;; ******************************************************************

;; assume it is canonicalize
(defmacro thickness-from-spec (spec)
  `(let ((sp ,spec))
     
     (if (fixnump sp)
         sp
       (second sp))))

;; like above, but returns 0 so can be used directly 
;;; for geometry calculations 

(defmacro thickness-adjustment (spec)
 `(let ((sp ,spec))
     
     (if (fixnump sp)
         sp
       (if sp
           (second sp)
         0))))

(defun canonicalize-line-spec (spec)
  (when spec
    (when (fixnump spec)
      (return-from canonicalize-line-spec spec))
    (when (and  (consp spec)
                (when-let (l (dotted-list-length spec))
                  (evenp l))
                (loop for cons on spec by 'cddr
                      do (unless (keywordp (car cons)) (return nil))
                      finally (return t)))
      (let ((thickness (getf spec :thickness 1))
            (spec  (copy-list spec)))
        (remf spec  :thickness)
        (return-from canonicalize-line-spec
        (list* :thickness thickness spec))))
    (error "An appropriate LINE-SPEC (must be NIL, Fixnum, or a a plist of arguemnts for GP:DRAW-lINE): ~a" spec)))
        
    
;;; Must be either horizontal or vetical. Top is true if in the top
;;; or left of the cell, so we know how to adjust for thickness. 
(defun grid-draw-border (port start-x start-y end-x end-y spec top)
  (let ((th (thickness-from-spec spec)))
    (when th
      (let ((thickness-correction (ash (1+ th) -1)))
        (unless top (setq thickness-correction (- 0 thickness-correction)))
        (if (eq start-x end-x)
            (progn (incf start-x thickness-correction) (incf end-x thickness-correction))
          (progn
            (incf start-y thickness-correction) (incf end-y thickness-correction)
            (when (> 40 start-y)
              nil
              )
            ) 
          )

        (if (fixnump spec)
            (gp:draw-line port start-x start-y end-x end-y :thickness spec)
          (progn         
            (apply 'gp:draw-line port start-x start-y end-x end-y  spec)))))))

;;; ******************************************************************
;;; Base class
;;; ******************************************************************

(defclass grid-section-any ()
  (
   (name :initarg :name :initform nil :accessor name)
   (resizable-p :initarg :resizable-p :initform t)
   (proportional-p :initarg :proportional-p :initform t)
   (data-object :initarg :data-object :initform nil :accessor data-object)

   (accepts-focus-p :initform t :initarg :accepts-focus-p :accessor accepts-focus-p)

   (size :initarg :size :initform 100)
   (cell-size :initform nil :accessor cell-size)
   
   (parent :initform nil :accessor section-parent)
   (visible :initform nil :accessor visible)
   (i-right-border-spec :initform nil  )
   (i-left-border-spec :initform 1 )
   (i-top-border-spec :initform 1 )
   (i-bottom-border-spec :initform nil )
   (index :initform nil)
   ))



(defmethod initialize-instance :after  ((self grid-section-any) &key 
                                        (right-border-spec 1)  left-border-spec
                                        top-border-spec (bottom-border-spec 1))
  (with-slots(i-right-border-spec i-left-border-spec i-top-border-spec i-bottom-border-spec) self
    (setf i-right-border-spec (canonicalize-line-spec right-border-spec))
    (setf i-left-border-spec (canonicalize-line-spec left-border-spec))
    (setf i-top-border-spec (canonicalize-line-spec top-border-spec))
    (setf i-bottom-border-spec (canonicalize-line-spec bottom-border-spec))))

(defmethod print-object ((self grid-section-any) out-stream)
  (print-unreadable-object (self out-stream :type t)
    (with-slots (name) self
      (princ name out-stream))))

(defclass grid-section (grid-section-any)
  (
   (scrollable-p :initarg :scrollable-p :initform t :reader scrollable-p)
   (scrollbar :initform nil)   
   (scrollbar-initialized :initform nil)
   (uniform-resizing-p :initarg :uniform-resizing-p :initform t)
   (empty-section-color :initarg :empty-section-color :initform :grey)

   (virtual-size :initform nil)
   (bands :accessor bands :initarg :bands :initform nil)
   ))

(defclass grid-column-section (grid-section vertical-section-mixin) ())
(defclass grid-row-section (grid-section horizontal-section-mixin) ())

(defclass grid-band (grid-section-any)
  (
   (enabled :initarg :enabled :initform nil)
   (dependents :initarg :dependents :initform nil)
   (band-dragging :initarg :band-dragging :initform nil)
   (focus-function :initform nil :initarg :focus-function :accessor band-focus-function)
   ))

(defmethod scrollable-p ((x grid-band)) nil)

(defmethod grid-band-p (thingy)
  (typep thingy 'grid-band)) ;; bwm

(defclass grid-display-band (grid-band)
  (
   (dependents :initarg :dependents :initform nil)
   (check-box-function :initform nil :initarg :check-box-function :reader band-check-box-function)
   (check-box-coordinates :initform '(2 2 8 8) :initarg :check-box-coordinates :reader band-check-box-coordinates)
   (drawing-function :initform nil :initarg :drawing-function :reader band-drawing-function)
   (motion-function :initform nil :initarg :motion-function :accessor band-motion-function)
   (release-function :initform nil :initarg :release-function :accessor band-release-function)
   (action-function :initform nil :initarg :action-function :accessor band-action-function)
   (click-function :initform nil :initarg :click-function :accessor band-click-function)

   (print-function :initarg :print-function :initform 'princ-to-string :accessor band-print-function)

   (data-reader :initarg :data-reader :accessor band-data-reader :initform nil)
   (data-getter :initarg :data-getter :accessor band-data-getter :initform nil)
   (data-writer :initarg :data-writer :accessor band-data-writer :initform nil)
   (read-converter :initarg :data-read-converter :accessor band-read-converter :initform nil)
   (write-converter :initarg :data-write-converter :accessor band-write-converter :initform 'identity)

   (editing-pane-type :initform nil :initarg  :editing-pane-type :accessor editing-pane-type)
   (test-function :initform 'equal :initarg :test-function)
   (sort-predicate :initform nil :initarg :sort-preicate)
   (range-reader :initarg :range-reader :reader band-range-reader :initform nil)
   (update-callback :initarg :update-callback :initform nil )

   (text-x-offset :initarg :text-x-offset :initform 4)
   (text-y-offset :initarg :text-x-offset :initform 21)

   (header-title :initarg :header-title
      :accessor header-title
      :initform nil)
   (header-horizontal-justification
    :initarg :header-horizontal-justification
    :accessor header-horizontal-justification
    :initform :center)
   ))

(defclass grid-special-band (grid-display-band) ())  ;;; overriding 

(defmethod (setf band-check-box-function) (function (self grid-band))
  (setf (slot-value self 'check-box-function) function)
  (invalidate-band self))

(defmethod (setf band-check-box-coordinates) (coordinates (self grid-band))
  (setf (slot-value self 'check-box-coordinates) coordinates)
  (invalidate-band self))

(defmethod (setf band-drawing-function) (function (self grid-band))
   (setf (slot-value self 'drawing-function) function)
    (invalidate-band self))

;;; That can be extended to recognize other specifications
(defun interpret-size (size self)
  (declare (ignore self))
  (cond ((integerp size) size)
        ((symbolp size) (symbol-value size))))
        
         
(defmethod total-size  ((self grid-band))
  (with-slots ( size cell-size) self
    (unless cell-size
      (setq cell-size 
            (interpret-size size self )))
     cell-size ))

(defmethod total-size  ((self grid-section))
  (with-slots (size cell-size) self
    (unless cell-size
      (setq cell-size 
            (interpret-size size self )))
    
    cell-size))

(defclass grid-data-row (grid-band horizontal-band-mixin) ())
(defclass grid-display-row (grid-display-band horizontal-band-mixin) ())
(defclass grid-special-row (grid-special-band horizontal-band-mixin) ( ) )


(defclass grid-data-column (grid-band vertical-band-mixin) ())

(defclass grid-display-column (grid-display-band vertical-band-mixin) ())
(defclass grid-special-column (grid-special-band vertical-band-mixin) ()) ;;; overriding 

(defun make-header-row ()
  (make-instance 'grid-special-row 
                 :data-getter :other-band
                 :data-reader 'header-title
                 :band-dragging t))

(defmethod bands ((x grid-band)) nil) ;; 


(defmethod section-virtual-size ((section grid-column-section))
  (with-slots (i-left-border-spec i-right-border-spec) section
    (let ((vs (+ (thickness-adjustment i-left-border-spec)
                 (thickness-adjustment i-right-border-spec))))
       (dolist (x (bands section))
         (incf vs (total-size x)))
       vs)))


(defmethod section-virtual-size ((section grid-row-section))
  (with-slots (i-bottom-border-spec i-top-border-spec) section
    (let ((vs (+ (thickness-adjustment i-top-border-spec)
                 (thickness-adjustment i-bottom-border-spec))))
       (dolist (x (bands section))
         (incf vs (total-size x)))
       vs)))
  


  

;;; ******************************************************************
;;; Deauls for the grid-widget
;;; ******************************************************************
(defvar *default-widget-outter-thickness* nil)

(defparameter *grid-widget-gesture-spec*
  '(((:button-1 :press) apply-event-to-cell cell-click nil)
    ((:button-1 :release) apply-event-to-cell cell-release-drag-check nil )
    ((:button-1 :motion) apply-event-to-cell  cell-motion-drag-check nil)
    ((:button-1 :press :control) apply-event-to-cell  cell-click :control)
    ((:button-1 :release :control) apply-event-to-cell  cell-release :control)
    ((:button-1 :press :meta) apply-event-to-cell  cell-click :meta)
    ((:button-1 :release :meta) apply-event-to-cell cell-release :meta)
    ((:button-1 :press :shift) apply-event-to-cell  cell-click :shift)
    ((:button-1 :release :shift) apply-event-to-cell  cell-release :shift)
    ((:button-1 :second-press)  apply-event-to-cell   cell-action nil)
    
    ((:gesture-spec #\return) grid-keyboard-focus-call perform-cell-action)
    ((:gesture-spec #\newline) grid-keyboard-focus-call perform-cell-action)
    ((:gesture-spec #\escape)  grid-escape)
    ((:gesture-spec :left) grid-keyboard-focus-call grid-change-column -1)
    ((:gesture-spec :right) grid-keyboard-focus-call grid-change-column 1)
    ((:gesture-spec :up) grid-keyboard-focus-call grid-change-row -1)
    ((:gesture-spec :down) grid-keyboard-focus-call grid-change-row 1)))
    

;;; ******************************************************************
;;;  The grid-widget object together with its capi methods for 
;;;  setting things up
;;; ******************************************************************

(defclass grid-widget (grid-section-any capi:pinboard-layout)
  (
   (column-sections :initarg :column-sections
                    :initform (list (make-instance 'grid-column-section))
                    :reader column-sections)
   (row-sections :initarg :row-sections 
                 :initform (list (make-instance 'grid-row-section))
                 :reader row-sections)

   (horizontal-scrollbars :initform nil)
   (vertical-scrollbars :initform nil)

   (scrollbar-height :initform 20)
   (scrollbar-width :initform 20)

   (inside-drag :initform nil)
   (inside-drag-offset :initform 0)
   (inside-drag-direction :initform nil)
   (inside-drag-bound :initform (cons nil nil))
   (inside-drag-other-data :initform nil)
   (drag-colour :initform :yellow :initarg :drag-colour)
   (drag-thickness :initform 1 :initarg :drag-thickness)
   (focus-row :initform nil)
   (focus-column :initform nil)
   (focus-coordinates :initform nil)
   (focus-thickness :initform 1 :initarg :focus-thickness)
   (focus-color :initform :red)

   (overlay :initform nil)

   (width :initform nil)
   (height :initform nil) 

   (grid-sheet :initarg :grid-sheet)
   ;;; caches for editing
   (editing-panes :initform nil :accessor grid-widget-editing-panes)
   (current-row :initform nil)
   current-column
   (editing-interface :initform nil)
   (row-num :initform 0)  ;;; count rows
   (col-num :initform 0)

   )
  (:default-initargs
   :draw-with-buffer t
   :visible-border t
   :initial-focus t
   :right-border-spec *default-widget-outter-thickness*
   :left-border-spec *default-widget-outter-thickness*
   :bottom-border-spec *default-widget-outter-thickness*
   :top-border-spec *default-widget-outter-thickness*
   :input-model *grid-widget-gesture-spec*
   :create-callback 'adjust-scrollbar-positions))

(defmethod invalidate-display-cache ((self grid-widget))
  (with-slots (focus-coordinates) self
    (setq focus-coordinates nil)))

(defmethod capi:calculate-constraints ((self grid-widget))

  (multiple-value-bind (width height)
      (calculate-size self)
    (with-slots (grid-sheet) self
      (capi:with-geometry grid-sheet
        (setf capi:%min-width% width
              capi:%min-height% height)))
    (capi:with-geometry self
      (setf capi:%min-width% width
            capi:%min-height% height))))



(defmethod check-horizontality ((section grid-section)  horizontal)
  (dolist (band (bands section))
    (setq horizontal (check-horizontality  band horizontal)))
  horizontal)

(defmethod horizontal-widget-p ((x grid-band)) nil)

(defmethod horizontal-widget-p ((x grid-display-column)) t)
(defmethod horizontal-widget-p ((x grid-data-row)) t)
(defmethod horizontal-widget-p ((x grid-special-row)) t)

(defmethod check-horizontality  ( (band grid-band)  horizontal)
  (let ((Ih (horizontal-widget-p band)))
    (if (eq horizontal :unknown)
        (setq horizontal ih)
      (unless (eq ih horizontal)
         (error "Incompatible mixing of columns and row types (not one of: GRID-DATA-COLUMN/GRID-SPECIAL-COLUMN x GRID-DISPLAY-ROW or GRID-DISPLAY-COLUMN x GRID-DATA-ROW/GRID-SPECIAL-ROW): ~a" (type-of band))))
     horizontal))

(defmethod check-horizontality ((x t) horizontal) 
  (error "Unknown object of type ~a in rows or columns" (type-of x)))


(defun update-sections-indices (sections)
  (let ((index 0))
    (dolist (section sections)
      (setf (slot-value section 'index) index)
      (let ((bands (bands section)))
        (if bands
            (dolist (band bands)
              (setf (slot-value band 'index) index)
              (incf index))
          (incf index))))
    index))
        
(defun update-indices (grid-widget)
  (setf (slot-value grid-widget 'col-num)
        (update-sections-indices (slot-value grid-widget 'column-sections)))
  (setf (slot-value grid-widget 'row-num)
        (update-sections-indices (slot-value grid-widget 'row-sections))))

(defmethod initialize-instance :after ((self grid-widget) &key)
  (with-slots (column-sections row-sections grid-sheet parent) self
    (let ((horizontal :unknown))
      (dolist (element column-sections)
        (check-horizontality element horizontal)
        (setup-parent element self))
    (dolist (element row-sections)
       (check-horizontality element horizontal)
      (setup-parent element self)))
    (setf parent self)
    (setf grid-sheet (make-instance 'grid-sheet))
    (let ((layout-description (list grid-sheet)))
      (setf (capi:layout-description self) layout-description)))
  (add-scrollbar-objects self)
  (update-indices self))


(defun add-a-set-of-scrollbars (grid sections orientation)
  (let (new-scrollbars)
    (loop for section in sections 
          when (scrollable-p section)
          do (with-slots (scrollbar ) section
               (unless scrollbar
                 (let ((new-scrollbar 
                        (make-instance 'capi:scroll-bar 
                                       :accepts-focus-p nil
                                       :orientation orientation)))
                   (setf scrollbar new-scrollbar)
                   (push new-scrollbar new-scrollbars) 
                   (push new-scrollbar (capi:layout-description grid)))))
          finally (return new-scrollbars))))

(defun add-scrollbar-objects (grid)
  (with-slots (row-sections column-sections horizontal-scrollbars
                            vertical-scrollbars) grid
    (setf vertical-scrollbars 
          (add-a-set-of-scrollbars grid row-sections :vertical))
    (setf horizontal-scrollbars 
          (add-a-set-of-scrollbars grid column-sections :horizontal))))

(defun adjustment-of-scrollbars (grid sections width height scrollbar-width scrollbar-height rowp)
  (let ((start 0))
    (loop for section in sections 
          when (scrollable-p section)
          do (with-slots (scrollbar  scrollbar-initialized) section
               
                 (unless scrollbar-initialized
                   (setf (capi:pinboard-pane-position scrollbar)
                         (if rowp 
                             (values (+ 2  (- width scrollbar-width)) start)
                           (values start  ( + 2 (- height scrollbar-height))))
                         (capi:pinboard-pane-size scrollbar)
                         (if rowp
                             (values scrollbar-width (total-size section))
                           (values (total-size section) scrollbar-height)))
                 
                   (setf (capi:range-callback scrollbar)
                         (if rowp
                             (make-callback grid
                                            nil start
                                            nil (total-size section))
                           (make-callback grid
                                          start nil
                                          (total-size section) nil)))
                   (setq scrollbar-initialized t)
                   (capi:range-set-sizes scrollbar
                                         :start 0 
                                         :end (section-virtual-size section)
                                         :slug-start 0
                                         :slug-end (total-size section))))
          do
               (incf start (total-size section)))))

(defun adjust-scrollbar-positions (grid)
  (with-slots (row-sections column-sections 
                            scrollbar-height
                            scrollbar-width) grid
    (multiple-value-bind (width height) (calculate-size grid)
      (adjustment-of-scrollbars grid row-sections width height scrollbar-width scrollbar-height t)
      (adjustment-of-scrollbars grid column-sections width height scrollbar-width scrollbar-height nil))))

;;; ******************************************************************
;;;       Utility functions for deducing the current clip region
;;; ******************************************************************

(defstruct (a-clip (:copier clip-copier)) x y right bottom)

(defun make-empty-clip (x y width height)
  (make-a-clip :x x :y y :right (+ x width) :bottom (+ y height)))

(defun reset-clip (the-clip x y width height)
  (setf (a-clip-x the-clip) x
        (a-clip-y the-clip) y
        (a-clip-right the-clip) (+ x width)
        (a-clip-bottom the-clip) (+ y height))
  the-clip)

(defmacro with-deduced-clipping (((left top right bottom) (clip x y width height))
                                 &body body)
  (rebinding (clip x y width height)
    (with-unique-names (x1 right1 y1 bottom1)
      `(progn
         (unless ,x
           (setq ,x 0 ,width 100000))
         (unless ,y 
           (setq ,y 0 ,height 100000))
         (let* ((,x1 (a-clip-x ,clip))
                ,right1)
           (if ,x1
               (setq ,right1  (a-clip-right ,clip))
             (setq ,x1 0 ,right1 100000))
           (let ((,y1 (a-clip-y ,clip))
                 ,bottom1)
             (if ,y1
                 (setq ,bottom1 (a-clip-bottom ,clip))
               (setq ,y1 0 ,bottom1 100000))
             (multiple-value-bind (,left ,top ,right ,bottom)
                 (gp:rectangle-sect ,x1 ,y1 ,right1 ,bottom1
                                    ,x ,y (+ ,x ,width) (+ ,y ,height))
               (when (and ,left ,top ,right ,bottom)
                 ,@body))))))))
             
(defun merge-clip (clip x y width height)
  (with-deduced-clipping
      ((left top right bottom)
       (clip x y width height))
    (make-a-clip :x left :y top
                 :right right 
                 :bottom bottom)))

(defun merge-clip-destructive (result-clip clip x y width height)
    (with-deduced-clipping
      ((left top right bottom)
       (clip x y width height))
      (setf (a-clip-x result-clip) left
            (a-clip-y result-clip) top
            (a-clip-right result-clip) right
            (a-clip-bottom result-clip) bottom)
      result-clip))

(defmacro clip-to-mask (clip)
  (rebinding (clip)
    `(let ((x (a-clip-x ,clip))
           (y (a-clip-y ,clip)))
       (list x y
             (- (a-clip-right ,clip) x)
             (- (a-clip-bottom ,clip) y)))))

(defun translate-clip-x (the-clip x)
  (let ((result-clip (clip-copier the-clip)))
    (incf (a-clip-x result-clip) x)
    (incf (a-clip-right result-clip) x)
    result-clip))

(defun translate-clip-y (the-clip y)
  (let ((result-clip (clip-copier the-clip)))
    (incf (a-clip-y result-clip) y)
    (incf (a-clip-bottom result-clip) y)
    result-clip))

(defun no-interaction (x y width height x1 y1 width1 height1)
  (or (> x1 (+ x width))
      (< (+ x1 width1) x)
      (< (+ y1 height1) y)
      (> y1 (+ y height))))


;;; ******************************************************************
;;;   User accessible grid manipulation functions
;;; ******************************************************************

(defmethod add-column ((self grid-section) item &optional pos )
  (with-slots (bands) self
    (setq bands
          (if (not pos)
              (append bands  (list item))
            (append (subseq bands 0 (max 0 pos))
                    (list item)
                    (subseq bands (max 0 pos)))))
    (setf (section-parent item) self))
  (update-indices (section-parent self))
  )

(defun section-sibling (section name)
  (find name (section-sibling-data section (section-parent section)) :key 'name))

(defmethod section-sibling-data ((self vertical-section-mixin)
                                 (parent grid-widget))
  (with-slots (column-sections) parent
    column-sections))

(defmethod section-sibling-data (self
                                 (parent grid-section))
  (with-slots (bands) parent
    bands))

(defun band (section name)
  (find name (bands section) :key 'name))

(defmethod find-column ((widget grid-widget) name)
  (find-in-sections (column-sections widget) name))

(defmethod find-row ((widget grid-widget) name)
   (find-in-sections (row-sections widget) name))

(defmethod find-column ((widget grid-section-any) name)
  (find-column (section-parent widget) name))

(defmethod find-row ((widget grid-section-any) name)
   (find-row (section-parent widget) name))

(defun find-in-sections (sections name)
  (dolist (section sections)
    (when (eq name (name section) ) (return section))
    (dolist (x (bands section))
          (when (eq name (name x) ) (return-from find-in-sections x)))))

;;; Maybe we should have a vector that we can just index into it
(defun find-index-in-sections (sections index)
  (dolist (section sections)
    (dolist (x (bands section))
      (when (eq index (slot-value x 'index) ) (return-from find-index-in-sections x)))
    (when (eq index (slot-value section 'index) ) (return section))))
  

(defun find-band-position (band)
  (position band (bands (section-parent band))
            :test #'eq))
;;;; find the data object. If either the row or column is special,
;;;; they supply it. Otherwise, if the column is grid-data-column
;;;; it supplies it, otherwise the row supplies it. 

(defmethod cell-data-object ((row grid-special-row) column)
   (slot-value row 'data-object))

(defmethod cell-data-object (row (column grid-special-column))
   (slot-value column 'data-object))

(defmethod cell-data-object (row (column grid-data-column))
   (slot-value column 'data-object))

(defmethod cell-data-object (row column )
   (slot-value row 'data-object))


;;;; return the object out of which the display behaviour comes. 
;;;; If either objects is special then we take it, otherwise if the
;;;; column is data take the row, otherwise take the compil

(defmethod cell-displayer ((row grid-special-row) column) row)

(defmethod cell-displayer (row (column grid-special-column)) column)

(defmethod cell-displayer (row (column grid-data-column)) row)

(defmethod cell-displayer (row column ) column)
      
      
(defmethod read-cell-value (row column)
  (let ((displayer (cell-displayer row column)))
    (with-slots ( data-reader read-converter data-getter) displayer
     
      (let ((data-object  (if data-getter
                              (case data-getter
                                (:other-band (if (eq displayer row) column row))
                                (:band displayer)
                                (:band-position (find-band-position displayer))
                                (:other-band-position 
                                 (find-band-position  (if (eq displayer row) column row)))
                                 (t (funcall data-getter row column)))
                            (cell-data-object row column))))
        (if data-object
            (if  data-reader
                (let ((data (funcall data-reader data-object)))
                  (if read-converter
                      (funcall read-converter data)
                    data))                             
              data-object))))))

(defmethod write-cell-value (row column value)
  (when-let (data-object (cell-data-object row column))
    (let ((displayer (cell-displayer row column)))
      (with-slots (data-writer write-converter update-callback) displayer
        (when (or data-writer update-callback)
          (let ((c-value  (funcall write-converter value)))
            (when (and  (or (not update-callback)
                            (funcall update-callback c-value data-object row column))
                        data-writer)
              (funcall data-writer c-value  data-object))
            (invalidate-cell row column)
            (when-let (row-deps (slot-value row 'dependents))
              (dolist (x row-deps)
                (invalidate-cell x column)))
            (when-let (column-deps (slot-value column 'dependents))
              (dolist (x column-deps)
                (invalidate-cell row x )))))))))

(defun redraw-pinboard-with-redisplay (pinboard x y width height)
  (capi:redraw-pinboard-layout pinboard x y width height t))

(defmethod invalidate-cell ((row grid-section-any) (col grid-section-any))
  (when-let (result-clip (clip-information-for-cell row col))
    (apply 'redraw-pinboard-with-redisplay 
           (top-level-widget row)
           (clip-to-mask result-clip))))

(defun traverse-section-list (list function)
  (dolist (x list)
    (funcall function x)
    (traverse-section-list (bands x) function)))


(defmethod invalidate-cell ((row grid-section-any) (col-name symbol))
  (traverse-section-list (column-sections  (top-level-widget row))
                    #'(lambda (column)
                        (when (eq (name column) col-name)
                          (invalidate-cell row column)))))

(defmethod invalidate-cell ((row-name symbol) (col grid-section-any))
    (traverse-section-list (row-sections  (top-level-widget col))
                 #'(lambda (row)
                 (when (eq (name row) row-name)
                   (invalidate-cell row col)))))

(defun x-y-width-height (row col)
  (list 0 
         0
        (if col  (total-size col) 2000000)
        (if row  (total-size row) 2000000)))

(defun clip-information-for-cell (row col)
  (deduce-clip (when row (section-parent row))
               (when col (section-parent col))
               row
               col
               (apply 'make-empty-clip (x-y-width-height row col ))))
                       


(defun invalidate-row (row )
  (when-let (result-clip (clip-information-for-cell row nil))
    (apply 'redraw-pinboard-with-redisplay
           (top-level-widget row)
           (clip-to-mask result-clip))))

(defun invalidate-column (column )
  (when-let (result-clip (clip-information-for-cell nil column ))
    (apply 'redraw-pinboard-with-redisplay
           (top-level-widget column)
           (clip-to-mask result-clip))))


(defmethod invalidate-band ((self horizontal-band-mixin))
  (invalidate-row self))

(defmethod invalidate-band ((self vertical-band-mixin))
  (invalidate-column self))

(defun redraw-band (band) (invalidate-band band))
(defun redraw-cell (row column ) (invalidate-cell row column))

(defun redraw-event-cell (self event)
  (redraw-pinboard-with-redisplay self
                                  (grid-event-cell-x event) 
                                  (grid-event-cell-y event)

                                  (grid-event-width event) (grid-event-height event)))
  
(defvar *destroy-overlay* #+mswindows nil #-mswindows t)
(defvar *wrap-overlay-with-interface* t)

(defun reset-state-of-grid (self &optional (focus t))
  (with-slots (overlay) self
    (when overlay
      (if *destroy-overlay*
          (progn 
            (setf (capi:layout-description self)
                  (delete overlay (capi:layout-description self)))
            (capi:destroy overlay)
            (when focus (capi:set-pane-focus self))
            (setf (slot-value self 'editing-panes) nil
                  (slot-value self 'editing-interface) nil))
         (capi:hide-pane overlay))
      (setq overlay nil))))




(defun update-and-reset-grid (self)
  (with-slots (overlay) self
    (when overlay
      (perform-contents-update self overlay)
      (reset-state-of-grid self))))
  
(defvar *x-adjust-for-ensure-visible*)
(defvar *y-adjust-for-ensure-visible*)

(defun cell-focus-change (row column on)
  (when-let (focus-function (if (grid-band-p row) ; make sure the slot is there BWM 1-20-21
                              (band-focus-function row)))
      (funcall focus-function row column on))
  (when-let (focus-function (if (grid-band-p column) ; ditto
                              (band-focus-function column)))
    (funcall focus-function row column on)))

(defun unset-focus (grid)
  (with-slots (focus-coordinates focus-row focus-column) grid
    (when focus-row
       (cell-focus-change focus-row focus-column nil))
      
    (setf focus-row nil)
    (when focus-coordinates
       (apply 'redraw-pinboard-with-redisplay grid focus-coordinates))
    (setf focus-coordinates nil)))

(defmethod set-focus (grid row  col )
  (with-slots (focus-coordinates focus-row focus-column) grid
    (unless (and (eq focus-row row ) (eq focus-column col))
      (when focus-row
        (when-let (view-clip (clip-information-for-cell focus-row focus-column))
          (destructuring-bind (x y width height) (clip-to-mask view-clip)
            (capi:redraw-pinboard-layout grid x y width height nil))))
      (setf focus-row row focus-column col)
      (when row
        (let ((*x-adjust-for-ensure-visible* 0)
              (*y-adjust-for-ensure-visible* 0))
          (destructuring-bind (x y width height) 
              (x-y-width-height row col)
            (ensure-visible (section-parent row)
                            (section-parent col)
                            row
                            col
                            x (+ x width)
                            y
                            (+ y height)))
          (setf focus-coordinates 
                (when-let (view-clip (clip-information-for-cell row col))
                  (clip-to-mask view-clip)))
          (when focus-coordinates
            (apply 'redraw-pinboard-with-redisplay grid focus-coordinates))
          (cell-focus-change row col t)
          (values *x-adjust-for-ensure-visible*
                  *y-adjust-for-ensure-visible*))))))

;;; ******************************************************************
;;;            Manipulation protocol on the grid-widget
;;; ******************************************************************

(defmethod column-section ((self grid-widget) name-to-find)
  (with-slots (column-sections row-sections) self
    (or (find name-to-find column-sections :test #'(lambda (x y) (eq x (name y))))
        (find name-to-find row-sections :test #'(lambda (x y) (eq x (name y)))))))

;;; ******************************************************************
;;;            Setup the parents of a particular section
;;; ******************************************************************

(defmethod setup-parent ((self grid-section-any) other)
  (declare (ignore self other)))

(defmethod setup-parent :after ((self grid-section-any) other)
  (with-slots (parent) self
    (setf parent other)))

(defmethod setup-parent ((self grid-section) other)
  (declare (ignore other))
  (with-slots (bands) self
    (dolist (item bands)
      (setup-parent item self))))

;;; ******************************************************************
;;;           Mouse control infrastructure
;;; ******************************************************************




(defstruct grid-event 
  x 
  y

  cell-x
  cell-y
  width
  height
  modifier   ;;; :control, :shift or :meta
  displayer  ;;; efectively caches, because can be computed from row and column
  data-object
  )
  
(defun apply-event-to-cell (grid-widget x y func modifier)
  (with-slots (row-sections column-sections) grid-widget
    (multiple-value-bind (col pos off size)
        (find-relevant-cell column-sections x 0)
      (declare (ignorable pos))
      (multiple-value-bind (row pos2 off2 size2)
          (find-relevant-cell row-sections y 0)
        (declare (ignorable pos2))
        (when (and row col)
          (let ((event (make-grid-event
                        :x x
                        :y y
                        :modifier modifier
                        :cell-x (- x off)
                        :cell-y (- y off2)
                        :width size
                        :height size2
                        :displayer (cell-displayer row col)
                        :data-object (cell-data-object row col))))
            (funcall func
                     grid-widget
                     row col
                     event)))))))

(defmethod find-relevant-cell ((sections list) coord pos)
  (dolist (item sections nil)
    (multiple-value-bind (found? pos offset size)
        (find-relevant-cell item coord pos)
      (when found?
        (return-from find-relevant-cell (values found? pos offset size))))
    (incf pos)
    (decf coord (cell-size item))))
  
(defmethod find-relevant-cell ((section grid-section) coord pos)
  (when (< coord (total-size section))
    (with-slots (scrollable-p scrollbar bands size) section
      (when scrollable-p
        (incf coord (capi:range-slug-start scrollbar)))
      (decf coord (thickness-adjustment (slot-value section (if (typep section 'horizontal-section-mixin)
                                                                'i-top-border-spec
                                                              'i-left-border-spec))))
      (let ((num-sections (length bands)))
        (if (eql num-sections 0)
            (values section pos coord size)
          (if (eql num-sections 1)
              (find-relevant-cell (first bands) coord pos)
            (progn
              (loop for band in bands
                    for position from 0
                    do (multiple-value-bind (found? pos offset size)
                           (find-relevant-cell band coord position)
                         (when found?
                           (return-from find-relevant-cell (values found? pos offset size))))
                    (decf coord (total-size band)))
              nil)))))))

(defmethod find-relevant-cell ((section grid-band) coord pos)
  (when (< coord (total-size section))
    (with-slots ( cell-size) section
          (values section pos coord cell-size))))

;;; ******************************************************************
;;;                Keyboard events
;;; ******************************************************************

(defun grid-escape (self x y char)
  (declare (ignorable x y char))
  (reset-state-of-grid self))


;;; This one is used to call functions that need the focus cell

(defun grid-keyboard-focus-call (self  x y char func &rest args)
  (declare (ignorable x y char))
  (with-slots (focus-row focus-column) self
    (when focus-row
      (apply func self focus-row focus-column args))))


(defun grid-change-row (self row column offset) 
  (let* ((new-index (+ (slot-value row 'index) offset))
         (new-row (find-index-in-sections (row-sections self) new-index)))
    (when new-row
      (set-focus self new-row column))))

;;; This one also wrap around in end of line. I Couldn't be bother to do 
;;; grid-change-row

(defun grid-change-column (self row column offset) 
  (let* ((new-index (+ (slot-value column 'index) offset))
          (col-num (slot-value self 'col-num))
          (rindex (slot-value row 'index)))
    (if (>= new-index col-num)
        (let ((max-row-num (1- (slot-value self 'row-num))))
           (loop (when (or (< new-index col-num)
                      (>= rindex max-row-num))
                 (return))
            (incf rindex)
            (decf new-index col-num))
           (setq row  (find-index-in-sections (row-sections self) rindex)))
    (when  (< new-index 0)
      (loop (when (or (>= new-index 0)
                      (<= rindex 0))
              (return))
            (decf rindex)
            (incf new-index col-num))
          (setq row  (find-index-in-sections (row-sections self) rindex))))
    (let ((new-column (find-index-in-sections (column-sections self) new-index)))
      (when (and new-column row)
        (set-focus self row new-column )))))
        
             





;;; ******************************************************************
;;;                Redisplay logic
;;; ******************************************************************

(defclass grid-sheet (capi:pinboard-object) ())

(defmethod capi:draw-pinboard-object (pinboard (object grid-sheet) &key x y width height)
  (redisplay-grid-widget pinboard object x y (1+ width) (1+ height)))

(defun make-callback (layout x y width height)
  (let ((old-state nil))
    #'(lambda (interface scrollbar action pos)
        (declare (ignore interface action))
        (reset-state-of-grid layout)
        (invalidate-display-cache layout)
        (unless (eql pos old-state)
          (let* ((x-new x)
                 (y-new y)
                 (width-new width)
                 (height-new height))
            (capi:with-geometry layout
              (redraw-pinboard-with-redisplay layout
                                              (or x-new 0)
                                              (or y-new 0)
                                              (or width-new capi:%width%)
                                              (or height-new capi:%height%)))))
        (setf old-state (capi:range-slug-start scrollbar)))))


;;; ******************************************************************
;;;    Calculate the dimensions of the various sections of the grid
;;; ******************************************************************

(defun calculate-size (grid-widget)
  (with-slots (width height) grid-widget
    (if (and width height)
        (values width height)
      (with-slots (row-sections column-sections horizontal-scrollbars vertical-scrollbars
                                scrollbar-width scrollbar-height) grid-widget
        (let ((new-width 0)
              (new-height 0))
          (dolist (column-section column-sections)
            (incf new-width (total-size column-section)))
          (dolist (row-section row-sections)
            (incf new-height (total-size row-section)))
          (when horizontal-scrollbars
            (incf new-height scrollbar-height))
          (when vertical-scrollbars
            (incf new-width scrollbar-width))
          (values (setq width new-width) (setq height new-height)))))))

;;; ******************************************************************
;;;       Macro to draw a line only if the area is not clipped out
;;; ******************************************************************

(defun draw-if-intersects (pinboard-layout x y width height x1 y1 width1 height1)
  (unless (no-interaction x y width height x1 y1 width1 height1)
    (gp:draw-rectangle pinboard-layout
                         x 
                         y
                         width
                         height
                         :filled t)))

;;; ******************************************************************
;;;              The redisplay code for the grid
;;; ******************************************************************

(defmacro walking-grid-structure ((grid this-column-section this-row-section start-x-coord start-y-coord column-index row-index) &body body)
  (rebinding (grid)
    (with-unique-names (start-x column-section col 
                                start-y row-section row)
      `(let ((,start-x 0))
         (loop for ,column-section in (column-sections ,grid)
             for ,col from 0
             do
             (let ((,start-y 0))
               (loop for ,row-section in (row-sections ,grid)
                     for ,row from 0
                     do
                     (let ((,start-x-coord ,start-x)
                           (,start-y-coord ,start-y)
                           (,row-index ,row)
                           (,column-index ,col)
                           (,this-column-section ,column-section )
                           (,this-row-section ,row-section))
                       (declare (ignorable ,start-x-coord ,start-y-coord ,row-index ,column-index))
                       ,@body)
                     (incf ,start-y (total-size ,row-section))))
             (incf ,start-x (total-size ,column-section)))))))

(defun redisplay-grid-widget (grid-widget drawn-pinboard-object x y width height)
  (declare (ignore drawn-pinboard-object))
  (with-slots (scrollbar-height scrollbar-width
                                i-right-border-spec
                                i-left-border-spec
                                i-top-border-spec
                                i-bottom-border-spec
                                vertical-scrollbars
                                horizontal-scrollbars
                                ) grid-widget
    (multiple-value-bind (grid-width grid-height)
        (calculate-size grid-widget)

      ;;; Draw the top level borders of the grid
      (let ((top-border-thickness (thickness-adjustment i-top-border-spec))
            (bottom-border-thickness (thickness-adjustment i-bottom-border-spec))
            (left-border-thickness (thickness-adjustment i-left-border-spec))
            (right-border-thickness (thickness-adjustment i-right-border-spec))

            (s-w (and vertical-scrollbars scrollbar-width))
            (s-h (and horizontal-scrollbars scrollbar-height)))

        (grid-draw-border  grid-widget 0 0 grid-width 0 i-top-border-spec t)
        (grid-draw-border  grid-widget 0 grid-height grid-width grid-height i-bottom-border-spec nil)
        
        (when s-h
          (let ((top-of-scrollbar  (- grid-height s-h)))
            (grid-draw-border  grid-widget 0 top-of-scrollbar grid-width top-of-scrollbar
                           i-bottom-border-spec nil)))
      
      (grid-draw-border  grid-widget 0 0 0 grid-height i-left-border-spec t)
      (grid-draw-border  grid-widget grid-width 0 grid-width grid-height i-right-border-spec nil)

      (when s-w
        (let ((left-of-scrollbar  (- grid-width s-w)))
           (grid-draw-border grid-widget left-of-scrollbar  0 left-of-scrollbar grid-height
                                   i-right-border-spec nil)))

      ;;; Walk the sections taking the clip with us as we go

      (let* ((left-x (max x left-border-thickness))
             (top-y   (max y top-border-thickness))
             (right-x  (min (+ x width)
                             (- grid-width (or s-w 0)
                                right-border-thickness )))
             (bottom-y (min (+ y height)
                             (- grid-height (or s-h 0)
                                 bottom-border-thickness )))
             (start-clip (make-empty-clip left-x
                                          top-y
                                          (- right-x left-x)
                                          (- bottom-y top-y))))
        (walking-grid-structure (grid-widget column-section row-section x y col row)
          (let* ((total-width (total-size column-section) )
                 (total-height  (total-size row-section) )
                 (the-clip
                  (merge-clip start-clip x y 
                              total-width
                              total-height)))
            (when the-clip
              (draw-grid grid-widget row-section column-section x y col row the-clip)))))))))


;;; ******************************************************************
;;;    Draw the grid borders
;;; ******************************************************************

(defun draw-vertical-borders  (pinboard-layout
                              grid-row-section  
                              column
                              x y
                               )
  (with-slots (i-left-border-spec i-right-border-spec) column
    (let* ((bottom  (+ y (total-size grid-row-section)))
           (right (+ x (total-size column))))

        (grid-draw-border pinboard-layout x y x bottom i-left-border-spec t)
        (grid-draw-border pinboard-layout right y right bottom i-right-border-spec nil))))

(defun draw-horizontal-borders (pinboard-layout
                              row
                              grid-column-section 
                              x y)
  (with-slots (i-bottom-border-spec i-top-border-spec) row
    (let* ((right (+ x (total-size grid-column-section)))
           (bottom (+ y (total-size row))))
       (grid-draw-border pinboard-layout x y right y i-top-border-spec t)
       (grid-draw-border pinboard-layout x bottom right bottom i-bottom-border-spec nil))))

  

;;; ******************************************************************
;;;   Find the relevant grid cells to draw 
;;; ******************************************************************

(defmethod draw-grid (pinboard-layout
                      (grid-row-section grid-section) 
                      column
                      x y
                      col row clip)
  (with-slots (scrollable-p scrollbar bands i-top-border-spec i-bottom-border-spec) grid-row-section
    (let ((height (total-size grid-row-section)))
    (let ((the-clip (merge-clip clip nil  y  
                                nil   height
                                       )))
      (when the-clip

        (gp:with-graphics-mask (pinboard-layout (clip-to-mask clip))

         (draw-horizontal-borders pinboard-layout grid-row-section column x y )
         (let* ((tbt   (thickness-adjustment i-top-border-spec) )
               (adjuested-y (+ y tbt)))
         (when-let (in-clip (merge-clip  the-clip nil (+ y tbt)
                                                      
                                         nil
                                         (- height  (thickness-adjustment i-bottom-border-spec) tbt  )))
           
           (let ((start-y adjuested-y))
             (when scrollable-p (decf start-y (capi:range-slug-start scrollbar)))
             (loop for band in bands
                   do (draw-grid pinboard-layout band column 
                                 x 
                                 start-y
                                 col row
                                 in-clip)
                   (incf start-y (total-size band))))))))))))
  
(defmethod draw-grid (pinboard-layout
                      row-data
                      (grid-column-section grid-section)
                      x y
                      col row clip)
  (with-slots (scrollable-p scrollbar bands 
                            i-left-border-spec i-right-border-spec) grid-column-section
    (let ((width (total-size grid-column-section)))
    (let ((the-clip (merge-clip clip  x  nil width nil)))
      (when the-clip
        (gp:with-graphics-mask (pinboard-layout (clip-to-mask clip))

          (draw-vertical-borders pinboard-layout row-data grid-column-section x y )
          (let* ((lbt  (thickness-adjustment i-left-border-spec) )
                (adjusted-x(+ x lbt)))

            (when-let (in-clip (merge-clip the-clip  adjusted-x nil
                                           (- width  (thickness-adjustment i-right-border-spec) lbt) nil))
            (let ((start-x adjusted-x))
              (when scrollable-p (decf start-x  (capi:range-slug-start scrollbar)))
              (loop for band in bands
                    do (draw-grid pinboard-layout row-data band 
                                 start-x 
                                  y col row
                                  in-clip)
                    (incf start-x (total-size band))))))))))))

(defmethod draw-grid (pinboard-layout
                      (row-data  grid-band)
                      (column-data grid-band)
                      x y
                      col row clip)
  (let* ((height (total-size row-data))
        (width (total-size column-data))
        (tbt (thickness-adjustment (slot-value row-data 'i-top-border-spec)))
        (bbt (thickness-adjustment (slot-value row-data 'i-bottom-border-spec)))
        (lbt (thickness-adjustment (slot-value column-data 'i-left-border-spec)))
        (rbt (thickness-adjustment (slot-value column-data 'i-right-border-spec)))
        (adjusted-x (+ x lbt))
        (adjusted-y  (+ y tbt))
        (adjusted-width (- width lbt rbt))
        (adjusted-height (- height tbt bbt)))

    (when-let (after (merge-clip clip adjusted-x adjusted-y adjusted-width adjusted-height))

      (draw-horizontal-borders pinboard-layout row-data column-data X Y  )
      (draw-vertical-borders   pinboard-layout row-data column-data X Y  )
      (gp:with-graphics-translation (pinboard-layout adjusted-x adjusted-y)

        (gp:with-graphics-mask (pinboard-layout (clip-to-mask after))

          (draw-cell row-data column-data pinboard-layout adjusted-width adjusted-height))))))

;;;; ****************************************************
;;;; Inline editor, hack for getting escape and return to work.
;;;; The buttons are of size 0, so are not vsible, but get the 
;;; escape and return.
;;; On GTK that doesn't hapen, and instead teh escape and return go 
;;; to the widget, and the grid-escape and perform-cell-action do
;;; the actual operations




(capi:define-interface inline-editing-interface ()
  ((pane :initform nil :initarg :pane))
  (:panes
    (buttons
     capi:push-button-panel
     :items 
     (list (make-instance 'capi:push-button 
                          :cancel-p t
                          :selection-callback 'finish-editing
                          :callback-type :interface-data
                          :data nil

                          :visible-min-height 0
                          :visible-max-height 0)
           
           (make-instance ' capi:push-button
                          :default-p t
                          :selection-callback 'finish-editing
                          :data t
                          :visible-min-height 0
                          :callback-type :interface-data
                          :visible-max-height 0))
     :visible-min-width 0 
     :accepts-focus-p nil
           )
   )
  (:layouts
   (main-layout
    capi:column-layout
    nil
    :y-gap 0
    )
    ))

(defun finish-editing (interface data)
  (let ((parent (capi:element-parent interface)))
    (when data (perform-contents-update parent   (slot-value interface 'pane)))
    (reset-state-of-grid parent)))

(defun wrap-pane-for-editing (interface pane)
  (if *wrap-overlay-with-interface*
      (let ((layout (slot-value interface 'main-layout)))
        (setf (capi:layout-description layout)
              (list pane (slot-value interface 'buttons)
                    )
              (capi:layout-y-ratios layout)   '(1 nil )

              (slot-value interface 'pane) pane)
        interface)
     pane))

(defmethod perform-contents-update  (self  (ep inline-editing-interface))
  (perform-contents-update self (slot-value ep 'pane)))

(defun force-some-geometry (pane)
  (capi:with-geometry pane
    (setf capi:%x% 0 
          capi:%y% 0 
          capi:%width% 7
          capi:%height% 1)))

(defun create-editing-interface (self)
  (Or (slot-value self 'editing-interface)
      (let ((new   (make-instance 'inline-editing-interface
                                  
                                  :visible-min-height nil
                                  :visible-min-width nil
                                  :visible-max-width nil
                                  :visible-max-height nil)))
        (force-some-geometry new)
        
        (setf (capi:layout-description self)
              (cons new (capi:layout-description self)))
        (dolist (x  (grid-widget-editing-panes self))
          (capi:hide-pane x))
        (setf (slot-value self 'editing-interface)
              new))))
;;; ******************************************************************
;;;    callbacks functionality
;;; ******************************************************************



(defun update-modified-cell (gw value)
  (write-cell-value (slot-value gw 'current-row)
                    (slot-value gw 'current-column) value))




(defmethod re-initialize-editing-pane (self row column (ep capi:option-pane))
  (let ((displayer (cell-displayer row column)))
    (let ((range (when-let (range-values-getter (band-range-reader displayer))
                   (funcall range-values-getter (cell-data-object row column)))))
      (with-slots ( test-function) displayer
        (setf (capi:collection-items ep) range 
              (capi:collection-test-function ep) test-function
              (capi:choice-selected-item ep) (read-cell-value row column)
              (capi:collection-print-function ep) (or (band-print-function displayer) 'print-to-string)
)))))

(defmethod perform-contents-update (self  (ep capi:option-pane))
  (update-modified-cell self (capi:choice-selected-item ep)))


(defmethod re-initialize-editing-pane (self row column (ep capi:text-input-pane))
  (setf (capi:text-input-pane-text ep) (read-cell-value row column))
        
  (setf (capi:text-input-pane-callback ep)
        #'(lambda (&rest x) 
            (declare (ignorable x))
            (update-and-reset-grid self))))

(defmethod perform-contents-update  (self  (ep capi:text-input-pane))
  (update-modified-cell self (capi:text-input-pane-text ep)))

(defmethod editing-pane-type ((x t)) nil)


(defun maybe-edit-contents (self row column )
  (let ((ept (editing-pane-type (cell-displayer row column))))
    (when ept
       (setf (slot-value self 'current-row) row
              (slot-value self 'current-column) column)
       (let* ((ep (or (find ept (grid-widget-editing-panes self) :key 'type-of)
                     (let ((new (make-instance ept
                                               :callback-type :data 
                                               :visible-min-width nil 
                                               :visible-min-height nil
                                               :visible-max-width nil
                                               :visible-max-height nil
                                               :min-width nil
                                               :min-height nil
                                               :width 0
                                               :height 0
                                               :x 0
                                               :y 0
                                               :font (capi:simple-pane-font self))))
                       (force-some-geometry new)
                       (unless *wrap-overlay-with-interface*
                         (setf (capi:layout-description self)
                               (cons new (capi:layout-description self)))
                         (dolist (x  (grid-widget-editing-panes self))
                           (capi:hide-pane x)))
                       (push new  (grid-widget-editing-panes self))
                       
                       new)))

             (interface  (if *wrap-overlay-with-interface*
                             (create-editing-interface self)
                           ep)))
        (re-initialize-editing-pane self row column ep)
        (setq interface (wrap-pane-for-editing interface ep))
      (with-slots (overlay) self
       

        (setq overlay interface)
       
        (destructuring-bind (x y width height)
          (clip-to-mask  (clip-information-for-cell row column))
        (setf (capi:pinboard-pane-position interface) (values  x y))
        (setf (capi:pinboard-pane-size interface) (values width height)))
        (capi:show-pane interface)
        (capi:set-pane-focus ep))))))






(defmethod cell-release (self
                         (row grid-band) (column grid-band)
                       event)
 
  (when-let (release-function (band-release-function (grid-event-displayer event)))
    (funcall release-function  row column  event)))

(defun perform-cell-action (self row column)
  (with-slots (overlay) self
    (if  overlay
        (finish-editing overlay t)
      (progn 
        (maybe-update-check-box row column )
        (maybe-edit-contents self row column )
        (when-let (release-function (band-action-function (cell-displayer row column)))
          (funcall release-function  row column  ))))))


(defmethod cell-action (self
                         (row grid-band) (column grid-band)
                       event)
  (perform-cell-action self row column))


(defmethod maybe-update-check-box ( row column )
  (when-let (check-box-function (slot-value (cell-displayer row column)
                                                'check-box-function))
    (when-let (data-object (cell-data-object row column))
      (let ((on (funcall check-box-function data-object)))
        (unless (eql on :disabled) ;bwm
          (funcall check-box-function data-object (not on))
          (invalidate-cell row column))))))

(defmethod cell-click :around (self row column
                                    event)
  (reset-state-of-grid self)
  (when (or (accepts-focus-p row)
             (accepts-focus-p column))
    (multiple-value-bind (x-adjust y-adjust)
        (set-focus self row column )
      (when (next-method-p)
        (when x-adjust (decf (grid-event-x event) x-adjust))
        (when y-adjust (decf (grid-event-y event) y-adjust))
        (call-next-method self row column event)))))

(defmethod cell-click (self row column
                            event)
  (if (and (grid-band-p row) (slot-value row 'band-dragging)) ; validate BWM 1/20/21
      (start-column-dragging self row column event)
    (if (and (grid-band-p column) (slot-value column 'band-dragging)) ; validate BWM 1/20/21
         (start-row-draging self row column event)
      (when-let (click-function (band-click-function (grid-event-displayer event)))
        (funcall click-function  row column  event)))))

(defmethod cell-motion :around (self row column
                                    event)
  (declare (ignore event ))
  (with-slots (focus-row focus-column) self
    (when (and (eq focus-row row) (eq focus-column column))
      (when (next-method-p)
        (call-next-method)))))

(defmethod cell-motion (self row column
                      event)
  (when-let (function (band-motion-function (grid-event-displayer event)))
    (funcall function  row column event)))

(defmethod cell-release :around (self row column
                                     event)
  (with-slots (focus-row focus-column) self
    (when (and (eq focus-row row) (eq focus-column column))
      (when (next-method-p)
        (call-next-method)))))

(defmethod cell-release (self row column
                              event)
  (when-let (function (band-release-function (grid-event-displayer event)))
    (funcall function row column event))
  )



;;; ******************************************************************
;;;    Control routines for drawing bits and pieces
;;; ******************************************************************

(defmethod draw-cell  (row column  pane width height)
  (declare (ignore  width height))
  (let ((displayer (cell-displayer row column)))
    (when-let (check-box-function  (slot-value displayer 'check-box-function) )
      (let ((check-box-coordinates  (slot-value displayer 'check-box-coordinates) )
            (check-box-selected-p (funcall check-box-function (cell-data-object row column))))
        (destructuring-bind (x y width height) check-box-coordinates
            (unless (eql check-box-selected-p :disabled) ;bwm
              (let ((left (+ x width))
                    (bottom (+ y height)))
                (let ((lines (list x y x bottom 
                                   left y left bottom
                                   x y left y
                                   x bottom left bottom)))
                  (declare (dynamic-extent lines))
                  (gp:draw-lines pane lines))
                (when check-box-selected-p
                  (let ((lines (list x y left bottom 
                                     x bottom left y)))
                    (declare (dynamic-extent lines))
                    (gp:draw-lines pane lines))))))))
  
    (when-let (print-function  (band-print-function displayer))
      (let ((data (read-cell-value row column)))
        (gp:draw-string pane
                        (funcall print-function data)
                        (slot-value displayer 'text-x-offset)
                        (slot-value displayer 'text-y-offset)
                        ;; (cell-text-graphic-args row column)
                        )))
    (when-let (drawing-function  (band-drawing-function displayer))
      (funcall drawing-function row column  pane width height))))



(defmethod deduce-clip ((self grid-section) col-parent row col the-clip)
  (with-slots (scrollable-p scrollbar i-top-border-spec i-bottom-border-spec) self
    (let* ((child-position (position row (bands self)))
           (tbt (thickness-adjustment i-top-border-spec))
           (height (+ tbt (apply '+ (mapcar 'total-size (subseq (bands self) 0 child-position)))))
           (bbt (thickness-adjustment i-bottom-border-spec)))
                
      (when scrollable-p
        (decf height (capi:range-slug-start scrollbar)))
      (when-let (new-clip (merge-clip (translate-clip-y the-clip height) nil tbt nil (- (total-size self) tbt bbt)))
                 (deduce-clip (section-parent self) col-parent self col
                              new-clip)))))

(defmethod deduce-clip (row-parent (self grid-section) row col the-clip)
  (with-slots (scrollable-p scrollbar i-left-border-spec i-right-border-spec) self
    (let* ((child-position (position col (bands self)))
            (lbt (thickness-adjustment i-left-border-spec))
             (rbt (thickness-adjustment i-right-border-spec))
           (width (+ lbt(apply '+ (mapcar 'total-size (subseq (bands self) 0 child-position))))))

      (when scrollable-p
        (decf width (capi:range-slug-start scrollbar)))
      (when-let (new-clip (merge-clip (translate-clip-x the-clip width) lbt nil (- (total-size self) LBT RBT) nil))
        (deduce-clip row-parent (section-parent self) row self
                     new-clip)))))

(defmethod deduce-clip ((row-parent grid-widget)
                        (col-parent grid-widget)
                        row
                        col
                        the-clip)
  (with-slots (column-sections row-sections i-top-border-spec i-bottom-border-spec i-left-border-spec i-right-border-spec scrollbar-width scrollbar-height) row-parent
    (multiple-value-bind (grid-width grid-height) (calculate-size row-parent)
      (let* ((col-position (position col column-sections))
             (row-position (position row row-sections))
             
             (tbt (thickness-adjustment i-top-border-spec))
             (bbt (thickness-adjustment i-bottom-border-spec))
             (sh (or scrollbar-height 0))
             (lbt (thickness-adjustment i-left-border-spec))
             (rbt (thickness-adjustment i-right-border-spec))
             (height (apply '+ (mapcar 'total-size (subseq row-sections 0 row-position))))
             (sw (or scrollbar-width 0))
             (width  (apply '+ (mapcar 'total-size (subseq column-sections 0 col-position)))))
        (when-let (result-clip (merge-clip (translate-clip-y (translate-clip-x the-clip width) height) 
                                           rbt tbt (- grid-width lbt rbt sw )
                                           (- grid-height tbt bbt sh )))
          result-clip)))))




(defmethod deduce-clip ((row-parent grid-widget)
                        (col-parent  NULL)
                        row
                        col
                        the-clip)
  (with-slots ( row-sections) row-parent
    (multiple-value-bind (grid-width grid-height) (calculate-size row-parent)
      (let* ((row-position (position row row-sections))
             (height (apply '+ (mapcar 'total-size (subseq row-sections 0 row-position)))))
        (when-let (result-clip (merge-clip (translate-clip-y  the-clip height) 
                                           0 0 grid-width grid-height))
          result-clip)))))




(defmethod deduce-clip ((row-parent  NULL)
                        (col-parent grid-widget)
                        row
                        col
                        the-clip)
  (with-slots (column-sections ) row-parent
    (multiple-value-bind (grid-width grid-height) (calculate-size col-parent)
      (let* ((col-position (position col column-sections))
             (width (apply '+ (mapcar 'total-size (subseq column-sections 0 col-position)))))
        (when-let (result-clip (merge-clip  (translate-clip-x the-clip width)
                                           0 0 grid-width grid-height))
          result-clip)))))

(defmethod ensure-visible ((row-parent grid-section) column-parent row column min-x max-x min-y max-y)
  (with-slots (scrollable-p scrollbar) row-parent
    (let* ((child-position (position row (bands row-parent)))
           (height (apply '+ (mapcar 'total-size (subseq (bands row-parent) 0 child-position)))))
      (let ((min-y (+ min-y height))
            (max-y (+ max-y height)))
        (when scrollable-p
          (let ((slug-start (capi:range-slug-start scrollbar))
                (slug-end (capi:range-slug-end scrollbar)))
          (unless (<= slug-start min-y max-y 
                      (+ (total-size row-parent)
                         slug-start))
            (when-let (amount 
                       (if (> slug-start  min-y)
                           (- slug-start min-y)
                         (when (< (- max-y min-y) (total-size row-parent))
                           (- slug-end max-y))))
              
              (decf *y-adjust-for-ensure-visible* amount)
              (decf slug-start amount)
              (capi:range-set-sizes scrollbar
                                    :slug-start slug-start
                                    :slug-end (- slug-end amount))
              (invalidate-display-cache (top-level-widget row-parent))
              (funcall (capi:range-callback scrollbar) 
                       (top-level-widget column-parent)
                       scrollbar
                       :move slug-start)))))
        (ensure-visible (section-parent row-parent) column-parent row-parent 
                        column min-x max-x  min-y max-y)))))

(defmethod ensure-visible (row-parent (column-parent grid-section) row column min-x max-x min-y max-y)
  (with-slots (scrollable-p scrollbar) column-parent
    (let* ((child-position (position column (bands column-parent)))
           (width (apply '+ (mapcar 'total-size (subseq (bands column-parent) 0 child-position)))))
      (let ((min-x (+ min-x width))
            (max-x (+ max-x width)))
        (when scrollable-p
           (let ((slug-start (capi:range-slug-start scrollbar))
                 (slug-end (capi:range-slug-end scrollbar)))
          (unless (<= slug-start min-x max-x
                      (+ (total-size column-parent)
                         slug-start))
            (when-let (amount (if (> slug-start  min-x)
                                  (- slug-start min-x)
                                (when  (< (- max-x min-x) (total-size column-parent))
                                  (- slug-end max-x))))
              (decf *x-adjust-for-ensure-visible* amount)
              (decf slug-start amount)
              (capi:range-set-sizes scrollbar 
                                    :slug-start slug-start
                                    :slug-end (- slug-end amount))
              (invalidate-display-cache (top-level-widget column-parent))
              (funcall (capi:range-callback scrollbar) 
                       (top-level-widget column-parent)
                       scrollbar
                       :move slug-start)))))
        (ensure-visible row-parent (section-parent column-parent) row 
                        column-parent min-x max-x min-y max-y)))))

(defmethod ensure-visible (row-parent column-parent row column min-x max-x min-y max-y)
  (declare (ignore row-parent column-parent row column min-x max-x min-y max-y)))


(defun start-column-dragging (self row column event)
  (let ((x (grid-event-x event))
        (cell-x  (grid-event-cell-x event)))
  (with-slots (inside-drag inside-drag-offset
                           inside-drag-direction
                           inside-drag-other-data) self
    (capi:with-geometry self
      (setf inside-drag-offset
            (- cell-x x)
            inside-drag-direction
            :vertical
            inside-drag-other-data
            (list x (section-parent column) row)
            inside-drag
            (list cell-x 0  (cell-size column) capi:%height%))))))

(defun start-row-draging (self row  column event)
  (let ((y (grid-event-y event))
        (cell-y (grid-event-cell-y event)))
  (with-slots ( inside-drag inside-drag-offset
                                 inside-drag-direction
                                 inside-drag-other-data) self
    (capi:with-geometry self
      (setf inside-drag-offset
            (- cell-y y)
            inside-drag-direction
            :horizontal
            inside-drag-other-data
            (list y (section-parent row) column)
            inside-drag
            (list 0 cell-y capi:%width% (cell-size row)))))))

(defun cell-motion-drag-check (self row column event)
  (with-slots (inside-drag inside-drag-direction)
      self
    (if (not inside-drag)
        (cell-motion self row column event)
      (do-drag inside-drag-direction self (grid-event-x event)  (grid-event-y event)))))

(defmacro invalidate-region (self old-x old-y old-width old-height new-x new-y new-width new-height)
  (rebinding (self old-x old-y old-width old-height new-x new-y new-width new-height)
    (with-unique-names (min-x min-y)
      `(let ((,min-x (min ,old-x ,new-x))
             (,min-y (min ,old-y ,new-y)))
         (redraw-pinboard-with-redisplay 
          ,self
          ,min-x
          ,min-y
          (- (max (+ ,old-x ,old-width) (+ ,new-x ,new-width)) ,min-x)
          (- (max (+ ,old-y ,old-height) (+ ,new-y ,new-height)) ,min-y))))))

(defun do-drag (dir self x y)
  (with-slots (inside-drag inside-drag-offset) self
    (destructuring-bind (old-x old-y old-width old-height) inside-drag
      #-mswindows
      (let (mods)
        (multiple-value-setq (x y mods)
            (capi-library:representation-pointer-position (capi-internals:representation self) t))
        (unless (intersection mods '(:button1 :button2 :button3 :button4 :button5))
          (return-from do-drag)))
      (let ((new-x (if (eq dir :horizontal) old-x (+ x inside-drag-offset)))
            (new-width old-width)
            (new-y (if (eq dir :horizontal) (+ y inside-drag-offset) old-y))
            (new-height old-height))
        (setf inside-drag (list new-x new-y new-width new-height))
        (invalidate-region self old-x old-y old-width old-height new-x new-y new-width new-height)))))


(defmethod top-level-widget ((self grid-section-any))
  (top-level-widget (section-parent self)))

(defmethod top-level-widget ((self grid-widget))
  self)

(defmethod movement-scope ((self grid-band)) 
  (section-parent self))
  

(defmethod movement-scope ((self grid-section))
  (section-parent self))

(defun cell-release-drag-check (self
                                row column
                               event)
  (with-slots (inside-drag
               inside-drag-other-data
               inside-drag-direction
               )
      self
    (if (not inside-drag)
        (cell-release self row column event)
      (progn 
        (let ((old-inside-drag inside-drag)
              (vertical-p (eq inside-drag-direction :vertical)))
          (setf inside-drag nil)
          (apply 'redraw-pinboard-with-redisplay self old-inside-drag)
          (let ((top
               (top-level-widget (second inside-drag-other-data))))
          (let ((ob1 (if vertical-p column row))

                (ob2 (find-relevant-cell 
                      (if vertical-p
                          (column-sections top)
                        (row-sections top))
                      (first inside-drag-other-data)
                      0)))
              (when (eq (movement-scope ob1) (movement-scope ob2))
                (let ((inside (movement-scope ob1)))
                  (unset-focus self)
                  (do-band-movement inside-drag-direction inside
                                          ob1 ob2 (< (if vertical-p
                                                         (- (grid-event-x event)
                                                            (grid-event-cell-x event))
                                                         (- (grid-event-y event)
                                                            (grid-event-cell-y event)))
                                                     (ash (total-size ob1) -1))
                                                       
                                                     )
                  (update-indices self)
                 )))))))))

  
(defun do-band-movement (dir parent band-to band-from before)
  
  (when (not (eq band-from band-to))
    (let ((prev nil)
          (mcons nil)
          (e-cons nil)
          (target nil)
          (tn nil))
        (loop for cons on  (bands parent)
              when (eq (car cons) band-from)
              do (setq e-cons prev mcons cons)
              when (eq (car cons) band-to)
              do (setq target (if before prev cons)
                       tn (if before cons  (cdr cons)))
              do (setq prev cons))
        (unless (eq e-cons target)
          (if e-cons
              (setf (cdr e-cons) (cdr (cdr e-cons)))
            (setf (bands parent) (cdr (bands parent))))
          (if target
              (setf (cdr target) mcons)
            (setf (bands parent) mcons))
          (setf (cdr mcons) tn)
    
        (let ((widget  (top-level-widget parent)))
        (if (eq dir :vertical)
            (dolist (row (row-sections widget))
              (invalidate-cell row parent))
            (dolist (col (column-sections widget))
              (invalidate-cell parent col))))))))

;;; ******************************************************************
;;;     A piece of code for a patch to pinboards
;;; ******************************************************************

(defmethod capi:pinboard-layout-display :after ((pinboard grid-widget)
                                                x y width height)
  (declare (ignore redraw-object))
  (with-slots (inside-drag focus-row focus-column focus-coordinates focus-thickness focus-color drag-colour drag-thickness) pinboard
    (when focus-row
      (unless focus-coordinates
        (setf focus-coordinates 
              (when-let (view-clip (clip-information-for-cell focus-row focus-column))
                (clip-to-mask view-clip))))
      (when focus-coordinates
        (destructuring-bind (x y width height) focus-coordinates
          (let ((thickness-adjust (floor focus-thickness 2)))
            (gp:draw-rectangle pinboard 
                               (+ x thickness-adjust)
                               (+ y thickness-adjust)
                               (- width focus-thickness) (- height focus-thickness) 
                               :foreground focus-color)))))
    (when inside-drag
      (destructuring-bind (x y width height) inside-drag
        (gp:draw-rectangle pinboard x y (1- width) (1- height)
                           :foreground drag-colour
                           :thickness drag-thickness)))))
