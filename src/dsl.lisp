;;; dsl.lisp - Declarative UI DSL for Charmed
;;; Provides a React-like declarative syntax for building terminal UIs

(in-package #:charmed)

;;; ============================================================
;;; Reactive State
;;; ============================================================

(defclass reactive-state ()
  ((value :initarg :value :accessor state-value)
   (subscribers :initform nil :accessor state-subscribers
                :documentation "Functions to call when value changes")
   (name :initarg :name :accessor state-name :initform nil
         :documentation "Optional name for debugging"))
  (:documentation "A reactive state cell that notifies subscribers on change."))

(defmethod print-object ((state reactive-state) stream)
  (print-unreadable-object (state stream :type t)
    (format stream "~@[~A ~]~S" (state-name state) (state-value state))))

(defun make-state (initial-value &optional name)
  "Create a reactive state cell with INITIAL-VALUE."
  (make-instance 'reactive-state :value initial-value :name name))

(defun state-get (state)
  "Get the current value of STATE."
  (state-value state))

(defun state-set (state new-value)
  "Set STATE to NEW-VALUE and notify subscribers."
  (unless (equal (state-value state) new-value)
    (setf (state-value state) new-value)
    (dolist (sub (state-subscribers state))
      (funcall sub new-value)))
  new-value)

(defun state-subscribe (state callback)
  "Subscribe CALLBACK to be called when STATE changes."
  (push callback (state-subscribers state)))

(defun state-unsubscribe (state callback)
  "Unsubscribe CALLBACK from STATE."
  (setf (state-subscribers state)
        (remove callback (state-subscribers state))))

(defun state-update (state fn)
  "Update STATE by applying FN to current value."
  (state-set state (funcall fn (state-value state))))

(defmacro with-state (bindings &body body)
  "Create reactive state bindings.
   Example: (with-state ((count 0) (name \"\")) ...)"
  `(let ,(mapcar (lambda (binding)
                   (destructuring-bind (name init) binding
                     `(,name (make-state ,init ',name))))
                 bindings)
     ,@body))

;;; ============================================================
;;; Component Protocol
;;; ============================================================

(defclass component ()
  ((props :initarg :props :accessor component-props :initform nil
          :documentation "Properties passed to component")
   (children :initarg :children :accessor component-children :initform nil
             :documentation "Child components")
   (widget :accessor component-widget :initform nil
           :documentation "The rendered widget instance")
   (parent :initarg :parent :accessor component-parent :initform nil
           :documentation "Parent component")
   (mounted-p :accessor component-mounted-p :initform nil
              :documentation "Whether component is mounted")
   (event-handlers :initform (make-hash-table :test 'eq) :accessor component-event-handlers
                   :documentation "Event handlers for this component"))
  (:documentation "Base class for declarative UI components."))

(defgeneric component-render (component)
  (:documentation "Render the component, returning a widget or component tree."))

(defgeneric component-mount (component)
  (:documentation "Called when component is added to the UI."))

(defgeneric component-unmount (component)
  (:documentation "Called when component is removed from the UI."))

(defgeneric component-update (component old-props new-props)
  (:documentation "Called when component props change."))

(defmethod component-mount ((c component))
  (setf (component-mounted-p c) t))

(defmethod component-unmount ((c component))
  (setf (component-mounted-p c) nil))

(defmethod component-update ((c component) old-props new-props)
  (declare (ignore old-props))
  (setf (component-props c) new-props))

;;; ============================================================
;;; Virtual DOM Node
;;; ============================================================

(defstruct vnode
  "Virtual DOM node for diffing."
  (type nil :type (or symbol class))
  (props nil :type list)
  (children nil :type list)
  (key nil)
  (widget nil))

(defun vnode-equal (a b)
  "Check if two vnodes are structurally equal."
  (and (eq (vnode-type a) (vnode-type b))
       (equal (vnode-props a) (vnode-props b))
       (= (length (vnode-children a)) (length (vnode-children b)))
       (every #'vnode-equal (vnode-children a) (vnode-children b))))

;;; ============================================================
;;; Element Creation (JSX-like)
;;; ============================================================

(defun create-element (type props &rest children)
  "Create a virtual DOM element.
   TYPE is a widget class or component function.
   PROPS is a plist of properties.
   CHILDREN are nested elements."
  (make-vnode :type type
              :props props
              :children (flatten-children children)
              :key (getf props :key)))

(defun flatten-children (children)
  "Flatten nested lists of children, removing nils."
  (let ((result nil))
    (labels ((collect (item)
               (cond
                 ((null item) nil)
                 ((and (listp item) (not (vnode-p item)))
                  (dolist (child item)
                    (collect child)))
                 (t (push item result)))))
      (dolist (child children)
        (collect child)))
    (nreverse result)))

;;; ============================================================
;;; DSL Macros
;;; ============================================================

(defmacro ui (&body body)
  "Top-level UI definition macro.
   Transforms declarative syntax into create-element calls."
  (expand-ui-body body))

(defun expand-ui-body (body)
  "Expand UI body forms."
  (if (= (length body) 1)
      (expand-ui-form (first body))
      `(progn ,@(mapcar #'expand-ui-form body))))

(defun expand-ui-form (form)
  "Expand a single UI form."
  (cond
    ;; Nil
    ((null form) nil)
    ;; String literal -> text node
    ((stringp form) form)
    ;; Symbol -> variable reference
    ((symbolp form) form)
    ;; List starting with keyword -> element with props
    ((and (listp form) (keywordp (first form)))
     (expand-element-form form))
    ;; List starting with symbol -> might be element or Lisp form
    ((and (listp form) (symbolp (first form)))
     (let ((sym (first form)))
       (cond
         ;; Known DSL elements
         ((member sym '(vbox hbox stack grid spacer))
          (expand-layout-form form))
         ;; Conditional forms
         ((eq sym 'when)
          (expand-when-form form))
         ((eq sym 'if)
          (expand-if-form form))
         ((eq sym 'cond)
          (expand-cond-form form))
         ;; List iteration
         ((eq sym 'for-each)
          (expand-for-each-form form))
         ;; Let binding
         ((eq sym 'let)
          (expand-let-form form))
         ;; Otherwise, treat as regular Lisp
         (t form))))
    ;; Other lists -> regular Lisp
    (t form)))

(defun expand-element-form (form)
  "Expand (:element-type props... children...) form."
  (let* ((type (first form))
         (rest (rest form))
         (props nil)
         (children nil))
    ;; Parse props (keyword-value pairs) and children
    (loop while rest do
      (let ((item (pop rest)))
        (if (and (keywordp item) rest)
            (let ((value (pop rest)))
              (push item props)
              (push value props))
            (push item children))))
    (setf props (nreverse props))
    (setf children (nreverse children))
    ;; Map element type to widget class
    (let ((widget-type (element-type-to-class type)))
      `(create-element ',widget-type
                       (list ,@props)
                       ,@(mapcar #'expand-ui-form children)))))

(defun element-type-to-class (type)
  "Map DSL element type keyword to widget class."
  (case type
    (:panel 'panel)
    (:list 'scrollable-list)
    (:input 'text-input)
    (:progress 'progress-bar)
    (:status 'status-bar)
    (:menu 'menu)
    (:menu-bar 'menu-bar)
    (:table 'table-widget)
    (:form 'form-widget)
    (:tree 'tree-view)
    (:dialog 'dialog)
    (:alert 'alert-dialog)
    (:confirm 'confirm-dialog)
    (:prompt 'input-dialog)
    (:text 'text-node)
    (:button 'button-widget)
    (t (intern (string-upcase (subseq (symbol-name type) 1)) :charmed))))

(defun expand-layout-form (form)
  "Expand layout forms like (vbox ...) and (hbox ...)."
  (let* ((layout-type (first form))
         (rest (rest form))
         (props nil)
         (children nil))
    ;; Parse props and children
    (loop while rest do
      (let ((item (pop rest)))
        (if (and (keywordp item) rest (not (keywordp (first rest))))
            (let ((value (pop rest)))
              (push item props)
              (push value props))
            (push item children))))
    (setf props (nreverse props))
    (setf children (nreverse children))
    (let ((layout-class (case layout-type
                          (vbox 'vertical-layout)
                          (hbox 'horizontal-layout)
                          (stack 'stack-layout)
                          (grid 'grid-layout)
                          (spacer 'spacer))))
      `(create-element ',layout-class
                       (list ,@props)
                       ,@(mapcar #'expand-ui-form children)))))

(defun expand-when-form (form)
  "Expand (when condition body...) form."
  (let ((condition (second form))
        (body (cddr form)))
    `(when ,condition
       ,@(mapcar #'expand-ui-form body))))

(defun expand-if-form (form)
  "Expand (if condition then else) form."
  (let ((condition (second form))
        (then-form (third form))
        (else-form (fourth form)))
    `(if ,condition
         ,(expand-ui-form then-form)
         ,(expand-ui-form else-form))))

(defun expand-cond-form (form)
  "Expand (cond clauses...) form."
  `(cond ,@(mapcar (lambda (clause)
                     (list (first clause)
                           (expand-ui-form (second clause))))
                   (rest form))))

(defun expand-for-each-form (form)
  "Expand (for-each var in list body...) form."
  ;; (for-each item in items (:list-item ...))
  (let ((var (second form))
        (in-kw (third form))
        (list-expr (fourth form))
        (body (cddddr form)))
    (declare (ignore in-kw))
    `(mapcar (lambda (,var)
               ,@(mapcar #'expand-ui-form body))
             ,list-expr)))

(defun expand-let-form (form)
  "Expand (let bindings body...) form."
  (let ((bindings (second form))
        (body (cddr form)))
    `(let ,bindings
       ,@(mapcar #'expand-ui-form body))))

;;; ============================================================
;;; Rendering Engine
;;; ============================================================

(defclass ui-renderer ()
  ((root :initarg :root :accessor renderer-root :initform nil
         :documentation "Root vnode")
   (mounted-widgets :initform (make-hash-table :test 'eq) :accessor renderer-widgets
                    :documentation "Map from vnode to widget")
   (container :initarg :container :accessor renderer-container :initform nil
              :documentation "Container panel for rendering")
   (dirty-p :accessor renderer-dirty-p :initform t
            :documentation "Whether re-render is needed"))
  (:documentation "Manages rendering of virtual DOM to widgets."))

(defvar *current-renderer* nil
  "The currently active UI renderer.")

(defun make-renderer (&key x y width height)
  "Create a new UI renderer."
  (make-instance 'ui-renderer
                 :container (make-instance 'panel
                                           :x (or x 1)
                                           :y (or y 1)
                                           :width (or width 80)
                                           :height (or height 24)
                                           :border nil)))

(defun render-vnode (vnode x y width height)
  "Render a vnode to a widget at the given position and size."
  (when (null vnode)
    (return-from render-vnode nil))
  (let* ((type (vnode-type vnode))
         (props (vnode-props vnode))
         (children (vnode-children vnode)))
    (cond
      ;; String -> just return it (for text content)
      ((stringp vnode) vnode)
      ;; Layout types
      ((member type '(vertical-layout horizontal-layout))
       (render-layout vnode type x y width height))
      ;; Regular widget
      (t
       (let ((widget (apply #'make-instance type
                            :x x :y y
                            :width width :height height
                            props)))
         (setf (vnode-widget vnode) widget)
         ;; Render children if applicable
         (when (and children (slot-exists-p widget 'children))
           (let ((child-widgets (mapcar (lambda (child)
                                          (render-vnode child x y width height))
                                        children)))
             (when (slot-boundp widget 'children)
               (setf (slot-value widget 'children) child-widgets))))
         widget)))))

(defun render-layout (vnode type x y width height)
  "Render a layout vnode."
  (let* ((props (vnode-props vnode))
         (children (vnode-children vnode))
         (gap (or (getf props :gap) 0))
         (direction (if (eq type 'vertical-layout) :vertical :horizontal))
         (n (length children))
         (widgets nil))
    (when (zerop n)
      (return-from render-layout nil))
    (if (eq direction :vertical)
        ;; Vertical layout
        (let* ((child-height (floor (- height (* gap (1- n))) n))
               (current-y y))
          (dolist (child children)
            (let ((w (render-vnode child x current-y width child-height)))
              (when w (push w widgets)))
            (incf current-y (+ child-height gap))))
        ;; Horizontal layout
        (let* ((child-width (floor (- width (* gap (1- n))) n))
               (current-x x))
          (dolist (child children)
            (let ((w (render-vnode child current-x y child-width height)))
              (when w (push w widgets)))
            (incf current-x (+ child-width gap)))))
    (nreverse widgets)))

(defun render-to-container (renderer vnode)
  "Render vnode tree to the renderer's container."
  (let* ((container (renderer-container renderer))
         (x (panel-x container))
         (y (panel-y container))
         (width (panel-width container))
         (height (panel-height container)))
    (setf (renderer-root renderer) vnode)
    (render-vnode vnode x y width height)))

;;; ============================================================
;;; Diffing and Reconciliation
;;; ============================================================

(defun diff-vnodes (old-vnode new-vnode)
  "Compute differences between old and new vnodes.
   Returns a list of patch operations."
  (cond
    ;; Both nil -> no change
    ((and (null old-vnode) (null new-vnode))
     nil)
    ;; Old nil, new exists -> create
    ((null old-vnode)
     (list (list :create new-vnode)))
    ;; Old exists, new nil -> remove
    ((null new-vnode)
     (list (list :remove old-vnode)))
    ;; Different types -> replace
    ((not (eq (vnode-type old-vnode) (vnode-type new-vnode)))
     (list (list :replace old-vnode new-vnode)))
    ;; Same type -> check props and children
    (t
     (let ((patches nil))
       ;; Check props
       (unless (equal (vnode-props old-vnode) (vnode-props new-vnode))
         (push (list :update-props old-vnode (vnode-props new-vnode)) patches))
       ;; Check children
       (let ((old-children (vnode-children old-vnode))
             (new-children (vnode-children new-vnode)))
         (loop for i from 0 below (max (length old-children) (length new-children))
               for old-child = (nth i old-children)
               for new-child = (nth i new-children)
               do (setf patches (nconc patches (diff-vnodes old-child new-child)))))
       patches))))

(defun apply-patches (patches)
  "Apply a list of patches to update the UI."
  (dolist (patch patches)
    (let ((op (first patch)))
      (case op
        (:create
         ;; Widget will be created on next render
         nil)
        (:remove
         (let* ((vnode (second patch))
                (widget (vnode-widget vnode)))
           (when widget
             (setf (panel-visible-p widget) nil))))
        (:replace
         (let* ((old-vnode (second patch))
                (old-widget (vnode-widget old-vnode)))
           (when old-widget
             (setf (panel-visible-p old-widget) nil))
           ;; New widget created on render
           ))
        (:update-props
         (let* ((vnode (second patch))
                (new-props (third patch))
                (widget (vnode-widget vnode)))
           (when widget
             (apply-props-to-widget widget new-props))))))))

(defun apply-props-to-widget (widget props)
  "Apply property updates to a widget."
  (loop for (key value) on props by #'cddr
        do (let ((slot-name (prop-to-slot key)))
             (when (and slot-name (slot-exists-p widget slot-name))
               (setf (slot-value widget slot-name) value)))))

(defun prop-to-slot (prop-key)
  "Convert a property keyword to a slot name."
  (case prop-key
    (:items 'items)
    (:value 'value)
    (:title 'title)
    (:label 'label)
    (:width 'width)
    (:height 'height)
    (:x 'x)
    (:y 'y)
    (:visible 'visible-p)
    (:active 'active-p)
    (t nil)))

;;; ============================================================
;;; Event System
;;; ============================================================

(defvar *event-handlers* (make-hash-table :test 'equal)
  "Global event handler registry.")

(defun register-handler (event-type handler &optional key)
  "Register an event handler."
  (let ((id (or key (gensym "HANDLER"))))
    (setf (gethash (cons event-type id) *event-handlers*) handler)
    id))

(defun unregister-handler (event-type key)
  "Unregister an event handler."
  (remhash (cons event-type key) *event-handlers*))

(defun dispatch-event (event-type &rest args)
  "Dispatch an event to all registered handlers."
  (maphash (lambda (key handler)
             (when (eq (car key) event-type)
               (apply handler args)))
           *event-handlers*))

(defun handle-widget-event (widget event-type &rest args)
  "Handle an event from a widget."
  (let ((handlers (component-event-handlers widget)))
    (when handlers
      (let ((handler (gethash event-type handlers)))
        (when handler
          (apply handler args))))))

;;; ============================================================
;;; Application Framework
;;; ============================================================

(defclass app ()
  ((state :initarg :state :accessor app-state :initform nil
          :documentation "Application state (reactive)")
   (renderer :accessor app-renderer :initform nil
             :documentation "UI renderer")
   (render-fn :initarg :render :accessor app-render-fn :initform nil
              :documentation "Function that returns the UI tree")
   (running-p :accessor app-running-p :initform nil
              :documentation "Whether app is running")
   (on-key :initarg :on-key :accessor app-on-key :initform nil
           :documentation "Key event handler")
   (on-mount :initarg :on-mount :accessor app-on-mount :initform nil
             :documentation "Called when app mounts")
   (on-unmount :initarg :on-unmount :accessor app-on-unmount :initform nil
               :documentation "Called when app unmounts"))
  (:documentation "A declarative UI application."))

(defun make-app (&key render state on-key on-mount on-unmount)
  "Create a new application."
  (make-instance 'app
                 :render render
                 :state state
                 :on-key on-key
                 :on-mount on-mount
                 :on-unmount on-unmount))

(defun app-render (app)
  "Render the application UI."
  (when (app-render-fn app)
    (funcall (app-render-fn app) (app-state app))))

(defun app-run (app)
  "Run the application main loop."
  (let* ((size (terminal-size))
         (width (first size))
         (height (second size)))
    (setf (app-renderer app) (make-renderer :x 1 :y 1 :width width :height height))
    (setf (app-running-p app) t)
    (when (app-on-mount app)
      (funcall (app-on-mount app) app))
    (with-raw-terminal ()
      (unwind-protect
           (loop while (app-running-p app) do
             ;; Render UI
             (let ((vnode (app-render app)))
               (render-to-container (app-renderer app) vnode))
             ;; Render all widgets
             (app-render-widgets app)
             (force-output)
             ;; Handle input
             (let ((key (read-key-with-timeout 50)))
               (when key
                 (if (app-on-key app)
                     (funcall (app-on-key app) app key)
                     (app-default-key-handler app key)))))
        (when (app-on-unmount app)
          (funcall (app-on-unmount app) app))))))

(defun app-render-widgets (app)
  "Render all widgets in the app."
  (clear-screen)
  (let ((root (renderer-root (app-renderer app))))
    (render-widget-tree root)))

(defun render-widget-tree (node)
  "Recursively render a widget tree."
  (cond
    ((null node) nil)
    ((vnode-p node)
     (let ((widget (vnode-widget node)))
       (when widget
         (panel-render widget))
       (dolist (child (vnode-children node))
         (render-widget-tree child))))
    ((listp node)
     (dolist (item node)
       (render-widget-tree item)))
    ((typep node 'panel)
     (panel-render node))))

(defun app-default-key-handler (app key)
  "Default key handler - quit on 'q' or Escape."
  (when (or (and (key-event-char key) (char= (key-event-char key) #\q))
            (eq (key-event-code key) +key-escape+))
    (app-stop app)))

(defun app-stop (app)
  "Stop the application."
  (setf (app-running-p app) nil))

;;; ============================================================
;;; Convenience Macros
;;; ============================================================

(defmacro defapp (name (&key state) &body body)
  "Define a declarative UI application.
   Example:
   (defapp my-app (:state ((count 0)))
     (vbox
       (:text :value (format nil \"Count: ~D\" (state-get count)))
       (:button :label \"Increment\" 
                :on-click (lambda () (state-update count #'1+)))))"
  (let ((state-bindings (mapcar (lambda (s)
                                  (if (listp s)
                                      `(,(first s) (make-state ,(second s) ',(first s)))
                                      `(,s (make-state nil ',s))))
                                state)))
    `(defun ,name ()
       (let ,state-bindings
         (let ((app (make-app
                     :state (list ,@(mapcar #'first state-bindings))
                     :render (lambda (state)
                               (declare (ignorable state))
                               (ui ,@body)))))
           (app-run app))))))

(defmacro define-component (name (&rest props) &body body)
  "Define a reusable UI component.
   Example:
   (define-component counter (initial-value)
     (with-state ((count initial-value))
       (vbox
         (:text :value (format nil \"~D\" (state-get count)))
         (:button :label \"+\" :on-click (lambda () (state-update count #'1+))))))"
  `(defun ,name (&key ,@props)
     (ui ,@body)))

;;; ============================================================
;;; Built-in Components
;;; ============================================================

(defclass text-node ()
  ((value :initarg :value :accessor text-value :initform ""))
  (:documentation "Simple text display node."))

(defclass button-widget (panel)
  ((label :initarg :label :accessor button-label :initform "Button")
   (on-click :initarg :on-click :accessor button-on-click :initform nil)
   (pressed-p :accessor button-pressed-p :initform nil))
  (:default-initargs :height 1 :border nil)
  (:documentation "Clickable button widget."))

(defmethod panel-render ((button button-widget))
  (let ((x (panel-x button))
        (y (panel-y button))
        (label (button-label button))
        (active (panel-active-p button)))
    (cursor-to y x)
    (if active
        (reverse-video)
        (bold))
    (format t "[ ~A ]" label)
    (reset)))

(defclass spacer ()
  ((size :initarg :size :accessor spacer-size :initform 1))
  (:documentation "Empty space for layouts."))

(defclass stack-layout ()
  ((children :initarg :children :accessor stack-children :initform nil)
   (active-index :initarg :active :accessor stack-active-index :initform 0))
  (:documentation "Stack layout showing one child at a time."))

(defclass grid-layout ()
  ((children :initarg :children :accessor grid-children :initform nil)
   (columns :initarg :columns :accessor grid-columns :initform 2)
   (gap :initarg :gap :accessor grid-gap :initform 1))
  (:documentation "Grid layout with fixed columns."))
