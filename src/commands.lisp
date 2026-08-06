
(in-package #:cl-interactive/command)

(defvar *interactive* nil
  "Dynamically bound to T within interactive calls.")
(defvar *current-interactive-command* nil
  "The current command being interactively invoked")
(defvar *current-interactive-arguments* nil
  "The arguments being passed to *current-interactive-command*")

(defclass command (c2mop:standard-generic-function)
  ((interactive-components :initarg :interactive-components
                           :accessor interactive-components)
   (non-interactive-args :initarg :non-interactive-args
                         :accessor non-interactive-args))
  (:metaclass c2mop:funcallable-standard-class)
  (:documentation "The command class. A subclass of generic functions."))

(defstruct (arg-type
            (:constructor make-arg-type (name doc fn)))
  "Data for converting an argument to its desired type"
  (name nil :type symbol :read-only t)
  (doc nil :type (or string null))
  (fn nil :type (function (t) t)))

(defvar *arg-types* (make-hash-table))

(defmacro define-arg-type (name (param-name) &body body)
  "Define or update a conversion function with name NAME that
converts an interactive argument to a specific type"
  (check-type name symbol "Arg types must be a symbol")
  (multiple-value-bind (values declarations doc)
      (alexandria:parse-body body :documentation t)
    (let ((already-existing (gensym "already-existing"))
          (lambda-symb (gensym "lambda-fn"))
          (gethash-form `(gethash ,name *arg-types*))
          (lambda-form `(lambda (,param-name)
                          ,@(when declarations
                              declarations)
                          ,@values)))
      `(let ((,already-existing ,gethash-form)
             (,lambda-symb ,lambda-form))
         (if ,already-existing
             (progn
               (setf (arg-type-doc ,already-existing) ,doc
                     (arg-type-fn ,already-existing) ,lambda-symb)
               ,already-existing)
             (setf ,gethash-form (make-arg-type ,name ,doc
                                                ,lambda-symb)))))))

(defstruct (interactive-spec (:constructor make-interactive-spec
                                 (type symb data converter)))
  (type nil :type (member :default :function :class)
            :read-only t)
  (symb nil :type (or symbol function)
            :read-only t)
  (data nil :read-only t)
  (converter nil :type (or arg-type null)
                          :read-only t))

(defmethod no-applicable-method ((gf command) &rest args)
  (error 'no-applicable-command-implementation
         :command gf
         :arguments args
         :input-method (input-method)))

(defclass interactive-component () ())

(defgeneric read-argument-interactively
    (command argument input-method interactive &key &allow-other-keys)
  (:documentation "Read an argument interactively.

COMMAND - the command object
ARGUMENT - a symbol naming the argument as it appears in DEFINE-COMMAND
INPUT-METHOD - the input method to use
INTERACTIVE - the interactive portion of the command argument"))

(defmacro with-gathered-args (arg-spec name &body body)
  "Place the given arguments and their names into a data structure suitable for
passing to CALL-COMMAND-INTERACTIVELY.

Example creating args var with argument FOO:
(with-gathered-args ((foo \"foo-value\"))
     args
  ...)"
  `(let ((,name (list ,@(mapcar (lambda (x)
                                  `(cons (quote ,(car x))
                                         ,(cadr x)))
                                arg-spec))))
     ,@body))

(defun call-command-interactively (command &key (input-method (input-method))
                                             already-gathered)
  "Call COMMAND interactively using INPUT-METHOD. This function loops through the
interactive argument list and obtains each argument using that list."
  (let ((cmd (if (symbolp command) (symbol-function command) command)))
    (multiple-value-bind (func arg-list)
        (gather-args-interactively cmd
                                   :input-method input-method
                                   :already-gathered already-gathered)
      (when func
        (call-command-with-argument-list func arg-list)))))

(defun gather-args-interactively (command &key (input-method (input-method))
                                            already-gathered)
  "Gather the arguments for COMMAND interactively"
  (declare (optimize (debug 3)))
  (let ((command (if (symbolp command) (symbol-function command) command))
        (*current-input-method* input-method))
    (restart-case
        (cl-interactive/input-method::with-interactive-error-handler (input-method)
          (unless (typep command 'command)
            (error 'not-a-command-error :command command))
          (let ((gathered-symbols (mapcar #'car already-gathered))
                (non-interactive (non-interactive-args command)))

            (finish-output)
            (when (set-difference non-interactive
                                  gathered-symbols)
              (error 'missing-required-arguments-error
                     :command command
                     :missing-arguments (set-difference non-interactive
                                                        gathered-symbols)
                     :given gathered-symbols)))
          (flet ((get-the-argument (arg spec)
                   (declare (type interactive-spec spec))
                   (let* ((it (interactive-spec-type spec))
                          (is (interactive-spec-symb spec))
                          (ia (interactive-spec-data spec))
                          (inst
                            (ecase it
                              ((:default) (if (find-class is)
                                              (apply #'make-instance is ia)
                                              (etypecase is
                                                (function is)
                                                (symbol (symbol-function is)))))
                              ((:class) (apply #'make-instance is ia))
                              ((:function) (etypecase is
                                             (function is)
                                             (symbol (symbol-function is))))))
                          (pre-value
                            (if (typep inst 'interactive-component)
                                (read-argument-interactively command
                                                             arg
                                                             input-method
                                                             inst)
                                (apply inst command input-method arg ia))))
                     (if (interactive-spec-converter spec)
                         (funcall (interactive-spec-converter spec) pre-value)
                         pre-value))))
            (let* ((to-gather (set-difference (interactive-components command)
                                              already-gathered :key #'car))
                   (argument-list
                     (nconc
                      already-gathered
                      (loop for (arg spec) in to-gather
                            collect (cons arg (get-the-argument arg spec))))))
              (values command argument-list))))
      (abort-command ()
        :report "Abort command"
        (values nil)))))

(defun %extract-param-types (ll)
  "Separate the lambda list into POSITIONAL, OPTIONAL, and KEY lists"
  (let ((front ll))
    (flet ((gather-until (keys)
             (do ((head (car front) (car front))
			      (items))
			     ((or (member head keys :test #'eql) (not front)) (nreverse items))
		       (push head items)
		       (setf front (rest front)))))
      (let ((positional (gather-until '(&key &optional)))
            (optional nil)
            (key-lst nil))
        (when (eql (car front) '&optional)
          (setf front (cdr front))
          (setf optional (gather-until '(&key &aux))))
        (when (eql (car front) '&key)
          (setf front (cdr front))
          (setf key-lst (gather-until '(&allow-other-keys &aux))))
        (values positional
                optional
                key-lst)))))

(defun %build-arg-list (ll arguments-list)
  (multiple-value-bind (positionals optionals keys)
      (%extract-param-types ll)
    (flet ((find-matching-positional (arg)
             (let ((found (find arg arguments-list :key #'car)))
                 (if found
                     (cdr found)
                     (error (format nil "No argument in ~S matching ~S"
                                    arguments-list arg))))))
    (nconc
     (mapcar #'find-matching-positional
             positionals)
     (mapcar #'find-matching-positional
             optionals)
     (mapcon (lambda (arg)
               (let* ((inner (car arg))
                      (name (if (listp inner)
                                (second (car inner))
                                inner))
                      (found (find name arguments-list :key #'car)))
                 (when found
                   (list (intern (string name) :keyword) (cdr found)))))
             keys)))))

(defun call-command-with-argument-list (command arguments-list)
  "Invoke COMMAND with the arguments specified by ARGUMENTS-LIST. ARGUMENTS-LIST
is a plist with the key being the name of the argument and the value being
the value of the argument. It's best to use gather-args-interactively to
construct it."
  (declare (optimize (debug 3)))
  (let ((*current-interactive-command* command)
        (*current-interactive-arguments*
          (%build-arg-list (c2mop:generic-function-lambda-list command)
                           arguments-list))
        (*interactive* t))
    (apply *current-interactive-command* *current-interactive-arguments*)))

;; Parsers and helpers for define-command
(defun parse-arguments-for-define-command (arguments-list)
  "Given an list of arguments as provided to DEFINE-COMMAND, return the
arguments as processed for a defgeneric form and an interactive argument list as
multiple values."
  (let ((current-type '&positional)
        (defgeneric-list nil)
        (interactive-components nil)
        (non-interactive-args nil)
        (rest-sentinel nil))
    (flet ((push-interactive (name spec)
             (push (list name spec)
                   interactive-components)))
      (dolist (argument arguments-list (values (reverse defgeneric-list)
                                               (reverse interactive-components)
                                               non-interactive-args))
        (if (member argument '(&optional &rest &key &allow-other-keys))
            (setf current-type argument
                  defgeneric-list (cons argument defgeneric-list))
            (ecase current-type
              ((&rest)
               (when (eql current-type '&rest)
                 (if rest-sentinel
                     (error "Multiple &rest arguments provided")
                     (setf rest-sentinel t)))
               (assert (symbolp argument) (argument)
                       "&REST arguments cannot have interactive components")
               (push argument defgeneric-list))
              ((&positional &optional)
               (multiple-value-bind (argname spec)
                   (parse-positional-optional-rest-argument-for-define-command
                    argument)
                 (push argname defgeneric-list)
                 ;; When no interactive information is given,
                 ;; it's not an interactive argument:
                 (if spec
                     (push-interactive argname spec)
                     (push argname non-interactive-args))))
              ((&key)
               (multiple-value-bind (argkey argname spec)
                   (parse-key-argument-for-define-command argument)
                 (push (list (list argkey argname)) defgeneric-list)
                 (push-interactive argname spec)))
              ((&allow-other-keys)
               (error "&ALLOW-OTHER-KEYS must be the final symbol in~
                     an interactive lambda list"))))))))

(defun parse-positional-optional-rest-argument-for-define-command (argument)
  "Return the argument name, the interactive type, the interactive
symbol, and the interactive arguments as multiple values"
  (cond ((symbolp argument)
         argument)
        ((and (consp argument)
              (symbolp (car argument)))
         (let ((spec (parse-interactive-component (cadr argument))))
           (values (car argument) spec)))
        (t (error 'define-command-invalid-argument-error
                  :argument argument
                  :type "positional/optional/rest"))))

(defun parse-key-argument-for-define-command (argument)
  "Return the argument keyword, argument name, interactive type, interactive
symbol, and interactive arguments as multiple values. For &KEY arguments only."
  (cond ((symbolp argument)
         (values (intern (string argument) :keyword)
                 argument))
        ((and (consp argument)
              (symbolp (car argument)))
         (let ((spec (parse-interactive-component (cadr argument))))
           (values (intern (string (car argument)) :keyword)
                   (car argument)
                   spec)))
        ((and (consp argument)
              (consp (car argument))
              (symbolp (caar argument))
              (symbolp (cadar argument)))
         (let ((spec (parse-interactive-component (cadr argument))))
           (values (caar argument) (cadar argument) spec)))
        (t (error 'define-command-invalid-argument-error
                  :type :key
                  :argument argument))))

(defun %process-interactive-spec (interactive-type interactive-symb details)
  (let ((valid-type '(:default :class :function)))
    (unless (member interactive-type valid-type)
      (error "Invalid interactive component ~S.~%Interactive type must be one of ~S, not ~S"
             interactive-type valid-type interactive-type)))
  ;; Assume that when someone writes #'foo, they want to check that
  ;; a function foo exists at compile time:
  (when (and (eql :function interactive-type)
             (consp interactive-symb)
             (eql 'function (car interactive-symb)))
    (setf interactive-symb (symbol-function (second interactive-symb))))
  (destructuring-bind (&key data type) details
    ;; As an alternative to emitting a constructor call, we could
    ;; impelment MAKE-LOAD-FORM on INTERACTIVE-SPEC, but that
    ;; seems a bit sketchy. It would enable us to move
    ;; some validation higher up in the callstack for this macro,
    ;; but it's not needed right now. We could also leave it alone
    ;; (including the function transformation) until further on,
    ;; but again, we don't do any processing of this data past
    ;; this point.
    `(make-interactive-spec
      ,interactive-type
      ,(if (functionp interactive-symb)
           `(function ,interactive-symb)
           `(quote ,interactive-symb))
      ,(cond
         ((null data) nil)
         ((listp data)
          `(list ,@data))
         (t
          `(list ,data)))
      ,(when type
         `(gethash ,type *arg-types*)))))

(defun parse-interactive-component (component)
  "Return the interactive-spec from the given interactive component."
  (cond ((null component) nil)
        ((symbolp component)
         (%process-interactive-spec :default component nil))
        ((and (consp component)
              (keywordp (car component)))
         (%process-interactive-spec (car component) (cadr component) (cddr component)))
        ((and (consp component)
              (symbolp (car component)))
         (%process-interactive-spec :default (car component) (cdr component)))
        (t (error "Invalid interactive component ~S" component))))

(defmacro with-options ((remaining &rest options) options-list &body body)
  "Bind OPTIONS list to their relevant options in an options list (an alist) for
the duration of BODY. Bind remaining options to REMAINING."
  (let ((r (gensym)))
    `(let* ((,remaining (copy-list ,options-list))
            ,@(mapcar (lambda (k)
                        (let ((kw (intern (string k) :keyword)))
                          `(,k (let ((,r (find ,kw ,remaining :key #'car)))
                                 (setf ,remaining (remove ,r ,remaining))
                                 ,r))))
                      options))
       ,@body)))

 ;; Define command macro
(defmacro define-command (name command-lambda-list &body body)
  "Define a generic function of class command.

NAME will be the name of the generic function.

COMMAND-LAMBDA-LIST is a command lambda list. It contains argument names,
optionally with their interactive components. An interactive component is either
a symbol denoting a class or a function, or a list of such a symbol and its
arguments. If the list contains a keyword as its first element that keyword
determines if the interactive component names a function or a class. If no
keyword is provided then classes are preferred to functions when a class
exists. When constructing a class make-instance is applied to the symbol and its
provided arguments. When calling a function the function is applied to the
command, input method, argument name, and its provided arguments. The shape of a
command lambda list is as follows:

({A | (A [{symbol | ([keyword] symbol argument*)}])}*
 [&optional {A | (A [{symbol | ([keyword] symbol argument*)}])}*]
 [&rest {A | (A [{symbol | ([keyword] symbol argument*)}])}]
 [&key {A | ({A | (keyword-name A)} [{symbol | ([keyword] symbol argument*)}])}*]
 [&allow-other-keys])"
  (let ((components (gensym)))
    (multiple-value-bind (defgen-ll interactive-components
                          non-interactive-args)
        (parse-arguments-for-define-command command-lambda-list)
      (with-options (remaining
                     database
                     canonical-name
                     no-canonical-name)
                    body
        `(progn
           (defgeneric ,name ,defgen-ll
             (:generic-function-class command)
             ,@remaining)
           ,@(unless (cadr no-canonical-name)
               `((add-to-database ,(if database
                                       `(or ,(cadr database)
                                            *default-command-database*)
                                       '*default-command-database*)
                                  #',name
                                  nil
                                  (or (and ,(cadr canonical-name)
                                           (string ,(cadr canonical-name)))
                                      (string ',name)))))
           (let ((,components
                   ,(cons 'list
                          (mapcar (lambda (com)
                                    (if (cadr com)
                                        (list 'list
                                              `(quote ,(car com))
                                              (cadr com)
                                              `(quote ,(caddr com))
                                              (cons 'list (cadddr com)))
                                        com))
                                  interactive-components))))
             (setf (interactive-components #',name) ,components))
           (setf (non-interactive-args #',name) (list ,@(mapcar
                                                         (lambda (x)
                                                           `(quote ,x))
                                                         non-interactive-args)))
           #',name)))))
