;;;; Grand River Transit

;;; TODO Time comparisons will screw up just past midnight, as schedule times
;;; become 24:nn:nn, but `now' is the following day, so lookups in calendar_dates
;;; don't match..... True of GRTransit app too.

(defparameter *work-directory* '(:absolute "home" "gdmalet" "src" "GRT-GTFS" "lisp" "tmp") "Where we're doing it")
(defparameter *working-date* (multiple-value-list (get-decoded-time)) "System's notion of today's date and time")
(defparameter *tomorrow* (multiple-value-list (decode-universal-time (+ (get-universal-time) (* 60 60 24))))
  "Tomorrow's date, used for dealing with times of the form 25:00:00")
(defparameter *tables* () "Root of all the tables read from the input files.")
(defparameter *table-files*
	 '(("agency.txt" . 2)
	   ("calendar_dates.txt" . 32)
	   ("calendar.txt" . 32)
	   ("fare_attributes.txt" . 32)
	   ("routes.txt" . 256)
	   ("shapes.txt" . 131072)
	   ("stops.txt" . 3072)
	   ("stop_times.txt" . 524288)
	   ("trips.txt" . 32768))
	   "A list of files to slurp in, and approximate number of lines per file.")

(defun get-table (table)
  "Return the hash table associates with the table name."
  (cdr (assoc table *tables* :test #'equalp)))

;;;$ grep 815413 trips.txt 
;;; 7,13FALL-All-Weekday-06,815413,"31 Lexington to UW",0,130424,70061
;;;
;;;$ grep 815413 stop_times.txt 
;;; [...]
;;; 815413,08:17:00,08:17:00,1123,9,0,0
;;;
;;; -- it's excluded by service:
;;;$ grep 13FALL-All-Weekday-06, calendar.txt
;;; 13FALL-All-Weekday-06,1,1,1,1,1,0,0,20131011,20131018

;;; Read table files, and store the results in pairlis *table-files*.
;;; Fetch with (assoc "trips" *tables* :test #'equalp)

(defun main ()
  (setq *tables* ())
  (mapc (lambda (p)
		  (setq *tables*
				(acons (string-to-symbol-name (car p))
					   (load-table (car p) (make-hash-table :test 'equal :size (cdr p)))
					   *tables*)))
		*table-files*))

(defun show-next-bus-at-stop (stop-id)
  "A pretty printed version of next-bus-at-stop."
  (mapc
   (lambda (bus)
	 (format t "~A ~4A \"~A\"~%"
			   (car bus)
			   (cadr bus)
			   (caddr bus)))
   (next-bus-at-stop stop-id))
  t)

(defun all-busses-at-stop (stop-id)
  "Return all the busses for a stop, in time order."

  ;; For each trip using a stop, stash time, routes number & headsign.
  (sort
   (mapcar
	(lambda (trip-details)
	  (multiple-value-bind (id sign) (trip-id-to-route-id (car trip-details))
		(list (cadr trip-details) id sign trip-details)))
	(trips-using-stop stop-id))
   (lambda (a b)
	 (string< (car a) (car b)))))

(defun next-bus-at-stop (stop-id)
  "Return route and time of the next bus to come through the given stop."
  (let ((routes-hash (make-hash-table :test 'equal :size 42))
		(routes-list ())
		(now (multiple-value-bind (s m h)
				 (decode-universal-time (get-universal-time))
			   (format nil "~2,'0d:~2,'0d:~2,'0d" h m s))))

	;; For each trip using a stop, stash all the routes numbers & headsigns.
	;; Use a hash table to track times.
	(mapc
	 (lambda (trip-details)
	   (multiple-value-bind (id sign) (trip-id-to-route-id (car trip-details))
		 (let ((this-time (cadr trip-details)))
		   ;(format t "this time ~A, now ~A~%" this-time now)
		   (when (string> this-time now)
			 (let ((latest (gethash (cons id sign) routes-hash)))
			   (if (or (null latest) (string< this-time latest))
				   (setf (gethash (cons id sign) routes-hash) this-time)))))))
	 (trips-using-stop stop-id))

	;; Fetch unique copies off the hash table and stash in list for return.
	(maphash
	 (lambda (route-sign time)
	   (push (list time (car route-sign) (cdr route-sign)) routes-list))
	 routes-hash)

	(sort
	 routes-list
	 (lambda (a b)
	   (string< (car a) (car b))))))

(defun routes-using-stop (stop-id)
  "Return a list of conses of routes and headsigns that use a particular stop."
  (let ((routes-hash (make-hash-table :test 'equal :size 42))
		(routes-list ()))

	;; For each trip using a stop, stash all the routes numbers & headsigns.
	;; Use a hash table to get a unique copy of each.
	(mapc
	 (lambda (trip-details)
	   (multiple-value-bind (id sign) (trip-id-to-route-id (car trip-details))
		 (setf (gethash (cons id sign) routes-hash) t)))
	 (trips-using-stop stop-id))

	;; Fetch unique copies off the hash table and stash in list for return.
	(maphash
	 (lambda (route-sign foo)
	   (declare (ignore foo))
	   (push route-sign routes-list))
	 routes-hash)
  routes-list))

;;; TODO -- this is a sequential search through values to return a key....
(defun trip-id-to-route-id (trip-id)
  "Return the route-id & trip headsign for a given trip-id"
  (maphash 
	 (lambda (route-id trips-instance)
	   (case (type-of trips-instance)
		 ('CONS
		  (mapc (lambda (route)
				  (when (equalp trip-id (slot-value route 'trip-id))
					(return-from trip-id-to-route-id
					  (values
					   route-id
					   (slot-value route 'trip-headsign)))))
				trips-instance))
		 (t
		  (if (equalp trip-id (slot-value trips-instance 'trip-id))
			  (return-from trip-id-to-route-id
				(values
				 route-id
				 (slot-value trips-instance 'trip-headsign)))))))
	 (get-table "trips")))

;;; TODO -- this is a sequential search through values to return a key....
;;; If a route does a loop back to a stop, there will be dups in this list.
(defun trips-using-stop (stop-id)
  "Return a list of trips, with arrival & departure times,
that use a particular stop. May contain dups."
  (let ((trip-ids ()))
	(maphash 
	 (lambda (trip-id stop-time-instance)
	   ;;(format t "Key: ~A, value ~A~%" trip-id stop-time-instance)
	   (case (type-of stop-time-instance)
		 ('CONS
		  (mapc (lambda (trip-stop)
				  (when (equalp stop-id (slot-value trip-stop 'stop-id))
					;; TODO could exit mapc early after next line
					(push (list trip-id
								(slot-value trip-stop 'arrival-time)
								(slot-value trip-stop 'departure-time))
						  trip-ids)))
				stop-time-instance))
		 (t
		  ;;; A trip with one stop?
		  (if (equalp stop-id (slot-value stop-time-instance 'stop-id))
			  (push (list trip-id
						  (slot-value stop-time-instance 'arrival-time)
						  (slot-value stop-time-instance 'departure-time))
					trip-ids)))))
	 (get-table "stop-times"))
	trip-ids))

;;; Load data from a standard text input file.
;;; If file is "foo.txt", stores each line from the file in an instance
;;; of a class that is created from the header line of the file. Each
;;; column heading is a slot name, and the truncated file name "foo" is
;;; used to name the class.
;;; The returned has table contains all these instances, each indexed by
;;; the first column in the file. If the value in that column is unique,
;;; then gethash will return an instance of the class; else it will
;;; return a list of instances.
(defun load-table (from-file table)
  "Load data from a standard test file."
  (with-open-file (s (make-pathname :directory *work-directory*
					 :name from-file)
					 :direction :input
					 :if-does-not-exist :error)

	;; Set up initial values: column names from first line of file, class name
	;; from the passed in file name, and define the class using the headers.
	(do* ((headers (mapcar #'string-to-symbol-name (parsed-input-line s)))
		  (class-name (string-to-symbol-name from-file))
		  (c (eval (list 'defclass (intern class-name) () (mapcar (lambda (x) (list (intern x))) headers))))
		  (l (parsed-input-line s) (parsed-input-line s)))
		((null l) table)

	  ;; For each subsequent line of the file, make and populate an instance of the class.
	  (let ((c (make-instance c))
			(h (gethash (car l) table)))

		;; Populate this instance of the class
		(mapc (lambda (name val) (setf (slot-value c (intern name)) val)) headers l)

		;; Stash the instance in the hash table
		(case (type-of h)
		  (null						; store instance of class
		   (setf (gethash (car l) table) c))
		  ('CONS						; add to the list
		   (setf (gethash (car l) table) (append h (list c))))
		  (t							; class presumably, so create list
		   (setf (gethash (car l) table) (list h c))))))))

;;; Given "foo_bar.txt", return "FOO-BAR"
(defun string-to-symbol-name (str)
  "Convert give file name to a symbol name."
  (substitute #\- #\_
			  (string-upcase
			   (subseq str 0 (position #\. str)))))

;;; Read one line from the input stream, and break up into a list.
;;; Separator is a comma.
;;; TODO - this will b0rk if a quoted string contains a comma or quote.
(defun parsed-input-line (stream)
  "Return a list of values from one line of comma separated input stream."
  (let ((line (read-line stream nil)))
    (when line
      (loop with last-comma = -1
	    while last-comma
	    collect (cleanup (subseq line (1+ last-comma)
			    (setf last-comma (position #\, line :start (1+ last-comma)))))))))

;;; Clean up crap coming from the input files.
(defun cleanup (str)
  "Trim leading & trailing space from string, & remove quotes."
  (when (> (length str) 0)
	(let ((s (string-trim '(#\Space) str)))
	  (when (eql (aref s 0) #\")
		(if (eql (aref s (1- (length s))) #\")
			(return-from cleanup (subseq s 1 (1- (length s))))
			(format t "~A is badly formed (quotes)~%" s)))
	  (return-from cleanup s))))

(defun timestr-to-epoch (timestr)
  "Convert a given HH:MM:SS time string to seconds since the epoch."
  ;(format t "time conversion \"~A\"~%" timestr)
  (let ((hms
		 (loop with last-colon = -1
			while last-colon
			collect (parse-integer (subseq timestr (1+ last-colon)
							(setf last-colon (position #\: timestr :start (1+ last-colon))))))))

	(if (> (nth 0 hms) 23)
		  (encode-universal-time
		   (nth 2 hms)					;second
		   (nth 1 hms)					;minute
		   (- (nth 0 hms) 24)			;hour
		   (nth 3 *tomorrow*)			;day of month
		   (nth 4 *tomorrow*)			;month
		   (nth 5 *tomorrow*))			;year
		  (encode-universal-time
		   (nth 2 hms)					;second
		   (nth 1 hms)					;minute
		   (nth 0 hms)					;hour
		   (nth 3 *working-date*)			;day of month
		   (nth 4 *working-date*)			;month
		   (nth 5 *working-date*)))))			;year
