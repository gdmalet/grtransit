;;;; Grand River Transit

;;; TODO Time comparisons will screw up just past midnight, as schedule times
;;; become 24:nn:nn, but `now' is the following day, so lookups in calendar_dates
;;; don't match..... True of GRTransit app too.

;(defpackage :grtransit
;  (:use :common-lisp))
;;  (:export check-passwords))
;(in-package :grtransit)

;; Need this for parsing the JSON returned from the realtime site
(require 'asdf)
(asdf:load-system :jsown)

(defparameter *work-directory* '(:absolute "home" "gdmalet" "src" "GRT-GTFS" "lisp" "tmp") "Where we're doing it")

(defparameter *working-date* (multiple-value-list (get-decoded-time)) "System's notion of today's date and time")
(defparameter *tomorrow* (multiple-value-list (decode-universal-time (+ (get-universal-time) (* 60 60 24))))
  "Tomorrow's date, used for dealing with times of the form 25:00:00")
(defparameter *yesterday* (multiple-value-list (decode-universal-time (- (get-universal-time) (* 60 60 24))))
  "Yesterday's date, used for dealing with times of the form 25:00:00")
(defparameter *today-ymd* (format nil "~D~2,'0D~2,'0D"
								  (nth 5 *working-date*) (nth 4 *working-date*) (nth 3 *working-date*))
  "Today's date in yyyymmdd format, for comparisons with calendar entries.")

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

(defconstant *day-names* '(MONDAY TUESDAY WEDNESDAY THURSDAY FRIDAY SATURDAY SUNDAY)
  "Used with index (day-of-week) to fetch values out of the calendar.")

(defun get-table (table)
  "Return the hash table associated with the table name."
  (cdr (assoc table *tables* :test #'equalp)))

(defun day-of-week ()
  "Return the day of week from *working-date*"
  (nth 6 *working-date*))

(defun main ()
  "Load the files into internal tables, and get ready for work."
  (setq *tables* ())
  (mapc (lambda (p)
		  (setq *tables*
				(acons (string-to-symbol-name (car p))
					   (load-table (car p) (make-hash-table :test 'equal :size (cdr p)))
					   *tables*)))
		*table-files*)

  ;; Create a table giving all trips using a stop, since this is such a common need.
  (setq *tables*
		(acons (string-to-symbol-name "stop-trips")
			   (let ((tbl (make-hash-table :test 'equal
										   :size (cdr (find "stops.txt" *table-files*
															:test (lambda (const l) (string= const (car l))))))))
				 (flet ((save-trip (trip-id instance)
						  ;;(format t "saving trip ~A to stop ~A~%" trip-id (slot-value instance 'stop-id))
						  (push (list trip-id
								  (slot-value instance 'arrival-time)
								  (slot-value instance 'departure-time))
								(gethash (slot-value instance 'stop-id) tbl))))
				   (maphash
					(lambda (trip-id stop-time-instance)
					  (case (type-of stop-time-instance)
						(CONS
						 (mapc (lambda (trip-stop)
								 (save-trip trip-id trip-stop))
							   stop-time-instance))
						(t
						 (save-trip trip-id stop-time-instance))))
					(get-table "stop-times")))
				 tbl)
			   *tables*))
  t)


(defun stops-with-no-trips ()
  "Find bogus stops."
  (maphash
   (lambda (stop-id details)
	 (declare (ignore details))
	 (unless (trips-using-stop stop-id nil)
		 (format t "~A~%" stop-id)))
   (get-table "stops")))

(defun next-bus-at-stop* (stop-id)
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
  ;; For each trip using a stop, stash trip id, times, route number & headsign.
  (sort
   (mapcar
	(lambda (trip-details)
	  (multiple-value-bind (id sign)
		  (get-route-details-for-trip (car trip-details))
		(list (car trip-details)
			  (cadr trip-details)
			  (caddr trip-details)
			  id sign)))
	(trips-using-stop stop-id))
   (lambda (a b)
	 (string< (cadr a) (cadr b)))))

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
	   (multiple-value-bind (id sign)
		   (get-route-details-for-trip (car trip-details))
		 (let ((this-time (cadr trip-details)))
		   ;;(format t "this time ~A, now ~A~%" this-time now)
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

(defun trips-for-route-by-headsign (stop-id headsign)
  "Return trips (in time order) passing through a particular stop,
limited to one route."
  (let ((trips ()))
	(mapcar
	 (lambda (trip-details)
	   (multiple-value-bind (id sign)
		   (get-route-details-for-trip (car trip-details))
		 (declare (ignore id))
		 (if (string= sign headsign)
			 (push trip-details trips))))
	 (trips-using-stop stop-id))

	(sort 
	 trips
	 (lambda (a b)
	   (string< (cadr a) (cadr b))))))

(defun routes-using-stop (stop-id)
  "Return a list of conses of routes and headsigns that use a particular stop."
  (let ((routes-hash (make-hash-table :test 'equal :size 42))
		(routes-list ()))

	;; For each trip using a stop, stash all the routes numbers & headsigns.
	;; Use a hash table to get a unique copy of each.
	(mapc
	 (lambda (trip-details)
	   (multiple-value-bind (id sign)
		   (get-route-details-for-trip (car trip-details))
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
(defun get-route-details-for-trip (trip-id)
  "Return the route-id, trip headsign & service-id for a given trip-id"
  (maphash 
	 (lambda (route-id trips-instance)
	   (case (type-of trips-instance)
		 (CONS
		  (mapc (lambda (route)
				  (when (equalp trip-id (slot-value route 'trip-id))
					(return-from get-route-details-for-trip
					  (values
					   route-id
					   (slot-value route 'trip-headsign)
					   (slot-value route 'service-id)))))
				trips-instance))
		 (t
		  (if (equalp trip-id (slot-value trips-instance 'trip-id))
			  (return-from get-route-details-for-trip
				(values
				 route-id
				 (slot-value trips-instance 'trip-headsign)
				 (slot-value trips-instance 'service-id)))))))
	 (get-table "trips")))

(defun trip-runs-today-p (trip-id)
  "Check whether a certain trip runs today."
  (multiple-value-bind (route-id headsign service-id)
	  (get-route-details-for-trip trip-id)
	(declare (ignore route-id headsign))
	(check-calendar service-id)))

;;; TODO -- this is a sequential search through values to return a key....
;;; If a route does a loop back to a stop, there will be dups in this list.
(defun trips-using-stop (stop-id &optional (verify t))
  "Return a list of trips, with arrival & departure times,
that use a particular stop. May contain dups."
  (unless verify
	(return-from trips-using-stop
	  (gethash stop-id (get-table "stop-trips"))))

  (let ((trip-details ()))
	(dolist (deets (gethash stop-id (get-table "stop-trips")))
	  (when (trip-runs-today-p (car deets))
		(push deets trip-details))
	  trip-details)
	trip-details))

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
		  (null							; store instance of class
		   (setf (gethash (car l) table) c))
		  (CONS							; add to the list
		   (setf (gethash (car l) table) (append h (list c))))
		  (t							; class presumably, so create list
		   (setf (gethash (car l) table) (list h c))))))))

;;; Given "foo_bar.txt", return "FOO-BAR"
(defun string-to-symbol-name (str)
  "Convert given file name to a symbol name."
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

(defun check-calendar (service-id)
  "Check dates in the calendar to see if a bus using the given service
id should be running today. Does this by checking for exceptions in
calendar-dates, and if the schedule is current in the calendar."
  (case (check-calendar-exception service-id)
	("1" t)								; added for today
	("2" nil)							; removed for today
	(t									; no exception, check schedule
	 (let ((schedule (gethash service-id (get-table "calendar"))))
	   (and
		(string>= *today-ymd* (slot-value schedule 'START-DATE))
		(string<= *today-ymd* (slot-value schedule 'END-DATE))
		(string= "1" (slot-value schedule (nth (day-of-week) *day-names*))))))))

(defun check-calendar-exception (service-id)
  "Check the calendar-dates to see if a trip runs today.
Returns string '1' if it is added today, '2' removed, nil if no exception."
  (flet ((maybe-return (instance)
		   (if (string>= *today-ymd* (slot-value instance 'DATE))
			   (return-from check-calendar-exception
				 (slot-value instance 'EXCEPTION-TYPE)))))

	(let ((override (gethash service-id (get-table "calendar-dates"))))
	  (case (type-of override)
		(CONS
		 (mapc #'maybe-return override)
		 nil)
		(CALENDAR-DATES
		 (maybe-return override)
		 nil)
		(t nil)))))						;no override

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


;;; See http://www.clisp.org/impnotes/socket.html for other examples
;;; Returns simple-base-string of returned web page, minus headers.
(defun wget-text (host page &optional (port 80))
  (with-open-stream (socket (socket:socket-connect port host :external-format :DOS))
	(format socket "GET ~A HTTP/1.1~%Host: ~A~%Connection: close~2%" page host)
   (LOOP :with content-length :for line = (READ-LINE socket nil nil)
      ;; header is separated from the data with a blank line
      :until (ZEROP (LENGTH line)) :do
	  ;;(format t "read header len ~A \"~A\"~%" (length line) line)
      (WHEN (and
			 (> (length line) #1=#.(LENGTH #2="Content-Length: "))
			 (STRING-equal line #2# :end1 #1#))
        (SETQ content-length (PARSE-INTEGER line :start #1#))
		;;(format t "got content length ~A~%" content-length))
		;; this will not work if the server does not supply the content-length header
		:finally (RETURN (LET ((data (MAKE-ARRAY content-length
												 :element-type '(unsigned-byte 8)))
							   (fill-pointer))
						   (read-line socket nil nil) ; swallow empty line
						   (SETF (STREAM-ELEMENT-TYPE socket) '(unsigned-byte 8)) ; binary input
						   ;; read the whole file in one system call
						   (setf fill-pointer (read-sequence data socket))
						   ;;(format t "fill-pointer: ~A~%~A~%" fill-pointer data)
						   (map 'string #'code-char
								(subseq data 0 (min content-length fill-pointer)))))))))

(defun next-bus-at-stop-realtime (stop-id)
  "Return realtime data with route and time of the next bus to come
through the given stop."
  (let ((routes-hash (make-hash-table :test 'equal :size 42))
		(times-list ()))
	;; Get a unique list of route numbers, so we can grab realtime data for them
	(mapc
	 (lambda (bus)
	   (multiple-value-bind (route headsign)
		   (values (car bus) (cdr bus))
		 (let ((this-one (gethash route routes-hash)))
		   (when (null this-one)
			 (format t "stashing ~A ~A~%" route headsign)
			 (setf (gethash route routes-hash) t)))))
	 (routes-using-stop stop-id))

	;; Get RT data for each unique route
	(maphash
	 (lambda (route foo)
	   (format t "fetching ~A~%" route)
	   (push (get-realtime stop-id route) times-list))
	 routes-hash)
	times-list))

(defun get-realtime (stop-id route)
  "Get the realtime data for a given route at given stop.
Returns a list of JSO objects, one for each arrival."
  (let ((url (make-array 0 :fill-pointer t :adjustable t :element-type 'character))
		(jso))
	(format url "/Stop/GetStopInfo?stopId=~A&routeId=~A" stop-id route)
	(setf jso (jsown:parse (wget-text "realtimemap.grt.ca" url)))
	(when (string= (jsown:val jso "status") "success")
	  (jsown:val jso "stopTimes"))))
