;;;; Grand River Transit

;;; Utility functions to support main grtransit.lisp

;;(in-package :grtransit)

(defun grtransit-startup ()
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

(defun string-to-symbol-name (str)
  "Convert given file name to a symbol name.
Given \"foo_bar.txt\", return \"FOO-BAR\""
  (substitute #\- #\_
			  (string-upcase
			   (subseq str 0 (position #\. str)))))


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
  "Load data from a standard text file."
  (format t "Loading table \"~A\"~%" from-file)
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

;;; Read one line from the input stream, and break up into a list.
;;; Separator is a comma.
;;; TODO - this will b0rk if a quoted string contains a comma or quote.
(defun parsed-input-line (stream)
  "Return a list of values from one line of comma separated input stream."
  (let ((line (read-line stream nil)))
    (when line
      (loop with last-comma = -1
	    while last-comma
	    collect (cleanup-input-line (subseq line (1+ last-comma)
			    (setf last-comma (position #\, line :start (1+ last-comma)))))))))

;;; Clean up crap coming from the input files.
(defun cleanup-input-line (str)
  "Trim leading & trailing space from string, & remove quotes."
  (when (> (length str) 0)
	(let ((s (string-trim '(#\Space) str)))
	  (when (eql (aref s 0) #\")
		(if (eql (aref s (1- (length s))) #\")
			(return-from cleanup-input-line (subseq s 1 (1- (length s))))
			(format t "~A is badly formed (quotes)~%" s)))
	  (return-from cleanup-input-line s))))

(defun get-table (table)
  "Return the hash table associated with the table name."
  (unless *tables*
	;;(format t "Loading tables~%")
	(grtransit-startup))
  (cdr (assoc table *tables* :test #'equalp)))

(defun day-of-week ()
  "Return the day of week from *working-date*"
  (nth 6 *working-date*))

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
		   (nth 3 *working-date*)		;day of month
		   (nth 4 *working-date*)		;month
		   (nth 5 *working-date*)))))	;year

(defun timediff (timestr)
  "Return the number of minutes between the time now and the passed in
HH:MM:SS string. Will be negative if the string is earlier than now."
  ;;(format t " -- timediff \"~A\"~%" timestr)
  (round (/ (- (timestr-to-epoch timestr)
			   (get-universal-time))
			60)))

(defun pretty-print-mins (mins &optional (with-sign nil))
  "Return a string that is suitable for display. Either just 99m if
|mins| < 60 (and force leading sign if with-sign is true), else
[-]9h09."
  (if (< (abs mins) 60)
	  (format nil (if with-sign "~@Dm" "~Dm") mins)	; format as +99m,
	  (format nil "~Dh~2,'0D"						;  else 9h09
			  (truncate mins 60)
			  (abs (rem mins 60)))))

(defun universal-time-from-json-date (json-date)
  "Given a JSON string of the form \"/Date(1445120640000)/\", convert
it to a unversal time. This involves extracting the date part, and
converting from Unix 1970/01/01 to Lisp 1900/01/01.
GRT seems to be returning times from GMT, so use that."
  (+
   (parse-integer (subseq json-date 6 16))
   (encode-universal-time 0 0 0 1 1 1970 0)))

;;; See http://www.clisp.org/impnotes/socket.html for other examples
(defun wget-text (host page &optional (port 80))
  "Returns simple-base-string of fetched web page, minus headers."
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

(defun show-table (table)
  "Show the first few entries in a table. Mostly for debugging."
  (let ((count 0))
	(maphash
	 (lambda (key val)
	   (format t "key \"~A\", val \"~A\"~%" key val)
	   (when (= count 0)
		 (format t " val is a ~A~%" (type-of val))
		 (case (type-of val)
		   (CONS
			(describe (car val)))
		   (t
			(describe val))))
	   (when (> (setf count (1+ count)) 10)
		 (return-from show-table)))
	 (get-table table))))
