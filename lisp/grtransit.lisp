;;;; Grand River Transit

;;; TODO Time comparisons will screw up just past midnight, as schedule times
;;; become 24:nn:nn, but `now' is the following day, so lookups in calendar_dates
;;; don't match..... True of GRTransit app too.

;;; Assuming asdf is set up on your system, you should be able to get the ball rolling
;;; by starting lisp, and doing this:
;;; (require 'asdf)
;;; (asdf:load-system 'grtransit)

;; Need this for parsing the JSON returned from the realtime site
;(require 'jsown)

(defpackage :grtransit
  (:use :common-lisp :jsown))
;;(in-package :grtransit)

(defparameter *work-directory* '(:absolute "home" "gdmalet" "src" "grtransit" "lisp" "tmp") "Where we're doing it")

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

(defparameter *realtime-cache* (make-hash-table :test 'equal))

(defconstant *day-names* '(MONDAY TUESDAY WEDNESDAY THURSDAY FRIDAY SATURDAY SUNDAY)
  "Used with index (day-of-week) to fetch values out of the calendar.")

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

(defun all-busses-at-stop (stop-id &optional (realtime nil))
  "Return a list of all the busses for a stop, in time order.
Each entry is a list of trip-id, arrival time, departure time, route, headsign.
If parm realtime is true; append minutes until, & date of arrival."
  (sort
   (mapcar
	(lambda (trip-details)
	  (multiple-value-bind (route-id sign)
		  (get-route-details-for-trip (car trip-details))
		(list (car trip-details)		; trip-id
			  (cadr trip-details)		; arrival time
			  (caddr trip-details)		; departure time
			  route-id sign
			  (when realtime (get-trip-realtime
							  (car trip-details)
							  stop-id route-id)))))
	(trips-using-stop stop-id))
   (lambda (a b)
	 (string< (cadr a) (cadr b)))))

(defun all-busses-at-stop* (stop-id &optional (realtime nil))
  "Pretty-printed version of all-busses-at-stop."
  (mapc
   (lambda (trip)
	 (let* ((exp-diff (timediff (cadr trip)))
			(pretty-diff (pretty-print-mins exp-diff)))
	   (if realtime
		   (case (type-of (nth 5 trip))		; if we have realtime data
			 (CONS
			  (multiple-value-bind (sec min hour)
				  (decode-universal-time
				   (universal-time-from-json-date (cdr (nth 5 trip))))
				(format t "~A ~5@A ~4@A ~32A (~3D ~2,'0d:~2,'0d:~2,'0d)~%"
						(nth 1 trip)
						pretty-diff
						(pretty-print-mins (- (car (nth 5 trip)) exp-diff) t)
						(nth 4 trip)
						(car (nth 5 trip))
						hour min sec)))
			 (t
			  (format t "~A ~5@A      ~A~%"
					  (nth 1 trip)
					  pretty-diff
					  (nth 4 trip))))
		   (format t "~A ~5@A  ~A~%"
				   (nth 1 trip)
				   pretty-diff
				   (nth 4 trip)))))
   (all-busses-at-stop stop-id realtime))
  t)

(defun next-bus-at-stop (stop-id)
  "For each route, return route and time of the next bus to come through the given stop."
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

(defun next-bus-at-stop-realtime (stop-id)
  "For each route, return realtime data with route and time of the
next bus to come through the given stop."
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
	   (declare (ignore foo))
	   (format t "fetching ~A~%" route)
	   (push (get-realtime stop-id route) times-list))
	 routes-hash)
	times-list))

(defun get-realtime (stop-id route)
  "Get the realtime data for a given route at given stop.
Returns a list of JSO objects, one for each arrival."
  ;; First see if we have this in the cache
  ;;(format t "Fetching realtime for stop-id ~a, route ~a~%" stop-id route)
  (let* ((key (format nil "~A:~A" stop-id route))
		 (cached (gethash key *realtime-cache*))
		 (now (get-universal-time)))
	;;(format t " cashed value: ~A~%" cached)
	(unless (and cached
				  (> (car cached) now))
	  (let ((url (make-array 0 :fill-pointer t :adjustable t :element-type 'character))
			(jso))
		(format url "/Stop/GetStopInfo?stopId=~A&routeId=~A" stop-id route)
		(format t "querying http://realtimemap.grt.ca~A~%" url)
		(setf jso (jsown:parse (wget-text "realtimemap.grt.ca" url)))
		(when (string= (jsown:val jso "status") "success")
		  (setf cached (cons (+ now 60) (jsown:val jso "stopTimes")))
		  (setf (gethash key *realtime-cache*) cached))))
	(cdr cached)))

(defun get-trip-realtime (trip stop-id route)
  "Get realtime data for a particular trip at a stop.
Returns a cons of minutes until arrival, and JSON date of arrival."
  (mapc
   (lambda (p)
	 (when (equal (jsown:val p "TripId") trip)
	   (return-from get-trip-realtime
		 (cons (jsown:val p "Minutes")
			   (jsown:val p "ArrivalDateTime")))))
   (get-realtime stop-id route))
  nil)

(defun active-trips (&optional (realtime nil))
  "Show all trips that are currently active, with optional realtime data."
  (let ((trips ()))
	;; iterate over all the stop times
	(maphash
	 (lambda (key value)
	   ;; check if this trip is active
	   (when
		   (and
			  (<= (timediff (slot-value (car value) 'departure-time)) 0)
			  (>= (timediff (slot-value (car (last value)) 'arrival-time)) 0))

		 ;; Find the closest stop time to now
		 (let* ((best
				(block the-best
				  (mapc
				   (lambda (trip)
					 (when (>= (timediff (slot-value trip 'departure-time)) 0)
					   (return-from the-best trip)))
				   (cdr value)))))

		   ;; Catch special case when we somehow don't match; must be
		   ;; the last entry that we want (mapc returned the whole list).
		   (when (eql (type-of best) 'CONS)
			 (setf best (car (last value))))

		   (format t "Trip ~A starts ~A, ends ~A; at stop-id ~A ~A ~A~%"
				   key
				   (slot-value (car value) 'departure-time)
				   (slot-value (car (last value)) 'arrival-time)
				   (slot-value best 'stop-id)
				   (slot-value best 'arrival-time)
				   (slot-value best 'departure-time))

		   (push best trips))))
	 (get-table "stop-times"))
	trips))
