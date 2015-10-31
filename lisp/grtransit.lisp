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

(in-package :grtransit)

(declaim (optimize (speed 3) (debug 3) (safety 0)))

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

(defun all-busses-at-stop* (stop-id &optional (realtime nil) (only-realtime nil))
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
				(format t "~A ~A ~5@A ~4@A ~32A (~3D ~2,'0d:~2,'0d:~2,'0d)~%"
						(nth 1 trip)
						(nth 2 trip)
						pretty-diff
						(pretty-print-mins (- (car (nth 5 trip)) exp-diff) t)
						(nth 4 trip)
						(car (nth 5 trip))
						hour min sec)))
			 (t
			  (unless only-realtime
				(format t "~A ~A ~5@A      ~A~%"
						(nth 1 trip)
						(nth 2 trip)
						pretty-diff
						(nth 4 trip)))))

			 (format t "~A ~A ~5@A  ~A~%"
					 (nth 1 trip)
					 (nth 2 trip)
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

(defun get-route-details-for-trip (trip-id)
  "Return the route-id, trip headsign & service-id for a given trip-id"
  (let ((trip (gethash trip-id (get-table "trips-by-trip"))))
	(values
	 (slot-value trip 'route-id)
	 (slot-value trip 'trip-headsign)
	 (slot-value trip 'service-id))))

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
;;;; TODO cache time should be 60, not 600
		  (setf cached (cons (+ now 60) (jsown:val jso "stopTimes")))
		  ;;(format t " --> new cached values: ~A~%" cached)
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

(defun active-trips* (&optional (realtime nil))
  "A pretty-printed version of active-trips."
  (mapc (lambda (value)
		  (let* ((next-stop (if (eql (type-of value) 'CONS)
								(car value)
								value))
				 (rt (if realtime (cdr value) nil))
				 (stop-times (gethash (slot-value next-stop 'trip-id) (get-table "stop-times"))))

			(multiple-value-bind (route-id headsign service-id)
				(get-route-details-for-trip (slot-value next-stop 'trip-id))
			  (declare (ignore route-id service-id))

			  (if realtime
				  (let ((hms (if rt
								 (multiple-value-bind (sec min hour)
									 (decode-universal-time
									  (universal-time-from-json-date (cdr rt)))
								   (format nil "~2,'0d:~2,'0d:~2,'0d" hour min sec))
								 ":-(  ")))

					(format t "Trip ~A starts ~A, ends ~A; ~A~% next stop ~A ~A ~A [~3D ~A]~%"
							(slot-value next-stop 'trip-id)
							(slot-value (car stop-times) 'departure-time)
							(slot-value (car (last stop-times)) 'arrival-time)
							headsign
							(slot-value next-stop 'stop-id)
							(slot-value next-stop 'arrival-time)
							(slot-value next-stop 'departure-time)
							(if rt (pretty-print-mins (- (car rt)
														 (timediff (slot-value next-stop 'arrival-time)))
													  t)
								"")
							hms))

				  (format t "Trip ~A starts ~A, ends ~A; ~A~% next stop ~A ~A ~A~%"
						  (slot-value next-stop 'trip-id)
						  (slot-value (car stop-times) 'departure-time)
						  (slot-value (car (last stop-times)) 'arrival-time)
						  headsign
						  (slot-value next-stop 'stop-id)
						  (slot-value next-stop 'arrival-time)
						  (slot-value next-stop 'departure-time))))))

		(active-trips realtime))
  t)

(defun active-trips (&optional (realtime nil))
  "Return a list of stop-times for all trips that are currently
active, with optional realtime data. Each stop-time is for the current
stop the bus is at, or then next stop if it's between stops."
  (let ((trips ()))
	;; iterate over all the stop times
	(maphash
	 (lambda (trip-id stop-times)
	   ;; check if this trip is active
	   (when
		   (and
			  (<= (timediff (slot-value (car stop-times) 'departure-time)) 0)
			  (>= (timediff (slot-value (car (last stop-times)) 'arrival-time)) 0))

		 ;; Find the closest stop time to now by walking along the stops
		 (let* ((best
				(block the-best
				  (mapc
				   (lambda (trip)
					 (when (>= (timediff (slot-value trip 'departure-time)) 0)
					   (return-from the-best trip)))
				   (cdr stop-times)))))

		   ;; Catch special case when we somehow don't match; must be
		   ;; the last entry that we want (mapc returned the whole list).
		   (when (eql (type-of best) 'CONS)
			 (setf best (car (last stop-times))))

		   (multiple-value-bind (route-id headsign service-id)
			   (get-route-details-for-trip trip-id)
			   (declare (ignore headsign))

			 ;; Make sure the trip runs today
			 (when (check-calendar service-id)
			   (if realtime
				 (let ((rt (get-trip-realtime trip-id (slot-value best 'stop-id) route-id)))
				   (push (cons best rt) trips))
				 (push best trips)))))))

	 (get-table "stop-times"))
	trips))
