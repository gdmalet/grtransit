;;;; Grand River Transit

;;; Tables definitions, constants, etc.

(defpackage :grtransit
  (:export active-trips)
  (:use common-lisp jsown))
(in-package :grtransit)

(declaim (optimize (speed 3) (debug 3) (safety 0)))

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
