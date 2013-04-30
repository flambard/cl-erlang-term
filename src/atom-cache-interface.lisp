(in-package :etf-atom-cache-interface)

;;;
;;; A generic interface for an atom cache.
;;; The application using this library is expected to implement methods for
;;; these generic functions if cached atoms are to be used.
;;;
;;; The special variable *ATOM-CACHE* should be bound to the relevant atom cache
;;; before encoding/decoding terms.
;;;


(defvar *atom-cache* nil
  "The current atom cache. Bind this variable prior to encoding/decoding terms
to make use of cached atoms in it.")


(defgeneric get-atom (reference cache)
  (:documentation "Get and return the atom from CACHE identified by REFERENCE.
The second return value is a boolean that indicates whether an atom was found
for REFERENCE. The first return value is NIL when no atom is found."))

(defgeneric put-atom (atom cache)
  (:documentation "Put ATOM into CACHE, creating a new entry for it if it does
not already exist. Returns the reference that identifies the atom, otherwise
returns NIL if for some reason the atom could not be cached."))
