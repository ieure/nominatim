;;; nominatim.el --- OSM Nominatim support          -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Ian Eure

;; Author: Ian Eure <public@lowbar.fyi>
;; URL: https://github.com/ieure/nominatim
;; Version: 0.9.1
;; Package-Requires: ((emacs "25") (s "1.12.0"))
;; Keywords: tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Geocode
;; =======
;;
;; Geocoding is the process of turning freeform text into geospatial
;; data.
;;
;; ,----
;; | (pp (nominatim-geocode "creepy's portland or"))
;; `----
;;
;; ,----
;; | [((place_id . 247862422)
;; |   (licence . "Data © OpenStreetMap contributors, ODbL 1.0. https://osm.org/copyright")
;; |   (osm_type . "way")
;; |   (osm_id . 519770048)
;; |   (boundingbox .
;; |                ["45.5173392" "45.5174831" "-122.6591702" "-122.6589738"])
;; |   (lat . "45.51741115")
;; |   (lon . "-122.659071975122")
;; |   (display_name . "Creepy's, 627, Southeast Morrison Street, Central East Side, Buckman, Portland, Multnomah County, Oregon, 97214, USA")
;; |   (class . "amenity")
;; |   (type . "bar")
;; |   (importance . 0.401)
;; |   (icon . "https://nominatim.openstreetmap.org/images/mapicons/food_bar.p.20.png")
;; |   (address
;; |    (bar . "Creepy's")
;; |    (house_number . "627")
;; |    (road . "Southeast Morrison Street")
;; |    (neighbourhood . "Central East Side")
;; |    (suburb . "Buckman")
;; |    (city . "Portland")
;; |    (county . "Multnomah County")
;; |    (state . "Oregon")
;; |    (postcode . "97214")
;; |    (country . "USA")
;; |    (country_code . "us")))]
;; `----
;;
;;
;; Reverse Geocode
;; ===============
;;
;; Reverse geocoding turns a lat/lon into a street address.
;;
;; ,----
;; | (pp (nominatim-reverse-geocode 45.52394445 -122.675869609626))
;; `----
;;
;; ,----
;; | ((place_id . 123976214)
;; |  (licence . "Data Â© OpenStreetMap contributors, ODbL 1.0. https://osm.org/copyright")
;; |  (osm_type . "way")
;; |  (osm_id . 207186676)
;; |  (lat . "45.52394445")
;; |  (lon . "-122.675869609626")
;; |  (display_name . "Ground Kontrol Arcade, 511, Northwest Couch Street, Chinatown, Old Town, Portland, Multnomah County, Oregon, 97209, USA")
;; |  (address
;; |   (pub . "Ground Kontrol Arcade")
;; |   (house_number . "511")
;; |   (road . "Northwest Couch Street")
;; |   (neighbourhood . "Chinatown")
;; |   (suburb . "Old Town")
;; |   (city . "Portland")
;; |   (county . "Multnomah County")
;; |   (state . "Oregon")
;; |   (postcode . "97209")
;; |   (country . "USA")
;; |   (country_code . "us"))
;; |  (boundingbox .
;; |               ["45.5238709" "45.524017" "-122.6759711" "-122.6757681"]))
;; `----

;;; Code:

(require 'ert)

(defconst nominatim--base-url "https://nominatim.openstreetmap.org"
  "Base URL of Nominatim.")

(defun nominatim--req (endpoint &optional query-args)
  "Make a request to Nominatim ENDPOINT with QUERY-ARGS."
  (let* ((url (concat nominatim--base-url "/" endpoint "?"
                      (url-build-query-string (cons '("format" "json") query-args))))
         (cached (url-is-cached url)))

    (with-current-buffer
        (if cached
            (url-fetch-from-cache url)
          (url-retrieve-synchronously url t))
      (unless cached
        (url-store-in-cache))
      (goto-char (point-min))
      (search-forward "\n\n")
      (json-read))))

(defmacro nominatim--field (elements addr field &optional break)
  "Push ADDR field FIELD onto ELEMENTS, followed by BREAK.
If FIELD isn't set, do nothing and return NIL.
   If FIELD's value was pushed, returns non-NIL."
  `(prog1 (if-let ((val (cdr (assoc ,field ,addr))))
             (push val ,elements))
    (when ,break
      (push ,break ,elements))))

(defun nominatim--printable (loc)
  "Return an abstract printable version of location LOC.
A human-readable string can be obtained by passing this to
   `nominatim--printable->oneline' or
   `nominatim--printable->nline'.

   The abstract representation is a list of text items, with
   :break (for a hard break, e.g. newline) and
   :soft-break (e.g. comma) interspersed."
  (let ((cc (thread-last (assoc 'address loc)
              (cdr)
              (assoc 'country_code)
              (cdr))))
    (cond ((string= cc "us") (nominatim--printable-us loc))
          (t (error "Don't know how to handle `%s' addresses" cc)))))

(defun nominatim--printable-us (loc)
  "Return a human-readable version of nominatim US location LOC."
  (let* ((elements)
         (addr (cdr (assoc 'address loc)))
         (type-sym (intern (cdr (assoc 'type loc)))))
      ;; Business name
      (nominatim--field elements addr type-sym :break)

      ;; House number
      (nominatim--field elements addr 'house_number)

      ;; Road
      (nominatim--field elements addr 'road :break)

      ;; FIXME suite, apartment, floor, etc

      ;; If the city is set, use it; otherwise, the county.
      (or (nominatim--field elements addr 'city :soft-break)
          (nominatim--field elements addr 'county :soft-break))

      ;; State
      (nominatim--field elements addr 'state)

      ;; Postcode
      (nominatim--field elements addr 'postcode :break)

      ;; Country
      (nominatim--field elements addr 'country)

      (seq-reverse elements)))

(defun nominatim--printable->oneline (printable-loc)
  "Return a one-line human-readable version PRINTABLE-LOC."
  (thread-first
      (lambda (s elt)
        ;; Replace all break types with commas
        (cond ((keywordp elt) (concat s ","))
              (t (concat s " " elt))))
    (reduce printable-loc)))

(defun nominatim--printable->nline (printable-loc)
  "Return a multi-line human-readable version PRINTABLE-LOC."
  (thread-first
      (lambda (s elt)
        ;; Soft breaks become commas; hard become newlines.
        (cond ((eq :soft-break elt) (concat s ","))
              ((eq :break elt) (concat s "\n"))
              ;; If the last char was a newline, don't add whitespace.
              ((string= "\n" (substring s (1- (length s)))) (concat s elt))
              (t (concat s " " elt))))
    (reduce printable-loc)))


;;;###autoload
(defun nominatim-reverse-geocode (lat lon)
  "Reverse geocode LAT,LON."
  (nominatim--req "reverse" `((lat ,lat) (lon ,lon))))

(defun nominatim-reverse-geocode-geoclue (geoclue-location)
  "Reverse geocode `GEOCLUE-LOCATION', a Geoclue2-format list."
  (nominatim-reverse-geocode (cdr (assoc 'latitude  geoclue-location))
                             (cdr (assoc 'longitude  geoclue-location))))

;;;###autoload
(defun nominatim-geocode (text)
  "Geocode TEXT with Nominatim.

   TEXT can be an address or the name of a place.
   Returns an array of results."
  (nominatim--req "search" `((addressdetails 1)
                             (q ,text))))

 ;;; Tests

(defconst nominatim--xfinity-test-loc
  '((place_id . 143200844)
    (licence . "Data Â© OpenStreetMap contributors, ODbL 1.0. https://osm.org/copyright")
    (osm_type . "way")
    (osm_id . 293769676)
    (boundingbox .
                 ["45.5480021" "45.5481288" "-122.5905444" "-122.5900386"])
    (lat . "45.54807395")
    (lon . "-122.59029280645")
    (display_name . "Xfinity, 7037, Northeast Sandy Boulevard, Roseway, Portland, Multnomah County, Oregon, 97213, USA")
    (class . "shop")
    (type . "electronics")
    (importance . 0.201)
    (address
     (electronics . "Xfinity")
     (house_number . "7037")
     (road . "Northeast Sandy Boulevard")
     (suburb . "Roseway")
     (city . "Portland")
     (county . "Multnomah County")
     (state . "Oregon")
     (postcode . "97213")
     (country . "USA")
     (country_code . "us"))))

(ert-deftest nominatim-printable-test ()
  (should (equal '("Xfinity" :break "7037" "Northeast Sandy Boulevard" :break
                   "Portland" :soft-break "Oregon" "97213" :break "USA")
                 (nominatim--printable nominatim--xfinity-test-loc))))

(ert-deftest nominatim-oneline-test ()
  (should (string= "Xfinity, 7037 Northeast Sandy Boulevard, Portland, Oregon 97213, USA"
                   (nominatim--printable->oneline (nominatim--printable nominatim--xfinity-test-loc)))))

(ert-deftest nominatim-nline-test ()
  (should (string= "Xfinity\n7037 Northeast Sandy Boulevard\nPortland, Oregon 97213\nUSA"
                   (nominatim--printable->nline (nominatim--printable nominatim--xfinity-test-loc)))))

(provide 'nominatim)

;;; nominatim.el ends here
