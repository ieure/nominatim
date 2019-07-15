;;; nominatim.el --- OSM Nominatim support          -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Ian Eure

;; Author: Ian Eure <public@lowbar.fyi>
;; URL: https://github.com/ieure/nominatim
;; Version: 0.7.0
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
;; |   (licence . "Data Â© OpenStreetMap contributors, ODbL 1.0. https://osm.org/copyright")
;; |   (osm_type . "way")
;; |   (osm_id . 519770048)
;; |   (boundingbox .
;;                  |                ["45.5173392" "45.5174831" "-122.6591702" "-122.6589738"])
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
;;    |  (licence . "Data Â© OpenStreetMap contributors, ODbL 1.0. https://osm.org/copyright")
;;    |  (osm_type . "way")
;;    |  (osm_id . 207186676)
;;    |  (lat . "45.52394445")
;;    |  (lon . "-122.675869609626")
;;    |  (display_name . "Ground Kontrol Arcade, 511, Northwest Couch Street, Chinatown, Old Town, Portland, Multnomah County, Oregon, 97209, USA")
;;    |  (address
;;        |   (pub . "Ground Kontrol Arcade")
;;        |   (house_number . "511")
;;        |   (road . "Northwest Couch Street")
;;        |   (neighbourhood . "Chinatown")
;;        |   (suburb . "Old Town")
;;        |   (city . "Portland")
;;        |   (county . "Multnomah County")
;;        |   (state . "Oregon")
;;        |   (postcode . "97209")
;;        |   (country . "USA")
;;        |   (country_code . "us"))
;;    |  (boundingbox .
;;                    |               ["45.5238709" "45.524017" "-122.6759711" "-122.6757681"]))
;; `----

;;; Code:

(defconst nominatim--base-url "https://nominatim.openstreetmap.org"
  "Base URL of Nominatim.")

(defun nominatim--req (endpoint &optional query-args)
  "Make a request to Nominatim ENDPOINT with QUERY-ARGS."
  (let ((url (concat nominatim--base-url "/" endpoint "?"
                     (url-build-query-string (cons '("format" "json") query-args))))
        (url-request-extra-headers ))
    (with-current-buffer
        (url-retrieve-synchronously url t)
      (goto-char (point-min))
      (search-forward "\n\n")
      (json-read))))

(defun nominatim--fields->str (entry fields)
  "Return a printable version of fields FIELDS in ENTRY."
  (let* ((type (assoc 'type entry))
         (entry-type-key (intern (cdr type)))
         (address (cons type
                        (cdr (assoc 'address entry))))
         (address (cons
                   `(type-value . ,(cdr (assoc entry-type-key address)))
                   address))
         (commas '(type-value road city)))
    (thread-last
        (cl-loop for field in fields
                 for raw-val = (cdr (assoc field address))
                 for val = (if (and raw-val (not (string= raw-val ""))
                                    (memq field commas))
                               (concat raw-val ",")
                             raw-val)
                 when val collect val)
      (s-join " "))))

(defun nominatim-entry->address (entry)
  "Return a printable address for Nominatim result ENTRY."
  (nominatim--fields->str
   entry
   '(type-value house_number road city state postcode country)))

;;;###autoload
(defun nominatim-reverse-geocode (lat lon)
  "Reverse geocode LAT,LON."
  (nominatim--req "reverse" `((lat ,lat) (lon ,lon))))

;;;###autoload
(defun nominatim-geocode (text)
  "Geocode TEXT with Nominatim.

   TEXT can be an address or the name of a place.
   Returns an array of results."
  (nominatim--req "search" `((addressdetails 1)
                             (q ,text))))

(provide 'nominatim)

;;; nominatim.el ends here
