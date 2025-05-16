;;; journelly.el --- https://journelly.com Org helpers  -*- lexical-binding: t -*-

;; Copyright (C) 2023 Alvaro Ramirez

;; Author: Alvaro Ramirez https://xenodium.com

;; This package is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This package is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Experimental commands to interact with https://journelly.com
;; org mode files.
;;
;; You can use `journelly-generate-metadata' in `org-capture-templates'
;;
;; To include location and weather information like:
;;
;; (setq org-capture-templates
;;       '(("j" "Journelly" entry (file "path/to/Journelly.org")
;;          "* %U @ %(journelly-generate-metadata)\n%?" :prepend t :eval t)))

;;; Code:

(defun journelly-generate-metadata ()
  (let* ((location (journelly-get-location))
         (weather (journelly-fetch-weather-summary
                   (map-elt location 'lat)
                   (map-elt location 'lon))))
    (format "%s
:PROPERTIES:
:LATITUDE: %s
:LONGITUDE: %s
:WEATHER_TEMPERATURE: %s
:WEATHER_SYMBOL: %s
:END:"
            (or (map-elt location 'description) "-")
            (map-elt location 'lat)
            (map-elt location 'lon)
            (alist-get 'temperature weather)
            (journelly-resolve-metno-to-sf-symbol
             (alist-get 'symbol weather)))))

(defun journelly-get-location ()
  "Get current location.

Return in the form:

`((lat . 1)
  (lon . 1)
  (description . \"Sunny House\"))

Signals an error if the location cannot be retrieved."
  (unless (executable-find "CoreLocationCLI")
    (error "Needs CoreLocationCLI (try brew install corelocationcli)"))
  (with-temp-buffer
    (if-let ((exit-code (call-process "CoreLocationCLI" nil t nil
                                      "--format" "%latitude\t%longitude\t%thoroughfare"))
             (success (eq exit-code 0))
             (parts (split-string (buffer-string) "\t")))
        `((lat . ,(string-to-number (nth 0 parts)))
          (lon . ,(string-to-number (nth 1 parts)))
          (description . ,(string-trim (nth 2 parts))))
      (error "No location available"))))

(defun journelly-fetch-weather (lat lon)
  "Fetch weather data from MET Norway API for LAT and LON.

Return the parsed JSON object."
  (let* ((url (format "https://api.met.no/weatherapi/locationforecast/2.0/compact?lat=%s&lon=%s" lat lon))
         (args (list "-s" url)))
    (with-temp-buffer
      (apply #'call-process "curl" nil t nil args)
      (goto-char (point-min))
      (json-parse-buffer :object-type 'alist))))

(defun journelly-fetch-weather-summary (lat lon)
  "Fetch weather data from MET Norway API for LAT and LON.

Return in the form:

 \='((temperature . \"16.9°C\")
 (symbol . \"cloudy\"))."
  (let* ((data (journelly-fetch-weather lat lon))
         (now (current-time))
         (entry (seq-find
                 (lambda (entry)
                   (let-alist entry
                     (time-less-p now (date-to-time .time))))
                 (let-alist data
                   .properties.timeseries)))
         (unit (let-alist data
                 .properties.meta.units.air_temperature)))
    (unless entry
      (error "Couldn't fetch weather data"))
    (let-alist entry
      `((temperature . ,(format "%.1f%s"
                                .data.instant.details.air_temperature
                                (cond
                                 ((string= unit "celsius") "°C")
                                 ((string= unit "fahrenheit") "°F")
                                 (t (concat " " unit)))))
        (symbol . ,(alist-get 'symbol_code .data.next_1_hours.summary))))))

(defun journelly-resolve-metno-to-sf-symbol (symbol)
  "Resolve Met.no weather SYMBOL strings to a corresponding SF Symbols."
  (let ((symbols '(("clearsky_day" . "sun.max")
                   ("clearsky_night" . "moon.stars")
                   ("clearsky_polartwilight" . "sun.horizon")
                   ("fair_day" . "sun.max")
                   ("fair_night" . "moon")
                   ("fair_polartwilight" . "sun.horizon")
                   ("partlycloudy_day" . "cloud.sun")
                   ("partlycloudy_night" . "cloud.moon")
                   ("partlycloudy_polartwilight" . "cloud.sun")
                   ("cloudy" . "cloud")
                   ("fog" . "cloud.fog")
                   ("lightrain" . "cloud.drizzle")
                   ("rain" . "cloud.rain")
                   ("heavyrain" . "cloud.heavyrain")
                   ("lightrainshowers_day" . "cloud.sun.drizzle")
                   ("lightrainshowers_night" . "cloud.moon.drizzle")
                   ("rainshowers_day" . "cloud.sun.rain")
                   ("rainshowers_night" . "cloud.moon.rain")
                   ("heavyrainshowers_day" . "cloud.sun.rain.fill")
                   ("heavyrainshowers_night" . "cloud.moon.rain.fill")
                   ("rainandthunder" . "cloud.bolt.rain")
                   ("lightrainandthunder" . "cloud.bolt.rain")
                   ("heavyrainandthunder" . "cloud.bolt.rain.fill")
                   ("rainshowers_and_thunder_day" . "cloud.sun.bolt.rain")
                   ("rainshowers_and_thunder_night" . "cloud.moon.bolt.rain")
                   ("heavyrainshowers_and_thunder_day" . "cloud.sun.bolt.rain.fill")
                   ("heavyrainshowers_and_thunder_night" . "cloud.moon.bolt.rain.fill")
                   ("sleet" . "cloud.sleet")
                   ("heavysleet" . "cloud.sleet.fill")
                   ("sleetshowers_day" . "cloud.sun.sleet")
                   ("sleetshowers_night" . "cloud.moon.sleet")
                   ("lightsleetshowers_day" . "cloud.sun.sleet")
                   ("lightsleetshowers_night" . "cloud.moon.sleet")
                   ("heavysleetshowers_day" . "cloud.sun.sleet.fill")
                   ("heavysleetshowers_night" . "cloud.moon.sleet.fill")
                   ("sleetandthunder" . "cloud.bolt.sleet")
                   ("heavysleetandthunder" . "cloud.bolt.sleet.fill")
                   ("sleetshowers_and_thunder_day" . "cloud.sun.bolt.sleet")
                   ("sleetshowers_and_thunder_night" . "cloud.moon.bolt.sleet")
                   ("snow" . "cloud.snow")
                   ("heavysnow" . "cloud.snow.fill")
                   ("snowshowers_day" . "cloud.sun.snow")
                   ("snowshowers_night" . "cloud.moon.snow")
                   ("heavysnowshowers_day" . "cloud.sun.snow.fill")
                   ("heavysnowshowers_night" . "cloud.moon.snow.fill")
                   ("snowandthunder" . "cloud.bolt.snow")
                   ("heavysnowandthunder" . "cloud.bolt.snow.fill")
                   ("snowshowers_and_thunder_day" . "cloud.sun.bolt.snow")
                   ("snowshowers_and_thunder_night" . "cloud.moon.bolt.snow")
                   ("thunderstorm" . "cloud.bolt"))))
    (map-elt symbols symbol)))

(provide 'journelly)

;;; journelly.el ends here
