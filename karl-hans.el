;;; karl-hans.el --- Clientdetector in the club

;; Copyright (C) 2009 Roman Glass <roman.glass@gmx.net>

;; Version: 1.0

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA

(require 'xml)

(defconst karl-hans-clients-tag
  'wlan
  "Describes the used tag for the number of clients connected to the hosts")

(defconst karl-hans-hosts-tag
  'hosts
  "Describes the used tag for the number of hosts")

(defconst karl-hans-url
  "http://bananentage.de/behmacfoobarbatnarfkrams.txt"
  "URL used to receive the data")

(defconst karl-hans-clients-msg
  "clients: "
  "Message that prints right before the variable number of clients")

(defconst karl-hans-hosts-msg
  "hosts: "
  "Message that prints right before the variable number of hosts")

(defun karl-hans ()
  "Detects clients and host at the ccchh and prints them out"
  (interactive)
  (karl-hans-retrieve-url))

(defun karl-hans-retrieve-url ()
  "Retrieves the xml-document to be parsed"
  (url-retrieve karl-hans-url
                'fetched-karl-hans-data))

(defun fetched-karl-hans-data (data)
  "Callback handler for fetching the karl-hans-data."
  (defun make-well-formed ()
    "Make this document pass the basic validation process"
    (goto-char (point-min))
    (insert "<root>")
    (goto-char (point-max))
    (insert "</root>"))
  (defun extract-children (xs)
    (nth 2
         (car xs)))
  (let ((result-buffer (current-buffer)) doc)
    (unwind-protect
        (progn
          (make-well-formed)
          (setq doc (xml-parse-region (point-min) (point-max))))
      (kill-buffer result-buffer))
    (let ((number-of-clients
           (extract-children (xml-get-children (car doc)
                                               karl-hans-clients-tag)))
          (number-of-hosts
           (extract-children (xml-get-children (car doc)
                                               karl-hans-hosts-tag))))
      (print (concat karl-hans-clients-msg
                     number-of-clients
                     ", "
                     karl-hans-hosts-msg
                     number-of-hosts)))))
