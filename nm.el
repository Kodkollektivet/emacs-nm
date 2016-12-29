(provide 'nm)

(defvar iface nil
  "Primary WiFi network interface.")

(defvar nmvar/vpn-profile nil
  "VPN profile.")

(defun nm/wifi-enabled-p ()
  "True if wifi is enabled according to NetworkManager."
  (let ((status (nth 1 (split-string
                        (shell-command-to-string "nmcli -f wifi general")
                        "\n" t " "))))
    (equal status "enabled")))

(defun nm/filter-interface-list (iface-list-complete)
  "Return a list of device names as seen by nmcli."
  (let ((iface-names '()))
    (mapc #'(lambda (iface)
              (push
               (substring (car (split-string iface "\n")) 40 nil)
               iface-names))
          iface-list-complete)
    iface-names))

(defun nm/return-nmcli-output (arg)
  "Return nmcli output that is parsed and good looking."
  (cond
   ((equal "active-profiles" arg)
    (split-string (shell-command-to-string
                   "nmcli -t -f NAME connection show --active") "\n" t " +"))
   ((equal "active-profile-details" arg)
    (let* ((output (shell-command-to-string "nmcli connection show --active"))
           (sub (substring output 0 (- (length output) 1))))
      (format sub)))
   ((equal "profiles" arg)
    ;; All profiles
    (split-string (shell-command-to-string
                   "nmcli -t -f NAME connection show") "\n" t " +"))
   ((equal "aps-details" arg)
    ;; Connection details
    (let* ((output (shell-command-to-string "nmcli -f SSID,SIGNAL,SECURITY,CHAN,ACTIVE,BSSID device wifi list"))
           (sub (substring output 0 (- (length output) 1))))
      (format sub)))
   ((equal "aps" arg)
    ;; Available WiFi APs
    (split-string (shell-command-to-string
                   "nmcli -t -f SSID device wifi") "\n" t " +"))))

(defun nm/find-vpn-profiles ()
  "Find VPN profiles.

This maybe should be within 'nm/return-nmcli-output'?"
  (let* (( two-dim-list
           (mapcar (lambda (x) (split-string x ":")) (split-string (shell-command-to-string
                               "nmcli -t -f TYPE,NAME connection show")))))
    (mapcar (lambda (x)
              (nth 1 x))(remove nil(mapcar (lambda (x) (if (equal (nth 0 x) "vpn") x)) two-dim-list )))))

(defun nm/set-interface ()
  "Lists and sets available nmcli interfaces with autocomplete.

Asks to set a VPN profile."
  (interactive)
  (let* ((output (shell-command-to-string "nmcli device show"))
         (iface-names (nm/filter-interface-list
                       (split-string output "\n\n"))))
    (setq iface (completing-read "Select WLAN interface: " iface-names nil t)))
  (if (yes-or-no-p "Set VPN profile?")
      (setq nmvar/vpn-profile (completing-read "Select VPN profile: "
                                         (nm/find-vpn-profiles) nil 'confirm))))

(defun nm/show-aps-list ()
  "List WiFi APs with detailed info in a temp buffer."
  (interactive)
  (with-temp-buffer
    (princ (nm/return-nmcli-output "aps-details"))))

(defun nm/show-active-connection-profiles ()
  "Show nm profiles that are active."
  (interactive)
  (with-temp-buffer
    (princ (nm/return-nmcli-output "active-profile-details"))))

(defun nm/connect-vpn-profile ()
  "Connect to a VPN profile."
  (interactive)
  (if (equal nmvar/vpn-profile nil)
      (nm/set-interface))
  (let* ((vpn (completing-read "Select VPN profile: " (nm/find-vpn-profiles))))
    (setq nmvar/vpn-profile vpn)
    (shell-command-to-string (format "nmcli connection up id %s" vpn))))

(defun nm/connect-to-wifi-network (network password)
  "Connect to a Wifi network and create NetworkManager profile.

This will create a NetworkManager profile with the SSID as the profile NAME."
  (interactive
   (list
    (completing-read "Network: " (nm/return-nmcli-output "aps"))
    (read-string "Password: ")))
  (let* ((fstr (format "nmcli device wifi connect %s password %s" network password)))
    (let ((output (shell-command-to-string (format "%s" fstr))))
      (message (format output))))
  (if (yes-or-no-p "Connect a VPN?")
      (nm/connect-vpn-profile)))

(defun nm/connect-with-profile ()
  "Activate connection using existing profile configuration."
  (interactive)
  (let ((config (completing-read
                 "Select profile: " (nm/return-nmcli-output "profiles"))))
    (shell-command-to-string (format "nmcli connection up id %s" config))
    (message (format "Connected to '%s'" config))))

(defun nm/show-wifi-status ()
  "Show connectivity information in minibuffer."
  (interactive)
  (message
   (shell-command-to-string "nmcli -f state,connectivity,wifi g")))

(defun nm/toggle-wifi ()
  "Toggle wifi up/down."
  (interactive)
  (if (nm/wifi-enabled-p)
      (shell-command-to-string "nmcli radio wifi off")
    (shell-command-to-string "nmcli radio wifi on")))
