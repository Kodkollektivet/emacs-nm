
(defun nm/connect-to-wifi-network (network password)
  "Connect to a Wifi network and create NetworkManager profile.

This will create a NetworkManager profile with the SSID as the profile NAME."
  
  (interactive
   (list
    (completing-read "Network: "
                     (mapcar (lambda (x) (replace-regexp-in-string " +$" "" x))
                             (cdr
                              (split-string
                               (shell-command-to-string
                                "nmcli -f SSID device wifi list") "\n"))))
    (read-string "Password: ")))
  (let* ((fstr (format "nmcli device wifi connect %s password %s" network password)))
    (let ((output (shell-command-to-string (format "%s" fstr))))
      (message (format output)))))
       

