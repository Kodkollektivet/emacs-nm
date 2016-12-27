(load-file "nm.el")

(ert-deftest ert-sanity-check ()
  "Sanity check!"
  (should (= (+ 1 1) 2)))


(ert-deftest ert-test-remove-trailing-spaces ()
  "Test the remove-trailing-spaces function."
  (should (equal (nm/remove-trailing-spaces '("a")) '("a")))
  (should (equal (nm/remove-trailing-spaces '("a" "b")) '("a" "b")))
  (should (equal (nm/remove-trailing-spaces '("a   " "b    ")) '("a" "b")))
  (should (equal (nm/remove-trailing-spaces '("a   " "b    ")) '("a" "b")))
  (should (equal (nm/remove-trailing-spaces '("Nu ska vi se   " "b    ")) '("Nu ska vi se" "b"))))
