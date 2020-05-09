(source gnu)
(source melpa)

(package "pbuf" "0.1" "helpers for buffer displaying process output.")
(package-file "pbuf.el")

(development
 (depends-on "ert-runner"
             :git "https://github.com/canatella/ert-runner.el"
             :branch "win-fix"
             :files (:defaults ("reporters" "reporters/*") ("bin" "bin/*")))
 (depends-on "ert-async")
 (depends-on "package-lint")
 (depends-on "with-simulated-input"))
