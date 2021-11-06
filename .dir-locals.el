((nil . ((eval . (progn
                   (require 'projectile)
                   (puthash (projectile-project-root)
                            "emacs -batch -f package-initialize -L . -f buttercup-run-discover"
                            projectile-test-cmd-map))))))
