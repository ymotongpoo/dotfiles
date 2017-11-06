(require 'package)

(package-initialize)
(unless package-archive-contents
    (package-refresh-contents))
(package-install-selected-packages)
