js-base-mode typescript-ts-base-mode

(func* "function*(" p ") {" n> p n "}" >)
(map "map(" p " => ) {" n> p n "}" >)
(log "console.log(" p ")")

ruby-base-mode

(fsl "# frozen_string_literal: true" n)
(init "def initialize(" p ")")
(cls "class " (s-upper-camel-case (file-name-base (or (buffer-file-name) (buffer-name)))) n "  " p n "end")
(astc "require \"test_helper\"" n n "class " (s-upper-camel-case (s-chop-suffix "_test" (file-name-base (or (buffer-file-name) (buffer-name))))) "Test < ActiveSupport::TestCase" n "  " p n "end")
(adit "require \"test_helper\"" n n "class " (s-upper-camel-case (s-chop-suffix "_test" (file-name-base (or (buffer-file-name) (buffer-name))))) "Test < ActionDispatch::IntegrationTest" n "  " p n "end")
(p "puts \"#############\"" n> "pp " p n> "puts \"#############\"")
(here (p "name: " name t) > "<<~" (upcase name) n> "  " p n> (upcase name) n>)

;; Local Variables:
;; mode: lisp-data
;; outline-regexp: "[a-z]"
;; End:
