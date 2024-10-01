(import (liii check))

(tm-define (test_11_36)
 (check (utf8->cork "苏E·12F88") => "<#82CF>E<centerdot>12F88"))
