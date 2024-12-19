use str

var flush_start = {
  print "\x02"
}
var flush_end = {
  print "\x05\r\n"
}

var flush_utf8 = { |content|
  $flush_start
  print "utf8:"$content
  $flush_end
}

var ns = (ns [&tm_elvish="0.0.1"])

var elvish_plugin_version = "0.0.1"

$flush_utf8 (str:join "" ["Elvish plugin " $elvish_plugin_version " by LiiiLabs"])

while true {
  var line = (read-line)
  if (not (eq $line "<EOF>")) {
    try {
      var result = [ (eval &ns=$ns $line) ]
      for result_line $result {
        $flush_utf8 (to-string $result_line)
      } else {
        $flush_utf8 ""
      }
    } catch e {
      $flush_utf8 (to-string $e)
    }
  }
}

