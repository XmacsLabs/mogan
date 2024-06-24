find devel/ | grep ".tm$" | each { |x|
  /usr/bin/MoganResearch --convert $x $"x"u --quit
}
