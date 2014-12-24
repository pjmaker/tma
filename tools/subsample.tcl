#! /bin/sh
# -*-tcl-*- \
	exec tclsh "$0" "$@"
#
# subsample.tcl - 
#

array set options {
  -resample 600
}

set nr 0
while {[gets line] > 0} {
  incr nr
  if {nr <= 1} {
    continue
  }
  set l [split $line ,]
  set t [lindex $l 0]
  set v [lindex $l 1]
  
