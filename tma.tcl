#! /bin/sh
# -*-tcl-*- \
	exec /opt/tma/bin/bltwish25 "$0" "$@"
#
# tma.tcl - a visualiser for time matters analyser, in particular irregularly
#  sampled time series coming out of Powerwater ASIM, OSIsoft PI, ... 
#

# 
# Copyright 2013-15 Phil Maker <philip.maker@gmail.com>
#
# This file is part of Tma.  Tma is free software: you can
# redistribute it and/or modify it under the terms of the GNU
# General Public License as published by the Free Software
# Foundation, either version 3 of the License, or (at your option)
# any later version.
#
# Tma is  distributed in  the hope that  it will be  useful, but
# WITHOUT  ANY  WARRANTY;  without  even the  implied  warranty  of
# MERCHANTABILITY or FITNESS FOR  A PARTICULAR PURPOSE. See the GNU
# General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with Tma. If not, see http://www.gnu.org/licenses/.
# 

set ::VERSION "0.0 (purple butterfly)"
# 0.1, 0.14, 0.149, ...
 
proc get_version {} {
  return "tma version $::VERSION"
}

# descriptions of commands, options and variables
proc describe {thing text} {
  append ::descriptions($thing) $text
}

proc descriptions {{pat *}} {
  set r {}
  foreach t [lsort -dictionary [array names ::descriptions $pat]] {
    append r $::descriptions($t) 
  }
  return $r
}

# some helpers for configuration on different platforms
proc windows_only {c} {
  if {$::tcl_platform(platform) eq "windows"} {
    uplevel #0 $c
  }
}

proc unix_only {c} {
  if {$::tcl_platform(platform) eq "unix"} {
    uplevel #0 $c
  }
}

# process options from argv
describe options {
The following command line options can be passed via the command line, e.g.
by editing the tma.bat file or using the command line. Of particular note is 
the <tt>-get file</tt> option that allows you to either get a file, e.g. 
<tt>X.csv</tt> or a tma script <tt>X.tma</tt>.
<p>
}

# list of (option value description) for all options
foreach {o v d} { 
  -plot_format_time "%H:%M:%S %Z\n%d/%m/%Y" "X-axis time format"
  -get {} "Get files matching this"
  -fullscreen 0 "Full screen mode on startup"
  -periods {
    "&Autorange"
    "1m" "10m" "&30m" 
    "1&h" "8h" "12h"
    "1&d" "3d" "7d" "28d" "30d" "31d" 
    "1&y" "5y"
  } "Periods in the top menu"
  -pi_tags "R_*" "Only use tags matching this pattern, e.g. R_* in remote"
  -pi_tag_reader 1 "Display the PI tag reader commands"
  -pitag_file "Pitags.csv" "File of PI Tag attributes"
  -pitag_exe "PItags.exe" "Program to read PI attributes from the server"
  -pitool_exe "PItool.exe" "Program that connect to the PI Server"
  -pi_server "172.16.12.140" "PI Server IP address/name"
  -NaN {} "IEEE NaN values are mapped to {} nothing or some other number"
  -nc_reader "acep-csv" "Program to read ACEP NetCDF (.nc) files into CSV"
  -check_log 1 "log checking information to a file"
  -console 1 "Display the console on startup"
  -console_lines 7 "How many lines we display on the console"
  -console_safe 0 "Use a safe(TCL) interpreter for the console and .tma files"
  -width 13i "Width of the plot"
  -unix_gs gs "Executable for ghostscript under *NIX"
  -windows_gs "c:/Program Files/gs/gs9.07/bin/gswin32.exe" "Path to ghostscript on windows"
  -unix_pdf_reader evince "UNIX PDF reader"
  -windows_pdf_reader "c:/Program Files/Adobe/Reader 9.0/Reader/AcroRd32.exe"  "Path to PDF reader for windows."
  -bltdebug 0 "Debug level, see blt::bltdebug"
  -colors {
    #ff0000 #00ff00 #0000ff #860000 #007b00 #8900cc #9c680b #e8c10b 
    #28e9fe #f900f8 #deff00 #78dc8b #00fea6 #a4b98c #d34c00 #005e9c 
    #d3007e #b9be4e #fcff00 #d17373 #9ed3a6    
  } "Default colors for plots"
} {
  set ::options($o) $v 
  describe options "$o - $d (default: <tt>$v</tt>)<br>"
}

# workaround for windows bugs (ttt review, maybe just check and die)
windows_only {
  array set ::options {
    -console_safe 0
  }
}

describe options "<p>Current options:<p>"

if {([llength $::argv] % 2) != 0} {
  puts "$argv0: usage $argv [-option val]"
  exit 1
}

foreach {o v} $::argv {
  if {![string match -* $o]} {
    puts "$argv0: $o should be an option"
    exit 1
  }
}
  
array set options $::argv

foreach v [lsort -dictionary [array names ::options]] {
  describe options "<tt>$v $::options($v)</tt><br>"
}

# import required packages
package require Tk 
package require Ttk 
package require BLT

# string routines

# matches uses string match and allows choice |
proc match {pat list} {
  set pats [split $pat |]
  set r {}
  foreach p $pats {
    foreach pp [lsearch -all -inline -glob $list $p] {
      lappend r $pp
    }
  }
  set r [lsort -dictionary -unique $r]
  return $r
}

# define and describe command definitions where commands are executable
# from .tma files, these are all in the ::command::* namespace along
# with the variables we use for modelling time and values.

describe commands "<h2>Commands for use in *.tma files</h2><p>"

namespace eval commands {}

# ttt add a trace mechanism at some time
proc command {name params description code} {
  describe commands "<tt>$name $params</tt> - $description<br>"
  uplevel #0 "proc ::commands::$name [list $params] [list $code]"
  interp alias {} ::$name {} ::commands::$name
}

# timed var support - simple timed global variable system using BLT vectors
#   to represent the what/when.

set ::commands::vars {} ;# list of all vars
array set ::commands::rename {} ;# rename rules for vars

command var {v args} {
  create variable v with properties args, e.g. var ErrE or var E -dv 10. 
  Explain variable renaming. ttt
  returns the variable name (which may have changed if rename command is used).
} {
  msg "var $v $args"
  foreach {p a} $args {
    if {$p eq "-name"} {
      msg "var $v -name $a"
      set v $a
    }
  }
  set v2 [string map [array get ::commands::rename] $v]
  msg "var $v renamed to $v2"
  set v $v2

  blt::vector create ::commands::${v}_ ;# what
  blt::vector create ::commands::${v}@ ;# when

  # default values
  array set ::commands::${v}= [default_properties] 
  # override individual properties
  array set ::commands::${v}= $args ;# properties
  if {[set ::commands::[set v]=(-color)] eq {}} {
    set ::commands::${v}=(-color) [nextcolor]
  }

  set ::commands::${v}# 0 ;# number of samples
  set ::commands::${v}* -1 ;# current index
  set ::commands::${v} 0
  set ::commands::${v}_b 0
  lappend ::commands::vars ${v}
  return $v
}

proc default_properties {} {
  return [property_sort {
    -dv 0
    -dz 0
    -mapy y
    -color {}
    -pixels 2
    -symbol none
  }]
}

proc property_sort {l} {
  array set a $l
  set ks [lsort -dictionary [array names a]]
  set r {}
  foreach k $ks {
    lappend r $k $a($k)
  }
  return $r
}


command varrename {args} {
  After this newly created variable names will be renamed using args,
  e.g. varname R_A_PG "" will remove the R_A_PG string from all names.
} {
  array set ::commands::rename $args
}
  
set ::nextcolor 0
proc nextcolor {} {
  if {$::nextcolor < [llength $::options(-colors)]} {
    set c [lindex $::options(-colors) $::nextcolor]
    incr ::nextcolor
    return $c
  } else {
    return [format "#%02X%02X%02X" \
		[expr int(rand()*150)] \
		[expr int(rand()*150)] \
		[expr int(rand()*150)]] 
  }
}

command var- {args} {remove variables matching args} {
  foreach pat $args {
    foreach v [vars $pat] {
      catch ".w element delete $v" 
      blt::vector destroy ::commands::${v}_ 
      blt::vector destroy ::commands::${v}@
      unset ::commands::${v}=
      unset ::commands::${v}
      set ::commands::vars [lsearch -inline -exact -all -not $::commands::vars $v]
    }
  }
  return [vars]
}

command vars {{pat *}} {list of all variables} {
  return [lsearch -all -inline $::commands::vars $pat]
}

command properties {var} {list of properties and attributes for var} {
  return [property_sort [array get ::commands::$var=]]
}

command property {var prop {def nodefault}} {return the value for property p} {
  if {[info exists ::commands::$var=($prop)]} {
    return [set ::commands::${var}=($prop)]
  } elseif {$def == "nodefault"} {
    error "property $var $prop failed"
  } else {
    return $def
  }
}

command let {v what when} {v has value what at time when} {
  # puts "** ::commands::${v}_ append '$what'"
  if {[expr $what != $what]} { # nan -> $::options(-NaN)
    if {$::options(-NaN) == {}} { ;# just drop the values
      return 
    }
    set what $::options(-NaN)
  } 
  # puts "*2 ::commands::${v}_ append $what"

  # puts "let $v $what [time::format $when] [time::format [st]]"
  if {[samples $v] == 0} { # no data yet 
    let0 $v $what $when
  } else { # > 0 samples 
    if {[expr abs($what) >= [property $v -dz]]} {
      letn $v $what $when
    } else {
      # ignore it within -dz deadband
    }
  }
  return $what
}

# let0 handles the case for no samples
proc let0 {v what when} {
  if {$when < [st]} { # sample is before so save it, still no real data
    set ::before_when($v) $when
    set ::before_what($v) $what
  } elseif {$when == [st]} { # at st so just put it in
    let2 $v $what $when
  } elseif {$when < [et]} { # after st but before et 
    if {[info exists ::before_what($v)]} {
      let2 $v $::before_what($v) [expr max($::before_when($v),[st])] 
    }
    let2 $v $what $when
  } elseif {$when == [et]} {
    let2 $v $what $when
  } else {
    msg "warning: $v didn't have data for the st..et"
  }
}

proc letn {v what when} {
  if {$when < [et]} {
    let2 $v $what $when
  } elseif {[::commands::${v}@ index end] != [et]} {
    let2 $v $what [expr min([et], $when)] 
  } else {
    # msg "let $v $what $when ignored"
  }
}

proc let2 {v what when} {
#  puts "let2 $v $what [time::format $when]"
  if {[samples $v] > 0} {
    set bt [::commands::${v}@ index end]
    set bv [::commands::${v}_ index end]
    if {abs($what - $bv) <= [set ::commands::${v}=(-dv)]} { # no change no save
      return
    }
  } else {
    set bt 0
  }

  ::commands::${v}_ append $what
  ::commands::${v}@ append $when
  incr ::commands::${v}#
  set ::commands::$v $what

  if {$when < $bt} { 
      msg "warning: timetravel let $v $what [time::format $when] before [time::format $bt]" 
    ::commands::${v}@ sort ::commands::${v}_
  } 
  # used by overtime code to see the current state
}

command samples {v} {the number of samples in v} {
  return [set ::commands::${v}#]
}

command when# {v i} {the time for sample i in v} {
  # N.B. index is used rather than ($i) because of a buglet in BLT
  # which happens for vectors inside namespaces.
  return [::commands::${v}@ index $i]
}

command setwhen# {v i t} {set the time for sample i in v} {
  set ::commands::${v}@($i) $t
}

command what# {v i} {the value for sample i in v} {
  return [::commands::${v}_ index $i]
}

command first# {v} {return the first sample for v} {
  return [what# $v 0]
}

command last# {v} {return the last sample for v} {
  return [what# $v [expr [sample $v] - 1]]
}

command let# {v what i} {v at offset i has value what} {
  if {$i < 0 || $i >= [samples $v]} {
    error "let# $v $what $i out of bounds"
  }
  ::commands::[set v]_ index $i $what  
}

# slow for now
command what {v t} {return value for v at time t} {
  for {set i 0} {$i < [samples $v] - 1} {incr i} {
    if {[when# $v $i] <= $t && $t < [when# $v [expr $i+1]]} {
      return [what# $v $i]
    } 
  } 
  return [what# $v [expr [samples $v] - 1]]
}

array set ::commands::file_to_varname {}
command file_to_varname {fn} { convert a filename to variable name} {
  set s [file tail [file rootname $fn]] 
  set s [string map [array get ::commands::file_to_varname] $s]
  return $s
}


# time

namespace eval time {}

proc time::scan {s} {
  if {[string is double $s]} {
    return $s
  }
  regexp {([^.]*)([.][0-9]+)?[+]([0-9][0-9]:[0-9][0-9])?$} $s -> ts ms tz
  if {[catch "clock scan $ts -format %Y-%m-%dT%H:%M:%S$tz" t0]} {
    set t0 [clock scan $ts]
  }
  set r [expr $t0 + 0$ms]
  # puts "$s -> $r"
  return $r
}

proc time::format {t} {
  return [clock format [expr int($t)]  \
	      -format %Y-%m-%dT%H:%M:%S][::format .%03d [expr int(1000*($t-int($t)))]]
}

array set ::monthnames {
  1 Jan 2 Feb 3 Mar 4 Apr 5 May 6 Jun 7 Jul 8 Aug 9 Sep 10 Oct 11 Nov 12 Dec
}

command monthname {n} {
  translate a month number <tt>n</tt> in 1..13 to a name, e.g. (2=Feb, note 1 or 13 are <em>both</em> Jan)
} {
  require {[string is int $n] && 1<= $n && $n <= 13} ;# 13=Jan
  if {$n == 13} {
    set n 1
  }
  ensure {[info exists ::monthnames($n)]}
  return $::monthnames($n)
}

command month:next {n} {map month n to the next month (1->2,.., 13->1)} {
  incr n
  if {$n > 12} {
    set n 1
  }
  return $n
}

# start/end time commands which limit the period we are interested 
# in by causing let to ignore the data

set ::st [expr [clock seconds] - [expr 30 * 24 * 60 * 60]]
set ::st [expr 7 * 24 * 60 * 60]; # >0 to make PI happy
set ::st_f [time::format $::st]

command st {{t {}}} {
  set start time to <tt>t</tt>, if no t returns current start time
} {
  if {$t ne {}} {
    set ::st [time::scan $t]
    set ::et [expr max($::st,$::et)]
    set ::st_f [time::format $::st]
    set ::et_f [time::format $::et]
    msg "st [time::format $::st]"
  }
  return $::st
}

set ::et [clock seconds]
set ::et_f [time::format $::et]

command et {{t {}}} {
  set end time to <tt>t</tt>, if not <tt>t</tt> returns end time
} {
  if {$t ne {}} {
    set ::et [time::scan $t]
    set ::st [expr min($::st,$::et)]
    set ::st_f [time::format $::st]
    set ::et_f [time::format $::et]
    msg "et [time::format $::et]"
  }
  return $::et
}

# PDF driver using ghostscript - this is pretty horrible since it has
#  to work in different coordinate systems and munge the postscript
#  in order to generate things in the right place.

namespace eval pdf {
  variable filename
  variable psfilename
  variable fd "no such file"
  variable gsopts \
      "-q -dBATCH -sDEVICE=pdfwrite -dNOPAUSE -dSAFER -sPAPERSIZE=a4"
  variable height [winfo fpixels . 11i]
  variable pagex 
  variable pagey 
  variable default_file "tma-default.pdf"
}

proc pdf::new {file} {
  variable filename
  variable psfilename
  variable fd 
  variable height
  variable pagex
  variable pagey

  set filename $file
  set psfilename [file rootname $filename].ps
  set fd [open $psfilename w]
  set pagex [winfo fpixels . 0.5i]
  set pagey [expr $height - [winfo fpixels . 0.5i]]
}

proc pdf::default {} {
  variable fd
  variable default_file

  if {[catch [list tell $fd]]} { # not open so start the default
    msg "pdf::default opening $default_file"
    pdf::new $default_file
  } else {
    # msg "pdf::default already open"
  }
}

proc pdf::show {} {
  variable fd
  variable filename
  variable tmpfilename
  variable gsopts
  variable psfilename
  newpage
  puts $fd "%EOF"
  close $fd 
  if {$::tcl_platform(platform) eq "windows"} {
    exec $::options(-windows_gs) {*}$gsopts \
	-sOutputFile=$filename -f $psfilename
    file delete $psfilename
    exec $::options(-windows_pdf_reader) $filename & 
  } else {
    exec $::options(-unix_gs) {*}$gsopts \
	-sOutputFile=$filename -f $psfilename
    file delete $psfilename
    exec $::options(-unix_pdf_reader) $filename & 
  }
}

proc pdf::newpage {} {
  pdf::default
  variable pagex 
  variable pagey 
  variable height 
  set pagex [winfo fpixels . 0.5i]
  set pagey [expr $height - [winfo fpixels . 0.5i]]
  variable fd
  puts $fd "showpage"
}

proc pdf::pixels2i {p} {
  return  [expr ($p/[winfo fpixels . 1i])]
}

proc pdf::vbox {d} {
  variable pagex
  variable pagey
  if {$pagey - [winfo fpixels . $d] < [winfo fpixels . 1i]} {
    newpage
  }
  set r "-pagex [pixels2i ${pagex}]i -pagey [pixels2i ${pagey}]i" 
  set pagey [expr $pagey - [winfo fpixels . $d]]
  return $r
}

proc pdf::vboxp {d} {
  variable pagex
  variable pagey
  variable height
  if {$pagey - [winfo fpixels . $d] < [winfo fpixels . 0.5i]} {
    newpage
  }
  set y [expr $height - $pagey]
  set r "-padx \{[pixels2i ${pagex}]i 0\} -pady \{[pixels2i ${y}]i 0\}" 
  append r " -height $d"
  set pagey [expr $pagey - [winfo fpixels . $d]]
  return $r
}

proc pdf::adjust {s} {
  set r {}
  set l 0
  for {set i 0} {$i < [string length $s]} {incr i} {
    set c [string index $s $i] 
    if {$c eq "\n"} {
      append r $c
      set l 0
    } elseif {$l < 90} {
      append r $c
      incr l
    } elseif {$l >= 97 && [string is space $c]} {
      append r "\n"
      set l 0
    } elseif {$l >= 80} {
      append r "$c\n"
      set l 0
    } else {
      append r $c
      incr l
    }
  }
  return $r
}

proc pdf::text {s font} {
  pdf::default

  # first break fmt s so it fits
  set s [pdf::adjust $s]

  # and then build up the PDF
  variable fd
  toplevel .pdftext
  wm attributes .pdftext -fullscreen 1
  set w .pdftext.t
  canvas $w -width 16i -height 10i
  pack $w

  
  set nlines [llength [split $s \n]]
  set linespace [font metrics $font -linespace]
  set h [pixels2i [expr $nlines * $linespace]]
  
  $w create text 1m 1m -text $s -anchor nw -font $font
  update
  after 20
  update
  after 50
  puts $fd [string map {%EOF {} showpage {}} \
		[$w postscript -height ${h}i  -pageanchor nw \
		     {*}[vbox ${h}i]]]
  update
  destroy .pdftext
}

array set pdf::plotopts {
  -center 0
  -colormode color 
  -decorations 0
  -landscape 0
  -maxpect 1
  -paperwidth 7.0i
  -paperheight 11.4i
  -width 7i
}

proc pdf::plot {w h args} {
  update
  variable fd
  variable plotopts
  array set opts [array get plotopts]
  array set opts $args
  report:text \n ;# ttt hack but it works
  puts -nonewline $fd [string map {%EOF {} showpage {}} \
			   [$w postscript output \
				{*}[array get opts] \
				{*}[vboxp $h]]]
}

# report generation

command report:new  {filename} {
  Start a new report to <tt>filename</tt>
} {
  pdf::new $filename
}

command report:text {t {font {Helvetica 12 normal}}} {
  Add text <tt>t</tt> in font <tt>font</tt> to report. 
  Note that variables and commands are expanded.
} {
  pdf::text [uplevel 1 [list subst $t]] $font
}

command report:plaintext {t {font {Courier 12 italic}}} {
  Add text <tt>t</tt> in font <tt>font</tt> to report.
  Note that variables and commands are not expanded.
} {
  pdf::text $t $font
}

command report:plot {h} {
  Add a plot with height <tt>h</tt>.
} {
  pdf::plot .w $h
}

command report:fullplot {} {
  Add a full page plot with height <tt>h</tt>.
} {
  pdf::plot .w 8i -landscape 1 -width 12i
}

command report:header {t} {
  Generate a report header
} {
  report:text "$t" {Helvetica 14 bold}
}

command report:newpage {} {
  Generate a new page in the report
} {
  pdf::newpage
}

command report:show {} {
  show the final report to the user
} {
  update
  update idletasks
  pdf::show
}

command title {t} {use title for the plot} {
  .w configure -title $t
}

command scale {v s} {scale v by s, e.g. 'scale FedP 2' doubles FedP} {
  ::commands::${v}_ set [blt::vector expr "::commands::${v}_ * $s"]
  return "scaled $v by $s"
}

command scalet {v s} {scale v timebase by s, e.g. 'scalet FedP 2' slow down FedP} {
  ::commands::${v}@ set [blt::vector expr "::commands::${v}@ * $s"]
  return "scalet $v time by $s"
}

command offset {v o} {offset v by o, e.g. 'offset FedP 2' adds 2} {
  ::commands::${v}_ set [blt::vector expr "::commands::${v}_ + $o"]
}

command offsett {v o} {offset v timebase by o, e.g. 'offsett FedP 2' adds 2s} {
  ::commands::${v}@ set [blt::vector expr "::commands::${v}@ + $o"]
}

command offset_remove {v} {remove offset for v by subtracting first value} {
  if {[samples $v] > 0} {
    offset $v [expr - [what# $v 0]]
  } else {
    msg "offset_remove $v - warning no samples to offset"
  }
}

# pi tags interface to metadata on the PI server
array set tags_descr {}
array set tags_units {}
array set tags_all_units {}
array set tags_all_sites {}

command tags:clear {} {clear all tag info} {
  array set ::tags_descr {}
  array set ::tags_units {}
}

# use the server program
command tags:getserver {} {get tag info from the server -- this is slow} {
  msg "tags:getserver - reading tag info from server $::options(-pi_server)"
  msg "tags:getserver - warning this is very slow"
  set cmd [list open "|$::options(-pitag_exe) $::options(-pi_server)" r]  
  if {[catch $cmd fd]} {
    msg "tags:readserver failed with $fd error"
    return 0
  }
  set n 0
  while {[gets $fd line] >= 0} {
    set fields [split $line ,]
    set ::tags_descr([lindex $fields 0]) [lindex $fields 1]
    set ::tags_units([lindex $fields 0]) [lindex $fields 2]
    set ::tags_all_units([lindex $field 2]) 1
    if {($n % 1000) == 0} {
      msg "$n read"
    }
    incr n
  }
  close $fd
  return 1
}

command tags:put {file} {put current list of tags into a file} {
  set fd [open $file w]
  foreach t [pi:tags *] {
    puts $fd "$t,$::tags_descr($t),$::tags_units($t)"
  }
  close $fd
}

command tags:get {file} {get current list of tags from a file} {
  set fd [open $file r]
  while {[gets $fd line] >= 0} {
    set fields [split $line ,]
    set tag [lindex $fields 0]
    if {![string match $::options(-pi_tags) $tag]} {
      continue
    }
    set description [lindex $fields 1]
    set units [lindex $fields 2]
    set units [string map {" " {}} $units]
    set ::tags_descr($tag) $description
    set ::tags_units($tag) $units
    set ::tags_all_units($units) 1
    if {[string match "R_*" $tag]} {
      set ::tags_all_sites([pi:site $tag]) 1
    }
  }
  close $fd
}

command tags:only {tagpat} {remove any not matching tagpat} {
  foreach tag [pi:tags *] {
    if {[string match $tagpat $tag]} {
      continue
    }
    array unset ::tags_descr $tag
    array unset ::tags_units $tag
  }

  array unset ::tags_all_units *
  foreach {t u} [array get ::tags_units] {
    set ::tags_all_units($u) 1
  }
}

proc tags:init {} {
  if {[catch "tags:get pi.tags"]} {
    msg "tags:init cannot read pi.tags file, recreating it from server"
    tags:getserver
    tags:put pi.tags
  } else {
    # we've just read it in from pi.tags above
  }
}

command pi:tags {{pat *}} {return all pi tags matching pat} {
  return [match $pat [array names ::tags_descr]]
}

command pi:description {tag} {return the description for tag} {
  return $::tags_descr($tag)
}

command pi:units {tag} {return the units for the tag} {
  return $::tags_units($tag)
}

command pi:all_units {} {return all the units} {
  return [lsort -dictionary [array names ::tags_all_units]]
}

command pi:all_sites {} {return all the sites} {
  return [array names ::tags_all_sites]
}

command pi:pwcregion {tag} {return the region for a PWC tag} {
  array set r {
    D Darwin
    A "Alice Springs"
    T "Tennant Creek"
    K "Katherine"
  }
  return $r([lindex [split $tag _] 1])
}

command pi:site {tag} {return the community id for a PWC tag} {
  return [lindex [split $tag _] 3]
}

command pi:part {tag n} {return part of the tagname at n using _ as a sep} {
  return [lindex [split $tag _] $n]
}

# linear ramping functions

command ramp:triangle {v rate} {generate a triangle wave} {
  var $v
  display $v
  set y 0.0
  set dir 1.0
  for {set t [st]} {$t <= [et]} {set t [expr $t + 1.0]} {
    let $v $y $t
    set y [expr min(1.0,max(0.0,$y + $dir*$rate))] 
    if {$y == 0.0 || $y == 1.0} {
      set dir [expr -$dir]
    }
  }
}

command filter:ramplimit {v up down} {ramp limit v upwards by up/s and down/s} {
  for {set i 1} {$i < [samples $v]} {incr i} {
    set dt [expr [when# $v $i] - [when# $v [expr $i-1]]]
    set bv [what# $v [expr $i-1]]
    set val [what# $v $i]
    set vmax [expr $bv + ($up*$dt)]
    set vmin [expr $bv - ($down*$dt)]
    let# $v [expr max($vmin,min($vmax,$val))] $i
  }

}

command random:flat {v {low 0} {high 1}} {random numbers low..high} {
  var $v
  display $v
  for {set t [st]} {$t <= [et]} {incr t} {
    let $v [expr rand()] $t
    scale $v [expr $high - $low]
    offset $v $low
  }
}

command random:boundedwalk {v {pru 0.01} {up 0.01} {prd 0.01} {down 0.01}} {generate a random walk on 0..1} {	
  var $v -dv 0.02
  display $v
  set w 0.0 
  for {set t [st]} {$t <= [et]} {set t [expr $t + 1]} {
    if {[expr rand() < 0.5]} {
      if {[expr rand() <= $pru]} {
	set w [expr $w + $up]
      }
    } else {
      if {[expr rand() <= $prd]} {
	set w [expr $w - $down]
      }
    }
    set w [expr min(1.0,max(0.0,$w))]
    let $v $w $t
  }
}

command random:unboundedwalk {v {pru 0.01} {up 0.01} {prd 0.01} {down 0.01}} {generate an unbounded random walk} {	
  var $v -dv 0.02
  display $v
  set w 0.0 
  for {set t [st]} {$t <= [et]} {set t [expr $t + 1]} {
    if {[expr rand() < 0.5]} {
      if {[expr rand() <= $pru]} {
	set w [expr $w + $up]
      }
    } else { 
      if {[expr rand() <= $prd]} {
	set w [expr $w - $down]
      }
    } 
    let $v $w $t
    if {[expr ($t % 10000)] == 0} {
      update
    }
  }
}

# ttt this doesn't do delta compression since it writes lots
# of repeats
command filter:deadband {v db} {filter using a +/- db deadband} {
  set val [what# $v 0]
  for {set i 1} {$i < [samples $v]} {incr i} {
    set cval [what# $v $i]
    if {abs($cval - $val) > $db} {
      set val $cval
    } 
    let# $v $val $i
  }
}

# ttt some problem as deadband
command filter:quantise {v s} {keep within quantised steps of size s} {
  for {set i 0} {$i < [samples $v]} {incr i} {
    set val [what# $v $i]
    let# $v [expr ($s*round($val/$s)) +0.5*$s] $i
  }
}

command filter:magictkln {v s} {tkln magic filter} {
  for {set i 1} {$i < [samples $v]} {incr i} {
    set pv [what# $v [expr $i-1]]
    let# $v [expr ($pv + [what# $v $i])/2.0] $i
  }
  set cval [what# $v 0]
  for {set i 1} {$i < [samples $v]} {incr i} {
    set val [what# $v $i]
    set err [expr abs($cval - $val)]
    if {$err > 30} {
      set cval [expr 15+(30.0*floor($val/30.0))]
    }
    let# $v $cval $i
  }
}



command display {v args} {display v on main plot with options args} {
  msg "display $v $args"
  plot::display .w $v \
      -mapy [property $v -mapy] \
      {*}$args
  return "display $v $args"
}

command display- {{pat *}} {undisplay variables matching pat} {
  foreach v [vars $pat] {
    plot::delete .w $v
  }
}

command forallt {vars code} {for all vars over time do code} {
  gui::busy 
  msg "forallt $vars $code"
  foreach v $vars {
    set i($v) 0
    set ::commands::$v 0 ;# not true but...
    set ::commands::${v}_b 0
  }

  set ::commands::t_b 0
  set ::commands::t 0 ;# to avoid time travel errors
  set mm 0
  set total_cycles 0
  while {1} {
    set vmin {}
    foreach v $vars {
      if {$i($v) < [samples $v] && 
	  ($vmin == {} || [when# $v $i($v)] < [when# $vmin $i($vmin)])} {
	set vmin $v
      }
    }
    if {$vmin == {}} {
      break
    } else {
      set ::commands::t_b $::commands::t
      set ::commands::${vmin}_b [set ::commands::$vmin]
      set ::commands::t [when# $vmin $i($vmin)]
      if {$::commands::t_b == 0} {
	set ::commands::dt 0
      } else {
	set ::commands::dt [expr $::commands::t - $::commands::t_b]
      }
      
      # puts "dt = $::commands::dt"
      set ::commands::$vmin [what# $vmin $i($vmin)]
      # ttt - note this is not a safe interpreter so consider
      # what happens...
      if {$::commands::dt > 0} {
	set result [namespace eval ::commands $code]
      }
      incr i($vmin)
    }
    if {[incr total_cycles] >= 2000} {
      update
      update idletasks
      set total_cycles 0
    }
  }
  gui::unbusy
  return ""
}

command put {filename vars} {descr} {
  msg "put $filename $vars"
  set fd [open $filename w]
  # print header line
  puts -nonewline $fd "t"
  foreach v [vars $vars] {
    puts -nonewline $fd ",$v"
  }
  puts $fd ""

  # generate records
  forallt [vars $vars] "::putworker $fd [vars $vars]" 
  close $fd
  return "put $filename $vars -> $filename"
}

proc putworker {fd args} {
  puts -nonewline $fd "[time::format $::commands::t]"
  foreach v $args {
    puts -nonewline $fd ",[set ::commands::$v]"
  }
  puts $fd ""
}

command statistics {{vars *}} {statistics for vars} {
  foreach v [vars $vars] { # simple stats over *_
    msg "$v samples [samples $v]"
    foreach stat {min q1 mean q3 max} {
      msg "$v $stat [blt::vector expr [set stat](::commands::[set v]_)]"
    }
  }
}

command what? {stat v} {statistic stat for v, e.g. what? min v} {
  return [blt::vector expr [set stat](::commands::${v}_)]
}

command stop {{t "forever"}} {stop the model running for t seconds} {
  if {$t eq "forever"} {
    # just wait forever
  } else {
    after [expr 500 + ($t * 1000)] {set ::forever 1}
  }
  vwait ::forever
}

command debug:trace {n} {trace executions at level n, 0 is off} {
  blt::bltdebug $n
}

command sumt {v} {sum over time for v} {
  set r 0.0
  for {set i 0} {$i < [samples $v] - 1} {incr i} {
    set r [expr $r + ([what# $v $i] * ([when# $v [expr $i+1]] - [when# $v $i]))]
  }
  return $r
}

command clone {v vn args} {clone vn from v with args} {
  var $vn {*}$args
  forallt $v "let $vn \$$v \$t"
}

command quit {{status 0}} {quit the program with status} {
  ::exit $status
}

command chartoptions {w o} {return the current options for w o} {
  foreach a [$w $o configure] {
    append r " [lindex $a 0] [list [lindex $a 4]]\n"
  }
  append r "\n"
  return $r
} 

command xaxis {args} {
  configure xaxis<br>
  <pre>[chartoptions .w xaxis]</pre>
} {
  msg "xaxis $args"
  if {$args eq {}} {
    return [chartoptions .w xaxis]
  }
  array set a $args
  if {[info exists a(-min)]} {
    st $a(-min)
  }
  if {[info exists a(-max)]} {
    et $a(-min)
  }
  catch {
    unset a(-min)
    unset a(-max)
  }

  .w xaxis configure {*}[array get a]
}

command yaxis {args} {
  configure yaxis<br>
  <pre>[chartoptions .w yaxis]</pre>
} {
  if {$args eq {}} {
    return [chartoptions .w yaxis]
  }
  .w yaxis configure {*}$args
}

command y2axis {args} {
  configure y2axis<br>
  <pre>[chartoptions .w y2axis]</pre>
} {
  if {$args eq {}} {
    return [chartoptions .w y2axis]
  }
  .w y2axis configure {*}$args
}

command legend {args} {
  configure legend<br>
  <pre>[chartoptions .w legend]</pre>
} {
  if {$args eq {}} {
    return [chartoptions .w legend]
  }
  .w legend configure {*}$args
}

command chartgrid {args} {
  configure chartgrid<br>
  <pre>[chartoptions .w grid]</pre>
} {
  if {$args eq {}} {
    return [chartoptions .w grid]
  }
  .w grid configure {*}$args
}

command chartcrosshairs {args} {
  configure chart crosshairs<br>
  <pre>[chartoptions .w crosshairs]</pre>
} {
  if {$args eq {}} {
    return [chartoptions .w crosshairs]
  }
  .w crosshairs configure {*}$args
}
  
command hline {y} {draw horizontal lines for each argument y} {
  .w marker create line -dashes {5 5} -coord {-Inf $y +Inf $y} 
}

command colorperiod {low high {col red}} {color period from low to high to col} {
  set low [time::scan $low]
  set high [time::scan $high]
  .w marker create polygon \
      -fill $col \
      -coord "$low +Inf $low -Inf $high -Inf $high +Inf $low +Inf"
}

command show {{args *}} {show vars matching this pattern on plot} {
  foreach pat $args {
    foreach e [.w element names $pat] {
      plot::show .w $e
    }
  }
  update
}

command hide {{args *}} {hide vars matching this pattern on plot} {
  foreach pat $args {
    foreach e [.w element names $pat] {
      plot::hide .w $e
    }
  }
  update
}

command reportevery {title {period {}} {args *}} {report for every day} {
  puts "reportevery [st] [et] $period"
  if {$period eq {}} {
    set period [expr 24*60*60]
  }
  hide *
  foreach p $args {
    show [vars $p]
  }
  for {set d [st]} {[st] + $period <= [et]} {set d [expr $d + $period]} {
    xaxis -min $d -max [expr $d + $period]
    report "$title for [time::format [st]]..[time::format [et]]" {} .w
  }
}

# ask:* commands to interact with humans

command ask:year {} {ask the user to select a particular year} {
  set defy [expr [clock format [clock seconds] -format %Y]]
  incr defy -1 ;# zero based 
  set l {}
  for {set  y [expr $defy - 2]} {$y <= $defy + 10} {incr y} {
    lappend l $y
  }
  set r [tk_dialog .ask \
	     "TMA" \
	     "Which year?" \
	     {} \
	     3 \
	     {*}$l]
  if {$r == -1} {
    error "ask:year - no year selected"
  } 
	 
  return [lindex $l $r]
}

command ask:month {} {ask the user to select a particular month} {
  set defm [expr [clock format [clock seconds] -format %m] -1]
  incr defm -1 ;# zero based 
  if {$defm < 0} { 
    set defm 11
  }
  set r [tk_dialog .ask \
	     "TMA" \
	     "Which month?" \
	     {} \
	     $defm \
	     "Jan" "Feb" "Mar" "Apr" "May" "Jun" \
	     "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"]
  if {$r == -1} {
    error "ask:month -  no month selected"
  } 
	 
  return [expr $r + 1]
}

command ask:oneof {text args} {ask the user to select something from args} {
  set r [tk_dialog .ask \
	     "TMA" \
	     $text \
	     {} \
	     {} \
	     {*}$args]
  if {$r == -1} {
    error "ask:oneof - none selected"
  }
  return [lindex $args $r]
}

command ask:checkplot {text t w args} {ask the user about an event} {
  xaxis -min [expr $t-($w/2.0)] -max [expr $t+($w/2.0)]
  ask:oneof $text {*}$args
}

# duration histograms

command report:duration {v} {generate a duration vs value statistics for v} {
  puts "report:duration $v"
  array set duration {}
  set dt 0
  for {set i 1} {$i < [samples $v]} {incr i} {
    set val [what# $v $i]
    set dt [expr [when# $v $i] - [when# $v [expr $i - 1]]] 
    if {[info exists duration($val)]} {
      set duration($val) [expr $duration($val) + $dt]
    } else {
      set duration($val) $dt
    }
  }
  set sum 0
  set min {}
  set max {}
  foreach {_ d} [array get duration] {
    set sum [expr $sum + $d]
    if {$_ < $min || $min == {}} {
      set min $_
    }
    if {$_ > $max || $max == {}} {
      set max $_
    }
  }

  
  # scale from pu to %
  foreach {val d} [array get duration] {
    set duration($val) [expr 100.0*($d/$sum)]
  }
 
  # 1% scan from min to max
  if {$max == {}} {
    set max 10
  }
  if {$min == {}} {
    set min 0
  }
  for {set low [expr min(0,$min)]} \
      {$low < [expr max(100,$max)]} \
      {set low [expr $low + ($max-$min)/10.0]} {
    set bucket($low) 0.0
    foreach {val d} [array get duration] {
      if {$low <= $val && $val < $low + ($max-$min)/10.0} {
 	set bucket($low) [expr $bucket($low) + $d]
      }
    }
  }

  toplevel .t
  set bw [blt::barchart .t.w]
  set xd {}
  set yd {}
  foreach {x y} [array get bucket] {
    lappend xd [expr ($x + 5)]
    lappend yd $y
  }
  $bw element create e1 \
      -xdata $xd -ydata $yd \
      -barwidth [expr ($max-$min)/20.0]
  $bw configure -relief flat
  $bw legend configure -hide 1
  $bw yaxis configure -min 0 -max 100 -title "% Duration"
  $bw xaxis configure -stepsize 10 \
      -rotate 0 -title $v -command duration:format \
      -min [expr min(0,$min)] \
      -max [expr max(100,$max)]
  pack $bw -fill both -expand 1
  pdf::plot .t.w 3i
  update 
  destroy .t
}

proc duration:format {w t} {
  return "$t.."
}


# test/assert tools - note this is for internal testing, for data validation
#  of PI, etc use check:*

array set test {
  passed 0
  failed 0
  total 0
}

command test {e} {the expression must be true, if not error} {
  incr ::test(total)
  msg "test $e"
  if {![uplevel 1 [list expr $e]]} {
    incr ::test(failed)
    msg "FAIL: test $e failed ($::test(failed)/$::test(total))"
    error "test $e failed"
  } else {
    incr ::test(passed)
    msg "PASS: test $e passed ($::test(passed)/$::test(total))"
  }
}

command assert {e} {the expression must true, if not error} {
  if {![uplevel 1 [list expr $e]]} {
    error "assert $e failed"
  }
}

command ensure {e} {the expression must true, if not error} {
  if {![uplevel 1 [list expr $e]]} {
    error "ensure $e failed"
  }
}

command require {e} {the expression must true, if not error} {
  if {![uplevel 1 [list expr $e]]} {
    error "require $e failed"
  }
}



# balloon help - ttt should work for toplevel widgets but doesn't

proc balloon {w help} {
  bind $w <Any-Enter> "after 1000 [list balloon:show %W [list $help]]"
  bind $w <Any-Leave> "destroy %W.balloon"
}

proc balloon:show {w arg} {
  if {[eval winfo containing  [winfo pointerxy .]]!=$w} {return}
  set top $w.balloon
  catch {destroy $top}
  toplevel $top -bd 1 -bg black
  wm overrideredirect $top 1
  if {[string equal [tk windowingsystem] aqua]}  {
    ::tk::unsupported::MacWindowStyle style $top help none
  }   
  pack [message $top.txt -aspect 10000 -bg lightyellow \
            -font fixed -text $arg]
  set wmx [winfo rootx $w]
  set wmy [expr [winfo rooty $w]+[winfo height $w]]
  wm geometry $top \
      [winfo reqwidth $top.txt]x[winfo reqheight $top.txt]+$wmx+$wmy
  after 4000 "destroy $top"
  raise $top
}

# plot support

namespace eval plot {}

array set plot::plots {}

proc plot::new {w args} {
  blt::stripchart $w -height 5i {*}$args

  $w legend configure -font "Helvetica 8 bold"

  $w xaxis configure -command plot::format_time
  $w xaxis configure -minorticks {0.0 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9}
  $w yaxis configure -minorticks {0.0 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9}
  $w y2axis configure -minorticks {0.0 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9}
  $w y2axis configure -hide 0
  $w legend configure -relief flat

  Blt_Crosshairs $w 
  $w crosshairs configure -color gold -dashes {6 3}

  $w grid configure -hide no -dashes { 2 2 } -color #f0f0f0
  Blt_ZoomStack $w
  
  # mouse scroll left or right
  bind $w <Button-4> "plot::scroll $w 0.02"
  bind $w <Button-5> "plot::scroll $w -0.02"
  # ttt see if works fg
  # bind $w <MouseWheel> "plot::scroll $w \[expr %D/480.0\]"

  # arrow keys are bound to . for now
  bind . <Right> "plot::scroll $w 0.02"
  bind . <Left> "plot::scroll $w -0.02"
  bind . <Shift-Right> "plot::scroll $w 1"
  bind . <Shift-Left> "plot::scroll $w -1"

  # active line
  $w pen configure activeLine -color black -pixels 2 -symbol square 
  $w element bind all <Enter> {
    %W legend activate [%W element get current]
  }
  $w element bind all <Leave> {
    %W legend deactivate [%W element get current]
  }

  # configuration daemons
  $w yaxis bind <Button-1> "plot::yaxis_configure $w y"
  $w y2axis bind <Button-1> "plot::yaxis_configure $w y2"

  # configure background for printing
  $w configure -background white ;# nice background + good for printing

  # markers - ttt
  # bind $w <Double-1> "plot::marker %W %x %y" 

  set ::plots($w) 1 
  return $w
}

proc plot::display {w v args} {
  # parray ::commands::${v}=
  $w element create $v \
      -ydata ::commands::${v}_ -xdata ::commands::${v}@ \
      -color [property $v -color] \
      -smooth step \
      -pixels [property $v -pixels] \
      -symbol [property $v -symbol] \
      {*}$args

  $w legend bind $v <Button-1> "plot::cycle $w $v"

  set ::state($v) 1
}

proc plot::delete {w v} {
  $w element delete $v
}

# cycle v on plot w with color c visibility
proc plot::cycle {w v} {
  puts "plot::cycle $w $v $::state($v)"
  if {$::state($v) == 1} {
    $w element configure $v -hide 0 -color black -symbol square 
    incr ::state($v)
  } elseif {$::state($v) == 2} {
    $w element configure $v -hide 1 -color white -symbol none
    incr ::state($v)
  } else {
    $w element configure $v -hide 0 -color [property $v -color] -symbol none
    set ::state($v) 1
  }
}

proc plot::hide {w v} {
  $w element configure $v -hide 1 -color white -symbol none
  set ::state($v) 3
}

proc plot::show {w v} {
  $w element configure $v -hide 0 -color [property $v -color] -symbol none
  set ::state($v) 1
}

set ::plot::markerN 1

proc plot::marker {w x y} {
  foreach {when what} [$w invtransform $x $y] { }
  $w marker create text -name #[incr ::plot::markerN] \
      -coords {$when +Inf} -text "       \#$::plot::markerN"
  $w marker create line -name L$::plot::markerN \
      -dashes {5 5} -coords {$when +Inf $when -Inf}
}

# format time on X-axis
proc plot::format_time {w s} {
  return [clock format [expr int($s)] \
	      -format $::options(-plot_format_time)]
}

# plot period of p seconds in w
proc plot::period {w p} { 
  if {$p == {}} {
    $w xaxis configure -min {} -max {}
  } else {
    foreach {minx maxx} [$w xaxis limits] { }
    set tc [expr ($minx + $maxx) / 2.0]
    $w xaxis configure -min [expr $tc - $p/2.0] -max [expr $tc + $p/2.0]
  }
}

# plot scroll w d (ratio of width)
proc plot::scroll {w d} {
  foreach {minx maxx} [$w xaxis limits] { }
  set step [expr ($maxx - $minx) * $d]
  set minx [expr $minx + $step]
  set maxx [expr $maxx + $step]
  $w xaxis configure -min $minx -max $maxx
}


# configure x/y/y2 axis


proc plot::yaxis_configure {w a} {
  button_box "Y-axis configure" \
      "Autorange" "plot::range $w $a {} {}" \
      "0.." "plot::range $w $a 0 {}" \
      "0 to 500" "plot::range $w $a 0 500"
}

proc plot::range {w a min max} {
  # foreach {miny maxy} [$w axis limits $a] { }
  $w axis configure $a -min $min -max $max
}

# popup button selection
proc button_box {title args} {
  set cols 8
  destroy .button_box
  set d [toplevel .button_box]
  wm title $d $title
  set n 0
  foreach {t c} $args {
    grid [button $d.b$n -text $t -command "$c"] \
	-row [expr $n/$cols] -column [expr $n%$cols] \
	-sticky nsew
    incr n
  }
  grid [button $d.b$n -text "OK" -command "destroy $d"] \
      -row [expr $n/$cols] -column [expr $n%$cols] \
      -sticky nsew
}

# printing support 

proc plot::print {w} {
  switch $::tcl_platform(platform) {
    "unix" {
      plot::print_unix $w
    }
    "windows" {
      plot::print_windows $w
    }
  }
}

proc plot::print_unix {w} {
  set tmap tma_tmap.ps ;# temporary file
  set fn [tk_getSaveFile -defaultextension .pdf -filetypes {{PDF .pdf}}]
  $w postscript output $tmap -maxpect 0 -center 0 -landscape 1 \
      -paperheight 12i -paperwidth 14i
  ps2pdf $tmap $fn
  exec evince $fn &
}

proc plot::print_windows {w} {
  # Get a list of printers (local, not network)
 set names [blt::printer names]

 # Open the first one
 set pid [blt::printer open [lindex $names 0]]

 # Print the graph
 $w print1 $pid

 # Close the printer.
 blt::printer close $pid
}

proc plot::print_copy {w} {
  $w snap -format emf CLIPBOARD
}

# file - the file input part
namespace eval file {}

# get a file
command get {{f {}} args} {get a file from time start to end and process it based on its type} {
  if {$f eq {}} {
    set files [tk_getOpenFile \
		   -multiple 1 \
		   -filetypes {
		     {{All files}  *}
		     {{CSV data}  {.csv}}
		     {{TMA scripts} {.tma}}
		     {{PI file data}  {.pi}}
		     {{ACEP NetCDF data}  {.nc}}
		   }]
    if {$files == {}} {
      return
    }
    foreach f $files { 
      get $f {*}$args
    }
    return
  }
  gui::busy
  if {[string match *:* $f]} {
    file::readtrendr $f
  } elseif {[string match *.tma $f]} {
    file::readtma $f {*}$args
  } elseif {[string match *.csv $f] ||\
	    [string match *.smt $f]} {
    file::readcsv $f {*}$args
  } elseif {[string match *.pi $f]} {
    file::readpifile $f {*}$args
  } elseif {[string match *.nc $f]} {
    file::readnetcdffile $f {*}$args
  } else {
    file::readpi $f {*}$args
  }
  gui::unbusy
}

# map a file to a variable name
proc file::file_to_name {fn} {
  return [commands::file_to_varname $fn]
}

# read a CSV file
proc file::readcsv {a args} {
  foreach fn [glob -nocomplain $a] {
    set fd [open $fn]
    gets $fd line
    close $fd
    set l [split $line ,]
    if {[lindex $l 0] eq "t"} {
      readasim $fn {*}$args
    } elseif {[lindex $l end] eq "Changed variable"} {
      readpcorp $fn {*}$args
    } elseif {[lindex $l 0] eq "Server"} {
      readsmt $fn {*}$args
    } else {
      msg "readcsv: cannot read $fn!"
    }
  }
}

# read a PI SMT file which is CSV in a slightly weird format
# but seems to be the only easy way to get data out of PI if 
# you use Excel 2003 and want to avoid the 64k limit.
proc file::readsmt {a args} {
  foreach fn [glob -nocomplain $a] {
    set tp 0
    set v [file_to_name $fn]
    if {[lsearch [vars] $v] != -1} {
      msg "ignoring $v"
      return
    }
    var $v {*}$args
    set fd [open $fn]
    gets $fd line
    while {[gets $fd line] > 0} {
      set l [split $line ,]
      set what [lindex $l 2]
      set when [clock scan [lindex $l 3] -format "%d/%N/%Y %l:%M:%S %P"]
      set tp $when
      if {[string is double $what] && [string is double $when]} {
	let $v $what $when
      } else {
	puts "ignored $line"
      }

      if {$when < $tp} {
	msg "time travel warning $fn $when < $tp"
	puts "$line"
	exit 1
      }

    }
    close $fd
    plot::display .w $v
  }
}

proc file::readnetcdffile {f args} {
  msg "readnetcdf $f"
  set varn [file rootname [file tail $f]]
  var $varn {*}$args
  set fd [open "|./acep-csv $f" r]
  while {[gets $fd line] > 0} {
    set l [split $line ,]
  
    let $varn [lindex $l 1] [lindex $l 0]
  }
  close $fd
  plot::display .w $varn
}


#* note: csv files must be in the ASIM format (t,v1,v2...)
proc file::readasim {fn args} {
  msg "readcsv $fn"
  foreach fn [glob -nocomplain $fn] {
    set fd [open $fn]
    gets $fd line
    set vars [split $line ,]
    foreach v [lrange $vars 1 end] {
      if {[lsearch [vars] $v] == -1} {
	var $v {*}$args
      }
    }
    set nl [llength $vars]
    while {[gets $fd line] > 0} {
      set l [split $line ,]
      if {[llength $l] < $nl} {
	error "$line doesn't have the right # of fields"
      }
      set when [time::scan  [lindex $l 0]]
      for {set i 1} {$i < $nl} {incr i} {
	set what [lindex $l $i]
	if {$what ne "" && 
	    [string is double $what] && 
	    [string is double $when]} {
	  let [lindex $vars $i] $what $when
	} else {
	  puts "ignored $line"
	}
      }
    }
    close $fd

    for {set i 1} {$i < $nl} {incr i} {
#      puts "Display .w [lindex $vars $i]"
      plot::display .w [lindex $vars $i]
    }
  }
}

proc file::readpcorp {fn args} {
  msg "readpcorp $fn"
  foreach fn [glob -nocomplain $fn] {
    set fd [open $fn]
    gets $fd line
    set vars [split $line ,]
    if {[lindex $vars end] eq "Changed variable"} { # Powercorp csv file
      set vars [lrange $vars 0 end-1]
    } else {
      error "Not a powercorp CSV file"
    }
    foreach v [lrange $vars 1 end] {
      if {[lsearch [vars] $v] == -1} {
	var $v {*}$args
	plot::display .w $v
      }
    }

    set nl [llength $vars]
    set nn 0
    while {[gets $fd line] > 0} {
      if {([incr nn] % 10000) == 0} {
	msg "reading sample $nn from $fn"
	update
	update idletasks
	after 1
      }
      # msg $line
      set l [split $line ,]
      if {[llength $l] < $nl} {
	error "$line doesn't have the right # of fields"
      }
      set when [time::scan  [lindex $l 0]]
      for {set i 1} {$i < $nl} {incr i} {
	set what [lindex $l $i]
	if {[string is double $what] && [string is double $when]} {
	  # puts "i=$i"
	  let [lindex $vars $i] $what $when
	} else {
	  puts "ignored $line"
	}
      }
    }
    close $fd
  }
}

# read a single pi tag
proc file::readpi {tag args} {
  # ttt review time format for pitool
  set start [clock format [expr int([st])] -format %d/%m/%Y]
  set end [clock format [expr int([et])] -format %d/%m/%Y]
  set server $::options(-pi_server)
  set command "|$::options(-pitool_exe) $server $tag \"$start\" \"$end\""
#  puts $command
  set fd [open $command r+] 
  set var [var $tag {*}$args]
  display $var 
  set nup 0
  while {[gets $fd line] >= 0} {
    foreach {when what} [split $line ,] { }
    set t [time::scan $when]
    let $var $what $t
    
    if {[incr nup] > 1000} {
      update
      set nup 0
    }
  }
  puts "EOF"
}

proc file::readpifile {file args} {
  msg "file::readpifile $file"
  set fd [open $file r]
  set var [file_to_varname $file]
  set var [var $var {*}$args]
  display $var
  while {[gets $fd line] > 0} {
    foreach {when what} [split $line ,] { }
    set t [time::scan $when]
    let $var $what $t
  }
}

proc file::readtrendr {l} {
  set host [lindex [split $l :] 0]
  set var [lindex [split $l :] 1]
  puts "connection to $host for $var"
  set fd [socket $host 50012]
  puts $fd "-st [st]"
  puts $fd "-et [et]"
  puts $fd "-read /trends/${var}.nc"
  puts $fd "-exit 0"
  flush $fd
  var $var
  display $var
  while {[gets $fd line] >= 0} {
    if {[scan $line "%f %f" what when]} {
      # puts "$var $what $when"
      let $var $what $when
    }
  }
}

proc file::readtma {fn args} {
  msg "reading $fn"
  if {$::options(-console_safe) == 0} {
    namespace eval ::commands [read [open $fn r]]
  } else { # safe interpreter
    console::source $fn
  }
}

# help - just some simple help thinglets

namespace eval help {}

proc help::show {s {title "*HELP*"}} {
  destroy .help
  toplevel .help
  wm title .help $title
  text .help.t -height 32 -width 132 -yscrollcommand ".help.s set"
  scrollbar .help.s -orient vertical -command ".help.t yview"
  tkhtml::render .help.t [subst [descriptions $s]]
  grid .help.t .help.s -sticky nsew
}

# console support - command line for tma
# 
# TODO history/reeval/multple line commands
#

namespace eval console {
  array set history {}
  variable nth 0

  if {$::options(-console_safe)} {
    set interp [::safe::interpCreate]
    foreach c [info commands ::commands::*] {
      $interp alias [namespace tail $c] $c
    }
  }

  proc new {} {
    set w .console
    frame $w
    text $w.t \
    	-height $::options(-console_lines) \
	-yscrollcommand "$w.s set"
    ttk::scrollbar $w.s -orient vertical -command "$w.t yview"

    bind $w.t <Return> "::console::handle_return"

    $w.t configure -foreground black
    $w.t tag configure stdout -foreground blue
    $w.t tag configure stdin -foreground black
    $w.t tag configure stderr -foreground red
    $w.t tag configure msg -foreground brown

    pack $w.t -fill x -expand 1 -side left
    pack $w.s -fill y -expand 0 -side left

    update
    return $w
  }

  proc handle_return {} {
    set w .console
    $w.t mark set insert end
    set cmd [list [$w.t get {insert linestart} {insert lineend}]]
    $w.t insert end "\n"
    gui::busy
    if {$::options(-console_safe) == 0} {
      if {[catch {uplevel #0 eval $cmd} result]} {
	insert $result stderr
      } else {
	insert $result stdout
      }
    } else {
      variable interp
      if {[catch {$interp eval {*}$cmd} result]} {
	insert $result stderr
      } else {
	insert $result stdout
      }
    }
    gui::unbusy
  }
  
  proc insert {s {tag msg}} {
    set w .console
    if {![winfo exists $w]} {
      return
    }
    $w.t insert end $s $tag
    $w.t mark set insert end
    $w.t see end
  } 
  
  proc source {fn} {
    variable interp
    return [$interp invokehidden source $fn]
  }
}

proc msg {s} {
  console::insert $s\n msg
  update idletasks
}

# gui - is the toplevel controls and gui

namespace eval gui {}

# PI tag selector gui 
proc gui::pi {w} {
  frame $w
  label $w.title -text "Read"

  # PI tags and Files
  ttk::combobox $w.tags -width 50 -height 20
  bind $w.tags <<ComboboxSelected>> "gui::tagupdate $w; gui:tagget $w.tags"
  bind $w.tags <Return> "gui::tagupdate $w; gui:tagget $w.tags"
  
  # Site selection
  label $w.sitet -text "Sites:" 
  ttk::combobox $w.sites -width 5
  bind $w.sites <<ComboboxSelected>> "gui::tagupdate $w"
  $w.sites set "*"

  # Unit selection
  label $w.unitt -text "Units:" 
  ttk::combobox $w.units -width 5
  bind $w.units <<ComboboxSelected>> "gui::tagupdate $w"
  $w.units set "*"

  # time selection
  set ::from [time::format [st]]
  label $w.lt -text "Period: "
  button $w.st -textvariable ::st_f -command "st \[date::choose \[st\]\]"
  label $w.dd -text ".."
  button $w.et -textvariable ::et_f -command "et \[date::choose \[et\]\]"

  # layout
  grid $w.title $w.tags \
      $w.sitet $w.sites \
      $w.unitt $w.units \
      $w.lt $w.st $w.dd $w.et

  gui::tagupdate $w

  return $w
}

proc gui::tagupdate {w} {
  set r {}
  set unit_pat [$w.units get]
  set site_pat [$w.sites get]
  foreach t [pi:tags] {
    if {[string match $unit_pat [pi:units $t]] &&
	[string match $site_pat [pi:site $t]]} {
      lappend r "$t - [pi:description $t]"
    }
  }
  $w.tags configure -values $r
  $w.sites configure -values [pi:all_sites]
  $w.units configure -values [pi:all_units]
  return 
}

proc gui:tagget {w} {
  set t [lindex [$w get] 0]
  get $t
}

proc gui::gui {w} {
  frame $w 
  pack [gui::pi $w.pi] -side left
  return $w
}

# menu support
proc m+ {head name {cmd ""}} {
  if {![winfo exists .m.m$head]} { 
    foreach {l u} [::tk::UnderlineAmpersand $head] break
    .m add cascade -label $l -underline $u -menu [menu .m.m$head -tearoff 0] 
  } 
  if {[regexp ^-+$ $name]} { 
    .m.m$head add separator 
  } elseif {[regexp {^\?(.+)} $name -> name]} {
    .m.m$head add checkbutton -label $name -variable $cmd 
  } else {
    foreach {l u} [::tk::UnderlineAmpersand $name] break
    .m.m$head add command -label $l -underline $u -comm $cmd
    m= $name $cmd
  }
}

# top level menu command/button
proc m* {name {cmd ""}} {
  foreach {l u} [::tk::UnderlineAmpersand $name] break
  .m add command -label $l -command $cmd -underline $u
  m= $name $cmd
}

# bind sequence to command in toplevel using a prefix 
# or Control or Alt. If already bound fatal error since
# modeful interfaces are evil.
proc m= {name cmd} {
  foreach {l u} [::tk::UnderlineAmpersand $name] break
  foreach p {Control Alt} {
    if {$cmd eq {}} {
      return
    } elseif {$u != -1} {
      set k  <$p-[string index $l $u]> 
      if {[bind . $k] eq ""} { 
	bind . $k $cmd
      } else {
	error "fatal: bind $k failed since its already bound to [bind . $k]"
      }
    }
  }
}
  
  

# top level menu 
. configure -menu [menu .m]
m+ &File &Open gui::open
m+ &File "&Save as" gui::save

m+ &File &Print gui::print
windows_only {
  m+ &File &Copy "plot::print_copy .w"
}
m+ &File "Refresh PI tags" "tags:clear; tags:getserver; tags:put pi.tags"
m+ &File -----
m+ &File E&xit gui::exit
m+ &File Restart {exec $argv0 {*}$argv &; exit 0}

m* Statistics "::commands::statistics *"
# e.g. m+ Options ?Verbose for a checkbutton

foreach p $::options(-periods) {
  if {$p eq "&Autorange"} {
    m* $p "plot::period .w {}"
  } else {
    set t [clock scan [string map {& "" m min h hour d day y year} $p] \
	       -base 0 -gmt 1]
    m* $p "plot::period .w $t"
  }
}

m+ &Help Overview {help::show overview "*Overview*"}
m+ &Help Options {help::show options "*Options*"} 
m+ &Help Commands {help::show commands "*Commands*"}
m+ &Help Keys {help::show keys "*Keys*"}
m+ &Help Version {help::show version "*Version*"} 
m+ &Help Copyright {help::show copyright "*Copyright*"}
m+ &Help Thanks {help::show thanks "*Thanks*"}

proc gui::open {} {
  commands::get
}

proc gui::save {} {
  set filename [tk_getSaveFile -defaultextension .csv]
  commands::put $filename * 
}

proc gui::print {} {
  plot::print .w
}

proc gui::exit {} {
  # ok to exit if we get excited
  ::exit 0
}

# busy widget
# busy widget that just stops user input during long activities.
# note: this could be changed to plain Tk rather than blt but it
# seems good enough for now since tk busy is only available in 8.6
proc gui::busy {} {
  blt::busy hold . -cursor watch
  update
}

proc gui::unbusy {} {
  blt::busy release .
}

# tkhtml renderer
#
# tkhtml.tcl from tkinspect redone as a simple HTML rendering package
#
# This software is copyright (C) 1995 by the Lawrence Berkeley Laboratory.
# 
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that: (1) source code distributions
# retain the above copyright notice and this paragraph in its entirety, (2)
# distributions including binary code include the above copyright notice and
# this paragraph in its entirety in the documentation or other materials
# provided with the distribution, and (3) all advertising materials mentioning
# features or use of this software display the following acknowledgement:
# ``This product includes software developed by the University of California,
# Lawrence Berkeley Laboratory and its contributors.'' Neither the name of
# the University nor the names of its contributors may be used to endorse
# or promote products derived from this software without specific prior
# written permission.
# 
# THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
# WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
# MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
#
# This code is based on Angel Li's (angel@flipper.rsmas.miami.edu) HTML
# rendering code.
#

namespace eval tkhtml {}

proc tkhtml::set_render_hook {hook} {
    variable priv
    set priv(render_hook) $hook
}

proc tkhtml::set_image_hook {hook} {
    variable priv
    set priv(image_hook) $hook
}

proc tkhtml::render {w html} {
    variable priv 
    variable entity

    $w config -state normal
    $w delete 1.0 end
    tkhtml::setup $w
    set priv(continue_rendering) 1
    tkhtml::set_tag
    while {$priv(continue_rendering)} {
	# normal state
	while {[set len [string length $html]]} {
	    # look for text up to the next <> element
	    if [regexp -indices "^\[^<\]+" $html match] {
		set text [string range $html 0 [lindex $match 1]]
		tkhtml::append_text $text
		set html \
		    [string range $html [expr [lindex $match 1]+1] end]
	    }
	    # we're either at a <>, or at the eot

	    if [regexp -indices "^<(\[^>\]+)>" $html match xentity] {
		set xentity [string range $html [lindex $xentity 0] \
			    [lindex $xentity 1]]
		# puts "xentity = $xentity"
		# set cmd [string tolower [lindex $xentity 0]]
		regsub -all {\n} $xentity { } xentity
		regexp {^([/a-zA-Z0-9]+)[ \t\n]*(.*)$} $xentity ignore_it cmd otherargs
		set cmd [string tolower $cmd]
		# puts "** cmd = '$cmd' otherargs='$otherargs'"
		# gets stdin
		if {[info exists entity($cmd)]} {
		    # do $cmd [lrange $xentity 1 end]
		    do $cmd $otherargs
		}
		set html \
		    [string range $html [expr [lindex $match 1]+1] end]
	    }
	    if [info exists priv(render_hook)] {
		eval $priv(render_hook) $len
	    }
	    if $priv(verbatim) break
	}
	# we reach here if html is empty, or verbatim is 1
	if !$len break
	# verbatim must be 1
	# append text until a </pre> is reached
	if {[regexp -indices -nocase $tkhtml::priv(verb_end_token) $html match]} {
	    set text [string range $html 0 [expr [lindex $match 0]-1]]
	    set html [string range $html [expr [lindex $match 1]+1] end]
	} else {
	    set text $html
	    set html ""
	}
	tkhtml::append_text $text
	if [info exists tkhtml::entity([string trim $tkhtml::priv(verb_end_token) <>])] {
	    tkhtml::do [string trim $tkhtml::priv(verb_end_token) <>]
	}
    }
    $w config -state disabled
}

proc tkhtml::defaults {} {
    variable priv
    set priv(defaults_set) 1
    set priv(default_font) times
    set priv(fixed_font) courier
    set priv(font_size) medium
    set priv(small_points) "60 80 100 120 140 180 240"
    set priv(medium_points) "80 100 120 140 180 240 360"
    set priv(large_points) "100 120 140 180 240 360 480"
    set priv(huge_points) "120 140 180 240 360 480 640"
    set priv(ruler_height) 6
    set priv(indent_incr) 4
    set priv(w) {}
    set priv(counter) -1
}

proc tkhtml::set_font {font size} {
    variable priv
    set priv(default_font) $font
    set priv(font) $font
    set priv(font_size) $size
}

proc tkhtml::setup {w} {
    if ![info exists ::tkhtml::priv(defaults_set)] defaults
    variable priv

    set priv(font) $priv(default_font)
    set priv(left) 1
    set priv(left2) 1
    set priv(right) 2
    set priv(justify) L
    set priv(weight) 0
    set priv(slant) 0
    set priv(underline) 0
    set priv(verbatim) 0
    set priv(pre) 0
    set priv(title) {}
    set priv(in_title) 0
    set priv(color) black
    set priv(li_style) bullet
    set priv(anchor_count) 0
    set priv(oln) 0
    set priv(verb_end_token) {}
    set priv(stack.font) {}
    set priv(stack.color) {}
    set priv(stack.justify) {}
    set priv(stack.li_style) {}
    set priv(stack.href) {}
    set priv(points_ndx) 2
    if {$priv(w) != $w} {
	set priv(w) $w
	$priv(w) tag config hr -relief sunken -borderwidth 2 \
	    -font -*-*-*-*-*-*-$priv(ruler_height)-*-*-*-*-*-*-*
	foreach elt [array names priv] {
	    if [regexp "^tag\\..*" $elt] {
		unset priv($elt)
	    }
	}
    }
}

proc tkhtml::define_font {name foundry family weight slant registry} {
    variable priv
    lappend priv(font_names) $name
    set priv(font_info.$name) \
	[list $foundry $family $weight $slant $registry]
}

proc tkhtml::define_entity {name body} {
    variable entity
    set entity($name) $body
}

proc tkhtml::do {cmd {argv {}}} {
    # puts "tkhtml::do $cmd $argv"
    variable entity
    eval $entity($cmd)
}

proc tkhtml::append_text {text} {
    variable priv
    if !$priv(verbatim) {
	if !$priv(pre) {
	    regsub -all "\[ \n\r\t\]+" [string trim $text] " " text
	}
	regsub -nocase -all "&amp;" $text {\&} text
	regsub -nocase -all "&lt;" $text "<" text
	regsub -nocase -all "&gt;" $text ">" text
	if ![string length $text] return
    }
    if {!$priv(pre) && !$priv(in_title)} {
	set p [$priv(w) get "end - 2c"]
	set n [string index $text 0]
	if {![regexp "\[ \n(\]" $p] && ![regexp "\[\\.,')\]" $n]} {
	    $priv(w) insert end " "
	}
	$priv(w) insert end $text $priv(tag)
	return
    }
    if {$priv(pre) && !$priv(in_title)} {
	$priv(w) insert end $text $priv(tag)
	return
    }
    append priv(title) $text
}

proc tkhtml::title {} {
    variable priv
    return $priv(title)
}

# a tag is constructed as: font?B?I?U?Points-LeftLeft2RightColorJustify
proc tkhtml::set_tag {} {
    variable priv
    variable href

    set i -1
    foreach var {foundry family weight slant registry} {
	set $var [lindex $priv(font_info.$priv(font)) [incr i]]
    }
    set x_font "-$foundry-$family-"
    set tag $priv(font)
    set args {}
    if {$priv(weight) > 0} {
	append tag "B"
	append x_font [lindex $weight 1]-
    } else {
	append x_font [lindex $weight 0]-
    }
    if {$priv(slant) > 0} {
	append tag "I"
	append x_font [lindex $slant 1]-
    } else {
	append x_font [lindex $slant 0]-
    }
    if {$priv(underline) > 0} {
	append tag "U"
	append args " -underline 1"
    }
    switch $priv(justify) {
	L { append args " -justify left" }
	R { append args " -justify right" }
	C { append args " -justify center" }
    }
    set pts [lindex $priv($priv(font_size)_points) \
	     $priv(points_ndx)]
    append tag $priv(points_ndx) - $priv(left) \
	$priv(left2) $priv(right) \
	$priv(color) $priv(justify)
    append x_font "normal-*-*-$pts-*-*-*-*-$registry-*"
    if $priv(anchor_count) {
	variable href
	set href [peek href]
	set href_tag href[incr priv(counter)]
	set tags [list $tag $href_tag]
	if [info exists priv(command)] {
	    $priv(w) tag bind $href_tag <1> \
		[list ::tkhtml::href_click $tkhtml::priv(command) $tkhtml::href]
	}
	$priv(w) tag bind $href_tag <Enter> \
	    [list $priv(w) tag configure $href_tag -foreground red]
	$priv(w) tag bind $href_tag <Leave> \
	    [list $priv(w) tag configure $href_tag \
	     -foreground $priv(color)]
    } else {
	set tags $tag
    }
    if {![info exists priv(tag.$tag)]} {
	set priv(tag_font.$tag) 1
	eval $priv(w) tag configure $tag \
	    -font $x_font -foreground $priv(color) \
	    -lmargin1 $priv(left)m \
	    -lmargin2 $priv(left2)m $args
    }
    if [info exists href_tag] {
	$priv(w) tag raise $href_tag $tag
    }
    set priv(tag) $tags
}

proc tkhtml::reconfig_tags {w} {
    variable priv
    foreach tag [$w tag names] {
	foreach font $priv(font_names) {
	    if [regexp "${font}(B?)(I?)(U?)(\[1-9\]\[0-9\]*)-" $tag t b i u points] {
		set j -1
		if {$font != $priv(fixed_font)} {
		    set font $priv(font)
		}
		foreach var {foundry family weight slant registry} {
		    set $var [lindex $priv(font_info.$font) [incr j]]
		}
		set x_font "-$foundry-$family-"
		if {$b == "B"} {
		    append x_font [lindex $weight 1]-
		} else {
		    append x_font [lindex $weight 0]-
		}
		if {$i == "I"} {
		    append x_font [lindex $slant 1]-
		} else {
		    append x_font [lindex $slant 0]-
		}
		set pts [lindex $priv($priv(font_size)_points) \
			 $points]
		append x_font "normal-*-*-$pts-*-*-*-*-$registry-*"
		$w tag config $tag -font $x_font
		break
	    }
	}
    }
}

proc tkhtml::push {stack value} {
    variable priv
    lappend priv(stack.$stack) $value
}

proc tkhtml::pop {stack} {
    variable priv
    set n [expr [llength $priv(stack.$stack)]-1]
    if {$n < 0} {
	puts "popping empty stack $stack"
	return ""
    }
    set val [lindex $priv(stack.$stack) $n]
    set priv(stack.$stack) [lreplace $priv(stack.$stack) $n $n]
    return $val
}

proc tkhtml::peek {stack} {
    variable priv
    return [lindex $priv(stack.$stack) end]
}

# TODO: review this code, in particular its handling of spaces
proc tkhtml::parse_fields {array_var string} {
    upvar $array_var array
    
    set pat {([^ \n\r=]+)="?([^"\n\r\t]*)"?}
    set dpat {[^ \n\r=]+="?[^"\n\r\t]+"?}

    # foreach arg $string 
    # puts "$string match $pat" ; 
    while {$string != ""} {
	# puts "processing: $string"
	if ![regexp $pat $string dummy field value] {
	    puts "malformed command field"
	    puts "field = \"$string\""
	    exit
	}
	set array([string tolower $field]) $value
	# puts "dpat = $dpat"
	# puts "string = $string"
	regsub $dpat $string {} string
	# puts "new string = $string"
    }
}

proc tkhtml::set_command {cmd} {
    variable priv
    set priv(command) $cmd
}

proc tkhtml::href_click {cmd href} {
    # puts "href::click $cmd $href"
    uplevel #0 [list $cmd $href]
}

# define the fonts we're going to use
set priv(font_names) ""
tkhtml::define_font helvetica adobe helvetica "medium bold" "r o" iso8859
tkhtml::define_font courier adobe courier "medium bold" "r o" iso8859
tkhtml::define_font times adobe times "medium bold" "r i" iso8859
tkhtml::define_font symbol adobe symbol "medium medium" "r r" adobe
command assert {e} {the expression must true, if not error} {
  if {![uplevel 1 [list expr $e]]} {
    error "assert $e failed"
  }
}
# define the entities we're going to handle
tkhtml::define_entity b { incr priv(weight); tkhtml::set_tag }
tkhtml::define_entity /b { incr priv(weight) -1; tkhtml::set_tag }
tkhtml::define_entity strong { incr priv(weight); tkhtml::set_tag }
tkhtml::define_entity /strong { incr priv(weight) -1; tkhtml::set_tag }
tkhtml::define_entity tt {
    variable priv
    tkhtml::push font $priv(font)
    set priv(font) $priv(fixed_font)
    tkhtml::set_tag
}
tkhtml::define_entity /tt {
    variable priv
    set priv(font) [tkhtml::pop font]
    tkhtml::set_tag
}
tkhtml::define_entity code { tkhtml::do tt }
tkhtml::define_entity /code { tkhtml::do /tt }
tkhtml::define_entity kbd { tkhtml::do tt }
tkhtml::define_entity /kbd { tkhtml::do /tt }
tkhtml::define_entity em { variable priv; incr priv(slant); tkhtml::set_tag }
tkhtml::define_entity /em { variable priv; incr priv(slant) -1; tkhtml::set_tag }
tkhtml::define_entity var { variable priv; incr priv(slant); tkhtml::set_tag }
tkhtml::define_entity /var { variable priv; incr priv(slant) -1; tkhtml::set_tag }
tkhtml::define_entity cite { variable priv; incr priv(slant); tkhtml::set_tag }
tkhtml::define_entity /cite { variable priv; incr priv(slant) -1; tkhtml::set_tag }
tkhtml::define_entity address {
    variable priv
    tkhtml::do br
    incr priv(slant)
    tkhtml::set_tag
}
tkhtml::define_entity /address {
    variable priv
    incr priv(slant) -1
    tkhtml::do br
    tkhtml::set_tag
}
tkhtml::define_entity /cite { 
    variable priv
    incr priv(slant) -1
    tkhtml::set_tag 
}

tkhtml::define_entity p {
    variable priv
    set x [$priv(w) get end-3c]
    set y [$priv(w) get end-2c]
    if {$x == "" && $y == ""} return
    if {$y == ""} {
	$priv(w) insert end "\n\n"
	return
    }
    if {$x == "\n" && $y == "\n"} return
    if {$y == "\n"} {
	$priv(w) insert end "\n"
	return
    }
    $priv(w) insert end "\n\n"
}
tkhtml::define_entity br {
    variable priv
    if {[$priv(w) get "end-2c"] != "\n"} {
	$priv(w) insert end "\n"
    }
}
tkhtml::define_entity title { variable priv; set priv(in_title) 1 }
tkhtml::define_entity /title { variable priv; set priv(in_title) 0 }

tkhtml::define_entity h1 { tkhtml::header 1 }
tkhtml::define_entity /h1 { tkhtml::/header 1 }
tkhtml::define_entity h2 { tkhtml::header 2 }
tkhtml::define_entity /h2 { tkhtml::/header 2 }
tkhtml::define_entity h3 { tkhtml::header 3 }
tkhtml::define_entity /h3 { tkhtml::/header 3 }
tkhtml::define_entity h4 { tkhtml::header 4 }
tkhtml::define_entity /h4 { tkhtml::/header 4 }
tkhtml::define_entity h5 { tkhtml::header 5 }
tkhtml::define_entity /h5 { tkhtml::/header 5 }
tkhtml::define_entity h6 { tkhtml::header 6 }
tkhtml::define_entity /h6 { tkhtml::/header 6 }

proc tkhtml::header {level} {
    variable priv
    tkhtml::do p
    set priv(points_ndx) [expr 6-$level]
    incr priv(weight)
    tkhtml::set_tag
}

proc tkhtml::/header {level} {
    variable priv
    set priv(points_ndx) 2
    incr priv(weight) -1
    tkhtml::set_tag
    tkhtml::do p
}

tkhtml::define_entity pre { 
    variable priv
    tkhtml::do tt
    tkhtml::do br
    incr priv(pre)
}
tkhtml::define_entity /pre {
    variable priv
    tkhtml::do /tt
    set priv(pre) 0
    tkhtml::do p
}

tkhtml::define_entity hr {
    variable priv
    tkhtml::do p
    $priv(w) insert end "\n" hr
}
tkhtml::define_entity a {
    variable priv
    tkhtml::parse_fields ar $argv
    tkhtml::push color $priv(color)

    if [info exists ar(href)] {
	tkhtml::push href $ar(href)
    } else {
	tkhtml::push href {}
    }
    incr priv(anchor_count)
    set priv(color) blue
    incr priv(underline)
    tkhtml::set_tag
}
tkhtml::define_entity /a {
    variable priv
    tkhtml::pop href
    incr priv(anchor_count) -1
    set priv(color) [tkhtml::pop color]
    incr priv(underline) -1
    tkhtml::set_tag
}
tkhtml::define_entity center {
    variable priv
    tkhtml::push justify $priv(justify)
    set priv(justify) C
    tkhtml::set_tag
}
tkhtml::define_entity /center {
    variable priv
    set priv(justify) [tkhtml::pop justify]
    tkhtml::set_tag
}
tkhtml::define_entity ul {
    variable priv
    if $priv(left) {
	tkhtml::do br
    } else {
	tkhtml::do p
    }
    incr priv(left) $priv(indent_incr)
    incr priv(left2) [expr $priv(indent_incr)+3]
    tkhtml::push li_style $priv(li_style)
    set priv(li_style) bullet
    tkhtml::set_tag
}
tkhtml::define_entity /ul {
    variable priv
    incr priv(left) -$priv(indent_incr)
    incr priv(left2) -[expr $priv(indent_incr)+3]
    set priv(li_style) [tkhtml::pop li_style]
    tkhtml::set_tag
    tkhtml::do p
}
tkhtml::define_entity ol {
    variable priv
    if $priv(left) {
	tkhtml::do br
    } else {
	tkhtml::do p
    }
    incr priv(left) $priv(indent_incr)
    incr priv(left2) [expr $priv(indent_incr)+3]
    tkhtml::push li_style $priv(li_style)
    tkhtml::push oln $priv(oln) ;# xx
    set priv(oln) 1
    set priv(li_style) number
    tkhtml::set_tag
}
tkhtml::define_entity /ol {
    variable priv
    incr priv(left) -$priv(indent_incr)
    incr priv(left2) -[expr $priv(indent_incr)+3]
    set priv(oln) [tkhtml::pop oln]
    set priv(li_style) [tkhtml::pop li_style]
    tkhtml::set_tag
    tkhtml::do p
}
tkhtml::define_entity li {
    variable priv
    tkhtml::do br
    if {$priv(li_style) == "bullet"} {
	set old_font $priv(font)
	set priv(font) symbol
	tkhtml::set_tag
	$priv(w) insert end "\xb7" $priv(tag)
	set priv(font) $old_font
	tkhtml::set_tag
    } elseif {$priv(li_style) == "number"} {
	set old_font $priv(font)
	set priv(font) symbol
	tkhtml::set_tag
	$priv(w) insert end "[tkhtml::format_oln $priv(oln) [llength $priv(stack.oln)]]" $priv(tag)
	incr priv(oln)
	set priv(font) $old_font
	tkhtml::set_tag
    }
}

proc tkhtml::format_oln {n depth} {
	return "$n." ;# plain numbers for now
}

tkhtml::define_entity listing { tkhtml::do pre }
tkhtml::define_entity /listing { tkhtml::do /pre }
tkhtml::define_entity code { tkhtml::do pre }
tkhtml::define_entity /code { tkhtml::do /pre }

tkhtml::define_entity img {
    variable priv
    tkhtml::parse_fields ar $argv
    if [info exists ar(src)] {
	set file $ar(src)
	if [info exists priv(image_hook)] {
	    set img [eval $tkhmlt_priv(image_hook) $file]
	} else {
	    if [catch {set img [image create photo -file $file]} err] {
		puts stderr "Couldn't create image $file: $err"
		return
	    }
	}
	set align bottom
	if [info exists ar(align)] {
	    set align [string tolower $ar(align)]
	}
	label $priv(w).$img -image $img
	$priv(w) window create end -window $priv(w).$img \
	    -align $align
    }
}


namespace eval date {
  option add *Button.padX 0
  option add *Button.padY 0
  proc choose {t} {
    variable month; variable year; variable date
    variable canvas; variable res
    variable day
    set year [clock format $t -format "%Y"]
    scan [clock format $t -format "%m"] %d month
    scan [clock format $t -format "%d"] %d day
    toplevel .chooseDate -bg white
    wm title .chooseDate "Choose Date:"
    frame .chooseDate.1
    entry .chooseDate.1.1 -textvar date::month -width 3 -just center
    button .chooseDate.1.2 -text + -command {date::adjust 1 0}
    button .chooseDate.1.3 -text - -command {date::adjust -1 0}
    entry .chooseDate.1.4 -textvar date::year -width 4 -just center
    button .chooseDate.1.5 -text + -command {date::adjust 0 1}
    button .chooseDate.1.6 -text - -command {date::adjust 0 -1}
    eval pack [winfo children .chooseDate.1] -side left \
	-fill both
    set canvas [canvas .chooseDate.2 -width 160 -height 160 -bg white]
    frame .chooseDate.3
    entry .chooseDate.3.1 -textvar date::date -width 10
    button .chooseDate.3.2 -text OK -command {set date::res $date::date}
    button .chooseDate.3.3 -text Cancel -command {set date::res {}}
    eval pack [winfo children .chooseDate.3] -side left
    eval pack [winfo children .chooseDate]
    display
    vwait ::date::res
    destroy .chooseDate
    set res
  }
  proc adjust {dmonth dyear} {
    variable month; variable year; variable day
    set year  [expr {$year+$dyear}]
    set month [expr {$month+$dmonth}]
    if {$month>12} {set month 1; incr year}
    if {$month<1} {set month 12; incr year -1}
    if {[numberofdays $month $year]<$day} {
      set day [numberofdays $month $year]
    }
    display
  }
  proc display {} {
    variable month; variable year
    variable date; variable day
    variable canvas
    $canvas delete all
    set x0 20; set x $x0; set y 20
    set dx 20; set dy 20
    set xmax [expr {$x0+$dx*6}]
    foreach i {S M T W T F S} {
      $canvas create text $x $y -text $i -fill blue
      incr x $dx
    }
    scan [clock format [clock scan $month/1/$year] \
	      -format %w] %d weekday
    set x [expr {$x0+$weekday*$dx}]
    incr y $dy
    set nmax [numberofdays $month $year]
    for {set d 1} {$d<=$nmax} {incr d} {
      set id [$canvas create text $x $y -text $d -tag day]
      if {$d==$day} {$canvas itemconfig $id -fill red}
      incr x $dx
      if {$x>$xmax} {set x $x0; incr y $dy}
    }
    $canvas bind day <1> {
      set item [%W find withtag current]
      set date::day [%W itemcget $item -text]
      set date::date "$date::day/$date::month/$date::year"
      %W itemconfig day -fill black
      %W itemconfig $item -fill red
    }
    set date "$day/$month/$year"
  }
  proc numberofdays {month year} {
    if {$month==12} {set month 1; incr year}
    clock format [clock scan "[incr month]/1/$year  1 day ago"] \
	-format %d
  }
} ;# end namespace date

# Copyright GPL-3

describe copyright <pre>[string map {< &lt; > &gt;} {
                    GNU GENERAL PUBLIC LICENSE
                       Version 3, 29 June 2007

 Copyright (C) 2007 Free Software Foundation, Inc. <http://fsf.org/>
 Everyone is permitted to copy and distribute verbatim copies
 of this license document, but changing it is not allowed.

                            Preamble

The GNU General Public License is a free, copyleft license for
software and other kinds of works.

The licenses for most software and other practical works are designed
to take away your freedom to share and change the works.  By contrast,
the GNU General Public License is intended to guarantee your freedom to
share and change all versions of a program--to make sure it remains free
software for all its users.  We, the Free Software Foundation, use the
GNU General Public License for most of our software; it applies also to
any other work released this way by its authors.  You can apply it to
your programs, too.

When we speak of free software, we are referring to freedom, not
price.  Our General Public Licenses are designed to make sure that you
have the freedom to distribute copies of free software (and charge for
them if you wish), that you receive source code or can get it if you
want it, that you can change the software or use pieces of it in new
free programs, and that you know you can do these things.

To protect your rights, we need to prevent others from denying you
these rights or asking you to surrender the rights.  Therefore, you have
certain responsibilities if you distribute copies of the software, or if
you modify it: responsibilities to respect the freedom of others.

For example, if you distribute copies of such a program, whether
gratis or for a fee, you must pass on to the recipients the same
freedoms that you received.  You must make sure that they, too, receive
or can get the source code.  And you must show them these terms so they
know their rights.

Developers that use the GNU GPL protect your rights with two steps:
(1) assert copyright on the software, and (2) offer you this License
giving you legal permission to copy, distribute and/or modify it.

For the developers' and authors' protection, the GPL clearly explains
that there is no warranty for this free software.  For both users' and
authors' sake, the GPL requires that modified versions be marked as
changed, so that their problems will not be attributed erroneously to
authors of previous versions.

Some devices are designed to deny users access to install or run
modified versions of the software inside them, although the manufacturer
can do so.  This is fundamentally incompatible with the aim of
protecting users' freedom to change the software.  The systematic
pattern of such abuse occurs in the area of products for individuals to
use, which is precisely where it is most unacceptable.  Therefore, we
have designed this version of the GPL to prohibit the practice for those
products.  If such problems arise substantially in other domains, we
stand ready to extend this provision to those domains in future versions
of the GPL, as needed to protect the freedom of users.

Finally, every program is threatened constantly by software patents.
States should not allow patents to restrict development and use of
software on general-purpose computers, but in those that do, we wish to
avoid the special danger that patents applied to a free program could
make it effectively proprietary.  To prevent this, the GPL assures that
patents cannot be used to render the program non-free.

The precise terms and conditions for copying, distribution and
modification follow.

                       TERMS AND CONDITIONS

  0. Definitions.

  "This License" refers to version 3 of the GNU General Public License.

  "Copyright" also means copyright-like laws that apply to other kinds of
works, such as semiconductor masks.

  "The Program" refers to any copyrightable work licensed under this
License.  Each licensee is addressed as "you".  "Licensees" and
"recipients" may be individuals or organizations.

  To "modify" a work means to copy from or adapt all or part of the work
in a fashion requiring copyright permission, other than the making of an
exact copy.  The resulting work is called a "modified version" of the
earlier work or a work "based on" the earlier work.

  A "covered work" means either the unmodified Program or a work based
on the Program.

  To "propagate" a work means to do anything with it that, without
permission, would make you directly or secondarily liable for
infringement under applicable copyright law, except executing it on a
computer or modifying a private copy.  Propagation includes copying,
distribution (with or without modification), making available to the
public, and in some countries other activities as well.

  To "convey" a work means any kind of propagation that enables other
parties to make or receive copies.  Mere interaction with a user through
a computer network, with no transfer of a copy, is not conveying.

  An interactive user interface displays "Appropriate Legal Notices"
to the extent that it includes a convenient and prominently visible
feature that (1) displays an appropriate copyright notice, and (2)
tells the user that there is no warranty for the work (except to the
extent that warranties are provided), that licensees may convey the
work under this License, and how to view a copy of this License.  If
the interface presents a list of user commands or options, such as a
menu, a prominent item in the list meets this criterion.

  1. Source Code.

  The "source code" for a work means the preferred form of the work
for making modifications to it.  "Object code" means any non-source
form of a work.

  A "Standard Interface" means an interface that either is an official
standard defined by a recognized standards body, or, in the case of
interfaces specified for a particular programming language, one that
is widely used among developers working in that language.

  The "System Libraries" of an executable work include anything, other
than the work as a whole, that (a) is included in the normal form of
packaging a Major Component, but which is not part of that Major
Component, and (b) serves only to enable use of the work with that
Major Component, or to implement a Standard Interface for which an
implementation is available to the public in source code form.  A
"Major Component", in this context, means a major essential component
(kernel, window system, and so on) of the specific operating system
(if any) on which the executable work runs, or a compiler used to
produce the work, or an object code interpreter used to run it.

  The "Corresponding Source" for a work in object code form means all
the source code needed to generate, install, and (for an executable
work) run the object code and to modify the work, including scripts to
control those activities.  However, it does not include the work's
System Libraries, or general-purpose tools or generally available free
programs which are used unmodified in performing those activities but
which are not part of the work.  For example, Corresponding Source
includes interface definition files associated with source files for
the work, and the source code for shared libraries and dynamically
linked subprograms that the work is specifically designed to require,
such as by intimate data communication or control flow between those
subprograms and other parts of the work.

  The Corresponding Source need not include anything that users
can regenerate automatically from other parts of the Corresponding
Source.

  The Corresponding Source for a work in source code form is that
same work.

  2. Basic Permissions.

  All rights granted under this License are granted for the term of
copyright on the Program, and are irrevocable provided the stated
conditions are met.  This License explicitly affirms your unlimited
permission to run the unmodified Program.  The output from running a
covered work is covered by this License only if the output, given its
content, constitutes a covered work.  This License acknowledges your
rights of fair use or other equivalent, as provided by copyright law.

  You may make, run and propagate covered works that you do not
convey, without conditions so long as your license otherwise remains
in force.  You may convey covered works to others for the sole purpose
of having them make modifications exclusively for you, or provide you
with facilities for running those works, provided that you comply with
the terms of this License in conveying all material for which you do
not control copyright.  Those thus making or running the covered works
for you must do so exclusively on your behalf, under your direction
and control, on terms that prohibit them from making any copies of
your copyrighted material outside their relationship with you.

  Conveying under any other circumstances is permitted solely under
the conditions stated below.  Sublicensing is not allowed; section 10
makes it unnecessary.

  3. Protecting Users' Legal Rights From Anti-Circumvention Law.

  No covered work shall be deemed part of an effective technological
measure under any applicable law fulfilling obligations under article
11 of the WIPO copyright treaty adopted on 20 December 1996, or
similar laws prohibiting or restricting circumvention of such
measures.

  When you convey a covered work, you waive any legal power to forbid
circumvention of technological measures to the extent such circumvention
is effected by exercising rights under this License with respect to
the covered work, and you disclaim any intention to limit operation or
modification of the work as a means of enforcing, against the work's
users, your or third parties' legal rights to forbid circumvention of
technological measures.

  4. Conveying Verbatim Copies.

  You may convey verbatim copies of the Program's source code as you
receive it, in any medium, provided that you conspicuously and
appropriately publish on each copy an appropriate copyright notice;
keep intact all notices stating that this License and any
non-permissive terms added in accord with section 7 apply to the code;
keep intact all notices of the absence of any warranty; and give all
recipients a copy of this License along with the Program.

  You may charge any price or no price for each copy that you convey,
and you may offer support or warranty protection for a fee.

  5. Conveying Modified Source Versions.

  You may convey a work based on the Program, or the modifications to
produce it from the Program, in the form of source code under the
terms of section 4, provided that you also meet all of these conditions:

    a) The work must carry prominent notices stating that you modified
    it, and giving a relevant date.

    b) The work must carry prominent notices stating that it is
    released under this License and any conditions added under section
    7.  This requirement modifies the requirement in section 4 to
    "keep intact all notices".

    c) You must license the entire work, as a whole, under this
    License to anyone who comes into possession of a copy.  This
    License will therefore apply, along with any applicable section 7
    additional terms, to the whole of the work, and all its parts,
    regardless of how they are packaged.  This License gives no
    permission to license the work in any other way, but it does not
    invalidate such permission if you have separately received it.

    d) If the work has interactive user interfaces, each must display
    Appropriate Legal Notices; however, if the Program has interactive
    interfaces that do not display Appropriate Legal Notices, your
    work need not make them do so.

  A compilation of a covered work with other separate and independent
works, which are not by their nature extensions of the covered work,
and which are not combined with it such as to form a larger program,
in or on a volume of a storage or distribution medium, is called an
"aggregate" if the compilation and its resulting copyright are not
used to limit the access or legal rights of the compilation's users
beyond what the individual works permit.  Inclusion of a covered work
in an aggregate does not cause this License to apply to the other
parts of the aggregate.

  6. Conveying Non-Source Forms.

  You may convey a covered work in object code form under the terms
of sections 4 and 5, provided that you also convey the
machine-readable Corresponding Source under the terms of this License,
in one of these ways:

    a) Convey the object code in, or embodied in, a physical product
    (including a physical distribution medium), accompanied by the
    Corresponding Source fixed on a durable physical medium
    customarily used for software interchange.

    b) Convey the object code in, or embodied in, a physical product
    (including a physical distribution medium), accompanied by a
    written offer, valid for at least three years and valid for as
    long as you offer spare parts or customer support for that product
    model, to give anyone who possesses the object code either (1) a
    copy of the Corresponding Source for all the software in the
    product that is covered by this License, on a durable physical
    medium customarily used for software interchange, for a price no
    more than your reasonable cost of physically performing this
    conveying of source, or (2) access to copy the
    Corresponding Source from a network server at no charge.

    c) Convey individual copies of the object code with a copy of the
    written offer to provide the Corresponding Source.  This
    alternative is allowed only occasionally and noncommercially, and
    only if you received the object code with such an offer, in accord
    with subsection 6b.

    d) Convey the object code by offering access from a designated
    place (gratis or for a charge), and offer equivalent access to the
    Corresponding Source in the same way through the same place at no
    further charge.  You need not require recipients to copy the
    Corresponding Source along with the object code.  If the place to
    copy the object code is a network server, the Corresponding Source
    may be on a different server (operated by you or a third party)
    that supports equivalent copying facilities, provided you maintain
    clear directions next to the object code saying where to find the
    Corresponding Source.  Regardless of what server hosts the
    Corresponding Source, you remain obligated to ensure that it is
    available for as long as needed to satisfy these requirements.

    e) Convey the object code using peer-to-peer transmission, provided
    you inform other peers where the object code and Corresponding
    Source of the work are being offered to the general public at no
    charge under subsection 6d.

  A separable portion of the object code, whose source code is excluded
from the Corresponding Source as a System Library, need not be
included in conveying the object code work.

  A "User Product" is either (1) a "consumer product", which means any
tangible personal property which is normally used for personal, family,
or household purposes, or (2) anything designed or sold for incorporation
into a dwelling.  In determining whether a product is a consumer product,
doubtful cases shall be resolved in favor of coverage.  For a particular
product received by a particular user, "normally used" refers to a
typical or common use of that class of product, regardless of the status
of the particular user or of the way in which the particular user
actually uses, or expects or is expected to use, the product.  A product
is a consumer product regardless of whether the product has substantial
commercial, industrial or non-consumer uses, unless such uses represent
the only significant mode of use of the product.

  "Installation Information" for a User Product means any methods,
procedures, authorization keys, or other information required to install
and execute modified versions of a covered work in that User Product from
a modified version of its Corresponding Source.  The information must
suffice to ensure that the continued functioning of the modified object
code is in no case prevented or interfered with solely because
modification has been made.

  If you convey an object code work under this section in, or with, or
specifically for use in, a User Product, and the conveying occurs as
part of a transaction in which the right of possession and use of the
User Product is transferred to the recipient in perpetuity or for a
fixed term (regardless of how the transaction is characterized), the
Corresponding Source conveyed under this section must be accompanied
by the Installation Information.  But this requirement does not apply
if neither you nor any third party retains the ability to install
modified object code on the User Product (for example, the work has
been installed in ROM).

  The requirement to provide Installation Information does not include a
requirement to continue to provide support service, warranty, or updates
for a work that has been modified or installed by the recipient, or for
the User Product in which it has been modified or installed.  Access to a
network may be denied when the modification itself materially and
adversely affects the operation of the network or violates the rules and
protocols for communication across the network.

  Corresponding Source conveyed, and Installation Information provided,
in accord with this section must be in a format that is publicly
documented (and with an implementation available to the public in
source code form), and must require no special password or key for
unpacking, reading or copying.

  7. Additional Terms.

  "Additional permissions" are terms that supplement the terms of this
License by making exceptions from one or more of its conditions.
Additional permissions that are applicable to the entire Program shall
be treated as though they were included in this License, to the extent
that they are valid under applicable law.  If additional permissions
apply only to part of the Program, that part may be used separately
under those permissions, but the entire Program remains governed by
this License without regard to the additional permissions.

  When you convey a copy of a covered work, you may at your option
remove any additional permissions from that copy, or from any part of
it.  (Additional permissions may be written to require their own
removal in certain cases when you modify the work.)  You may place
additional permissions on material, added by you to a covered work,
for which you have or can give appropriate copyright permission.

  Notwithstanding any other provision of this License, for material you
add to a covered work, you may (if authorized by the copyright holders of
that material) supplement the terms of this License with terms:

    a) Disclaiming warranty or limiting liability differently from the
    terms of sections 15 and 16 of this License; or

    b) Requiring preservation of specified reasonable legal notices or
    author attributions in that material or in the Appropriate Legal
    Notices displayed by works containing it; or

    c) Prohibiting misrepresentation of the origin of that material, or
    requiring that modified versions of such material be marked in
    reasonable ways as different from the original version; or

    d) Limiting the use for publicity purposes of names of licensors or
    authors of the material; or

    e) Declining to grant rights under trademark law for use of some
    trade names, trademarks, or service marks; or

    f) Requiring indemnification of licensors and authors of that
    material by anyone who conveys the material (or modified versions of
    it) with contractual assumptions of liability to the recipient, for
    any liability that these contractual assumptions directly impose on
    those licensors and authors.

  All other non-permissive additional terms are considered "further
restrictions" within the meaning of section 10.  If the Program as you
received it, or any part of it, contains a notice stating that it is
governed by this License along with a term that is a further
restriction, you may remove that term.  If a license document contains
a further restriction but permits relicensing or conveying under this
License, you may add to a covered work material governed by the terms
of that license document, provided that the further restriction does
not survive such relicensing or conveying.

  If you add terms to a covered work in accord with this section, you
must place, in the relevant source files, a statement of the
additional terms that apply to those files, or a notice indicating
where to find the applicable terms.

  Additional terms, permissive or non-permissive, may be stated in the
form of a separately written license, or stated as exceptions;
the above requirements apply either way.

  8. Termination.

  You may not propagate or modify a covered work except as expressly
provided under this License.  Any attempt otherwise to propagate or
modify it is void, and will automatically terminate your rights under
this License (including any patent licenses granted under the third
paragraph of section 11).

  However, if you cease all violation of this License, then your
license from a particular copyright holder is reinstated (a)
provisionally, unless and until the copyright holder explicitly and
finally terminates your license, and (b) permanently, if the copyright
holder fails to notify you of the violation by some reasonable means
prior to 60 days after the cessation.

  Moreover, your license from a particular copyright holder is
reinstated permanently if the copyright holder notifies you of the
violation by some reasonable means, this is the first time you have
received notice of violation of this License (for any work) from that
copyright holder, and you cure the violation prior to 30 days after
your receipt of the notice.

  Termination of your rights under this section does not terminate the
licenses of parties who have received copies or rights from you under
this License.  If your rights have been terminated and not permanently
reinstated, you do not qualify to receive new licenses for the same
material under section 10.

  9. Acceptance Not Required for Having Copies.

  You are not required to accept this License in order to receive or
run a copy of the Program.  Ancillary propagation of a covered work
occurring solely as a consequence of using peer-to-peer transmission
to receive a copy likewise does not require acceptance.  However,
nothing other than this License grants you permission to propagate or
modify any covered work.  These actions infringe copyright if you do
not accept this License.  Therefore, by modifying or propagating a
covered work, you indicate your acceptance of this License to do so.

  10. Automatic Licensing of Downstream Recipients.

  Each time you convey a covered work, the recipient automatically
receives a license from the original licensors, to run, modify and
propagate that work, subject to this License.  You are not responsible
for enforcing compliance by third parties with this License.

  An "entity transaction" is a transaction transferring control of an
organization, or substantially all assets of one, or subdividing an
organization, or merging organizations.  If propagation of a covered
work results from an entity transaction, each party to that
transaction who receives a copy of the work also receives whatever
licenses to the work the party's predecessor in interest had or could
give under the previous paragraph, plus a right to possession of the
Corresponding Source of the work from the predecessor in interest, if
the predecessor has it or can get it with reasonable efforts.

  You may not impose any further restrictions on the exercise of the
rights granted or affirmed under this License.  For example, you may
not impose a license fee, royalty, or other charge for exercise of
rights granted under this License, and you may not initiate litigation
(including a cross-claim or counterclaim in a lawsuit) alleging that
any patent claim is infringed by making, using, selling, offering for
sale, or importing the Program or any portion of it.

  11. Patents.

  A "contributor" is a copyright holder who authorizes use under this
License of the Program or a work on which the Program is based.  The
work thus licensed is called the contributor's "contributor version".

  A contributor's "essential patent claims" are all patent claims
owned or controlled by the contributor, whether already acquired or
hereafter acquired, that would be infringed by some manner, permitted
by this License, of making, using, or selling its contributor version,
but do not include claims that would be infringed only as a
consequence of further modification of the contributor version.  For
purposes of this definition, "control" includes the right to grant
patent sublicenses in a manner consistent with the requirements of
this License.

  Each contributor grants you a non-exclusive, worldwide, royalty-free
patent license under the contributor's essential patent claims, to
make, use, sell, offer for sale, import and otherwise run, modify and
propagate the contents of its contributor version.

  In the following three paragraphs, a "patent license" is any express
agreement or commitment, however denominated, not to enforce a patent
(such as an express permission to practice a patent or covenant not to
sue for patent infringement).  To "grant" such a patent license to a
party means to make such an agreement or commitment not to enforce a
patent against the party.

  If you convey a covered work, knowingly relying on a patent license,
and the Corresponding Source of the work is not available for anyone
to copy, free of charge and under the terms of this License, through a
publicly available network server or other readily accessible means,
then you must either (1) cause the Corresponding Source to be so
available, or (2) arrange to deprive yourself of the benefit of the
patent license for this particular work, or (3) arrange, in a manner
consistent with the requirements of this License, to extend the patent
license to downstream recipients.  "Knowingly relying" means you have
actual knowledge that, but for the patent license, your conveying the
covered work in a country, or your recipient's use of the covered work
in a country, would infringe one or more identifiable patents in that
country that you have reason to believe are valid.

  If, pursuant to or in connection with a single transaction or
arrangement, you convey, or propagate by procuring conveyance of, a
covered work, and grant a patent license to some of the parties
receiving the covered work authorizing them to use, propagate, modify
or convey a specific copy of the covered work, then the patent license
you grant is automatically extended to all recipients of the covered
work and works based on it.

  A patent license is "discriminatory" if it does not include within
the scope of its coverage, prohibits the exercise of, or is
conditioned on the non-exercise of one or more of the rights that are
specifically granted under this License.  You may not convey a covered
work if you are a party to an arrangement with a third party that is
in the business of distributing software, under which you make payment
to the third party based on the extent of your activity of conveying
the work, and under which the third party grants, to any of the
parties who would receive the covered work from you, a discriminatory
patent license (a) in connection with copies of the covered work
conveyed by you (or copies made from those copies), or (b) primarily
for and in connection with specific products or compilations that
contain the covered work, unless you entered into that arrangement,
or that patent license was granted, prior to 28 March 2007.

  Nothing in this License shall be construed as excluding or limiting
any implied license or other defenses to infringement that may
otherwise be available to you under applicable patent law.

  12. No Surrender of Others' Freedom.

  If conditions are imposed on you (whether by court order, agreement or
otherwise) that contradict the conditions of this License, they do not
excuse you from the conditions of this License.  If you cannot convey a
covered work so as to satisfy simultaneously your obligations under this
License and any other pertinent obligations, then as a consequence you may
not convey it at all.  For example, if you agree to terms that obligate you
to collect a royalty for further conveying from those to whom you convey
the Program, the only way you could satisfy both those terms and this
License would be to refrain entirely from conveying the Program.

  13. Use with the GNU Affero General Public License.

  Notwithstanding any other provision of this License, you have
permission to link or combine any covered work with a work licensed
under version 3 of the GNU Affero General Public License into a single
combined work, and to convey the resulting work.  The terms of this
License will continue to apply to the part which is the covered work,
but the special requirements of the GNU Affero General Public License,
section 13, concerning interaction through a network will apply to the
combination as such.

  14. Revised Versions of this License.

  The Free Software Foundation may publish revised and/or new versions of
the GNU General Public License from time to time.  Such new versions will
be similar in spirit to the present version, but may differ in detail to
address new problems or concerns.

  Each version is given a distinguishing version number.  If the
Program specifies that a certain numbered version of the GNU General
Public License "or any later version" applies to it, you have the
option of following the terms and conditions either of that numbered
version or of any later version published by the Free Software
Foundation.  If the Program does not specify a version number of the
GNU General Public License, you may choose any version ever published
by the Free Software Foundation.

  If the Program specifies that a proxy can decide which future
versions of the GNU General Public License can be used, that proxy's
public statement of acceptance of a version permanently authorizes you
to choose that version for the Program.

  Later license versions may give you additional or different
permissions.  However, no additional obligations are imposed on any
author or copyright holder as a result of your choosing to follow a
later version.

  15. Disclaimer of Warranty.

  THERE IS NO WARRANTY FOR THE PROGRAM, TO THE EXTENT PERMITTED BY
APPLICABLE LAW.  EXCEPT WHEN OTHERWISE STATED IN WRITING THE COPYRIGHT
HOLDERS AND/OR OTHER PARTIES PROVIDE THE PROGRAM "AS IS" WITHOUT WARRANTY
OF ANY KIND, EITHER EXPRESSED OR IMPLIED, INCLUDING, BUT NOT LIMITED TO,
THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
PURPOSE.  THE ENTIRE RISK AS TO THE QUALITY AND PERFORMANCE OF THE PROGRAM
IS WITH YOU.  SHOULD THE PROGRAM PROVE DEFECTIVE, YOU ASSUME THE COST OF
ALL NECESSARY SERVICING, REPAIR OR CORRECTION.

  16. Limitation of Liability.

  IN NO EVENT UNLESS REQUIRED BY APPLICABLE LAW OR AGREED TO IN WRITING
WILL ANY COPYRIGHT HOLDER, OR ANY OTHER PARTY WHO MODIFIES AND/OR CONVEYS
THE PROGRAM AS PERMITTED ABOVE, BE LIABLE TO YOU FOR DAMAGES, INCLUDING ANY
GENERAL, SPECIAL, INCIDENTAL OR CONSEQUENTIAL DAMAGES ARISING OUT OF THE
USE OR INABILITY TO USE THE PROGRAM (INCLUDING BUT NOT LIMITED TO LOSS OF
DATA OR DATA BEING RENDERED INACCURATE OR LOSSES SUSTAINED BY YOU OR THIRD
PARTIES OR A FAILURE OF THE PROGRAM TO OPERATE WITH ANY OTHER PROGRAMS),
EVEN IF SUCH HOLDER OR OTHER PARTY HAS BEEN ADVISED OF THE POSSIBILITY OF
SUCH DAMAGES.

  17. Interpretation of Sections 15 and 16.

  If the disclaimer of warranty and limitation of liability provided
above cannot be given local legal effect according to their terms,
reviewing courts shall apply local law that most closely approximates
an absolute waiver of all civil liability in connection with the
Program, unless a warranty or assumption of liability accompanies a
copy of the Program in return for a fee.

                     END OF TERMS AND CONDITIONS

            How to Apply These Terms to Your New Programs

  If you develop a new program, and you want it to be of the greatest
possible use to the public, the best way to achieve this is to make it
free software which everyone can redistribute and change under these terms.

  To do so, attach the following notices to the program.  It is safest
to attach them to the start of each source file to most effectively
state the exclusion of warranty; and each file should have at least
the "copyright" line and a pointer to where the full notice is found.

    <one line to give the program's name and a brief idea of what it does.>
    Copyright (C) <year>  <name of author>

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.

Also add information on how to contact you by electronic and paper mail.

  If the program does terminal interaction, make it output a short
notice like this when it starts in an interactive mode:

    <program>  Copyright (C) <year>  <name of author>
    This program comes with ABSOLUTELY NO WARRANTY; for details type `show w'.
    This is free software, and you are welcome to redistribute it
    under certain conditions; type `show c' for details.

The hypothetical commands `show w' and `show c' should show the appropriate
parts of the General Public License.  Of course, your program's commands
might be different; for a GUI interface, you would use an "about box".

  You should also get your employer (if you work as a programmer) or school,
if any, to sign a "copyright disclaimer" for the program, if necessary.
For more information on this, and how to apply and follow the GNU GPL, see
<http://www.gnu.org/licenses/>.

  The GNU General Public License does not permit incorporating your program
into proprietary programs.  If your program is a subroutine library, you
may consider it more useful to permit linking proprietary applications with
the library.  If this is what you want to do, use the GNU Lesser General
Public License instead of this License.  But first, please read
<http://www.gnu.org/philosophy/why-not-lgpl.html>.
}]</pre>

# ps/pdf support

proc ps2pdf {inf outf} {
  msg "ps2df conversion"
  gui::busy
  if {$::tcl_platform(platform) eq "windows"} {
    exec $::options(-windows_gs) \
	-q -P- -dNOPAUSE -dBATCH \
	-sDEVICE=pdfwrite \
	-sSAFER \
	-sOutputFile=$outf \
	-sPAPERSIZE=a4 \
	$inf
  } else {
    exec ps2pdf $inf $outf
  }
  gui::unbusy
}

proc pdfshow {f} {
  if {$::tcl_platform(platform) eq "windows"} {
    exec $::options(-windows_pdf_reader) $f &
  } else {
    exec evince $f &
  }
}



# Thanks, Version, Overview documentation
describe thanks {
Thanks to BLT, TCL, TK, ...
}

# Versioning info
describe version "<h1>TM Version <tt>$::VERSION</tt><p></h1>"
describe version {
  <p>
  This a moderately early version of the TMA time series explorer. 
  Its not perfect, it will change, have a nice day. 
  <p>
  Phil Maker <pjm@gnu.org>, <philip.maker@gmail.com>
  <p>
}
describe version "Using packages:<p><ol>"
foreach v { tcl_patchLevel tk_patchLevel blt_patchLevel} {
  describe version "<li> <tt>$v [set $v]</tt>"
}
foreach v [array names ::tcl_platform] {
  describe version "<li> <tt>$v [set ::tcl_platform($v)]</tt>"
}
describe version "<li> <tt>host [info hostname]</tt>"
describe version "</ol>"

# Overview info
describe overview {
This is intended to be a quick overview of the system.

<ol>
<li> Left/Right Arrow moves the plot through time.
<li> Shift Left/Right Arrow moves the plot through time faster.
<li> Mouse-Left .. Mouse-Left is zoom.
<li> Mouse-Right is unzoom.
<li> Double click on y axis configures it.
</ol>
}

# startup

proc main {} {  
  blt::bltdebug $::options(-bltdebug)
  tags:init

  if {$::options(-pi_tag_reader)} {
    pack [gui::gui .c] -fill x -expand 1
  }
  
  pack [plot::new .w] -fill both -expand 1
  
  if {$::options(-console)} {
    pack [console::new] -fill x -expand 1
  }
  
  wm title . "Tma"
  if {$::options(-fullscreen)} {
    wm attributes . -fullscreen 1
  } else {
    .w configure -width $::options(-width)
  }
  
  foreach {o f} $::argv { 
    if {$o eq "-get"} {
      commands::get $f
    }
  }
}

#
# check:* checking (or data validation) support
#

namespace eval check {
  variable passed 0 
  variable failed 0
  variable fd 
  variable fn
  set fd {}
  variable show_passed 1
}

command check:log {s} {log a message to console, report and check log} {
  msg $s
  report:plaintext $s
  if {$::options(-check_log)} {
    if {$::check::fd eq {}} {
      set ::check::fn [tk_getSaveFile -defaultextension .chk \
			   -initialfile "tma-latest.chk" \
			   -filetypes {{CHK .chk}}]
      
      set ::check::fd [open $::check::fn w]
    } 
    puts $::check::fd $s
    flush $::check::fd
  }
}

command check:show_passed {b} {enable/disable reports for passed tests} {
  msg "check:show_passed $b" 
  set ::check::show_passed $b
}

command check:diff {{s {}} {t {}}} {show the differences between check logs in files s and t where t defaults the current log file and s defaults to asking the user for a filename. Alternately just give both files. For this command to work tkdiff must be installed.
} {
  if {$s eq {}} {
    set s [tk_getOpenFile \
	       -filetypes {
		 {{Check logs} {*.chk}}
		 {{TMA scripts} {*.tma}}
		 {{All files} {*}}
	       }]
  }
  if {$t eq {}} {
    set t $::check::fn
  }
      
  exec tkdiff $s $t &
}

command check:saveas {filename} {save the current log into filename} {
  file copy $::check::fn $filename 
}

command check:passed {s} {s is passed} {
  incr ::check::passed
  if {$::check::show_passed} {
    check:log "check $s PASSED [check:statistics]"
  }
}

command check:failed {s} {s failed} {
  incr ::check::failed
  # always show failure
  check:log "check $s FAILED [check:statistics]"
}

command check:statistics {} {returns the check statistics passed/total tests} {
  set nt [expr $check::passed+$::check::failed]
  return "($check::passed/$nt)"
}

command check:eval {args} {evaluate args and check if its true} {
  if {[uplevel 1 $args]} {
    check:passed $args
  } else {
    check:failed $args
  }
}

command check:expr {args} {ensure expr e is true} {
  uplevel 1 check:eval [list expr {*}$args]
}

command check:tag {tagpats args} {check a tag} {
  foreach tp $tagpats {
    foreach tag [pi:tags $tp] {
      get $tag -name this
      for {set i 0} {$i < [samples this]} {incr i} {
	set this [what# this $i]
	foreach e $args {
	  check:expr $e
	}
      }
      var- this
    }
  }
}

command check:changing {{tagpats *}} {Check that tagpats are changing} {
  foreach tp $tagpats {
    foreach tag [pi:tags $tp] {
      get $tag
      if {[samples $tag] <= 1} { 
	msg "$tag is not changing"
	report:text "$tag is not changing"
	# report:plot 2i ;# no point in plotting it if its not changed.
      }
      var- *
    }
  }
}

command check:increasing {tagpats} {check that these tags are increasing} {
    foreach tp $tagpats {
      foreach tag [pi:tags $tp] {
	get $tag
	if {[samples $tag] == 0} {
	  check:failed "check:increasing $tag no samples"
	} else {
	  set v [what# $tag 0]
	  for {set i 1} {$i < [samples $tag]} {incr i} {
	    if {$v <= [what# $tag $i]} { ;# ok
	      set v [what# $tag $i]
	    } else { # failued
	      check:failed "check:increasing $tag failed at $i"
	      # possibly do a plot
	      return
	    }
	  }
	}
	var- *
      }
    }
}

command check:nonzero {tag} {check that tag is nonzero} {
  for {set i 0} {$i < [samples $tag]} {incr i} {
    if {[what# $tag $i] == 0} {
      msg "$tag is 0 at [time::format [when# $tag $i]]"
      report:text "$tag is 0 at [time::format [when# $tag $i]]"
    }
  }
}
# check all tags matching 
command check:units {{tagpat *} args} {check if all tags matching tagpat have a valid unit which is given in args} {
  msg "check:units $tagpat $args"
  foreach tag [pi:tags $tagpat] {
    if {[lsearch -exact $args [pi:units $tag]] != -1} {
      check:passed "check:units $tag [pi:units $tag]"
    } else {
      check:failed "check:units $tag [pi:units $tag]"
    }
  }
}

# check:name* - various naming convention checks

command check:name_parts {tagpat args} {check each name matching tagpat 
by breaking the tagname into parts using _ as a separator and the checking
if the corresponding pattern in args matches using string match. For example:

<pre>
check:name_parts * R A|D CN|SS|PG
</pre>

Checks all tags matching * (all tags) and in order to pass the
string should look have an R in the first part, A or D in the second 
part, CN or SS or PG in the third part. After that everything is acceptable.
So the following names are acceptable:

<pre>
R_D_PG_HIGH_TEMP_ALARM
R_A_SS_P
</pre>

But not:

<pre>
R_PG_A_ALARM
R_A_D_VALUE
</pre>
} {
  foreach tag [pi:tags $tagpat] {
    set s [split $tag _]
    set i 0
    set ok 1
    foreach p $args {
      if {$i >= [llength $s]} {
	check:failed "check:name_parts $tag $args at position $i, no field in $tag"
	set ok 0
      } elseif {![regexp -- $p [lindex $s $i]]} {
	check:failed "check:name_parts $tag at position [expr $i+1] expected $p"
	set ok 0
      } else {
	# ok if we get here
      }
      incr i
    }
    if {$ok} {
      check:passed "check:name_parts $tag"
    }
  }
}
# and finally start the dog and pony show      
main




