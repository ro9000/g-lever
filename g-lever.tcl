
proc structure_test_passed {} {

  global PROGRAM_NAME
  global PROGRAM_RUN

  global EXPERIMENTAL

  global MAIN_DIR
  global MAIN_EXE
  global MAIN_X
  global MAIN_DB
  global MAIN_BACKUPS_DIR


  # The sanctified structure:
  #
  # PROGRAM_NAME/godlever/godlever.exe
  # PROGRAM_NAME/main/PROGRAM_NAME.exe


  puts "--structure test--"

  if {![file exists $MAIN_EXE]} {
    puts "  $MAIN_EXE doesn't exist"
    return 0
  }



  return 1
}

set PROGRAM_NAME [file tail [file dirname [file dirname [info nameofexe]]]]
puts "PROGRAM_NAME $PROGRAM_NAME"

set PROGRAM_RUN [file tail [file dirname [info nameofexe]]]
puts "PROGRAM_RUN $PROGRAM_RUN"



if {($PROGRAM_NAME eq "godlever") && ($PROGRAM_RUN eq "main")} {
  set EXPERIMENTAL 1
} else {
  set EXPERIMENTAL 0
}
puts "EXPERIMENTAL $EXPERIMENTAL"



set MAIN_DIR   [file normalize [file join $dir .. main]]
set MAIN_EXE   [file join $MAIN_DIR ${PROGRAM_NAME}.exe]
set MAIN_X     [file join $MAIN_DIR ${PROGRAM_NAME}.x]
set MAIN_DB    [file join $MAIN_DIR ${PROGRAM_NAME}.gldb]
set MAIN_BACKUPS_DIR [file join $MAIN_DIR godlever-backups]


if {$EXPERIMENTAL} {
  set DB_FILE [file join $MAIN_DIR experimental experimental.gldb]
} else {
  set DB_FILE $MAIN_DB
}
puts "DB_FILE $DB_FILE"



if {![structure_test_passed]} {
  console show
  puts "FATAL ERROR:  structure test failed."
  puts "qed."
  return
}



if {$EXPERIMENTAL} {
  wm iconbitmap . -default [file join $dir experimental-icon.ico]
}

# disabled since i'm moving to timestamps.  but the logic is good for finding next backup number.
proc USING_NUMS_backup_gldb_and_x {} {

  global MAIN_DB

  set bz [glob -nocomp -tails -dir [file dirname $MAIN_DB] -- [file tail $MAIN_DB].????]
  set bz [lsort -dict $bz]
  set latest [lindex $bz end]

  set latest_decorated_num \
    [string range $latest [expr [string last "." $latest]+1] end]
  set latest_num [string trimleft $latest_decorated_num 0]

  if {![string is integer -strict $latest_num]} {
    error "can't get last backup number (last found is >$latest<)"
  }

  incr latest_num

  set backup_ext [format %04s $latest_num]
  set backup_db $MAIN_DB.$backup_ext
  set backup_x  $MAIN_DB.$backup_ext.x

  puts --$backup_db
  puts --$backup_x


  return
}

proc make_sure_backup_dir_exists {} {
  global MAIN_BACKUPS_DIR
  if {![file exists $MAIN_BACKUPS_DIR]} {file mkdir $MAIN_BACKUPS_DIR}
}

proc backup_gldb_and_x {{label ""}} {

  global PROGRAM_NAME
  global MAIN_DIR
  global MAIN_DB
  global MAIN_X
  global MAIN_BACKUPS_DIR

  set backup_time [now]
  if {$label eq ""} {set k ""} {set k ".$label"}
  set backup_db $PROGRAM_NAME.$backup_time.gldb$k
  set backup_x  $PROGRAM_NAME.$backup_time.x$k


  make_sure_backup_dir_exists
  eralon_copyfile $MAIN_DB [file join $MAIN_BACKUPS_DIR $backup_db]
  eralon_copyfile $MAIN_X  [file join $MAIN_BACKUPS_DIR $backup_x]

}

set bxui_toplevel .backup_ui

proc bxui_init {} {

  set t $::bxui_toplevel

  if {[winfo exists $t]} {destroy $t}

  toplevel $t

  set gx [expr [winfo pointerx .] - 50]
  set gy [expr [winfo pointery .] - 50]
  wm geom $t +$gx+$gy

  entry $t.e -font {Verdana 12 bold} -bg black -fg white \
    -insertbackground green -insertwidth 3
  pack $t.e -fill x

  bind $t.e <Return> bxui_execute
  bind $t.e <Escape> bxui_destroy

  focus $t.e

}

proc bxui_destroy {} {

  set t $::bxui_toplevel

  destroy $t
  
}

proc bxui_execute {} {

  set t $::bxui_toplevel

  set str [$t.e get]

  if {[string trim $str] eq ""} {
    backup_gldb_and_x
  } else {
    backup_gldb_and_x [string trim $str]
  }

  bxui_destroy

}

####xia####alpha-go####

set dir [file dirname [info nameofexe]]


console show

console eval {
  bind Console <Control-Key-p> {tk::ConsoleHistory prev}
  bind Console <Control-Key-n> {tk::ConsoleHistory next}
  .console conf -font {"Lucida Console" 8}
}


puts -nonewline "-=> evaling [file tail [info script]] @ [clock format [clock sec] -format "%Y-%m-%d--%H-%M-%S"]"
puts -nonewline " ($::tcl_patchLevel)"
if {[info exists ::Interp]} {puts " ($::Interp)"} else {puts ""}

lappend auto_path [file join $dir lib]

####ixa####

####xia####base-funcs####
proc ? {} {puts $::errorInfo}
proc dump {p} {list proc $p [info args $p] [info body $p]}
proc timestamp_format {x} {clock format $x -format "%Y-%m-%d--%H-%M-%S"}
proc now {} {timestamp_format [clock sec]}
proc shell_open {x} {winutils::shell -verb open $x}
proc file_save {fn x} {set f [open $fn w] ; puts -nonewline $f $x ; close $f}
proc file_get {fn} {set f [open $fn r] ; set c [read $f] ; close $f ; return $c}
proc this_prok {} {uplevel {lindex [info level 0] 0}}
proc pn03_pause {ms} {set ::pn03_x 0 ; after $ms {set ::pn03_x 1} ; tkwait variable ::pn03_x}
proc global_n_set {x value} {uplevel [list global $x] ; uplevel [list set $x $value]}
####ixa####

package require sqlite3
package require sqlitecon

package require winutils

package require twapi

package require muzic


set exe_copyfile [file join $dir exe copyfile.exe]
set exe_md5gold  [file join $dir exe md5gold.exe]

####xia####emacs-control####

proc raise_emacs {} {

  # this works as well but hardcodes the emacs path
  # (idea: could use windows registry info to not hardcode path)
  #
  #exec {c:\data\emacs\bin\gnuclientw.exe} -x -q -e

  ::twapi::set_foreground_window [lindex [::twapi::find_windows -class Emacs] 0]
}

####ixa####

####xia####hyperspeed####

proc ap {code list} {
  set z {}
  foreach el $list {
    lappend z [eval $code [list $el]]
  }
  return $z
}

proc tap {code list} {
  set z {}
  foreach el $list {
    if {[eval $code [list $el]]} {
      lappend z $el
    }
  }
  return $z
}






proc 5ap {lambda list} {
  set z {}
  foreach el $list {
    lappend z [apply $lambda $el]
  }
  return $z
}

proc t5ap {lambda list} {
  set z {}
  foreach el $list {
    if {[apply $lambda $el]} {
      lappend z $el
    }
  }
  return $z
}

proc x5ap {lambda_body list} {
  5ap [list x $lambda_body] $list
}

proc xt5ap {lambda_body list} {
  t5ap [list x $lambda_body] $list
}









proc chain_apply {list chains} {
  foreach code $chains {
    set list [eval $code [list $list]]
  }
  return $list
}




proc zset {vars list} {

  set elz {}
  set zz  {}

  set i 0
  foreach v $vars {

    lappend elz el$i
    lappend zz  z$i

    upvar $v z$i
    set z$i {}

    incr i
  }



  foreach $elz $list {

    foreach el $elz z $zz {
      lappend $z [set $el]
    }

  }


  return
}




proc scan_geom {w} {
  scan [wm geom $w] "%dx%d+%d+%d" w h x y ; return [list $w $h $x $y]
}

proc notbl {x body} {
  if {![eq "" $x]} {uplevel $body}
}


proc cont {ex} {
  if {[uplevel [list expr $ex]]} {return -code continue}
}


proc raise_order {args} {
  foreach w $args {
    cont {![winfo exists $w]}
    focus -force $w
  }
}


####ixa####

####xia####base-aliases####
proc a> {a args} {eval [list interp alias {} $a {}] $args}
a> a< interp alias {}

a> :: namespace inscope ::
a> g# uplevel #0

a> eq string equal
####ixa####

####xia####restart####
proc restart_this_process {} {
  final_finish
  winutils::launch [info nameofexe]
  exit
}

####ixa####

####xia####all-bindings####
bind all <Alt-F12> {console show}
bind all <F2> {puts [winfo containing [winfo pointerx %W] [winfo pointery %W]]}
####ixa####

####xia####ss-saturn####


## replaced with db
##sqlite3 ss [file join $dir saturn.db]


proc ss_INIT {} {

  ss eval {

    create table program (
      name text not null primary key, 
      value text not null
    );

  }

}



proc ss_get {name} {
  return [ss one {select value from program where name=$name}]
}

proc ss_set {name value} {
  ss eval {replace into program (name, value) values ($name, $value)}
}

proc ss_del {name} {
  ss eval {delete from program where name=$name}
}

proc ss_all {} {
  return [ss eval {select name, value from program order by name asc}]
}


####ixa####

####xia####ss-solarstations####

proc solarstations_load {w name} {
  if {![winfo exists $w]} return
  set geom [ss_get $name]
  notbl $geom {wm geom $w $geom}
}

proc solarstations_save_just_position {w name} {
  if {![winfo exists $w]} return
  lassign [scan_geom $w] w h x y
  ss_set $name +$x+$y
}

proc solarstations_save_full_geom {w name} {
  if {![winfo exists $w]} return
  ss_set $name [wm geom $w]
}


proc solarstations_console_load {} {
  set name "coords_console"
  set geom [ss_get $name]
  notbl $geom [list console eval [list wm geom . $geom]]
}

proc solarstations_console_save_full_geom {} {
  set name "coords_console"
  ss_set $name [console eval {wm geom .}]
}

####ixa####

####xia####timers####
proc up1timer {body} {
  set begin_time [clock seconds]
  uplevel $body
  set end_time [clock seconds]
  set time_elapsed [expr $end_time - $begin_time]
  puts "**elapsed $time_elapsed s**"
}
####ixa####

####xia####eralon####


proc eralon_core {prog} {

  global_n_set eralon_line   ""
  global_n_set eralon_ret    BOGUS
  global_n_set eralon_wait   0

  set h [open "| $prog" r]
  fconfigure $h -blocking 0
  fileevent $h readable [list eralon_readable $h]

  tkwait variable eralon_wait

  #foreach el [info vars eralon*] {puts "$el -> [set $el]"}

  return [list $eralon_ret $eralon_line]
}

proc eralon_readable {h} {

  global eralon_line
  global eralon_ret
  global eralon_wait

  if {[eof $h]} {

    fconfigure $h -blocking 1   ;# put it in blocking mode to set ::errorCode
    if {[catch {close $h}]} {
      set ret [lindex $::errorCode end]    ;# ::errorCode looks like {CHILDSTATUS 3592 2}
    } else {
      set ret 0
    }
    
    set eralon_ret $ret

    set eralon_wait 1  ;# finished with the piped process, now return control to caller

  } elseif {[gets $h line] != -1} {
    set eralon_line $line
  }

  return
}




proc eralon_md5 {fn} {

  set prog [list $::exe_md5gold $fn]
  foreach {ret md5} [eralon_core $prog] break
  
  if {![eq 0 $ret]} {error "[this_prok]: bad exit code >$ret< for >$fn<"}

  if {[string length $md5] != 32} {
    error "[this_prok]: bad md5 (not 32 chars long) >$md5< for >$fn<"
  }
  
  return $md5
}




proc eralon_copyfile {src dest} {

  set prog [list $::exe_copyfile $src $dest]
  foreach {ret _line} [eralon_core $prog] break
  
  if {![eq 0 $ret]} {error "[this_prok]: bad exit code >$ret< for copying >$src< to >$dest<"}

  return
}


####ixa####

proc make_icons_for_nodes_that_have_none {} {
  set nodes [db eval {select id from nodes}]
  foreach id $nodes {
    set icon [db one {select icon_spec from nodes2icons where node_id = $id}]
    #puts "$id -> >$icon<"
    if {$icon eq ""} {
      puts "--creating icon spec for $id"
      set new_spec [iel_gen_full_icon_spec]
      db eval {insert into nodes2icons (node_id, icon_spec) values ($id, $new_spec)}
    }
  }
}


proc make_music_for_nodes_that_have_none {} {
  set nodes [db eval {select id from nodes}]
  foreach id $nodes {
    set music [db one {select music_spec from nodes2music where node_id = $id}]
    #puts "$id -> >$music<"
    if {$music eq ""} {
      puts "--creating music spec for $id"
      set new_spec [xtl_random]
      db eval {insert into nodes2music (node_id, music_spec) values ($id, $new_spec)}
    }
  }
}

sqlite3 db $DB_FILE


proc ss {args} {uplevel [list eval db $args]} ;# now ss's table is part of db, no more saturn.db

proc db_table_exists {t} {
  set sql [format {select count(*) from %s} $t]
  if {[catch {db eval $sql}]} {
    return 0
  } else {
    return 1
  }
}

if {![db_table_exists "nodes"]} {
  db eval {
    create table nodes (id text primary key, x int, y int, code text, parent_node text);
  }
}


if {![db_table_exists "program"]} {
  ss_INIT
}


if {![db_table_exists "texts"]} {
  db eval {
    create table texts (
      node_id text primary key, 
      ins text, 
      yview text, 
      geom text, 
      onscreen text
    );
  }
}


if {![db_table_exists "nodes2icons"]} {
  db eval {
    create table nodes2icons (node_id text primary key, icon_spec text);
  }
}


if {![db_table_exists "nodes2music"]} {
  db eval {
    create table nodes2music (node_id text primary key, music_spec text);
  }
}

sqlitecon::create .db {> } "db" db
.db.t conf -font {{Lucida Console} 8}

proc db_console_exec {x} {

  sqlitecon::SetLine .db.t $x
  sqlitecon::Enter .db.t

}

bind Sqlitecon <MouseWheel> {
  .db.t yview scroll [expr {-%D/120 * 4}] units
}

##select id, x, y, sql_tidbit(20,code), parent_node from nodes;
proc sql_tidbit {len x} {return [string range [string trim [string map {\n " "} $x]] 0 [expr $len-1]]}
db function sql_tidbit sql_tidbit

proc oval_center {coords} {
  foreach {x1 y1 x2 y2} $coords break
  set radius [expr ($x2-$x1)/2]
  set x0 [expr $x1+$radius]
  set y0 [expr $y1+$radius]
  return [list $x0 $y0]
}

proc oval_coords {x0 y0 size} {
  return [list [expr $x0-($size/2)] [expr $y0-($size/2)] [expr $x0+($size/2)] [expr $y0+($size/2)]]
}

proc lpick L {lindex $L [expr {int(rand()*[llength $L])}]}

proc gen_random_string { length {chars {a b c d e f g h i j k l m n o p q r s t u v w x y z}} } {
  for {set i 0} {$i<$length} {incr i} {append res [lpick $chars]}
  return $res
}

# zvar is the variable that accumulates all the different unique identifiers

proc unique_identifier {zvar length {chars ""}} {

  upvar $zvar z

  set cmd [list gen_random_string $length]
  if {$chars ne ""} {lappend cmd $chars}

  set found 0
  set tries 0
  while {$tries < 1000} {
    set str [eval $cmd]
    if {[lsearch -exact $z $str] == -1} {
      lappend z $str
      set found 1
      break
    }
    incr tries
  }

  if {!$found} {error "couldn't find a new unique identifier in 1000 tries"}

  return $str
}

proc final_finish {} {
  ss_set exiting_at_time_t [clock seconds]
  solarstations_save_full_geom  .    coords_main
  solarstations_save_full_geom  .db  coords_db
  solarstations_console_save_full_geom
}

proc raise_importants {} {
  raise_order .
}


wm protocol . WM_DELETE_WINDOW {final_finish ; exit}


solarstations_load  .    coords_main
solarstations_load  .db  coords_db
solarstations_console_load

update

raise_importants

# hotkeys (not using winutils anymore because of multiple programs stepping on each others hotkeys)

if {!$EXPERIMENTAL} {

  bind all <Alt-F12> ::xc::new_experimental_process
  bind all <Alt-F9> bxui_init   ;# call the backup ui

}

# Traditional hotkeys

if 0 {
winutils::hotkey #auto Alt+f1 raise_emacs
winutils::hotkey #auto Alt+f2 {console eval {focus -force .console} ; raise_importants}
winutils::hotkey #auto Alt+f3 {console eval {focus -force .console}}
winutils::hotkey #auto Alt+f4 {focus -force .db.t}
winutils::hotkey #auto Alt+f12 restart_this_process
}

namespace eval ::xc {

  variable zcanvas {}
  variable INIT 0

}

# instead of writing:
#   upvar ::xc::${canvas_id}::t t
# you can do this:
#   link t
#
proc ::xc::link {v} {
  uplevel [format {upvar ::xc::${canvas_id}::%s %s} $v $v]
}

proc ::xc::init {} {

  variable INIT
  if {$INIT} {
    puts "already ran [this_prok]"
    return
  } else {
    set INIT 1
  }

  set ::xc::movable_is_active_flag 0

  ::xc::create_canvas_with_parent -99  ;# root canvas


}

proc ::xc::destroy_canvas {canvas_id} {
  
  link t
  link node_status_w
  link parent_icon_label

  ::xc::delete_all_canvas_node_items $canvas_id

  ::xc::reclaim_silver_thread $parent_icon_label

  # destroy toplevels
  #
  destroy $t
  destroy $node_status_w
  ::xc::destroy_canvas_searchw $canvas_id

  # delete personal namespace
  #
  set ns ::xc::$canvas_id
  namespace delete $ns

  # remove from ::xc::zcanvas
  #
  set i [lsearch -exact $::xc::zcanvas $canvas_id]
  set ::xc::zcanvas [lreplace $::xc::zcanvas $i $i]

}

proc ::xc::create_canvas_with_parent {parent} {

  puts "new canvas with parent $parent"

  set new_canvas [::xc::create_canvas]
  ::xc::load_nodes $new_canvas $parent
  set ::xc::${new_canvas}::parent $parent

  ::xc::refresh_toplevel_canvas_icon $new_canvas

  ::xc::raise_canvas $new_canvas  

  return $new_canvas
}

proc ::xc::create_canvas {} {

  # add new identifier to ::xc::zcanvas
  set canvas_id [unique_identifier xc::zcanvas 10]

  set ns ::xc::$canvas_id
  namespace eval $ns {}

  ::xc::build_canvas $canvas_id

  return $canvas_id
}

proc ::xc::refresh_toplevel_canvas_icon {canvas_id} {

  link parent
  link parent_icon_label

  ::xc::reclaim_silver_thread $parent_icon_label
  ::xc::tile_silver_thread $parent $parent_icon_label

  return
}

proc ::xc::build_canvas {canvas_id} {



  link t
  link xc

  link parent

  link node_status_w

  link parent_icon_label


  set parent ""

  set t [toplevel .canvas_$canvas_id]

  wm geom $t 800x300

  wm protocol $t WM_DELETE_WINDOW [list wm withdraw $t]



  pack [frame $t.topf -bg black] -fill x

  set parent_icon_label [frame $t.topf.parent_icon_label]
  pack $parent_icon_label -side left
  
  pack [label $t.topf.parent_name_label -fg green -bg black -font {Georgia 14} -textvariable ::xc::${canvas_id}::parent] -side right

  pack [frame $t.breaker -background green -height 1] -fill x

  set xc [canvas $t.xc -background black -highlightthickness 0]
  pack $xc -fill both -expand 1




  


  #### status box ####

  set node_status_w .canvas_${canvas_id}_node_status_window
  toplevel $node_status_w
  $node_status_w conf -bg black

  pack [text $node_status_w.tx -wrap word -relief flat] -fill both -expand 1 -padx 4 -pady 4
  $node_status_w.tx tag conf snippet -font {"Lucida Console" 7} -foreground navy
  $node_status_w.tx tag conf nocode  -font {"Lucida Console" 9} -foreground red
  $node_status_w.tx tag conf title_info -font {Verdana 14 bold} -foreground forestgreen
  $node_status_w.tx tag conf childs -font {Verdana 14 bold}

  wm overrideredirect $node_status_w 1
  wm geom $node_status_w 400x300
  wm withdraw $node_status_w






  ::xc::canvas_searchw_build $canvas_id







  #### canvas bindings ####


  bind $xc <Double-Button-1> [list ::xc::create_node $canvas_id %x %y]



  bind $t <FocusIn> [list focus -force $xc]  ;# the canvas should always be the focused widget

  bind $xc <Delete> [list ::xc::delete_node $canvas_id]

  bind $xc <KeyPress-m> [list ::xc::canvas_change_music $canvas_id]
  bind $xc <KeyPress-i> [list ::xc::canvas_change_icon  $canvas_id]




  #### element bindings ####


  # drag nodes within the canvas
  #
  $xc bind node <1>                +[list ::xc::movable_node_start     $canvas_id %X %Y %x %y]
  $xc bind node <B1-Motion>        +[list ::xc::movable_node_motion    $canvas_id %X %Y %x %y]
  $xc bind node <ButtonRelease-1>  +[list ::xc::movable_node_released  $canvas_id %X %Y %x %y]


  # status box
  #
  $xc bind node <Enter>      +[list ::xc::node_status_enter   $canvas_id %X %Y]
  $xc bind node <1>          +[list ::xc::node_status_click   $canvas_id %X %Y]
  $xc bind node <Motion>     +[list ::xc::node_status_motion  $canvas_id %X %Y]
  $xc bind node <B1-Motion>  +[list ::xc::node_status_motion  $canvas_id %X %Y]
  $xc bind node <Leave>      +[list ::xc::node_status_leave   $canvas_id]


  # music!
  #
  $xc bind node <Enter> +[list ::xc::canvas_enter_node_play_music $canvas_id]
  $xc bind node <Leave> +[list ::xc::canvas_leave_node_play_music $canvas_id]


  # hilite current node
  #
  $xc bind node <Enter> +[list ::xc::node_color_change_enter $canvas_id]
  $xc bind node <Leave> +[list ::xc::node_color_change_leave $canvas_id]


  # expand multi nodes into their own canvas
  #
  $xc bind multi <Double-Button-1> +[list ::xc::expand_multi_node $canvas_id]


  # turn singles into multis
  #
  $xc bind single <Double-Button-3> +[list ::xc::single_to_multi $canvas_id]

  
  # text window for singles
  #
  $xc bind single <Double-Button-1> +[list ::xc::open_text_window_for_single $canvas_id]



  bind $t <Control-s> [list ::xc::show_canvas_search_window $canvas_id]
  bind $t <Control-g> [list ::xc::hide_canvas_search_window $canvas_id]



}

proc ::xc::other_node_enter {canvas_id wx wy} {

  link xc
  link ghost_node

  set x [$xc canvasx $wx] ; set y [$xc canvasy $wy]

  #puts "other_node_enter  $canvas_id: $x $y"

  set ghost_node [$xc create oval 10 10 18 18]
  $xc itemconf $ghost_node -fill yellow -outline yellow

  eval [list $xc coords $ghost_node] [oval_coords $x $y 8]

}

proc ::xc::other_node_motion {canvas_id wx wy} {

  link xc
  link ghost_node

  set x [$xc canvasx $wx] ; set y [$xc canvasy $wy]

  #puts "other_node_motion  $canvas_id: $x $y"

  eval [list $xc coords $ghost_node] [oval_coords $x $y 8]

}


proc ::xc::other_node_leave {canvas_id} {

  link xc
  link ghost_node

  #puts "other_node_leave  $canvas_id"

  $xc delete $ghost_node
  
}


proc ::xc::other_node_released {canvas_id wx wy db_node_id} {

  link xc
  link ghost_node
  link parent

  set x [$xc canvasx $wx] ; set y [$xc canvasy $wy]

  #puts "other_node_released  $canvas_id: $x $y $db_node_id"

  db eval {update nodes set x = $x, y = $y, parent_node = $parent where id = $db_node_id}

  ::xc::canvas_refresh $canvas_id

  $xc delete $ghost_node

}

proc ::xc::canvas_searchw_build {canvas_id} {

  link canvas_searchw

  set canvas_searchw .canvas_${canvas_id}_search_window
  toplevel $canvas_searchw


  pack [label $canvas_searchw.info -bg black -fg green \
    -font {Georgia 14} -textvariable ::xc::${canvas_id}::parent] -fill x

  pack [frame $canvas_searchw.breaker -background green -height 1] -fill x

  pack [text $canvas_searchw.tx -wrap word -relief flat] -fill both -expand 1
  $canvas_searchw.tx conf -font {"Lucida Console" 8} -insertwidth 0 -bg black -fg pink


  pack [entry $canvas_searchw.entry -font {Verdana 12 bold} -bg black -fg orange \
    -insertbackground pink -insertwidth 3] -fill x


  bind $canvas_searchw.entry <KeyRelease> [list ::xc::execute_canvas_search_window $canvas_id]

  bind $canvas_searchw <Control-s> [list ::xc::show_canvas_search_window $canvas_id]
  bind $canvas_searchw <Control-g> [list ::xc::hide_canvas_search_window $canvas_id]

  bind $canvas_searchw <Control-u> [list ::xc::reset_canvas_search $canvas_id]

  # clone toplevel bindings onto text widget, then make it read-only (won't process any other keypress)
  foreach el {<Control-s> <Control-g> <Control-u>} {
    bind $canvas_searchw.tx $el [bind $canvas_searchw $el]
  }
  xt86_yes_ro $canvas_searchw.tx



  focus $canvas_searchw.entry

  wm geom $canvas_searchw 340x380
  wm protocol $canvas_searchw WM_DELETE_WINDOW [list wm withdraw $canvas_searchw]
  wm withdraw $canvas_searchw

}

proc ::xc::destroy_canvas_searchw {canvas_id} {

  link canvas_searchw

  ::xc::delete_images_in_tx $canvas_searchw.tx

  destroy $canvas_searchw

}

proc ::xc::show_canvas_search_window {canvas_id} {

  link canvas_searchw

  wm deiconify $canvas_searchw
  raise $canvas_searchw

  focus $canvas_searchw.entry

}



proc ::xc::hide_canvas_search_window {canvas_id} {

  link t
  link canvas_searchw

  wm withdraw $canvas_searchw

  focus -force $t

}


proc ::xc::reset_canvas_search {canvas_id} {

  link canvas_searchw

  $canvas_searchw.entry delete 0 end

  focus $canvas_searchw.entry


}

proc ::xc::canvas_search_window_new_zmz_tag {} {
  return zmz_[gen_random_string 10]
}

proc ::xc::canvas_search_window_delete_all_zmz_tags {tx} {
  foreach el [$tx tag names] {
    if {[string match zmz* $el]} {
      $tx tag delete $el
    }
  }
  return
}

proc ::xc::execute_canvas_search_window {canvas_id} {

  link canvas_searchw

  link parent

  set tx $canvas_searchw.tx 

  set str [$canvas_searchw.entry get]

  ::xc::delete_images_in_tx $tx  ;# call before deleting content

  $tx delete 1.0 end

  ::xc::canvas_search_window_delete_all_zmz_tags $tx

  if {[string length $str] < 3} {
    $tx insert end "not enough chars yet to do a search..."
    return
  }

  #puts "searching for: >$str<"

  $tx tag conf num_matches -font {Georgia 14 bold}

  set accum_nodes [::xc::walk_tree_starting_from $parent]
  foreach node_id $accum_nodes {

    #puts "searching $el"
    set code [db one {select code from nodes where id=$node_id}]
    set mz [ux900_exact_indices $code $str]
    if {[llength $mz] == 0} continue
    #puts "found in $node_id ($mz)"

    set link_tag [::xc::canvas_search_window_new_zmz_tag]
    $tx tag conf $link_tag -font {Georgia 10} -foreground cyan
    $tx tag bind $link_tag <1> \
      [list ::xc::open_canvas_search_result $node_id $str]
    $tx tag bind $link_tag <Enter> {%W conf -cursor star}
    $tx tag bind $link_tag <Leave> {%W conf -cursor ""}

    set img_tag [::xc::canvas_search_window_new_zmz_tag]
    $tx tag bind $img_tag <Enter> \
      [list ::xc::play_music_for_node $node_id]
    $tx tag bind $img_tag <Leave> \
      [list ::xc::stop_music]
    $tx tag bind $img_tag <Double-Button-1> \
      [list ::xc::open_canvas_search_result $node_id $str]

    $tx insert end "$node_id" $link_tag
    $tx insert end "\n$mz\n"

    set img [::xc::get_image_for_node_id $node_id]
    $tx image create end -image $img
    $tx tag add $img_tag end-2c end-1c  ;# gross range but needed

    $tx insert end "   "
    $tx insert end "[llength $mz]" num_matches " matches\n\n"

  }

}

proc ::xc::open_canvas_search_result {node_id search_string} {

  #puts "ok lets go @ $node_id"
  
  ::xc::open_text_window_for_single_given_node $node_id

  ::xc::load_text_window_search_given_node_id $node_id $search_string

  

}

proc ::xc::delete_images_in_tx {tx} {

  set images [$tx image names]

  #puts "deleting [llength $images] images"

  foreach img $images {
    image delete $img
  }

}

proc ::xc::canvas_widget_to_canvas_id {w} {
  regexp {\.canvas_(.+?)\.} $w -> m
  return $m
}



proc ::xc::all_parent_nodes {n} {

  set z {}

  set maxloop 0

  while 1 {
    incr maxloop ; if {$maxloop > 1000} {error "maxxing loop"}

    if {[lsearch -exact $z $n] != -1} {error "broken tree with looping parents"}

    if {$n eq "-99"} break

    lappend z $n

    set n [db one {select parent_node from nodes where id = $n}]
  }

  return $z
}

proc ::xc::movable_node_load_vars {} {

uplevel {
  link xc
  link movable_node
  link movable_orig_x0
  link movable_orig_y0

  link movable_canvases
  link movable_in_another
  link movable_last_canvas
  link movable_last_c_id
  link movable_look

}

}


proc ::xc::movable_mnemonic_containing {canvas_id containing} {

  ::xc::movable_node_load_vars

  set inside ""
  if {$containing eq $xc} {
    set inside "itself"
  } elseif {[lsearch -exact $movable_canvases $containing] != -1} {
    set inside "another"
  } else {
    set inside "na"
  }

  return $inside
}

proc ::xc::movable_is_active {} {
  return $::xc::movable_is_active_flag
}


proc ::xc::movable_node_start {canvas_id X Y wx wy} {

  ::xc::movable_node_load_vars


  set movable_node [$xc find withtag current]

  foreach {movable_orig_x0 movable_orig_y0} [::xc::node_xy $canvas_id $movable_node] break

  $xc raise $movable_node

  set db_node_id [::xc::extract_db_node_id_from_tags [$xc gettags $movable_node]]

  set movable_canvases {}
  foreach cid $::xc::zcanvas {
    #puts "checking $cid (.canvas_${cid}.xc)"
    if {[lsearch -exact [::xc::all_parent_nodes [set ::xc::${cid}::parent]] $db_node_id] != -1} continue
    lappend movable_canvases .canvas_${cid}.xc
  }
  set movable_in_another 0
  set movable_last_canvas ""
  set movable_last_c_id ""
  set movable_look ""

  set ::xc::movable_is_active_flag 1

}

proc ::xc::movable_node_motion {canvas_id X Y wx wy} {

  ::xc::movable_node_load_vars

  set containing [winfo containing $X $Y]
  set inside [::xc::movable_mnemonic_containing $canvas_id $containing]


  ################ itself
  #
  if {$inside eq "itself"} {

    #puts "--itself"

    if {$movable_look ne "itself"} {
      set movable_look "itself"
      ::xc::set_node_look_inside_itself $canvas_id $movable_node      
    }

    if {$movable_in_another == 1} {
      set movable_in_another 0
      ::xc::other_node_leave $movable_last_c_id
    }

    set x [$xc canvasx $wx]
    set y [$xc canvasy $wy]
    ::xc::place_node $canvas_id $movable_node $x $y

  }


  ################ another
  #
  if {$inside eq "another"} {

    #puts "--another"

    if {$movable_look ne "another"} {
      set movable_look "another"
      ::xc::set_node_look_inside_another_canvas $canvas_id $movable_node      
    }

    if {$movable_in_another == 0} {
      set movable_in_another 1

      set movable_last_canvas $containing
      set movable_last_c_id [::xc::canvas_widget_to_canvas_id $movable_last_canvas]

      ::xc::place_node $canvas_id $movable_node $movable_orig_x0 $movable_orig_y0

      set other_wx [expr {$X - [winfo rootx $movable_last_canvas]}]
      set other_wy [expr {$Y - [winfo rooty $movable_last_canvas]}]
      ::xc::other_node_enter $movable_last_c_id $other_wx $other_wy

   } else {

      set other_wx [expr {$X - [winfo rootx $movable_last_canvas]}]
      set other_wy [expr {$Y - [winfo rooty $movable_last_canvas]}]
      ::xc::other_node_motion $movable_last_c_id $other_wx $other_wy

    }

  }


  ################ na
  #
  if {$inside eq "na"} {

    #puts "--na"

    if {$movable_look ne "na"} {
      set movable_look "na"
      ::xc::set_node_look_inside_not_compatible $canvas_id $movable_node
    }

    if {$movable_in_another == 1} {
      set movable_in_another 0
      ::xc::other_node_leave $movable_last_c_id
    }

    ::xc::place_node $canvas_id $movable_node $movable_orig_x0 $movable_orig_y0

  }


  return
}

proc ::xc::movable_node_released {canvas_id X Y wx wy} {

  ::xc::movable_node_load_vars

  set containing [winfo containing $X $Y]
  set inside [::xc::movable_mnemonic_containing $canvas_id $containing]

  set db_node_id [::xc::extract_db_node_id_from_tags [$xc gettags $movable_node]]



  ################ itself
  #
  if {$inside eq "itself"} {

    foreach {x y} [::xc::node_xy $canvas_id $movable_node] break

    if {($x eq $movable_orig_x0) && ($y eq $movable_orig_y0)} return

    puts "setting $db_node_id to $x,$y"
    db eval {update nodes set x=$x, y=$y where id=$db_node_id}

  }



  ################ another
  #
  if {$inside eq "another"} {

    # handle double click on multi node opening new canvas right under mouse
    if {$movable_last_canvas eq ""} return


    set other_wx [expr {$X - [winfo rootx $movable_last_canvas]}]
    set other_wy [expr {$Y - [winfo rooty $movable_last_canvas]}]

    ::xc::other_node_released $movable_last_c_id $other_wx $other_wy $db_node_id

    ::xc::canvas_refresh $canvas_id


  }


  set ::xc::movable_is_active_flag 0

  return
}

proc ::xc::delete_node {canvas_id} {

  if {![::xc::is_canvas_element_a_node $canvas_id "current"]} return

  set db_node_id [::xc::db_node_id_from_canvas_node_id $canvas_id "current"]


  # check if text window is open for this node, if so, raise it and return
  set nt [::xc::existing_text_window_for_node $db_node_id]
  if {$nt ne ""} {
    ::xc::raise_text_window $nt
    return
  }

  if {![::xc::is_empty_node $db_node_id]} return


  puts "deleting >$db_node_id<"


  db eval {delete from nodes where id=$db_node_id}
  db eval {delete from texts where node_id=$db_node_id}


  # the mouse was over this deleted node so the status box showed but
  # now the mouse has 'left' this deleted node.  and so we move on.  zen.
  #
  ::xc::node_status_leave $canvas_id  


  ::xc::destroy_canvas_with_parent $db_node_id

  ::xc::canvas_refresh $canvas_id

  # obsolete
  ####::xc::grandparent_canvas_refresh $canvas_id

  return
}

proc ::xc::canvas_refresh {canvas_id} {

  link parent

  ::xc::delete_all_canvas_node_items $canvas_id

  ::xc::load_nodes $canvas_id $parent

}


proc ::xc::grandparent_canvas_refresh {canvas_id} {

  link parent

  set grandparent [db one {select parent_node from nodes where id=$parent}]
  if {$grandparent eq ""} return

  set gp_cid [::xc::get_canvas_with_parent $grandparent]
  if {$gp_cid eq ""} return

  puts "refreshing canvas of $grandparent ($gp_cid)"
  ::xc::canvas_refresh $gp_cid

}

proc ::xc::play_music_for_node {db_node_id} {
  set music [::xc::get_music_for_node $db_node_id]
  ::xc::play_music $music
}
proc ::xc::play_music {music} {
  eval xtl_play $music
}
proc ::xc::stop_music {} {
  xtl_reset
}

proc ::xc::get_music_for_node {db_node_id} {
  if {$db_node_id eq "-99"} {return "50 100 55 100"}
  return [db one {select music_spec from nodes2music where node_id = $db_node_id}]
}

proc ::xc::new_music_for_node {node_id} {
  set new_music [xtl_random]
  db eval {replace into nodes2music (node_id, music_spec) values ($node_id, $new_music)}
  return
}



proc ::xc::raise_text_window {nt} {
  raise $nt
  focus -force $nt.tx
}

proc ::xc::raise_canvas {canvas_id} {
  link t
  wm deiconify $t
  raise $t
  focus $t
}


proc ::xc::destroy_canvas_with_parent {parent} {
  set cid [::xc::get_canvas_with_parent $parent]
  if {$cid ne ""} {::xc::destroy_canvas $cid}
}


proc ::xc::is_empty_node {db_node_id} {

  set num_childs [::xc::num_childs_of_node $db_node_id]
  set code [::xc::trimmed_code_for_node_id $db_node_id]

  return [expr {($num_childs == 0) && ($code eq "")}]
}


proc ::xc::is_canvas_element_a_node {canvas_id canvas_element_id} {

  link xc

  set tags [$xc gettags $canvas_element_id]

  if {[lsearch -exact $tags "node"] == -1} {
    return 0
  } else {
    return 1
  }
}


proc ::xc::extract_db_node_id_from_tags {tags} {
  set db_node_id ""
  foreach el $tags {
    if {[regexp node_(.+) $el -> _]} {
      set db_node_id $el ; break
    }
  }
  if {$db_node_id eq ""} {error "can't get node name"}
  return $db_node_id
}

proc ::xc::db_node_id_from_canvas_node_id {canvas_id canvas_node_id} {
  link xc
  return [::xc::extract_db_node_id_from_tags [$xc gettags $canvas_node_id]]
}


proc ::xc::num_childs_of_node {node_id} {
  set num_childs [db one {select count(id) from nodes where parent_node = $node_id}]
  return $num_childs
}


proc ::xc::raw_code_for_node_id {node_id} {
  set code [db one {select code from nodes where id = $node_id}]
  return $code
}

proc ::xc::trimmed_code_for_node_id {node_id} {
  set code [db one {select code from nodes where id = $node_id}]
  return [string trim $code]
}


proc ::xc::new_db_node {x y code parent_node} {

  set db_node_id [::xc::new_database_node_id]

  db eval {insert into nodes (id, x, y, code, parent_node) values
    ($db_node_id, $x, $y, $code, $parent_node)}

  ::xc::new_spec_for_node $db_node_id

  ::xc::new_music_for_node $db_node_id

  return $db_node_id
}


proc ::xc::set_code_for_db_node {db_node_id code} {
  if {[::xc::get_node_type $db_node_id] eq "multi"} {
    error "can't set code for multi $db_node_id"
  }
  db eval {update nodes set code=$code where id = $db_node_id}
  return
}


proc ::xc::all_text_windows {} {
  set z {}
  foreach w [winfo child .] {
    if {[regexp {^\.text__(node_\w+_\d+)$} $w -> node]} {
      lappend z $node $w
    }
  }
  return $z
}

proc ::xc::existing_text_window_for_node {db_node_id} {
  set nt .text__${db_node_id}
  if {[winfo exists $nt]} {return $nt} {return ""}
}

proc ::xc::get_spec_for_node {node_id} {
  return [db one {select icon_spec from nodes2icons where node_id = $node_id}]
}

proc ::xc::new_spec_for_node {node_id} {
  set new_spec [iel_gen_full_icon_spec]
  db eval {replace into nodes2icons (node_id, icon_spec) values ($node_id, $new_spec)}
  return
}

proc ::xc::enum_canvases {} {
  set z {}
  foreach cid $::xc::zcanvas {
    lappend z $cid [set ::xc::${cid}::parent]
  }
  return $z
}

proc ::xc::get_canvas_with_parent {given_parent_node} {

  foreach {cid parent_node} [::xc::enum_canvases] {
    if {$given_parent_node eq $parent_node} {return $cid}
  }

  return ""
}

proc ::xc::new_database_node_id {} {
  return "node_[gen_random_string 10]_[clock seconds]"
}

proc ::xc::node_xy {canvas_id node} {
  link xc
  return [$xc coords $node]
}

proc ::xc::place_node {canvas_id node x y} {
  link xc
  $xc coords $node $x $y
}

proc ::xc::set_node_look_inside_itself {canvas_id node} {
  #puts "look is ITSELF"
  ::xc::set_canvas_node_item_border $canvas_id $node green
}

proc ::xc::set_node_look_inside_another_canvas {canvas_id node} {
  #puts "look is ANOTHER"
  ::xc::set_canvas_node_item_border $canvas_id $node yellow
}

proc ::xc::set_node_look_inside_not_compatible {canvas_id node} {
  #puts "look is NOT COMPAT"
  ::xc::set_canvas_node_item_border $canvas_id $node red
}



proc ::xc::node_color_change_enter {canvas_id} {

  if {[::xc::movable_is_active]} return

  #puts "look is ENTER"
  set node "current"
  ::xc::set_canvas_node_item_border $canvas_id $node cyan
}

proc ::xc::node_color_change_leave {canvas_id} {

  if {[::xc::movable_is_active]} return

  #puts "look is LEAVE"
  set node "current"
  ::xc::set_canvas_node_item_border $canvas_id $node white
}

proc ::xc::create_node {canvas_id wx wy} {

  link xc
  link parent


  # replace this with a well-grounded-node? proc that checks all parents to make sure its ok
  #
  # is this canvas eligible for having new nodes?  does it still have a parent node?  
  # or has that parent been deleted right under us?
  if {$parent ne "-99"} {
    if {[db one {select parent_node from nodes where id=$parent}] eq ""} return
  }


  set x [$xc canvasx $wx]
  set y [$xc canvasy $wy]

  set overlapped [$xc find overlapping [expr $x-4] [expr $y-4] [expr $x+4] [expr $y+4]]
  if {$overlapped ne ""} return


  set code ""
  ::xc::new_db_node $x $y $code $parent


  ::xc::canvas_refresh $canvas_id

  # obsolete
  ####::xc::grandparent_canvas_refresh $canvas_id

}

proc ::xc::expand_multi_node_given_node_id {db_node_id} {

  set node_type [::xc::get_node_type $db_node_id]
  if {$node_type ne "multi"} return

  set cid [::xc::get_canvas_with_parent $db_node_id]

  if {$cid eq ""} {
    ::xc::create_canvas_with_parent $db_node_id
  } else {
    ::xc::raise_canvas $cid
    ::xc::canvas_refresh $cid
  }

}

proc ::xc::expand_multi_node {canvas_id} {

  set db_node_id [::xc::db_node_id_from_canvas_node_id $canvas_id "current"]

  ::xc::expand_multi_node_given_node_id $db_node_id

}



proc ::xc::single_to_multi {canvas_id} {

  set db_node_id [::xc::db_node_id_from_canvas_node_id $canvas_id "current"]

  # check if text window is open for this node, if so, raise it and return
  set nt [::xc::existing_text_window_for_node $db_node_id]
  if {$nt ne ""} {
    ::xc::raise_text_window $nt
    return
  }

  set code [::xc::trimmed_code_for_node_id $db_node_id]

  if {$code ne ""} {

    ::xc::set_code_for_db_node $db_node_id ""
    set x 40 ; set y 40
    ::xc::new_db_node $x $y $code $db_node_id

  }

  ::xc::create_canvas_with_parent $db_node_id

  ::xc::canvas_refresh $canvas_id

}

proc ::xc::open_text_window_for_single {canvas_id} {

  link xc

  set db_node_id [::xc::extract_db_node_id_from_tags [$xc gettags current]]

  ::xc::open_text_window_for_single_given_node $db_node_id

}

proc ::xc::open_text_window_for_single_given_node {db_node_id} {


  set nt .text__${db_node_id}

  if {[winfo exists $nt]} {
    ::xc::raise_text_window $nt
    return
  }
  
  toplevel $nt

  # set the height to large and the line height to 1 so 
  # that the saved label has enough room to show up

  wm geom $nt 400x200


  pack [frame $nt.silver] -fill x
  ::xc::tile_silver_thread $db_node_id $nt.silver


  pack [text $nt.tx -height 1 -relief flat -bg black -fg yellow -insertwidth 3 -insertbackground cyan -undo 1] -fill both -expand 1

  $nt.tx conf -font {"Lucida Console" 7}

  $nt.tx ins end [db one {select code from nodes where id=$db_node_id}]

  bind $nt.tx <Control-q> [list ::xc::save_text_window $nt $db_node_id]
  wm protocol $nt WM_DELETE_WINDOW [list ::xc::save_text_window_and_close $nt $db_node_id]

  label $nt.l -text SAVED -font {Verdana 18 bold}






  $nt.tx tag conf search_match -background green -foreground black

  entry $nt.search -font {Verdana 12 bold} -bg black -fg orange -insertbackground pink -insertwidth 3

  bind $nt.search <KeyRelease> [list ::xc::text_search_actuate $nt]
  bind $nt.search <Return> [list focus -force $nt.tx]

  bind $nt.tx <Control-s> [list ::xc::text_search $nt]

  bind $nt.tx     <Control-g> [list ::xc::text_search_end $nt]
  bind $nt.search <Control-g> [list ::xc::text_search_end $nt]

  bind $nt.search <Control-u> [list $nt.search delete 0 end]




  # restore geom, yview, and ins from db
  db eval {select ins, yview, geom from texts where node_id = $db_node_id} {
    #puts ins:$ins
    #puts yview:$yview
    #puts geom:$geom
    wm geom $nt $geom
    $nt.tx yview moveto $yview
    $nt.tx mark set insert $ins
    #$nt.tx see $ins
  }


  focus -force $nt.tx

}

proc ::xc::save_text_all {nt db_node_id} {
  set c [$nt.tx get 1.0 end-1c]
  ::xc::set_code_for_db_node $db_node_id $c

  set ins [$nt.tx index insert]
  set yview [lindex [$nt.tx yview] 0]
  set geom [winfo geom $nt]
  #puts ins:$ins
  #puts yview:$yview
  #puts geom:$geom
  db eval {replace into texts (node_id, ins, yview, geom, onscreen) 
    values ($db_node_id, $ins, $yview, $geom, '1')}


}

proc ::xc::save_text_window {nt db_node_id} {
  ::xc::save_text_all $nt $db_node_id
  pack $nt.l -fill x
  after 500 [list pack forget $nt.l]
}

proc ::xc::save_text_window_and_close {nt db_node_id} {
  ::xc::save_text_all $nt $db_node_id
  ::xc::reclaim_silver_thread $nt.silver
  destroy $nt
}

proc ::xc::tile_silver_thread {db_node_id frame} {

  # all parents for node, keep stacking images left to right, make em clickble

  set thread_nodes [::xc::all_parent_nodes $db_node_id]
  lappend thread_nodes -99  ;# add the root node because the func doesnt

  #puts silver>$thread_nodes<

  $frame conf -bg black

  set first 1
  foreach node $thread_nodes {

    set img [::xc::get_image_for_node_id $node]

    pack [label $frame.$node -image $img -bd 0] \
      -side left -anchor n -pady 1 -padx 2
    bind $frame.$node <Double-Button-1> [list ::xc::expand_multi_node_given_node_id $node]
    bind $frame.$node <Enter> [list ::xc::play_music_for_node $node]
    bind $frame.$node <Leave> [list ::xc::stop_music]

    if {$first} {set first 0 ; 
      pack [frame $frame.breaker -width 40 -bg black] -side left
    }

  }

  return
}

proc ::xc::reclaim_silver_thread {frame} {
  foreach w [winfo children $frame] {
    if {[winfo class $w] eq "Label"} {
      set img [$w cget -image]
      if {$img ne ""} {image delete $img}
    }
  }
  return
}

# returns string indices for exact match
#
proc ux900_exact_indices {c str} {

  # explicitly escape all non-alphanumeric characters
  regsub -all {\W} $str {\\&} quot_str

  set mz [regexp -all -indices -inline -- $quot_str $c]

  return $mz
}


# returns text widget indices for exact match
#
proc ux900_exact_textw_indices {c str} {

  set z {}
  foreach m [ux900_exact_indices $c $str] {
    foreach {m0 m1} $m break
    incr m1
    lappend z [list 1.0+${m0}c 1.0+${m1}c]
  }

  return $z
}

# nice and easy read only text widgets

proc xt86_yes_ro {tx} {
  bind $tx <KeyPress> break
  bind $tx <Control-c> {event generate %W <<Copy>>}
}

proc xt86_no_ro {tx} {
  bind $tx <KeyPress> {}
}

proc ::xc::text_search {nt} {

  pack $nt.search -fill x
  focus -force $nt.search


}


proc ::xc::text_search_end {nt} {

  $nt.tx tag remove search_match 1.0 end

  pack forget $nt.search
  focus -force $nt.tx

}

proc ::xc::text_search_actuate {nt} {

  set str [$nt.search get]
  set c [$nt.tx get 1.0 end-1c]

  #puts "searching thru $nt for $str"

  $nt.tx tag remove search_match 1.0 end

  if {$str eq ""} return

  foreach m [ux900_exact_textw_indices $c $str] {
    foreach {m0 m1} $m break
    $nt.tx tag add search_match $m0 $m1
  }

  return
}

proc ::xc::load_text_window_search_given_node_id {node_id search_string} {
  set nt .text__${node_id}
  ::xc::load_text_window_search $nt $search_string
}

proc ::xc::load_text_window_search {nt str} {
  ::xc::text_search $nt
  $nt.search delete 0 end
  $nt.search insert end $str
  ::xc::text_search_actuate $nt
  after 100 [list focus -force $nt.tx]  ;# hack and i dont know why
}

proc ::xc::canvas_enter_node_play_music {canvas_id} {
  set db_node_id [::xc::db_node_id_from_canvas_node_id $canvas_id "current"]
  ::xc::play_music_for_node $db_node_id
}

proc ::xc::canvas_leave_node_play_music {canvas_id} {
  ::xc::stop_music
}

proc ::xc::canvas_change_icon {canvas_id} {

  if {![::xc::is_canvas_element_a_node $canvas_id "current"]} return

  set db_node_id [::xc::db_node_id_from_canvas_node_id $canvas_id "current"]

  ::xc::new_spec_for_node $db_node_id
  
  ::xc::canvas_refresh $canvas_id

}


proc ::xc::canvas_change_music {canvas_id} {

  if {![::xc::is_canvas_element_a_node $canvas_id "current"]} return

  set db_node_id [::xc::db_node_id_from_canvas_node_id $canvas_id "current"]

  ::xc::new_music_for_node $db_node_id

  # not explicitely necessary but it will force a replay of the icon since 
  # the mouse is now over it
  #
  ::xc::canvas_refresh $canvas_id  

}

proc ::xc::node_status_enter {canvas_id X Y} {

  link xc
  link node_status_w

  set tags [$xc gettags current]
  set db_node_id [::xc::extract_db_node_id_from_tags $tags]

  set nX [expr $X + 10]
  set nY [expr $Y + 40]

  wm geom $node_status_w +$nX+$nY
  wm deiconify $node_status_w
  raise $node_status_w


  set num_childs [::xc::num_childs_of_node $db_node_id]
  lassign [oval_center [$xc coords current]] x0 y0
  set snippet [string trim [string range [db one {select code from nodes where id=$db_node_id}] 0 end]]

  $node_status_w.tx del 1.0 end
  $node_status_w.tx ins end "$db_node_id" title_info
  $node_status_w.tx ins end "\n($x0, $y0)"
  $node_status_w.tx ins end "\nchilds: $num_childs" childs
  $node_status_w.tx ins end "\n$tags"
  if {$snippet ne ""} {
    $node_status_w.tx ins end "\n\n" "" $snippet snippet
  } else {
    $node_status_w.tx ins end "\n\n" "" "NO CODE" nocode
  }
  

}

proc ::xc::node_status_click {canvas_id X Y} {

  link node_status_w
  raise $node_status_w

}


proc ::xc::node_status_motion {canvas_id X Y} {

  link node_status_w

  set nX [expr $X + 10]
  set nY [expr $Y + 40]

  wm geom $node_status_w +$nX+$nY

}


proc ::xc::node_status_leave {canvas_id} {

  link node_status_w
  wm withdraw $node_status_w

}

proc ::xc::set_canvas_node_item_border {canvas_id canvas_node_item bdcolor} {
  set img [::xc::get_image_for_canvas_node_item $canvas_id $canvas_node_item]
  iel_paint_border $img $bdcolor
}


proc ::xc::get_image_for_canvas_node_item {canvas_id canvas_node_item} {
  link xc
  return [$xc itemcget $canvas_node_item -image]
}

proc ::xc::delete_all_canvas_node_items {canvas_id} {

  link xc

  foreach canvas_node [$xc find withtag node] {
    ::xc::delete_canvas_node_item $canvas_id $canvas_node
  }

  return
}

proc ::xc::delete_canvas_node_item {canvas_id canvas_node_item} {

  link xc

  set img [::xc::get_image_for_canvas_node_item $canvas_id $canvas_node_item]
  image delete $img

  $xc delete $canvas_node_item

  return
}

proc ::xc::get_image_for_node_id {node_id} {
  set node_type [::xc::get_node_type $node_id]
  set img [::xc::get_image_for_node_id_given_node_type $node_type $node_id]
  return $img  
}

proc ::xc::get_image_for_node_id_given_node_type {node_type node_id} {

  set spec [::xc::get_spec_for_node $node_id]

  if {$node_id eq "-99"} {
    set spec {{black white} {sqr 0 0}}
  }

  if {$spec eq ""} {error "no icon spec for $node_id"}

  if {$node_type eq "single"} {

    set img [image create photo -height 40 -width 40]
    iel_paint_icon_given_spec $img $spec
    iel_paint_border $img white

  } else {

    # multinode green flag on bottom of icon

    set img [image create photo -height 46 -width 40]
    iel_paint_icon_given_spec $img $spec
    iel_paint_border $img white
    $img put black -to 0 40 46 46
    $img put green -to 20 41 46 46

  }

  return $img
}

proc ::xc::create_canvas_node_item {canvas_id node_type node_id} {

  link xc

  set img [::xc::get_image_for_node_id_given_node_type $node_type $node_id]
  set node [$xc create image 10 10 -image $img -tags [list node $node_type $node_id]]

  return $node
}


proc ::xc::get_node_type {node_id} {

  set num_childs [::xc::num_childs_of_node $node_id]
  if {$num_childs == 0} {set node_type single} else {set node_type multi}

  # additional check if there is a canvas for this node still open then its a multi
  # possible that it was a multi, all was deleted in it, and it's still open
  if {[::xc::get_canvas_with_parent $node_id] ne ""} {set node_type multi}

  return $node_type
}



proc ::xc::load_nodes {canvas_id parent_node} {

  puts "::xc::load_nodes $canvas_id $parent_node"

  db eval {select id as node_id, x, y from nodes where parent_node = $parent_node} {

    set node_type [::xc::get_node_type $node_id]
    set node [::xc::create_canvas_node_item $canvas_id $node_type $node_id]
    ::xc::place_node $canvas_id $node $x $y

  }

}

proc ::xc::walk_tree_aux_ordered_children {parent} {
  return [db eval {select id, x, code from nodes where parent_node = $parent order by x}]
}

proc ::xc::walk_tree_starting_from {start_node_id} {



  set zmulti $start_node_id

  set num_loops 1

  set FROZEN $start_node_id


  #########
  while 1 {
  if {$num_loops >= 1000} {error "breaking OUTER loop"}



  set p [lindex $zmulti 0]
  set zmulti [lrange $zmulti 1 end]
  if {$p eq ""} break
  set ordered_childs [::xc::walk_tree_aux_ordered_children $p]

  set inner_zmulti {}

  set RAGE {}

  ::xc::walk_tree_debug "\nchilds of $p"
  foreach {node_id x code} $ordered_childs {

    set num_childs [::xc::num_childs_of_node $node_id]
    if {$num_childs == 0} {set node_type single} else {set node_type multi}

    if {$node_type eq "multi"} {
      lappend inner_zmulti $node_id
    }

    ::xc::walk_tree_debug "  $node_id -> $num_childs childs   (>[string trim $code]<)"

    lappend RAGE $node_id

  }

  set zmulti [concat $inner_zmulti $zmulti]


  #puts FROZEN:$FROZEN
  #puts RAGE:$RAGE

  set ICE {}
  foreach el $FROZEN {
    if {$el eq $p} {
      lappend ICE $p
      foreach r $RAGE {lappend ICE $r}
    } else {
      lappend ICE $el
    }
  }
  set FROZEN $ICE


  #puts FROZEN:$FROZEN



  incr num_loops

  }
  ##############



  #puts -->$zmulti


  ::xc::walk_tree_debug "\nFROZEN:"
  foreach node_id $FROZEN {
    set code [db one {select code from nodes where id=$node_id}]
    ::xc::walk_tree_debug "  $node_id   >[string trim $code]<"
  }
  ::xc::walk_tree_debug "\n"



  return $FROZEN
}

set XC_WALK_TREE_DEBUG 0
proc ::xc::walk_tree_debug {msg} {
  if {!$::XC_WALK_TREE_DEBUG} return
  puts $msg ; flush stdout
}

proc ::xc::walk_tree_give_accum_code {start_node_id} {

  set total ""
  set accum_nodes [::xc::walk_tree_starting_from $start_node_id]
  foreach node_id $accum_nodes {
    set code [string trim [db one {select code from nodes where id=$node_id}]]
    if {$code ne ""} {append total \n\n$code}
  }
  if {$total ne ""} {append total \n\n}  ;# trailing newlines

  ::xc::walk_tree_debug ACCUM>>>>$total<<<<ACCUM

  return $total
}


proc ::xc::eval_code_from_root {} {

  puts "\n[now] evaling..."
  set code [::xc::walk_tree_give_accum_code -99]
  uplevel #0 [list eval $code]

}

proc ::xc::make_experimental_file {} {

  global PROGRAM_NAME  
  global MAIN_X
  global MAIN_BACKUPS_DIR


  set timestamp [now]
  set code [::xc::walk_tree_give_accum_code -99]
  
  set c ""
  append c "\# $timestamp HIYA"
  append c "\n\n"
  append c $code


  file_save $MAIN_X $c


  make_sure_backup_dir_exists
  set easy_diffing [file join $MAIN_BACKUPS_DIR $PROGRAM_NAME.For-Easy-Diffing.x]
  if {[file exists $easy_diffing]} {file delete $easy_diffing}
  eralon_copyfile $MAIN_X $easy_diffing

}



proc ::xc::new_experimental_process {} {

  global MAIN_EXE

  ::xc::make_experimental_file

  winutils::launch -directory [file dirname $MAIN_EXE] $MAIN_EXE

}

# 0 to n-1
proc iel_randn {n} {expr {int(rand() * $n)}}

proc iel_randcolor {} {format #%06x [expr {int(rand() * 0xFFFFFF)}]}

proc iel_randlist {L} {lindex $L [iel_randn [llength $L]]}



proc iel_shape_ell {img fg x y} {
  $img put $fg -to $x $y [expr $x+16] [expr $y+4]
  $img put $fg -to $x $y [expr $x+4]  [expr $y+16]
}

proc iel_shape_sqr {img fg x y} {
  $img put $fg -to $x $y [expr $x+8] [expr $y+8]
}

proc iel_paint_bg {img bg} {
  $img put $bg -to 0 0 40 40
}


proc iel_gen_paint_spec {} {

  set num_drawn_shapes 10

  set z {}
  for {set i 0} {$i < $num_drawn_shapes} {incr i} {
    set shape [iel_randlist {ell sqr}]
    set x [iel_randn 40] ; set y [iel_randn 40]
    lappend z $shape $x $y
  }

  return $z
}

proc iel_gen_full_icon_spec {} {
  set paint_spec [iel_gen_paint_spec]
  set fg [iel_randcolor] ; set bg [iel_randcolor]
  return [list [list $fg $bg] $paint_spec]
}


proc iel_paint_icon_given_spec {img full_icon_spec} {

  # get fg, bg, paint_spec
  foreach {fg bg} [lindex $full_icon_spec 0] break
  set paint_spec [lindex $full_icon_spec 1]

  iel_paint_bg $img $bg

  foreach {shape x y} $paint_spec {
    set shape_cmd iel_shape_$shape
    $shape_cmd $img $fg $x $y
  }

  return
}


proc iel_paint_icon_new_spec {img} {
  set spec [iel_gen_full_icon_spec]
  iel_paint_icon_given_spec $img $spec
  return $spec
}



proc iel_paint_border {img bdcolor} {

  foreach {x1 y1 x2 y2} {

     0  0  40  1

    39  0  40 40

     0 39  40 40

     0 39   1  1

  } {
    $img put $bdcolor -to $x1 $y1 $x2 $y2
  }

  return
}





if 0 {

pack [label .x_label -border 0 -bg gray]
. conf -bg black

set img [image create photo -height 40 -width 40]
.x_label conf -image $img

bind all <F3> {
  iel_paint_icon_new_spec $img
  iel_paint_border $img white
}


}

if 0 {

# this whole things works great, keep as is for later hacking fun

package require muzic

set default_sf2  [file join $dir sf2 default.sf2]
set enigma_sf2   [file join $dir sf2 enigma.SF2]
set hsvox_sf2    [file join $dir sf2 {HS vox.sf2}]

muzic::soundfont $default_sf2
muzic::channel 0 6   ;# haunting organ
muzic::channel 1 45  ;# soothing drum 

# for some reason this channel doesn't get initialized!
muzic::soundfont $enigma_sf2
muzic::channel 2 0   ;# apache soso

muzic::soundfont $hsvox_sf2
muzic::channel 3 10  ;# shuddering


muzic::playnote 0 60 100



proc scan_sf2 {fn} {
  if {[muzic::soundfont $fn] eq "-1"} {error "can't load sf2 file >$fn<"}

  set reserved_scan_chan 15

  for {set i 0} {$i < 1000} {incr i} {
    set load [muzic::channel $reserved_scan_chan $i]
    if {$load eq "0"} {
      # found one
      puts "$i valid"
      p 15 55 1000  ;# play that tune @ 55 pitch for 1000 ms
      pn03_pause 1200
    }
  }

  return
}



# cool prefix for multis?  -> v 66 42 42 66

# song  -> v 44 44 44 90 90 90 66 42 40 40 42 66 42 42




proc p {chan pitch duration} {
  set volume 100
  muzic::playnote $chan $pitch $volume $duration
  return
}


set v_chan     1
set v_duration 500

proc v args {
  set count 0
  foreach el $args {
    incr count
    after [expr $count * $::v_duration] [list p $::v_chan $el $::v_duration]
  }
}


# same as RandomInteger4
proc ri {min max} {
  return [expr {int(rand()*($max-$min+1)+$min)}]
}
proc ri3 {} {ri 30 39}
proc ri4 {} {ri 40 49}
proc ri5 {} {ri 50 59}
proc ri6 {} {ri 60 69}
proc ri7 {} {ri 70 79}

# twin tones
proc tt {} {
  set a [ri 3 7]
  set b $a ; while {$b eq $a} {set b [ri 3 7]}
  set tones [list v [ri$a] [ri$a] [ri$b] [ri$b]]
  puts $tones
  eval $tones
}

# random tones
proc rt {n} {
  set tones {v}
  for {set i 0} {$i < $n} {incr i} {lappend tones [ri 30 70]}
  puts $tones
  eval $tones
}


proc vtl {args} {
  set duration 0
  foreach {tone +duration} $args {
    set ai [after $duration [list p $::v_chan $tone ${+duration}]]
    puts $ai
    incr duration ${+duration}
  }
  puts "total duration: $duration"
}

proc rtl {n} {
  set z {vtl}
  for {set i 0} {$i < $n} {incr i} {lappend z [ri 30 70] [ri 100 250]}
  puts $z
  eval $z
}

# vtl 44 192 50 204 44 2000

# vtl [ri 50 70] [ri 200 2000] [ri 50 70] [ri 200 2000] [ri 50 70] [ri 200 2000] [ri 50 70] [ri 200 2000]











set xtl_playing {}
set xtl_aiz {}

proc xtl_noteon {chan tone velocity} {

  global xtl_playing

  #puts "XTL NOTEON $chan $tone"
  ::muzic::noteon $chan $tone $velocity

  set h [list $chan $tone]
  if {[lsearch -exact $xtl_playing $h] == -1} {
    lappend xtl_playing $h
  }

}

proc xtl_noteoff {chan tone} {

  global xtl_playing

  #puts "XTL NOTEOff $chan $tone"
  ::muzic::noteoff $chan $tone

  set h [list $chan $tone]
  if {[set i [lsearch -exact $xtl_playing $h]] != -1} {
    set xtl_playing [lreplace $xtl_playing $i $i]
  }  
  
}

proc xtl_note_reset {} {

  global xtl_playing

  # stop all playing tones
  foreach el $xtl_playing {
    foreach {chan tone} $el break
    xtl_noteoff $chan $tone
    puts "stopped $chan $tone"
  }

}

# exclusive play, args is a flat list of tone/duration
proc xtl {args} {

  puts XTL-begin

  global xtl_aiz


  # cancel all remaining events for the last xtl tune
  foreach el $xtl_aiz {
    after cancel $el
    #puts "cancelled $el"
  }
  set xtl_aiz {}



  xtl_note_reset


  set chan $::v_chan

  # volume (muzic library code refers to it as velocity)
  set internal_velocity 100  ;# integer in the range of 0 to 100
  set noteon_velocity [expr {int($internal_velocity*127/100)}]

  set commz {}

  set pos 0
  foreach {tone duration} $args {
    #puts ""
    set timeOn $pos
    set timeOff [expr $pos + $duration]
    #puts "On:  after $timeOn ($tone)"
    #puts "Off: after $timeOff ($tone)"    
    lappend commz [list $timeOn  [list xtl_noteon  $chan $tone $noteon_velocity]]
    lappend commz [list $timeOff [list xtl_noteoff $chan $tone]]
    incr pos $duration
  }
  puts "total duration: $pos"

  puts ""

  foreach el $commz {
    foreach {time cmd} $el break
    set ex [list after $time $cmd]
    set ai [eval $ex]
    lappend xtl_aiz $ai
    puts "id: $ai -> $ex"
  }


  puts XTL-end

  return
}






# given a total length of time, and 4 tones, give each tone a random slice of
# the total time

proc xr {} {

  set a1 [ri 1 10]
  set a2 [ri 1 10]
  set a3 [ri 1 10]
  set a4 [ri 1 10]

  set atot [expr ($a1 + $a2 + $a3 + $a4) * 1.0]

  set p1 [expr $a1 / $atot]
  set p2 [expr $a2 / $atot]
  set p3 [expr $a3 / $atot]
  set p4 [expr $a4 / $atot]

  foreach p [info vars p*] {puts "$p -> [set $p]"}

  set dtotal 600

  set code {xtl}
  foreach p [list $p1 $p2 $p3 $p4] {
    set len [expr int($p * $dtotal)]
    lappend code [ri 50 70] $len
  }

  puts $code
  eval $code

  interp alias {} xe {} eval $code  ;# shortcut to redo

  return
}







} ;# if 0

set xtl_playing {}
set xtl_aiz {}

set xtl_chan 1

proc xtl_is_initialized {} {return 0}  ;# this will get overriden when it is initialized

proc xtl_init_soundfonts_and_channels {} {

global dir

set xtl_sf2_default  [file join $dir sf2 default.sf2]
set xtl_sf2_enigma   [file join $dir sf2 enigma.SF2]
set xtl_sf2_hsvox    [file join $dir sf2 {HS vox.sf2}]


# now im only initializing the one i use, so it may speed up this proc

muzic::soundfont $xtl_sf2_default
##muzic::channel 0 6   ;# haunting organ
muzic::channel 1 45  ;# soothing drum 


# for some reason this channel doesn't get initialized!
##muzic::soundfont $xtl_sf2_enigma
##muzic::channel 2 0   ;# apache soso


##muzic::soundfont $xtl_sf2_hsvox
##muzic::channel 3 10  ;# shuddering

proc xtl_is_initialized {} {return 1}  ;# override

} ;# end proc

proc xtl_noteon {chan tone velocity} {
  if {![xtl_is_initialized]} return

  global xtl_playing

  #puts "XTL NOTEON $chan $tone"
  ::muzic::noteon $chan $tone $velocity

  set h [list $chan $tone]
  if {[lsearch -exact $xtl_playing $h] == -1} {
    lappend xtl_playing $h
  }

}

proc xtl_noteoff {chan tone} {
  if {![xtl_is_initialized]} return

  global xtl_playing

  #puts "XTL NOTEOff $chan $tone"
  ::muzic::noteoff $chan $tone

  set h [list $chan $tone]
  if {[set i [lsearch -exact $xtl_playing $h]] != -1} {
    set xtl_playing [lreplace $xtl_playing $i $i]
  }  
  
}

proc xtl_reset {} {

  xtl_reset_aiz
  xtl_reset_playing

}

proc xtl_reset_aiz {} {

  global xtl_aiz

  # cancel all remaining events for the last xtl tune
  foreach el $xtl_aiz {
    after cancel $el
    #puts "cancelled $el"
  }
  set xtl_aiz {}

}

proc xtl_reset_playing {} {

  global xtl_playing

  # stop all playing tones
  foreach el $xtl_playing {
    foreach {chan tone} $el break
    xtl_noteoff $chan $tone
    #puts "stopped $chan $tone"
  }

}

# exclusive play, args is a flat list of tone/duration
proc xtl_play {args} {

  #puts XTL-begin

  global xtl_aiz
  global xtl_chan

  xtl_reset

  set chan $xtl_chan

  # volume (muzic library code refers to it as velocity)
  set internal_velocity 100  ;# integer in the range of 0 to 100
  set noteon_velocity [expr {int($internal_velocity*127/100)}]

  set commz {}

  set pos 0
  foreach {tone duration} $args {
    #puts ""
    set timeOn $pos
    set timeOff [expr $pos + $duration]
    #puts "On:  after $timeOn ($tone)"
    #puts "Off: after $timeOff ($tone)"    
    lappend commz [list $timeOn  [list xtl_noteon  $chan $tone $noteon_velocity]]
    lappend commz [list $timeOff [list xtl_noteoff $chan $tone]]
    incr pos $duration
  }
  #puts "total duration: $pos"

  #puts ""

  foreach el $commz {
    foreach {time cmd} $el break
    set ex [list after $time $cmd]
    set ai [eval $ex]
    lappend xtl_aiz $ai
    #puts "id: $ai -> $ex"
  }


  #puts XTL-end

  return
}

# same as RandomInteger4  --  returns number from [min,max]
proc xtl_ri {min max} {
  return [expr {int(rand()*($max-$min+1)+$min)}]
}

# returns flat list of tones/duration ready for input to xtl_play
# call like this:  eval xtl_play [xtl_random]
#
proc xtl_random {} {

  set a1 [xtl_ri 1 10]
  set a2 [xtl_ri 1 10]
  set a3 [xtl_ri 1 10]
  set a4 [xtl_ri 1 10]

  set atot [expr ($a1 + $a2 + $a3 + $a4) * 1.0]

  set p1 [expr $a1 / $atot]
  set p2 [expr $a2 / $atot]
  set p3 [expr $a3 / $atot]
  set p4 [expr $a4 / $atot]

  #foreach p [info vars p*] {puts "$p -> [set $p]"}

  set dtotal 600

  set code {}
  foreach p [list $p1 $p2 $p3 $p4] {
    set len [expr int($p * $dtotal)]
    lappend code [xtl_ri 50 70] $len
  }

  return $code
}

#make_icons_for_nodes_that_have_none
#make_music_for_nodes_that_have_none

bind all <F4> {
  puts "Images Count: [llength [image names]]"
}

if 0 {


# make a node in the database
set id node_[gen_random_string 10]_[clock seconds] ; db eval {insert into nodes values ($id, 150, 30, "", -99)}



}

puts "now initializing xc"
update
::xc::init


puts "now initializing soundfonts and channels"
update
xtl_init_soundfonts_and_channels


puts "ready!"

