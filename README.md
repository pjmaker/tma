# tma

A time series data analysis display and analysis tool orginally
designed for reporting on power systems. One application is 
within the ASIM modelling framework (http://githhub.com/ThinkOfaNumber/Asim).
Its typically used to:

* Display real time data which is normally timestamped rather than 
sampled at a fixed rate.
* Analyse and generate new time series, e.g. a value smoothed with
a ramp filter.
* And to generate PDF based reports of the final results, typically
for archival.

The following data formats are supported:

* OSIsoft PI - from a server via the included .exe files.
* ''*.smst'' files - files from OSIsoft PI SMT tools export
* ''*.pi'' files - raw OSIsoft PI files extracted by PITool.exe
* ''*.csv'' in ASIM format  
* ''*.csv'' in Powercorp SMStrends export format
* ''*.nc'' in ACEP NetCDF format

The preferred format is ''*.csv'' using ASIM format which 
looks:

''''
t,Gen1P
100,10
110.4,12
190,7
''''

All of the above are scriptable in TCL (http://wiki.tcl.tk).

In order to install you need one of the runtimes such as
<http://github.com/pjmaker/tma-unix-rt> or
<http://github.com/pjmaker/tma-windoze-rt>. 

