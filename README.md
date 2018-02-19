# GlobeViz
![Alt text](loop.gif "You spin me right round!")

Attempting to reconstruct the visualisations made by Matthew Leonawicz (http://leonawicz.github.io/gc_animation_example/app_traffic_example.html) using my own travels data as network.

## Notes
Uses a "hacked" version of R's inherent mcapply method for parallel processing in order to run on windows. As a warning from the original author, do not attempt to run this script unless you have a lot of RAM and CPU cores, as rendering the underlying raster map takes a lot of resources.