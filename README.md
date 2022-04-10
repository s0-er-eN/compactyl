# Compactyl - A Compact Dactyl ManuForm Keyboard

This is a fork of the [dereknheiley_fork](https://github.com/dereknheiley/compactyl) of [okke fork](https://github.com/okke-formsma/dactyl-manuform-tight) of the [l4u fork](https://github.com/l4u/dactyl-manuform-mini-keyboard) of the [tshort fork](https://github.com/tshort/dactyl-keyboard) of the [Dactyl](https://github.com/adereth/dactyl-keyboard), a parameterized, split-hand, concave, columnar, ergonomic keyboard.

## Features/Improvements in this fork/branch
- Remove the last row having 2 keys via `last_row_with_2_keys`
- Add your own text to the case/bottom via `bottom_plate_text`/`case_text`
- Add cap-cutout-handlers in `cap-cutout-handler`
- Customize cap cutouts for each position via `sa-cap-cutout-mod-generic` (see `cap-cutout-handler-3x5-no-2-keys-last-row`)
- Reduce hotswap case cutout to prevent gaps in walls during FDM printing (see `key-places` and `hotswap-case-cutout-less-left`)
- Closed some wall gaps
- Control `use_hotswap_led_cutout`
- Control `use_hotswap_friction_holes`
- Control `use_switch-dogbone-cutout`

## V3.1 Features
- Added support for tilting outer index and pinky columns only
- Increased tent angle to 40 deg
- Adjusted case to remove jank around top index corner
- Changed to use vertical MCU holder
- Upsized top plate attachment screws from M2 to M3
- Modified case walls connecting to thumb
- Updated rendered thumb caps to rotated SA R1, which I use.
![Compactyl V3.1 top](images/compactyl-v3.1.png)
![Compactyl V3.1 top](images/compactyl-v3.1-back.png)

## V3 Features
- Added option for two part design per keyboard half
![Compactyl V3 back](images/compactyl-V3-exploded.png)

## V2.1 Features
- Increased tent angle
	- Added global thumb angle adjustment
	- Adjusted case to still accommodate usb-holder with increased tent
- Reworked gel wrist rest holder to not distort at higher tenting angles
- Reinforced connection between columns with cutouts for connector / wiring clearance
- Added option to increase angle of lower two lower keys (inspired by rmtz Naive design)
- Added rendered option for keycap preview
- (Experimental) Added optional retention clip to assist with one-piece flexible PCB
- (Experimental) Added optional solderless socket option
- NOTE: you will probably have to increase OpenSCAD params:
	- Turn off rendering at ~2,000,000 
	- CGAL cache size ~4000mb
	- PolySet cache size ~4000mb
![Compactyl V2](images/compactyl-V2.1.png)
![Compactyl V2 back](images/compactyl-V2.1-back.png)

## V2 Features
- Returned back to one solid piece top case deisgn for easier printing
- Subtraced space around keycaps to improve clearances
	- better clearance on top row for middle and ring finger keys
	- thick ABS caps still not recommended
- Adjusted stagger and column height deltas
- Extended bottom plate to connect wrist rest
	- Wrist rest can be positioned to adjust X, Y, or Z placement (adding or removing shims)
- Added modified version of cyrstalhands wrist rest holder
	- slanted, removed case connector tubes, added threaded insert holes
![Compactyl V2](images/compactyl-V2.png)

## V1 Features
- Hotswap north facing switch plates
- Increase pinky stagger
- Adjusted column spacing for better clearance
- Continuous (vs. recessed) bottom plate with filleted holes
- Exoskeleton top case design
- Rendered both 4x6 and 5x6 versions with bottom plate
- (orig. Tightyl) As small around the keys as possible
- (orig. Tightyl) Smoother transition between thumb and fingers (less facets)
- (orig. Tightyl) Thicker walls in steep regions where walls were too thin

![](images/compactyl-real-5x6-glamour.jpeg)
![](images/compactyl.png)
![](images/assembly.png)
[![Kailh Box White Sound Test](images/compactyl-real-4x6.jpg)](https://www.youtube.com/watch?v=2oHvrcsFb9k)
![](images/compactyl-real-5x6.jpeg)

## Update USB holder
- Created ("working" but unpolished) SCAD version of USB holder for modifications
- Provide better clearance to USB cables plugging into controller
- Adjust size to better hold Elite-C controller
- Taper angle to reduce waste material
- Make holder shape symetrical to case notches so it fits upside down or right side up
- Add option for square reset button to be held as well
![](images/elite-c_trrs_holder.png)
![](images/elite-c_trrs_reset_holder.png)
![](images/elite-c_trrs_reset_holder_real.jpeg)

## Wiring
- Uses typical COL2ROW diode direction and pins for dactyl manuform
- Should be backwards compatible with dactyl-manuform 5x6 firmware from confrom
	- https://config.qmk.fm/#/handwired/dactyl_manuform/5x6/LAYOUT_5x6
	- https://ohkeycaps.com/pages/via-support
![](images/compactyl_dactyl_manuform_wiring_left.jpeg)
![](images/compactyl_dactyl_manuform_wiring_right.jpeg)
![](images/compactyl-real-4x6-wiring.jpeg)

## Comparison with previous model
- Orig. Tightyl (white) had more row curvature, and wider spaceing between columns
- New Compactyl (blue) uses a flatter row curvature, but increases tent to result in similar keyboard angle
- New Compactyl (blue) is approximately 4mm taller (plus additional heigh for bottom plate if used) because of space requried by hot swap holders below pinky columns.
![](images/compare.png)


## Generate OpenSCAD and STL models

**Setting up the Clojure environment**
* [Install the Clojure runtime](https://clojure.org)
* [Install the Leiningen project manager](http://leiningen.org/)
* [Install OpenSCAD](http://www.openscad.org/)

### Generating a Design
* Run `lein auto generate`
* This will regenerate the `things/*.scad` files whenever the .clr file is saved
* Use OpenSCAD to open a `.scad` file.
* Make changes to design in `src/dactyl_keyboard/dactyl.clj`, open scad files will auto regenerate, OpenSCAD will rerender.
* When done, use OpenSCAD to render, then export model STL files which can be printed by 3d printer slicing software.

### Batch (parallel) Processing
* Edit the path for OpenSCAD in `create-models.sh` if needed
* Change any other settings for which files you want to render
* Wait for STL files to appear (this may take a minute or two) 

## Tips

* I gave up trying to keep good values for both 4x6 and 5x6 options. So when adjusting the number of rows / cols, or tenting angles, (amoung other larger design changes) you will most likely have to update some magic numbers. I recommend opening the test.scad file and manipulate:
	* defn column-curvature 
	* defn centerrow
	* def centercol
	* defn column-offset
	* def keyboard-z-offset
	* def wrist-rest-z-height-adj
	* defn usb-holder-offset-coordinates
	* defn screw-insert-all-shapes
* When trying things out, 10 seconds of rendering time in OpenSCAD is really annoying. Load one of the test outputs with commented out parts that you aren't changing / don't use.
* If you're not sure what things are generted by a piece of code, color them in using something like
`(->> SOMETHING_HERE (color RED))` (see examples in dactyl.clj).

## License

Copyright © 2015-2020 Matthew Adereth, Tom Short, Leo Lou, Okke Formsma, Derek Nheiley

The source code for generating the models is distributed under the [GNU AFFERO GENERAL PUBLIC LICENSE Version 3](LICENSE).

The generated models are distributed under the [Creative Commons Attribution-ShareAlike 4.0 International (CC BY-SA 4.0)](LICENSE-models).
