#!/bin/sh

# +++ Amthrax

set -e

rm -rf transtemp
FILES=
for x in *; do FILES="$x $FILES"; done
mkdir transtemp
cp -r $FILES transtemp
cd transtemp
for x in `find -name "*.png"`; do
    composite -dissolve 130 null: $x xc:black -matte $x; 
done

# ---

# This file is better seen using 4 spaces tabs!!

if test ! -d cursors;
then mkdir cursors
else
rm -rf cursors
mkdir cursors
fi

########### Build cursors

#---------- Build static cursors

xcursorgen Arrow.conf		cursors/Arrow
xcursorgen Copy.conf		cursors/Copy
xcursorgen Cross.conf		cursors/Cross
xcursorgen Hand.conf		cursors/Hand
xcursorgen Handwriting.conf	cursors/Handwriting
xcursorgen Help.conf		cursors/Help
xcursorgen IArrow.conf		cursors/IArrow
xcursorgen IBeam.conf		cursors/IBeam
xcursorgen Link.conf		cursors/Link
xcursorgen MiniNS.conf		cursors/MiniNS
xcursorgen MiniWE.conf		cursors/MiniWE
xcursorgen Move.conf		cursors/Move
xcursorgen NO.conf			cursors/NO
xcursorgen SizeAll.conf		cursors/SizeAll
xcursorgen SizeNESW.conf	cursors/SizeNESW
xcursorgen SizeNS.conf		cursors/SizeNS
xcursorgen SizeNWSE.conf	cursors/SizeNWSE
xcursorgen SizeWE.conf		cursors/SizeWE
xcursorgen UpArrow.conf		cursors/UpArrow


#---------- Build animated cursors

cd AppStarting
xcursorgen AppStarting.conf	../cursors/AppStarting

cd ../Wait
xcursorgen Wait.conf		../cursors/Wait

cd ../cursors



########### Create copies and symlinks

#---------- AppStarting
#cp AppStarting			left_ptr_watch
cp Wait				left_ptr_watch
cp -s left_ptr_watch	08e8e1c95fe2fc01f976f1e063a24ccd
cp -s left_ptr_watch	3ecb610c1bf2410f44200f48c40d3599
rm -f AppStarting

#---------- Arrow
cp Arrow				left_ptr
cp Arrow				top_left_arrow
rm -f Arrow

#---------- Copy
cp Copy					copy
cp -s copy				1081e37283d90000800003c07f3ef6bf
cp -s copy				6407b0e94181790501fd1e167b474872
rm -f Copy

#---------- Cross
cp Cross				cross
cp Cross				crosshair
cp Cross				cross_reverse
cp Cross				tcross
rm -f Cross

#---------- Hand
cp Hand					hand
cp Hand					hand1
cp Hand					hand2
cp -s hand1				9d800788f1b08800ae810202380a0822
cp -s hand2				e29285e634086352946a0e7090d73106
rm -f Hand

#---------- Handwriting
cp Handwriting			pencil
rm -f Handwriting

#---------- Help
cp Help					question_arrow
cp -s question_arrow	d9ce0ab605698f320427677b458ad60b
cp -s question_arrow	5c6cd98b3f3ebcb1f9c7f1c204630408
rm -f Help

#---------- IArrow
cp IArrow				arrow
cp IArrow				draft_large
cp IArrow				draft_small
cp IArrow				right_ptr
rm -f IArrow

#---------- IBeam
cp IBeam				xterm
rm -f IBeam

#---------- Link
cp Link					link
cp -s link				3085a0e285430894940527032f8b26df
cp -s link				640fb0e74195791501fd1ed57b41487f
rm -f Link

#---------- MiniNS
cp MiniNS				sb_v_double_arrow
cp -s sb_v_double_arrow	2870a09082c103050810ffdffffe0204
rm -f MiniNS

#---------- MiniWE
cp MiniWE				sb_h_double_arrow
cp -s sb_h_double_arrow	14fef782d02440884392942c11205230
rm -f MiniWE

#---------- Move
cp Move					move
cp -s move				4498f0e0c1937ffe01fd06f973665830
cp -s move				9081237383d90e509aa00f00170e968f
rm -f Move

#---------- NO
cp NO 					crossed_circle
cp NO 					X_cursor
cp -s crossed_circle	03b6e0fcb3499374a867c041f52298f0
rm -f NO

#---------- SizeAll
cp SizeAll				fleur
cp SizeAll				plus
rm -f SizeAll

#---------- SizeNESW
cp SizeNESW				bottom_left_corner
cp SizeNESW				fd_double_arrow
cp SizeNESW				ll_angle
cp SizeNESW				top_right_corner
cp -s fd_double_arrow	fcf1c3c7cd4491d801f1e1c78f100000
rm -f SizeNESW

#---------- SizeNS
cp SizeNS				base_arrow_down
cp SizeNS				based_arrow_down
cp SizeNS				base_arrow_up
cp SizeNS				based_arrow_up
cp SizeNS				bottom_side
cp SizeNS				double_arrow
cp SizeNS				sb_down_arrow
cp SizeNS				sb_up_arrow
cp SizeNS				top_side
cp SizeNS				v_double_arrow
cp -s v_double_arrow	00008160000006810000408080010102
rm -f SizeNS

#---------- SizeNWSE
cp SizeNWSE				bd_double_arrow
cp SizeNWSE				bottom_right_corner
cp SizeNWSE				lr_angle
cp SizeNWSE				top_left_corner
cp -s bd_double_arrow	c7088f0f3e6c8088236ef8e1e3e70000
rm -f SizeNWSE

#---------- SizeWE
cp SizeWE				h_double_arrow
cp SizeWE				left_side
cp SizeWE				right_side
cp SizeWE				sb_left_arrow
cp SizeWE				sb_right_arrow
cp -s h_double_arrow	028006030e0e7ebffc7f7070c0600140
rm -f SizeWE

#---------- UpArrow
cp UpArrow				center_ptr
rm -f UpArrow

#---------- Wait
cp Wait					watch
rm -f Wait


# +++ Amthrax

cd ..
rm -r ../../cursors
mv cursors ../../
cd ..
rm -r transtemp

# ---


########### Done!
echo "Done!"
