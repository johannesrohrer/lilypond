#!/usr/bin/python
import sys
import os
import os.path
import shutil

dirs = ['ancient','chords','connecting','contemporary','expressive','guitar','parts','repeats','scheme','spacing','staff','text','vocal']
notsafe=[]

try:
	in_dir = sys.argv[1]
except:
	print "Please specify input_file."
	sys.exit()

def copy_with_warning(src, dest):
	msg = '%%  Do not edit this file; it is auto-generated from LSR!\n'
	open (dest, 'w').write( msg + open (src).read() )


def copy_dir_with_test(srcdir, destdir):
	if not(os.path.exists(srcdir)):
		return
	file_names = os.listdir (srcdir)
	for file in file_names:
		if (file.endswith ('.ly')):
			src = os.path.join (srcdir, file)
			dest = os.path.join (destdir, file)
			copy_with_warning(src, dest)
			s = os.system('lilypond -dsafe -dbackend=svg -o /tmp/lsrtest ' + dest)
			if s:
				notsafe.append(dest)


for dir in dirs:
	srcdir = os.path.join (in_dir, dir)
	destdir = os.path.join ('input', 'lsr', dir)
	if not(os.path.isdir(destdir)):
		print "Please run this script from the head of the source tree,"
		print "  and/or check that you have the right categories."
		sys.exit()

	## clean out existing files
	file_names = os.listdir (destdir)
	for file in file_names:
		if (file.endswith ('.ly')):
			if (file[:3] != 'AAA'):
				os.remove( os.path.join(destdir,file) )
	## copy in new files from LSR download
	copy_dir_with_test( srcdir, destdir )
	## copy in new files in source tree
	copy_dir_with_test( os.path.join ('input', 'tolsr', dir), destdir )


file=open("lsr-unsafe.txt", 'w')
for s in notsafe:
	file.write(s+'\n')
file.close()
print
print
print "Unsafe files printed in lsr-unsafe.txt: CHECK MANUALLY!"
print "  xargs git-diff < lsr-unsafe.txt"
print

