#!/usr/bin/python
import os, sys, subprocess
FNULL=open(os.devnull,"w")


def showParameters(n):
            cmd = 'uvcdynctrl -d /dev/video' + n + ' -c'
            p = subprocess.Popen(cmd, shell=True, stdout=subprocess.PIPE, stderr=FNULL)
            for line in p.stdout.readlines()[1:]:
                        par = line[2:-1]
                        if ( par.startswith('Restore') or
                             par.startswith('Save') or
                             par.startswith('Pan') or
                             par.startswith('Tilt') ):
                                    continue
                        getcmd = "uvcdynctrl -d /dev/video" + n + " -g '" + par + "'"
                        p2 = subprocess.Popen(getcmd, shell=True, stdout=subprocess.PIPE, stderr=FNULL)
                        for line in p2.stdout.readlines():
                                    print par + " : ", line,
                        retval = p2.wait()
            retval = p.wait()



if __name__ == "__main__" :
            if (len(sys.argv) > 1):
                        showParameters(sys.argv[1])
            else :
                        showParameters('1')
