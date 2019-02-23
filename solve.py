#!/usr/bin/env python3

import os
import subprocess
import sys
import threading
import time

loc = os.path.dirname(os.path.abspath(__file__))
asp_path = os.path.join(loc,'asp')

def handlestderr(inhandle,outfile):
    with open(outfile,'w') as outhandle:
        for line in inhandle:
            print(time.time(),':',line.decode(),file=outhandle,end='')

def main(args,outhandle=sys.stdout,errfile=None):
    proc = subprocess.Popen(
        [loc + '/.stack-work/install/x86_64-linux/custom-dspies-snapshot-7-ZAyK3hVJpru7/8.6.3/bin/mcp-exe'] + args,
        stdout=subprocess.PIPE,
        stderr=(errfile and subprocess.PIPE),
        env={'ASP_PATH' : asp_path}
        )
    try:
        if errfile is not None:
            threading.Thread(target=handlestderr,args=(proc.stderr,errfile)).start()
        print(proc.stdout.read().decode(),file=outhandle)
    finally:
        proc.kill()

if __name__ == '__main__':
    main(sys.argv[1:])
