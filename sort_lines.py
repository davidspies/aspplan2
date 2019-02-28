#!/usr/bin/env python3

import sys

def main(filename):
    lines = open(filename, 'r').read().strip().split('\n')
    lines.sort()
    with open(filename, 'w') as f:
        for line in lines:
            f.write(line + '\n')

if __name__ == '__main__':
    main(*sys.argv[1:])
