#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import pycow
import dujitang
import os
import time

bb = dujitang.dujitang()
cow = pycow.cowbb()

while True:
    cow.content = bb.get_sentence()
    os.system('cls')
    print(cow.cow_say())
    time.sleep(30)


'''
if __name__ == '__main__':
    test code
'''

