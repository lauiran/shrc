#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import dujitang
import sys
import random
from time import time

def cowsayorthink(bbt):
    if bbt == 'think':
        ch = 'o'
    else:
        ch = '\\'

    return ('        %s   ^__^\n'
            '         %s  (oo)\\_______\n'
            '            (__)\\       )\\/\\\n'
            '                ||----w |\n'
            '                ||     ||\n' %(ch, ch))


def strwidth(s):
    hansnum = (len(s.encode()) - len(s)) // 2
    charnum = len(s) - hansnum
    sw = charnum + hansnum * 5 // 3
    sw = charnum + hansnum * 2
    return sw

def getlen(bb, ll):
    if len(bb) == len(bb.encode()):
        if len(bb) > ll:
            return bb[:ll]
        else:
            return bb

    for i in range(1, len(bb)):
        bbp = bb[:i]
        bbl = strwidth(bbp)
        diff = strwidth(bbp[-1:]) // 2
        if bbl >= (ll-diff) or i == len(bb):
            return bbp

    return bb[:ll]


def fill_blank(bb, maxlen):
    bbl = strwidth(bb)

    if bbl < maxlen:
        return bb + ' ' * (maxlen - bbl)

    return bb


def content(bb, maxlen, bbt):
    if bbt == 'think':
        ch = '('
        ch2 = ')'
        ch3 = '('
        ch4 = ')'
        ch5 = '('
        ch6 = ')'
        ch7 = '('
        ch8 = ')'
    else:
        ch = '/'
        ch2 = '\\'
        ch3 = '|'
        ch4 = '|'
        ch5 = '\\'
        ch6 = '/'
        ch7 = '<'
        ch8 = '>'

    maxlen = strwidth(getlen(bb, maxlen))
    above = ' ' + '_' * (maxlen + 2) + '\n'
    foot = ' ' + '-' * (maxlen + 2) + '\n'

    start = 0
    cont = list()
    cont.append(above)
    while start < len(bb):
        bbp = getlen(bb[start:], maxlen)
        bbl = len(bbp)
        if start == 0:
            if start + bbl >= len(bb):
                chs = ch7
                che = ch8
            else:
                chs = ch
                che = ch2
        elif start + bbl >= len(bb):
            chs = ch5
            che = ch6
        else:
            chs = ch3
            che = ch4
        bbp = fill_blank(bbp, maxlen)
        cont.append('%s %s %s\n' %(chs, bbp, che))
        start += bbl

    cont.append(foot)

    return ''.join(cont) + cowsayorthink(bbt)


class cowbb:
    def __init__(self, width=39, cont=''):
        self.width = width
        self.content = cont

    def cow_say(self):
        return content(self.content, self.width, 'say')

    def cow_think(self):
        return content(self.content, self.width, 'think')

if __name__ == '__main__':
    bb = ''
    bb2 = ''
    nbb = dujitang.dujitang()

    if len(sys.argv) < 2 or sys.argv[1] != '-n':
        bb = input()
        bb2 = bb
    if len(bb) == 0:
        #bb = nbb.get_sentence_dry()
        bb = nbb.get_djt()
        bb2 = nbb.get_nhw()
    random.seed(time())
    if 0 == random.choice([0,1]) :
        print(cowbb(cont=bb).cow_say())
    else :
        print(cowbb(cont=bb2).cow_think())

