#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import requests
import hash_log
import sys, io, os, re
from os.path import realpath, join

_curpyfiledir = realpath(join(realpath(__file__), '..'))
logfile = join(_curpyfiledir, 'nows_history.txt')

class dujitang:
    source = list()

    def __init__(self):
        self.sentence_log_f = logfile
        self.hash_map = hash_log.hash_map(self.sentence_log_f)

    @classmethod
    def reg_source(cls, src):
        cls.source.append(src)

    def sentence_log(self, sentence):
        with io.open(self.sentence_log_f, 'a', encoding='utf8') as f:
            f.write('%s\n' %(sentence))

    def get_sentence(self, cnt=3):
        sentence = []
        for cnt in range(0, cnt):
            for ss in self.source:
                sentence = ss().get_sentence()
                for m in sentence:
                    if self.hash_map.add_item(m) is True:
                        self.sentence_log(m)
                        return m

        if len(sentence) == 0:
            return ' '
        else:
            return sentence[0]

    def fetch_more(self, cnt=9):
        for i in range(0, cnt):
            self.get_sentence(1)
        self.hash_map.write_back()

    def get_sentence_dry(self):
        m = self.hash_map.get_sentence()
        if m is None:
            self.fetch_more()
            m = self.hash_map.get_sentence()
            if m is None:
                self.hash_map.pos_reset()
                m = self.hash_map.get_sentence()
        self.hash_map.write_back()
        if m is None:
            return ''
        return m


class web_site:
    def __init__(self, _url):
        self.url = _url

    #@abstractmethod
    def reg_sentence(self, text):
        pass

    def get_sentence(self):
        try:
            rsp = requests.get(self.url, timeout=3)
            if rsp.ok is True:
                sentence = self.reg_sentence(rsp.text)
                return sentence
        except requests.exceptions.ConnectTimeout:
            pass
        return list()

@dujitang.reg_source
class nihaowua(web_site):
    def __init__(self):
        self.url = 'https://www.nihaowua.com/home.html'
        web_site.__init__(self, self.url)

    def reg_sentence(self, text):
        r = []
        pat = re.compile('<li><a href="https://www.nihaowua.com/archives.*</li>')
        pat2 = re.compile('<.*?>')
        p1 = pat.findall(text)
        for p in p1:
            p2 = pat2.split(p)
            r = r + p2
        return r

#@dujitang.reg_source
class nows(web_site):
    def __init__(self):
        self.url = 'http://www.nows.fun/'
        web_site.__init__(self, self.url)

    def reg_sentence(self, text):
        reg = re.compile('<span id="sentence".*/span>')
        reg2 = re.compile('<.*?>')
        return [reg2.sub('', m).strip() for m in reg.findall(text)]

#@dujitang.reg_source
class dzy(web_site):
    def __init__(self):
        self.url = 'http://www.dzy.io/dujitang.php'
        web_site.__init__(self, self.url)

    def reg_sentence(self, text):
        reg = re.compile('^"|"$')
        text = reg.sub('', text)
        text = text.encode('raw_unicode-escape').decode('raw_unicode-escape')
        return [text]


if __name__ == '__main__':
    m = dujitang()
    if len(sys.argv) >= 2:
        if sys.argv[1] == '-n':
            m.fetch_more()
    else:
        print('%s' %(m.get_sentence()))

