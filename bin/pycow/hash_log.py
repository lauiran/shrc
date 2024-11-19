#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import io

class hash_map:
    def __init__(self, his_f='nows_history.txt'):
        self._pos = 0
        self._history_file = his_f
        self._sentences = list()
        self._hash_dic = dict()
        self.load_history_file()

    def load_history_file(self):
        with io.open(self._history_file, 'r', encoding='utf8') as f:
            line = f.readline().strip()
            self._pos = int(line)
            for line in f.readlines():
                self.add_item(line.strip())

    def dump(self):
        cnt = 0
        for line in self._sentences:
            cnt += 1
            print('%6d %s' %(cnt, line))

    def write_back(self):
        with io.open(self._history_file, 'w', encoding='utf8') as f:
            f.write('%d\n' %(self._pos))
            for line in self._sentences:
                f.write('%s\n' %(line))

    def add_item(self, m):
        hv = hash(m)
        if hv not in self._hash_dic.keys():
            self._hash_dic[hv] = list()
        if m not in self._hash_dic[hv]:
            self._sentences.append(m)
            self._hash_dic[hv].append(m)
            return True
        return False

    def get_sentence(self):
        if self._pos >= len(self._sentences):
            return None
        m = self._sentences[self._pos]
        self._pos += 1
        return m

    def pos_reset(self):
        self._pos = 0

if __name__ == '__main__':
    hm = hash_map()
    hm.dump()
    print(hm.get_sentence())
    hm.write_back()

