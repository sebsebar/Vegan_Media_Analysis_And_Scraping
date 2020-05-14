# -*- coding: utf-8 -*-
#!/usr/bin/python
"""
Created on Mon Dec  4 19:11:02 2017

@author: Sebastian
"""


from itertools import zip_longest

def grouper(n, iterable, fillvalue=None):
    "Collect data into fixed-length chunks or blocks"
    # grouper(3, 'ABCDEFG', 'x') --> ABC DEF Gxx
    args = [iter(iterable)] * n
    return zip_longest(fillvalue=fillvalue, *args)

n = 50

with open('2016-2017_jp_veg.txt') as f:
    for i, g in enumerate(grouper(n, f, fillvalue=''), 1):
        with open('article{0}.txt'.format(i), 'w') as fout:
            fout.writelines(g)
            