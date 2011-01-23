#! /usr/bin/env python

from datetime import datetime

d = datetime.now()
print d.strftime("%b %d (%a)\n%H:%M:%S")
