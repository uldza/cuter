#!/bin/bash
grep vsn src/cuter.app.src | awk '{print $2}' | sed 's/\"},//' | sed 's/"//'
