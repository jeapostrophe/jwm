#!/bin/sh
for i in bin/* ; do
    ln -sf $(pwd)/${i} ~/${i}
done
