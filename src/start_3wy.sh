#!/bin/sh

./bin/wysteria --i-am \!Alice --gmw-port 9000 examples/$1.wy &
./bin/wysteria --i-am \!Bob --gmw-port 9001 examples/$1.wy &
./bin/wysteria --i-am \!Charlie --gmw-port 9002 examples/$1.wy &
