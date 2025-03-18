export LD_LIBRARY_PATH=.:$LD_LIBRARY_PATH
gcc -shared -fPIC -o librouter_wrapper.so router_wrapper.c -fvisibility=default
cobc -x -free webserver.cbl -L. -lrouter_wrapper -lc

