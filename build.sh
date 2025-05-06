cd ./cobol_webserver
export LD_LIBRARY_PATH=.:$LD_LIBRARY_PATH
gcc -shared -fPIC -o librouter_wrapper.so router_wrapper.c -fvisibility=default
export LD_LIBRARY_PATH=.:$LD_LIBRARY_PATH
cobc -x -free -o webserver webserver.cbl -L. -lrouter_wrapper -lc
