cargo build --release

for path in "binary_trees" "equality" "fib" "instantiation" "invocation" "method_call" "properties" "string_equality" "trees" "zoo_batch" "zoo"
do 
    echo ""
    echo $path
    echo ""
    for ((i=0; i<${1:-1}; i++))
    do
        ./target/release/rslox -f ./crafting_interpreters_test_files/benchmark.ignore/$path.lox
        #./clox ./crafting_interpreters_test_files/benchmark.ignore/$path.lox
    done
done
