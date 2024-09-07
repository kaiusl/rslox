cargo build --release

for path in "equality" "fib" "string_equality"
do 
    echo ""
    echo $path
    for ((i=0; i<${1:-1}; i++))
    do
        ./target/release/rslox -f ./crafting_interpreters_test_files/benchmark.ignore/$path.lox
    done
done
