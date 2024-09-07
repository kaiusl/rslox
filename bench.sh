cargo build --release

echo "equality"
./target/release/rslox -f ./crafting_interpreters_test_files/benchmark.ignore/equality.lox

echo ""
echo "fib"
./target/release/rslox -f ./crafting_interpreters_test_files/benchmark.ignore/fib.lox


echo ""
echo "string_equality"
./target/release/rslox -f ./crafting_interpreters_test_files/benchmark.ignore/string_equality.lox
