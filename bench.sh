cargo build --release

echo "equality"
for v in {1..3}
do
./target/release/rslox -f ./crafting_interpreters_test_files/benchmark.ignore/equality.lox
done

echo ""
echo "fib"
for v in {1..3}
do
./target/release/rslox -f ./crafting_interpreters_test_files/benchmark.ignore/fib.lox
done

echo ""
echo "string_equality"
for v in {1..3}
do
./target/release/rslox -f ./crafting_interpreters_test_files/benchmark.ignore/string_equality.lox
done