echo "### franca -> f_l"
franca examples/default_driver.fr build compiler/other_main.fr -aot=llvm || exit
cp a.out target/f_l

echo "### f_l -> f_ll"
./target/f_l examples/default_driver.fr build compiler/other_main.fr -aot=llvm || exit
cp a.out target/f_ll

echo "### f_l == f_ll ?"
diff target/f_l target/f_ll || exit
echo "### f_l has a working l-backend"

echo "### f_l -> f_lq"
./target/f_l examples/default_driver.fr build compiler/other_main.fr -aot=qbe-exe || exit
cp a.out target/f_lq

echo "### f_lq -> f_lql"
./target/f_lq examples/default_driver.fr build compiler/other_main.fr -aot=llvm || exit
cp a.out target/f_lql

echo "### f_l == f_lql ?"
diff target/f_l target/f_lql || exit
echo "### f_lq has a working l-backend"

echo "### f_lq -> f_lqq"
./target/f_lq examples/default_driver.fr build compiler/other_main.fr -aot=qbe-exe || exit
cp a.out target/f_lqq

# this wont work
echo "### f_lq == f_lqq ?"
diff target/f_lq target/f_lqq || exit
echo "### f_lq has a working q-backend"

