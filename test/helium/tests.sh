date > test.log

#make the compiler
echo make the compiler >> test.log
echo make the compiler
cd ../../src/lib
make > test.out 2> test.out
cat test.out
cd ../../test/helium

#make the system
echo make the system
cd ../../src/runtime
make > test.out 2> test.out
cat test.out
cat test.out >> test.log 
cd ../../test/helium

test.sh LvmLang
test.sh LvmIO
test.sh LvmException
test.sh HeliumLang
test.sh PreludePrim