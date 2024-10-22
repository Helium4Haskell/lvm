date > test.log

#make the compiler
echo make the compiler >> test.log
echo make the compiler
cd ../../src/lib
make > test.out 2> test.out
cat test.out
cat test.out >> test.log 
cd ../../test/core
echo >> test.log

#make the system
echo make the system >> test.log
echo make the system
cd ../../src/runtime
make > test.out 2> test.out
cat test.out
cat test.out >> test.log 
cd ../../test/core
echo >> test.log

#test all files
for t in *.core; 
{
  test.sh ${t%.core}
}

