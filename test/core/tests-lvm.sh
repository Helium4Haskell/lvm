date > test.log

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
for t in *.lvm; 
{
  test-lvm.sh ${t%.lvm}
}

