echo test $1 >> test.log
echo compiling $1

rm -f $1.out
runhugs ../../src/lib/core/main $1    >>$1.out  2>>$1.out

echo running $1
../../src/runtime/lvmrun $1 >>$1.out   2>>$1.out

if runhugs ../compare.hs $1.out $1.ok >> /dev/null; then
echo ok
else
echo -- error --
cat $1.out
runhugs ../compare.hs $1.out $1.ok
echo -----------
echo -- error -- >> test.log
cat $1.out	 >> test.log
runhugs ../compare.hs $1.out $1.ok >> test.log 2>> test.log;
echo ----------- >> test.log
fi

#echo >> test.log
echo
