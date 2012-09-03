#echo test $1 >> test.log
#echo compiling $1
#
#rm -f $1.out
../../src/lib/coreasm --silent --dump-core=$1-pp.core $1
../../src/lib/coreasm --silent $1-pp.core

if compare $1.lvm $1-pp.lvm >> /dev/null; then
   echo $1 '(ok)'
else 
   echo
   echo -- ERROR: $1 --------------
   ls -al $1.lvm $1-pp.lvm
   compare $1.lvm $1-pp.lvm
   diff $1.instr $1-pp.instr
   echo asm
   diff $1.asm $1-pp.asm
   echo opt
   diff $1.opt $1-pp.opt
   echo -----------------------------
   echo 
fi

# cleaning up
rm -f $1-pp.core
rm -f $1-pp.lvm