date > test.log
for t in *.core; 
{
  test.sh ${t%.core}
}

