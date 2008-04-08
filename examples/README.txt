pingpong: working and complete

auction: working but doesn't have the timeout, end of auction stuff

bank: incomplete

telephone: versions first through sixth are complete and working
           seventh is incomplete/not-working

im: single/fox: working
    single/wx: working but incomplete
    distributed: very incomplete

fib: a fib computation after Doug Lea's Fork/Join framework The goal
     of this is to show speedup on jruby, which has real
     concurrency. Right now, this is flakey. First, it doesn't bound
     concurrency (Doug's whole point) which gets out of control in the
     original version. The conservative version does weird things for
     me on linux. But so does the pure thread version (though the
     dramatis version is weirder than the thread version).
