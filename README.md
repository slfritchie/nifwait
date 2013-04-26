
This is a fork of https://github.com/basho/nifwait.

You're probably looking for one of two branches in this
repository.  The `master` branch does not contain anything
except this README file.

1. The `sleep` branch, which corresponds to the basho repo's `master` branch.
2. The `md5` branch, which corresponds to the basho repo's `md5` branch.

The `sleep` branch is Joe's original, using a pair of simple custom NIF
functions to demonstrate scheduler imbalance when NIFs execute for 
too long without returning control to Erlang code.

The `md5` branch does the same, but without using a custom NIF.
Instead, it uses the MD5 functions in the Erlang/OTP `crypto` module
to demonstrate the same scheduling problem.
