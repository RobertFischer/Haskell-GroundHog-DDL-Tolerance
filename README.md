# Test of the Tolerance for DDL Changes in Haskell's Groundhog 

[Groundhog](https://hackage.haskell.org/package/groundhog) is apparently touchy about DDL changes on the tables backing its entities. I was curious if we could attach
triggers and OIDs to them on PostgreSQL. Turns out you can, and this package is the proof!

# To run

Do `./run.sh` in bash or something bash-like
