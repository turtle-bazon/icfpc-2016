Team Skobochka Members
----------------------

* Gleb Golubitsky (sectoid);
* Azamat S. Kalimoulline (turtle);
* Alexey Voznuyk (swizard);
* Fedor Shashin;
* Pavel Sujkov (jtootf);

Building & running
==================

1. Install `stack` tool [1]
2. `stack setup`
3. `stack build`
4. `stack exec icfpc2016-exe`

Installing clipper
==================

1. `% cd distr; mkdir clipper; cd clipper; unzip ../clipper_ver5.1.6.zip`
2. `% cd cpp`
3. `% cmake .`
4. `% make && sudo make install`
5. Make sure you have git version of `stack`. If not, run `% stack upgrade --git`.
6. Uncomment relevant `ghc-options` section in `stack.yaml`

Links
=====
[1] - https://docs.haskellstack.org/en/stable/install_and_upgrade/
