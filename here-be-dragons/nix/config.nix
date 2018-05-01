{
  allowBroken = true;
  allowUnfree = true;
  packageOverrides = super: let self = super.pkgs; in {

     legalesePoets =
     self.haskell.packages.ghc742.ghcWithPackages
        (haskellPackages:
      let
        inherit (haskellPackages)
          base unix bytestring deepseq containers template-haskell
          directory filepath pretty process array old-locale
          old-time time ghc-prim happy integer-gmp binary
          mkDerivation functor-infix bifunctors;
      in (builtins.attrValues (rec { 
        ansi-terminal = mkDerivation {
          pname = "ansi-terminal";
          version = "0.6.2.3";
          sha256 = "0hpfw0k025y681m9ml1c712skrb1p4vh7z5x1f0ci9ww7ssjrh2d";
          libraryHaskellDepends = [ base unix ];
          homepage = "https://github.com/feuerbach/ansi-terminal";
          description = "Simple ANSI terminal support, with Windows compatibility";
          license = self.stdenv.licenses.bsd3;
        };

        ansi-wl-pprint = mkDerivation {
          pname = "ansi-wl-pprint";
          version = "0.6.7.3";
          sha256 = "025pyphsjf0dnbrmj5nscbi6gzyigwgp3ifxb3psn7kji6mfr29p";
          libraryHaskellDepends = [ ansi-terminal base ];
          homepage = "http://github.com/ekmett/ansi-wl-pprint";
          description = "The Wadler/Leijen Pretty Printer for colored ANSI terminal output";
          license = self.stdenv.licenses.bsd3;
        };
        bytestring-builder = mkDerivation {
          pname = "bytestring-builder";
          version = "0.10.8.1.0";
          sha256 = "1hnvjac28y44yn78c9vdp1zvrknvlw98ky3g4n5vivr16rvh8x3d";
          libraryHaskellDepends = [ base bytestring deepseq ];
          description = "The new bytestring builder, packaged outside of GHC";
          license = self.stdenv.licenses.bsd3;
        };
        call-stack = mkDerivation {
          pname = "call-stack";
          version = "0.1.0";
          sha256 = "1qmihf5jafmc79sk52l6gpx75f5bnla2lp62kh3p34x3j84mwpzj";
          libraryHaskellDepends = [ base ];
          doCheck = false;
          homepage = "https://github.com/sol/call-stack#readme";
          description = "Use GHC call-stacks in a backward compatible way";
          license = self.stdenv.licenses.mit;
        };
        compdata = mkDerivation {
          pname = "compdata";
          version = "0.7.0.2";
          sha256 = "1657wjgd9kmv1mq085v35d05w242b1a5vyyrllixd24j764w1434";
          revision = "1";
          editedCabalFile = "fdcd2b5ddae78e5d3043dc794b2b21c698d6f2b90f7d974b008125124b82c1b9";
          libraryHaskellDepends = [
            base containers deepseq derive mtl QuickCheck template-haskell
            th-expand-syns transformers
          ];
          doCheck = false;
          description = "Compositional Data Types";
          license = self.stdenv.licenses.bsd3;
        };
        ConfigFile = mkDerivation {
          pname = "ConfigFile";
          version = "1.1.4";
          sha256 = "057mw146bip9wzs7j4b5xr1x24d8w0kr4i3inri5m57jkwspn25f";
          libraryHaskellDepends = [ base containers MissingH mtl parsec ];
          homepage = "http://software.complete.org/configfile";
          description = "Configuration file reading & writing";
          license = self.stdenv.licenses.bsd3;
        };
        cpphs = mkDerivation {
          pname = "cpphs";
          version = "1.20.2";
          sha256 = "1nr9hsnkz4spc4xdphkaqw97kkikpmycxr6nznjchrzql09dgcfw";
          isLibrary = true;
          isExecutable = true;
          libraryHaskellDepends = [
            base directory old-locale old-time polyparse
          ];
          executableHaskellDepends = [
            base directory old-locale old-time polyparse
          ];
          homepage = "http://projects.haskell.org/cpphs/";
          description = "A liberalised re-implementation of cpp, the C pre-processor";
          license = "LGPL";
        };
        datetime = mkDerivation {
          pname = "datetime";
          version = "0.3.1";
          sha256 = "0jmxxmv5s9rch84ivfjhqxdqnvqqzvabjs152wyv47h5qmvpag1k";
          libraryHaskellDepends = [ base old-locale old-time time ];
          doCheck = false;
          homepage = "http://github.com/stackbuilders/datetime";
          description = "Utilities to make Data.Time.* easier to use";
          license = "GPL";
        };
        derive = mkDerivation {
          pname = "derive";
          version = "2.5.26";
          sha256 = "0vaa9s5x2aas2cd35gv5a3hpzdrarn4bz5rw0mv3p7d7gxg2xvxn";
          isLibrary = true;
          isExecutable = true;
          libraryHaskellDepends = [
            base bytestring containers directory filepath haskell-src-exts
            pretty process syb template-haskell transformers uniplate
          ];
          executableHaskellDepends = [ base ];
          homepage = "https://github.com/ndmitchell/derive#readme";
          description = "A program and library to derive instances for data types";
          license = self.stdenv.licenses.bsd3;
        };
        equivalence = mkDerivation {
          pname = "equivalence";
          version = "0.3.1";
          sha256 = "0zi053l03r5qcnpsphnq8xqazd0cbgj9n44hn47s1hagdra3j1bs";
          libraryHaskellDepends = [
            base containers mtl STMonadTrans transformers transformers-compat
          ];
          doCheck = false;
          homepage = "https://bitbucket.org/paba/equivalence/";
          description = "Maintaining an equivalence relation implemented as union-find using STT";
          license = self.stdenv.licenses.bsd3;
        };
        ghc-paths = mkDerivation {
          pname = "ghc-paths";
          version = "0.1.0.9";
          sha256 = "0ibrr1dxa35xx20cpp8jzgfak1rdmy344dfwq4vlq013c6w8z9mg";
          libraryHaskellDepends = [ base ];
          description = "Knowledge of GHC's installation directories";
          license = self.stdenv.licenses.bsd3;
        };
        GraphSCC = mkDerivation {
          pname = "GraphSCC";
          version = "1.0.4";
          sha256 = "1wbcx3wb02adb7l4nchxla3laliz0h5q074vfw4z0ic833k977bq";
          libraryHaskellDepends = [ array base containers ];
          description = "Tarjan's algorithm for computing the strongly connected components of a graph";
          license = self.stdenv.licenses.bsd3;
        };
        hashable = mkDerivation {
          pname = "hashable";
          version = "1.2.4.0";
          sha256 = "1wrwpchksxd1i63ydpqy6jkdzxrb5qsy64rfiv9lik9r1kdp35pv";
          libraryHaskellDepends = [
            base bytestring ghc-prim integer-gmp text
          ];
          doCheck = false;
          homepage = "http://github.com/tibbe/hashable";
          description = "A class for types that can be converted to a hash value";
          license = self.stdenv.licenses.bsd3;
        };
        haskell-src-exts = mkDerivation {
          pname = "haskell-src-exts";
          version = "1.17.1";
          sha256 = "1g98amhn2b76g38y3dc55nny5812gqyqmswl1fniaiai41vm8p5s";
          revision = "1";
          editedCabalFile = "c07248f2a7b4bee1c7777dc6e441e8d1f32a02fb596ea49f47074c68b3c9ea0b";
          libraryHaskellDepends = [ array base cpphs ghc-prim pretty ];
          libraryToolDepends = [ happy ];
          doCheck = false;
          homepage = "https://github.com/haskell-suite/haskell-src-exts";
          description = "Manipulating Haskell source: abstract syntax, lexer, parser, and pretty-printer";
          license = self.stdenv.licenses.bsd3;
        };
        hslogger = mkDerivation {
          pname = "hslogger";
          version = "1.2.10";
          sha256 = "0as5gvlh6pi2gflakp695qnlizyyp059dqrhvjl4gjxalja6xjnp";
          libraryHaskellDepends = [
            base containers directory mtl network old-locale process time unix HUnit
          ];
          homepage = "http://software.complete.org/hslogger";
          description = "Versatile logging framework";
          license = self.stdenv.licenses.bsd3;
        };
        HTTP = mkDerivation {
          pname = "HTTP";
          version = "4000.3.3";
          sha256 = "1wlvvqcxsnd2is3khsla0vd8i9cy12v1pg6d6i13ihcd131a7bdv";
          libraryHaskellDepends = [
            array base bytestring mtl network parsec time
          ];
          doCheck = false;
          jailbreak = true;
          homepage = "https://github.com/haskell/HTTP";
          description = "A library for client-side HTTP";
          license = self.stdenv.licenses.bsd3;
        };
        HUnit = mkDerivation {
          pname = "HUnit";
          version = "1.4.0.0";
          sha256 = "15gcygf640dbwqvvwfv6bw0gbw3grcbbm6pkh40lxlqyq4dbvqyw";
          libraryHaskellDepends = [ base call-stack deepseq ];
          doCheck = false;
          homepage = "https://github.com/hspec/HUnit#readme";
          description = "A unit testing framework for Haskell";
          license = self.stdenv.licenses.bsd3;
        };
        IndentParser = mkDerivation {
          pname = "IndentParser";
          version = "0.2.1";
          sha256 = "0l9k8md2n0vhjqlvxcaf43i4cv09lnbbbw8vfz7bkbzhbwirs32j";
          libraryHaskellDepends = [ base parsec ];
          homepage = "http://www.cse.iitk.ac.in/~ppk";
          description = "Combinators for parsing indentation based syntatic structures";
          license = "GPL";
        };
        MissingH = mkDerivation {
          pname = "MissingH";
          version = "1.4.0.1";
          sha256 = "0wcvgrmav480w7nf4bl14yi0jq2yzanysxwzwas9hpb28vyjlgr8";
          libraryHaskellDepends = [
            array base containers directory filepath hslogger HUnit mtl network
            old-locale old-time parsec process random regex-compat time unix
          ];
          doCheck = false;
          homepage = "http://software.complete.org/missingh";
          description = "Large utility library";
          license = self.stdenv.licenses.bsd3;
        };
        mtl = mkDerivation {
          pname = "mtl";
          version = "2.1.3.1";
          sha256 = "1xpn2wjmqbh2cg1yssc6749xpgcqlrrg4iilwqgkcjgvaxlpdbvp";
          libraryHaskellDepends = [ base transformers ];
          homepage = "http://github.com/ekmett/mtl";
          description = "Monad classes, using functional dependencies";
          license = self.stdenv.licenses.bsd3;
        };
        nats = mkDerivation {
          pname = "nats";
          version = "1.1.1";
          sha256 = "1kfl2yy97nb7q0j17v96rl73xvi3z4db9bk0xychc76dax41n78k";
          libraryHaskellDepends = [ hashable ];
          homepage = "http://github.com/ekmett/nats/";
          description = "Natural numbers";
          license = self.stdenv.licenses.bsd3;
        };
        network = mkDerivation {
          pname = "network";
          version = "2.5.0.0";
          sha256 = "1x90fdzfigqq2vbjqg73p4vyy7p1z0apj79cpl7i9v9amxa6y5mj";
          libraryHaskellDepends = [ base bytestring parsec unix ];
          doCheck = false;
          homepage = "https://github.com/haskell/network";
          description = "Low-level networking interface";
          license = self.stdenv.licenses.bsd3;
        };
        optparse-applicative = mkDerivation {
          pname = "optparse-applicative";
          version = "0.13.0.0";
          sha256 = "1b0c5fdq8bd070g24vrjrwlq979r8dk8mys6aji9hy1l9pcv3inf";
          libraryHaskellDepends = [
            ansi-wl-pprint base process transformers transformers-compat semigroups
          ];
          doCheck = false;
          homepage = "https://github.com/pcapriotti/optparse-applicative";
          description = "Utilities and combinators for parsing command line options";
          license = self.stdenv.licenses.bsd3;
        };
        parsec = mkDerivation {
          pname = "parsec";
          version = "3.1.11";
          sha256 = "0vk7q9j2128q191zf1sg0ylj9s9djwayqk9747k0a5fin4f2b1vg";
          libraryHaskellDepends = [ base bytestring mtl text ];
          doCheck = false;
          homepage = "https://github.com/aslatter/parsec";
          description = "Monadic parser combinators";
          license = self.stdenv.licenses.bsd3;
        };
        polyparse = mkDerivation {
          pname = "polyparse";
          version = "1.12";
          sha256 = "05dya1vdvq29hkhkdlsglzhw7bdn51rvs1javs0q75nf99c66k7m";
          libraryHaskellDepends = [ base bytestring text ];
          homepage = "http://code.haskell.org/~malcolm/polyparse/";
          description = "A variety of alternative parser combinator libraries";
          license = "LGPL";
        };
        primitive = mkDerivation {
          pname = "primitive";
          version = "0.6.1.0";
          sha256 = "1j1q7l21rdm8kfs93vibr3xwkkhqis181w2k6klfhx5g5skiywwk";
          revision = "1";
          editedCabalFile = "6ec7c2455c437aba71f856b797e7db440c83719509aa63a9a3d1b4652ca3683d";
          libraryHaskellDepends = [ base ghc-prim transformers ];
          doCheck = false;
          homepage = "https://github.com/haskell/primitive";
          description = "Primitive memory-related operations";
          license = self.stdenv.licenses.bsd3;
        };
        QuickCheck = mkDerivation {
          pname = "QuickCheck";
          version = "2.9.2";
          sha256 = "119np67qvx8hyp9vkg4gr2wv3lj3j6ay2vl4hxspkg43ymb1cp0m";
          libraryHaskellDepends = [
            base containers random template-haskell tf-random transformers nats semigroups
          ];
          doCheck = false;
          homepage = "https://github.com/nick8325/quickcheck";
          description = "Automatic testing of Haskell programs";
          license = self.stdenv.licenses.bsd3;
        };
        random = mkDerivation {
          pname = "random";
          version = "1.1";
          sha256 = "0nis3lbkp8vfx8pkr6v7b7kr5m334bzb0fk9vxqklnp2aw8a865p";
          libraryHaskellDepends = [ base time ];
          doCheck = false;
          description = "random number library";
          license = self.stdenv.licenses.bsd3;
        };
        regex-base = mkDerivation {
          pname = "regex-base";
          version = "0.93.2";
          sha256 = "0y1j4h2pg12c853nzmczs263di7xkkmlnsq5dlp5wgbgl49mgp10";
          libraryHaskellDepends = [ array base bytestring containers mtl ];
          jailbreak = true;
          homepage = "http://sourceforge.net/projects/lazy-regex";
          description = "Replaces/Enhances Text.Regex";
          license = self.stdenv.licenses.bsd3;
        };
        regex-compat = mkDerivation {
          pname = "regex-compat";
          version = "0.95.1";
          sha256 = "0fwmima3f04p9y4h3c23493n1xj629ia2dxaisqm6rynljjv2z6m";
          libraryHaskellDepends = [ array base regex-base regex-posix ];
          homepage = "http://sourceforge.net/projects/lazy-regex";
          description = "Replaces/Enhances Text.Regex";
          license = self.stdenv.licenses.bsd3;
        };
        regex-posix = mkDerivation {
          pname = "regex-posix";
          version = "0.95.2";
          sha256 = "0gkhzhj8nvfn1ija31c7xnl6p0gadwii9ihyp219ck2arlhrj0an";
          libraryHaskellDepends = [
            array base bytestring containers regex-base
          ];
          homepage = "http://sourceforge.net/projects/lazy-regex";
          description = "Replaces/Enhances Text.Regex";
          license = self.stdenv.licenses.bsd3;
        };
        semigroups = mkDerivation {
          pname = "semigroups";
          version = "0.18.2";
          sha256 = "1r6hsn3am3dpf4rprrj4m04d9318v9iq02bin0pl29dg4a3gzjax";
          libraryHaskellDepends = [ base bytestring-builder hashable nats tagged text transformers unordered-containers ];
          homepage = "http://github.com/ekmett/semigroups/";
          description = "Anything that associates";
          license = self.stdenv.licenses.bsd3;
        };
        STMonadTrans = mkDerivation {
          pname = "STMonadTrans";
          version = "0.3.3";
          sha256 = "05d37ax0j8dp1h1w6pxkf1415mzn4gasdhn7gbsr8ay46iv1r4fr";
          libraryHaskellDepends = [ array base mtl ];
          description = "A monad transformer version of the ST monad";
          license = self.stdenv.licenses.bsd3;
        };
        syb = mkDerivation {
          pname = "syb";
          version = "0.6";
          sha256 = "1p3cnqjm13677r4a966zffzhi9b3a321aln8zs8ckqj0d9z1z3d3";
          libraryHaskellDepends = [ base ];
          doCheck = false;
          homepage = "http://www.cs.uu.nl/wiki/GenericProgramming/SYB";
          description = "Scrap Your Boilerplate";
          license = self.stdenv.licenses.bsd3;
        };
        tagged = mkDerivation {
          pname = "tagged";
          version = "0.8.5";
          sha256 = "16cdzh0bw16nvjnyyy5j9s60malhz4nnazw96vxb0xzdap4m2z74";
          libraryHaskellDepends = [
            base deepseq template-haskell transformers transformers-compat
          ];
          homepage = "http://github.com/ekmett/tagged";
          description = "Haskell 98 phantom types to avoid unsafely passing dummy arguments";
          license = self.stdenv.licenses.bsd3;
        };
        text =mkDerivation {
          pname = "text";
          version = "1.2.2.1";
          sha256 = "0nrrzx0ws7pv4dx9jbc6jm2734al1cr0m6iwcnbck4v2yfyv3p8s";
          libraryHaskellDepends = [
            array base binary bytestring deepseq ghc-prim integer-gmp
          ];
          doCheck = false;
          homepage = "https://github.com/bos/text";
          description = "An efficient packed Unicode text type";
          license = self.stdenv.licenses.bsd3;
        };
        tf-random = mkDerivation {
          pname = "tf-random";
          version = "0.5";
          sha256 = "0445r2nns6009fmq0xbfpyv7jpzwv0snccjdg7hwj4xk4z0cwc1f";
          libraryHaskellDepends = [ base primitive random time ];
          description = "High-quality splittable pseudorandom number generator";
          license = self.stdenv.licenses.bsd3;
        };
        th-expand-syns = mkDerivation {
          pname = "th-expand-syns";
          version = "0.4.0.0";
          sha256 = "1sjy7a17zwyvlbkc8gklii67sy78wpnw35fyb00lsbnpk4cryd2r";
          libraryHaskellDepends = [ base containers syb template-haskell ];
          doCheck = false;
          description = "Expands type synonyms in Template Haskell ASTs";
          license = self.stdenv.licenses.bsd3;
        };
        thrift = mkDerivation {
          pname = "thrift";
          version = "0.6.0";
          sha256 = "038h4ifpgq1g6sq468ikc7cj5qks68mszwz3gq8jwbdn8ca47275";
          revision = "1";
          editedCabalFile = "a41ce110c4581f23fcbb269fd093c554382bd108f88b3cbb58eb1cba5928cea0";
          libraryHaskellDepends = [
            base binary bytestring ghc-prim HTTP network
          ];
          homepage = "http://thrift.apache.org";
          description = "Haskell bindings for the Apache Thrift RPC system";
          license = "unknown";
        };
        transformers = mkDerivation {
          pname = "transformers";
          version = "0.3.0.0";
          sha256 = "14cmfdi4cmirbrc3x2h6ly08j0mb4p59mxbqkqw8rnbsr4g0rap5";
          revision = "1";
          editedCabalFile = "4ec3dd53be60415dad46e00ec6c7f78bdc37dcda1670cf5abe5c480719b78b60";
          libraryHaskellDepends = [ base ];
          description = "Concrete functor and monad transformers";
          license = self.stdenv.licenses.bsd3;
        };
        transformers-compat = mkDerivation {
          pname = "transformers-compat";
          version = "0.5.1.3";
          sha256 = "1qc12lwvdsfj81rr5z4ldnd1g5jpqd7bhzn6yb1dx6hfnd0239jc";
          libraryHaskellDepends = [ base ghc-prim transformers mtl ];
          homepage = "http://github.com/ekmett/transformers-compat/";
          description = "A small compatibility shim exposing the new types from transformers 0.3 and 0.4 to older Haskell platforms.";
          license = self.stdenv.licenses.bsd3;
        };
        uniplate = mkDerivation {
          pname = "uniplate";
          version = "1.6.12";
          sha256 = "1dx8f9aw27fz8kw0ad1nm6355w5rdl7bjvb427v2bsgnng30pipw";
          libraryHaskellDepends = [
            base containers hashable syb unordered-containers
          ];
          homepage = "http://community.haskell.org/~ndm/uniplate/";
          description = "Help writing simple, concise and fast generic operations";
          license = self.stdenv.licenses.bsd3;
        };
        unordered-containers = mkDerivation {
          pname = "unordered-containers";
          version = "0.2.7.1";
          sha256 = "00npqiphivjp2d7ryqsdavfn4m5v3w1lq2azhdsrfh0wsvqpg4ig";
          libraryHaskellDepends = [ base deepseq hashable ];
          doCheck = false;
          homepage = "https://github.com/tibbe/unordered-containers";
          description = "Efficient hashing-based container types";
          license = self.stdenv.licenses.bsd3;
        };
        wl-pprint = mkDerivation {
          pname = "wl-pprint";
          version = "1.2";
          sha256 = "166zvk4zwn2zaa9kx66m1av38m34qp6h4i65bri2sfnxgvx0700r";
          libraryHaskellDepends = [ base ];
          description = "The Wadler/Leijen Pretty Printer";
          license = self.stdenv.licenses.bsd3;
        };
        xml = mkDerivation {
          pname = "xml";
          version = "1.3.14";
          sha256 = "0g814lj7vaxvib2g3r734221k80k7ap9czv9hinifn8syals3l9j";
          libraryHaskellDepends = [ base bytestring text ];
          homepage = "http://code.galois.com";
          description = "A simple XML library";
          license = self.stdenv.licenses.bsd3;
        };


     
         poets = mkDerivation {
           pname = "poets";
           version = "0.1.2";
  
          src = let

#             repo = self.fetchhg {
#               url = "https://bitbucket.org/jespera/poets";
#               rev = "c0ee7194ce57d2ad6ca8894c8a44e88e546d5f4a";
#               sha256 = "10d5r7r8wzzcna02b1qciwvyhxavnzgz3wkjjvi4cvxk8avg4his";
#             };
 
             repo = self.fetchgit {
                url    = "https://github.com/legalese/poets.git";
                rev    = "a96217bfd92ba1e9247ca5d321fdc139a65bfc68";
#                date   = "2016-12-27T15:59:06+08:00";
                sha256 = "1iqnjypp8f6m3626nqh2fvzl5a5rbd6lxm8qsc0igmh230hpr76j";
             };

           in self.stdenv.mkDerivation {
             name = "poets-dir-plz";
             builder = self.writeScript "builder.sh" ''
               ${self.coreutils}/bin/mkdir -p $out
               ${self.coreutils}/bin/cp -r ${repo}/src/haskell/* $out
               ${self.coreutils}/bin/chmod a+w $out/poets.cabal
               ${self.coreutils}/bin/cp ${./poets.cabal} $out/poets.cabal
             '';
           };
 


          libraryHaskellDepends = [
            array base compdata ConfigFile containers datetime directory
            equivalence filepath ghc-paths GraphSCC hslogger IndentParser
            MissingH mtl network parsec STMonadTrans template-haskell thrift
            time wl-pprint xml
          ];
          description = "The POETS project";
          license = self.stdenv.licenses.mit;
        };
        bifunctors = mkDerivation {
          pname = "bifunctors";
          version = "5.1";
          sha256 = "1p9m8rbrfzh9a4hgllh2hldpc97nxlis97y5biak6g4czs6xxn8b";
          revision = "1";
          editedCabalFile = "b4c4393c49cb8a9f4dbae48fc68fcd1fdc1797f73a9a60320068c8704573432b";
          libraryHaskellDepends = [
            base containers semigroups tagged template-haskell
          ];
          doCheck = false;
          homepage = "http://github.com/ekmett/bifunctors/";
          description = "Bifunctors";
          license = self.stdenv.licenses.bsd3;
        };


#      test1HaskellEnv =
#      self.haskell.packages.ghc742.ghcWithPackages
#         (haskellPackages: with haskellPackages; [
#            mtl QuickCheck random text alex cpphs happy ghc-paths
#               # or anything you like.
#         ]);
# 
})));
        
#      test2HaskellEnv = self.haskell.packages.ghc742.ghcWithPackages (haskellPackages:
#       let
#         inherit (haskellPackages)
#           base unix bytestring deepseq containers template-haskell
#           directory filepath pretty process array old-locale
#           old-time time ghc-prim happy integer-gmp binary
#           mkDerivation functor-infix bifunctors;
#       in (builtins.attrValues (rec { 
#         ansi-terminal = mkDerivation {
#           pname = "ansi-terminal";
#           version = "0.6.2.3";
#           sha256 = "0hpfw0k025y681m9ml1c712skrb1p4vh7z5x1f0ci9ww7ssjrh2d";
#           libraryHaskellDepends = [ base unix ];
#           homepage = "https://github.com/feuerbach/ansi-terminal";
#           description = "Simple ANSI terminal support, with Windows compatibility";
# #          license = self.licenses.bsd3;
#         };
# })));
#      test3HaskellEnv =
#      pkgs.haskell.packages.ghc742.ghcWithPackages
#         (haskellPackages: with haskellPackages; [
#            mtl QuickCheck random text alex cpphs happy ghc-paths
#               # or anything you like.
#         ]);
#         

  };
}
