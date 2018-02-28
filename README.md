# decisiontree.hs

Simple categorical decision tree for Haskell

## Installation

The repository provides the necessary configuration for a quick cabal
install:

    $ git clone https://github.com/wbadart/decisiontree.hs \
        && cabal install decisiontree.hs

## Usage

The package exposes the modules `DecisionTree` and `Entropy`. The
provides the structural algorithms for filtering training instances
down the branches of the tree while the latter one, as the name
suggests, provides the entropy calculations for branching criteria.

For instance, to run an ID3-like classification on an instance, use:

    import DecisionTree (classify)
    import Entropy (informationGain)

    classification =
        classify trainingInstances informationGain ["red", "hot", "chili"]
            where trainingInstances = someDataYouHave

The package also provides a sample driver program via `Main.hs`,
which compiles to the binary `interactivedt` (which is currently
hard-coded to information gain) which can be used as follows:

    $ interactivedt mydata.csv
    instance> sushi,CA,cloudy
    "positive"
    instance> burritos,IN,rainy
    ...


## License

Distributed under the MIT license. See [LICENSE] and [opensource.org] for
details.


[LICENSE]: ./LICENSE
[opensource.org]: https://opensource.org/licenses/MIT
