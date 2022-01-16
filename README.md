# Orcfax Plutus Repository


This project gives an example for a simple Oracle using the Plutus Platform. The oracle is not meant for production.


## Setting up

Install nix: (https://nixos.org/download.html)

## The Plutus Application Backend (PAB) example

We have provided an Oracle PAB application in `./pab/Main.hs`. With the PAB we can serve and interact
with oracle contract over a web API. You can read more about the PAB here: [PAB Architecture](https://github.com/input-output-hk/plutus-apps/blob/main/plutus-pab/ARCHITECTURE.adoc).


Here, the PAB is configured with two endpoint contracts, the `Init` (initializes and starts an oracle contract) and `Oracle oracleParams` (updates the value of the oracle) `./src/Oracle/*.hs`.

1. Build the PAB executable:

```
cabal build orcfax-pab
```

2. Run the PAB binary:

```
cabal exec -- orcfax-pab
````

This will then start up the server on port 9080, and run a PAB simulator. The simulation includes starting an oracle and updating the value of the oracle. Read the simulator log for details. Look at the `./pab/Main.hs` to change the parameter of simulation

3.  run Test Emulator in repl

```
cabal repl
> import Oracle.Test
> test
```

For changing the details of the test see `./src/Oracle/Test.hs`
