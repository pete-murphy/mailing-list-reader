# List available recipes
help:
    @just --unstable --list

test:
    @ghcid --command="cabal repl mailing-list-reader-test mailing-list-reader --enable-multi-repl --ghc-options='-Wno-all'" --test="Spec.main"

# Run a simple python server in temp
dev:
    @open http://localhost:8000
    @python3 -m http.server --directory temp

query FILTER="." INPUTFILE="temp/data.ndjson":
    @while read -r line; do echo "$line" | jq {{ FILTER }}; done < {{ INPUTFILE }}
  