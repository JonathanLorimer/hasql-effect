{s}: 
{
  ghcidScript = s "dev" "ghcid --command 'cabal new-repl lib:hasql-effect' --allow-eval --warnings";
  testScript = s "test" "cabal run test:hasql-effect-tests";
  hoogleScript = s "hgl" "hoogle serve";
}
