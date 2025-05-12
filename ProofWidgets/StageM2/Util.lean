instance instMonad : Monad List.{u} where
  pure x := [x]
  bind l f := l.flatMap f
  map f l := l.map f
