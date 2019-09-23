module Run.Lenses where

import Prelude

import Control.Monad.Free (hoistFree)
import Data.Functor.Variant (FProxy, SProxy, VariantF, inj, on)
import Data.Lens (Getter, Lens', Setter, Setter', over, set, view)
import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol)
import Prim.Row (class Cons)
import Run (Run(..))
import Run.State (STATE, State(..), _state, gets, getsAt, modify)
import Unsafe.Coerce (unsafeCoerce)

hoistRun :: forall a b r r1 r2 sym. Functor a => Functor b => IsSymbol sym => Cons sym (FProxy a) r r1 => Cons sym (FProxy b) r r2 => SProxy sym -> (a ~> b) -> Run r1 ~> Run r2
hoistRun s t (Run f) = Run $ hoistFree ((hoistVariant s) t) f

hoistVariant :: forall a b r r1 r2 sym. Functor b => IsSymbol sym => Cons sym (FProxy a) r r1 => Cons sym (FProxy b) r r2 => SProxy sym -> (a ~> b) ->  VariantF r1 ~> VariantF r2
hoistVariant s t f = on s (inj s <<< t) unsafeCoerce f

zoom' :: forall s a. Lens' s a -> State a ~> State s
zoom' l (State s a) = State (l s) (a <<< view l)

zoomAt :: forall s a r r1 r2 sym.
  IsSymbol sym =>
  Cons sym (STATE a) r r1 =>
  Cons sym (STATE s) r r2 =>
  SProxy sym -> Lens' s a -> Run r1 ~> Run r2
zoomAt s l = hoistRun s (zoom' l)

zoom :: forall s a r. Lens' s a -> Run (state :: STATE a | r) ~> Run (state :: STATE s | r)
zoom = zoomAt _state

use :: forall s t a b r. Getter s t a b -> Run (state :: STATE s | r) a
use l = gets (view l)

useAt :: forall s t a b r r1 sym.
  IsSymbol sym =>
  Cons sym (STATE s) r r1 =>
  SProxy sym -> Getter s t a b -> Run r1 a
useAt s l = getsAt s (view l)

infix 4 assign as .=
infix 4 modifying as %=
infix 4 addModifying as +=
infix 4 mulModifying as *=
infix 4 subModifying as -=
infix 4 divModifying as //=
infix 4 disjModifying as ||=
infix 4 conjModifying as &&=
infix 4 appendModifying as <>=
infix 4 appendModifying as ++=
infix 4 assignJust as ?=

-- | Set the foci of a `Setter` in a monadic state to a constant value.
assign :: forall s a b r. Setter s s a b -> b -> Run (state :: STATE s | r) Unit
assign p b = modify (set p b)

-- | Modify the foci of a `Setter` in a monadic state.
modifying :: forall s a b r. Setter s s a b -> (a -> b) -> Run (state :: STATE s | r) Unit
modifying p f = void (modify (over p f))

addModifying :: forall s a r. Semiring a => Setter' s a -> a -> Run (state :: STATE s | r) Unit
addModifying p = modifying p <<< add

mulModifying :: forall s a r. Semiring a => Setter' s a -> a -> Run (state :: STATE s | r) Unit
mulModifying p = modifying p <<< flip mul

subModifying :: forall s a r. Ring a => Setter' s a -> a -> Run (state :: STATE s | r) Unit
subModifying p = modifying p <<< flip sub

divModifying :: forall s a r. EuclideanRing a => Setter' s a -> a -> Run (state :: STATE s | r) Unit
divModifying p = modifying p <<< flip div

disjModifying :: forall s a r. HeytingAlgebra a => Setter' s a -> a -> Run (state :: STATE s | r) Unit
disjModifying p = modifying p <<< flip disj

conjModifying :: forall s a r. HeytingAlgebra a => Setter' s a -> a -> Run (state :: STATE s | r) Unit
conjModifying p = modifying p <<< flip conj

appendModifying :: forall s a r. Semigroup a => Setter' s a -> a -> Run (state :: STATE s | r) Unit
appendModifying p = modifying p <<< flip append

assignJust :: forall s a b r. Setter s s a (Maybe b) -> b -> Run (state :: STATE s | r) Unit
assignJust p = assign p <<< Just
